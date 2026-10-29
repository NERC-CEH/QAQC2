#!/usr/bin/env python3

"""
Summarise WRF output variables for QAQC.

This script:
  - reads WRF output files for one domain, e.g. d03;
  - computes user-selected summary statistics for selected variables;
  - writes one NetCDF file per variable/stat/time period;
  - optionally reads common settings from the R QAQC user input file pointed to
    by the WRF_QAQC_USER_INPUT environment variable.

Filename format:
  {DOMAIN_TAG}_{wrf_var}_{stat}_{start_date}_{end_date}.nc

Notes:
  - START_DATE and END_DATE are user-facing and inclusive.
  - If START_DATE or END_DATE is None, the available WRF timestamps are used.
  - For normal stats, timestamps are selected as:
        START_DATE 00:00 <= time < END_DATE + 1 day 00:00
  - For precip, "precip" is a pseudo-variable:
        (RAINNC + RAINC) at END_DATE + 1 day 00:00
      - (RAINNC + RAINC) at START_DATE 00:00
"""

import json
import os
import re
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path

import netCDF4 as nc
import numpy as np
import xarray as xr
import wrf
from wrf import getvar, to_np


# =============================================================================
# User input
# =============================================================================

# Standalone defaults, optionally overridden by the R QAQC user input file.
WRFOUT_DIR = "/path/to/WRF/output"
DOMAIN_TAGS = ["d01", "d03"]

# Standalone output directory settings.
# If WRF_SUMMARY_BASE_DIR is None, summaries are written to:
#   parent of WRFOUT_DIR / WRF_SUMMARY_SUBDIR
# When USE_QAQC_OUTPUT_DIR is True, TEST_WRF_SUMMARY_DIR from the
# R QAQC user input file overrides these settings.
WRF_SUMMARY_BASE_DIR = None
WRF_SUMMARY_SUBDIR = "WRF_summaries"

# If None, the script looks for wrfout_{DOMAIN_TAG}_*
WRFOUT_FILE_GLOB = None

# Use "precip" for accumulated precipitation from RAINNC + RAINC.
# Use "ws" as an alias for 10 m wind speed from uvmet10_wspd_wdir.
WRF_SUMMARY_VARS = [
    "T2",
    "td2",
    "rh2",
    "PSFC",
    "ws",
    "precip",
]

# Supported for non-precip variables:
#   mean, min, max, median, pXX
# For precip, this is ignored and "accum" is used automatically.
STATS = [
    "mean",
]

# Use None to summarise the full available WRF period.
START_DATE = None
END_DATE = None

OVERWRITE = False

# Percentiles/median need temporary on-disk stacking.
# If None, uses summary_dir / "_tmp".
TMP_DIR = None

# Spatial chunk size for percentile calculations.
# Larger is faster but uses more memory.
PERCENTILE_SPATIAL_CHUNK = 20000

# Compression level for output NetCDF.
NETCDF_COMPLEVEL = 4

# T2 from WRF is normally Kelvin. Set True to write T2 summaries in Celsius.
CONVERT_T2_TO_CELSIUS = True

# Read common settings from the R user input file if WRF_QAQC_USER_INPUT is set.
USE_QAQC_USER_INPUT = True

# Which values to adopt from the R QAQC user input file.
USE_QAQC_TEST_DIR = True
USE_QAQC_TEST_DOMAINS = True
USE_QAQC_OUTPUT_DIR = True
USE_QAQC_SUMMARY_VARS = True
USE_QAQC_SUMMARY_STATS = True

# =============================================================================
# Logging helpers
# =============================================================================

SCRIPT_START_TIME = time.time()


def fmt_elapsed(seconds):
    seconds = int(round(seconds))
    hours, rem = divmod(seconds, 3600)
    minutes, seconds = divmod(rem, 60)

    if hours > 0:
        return f"{hours:d}h {minutes:02d}m {seconds:02d}s"

    if minutes > 0:
        return f"{minutes:d}m {seconds:02d}s"

    return f"{seconds:d}s"


def log(msg):
    elapsed = fmt_elapsed(time.time() - SCRIPT_START_TIME)
    print(f"[+{elapsed}] {msg}", flush=True)


# =============================================================================
# QAQC R user input handling
# =============================================================================


def has_value(x):
    if x is None:
        return False

    if isinstance(x, str):
        return x.strip() != ""

    if isinstance(x, (list, tuple)):
        return len(x) > 0 and not all(v is None or str(v).strip() == "" for v in x)

    return True


def as_list(x):
    if x is None:
        return []

    if isinstance(x, list):
        return [str(v) for v in x if v is not None and str(v).strip() != ""]

    if isinstance(x, tuple):
        return [str(v) for v in x if v is not None and str(v).strip() != ""]

    if str(x).strip() == "":
        return []

    return [str(x)]


def read_qaqc_user_input(user_input_path=None):
    """Read selected values from an R QAQC user input file as JSON."""

    if user_input_path is None:
        user_input_path = os.environ.get("WRF_QAQC_USER_INPUT", "")

    if not has_value(user_input_path):
        return {}

    user_input_path = str(user_input_path)

    r_code = r'''
suppressPackageStartupMessages({
  library(fs)
  library(jsonlite)
})

user_input_file = Sys.getenv("WRF_QAQC_USER_INPUT")

if (is.na(user_input_file) || user_input_file == "") {
  stop("WRF_QAQC_USER_INPUT is empty.")
}

if (!fs::file_exists(user_input_file)) {
  stop(paste0("User input file not found: ", user_input_file))
}

source(user_input_file)

get_or_null = function(x) {
  if (!exists(x, inherits = TRUE)) {
    return(NULL)
  }

  val = get(x, inherits = TRUE)

  if (is.null(val) || length(val) == 0 || all(is.na(val))) {
    return(NULL)
  }

  as.character(val)
}

out = list(
  TEST_DIR = get_or_null('TEST_DIR'),
  TEST_DOMAINS = get_or_null('TEST_DOMAINS'),
  TEST_WRF_SUMMARY_DIR = get_or_null('TEST_WRF_SUMMARY_DIR'),
  WRF_SUMMARY_VARS = get_or_null('WRF_SUMMARY_VARS'),
  WRF_SUMMARY_STATS = get_or_null('WRF_SUMMARY_STATS')
)

cat(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null"))
'''

    env = os.environ.copy()
    env["WRF_QAQC_USER_INPUT"] = user_input_path

    log(f"Reading QAQC user input file: {user_input_path}")

    try:
        result = subprocess.run(
            ["Rscript", "-e", r_code],
            capture_output=True,
            text=True,
            check=True,
            env=env,
        )
    except subprocess.CalledProcessError as exc:
        msg = (
            "Failed to read the QAQC R user input file using Rscript.\n"
            f"Command stdout:\n{exc.stdout}\n\n"
            f"Command stderr:\n{exc.stderr}"
        )
        raise RuntimeError(msg) from exc

    try:
        return json.loads(result.stdout)
    except json.JSONDecodeError as exc:
        msg = (
            "Failed to parse JSON returned by the QAQC R user input file.\n"
            f"Command stdout:\n{result.stdout}\n\n"
            f"Command stderr:\n{result.stderr}"
        )
        raise RuntimeError(msg) from exc


def apply_qaqc_user_input():
    """Optionally override Python settings using the R QAQC user input file."""

    global WRFOUT_DIR
    global DOMAIN_TAGS
    global WRF_SUMMARY_BASE_DIR
    global WRF_SUMMARY_SUBDIR
    global WRF_SUMMARY_VARS
    global STATS

    if not USE_QAQC_USER_INPUT:
        return

    user_input_path = os.environ.get("WRF_QAQC_USER_INPUT", "")

    if not has_value(user_input_path):
        log("WRF_QAQC_USER_INPUT is not set; using Python-script settings.")
        return

    qaqc_input = read_qaqc_user_input(user_input_path)

    if USE_QAQC_TEST_DIR and has_value(qaqc_input.get("TEST_DIR")):
        WRFOUT_DIR = as_list(qaqc_input["TEST_DIR"])[0]

    if USE_QAQC_TEST_DOMAINS and has_value(qaqc_input.get("TEST_DOMAINS")):
        DOMAIN_TAGS = as_list(qaqc_input["TEST_DOMAINS"])

    if USE_QAQC_OUTPUT_DIR and has_value(qaqc_input.get("TEST_WRF_SUMMARY_DIR")):
        wrf_summary_dir = Path(as_list(qaqc_input["TEST_WRF_SUMMARY_DIR"])[0])
        WRF_SUMMARY_BASE_DIR = str(wrf_summary_dir.parent)
        WRF_SUMMARY_SUBDIR = wrf_summary_dir.name

    if USE_QAQC_SUMMARY_VARS and has_value(qaqc_input.get("WRF_SUMMARY_VARS")):
        WRF_SUMMARY_VARS = as_list(qaqc_input["WRF_SUMMARY_VARS"])

    if USE_QAQC_SUMMARY_STATS and has_value(qaqc_input.get("WRF_SUMMARY_STATS")):
        STATS = as_list(qaqc_input["WRF_SUMMARY_STATS"])


# =============================================================================
# General helpers
# =============================================================================


def find_wrf_files(wrfout_dir, domain_tag, file_glob=None):
    wrfout_dir = Path(wrfout_dir)

    if file_glob is None:
        file_glob = f"wrfout_{domain_tag}_*"

    files = sorted(wrfout_dir.glob(file_glob))
    files = [p for p in files if p.is_file()]

    if not files:
        raise FileNotFoundError(
            f"No WRF files found in {wrfout_dir} using glob '{file_glob}'"
        )

    return [str(p) for p in files]


def resolve_summary_dir():
    """Resolve final output directory for WRF summary NetCDF files."""

    if WRF_SUMMARY_BASE_DIR is None:
        base_dir = Path(WRFOUT_DIR).parent
    else:
        base_dir = Path(WRF_SUMMARY_BASE_DIR)

    return base_dir / WRF_SUMMARY_SUBDIR


def as_float_array(x):
    """Convert wrf-python/xarray/numpy output to a plain float32 ndarray."""

    arr = to_np(x)

    if np.ma.isMaskedArray(arr):
        arr = arr.filled(np.nan)

    return np.asarray(arr, dtype="float32")


def masked_to_float_array(x, dtype="float32"):
    """Convert a netCDF variable slice to ndarray, preserving masked values as NaN."""

    if np.ma.isMaskedArray(x):
        x = x.filled(np.nan)

    return np.asarray(x, dtype=dtype)


def ensure_time_yx(arr, expected_nt, wrf_var):
    """Ensure an array has shape time x south_north x west_east."""

    arr = np.asarray(arr)

    if arr.ndim == 2:
        if expected_nt != 1:
            raise ValueError(
                f"Variable '{wrf_var}' returned a 2-D array but {expected_nt} "
                "timestamps were expected."
            )
        return arr[np.newaxis, :, :]

    if arr.ndim == 3:
        return arr

    raise ValueError(
        f"Variable '{wrf_var}' produced an array with shape {arr.shape}. "
        "Expected 2-D or 3-D time/y/x data."
    )


def fmt_date_for_fname(date_string):
    return datetime.strptime(date_string, "%Y-%m-%d").strftime("%d%b%y")


def normalise_stat_name(stat):
    stat = str(stat).lower()

    if stat in {"mean", "min", "max", "median", "accum"}:
        return stat

    if re.fullmatch(r"p\d+(\.\d+)?", stat):
        return stat

    raise ValueError(
        f"Unsupported stat '{stat}'. Use mean, min, max, median, accum, "
        "or pXX, e.g. p95."
    )


def stat_to_percentile(stat):
    stat = normalise_stat_name(stat)

    if stat == "median":
        return 50.0

    match = re.fullmatch(r"p(\d+(\.\d+)?)", stat)
    if match:
        return float(match.group(1))

    raise ValueError(f"Stat '{stat}' is not a percentile stat")


def safe_var_name(wrf_var):
    return str(wrf_var).replace("/", "_").replace(" ", "_")


def output_filename(domain_tag, wrf_var, stat, start_date, end_date):
    start_lab = fmt_date_for_fname(start_date)
    end_lab = fmt_date_for_fname(end_date)

    return (
        f"{domain_tag}_"
        f"{safe_var_name(wrf_var)}_"
        f"{normalise_stat_name(stat)}_"
        f"{start_lab}_"
        f"{end_lab}.nc"
    )


def split_stats(stats):
    """
    Split requested stats into:
      - streaming stats: mean/min/max, calculated together in one pass;
      - percentile stats: median/pXX, calculated separately.
    """

    stats_norm = [normalise_stat_name(stat) for stat in stats]

    streaming_stats = [
        stat for stat in stats_norm
        if stat in {"mean", "min", "max"}
    ]

    percentile_stats = [
        stat for stat in stats_norm
        if stat == "median" or re.fullmatch(r"p\d+(\.\d+)?", stat)
    ]

    other_stats = sorted(
        set(stats_norm) - set(streaming_stats) - set(percentile_stats)
    )

    if other_stats:
        raise ValueError(f"Unsupported non-precip stats: {other_stats}")

    return streaming_stats, percentile_stats


def output_path(summary_dir, domain_tag, wrf_var, stat, start_date, end_date):
    return summary_dir / output_filename(
        domain_tag=domain_tag,
        wrf_var=wrf_var,
        stat=stat,
        start_date=start_date,
        end_date=end_date,
    )


# =============================================================================
# Time handling
# =============================================================================


def decode_wrf_times(ds):
    """Read WRF Times from one open NetCDF file using netCDF4 only."""

    if "Times" not in ds.variables:
        times = wrf.extract_times(ds, wrf.ALL_TIMES)
        return np.asarray(times).astype("datetime64[ns]")

    raw = ds.variables["Times"][:]

    out = []
    for row in raw:
        if np.ma.isMaskedArray(row):
            row = row.filled(b" ")

        if row.dtype.kind in {"S", "a"}:
            text = b"".join(row.tolist()).decode("utf-8").strip()
        else:
            text = "".join(row.astype(str).tolist()).strip()

        text = text.replace("_", "T")
        out.append(np.datetime64(text, "ns"))

    return np.asarray(out, dtype="datetime64[ns]")


def extract_times_from_files(wrf_files):
    """Extract all WRF timestamps file by file."""

    all_times = []

    for i, wrf_file in enumerate(wrf_files, start=1):
        with nc.Dataset(wrf_file) as ds:
            all_times.append(decode_wrf_times(ds))

        if i % 50 == 0:
            log(f"Read timestamps from {i}/{len(wrf_files)} files")

    return np.concatenate(all_times).astype("datetime64[ns]")


def resolve_summary_dates(times, start_date, end_date):
    """Resolve user-facing inclusive START_DATE and END_DATE."""

    first_time = times[0]
    last_time = times[-1]

    if start_date is None:
        start_date = str(first_time.astype("datetime64[D]"))

    if end_date is None:
        last_date = last_time.astype("datetime64[D]")

        # If the final timestamp is exactly midnight, treat it as the +1
        # boundary timestep and use the previous date as the inclusive end date.
        if last_time == last_date:
            end_date_np = last_date - np.timedelta64(1, "D")
        else:
            end_date_np = last_date

        end_date = str(end_date_np)

    return start_date, end_date


def select_time_indices(times, start_date, end_date):
    """
    Select timestamps for normal statistics.

    END_DATE is inclusive at the user level. Internally:
      start <= time < end_date + 1 day
    """

    start = np.datetime64(start_date)
    end_exclusive = np.datetime64(end_date) + np.timedelta64(1, "D")

    idx = np.where((times >= start) & (times < end_exclusive))[0]

    if idx.size == 0:
        raise ValueError(
            f"No WRF timesteps found between {start_date} and {end_date}"
        )

    return idx


def selected_file_records(wrf_files, start_date, end_date):
    """Return one record per file containing selected local timestep indices."""

    start = np.datetime64(start_date)
    end_exclusive = np.datetime64(end_date) + np.timedelta64(1, "D")

    records = []

    for i, wrf_file in enumerate(wrf_files, start=1):
        with nc.Dataset(wrf_file) as ds:
            file_times = decode_wrf_times(ds)

        local_idx = np.where((file_times >= start) & (file_times < end_exclusive))[0]

        if local_idx.size > 0:
            records.append(
                {
                    "wrf_file": wrf_file,
                    "file_index": i,
                    "file_times": file_times,
                    "local_idx": local_idx,
                }
            )

    if not records:
        raise ValueError(
            f"No WRF files contain timesteps between {start_date} and {end_date}"
        )

    return records


def find_exact_time_in_files(wrf_files, target_time):
    """Find a target timestamp and return file path plus local timestep index."""

    target_time = np.datetime64(target_time, "ns")

    for wrf_file in wrf_files:
        with nc.Dataset(wrf_file) as ds:
            file_times = decode_wrf_times(ds)

        matches = np.where(file_times == target_time)[0]

        if matches.size > 0:
            return wrf_file, int(matches[0])

    raise ValueError(f"No exact WRF timestamp found: {target_time}")


# =============================================================================
# WRF variable extraction
# =============================================================================


def canonical_wrf_var(wrf_var):
    """
    User-facing aliases.

    "ws" is saved as "ws" in the filename/output variable, but internally
    calculated from wrf-python diagnostic uvmet10_wspd_wdir.
    """

    aliases = {
        "ws": "uvmet10_wspd_wdir",
        "precip": "precip",
    }

    return aliases.get(wrf_var, wrf_var)


def read_native_file_block(ds, wrf_var, local_idx):
    """Read a native WRF variable block directly from NetCDF."""

    var = ds.variables[wrf_var]

    if "Time" not in var.dimensions:
        arr = masked_to_float_array(var[:])
        arr = ensure_time_yx(arr, expected_nt=1, wrf_var=wrf_var)
        return arr

    time_axis = var.dimensions.index("Time")

    if time_axis != 0:
        raise ValueError(
            f"Native variable '{wrf_var}' has Time axis at position {time_axis}; "
            "this script currently expects Time to be the first dimension."
        )

    arr = masked_to_float_array(var[local_idx, :, :])
    arr = ensure_time_yx(arr, expected_nt=len(local_idx), wrf_var=wrf_var)

    return arr


def read_diagnostic_file_block(ds, wrf_var, local_idx, file_times):
    """Read a wrf-python diagnostic from one file, then subset local timesteps."""

    internal_var = canonical_wrf_var(wrf_var)

    if internal_var == "uvmet10_wspd_wdir":
        da = getvar(ds, "uvmet10_wspd_wdir", timeidx=wrf.ALL_TIMES)

        if hasattr(da, "sel") and "wspd_wdir" in da.dims:
            da = da.sel(wspd_wdir="wspd")
        else:
            arr_tmp = as_float_array(da)

            # Expected fallback shapes are either 2 x time x y x x or
            # time x 2 x y x x. Keep the first component, wind speed.
            if arr_tmp.ndim == 4 and arr_tmp.shape[0] == 2:
                arr_tmp = arr_tmp[0, :, :, :]
            elif arr_tmp.ndim == 4 and arr_tmp.shape[1] == 2:
                arr_tmp = arr_tmp[:, 0, :, :]
            else:
                raise ValueError(
                    "Could not identify wind-speed component from "
                    f"uvmet10_wspd_wdir output with shape {arr_tmp.shape}."
                )

            arr_all = ensure_time_yx(
                arr_tmp,
                expected_nt=len(file_times),
                wrf_var=wrf_var,
            )
            return arr_all[local_idx, :, :]

    else:
        da = getvar(ds, internal_var, timeidx=wrf.ALL_TIMES)

    arr_all = as_float_array(da)
    arr_all = ensure_time_yx(
        arr_all,
        expected_nt=len(file_times),
        wrf_var=wrf_var,
    )

    return arr_all[local_idx, :, :]


def read_wrf_file_block(ds, wrf_var, local_idx, file_times):
    """Read selected timesteps for one variable from one open WRF file."""

    internal_var = canonical_wrf_var(wrf_var)

    if internal_var in ds.variables:
        return read_native_file_block(ds, internal_var, local_idx)

    return read_diagnostic_file_block(
        ds=ds,
        wrf_var=wrf_var,
        local_idx=local_idx,
        file_times=file_times,
    )


def read_static_2d(ds, var_name):
    """Read a static or time-varying 2-D WRF coordinate variable."""

    if var_name not in ds.variables:
        raise ValueError(f"Required coordinate variable '{var_name}' not found.")

    var = ds.variables[var_name]
    arr = masked_to_float_array(var[:])

    if arr.ndim == 3:
        arr = arr[0, :, :]

    if arr.ndim != 2:
        raise ValueError(
            f"Coordinate variable '{var_name}' has unexpected shape {arr.shape}."
        )

    return arr.astype("float32")


def apply_post_processing(wrf_var, summary):
    """Apply variable-specific conversions after the statistic has been calculated."""

    out = np.asarray(summary, dtype="float32")

    if wrf_var == "T2" and CONVERT_T2_TO_CELSIUS:
        out = out - 273.15

    if wrf_var == "PSFC":
        out = out / 100.0

    return out.astype("float32")


def output_units(wrf_var, example_file=None):
    if wrf_var == "T2" and CONVERT_T2_TO_CELSIUS:
        return "degC"

    if wrf_var == "PSFC":
        return "hPa"

    if wrf_var == "precip":
        return "mm"

    if wrf_var == "ws":
        return "m s-1"

    if wrf_var == "rh2":
        return "%"

    if wrf_var == "td2":
        return "degC"

    if example_file is not None:
        with nc.Dataset(example_file) as ds:
            internal_var = canonical_wrf_var(wrf_var)
            if internal_var in ds.variables:
                return getattr(ds.variables[internal_var], "units", "")

    return ""


# =============================================================================
# Summary calculations
# =============================================================================


def summarise_streaming_multi_filewise(wrf_files, wrf_var, start_date, end_date, stats):
    """
    Memory-safe file-wise streaming summaries for mean/min/max.

    This avoids repeated wrf.getvar() calls on a long multi-file list.
    """

    stats = [normalise_stat_name(stat) for stat in stats]

    supported = {"mean", "min", "max"}
    unsupported = sorted(set(stats) - supported)

    if unsupported:
        raise ValueError(
            "summarise_streaming_multi_filewise() only supports mean/min/max. "
            f"Unsupported stats: {unsupported}"
        )

    need_mean = "mean" in stats
    need_min = "min" in stats
    need_max = "max" in stats

    records = selected_file_records(
        wrf_files=wrf_files,
        start_date=start_date,
        end_date=end_date,
    )

    acc_sum = None
    acc_count = None
    acc_min = None
    acc_max = None

    n_processed = 0
    first_selected_file = records[0]["wrf_file"]

    for rec_i, record in enumerate(records, start=1):
        wrf_file = record["wrf_file"]
        file_times = record["file_times"]
        local_idx = record["local_idx"]

        with nc.Dataset(wrf_file) as ds:
            arr = read_wrf_file_block(
                ds=ds,
                wrf_var=wrf_var,
                local_idx=local_idx,
                file_times=file_times,
            )

        if acc_sum is None and need_mean:
            acc_sum = np.zeros(arr.shape[1:], dtype="float64")
            acc_count = np.zeros(arr.shape[1:], dtype="int32")

        if acc_min is None and need_min:
            acc_min = np.full(arr.shape[1:], np.nan, dtype="float32")

        if acc_max is None and need_max:
            acc_max = np.full(arr.shape[1:], np.nan, dtype="float32")

        valid = np.isfinite(arr)

        if need_mean:
            acc_sum += np.nansum(arr, axis=0)
            acc_count += np.sum(valid, axis=0).astype("int32")

        if need_min:
            acc_min = np.fmin(acc_min, np.nanmin(arr, axis=0))

        if need_max:
            acc_max = np.fmax(acc_max, np.nanmax(arr, axis=0))

        n_processed += arr.shape[0]

        if rec_i % 10 == 0 or rec_i == len(records):
            log(
                f"{wrf_var}: processed {n_processed} timesteps "
                f"from {rec_i}/{len(records)} selected files "
                f"for stats {', '.join(stats)}"
            )

    out = {}

    if need_mean:
        mean = acc_sum / acc_count
        mean[acc_count == 0] = np.nan
        out["mean"] = apply_post_processing(wrf_var, mean)

    if need_min:
        out["min"] = apply_post_processing(wrf_var, acc_min)

    if need_max:
        out["max"] = apply_post_processing(wrf_var, acc_max)

    return out, first_selected_file


def summarise_percentile_memmap_filewise(
    wrf_files,
    wrf_var,
    start_date,
    end_date,
    stat,
    tmp_dir,
    spatial_chunk=PERCENTILE_SPATIAL_CHUNK,
):
    """Exact median/percentile using a temporary on-disk stack, file-wise."""

    percentile = stat_to_percentile(stat)

    records = selected_file_records(
        wrf_files=wrf_files,
        start_date=start_date,
        end_date=end_date,
    )

    first_file = records[0]["wrf_file"]

    with nc.Dataset(first_file) as ds:
        first_block = read_wrf_file_block(
            ds=ds,
            wrf_var=wrf_var,
            local_idx=records[0]["local_idx"],
            file_times=records[0]["file_times"],
        )

    nt = sum(len(record["local_idx"]) for record in records)
    ny, nx = first_block.shape[1:]

    tmp_dir = Path(tmp_dir)
    tmp_dir.mkdir(parents=True, exist_ok=True)

    tmp_file = tmp_dir / (
        f"{safe_var_name(wrf_var)}_{normalise_stat_name(stat)}_"
        f"stack_{os.getpid()}.dat"
    )

    log(
        f"{wrf_var} {stat}: creating temporary stack "
        f"{tmp_file} with shape ({nt}, {ny}, {nx})"
    )

    stack = np.memmap(
        tmp_file,
        dtype="float32",
        mode="w+",
        shape=(nt, ny, nx),
    )

    write_pos = 0

    for rec_i, record in enumerate(records, start=1):
        if rec_i == 1:
            block = first_block
        else:
            with nc.Dataset(record["wrf_file"]) as ds:
                block = read_wrf_file_block(
                    ds=ds,
                    wrf_var=wrf_var,
                    local_idx=record["local_idx"],
                    file_times=record["file_times"],
                )

        n_block = block.shape[0]
        stack[write_pos:write_pos + n_block, :, :] = block
        write_pos += n_block

        if rec_i % 10 == 0 or rec_i == len(records):
            log(
                f"{wrf_var} {stat}: loaded {write_pos}/{nt} timesteps "
                f"from {rec_i}/{len(records)} selected files into temporary stack"
            )

    stack.flush()

    flat = stack.reshape(nt, ny * nx)
    out_flat = np.empty(ny * nx, dtype="float32")

    for start in range(0, ny * nx, spatial_chunk):
        stop = min(start + spatial_chunk, ny * nx)

        out_flat[start:stop] = np.nanpercentile(
            flat[:, start:stop],
            percentile,
            axis=0,
        ).astype("float32")

        log(
            f"{wrf_var} {stat}: calculated spatial chunk "
            f"{start}:{stop} of {ny * nx}"
        )

    out = out_flat.reshape(ny, nx)

    try:
        del stack
        tmp_file.unlink()
    except Exception as exc:
        log(f"WARNING: could not remove temporary file {tmp_file}: {exc}")

    return apply_post_processing(wrf_var, out), first_file


def get_precip_cumulative_from_file(wrf_file, local_idx):
    """Read cumulative RAINNC + RAINC from one file/local time index."""

    with nc.Dataset(wrf_file) as ds:
        rainnc = masked_to_float_array(ds.variables["RAINNC"][local_idx, :, :])
        rainc = masked_to_float_array(ds.variables["RAINC"][local_idx, :, :])

    return rainnc + rainc


def summarise_precip_accum_filewise(wrf_files, start_date, end_date):
    """Accumulated precipitation over the user-defined inclusive date period."""

    start_boundary = np.datetime64(start_date, "ns")
    end_boundary = np.datetime64(end_date, "ns") + np.timedelta64(1, "D")

    start_file, start_idx = find_exact_time_in_files(wrf_files, start_boundary)
    end_file, end_idx = find_exact_time_in_files(wrf_files, end_boundary)

    log(
        "precip accum: using boundaries "
        f"{start_boundary} -> {end_boundary}"
    )
    log(f"precip start file: {start_file}, local index {start_idx}")
    log(f"precip end file:   {end_file}, local index {end_idx}")

    precip_start = get_precip_cumulative_from_file(start_file, start_idx)
    precip_end = get_precip_cumulative_from_file(end_file, end_idx)

    out = precip_end - precip_start

    # Remove tiny negative values from numerical noise only.
    out = np.where((out < 0) & (out > -1e-4), 0.0, out)

    return out.astype("float32"), start_file


# =============================================================================
# NetCDF writing
# =============================================================================
def wrf_proj4_from_attrs(ds):
    """Build a simple proj4 CRS string from WRF global projection attributes."""

    map_proj = int(getattr(ds, "MAP_PROJ", 0))

    if map_proj == 1:
        # Lambert conformal
        return (
            "+proj=lcc "
            f"+lat_1={float(getattr(ds, 'TRUELAT1'))} "
            f"+lat_2={float(getattr(ds, 'TRUELAT2'))} "
            f"+lat_0={float(getattr(ds, 'MOAD_CEN_LAT', getattr(ds, 'CEN_LAT')))} "
            f"+lon_0={float(getattr(ds, 'STAND_LON'))} "
            "+x_0=0 +y_0=0 "
            "+ellps=sphere "
            "+units=m "
            "+no_defs"
        )

    if map_proj == 2:
        # Polar stereographic
        return (
            "+proj=stere "
            "+lat_0=90 "
            f"+lat_ts={float(getattr(ds, 'TRUELAT1'))} "
            f"+lon_0={float(getattr(ds, 'STAND_LON'))} "
            "+x_0=0 +y_0=0 "
            "+ellps=sphere "
            "+units=m "
            "+no_defs"
        )

    if map_proj == 3:
        # Mercator
        return (
            "+proj=merc "
            f"+lat_ts={float(getattr(ds, 'TRUELAT1'))} "
            f"+lon_0={float(getattr(ds, 'STAND_LON'))} "
            "+x_0=0 +y_0=0 "
            "+ellps=sphere "
            "+units=m "
            "+no_defs"
        )

    if map_proj == 6:
        # Latitude-longitude
        return "+proj=longlat +datum=WGS84 +no_defs"

    return ""


def projected_xy_from_lonlat(lat_np, lon_np, proj4):
    """
    Project WRF XLAT/XLONG to the projected CRS and infer regular x/y
    cell-centre coordinates in metres.
    """

    from pyproj import CRS, Transformer

    crs_src = CRS.from_epsg(4326)
    crs_dst = CRS.from_proj4(proj4)

    transformer = Transformer.from_crs(
        crs_src,
        crs_dst,
        always_xy=True,
    )

    x_proj, y_proj = transformer.transform(
        lon_np.astype("float64"),
        lat_np.astype("float64"),
    )

    # For WRF/stars output, x should have length nx and y should have length ny.
    x_coord = np.nanmedian(x_proj, axis=0)
    y_coord = np.nanmedian(y_proj, axis=1)

    x_delta = np.nanmedian(np.diff(x_coord))
    y_delta = np.nanmedian(np.diff(y_coord))

    # If the chosen orientation is wrong, the coordinate barely changes.
    if abs(x_delta) < 1:
        x_coord = np.nanmedian(x_proj, axis=1)

    if abs(y_delta) < 1:
        y_coord = np.nanmedian(y_proj, axis=0)

    return x_coord.astype("float32"), y_coord.astype("float32")


def add_grid_mapping_attrs(ds_out, var_out_name, proj4):
    """Attach simple CF-style grid mapping metadata to the summary variable."""

    if not proj4:
        return ds_out

    ds_out["crs"] = xr.DataArray(
        np.int32(0),
        attrs={
            "grid_mapping_name": "wrf_projected_crs",
            "proj4": proj4,
            "spatial_ref": proj4,
        },
    )

    ds_out[var_out_name].attrs["grid_mapping"] = "crs"

    return ds_out

def make_output_dataset(
    summary,
    example_file,
    domain_tag,
    wrf_var,
    stat,
    start_date,
    end_date,
    wrf_files,
):
    with nc.Dataset(example_file) as ds:
        lat_np = read_static_2d(ds, "XLAT")
        lon_np = read_static_2d(ds, "XLONG")

        ny, nx = lat_np.shape

        proj4 = wrf_proj4_from_attrs(ds)

        if int(getattr(ds, "MAP_PROJ", 0)) == 6:
            x_coord = lon_np[0, :].astype("float32")
            y_coord = lat_np[:, 0].astype("float32")
            x_units = "degrees_east"
            y_units = "degrees_north"
            x_long_name = "longitude"
            y_long_name = "latitude"
        else:
            x_coord, y_coord = projected_xy_from_lonlat(
                lat_np=lat_np,
                lon_np=lon_np,
                proj4=proj4,
            )
            x_units = "m"
            y_units = "m"
            x_long_name = "projection x coordinate"
            y_long_name = "projection y coordinate"

    var_out_name = safe_var_name(wrf_var)
    units = output_units(wrf_var, example_file=example_file)

    var_attrs = {
        "description": f"{stat} summary of {wrf_var}",
        "wrf_var": wrf_var,
        "summary_stat": stat,
        "domain_tag": domain_tag,
        "start_date": start_date,
        "end_date": end_date,
    }

    if units:
        var_attrs["units"] = units

    ds_out = xr.Dataset(
        data_vars={
            var_out_name: (
                ("y", "x"),
                np.asarray(summary, dtype="float32"),
                var_attrs,
            ),
            "XLAT": (
                ("y", "x"),
                lat_np,
                {
                    "description": "latitude",
                    "units": "degrees_north",
                },
            ),
            "XLONG": (
                ("y", "x"),
                lon_np,
                {
                    "description": "longitude",
                    "units": "degrees_east",
                },
            ),
        },
        coords={
            "x": (
                "x",
                x_coord,
                {
                    "standard_name": "projection_x_coordinate",
                    "long_name": x_long_name,
                    "units": x_units,
                    "axis": "X",
                },
            ),
            "y": (
                "y",
                y_coord,
                {
                    "standard_name": "projection_y_coordinate",
                    "long_name": y_long_name,
                    "units": y_units,
                    "axis": "Y",
                },
            ),
        },
        attrs={
            "source": "WRF QAQC summary",
            "domain_tag": domain_tag,
            "wrf_var": wrf_var,
            "summary_stat": stat,
            "start_date": start_date,
            "end_date": end_date,
            "end_date_is_inclusive": "true",
            "n_source_files": len(wrf_files),
            "source_files": ";".join(wrf_files),
            "processing_method": "file-wise streaming",
            "wrf_projection_proj4": proj4,
        },
    )

    ds_out = add_grid_mapping_attrs(
        ds_out=ds_out,
        var_out_name=var_out_name,
        proj4=proj4,
    )

    return ds_out

def write_summary_nc(
    out_file,
    summary,
    example_file,
    domain_tag,
    wrf_var,
    stat,
    start_date,
    end_date,
    wrf_files,
):
    out_file = Path(out_file)
    out_file.parent.mkdir(parents=True, exist_ok=True)

    ds_out = make_output_dataset(
        summary=summary,
        example_file=example_file,
        domain_tag=domain_tag,
        wrf_var=wrf_var,
        stat=stat,
        start_date=start_date,
        end_date=end_date,
        wrf_files=wrf_files,
    )

    var_out_name = safe_var_name(wrf_var)

    encoding = {
        var_out_name: {
            "zlib": True,
            "complevel": NETCDF_COMPLEVEL,
            "_FillValue": np.float32(np.nan),
        },
        "XLAT": {
            "zlib": True,
            "complevel": NETCDF_COMPLEVEL,
        },
        "XLONG": {
            "zlib": True,
            "complevel": NETCDF_COMPLEVEL,
        },
        "x": {
            "zlib": False,
        },
        "y": {
            "zlib": False,
        },
        "crs": {
            "zlib": False,
        },
    }

    ds_out.to_netcdf(
        out_file,
        engine="netcdf4",
        encoding=encoding,
    )


# =============================================================================
# Orchestration
# =============================================================================


def summarise_streaming_group(
    wrf_files,
    summary_dir,
    domain_tag,
    start_date,
    end_date,
    wrf_var,
    stats,
):
    """Calculate mean/min/max for one variable and write one NetCDF per stat."""

    t0 = time.time()

    stats = [normalise_stat_name(stat) for stat in stats]

    stats_to_compute = []

    for stat in stats:
        out_file = output_path(
            summary_dir, domain_tag, wrf_var, stat, start_date, end_date
        )

        if out_file.exists() and not OVERWRITE:
            log(f"Skipping existing file: {out_file}")
        else:
            stats_to_compute.append(stat)

    if not stats_to_compute:
        log(f"All streaming summaries already exist for {wrf_var}; skipping.")
        return

    log("=" * 80)
    log(
        f"Summarising {wrf_var} using file-wise streaming stats: "
        f"{', '.join(stats_to_compute)}"
    )

    summaries, example_file = summarise_streaming_multi_filewise(
        wrf_files=wrf_files,
        wrf_var=wrf_var,
        start_date=start_date,
        end_date=end_date,
        stats=stats_to_compute,
    )

    for stat, summary in summaries.items():
        out_file = output_path(
            summary_dir, domain_tag, wrf_var, stat, start_date, end_date
        )

        t_write0 = time.time()

        write_summary_nc(
            out_file=out_file,
            summary=summary,
            example_file=example_file,
            domain_tag=domain_tag,
            wrf_var=wrf_var,
            stat=stat,
            start_date=start_date,
            end_date=end_date,
            wrf_files=wrf_files,
        )

        log(
            f"Wrote: {out_file} "
            f"(write time: {fmt_elapsed(time.time() - t_write0)})"
        )

    log(
        f"Completed streaming summaries for {wrf_var}: "
        f"{', '.join(stats_to_compute)} "
        f"in {fmt_elapsed(time.time() - t0)}"
    )


def summarise_percentile_one(
    wrf_files,
    summary_dir,
    domain_tag,
    start_date,
    end_date,
    wrf_var,
    stat,
):
    """Calculate one median/pXX summary for one variable and write one NetCDF."""

    t0 = time.time()
    stat = normalise_stat_name(stat)

    out_file = output_path(
        summary_dir, domain_tag, wrf_var, stat, start_date, end_date
    )

    if out_file.exists() and not OVERWRITE:
        log(f"Skipping existing file: {out_file}")
        return

    log("=" * 80)
    log(f"Summarising {wrf_var} using file-wise percentile stat: {stat}")
    log(f"Output: {out_file}")

    tmp_dir = TMP_DIR
    if tmp_dir is None:
        tmp_dir = summary_dir / "_tmp"

    summary, example_file = summarise_percentile_memmap_filewise(
        wrf_files=wrf_files,
        wrf_var=wrf_var,
        start_date=start_date,
        end_date=end_date,
        stat=stat,
        tmp_dir=tmp_dir,
    )

    t_write0 = time.time()

    write_summary_nc(
        out_file=out_file,
        summary=summary,
        example_file=example_file,
        domain_tag=domain_tag,
        wrf_var=wrf_var,
        stat=stat,
        start_date=start_date,
        end_date=end_date,
        wrf_files=wrf_files,
    )

    log(
        f"Wrote: {out_file} "
        f"(write time: {fmt_elapsed(time.time() - t_write0)}, "
        f"total: {fmt_elapsed(time.time() - t0)})"
    )


def summarise_precip_one(
    wrf_files,
    summary_dir,
    domain_tag,
    start_date,
    end_date,
):
    """Calculate accumulated precipitation and write one NetCDF."""

    t0 = time.time()
    wrf_var = "precip"
    stat = "accum"

    out_file = output_path(
        summary_dir, domain_tag, wrf_var, stat, start_date, end_date
    )

    if out_file.exists() and not OVERWRITE:
        log(f"Skipping existing file: {out_file}")
        return

    log("=" * 80)
    log("Summarising precip using accum")
    log(f"Output: {out_file}")

    summary, example_file = summarise_precip_accum_filewise(
        wrf_files=wrf_files,
        start_date=start_date,
        end_date=end_date,
    )

    t_write0 = time.time()

    write_summary_nc(
        out_file=out_file,
        summary=summary,
        example_file=example_file,
        domain_tag=domain_tag,
        wrf_var=wrf_var,
        stat=stat,
        start_date=start_date,
        end_date=end_date,
        wrf_files=wrf_files,
    )

    log(
        f"Wrote: {out_file} "
        f"(write time: {fmt_elapsed(time.time() - t_write0)}, "
        f"total: {fmt_elapsed(time.time() - t0)})"
    )


# =============================================================================
# Validation and main
# =============================================================================


def validate_configuration():
    """Check configuration before opening WRF files."""

    if WRFOUT_DIR is None or str(WRFOUT_DIR).strip() == "":
        raise ValueError("WRFOUT_DIR is empty.")

    if not DOMAIN_TAGS:
        raise ValueError("DOMAIN_TAGS is empty.")
    
    if any(
        domain is None or not str(domain).strip()
        for domain in DOMAIN_TAGS
    ):
        raise ValueError("DOMAIN_TAGS contains an empty domain identifier.")

    if not WRF_SUMMARY_VARS:
        raise ValueError("WRF_SUMMARY_VARS is empty.")

    if not STATS:
        raise ValueError("STATS is empty.")

    if START_DATE is not None and str(START_DATE).strip() == "":
        raise ValueError("START_DATE is empty string. Use None or 'YYYY-MM-DD'.")

    if END_DATE is not None and str(END_DATE).strip() == "":
        raise ValueError("END_DATE is empty string. Use None or 'YYYY-MM-DD'.")

    if START_DATE is not None:
        try:
            np.datetime64(START_DATE)
        except Exception as exc:
            raise ValueError(
                f"Invalid START_DATE '{START_DATE}'. Expected 'YYYY-MM-DD' or None."
            ) from exc

    if END_DATE is not None:
        try:
            np.datetime64(END_DATE)
        except Exception as exc:
            raise ValueError(
                f"Invalid END_DATE '{END_DATE}'. Expected 'YYYY-MM-DD' or None."
            ) from exc


def main():
    log(f"Python executable: {sys.executable}")

    apply_qaqc_user_input()
    validate_configuration()

    summary_dir = resolve_summary_dir()
    summary_dir.mkdir(parents=True, exist_ok=True)

    streaming_stats, percentile_stats = split_stats(STATS)

    log("Final WRF summary configuration:")
    log(f"  WRFOUT_DIR           = {WRFOUT_DIR}")
    log(f"  DOMAIN_TAGS          = {DOMAIN_TAGS}")
    log(f"  WRF_SUMMARY_BASE_DIR = {WRF_SUMMARY_BASE_DIR}")
    log(f"  WRF_SUMMARY_SUBDIR   = {WRF_SUMMARY_SUBDIR}")
    log(f"  summary_dir          = {summary_dir}")
    log(f"  WRF_SUMMARY_VARS     = {WRF_SUMMARY_VARS}")
    log(f"  STATS                = {STATS}")
    log(f"  START_DATE           = {START_DATE}")
    log(f"  END_DATE             = {END_DATE}")

    log(
        "Requested non-precip stats: "
        f"streaming={streaming_stats}, percentile={percentile_stats}"
    )

    for domain_tag in DOMAIN_TAGS:
        log(f"Processing WRF domain: {domain_tag}")

        wrf_files = find_wrf_files(
            wrfout_dir=WRFOUT_DIR,
            domain_tag=domain_tag,
            file_glob=WRFOUT_FILE_GLOB,
        )

        times = extract_times_from_files(wrf_files)

        log(f"  WRF files found = {len(wrf_files)}")
        log(f"  First WRF file  = {wrf_files[0]}")
        log(f"  Last WRF file   = {wrf_files[-1]}")
        log(f"  First timestamp = {times[0]}")
        log(f"  Last timestamp  = {times[-1]}")

        start_date, end_date = resolve_summary_dates(
            times=times,
            start_date=START_DATE,
            end_date=END_DATE,
        )

        time_indices = select_time_indices(
            times=times,
            start_date=start_date,
            end_date=end_date,
        )

        log(f"  START_DATE = {start_date}")
        log(f"  END_DATE   = {end_date}")

        log(
            f"Selected {len(time_indices)} timestamps for normal stats "
            f"from {times[time_indices[0]]} to {times[time_indices[-1]]}"
        )

        for wrf_var in WRF_SUMMARY_VARS:
            if wrf_var == "precip":
                summarise_precip_one(
                    wrf_files=wrf_files,
                    summary_dir=summary_dir,
                    domain_tag=domain_tag,
                    start_date=start_date,
                    end_date=end_date,
                )
                continue
        
            if streaming_stats:
                summarise_streaming_group(
                    wrf_files=wrf_files,
                    summary_dir=summary_dir,
                    domain_tag=domain_tag,
                    start_date=start_date,
                    end_date=end_date,
                    wrf_var=wrf_var,
                    stats=streaming_stats,
                )
        
            for stat in percentile_stats:
                summarise_percentile_one(
                    wrf_files=wrf_files,
                    summary_dir=summary_dir,
                    domain_tag=domain_tag,
                    start_date=start_date,
                    end_date=end_date,
                    wrf_var=wrf_var,
                    stat=stat,
                )

        log(f"Completed WRF domain: {domain_tag}")

    log("All requested WRF summaries completed.")

if __name__ == "__main__":
    main()
