
# USER INPUT --------------------------------------------------------------

# =============================================================================
# General configuration
# =============================================================================

# Year of observations to process.
YEAR = 2021

# Directory for processed observational data.
OBS_REPO = NULL

# =============================================================================
# Region selection
# =============================================================================

# Supply either a WRF output file or a geofile to constrain station selection.
# Set the unused option to NULL.

WRF_FILE = NULL
REGION_FILE = NULL
# =============================================================================
# GHCNh processing configuration
# =============================================================================

# Preferred report type for the meteorological observations.
# FM12 corresponds to SYNOP reports and is generally preferred because
# observations are normally reported at the top of the hour.
GHCNH_PREFERRED_REPORT_TYPE = 'FM12'
GHCNH_PRECIP_REPORT_TYPES = c('FM12')

# Meteorological variables to retain and process.
# Names are the variable names used in the QAQC repository.
# Values are the corresponding variables in the GHCNh data.
# !!!Please only comment out unwanted variables to preserve the mapping here!!!
# Relative humidity may be reported directly or derived from air temperature
# and dew point, depending on the data source.
GHCNH_MET_VARS = c(
  wd        = 'wd',          # wind direction
  ws        = 'ws',          # wind speed
  T2        = 'air_temp',    # air temperature at 2 m
  td2       = 'dew_point',   # dew-point temperature
  PSFC      = 'atmos_pres',  # surface pressure
  rh2        = 'rh'           # relative humidity
  
  # slp      = 'sea_pres',   # sea-level pressure
  # twb = 'wet_bulb',        # wet-bulb temperature - not implemented yet
  # wg       = 'wg'          # wind gust - not implemented yet
)

# Whether observations away from the top of the hour may be assigned to their
# nearest clock hour. If FALSE, only exact top-of-hour observations are retained;
# the missing hours will later appear as NA when the full hourly series is built.
GHCNH_ALLOW_OFF_HOUR_OBS = TRUE

# How to resolve an exact tie when equally close observations occur before and
# after the target hour:
# 'before' = retain the earlier observation
# 'after'  = retain the later observation
# 'mean'   = average the two observations using openair::timeAverage()
GHCNH_HOURLY_TIE_METHOD = 'before'

# QC statuses that are considered acceptable for the standard meteorological
# variables. Observations with any other QC status are replaced with NA.
GHCNH_MET_KEEP_QC_STATUS = c(
  'passed'
  # 'passed_gross',
  # 'accepted',
  # 'calculated',
  # 'edited',
  # 'inserted',
  # 'missing_qc'
  # 'suspect',
  # 'flagged',
  # 'erroneous',
  # 'removed',
  # 'not_checked',
  # 'unknown'
)

# QC statuses that are considered acceptable for precipitation records.
# Precipitation processing is currently more conservative than the processing
# of the other variables, so only fully passed records or missing_qc are retained.
# !!!WIP there is discrepancy between official GHCNH documentation and the
# qc flags in the worldmet download.
GHCNH_PRECIP_KEEP_QC_STATUS = c(
  'passed'
)

# Format of the processed precipitation output.
# hourly precip data are always output when available. If they are not select 
# the preferred format of the aggregated data
# 'aggregated' retains the cleaned accumulation records at their original
# reporting periods.
# 'disaggregated_hourly' distributes each retained accumulation evenly across
# its reporting period.
GHCNH_PRECIP_OUTPUT = c(
  'aggregated'
  #'disaggregated_hourly'
)

# Maximum acceptable same-length precipitation messiness index.
# The index is calculated separately for each accumulation period as:
#
# same-length overlapping window conflicts / number of QC-passed records
#
# A value of 0 requires an accumulation period to contain no overlapping
# same-length reporting windows.
GHCNH_SAME_LEN_MESSINESS_THRESHOLD = 0.1

# Whether to remove an entire precipitation accumulation period when its
# same-length messiness index exceeds GHCNH_SAME_LEN_MESSINESS_THRESHOLD.
#
# The messiness index is calculated separately for each accumulation period
# (for example 6, 12 and 24 hours) and measures how often records with the same
# accumulation length overlap one another.
#
# If TRUE, any accumulation period that exceeds the threshold is excluded
# before the remaining precipitation records are cleaned from longest to
# shortest accumulation period. Cleaner accumulation periods from the same
# station can still be retained.
#
# If FALSE, no accumulation periods are removed automatically, even when they
# exceed the threshold. The problematic periods will remain in the data and
# should be identified from the precipitation diagnostics.
GHCNH_REMOVE_MESSY_ACCUM_PERIODS = TRUE

# Whether processing should stop for a station when the precipitation field
# appears to use precipitation-specific measurement codes rather than the
# worldmet/GHCNh-style QC codes expected by the current workflow.
#
# Precipitation-specific codes can represent meanings such as trace amounts,
# estimated values, incomplete accumulations, or the start/end of accumulated,
# deleted or missing periods. These codes cannot currently be interpreted
# safely using the general QC lookup tables used for the other variables.
#
# TRUE causes processing of that station to fail and records the reason in the
# processing log, preventing incorrectly interpreted precipitation data from
# being saved.
#
# FALSE would allow processing to continue even though the precipitation QC
# meanings are not handled correctly. This is not currently safe and should not
# be used until explicit support for the precipitation-specific code system has
# been implemented.
#
# Must remain TRUE for now.
GHCNH_STOP_ON_PRECIP_SPECIFIC_QC = TRUE

# Whether processing should stop when the precipitation QC code system remains
# ambiguous after inspection of the codes present in the station dataset.
#
# Ambiguity would occur when only codes such as 1 and 2 are present, because
# those values can have different meanings under different precipitation code
# systems. 
# The QC system is assumed to be the worldmet/GHCNh-style system when:
# - all observed precipitation QC codes are within 1 to 5;
# - code 1 accounts for at least 50% of the non-missing QC flags.
#
# If these conditions are not met, but no clearly precipitation-specific codes
# are present, the system is classified as ambiguous.
#
# If TRUE, any station with an ambiguous precipitation QC system is rejected and
# the reason is written to the processing log.
#
# If FALSE, processing continues using the assumed worldmet/GHCNh-style QC
# interpretation. The assumption is recorded in the precipitation summary so
# that ambiguous stations can be reviewed later.
#
# FALSE is less conservative but avoids rejecting sites where the available
# evidence strongly suggests that the standard worldmet/GHCNh QC mapping is
# being used.
GHCNH_STOP_ON_AMBIGUOUS_PRECIP_QC = FALSE

# Whether the full interpreted precipitation QC table should be included in
# each station's saved RDS file.
# This can be large and is normally only useful for debugging.
GHCNH_RETURN_PRECIP_QC = FALSE

# Whether the consolidated precipitation table should be included in each
# station's saved RDS file.
# This contains QC-passed accumulation records before removal of messy periods
# and overlapping windows.
GHCNH_RETURN_PRECIP_CONSOLIDATED = FALSE

GHCNH_RETURN_PRECIP_HOURLY_SELECTED_RECORDS = FALSE

suppressPackageStartupMessages({
  library(reticulate)
  library(tidyverse)
  library(fs)
  library(sf)
  library(furrr)
  library(future)
  library(parallelly)
  library(checkmate)
  library(worldmet)
  library(openair)
  library(rvest)
  library(logger)
  library(glue)
})

source('/home/tomasliska/QAQC_SOURCE_CODE/utils/utils.R')
source('/home/tomasliska/QAQC_SOURCE_CODE/utils/GHCN_met_data_process_funcs.R')

platform = runtime_platform()

Sys.unsetenv(c('PYTHONPATH', 'PYTHONHOME'))

Sys.setenv(
  PYTHONNOUSERSITE = '1',
  OMP_NUM_THREADS = '1',
  MKL_NUM_THREADS = '1',
  OPENBLAS_NUM_THREADS = '1',
  HDF5_USE_FILE_LOCKING = 'FALSE'
)

if (!exists('WRF_QAQC_PYTHON') ||
    is.null(WRF_QAQC_PYTHON) ||
    is.na(WRF_QAQC_PYTHON) ||
    WRF_QAQC_PYTHON == '') {
  stop(glue::glue('Please set WRF_QAQC_PYTHON in {USER_INPUT_FILE}.'))
}

WRF_QAQC_PYTHON = fs::path_expand(WRF_QAQC_PYTHON)

if (!fs::file_exists(WRF_QAQC_PYTHON)) {
  stop('Python executable does not exist: ', WRF_QAQC_PYTHON)
}

Sys.setenv(
  RETICULATE_PYTHON = WRF_QAQC_PYTHON,
  RETICULATE_PYTHON_FALLBACK = 'FALSE'
)

if (platform %in% c('slurm-login', 'slurm-compute')) {
  
  qaqc_env_lib = fs::path(
    fs::path_dir(fs::path_dir(WRF_QAQC_PYTHON)),
    'lib'
  )
  
  Sys.setenv(
    LD_LIBRARY_PATH = paste(
      qaqc_env_lib,
      Sys.getenv('LD_LIBRARY_PATH'),
      sep = ':'
    )
  )
}

reticulate::use_python(
  WRF_QAQC_PYTHON,
  required = TRUE
)

workers = dplyr::case_when(
  platform == 'slurm-login' ~ min(2, parallelly::availableCores()),
  platform == 'slurm-compute' ~ parallelly::availableCores(),
  platform %in% c('mac', 'windows') ~ min(4, parallelly::availableCores()),
  TRUE ~ min(2, parallelly::availableCores())
)

wrf = tryCatch(reticulate::import('wrf'), error = function(e) NULL)
np = tryCatch(reticulate::import('numpy'), error = function(e) NULL)
ncpy = tryCatch(reticulate::import('netCDF4'), error = function(e) NULL)
xr = tryCatch(reticulate::import('xarray'), error = function(e) NULL)
ccrs = tryCatch(reticulate::import('cartopy.crs'), error = function(e) NULL)
metpy = tryCatch(reticulate::import('metpy'), error = function(e) NULL)
py_builtins = tryCatch(reticulate::import_builtins(), error = function(e) NULL)

py_modules = list(
  wrf = wrf,
  numpy = np,
  netCDF4 = ncpy,
  xarray = xr,
  cartopy_crs = ccrs,
  metpy = metpy,
  builtins = py_builtins
)

missing_py_modules = py_modules %>%
  purrr::keep(is.null) %>%
  names()

if (length(missing_py_modules) > 0) {
  stop(
    glue::glue(
      'Required Python modules are not available: ',
      '{paste(missing_py_modules, collapse = ", ")}'
    )
  )
}

# quality control flag definition -----------------------------------------

ghcnh_integrated_qc_flags = tibble::tribble(
  ~qc, ~qc_meaning,
  'a', 'Repeated day streaks',
  'b', 'Distribution - all',
  'c', 'Climatological outlier',
  'd', 'Distribution - monthly',
  'e', 'Clean up',
  'f', 'Frequent value',
  'h', 'High flag rate',
  'i', 'Precision',
  'k', 'Repeating streaks',
  'l', 'Failed logistical consistency',
  'm', 'Humidity',
  'n', 'Neighbour comparison outlier',
  'o', 'Odd cluster',
  'p', 'Pressure',
  'r', 'World records exceedance',
  's', 'Spike check',
  't', 'Timestamp issue',
  'u', 'Diurnal inconsistency check',
  'v', 'Variance',
  'w', 'Winds',
  'x', 'Excess streak proportion',
  'z', 'Wind logical - calm masked zero direction'
) %>%
  dplyr::mutate(
    qc_system = 'integrated_ghcnh',
    qc_status = 'flagged'
  )

ghcnh_legacy_qc_flags_313_346 = tibble::tribble(
  ~qc, ~qc_meaning, ~qc_status,
  '0', 'Passed gross limits check', 'passed_gross',
  '1', 'Passed all quality control checks', 'passed',
  '2', 'Suspect', 'suspect',
  '3', 'Erroneous', 'erroneous',
  '4', 'Passed gross limits check, data originate from an NCEI data source', 'passed_gross',
  '5', 'Passed all quality control checks, data originate from an NCEI data source', 'passed',
  '6', 'Suspect, data originate from an NCEI data source', 'suspect',
  '7', 'Erroneous, data originate from an NCEI data source', 'erroneous',
  '9', 'Passed gross limits check if element is present', 'passed_gross',
  'A', 'Data value flagged as suspect, but accepted as good value', 'accepted',
  'U', 'Data value replaced with edited value', 'edited',
  'P', 'Data value not originally flagged as suspect, but replaced by validator', 'edited',
  'I', 'Data value not originally in data, but inserted by validator', 'inserted',
  'M', 'Manual change made to value based on information provided by NWS or FAA', 'edited',
  'R', 'Data value replaced with value computed by NCEI software', 'edited'
) %>%
  dplyr::mutate(
    source_code_group = '313_314_315_322_335_343_344_346',
    source_code = list(c('313', '314', '315', '322', '335', '343', '344', '346'))
  ) %>%
  tidyr::unnest(source_code)

ghcnh_legacy_qc_flags_220_348 = tibble::tribble(
  ~qc, ~qc_meaning, ~qc_status,
  '0', 'Not checked', 'not_checked',
  '1', 'Good', 'passed',
  '2', 'Suspect', 'suspect',
  '3', 'Erroneous', 'erroneous',
  '4', 'Calculated', 'calculated',
  '5', 'Removed', 'removed'
) %>%
  dplyr::mutate(
    source_code_group = '220_221_222_223_347_348',
    source_code = list(c('220', '221', '222', '223', '347', '348'))
  ) %>%
  tidyr::unnest(source_code)


ghcnh_derived_qc_flags = tibble::tribble(
  ~qc, ~qc_meaning, ~qc_status,
  'o', 'Out of range, relative humidity only, values < 1 or > 100', 'erroneous',
  'f', 'Suspect or error flags for one or more input measurements', 'suspect'
) %>%
  dplyr::mutate(
    qc_system = 'derived_variable'
  )

ghcnh_qc_status_rank = tibble::tribble(
  ~qc_status, ~qc_rank,
  'passed', 1L,
  'passed_gross', 2L,
  'accepted', 2L,
  'calculated', 2L,
  'edited', 3L,
  'inserted', 3L,
  'missing_qc', 4L,
  'input_qc_required', 4L,
  'suspect', 5L,
  'flagged', 5L,
  'erroneous', 6L,
  'removed', 6L,
  'not_checked', 7L,
  'unknown', 7L
)

ghcnh_legacy_qc_flags = dplyr::bind_rows(
  ghcnh_legacy_qc_flags_313_346,
  ghcnh_legacy_qc_flags_220_348
)

# VALIDATE REGION INPUTS --------------------------------------------------

# Exactly one region selection method must be provided.
if (is.null(WRF_FILE) == is.null(REGION_FILE)) {
  stop("Exactly one of WRF_FILE or REGION_FILE must be provided.")
}

if (!is.null(WRF_FILE)) {
  
  checkmate::assert_file_exists(WRF_FILE)
  
  # Check that the file can be opened as a NetCDF dataset.
  wrf_nc = tryCatch(
    ncpy$Dataset(WRF_FILE),
    error = function(e) {
      stop("WRF_FILE is not a readable NetCDF file: ", conditionMessage(e))
    }
  )
  
  wrf_nc$close()
  
} else {
  
  checkmate::assert_file_exists(REGION_FILE)
  
  # Read and validate the geographical region.
  region = tryCatch(
    sf::st_read(REGION_FILE, quiet = TRUE),
    error = function(e) {
      stop("REGION_FILE could not be read as geospatial data: ",
           conditionMessage(e))
    }
  )
  
  if (
    nrow(region) == 0 ||
    !all(sf::st_geometry_type(region) %in% c('POLYGON', 'MULTIPOLYGON')) ||
    any(!sf::st_is_valid(region)) ||
    is.na(sf::st_crs(region))
  ) {
    stop("REGION_FILE must contain valid polygon geometries with a defined CRS.")
  }
}

# MAIN --------------------------------------------------------------------

# Use the innermost WRF domain to identify available GHCNh stations.
wrf_domain = unname(tail(TEST_DOMAINS, 1))

wrf_files = TEST_DIR %>%
  fs::dir_ls() %>%
  stringr::str_subset(
    stringr::str_c('wrfout_', wrf_domain, '_')
  ) %>%
  sort()

if (length(wrf_files) == 0) {
  stop(
    glue::glue(
      "No WRF output files found for domain '{wrf_domain}' in {TEST_DIR}."
    )
  )
}

# Extract the simulation year from the first WRF output file.
wrf_nc = ncpy$Dataset(wrf_files[[1]])

wrf_times = wrf$extract_times(
  wrf_nc,
  timeidx = 0L,
  meta = FALSE
)

YEAR = as.integer(
  format(
    as.POSIXct(
      reticulate::py_to_r(wrf_times),
      tz = 'UTC'
    ),
    '%Y'
  )
)

logger::log_info('GHCNh processing year: {YEAR}')

met_sites = import_ghcn_stations(database = 'hourly', return = 'sf')

#extract model domain x,y dimension sizes
sn_dim = wrf_nc$dimensions$south_north$size
we_dim = wrf_nc$dimensions$west_east$size

lon = intersect(c('longitude', 'lon', 'long', 'lng'), names(met_sites))
lat = intersect(c('latitude', 'lat'), names(met_sites))
code = intersect(c('station_id', 'id', 'code'), names(met_sites))
met_sites = rename(met_sites, code = !!code)

#extract site indeces and filter out those outside the model domain

met_indeces = wrf$ll_to_xy(wrf_nc, latitude = met_sites[[lat]], longitude = met_sites[[lon]], meta = F)
met_indeces = as_tibble(met_indeces, .name_repair = ~met_sites$code) %>% 
  mutate(index = c('i', 'j')) %>% 
  pivot_longer(-index, names_to = 'code', values_to = 'val') %>%
  pivot_wider(id_cols = code, names_from = index, values_from = val) %>% 
  filter(between(i, 0, we_dim - 1),between(j, 0, sn_dim - 1)) # python is 0-start index

met_sites_in = met_sites %>% 
  filter(code %in% met_indeces$code) %>%
  left_join(met_indeces, by = 'code')

wrf_nc$close()

met_sites_in = met_sites_in %>%
  dplyr::mutate(
    station_name_file = sanitise_string(
      .data[['name']],
      case = 'title',
      separator = '_'
    ),
    output_pth = fs::path(
      OBS_REPO,
      'Data',
      paste0(
        station_name_file,
        '_',
        code,
        '_',
        YEAR,
        '_ghcnh_processed.rds'
      )
    )
  )

# # OPTIONAL: DOWNSAMPLE SITES BEFORE DOWNLOADING DATA IF DOMAIN TOO LARGE
# cat("Number of stations in domain: ", nrow(met_sites_in), "\n")
# 
# # Optional safety warning for very large domains
# if (nrow(met_sites_in) > 300) {
#   warning("Large domain detected ? downloading all sites may take a long time.")
# }
# 
# do_downsample = readline("Downsample BEFORE downloading raw data? (y/n): ")
# 
# if (stringr::str_to_lower(stringr::str_trim(do_downsample)) %in% c('y', 'yes')) {
#   for (s in seq_along(SITES_SAMPLING_TYPE)) {
#     met_sites_in =
#       met_sites_in %>%
#       sample_sites(
#         type = SITES_SAMPLING_TYPE[[s]],
#         selector = SITES_SAMPLING_SELECTOR[[s]],
#         n = N_SITES[[s]]
#       )
#   }
#   message("Downsampling applied.")
# } else {
#   message("Skipping downsampling - all sites will be downloaded.")
# }

# obs data download and processing ----------------------------------------

future::plan(
  future::multisession,
  workers = workers
)

ghcnh_processing_results = furrr::future_pmap(
  list(
    station = met_sites_in$code,
    output_pth = met_sites_in$output_pth
  ),
  function(station, output_pth) {
    
    import_process_save_ghcnh_one_station_safe(
      station = station,
      year = YEAR,
      output_pth = output_pth,
      preferred_report_type = GHCNH_PREFERRED_REPORT_TYPE,
      precip_report_types = GHCNH_PRECIP_REPORT_TYPES,
      met_vars = GHCNH_MET_VARS,
      allow_off_hour_obs = GHCNH_ALLOW_OFF_HOUR_OBS,
      hourly_tie_method = GHCNH_HOURLY_TIE_METHOD,
      met_keep_qc_status = GHCNH_MET_KEEP_QC_STATUS,
      precip_keep_qc_status = GHCNH_PRECIP_KEEP_QC_STATUS,
      precip_output = GHCNH_PRECIP_OUTPUT,
      same_len_messiness_threshold = GHCNH_SAME_LEN_MESSINESS_THRESHOLD,
      stop_on_precip_specific_qc = GHCNH_STOP_ON_PRECIP_SPECIFIC_QC,
      stop_on_ambiguous_precip_qc = GHCNH_STOP_ON_AMBIGUOUS_PRECIP_QC,
      remove_messy_accum_periods = GHCNH_REMOVE_MESSY_ACCUM_PERIODS,
      return_precip_qc = GHCNH_RETURN_PRECIP_QC,
      return_precip_consolidated = GHCNH_RETURN_PRECIP_CONSOLIDATED,
      return_precip_hourly_selected_records = GHCNH_RETURN_PRECIP_HOURLY_SELECTED_RECORDS
    )
  },
  .options = furrr::furrr_options(
    seed = TRUE,
    packages = c(
      'worldmet',
      'dplyr',
      'tidyr',
      'purrr',
      'stringr',
      'lubridate',
      'tibble',
      'fs',
      'readr'
    )
  )
)

future::plan(future::sequential)

ghcnh_processing_log = ghcnh_processing_results %>%
  purrr::map('processing_log') %>%
  dplyr::bind_rows()%>%
  dplyr::mutate(
    output_file = dplyr::if_else(
      !is.na(output_file) & fs::file_exists(output_file),
      output_file,
      NA_character_
    )
  )

ghcnh_precip_summary = ghcnh_processing_results %>%
  purrr::map('precip_summary') %>%
  purrr::compact() %>%
  dplyr::bind_rows()

ghcnh_data_capture_summary = ghcnh_processing_results %>%
  purrr::map('data_capture_summary') %>%
  purrr::compact() %>%
  dplyr::bind_rows()

ghcnh_hourly_selection_summary = ghcnh_processing_results %>%
  purrr::map('hourly_selection_summary') %>%
  purrr::compact() %>%
  dplyr::bind_rows()


processing_log_pth = fs::path(
  OBS_REPO,
  paste0(
    'GHCNh_processing_log',
    '_',
    YEAR,
    '.rds'
  )
)

precip_summary_pth = fs::path(
  OBS_REPO,
  paste0(
    'GHCNh_precip_summary',
    '_',
    YEAR,
    '.rds'
  )
)

data_capture_summary_pth = fs::path(
  OBS_REPO,
  paste0(
    'GHCNh_data_capture_summary',
    '_',
    YEAR,
    '.rds'
  )
)

hourly_selection_summary_pth = fs::path(
  OBS_REPO,
  'GHCNh_hourly_selection_summary.rds'
)

readr::write_rds(
  ghcnh_processing_log,
  processing_log_pth
)

readr::write_rds(
  ghcnh_precip_summary,
  precip_summary_pth
)

readr::write_rds(
  ghcnh_data_capture_summary,
  data_capture_summary_pth
)

readr::write_rds(
  ghcnh_hourly_selection_summary,
  hourly_selection_summary_pth
)

sites_available = met_sites_in %>%
  dplyr::semi_join(
    ghcnh_processing_log %>%
      dplyr::filter(
        status == 'processed',
        !is.na(output_file)
      ),
    by = 'code'
  ) %>%
  dplyr::select(
    -dplyr::any_of('station_name_file')
  )

sites_available_pth = fs::path(
  OBS_REPO,
  paste0(
    'Sites_available',
    '_',
    YEAR,
    '.rds'
  )
)

readr::write_rds(
  sites_available,
  sites_available_pth
)


# test --------------------------------------------------------------------

# heathrow_test = import_process_save_ghcnh_one_station(
#   station = 'UKI0000EGLL',
#   year = YEAR,
#   output_pth = NULL,
#   preferred_report_type = GHCNH_PREFERRED_REPORT_TYPE,
#   met_vars = GHCNH_MET_VARS,
#   allow_off_hour_obs = GHCNH_ALLOW_OFF_HOUR_OBS,
#   hourly_tie_method = GHCNH_HOURLY_TIE_METHOD,
#   met_keep_qc_status = GHCNH_MET_KEEP_QC_STATUS,
#   precip_keep_qc_status = GHCNH_PRECIP_KEEP_QC_STATUS,
#   precip_report_types = GHCNH_PRECIP_REPORT_TYPES,
#   precip_output = GHCNH_PRECIP_OUTPUT,
#   same_len_messiness_threshold = GHCNH_SAME_LEN_MESSINESS_THRESHOLD,
#   stop_on_precip_specific_qc = GHCNH_STOP_ON_PRECIP_SPECIFIC_QC,
#   stop_on_ambiguous_precip_qc = GHCNH_STOP_ON_AMBIGUOUS_PRECIP_QC,
#   remove_messy_accum_periods = GHCNH_REMOVE_MESSY_ACCUM_PERIODS,
#   return_precip_qc = TRUE,
#   return_precip_consolidated = GHCNH_RETURN_PRECIP_CONSOLIDATED,
#   return_precip_hourly_selected_records =
#     GHCNH_RETURN_PRECIP_HOURLY_SELECTED_RECORDS
# )
