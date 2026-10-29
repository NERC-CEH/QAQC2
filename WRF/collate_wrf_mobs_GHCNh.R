suppressPackageStartupMessages({
  library(reticulate)
  library(fs)
  library(glue)
  library(tidyverse)
  library(sf)
  library(furrr)
  library(checkmate)
  library(data.table)
  library(logger)
})

USER_INPUT_FILE = Sys.getenv(
  'WRF_QAQC_USER_INPUT',
  unset = 'wrf_qaqc_user_input.R'
)

if (is.na(USER_INPUT_FILE) || USER_INPUT_FILE == '') {
  stop('WRF_QAQC_USER_INPUT is empty.')
}

if (!fs::file_exists(USER_INPUT_FILE)) {
  stop(glue::glue('User input file not found: {USER_INPUT_FILE}'))
}

USER_INPUT_FILE = normalizePath(
  USER_INPUT_FILE,
  mustWork = TRUE
)

message(glue::glue('Using WRF QAQC user input file: {USER_INPUT_FILE}'))

source(USER_INPUT_FILE)

source('/home/tomasliska/QAQC_SOURCE_CODE/utils/emep_qaqc_funcs.R')
source('/home/tomasliska/QAQC_SOURCE_CODE/utils/utils.R')

platform = runtime_platform()

if (!platform %in% c('slurm-compute', 'slurm-login')) {
  stop(
    'collate_wrf_mobs.R is intended to run on the HPC only. ',
    'Current platform: ', platform
  )
}

# Use Python from the active Conda environment.
wrf_qaqc_python = unname(Sys.which('python'))

if (!nzchar(wrf_qaqc_python) || !fs::file_exists(wrf_qaqc_python)) {
  stop('Python executable not found in the active environment.')
}

Sys.unsetenv('PYTHONPATH')
Sys.unsetenv('PYTHONHOME')

Sys.setenv(
  PYTHONNOUSERSITE = '1',
  RETICULATE_PYTHON = wrf_qaqc_python,
  RETICULATE_PYTHON_FALLBACK = 'FALSE',
  OMP_NUM_THREADS = '1',
  MKL_NUM_THREADS = '1',
  OPENBLAS_NUM_THREADS = '1',
  HDF5_USE_FILE_LOCKING = 'FALSE'
)

qaqc_env_lib = fs::path(
  fs::path_dir(fs::path_dir(wrf_qaqc_python)),
  'lib'
)

Sys.setenv(
  LD_LIBRARY_PATH = paste(
    qaqc_env_lib,
    Sys.getenv('LD_LIBRARY_PATH'),
    sep = ':'
  )
)

reticulate::use_python(wrf_qaqc_python, required = TRUE)

workers = dplyr::case_when(
  platform == 'slurm-compute' ~ parallelly::availableCores(),
  platform == 'slurm-login' ~ min(2, parallelly::availableCores()),
  TRUE ~ NA_integer_
)

if (is.na(workers)) {
  stop('Unknown platform: please configure platform handling.')
}

collate_workers = min(8, workers)

wrf = reticulate::import('wrf')
ncpy = reticulate::import('netCDF4')
np = reticulate::import('numpy')

if (any(sapply(list(wrf, ncpy, np), is.null))) {
  stop('One or more required Python modules could not be imported.')
}

logger::log_info('Starting WRF MOBS collation')
logger::log_info('Platform: {platform}')
logger::log_info('Python: {wrf_qaqc_python}')
logger::log_info('Workers: {collate_workers}')

# INPUT FILES -------------------------------------------------------------

# Use the innermost Test domain for MOBS collation.
mobs_domain = unname(tail(TEST_DOMAINS, 1))

wrf_files = TEST_DIR %>%
  fs::dir_ls(regexp = stringr::str_c('^wrfout_', mobs_domain, '_')) %>%
  sort()

if (length(wrf_files) == 0) {
  stop(glue::glue(
    "No WRF files found in {TEST_DIR} matching wrfout_{mobs_domain}_*"
  ))
}

logger::log_info('Found {length(wrf_files)} WRF files')
logger::log_info('First WRF file: {wrf_files[[1]]}')
logger::log_info('Last WRF file: {wrf_files[[length(wrf_files)]]}')

obs_dir = fs::path(OBS_REPO)


checkmate::assert_directory_exists(obs_dir)

sites_available_pth = fs::path(
  obs_dir,
  paste0(
    'Sites_available_',
    RUNYEAR,
    '.rds'
  )
)

checkmate::assert_file_exists(sites_available_pth)

sites_available = readr::read_rds(
  sites_available_pth
)

checkmate::assert_names(
  names(sites_available),
  must.include = c(
    'code',
    'output_pth'
  )
)

selected_sites_source_pth = fs::path_abs(
  SELECTED_SITES_FILE
)

selected_sites_output_pth = fs::path_abs(
  fs::path(
    data_pth_out,
    fs::path_file(SELECTED_SITES_FILE)
  )
)

if (fs::file_exists(selected_sites_source_pth)) {
  
  if (selected_sites_source_pth != selected_sites_output_pth) {
    
    if (fs::file_exists(selected_sites_output_pth)) {
      stop(
        'A selected-sites file with the same name already exists in ',
        'the output directory: ',
        selected_sites_output_pth
      )
    }
    
    fs::file_move(
      selected_sites_source_pth,
      selected_sites_output_pth
    )
  }
  
} else if (!fs::file_exists(selected_sites_output_pth)) {
  
  stop(
    'File containing selected station codes was not found at either: ',
    selected_sites_source_pth,
    ' or ',
    selected_sites_output_pth
  )
}

# From this point onwards, always use the path in data_pth_out.
selected_sites_pth = selected_sites_output_pth

selected_site_codes = readr::read_lines(
  selected_sites_pth
) %>%
  stringr::str_remove('#.*$') %>%
  stringr::str_trim() %>%
  purrr::discard(~ .x == '') %>%
  unique()

if (length(selected_site_codes) == 0) {
  stop(
    'No station codes were found in: ',
    selected_sites_pth
  )
}


missing_site_codes = setdiff(
  selected_site_codes,
  sites_available$code
)

if (length(missing_site_codes) > 0) {
  stop(
    'The following selected station codes are not present in ',
    fs::path_file(sites_available_pth),
    ': ',
    paste(missing_site_codes, collapse = ', ')
  )
}

sites_geo = sites_available %>%
  dplyr::filter(code %in% selected_site_codes) %>%
  dplyr::arrange(code)

lat_name = intersect(c('latitude', 'lat'), names(sites_geo))
lon_name = intersect(c('longitude', 'lng', 'lon'), names(sites_geo))

sites_geo = sites_geo %>%
  dplyr::rename(latitude = all_of(lat_name),
                longitude = all_of(lon_name))

sites_geo = sites_geo %>%
  dplyr::select(
    -dplyr::any_of(c(
      'i',
      'j',
      'i_py',
      'j_py',
      'cell_id'
    ))
  )

sites_pth = fs::path(
  data_pth_out,
  '00Sites_used.rds'
)

# Use the paths to the processed observation files in the repository.
# The files themselves are not copied into the QAQC output directory.
obs_data_pths = sites_geo %>%
  sf::st_drop_geometry() %>%
  dplyr::pull(output_pth) %>%
  purrr::map_chr(
    resolve_obs_file_path,
    obs_repo = OBS_REPO
  )

missing_obs_files = obs_data_pths[
  !fs::file_exists(obs_data_pths)
]

if (length(missing_obs_files) > 0) {
  stop(
    'The following processed observation files do not exist: ',
    paste(missing_obs_files, collapse = ', ')
  )
}

if (length(obs_data_pths) == 0) {
  stop(
    'No processed observation files were selected from: ',
    obs_dir
  )
}

logger::log_info(
  'Selected {nrow(sites_geo)} stations with {length(obs_data_pths)} observation files'
)



# SITE INDICES ------------------------------------------------------------

logger::log_info('Extracting WRF i/j indices for selected sites')

ds_idx = ncpy$Dataset(wrf_files[[1]])

idx_py = wrf$ll_to_xy(
  ds_idx,
  latitude = sites_geo$latitude,
  longitude = sites_geo$longitude,
  meta = FALSE
)

ds_idx$close()

# Fallback helper for cases where reticulate cannot convert idx_py cleanly.
pymain = reticulate::import_main(convert = FALSE)

reticulate::py_run_string("
import numpy as np, netCDF4, wrf

def ll_to_ij_list(ds_path, lats, lons):
    ds = netCDF4.Dataset(ds_path)
    xy = wrf.ll_to_xy(ds, latitude=lats, longitude=lons, meta=False)
    ds.close()
    if isinstance(xy, (list, tuple)) and len(xy) == 2:
        x, y = xy
        arr = np.column_stack([x, y]).astype('int64')
    else:
        arr = np.asarray(xy)
        if arr.ndim == 1:
            arr = arr.reshape(-1, 2)
        elif arr.ndim == 2 and arr.shape[0] == 2 and arr.shape[1] != 2:
            arr = arr.T
        arr = arr.astype('int64')
    return arr.tolist()
")

if (inherits(idx_py, 'numpy.ndarray')) {
  ij_list = pymain$ll_to_ij_list(
    wrf_files[[1]],
    sites_geo$latitude,
    sites_geo$longitude
  )
  
  idx_py_mat = do.call(rbind, reticulate::py_to_r(ij_list))
  
} else {
  idx_py_mat = reticulate::py_to_r(idx_py)
  
  if (is.matrix(idx_py_mat) && nrow(idx_py_mat) == 2L) {
    idx_py_mat = t(idx_py_mat)
  }
}

idx_tibble = tibble::tibble(
  i_py = as.integer(idx_py_mat[, 1]),
  j_py = as.integer(idx_py_mat[, 2])
) %>%
  dplyr::mutate(
    i_py_nearest = i_py,
    j_py_nearest = j_py,
    coastal_land_correction = FALSE
  )

checkmate::assert_count(MAX_NDIST)

logger::log_info('Checking WRF land/water classification at selected sites')

ds_land = ncpy$Dataset(wrf_files[[1]])

xland = reticulate::py_to_r(
  ds_land$variables[['XLAND']]$`__getitem__`(reticulate::tuple(0L))
)

xlat = reticulate::py_to_r(
  ds_land$variables[['XLAT']]$`__getitem__`(reticulate::tuple(0L))
)

xlong = reticulate::py_to_r(
  ds_land$variables[['XLONG']]$`__getitem__`(reticulate::tuple(0L))
)

ds_land$close()

# WRF arrays have dimensions [south_north, west_east].
# In R these correspond to [j + 1, i + 1].

nearest_xland = xland[
  cbind(idx_tibble$j_py + 1L, idx_tibble$i_py + 1L)
]

idx_tibble$wrf_nearest_xland = nearest_xland

station_points = sf::st_as_sf(
  sites_geo,
  coords = c('longitude', 'latitude'),
  crs = 4326,
  remove = FALSE
)

nearest_coords = tibble::tibble(
  longitude = xlong[
    cbind(idx_tibble$j_py + 1L, idx_tibble$i_py + 1L)
  ],
  latitude = xlat[
    cbind(idx_tibble$j_py + 1L, idx_tibble$i_py + 1L)
  ]
)

nearest_points = sf::st_as_sf(
  nearest_coords,
  coords = c('longitude', 'latitude'),
  crs = 4326
)

idx_tibble$nearest_distance_km = as.numeric(
  sf::st_distance(
    station_points,
    nearest_points,
    by_element = TRUE
  )
) / 1000

idx_tibble$land_distance_km = NA_real_

if (isTRUE(MOBS_COASTAL_LAND_CORRECTION)) {
  
  marine_sites = which(idx_tibble$wrf_nearest_xland == 2)
  
  for (site_idx in marine_sites) {
    
    i0 = idx_tibble$i_py_nearest[site_idx]
    j0 = idx_tibble$j_py_nearest[site_idx]
    
    # Candidate cells within MAX_NDIST of the original cell.
    i_range = seq(
      max(0L, i0 - MAX_NDIST),
      min(ncol(xland) - 1L, i0 + MAX_NDIST)
    )
    
    j_range = seq(
      max(0L, j0 - MAX_NDIST),
      min(nrow(xland) - 1L, j0 + MAX_NDIST)
    )
    
    candidates = tidyr::expand_grid(
      i_py = i_range,
      j_py = j_range
    ) %>%
      dplyr::mutate(
        xland = xland[
          cbind(j_py + 1L, i_py + 1L)
        ]
      ) %>%
      dplyr::filter(xland == 1)
    
    if (nrow(candidates) == 0) {
      next
    }
    
    # Coordinates of candidate land-cell centres.
    candidates = candidates %>%
      dplyr::mutate(
        longitude = xlong[
          cbind(j_py + 1L, i_py + 1L)
        ],
        latitude = xlat[
          cbind(j_py + 1L, i_py + 1L)
        ]
      )
    
    candidate_points = sf::st_as_sf(
      candidates,
      coords = c('longitude', 'latitude'),
      crs = 4326,
      remove = FALSE
    )
    
    distances = as.numeric(
      sf::st_distance(
        station_points[site_idx, ],
        candidate_points
      )
    ) / 1000
    
    nearest_land_idx = which.min(distances)
    
    idx_tibble$i_py[site_idx] =
      candidates$i_py[nearest_land_idx]
    
    idx_tibble$j_py[site_idx] =
      candidates$j_py[nearest_land_idx]
    
    idx_tibble$land_distance_km[site_idx] =
      distances[nearest_land_idx]
    
    idx_tibble$coastal_land_correction[site_idx] = TRUE
  }
  
  logger::log_info(
    'Applied coastal land correction to {sum(idx_tibble$coastal_land_correction)} stations'
  )
}

idx_tibble = idx_tibble %>%
  dplyr::mutate(
    site_type = dplyr::case_when(
      wrf_nearest_xland == 1 ~ 'land',
      coastal_land_correction ~ 'coastal',
      TRUE ~ 'marine'
    ),
    extraction_distance_km = dplyr::if_else(
      coastal_land_correction,
      land_distance_km,
      nearest_distance_km
    )
  )

sites_geo = sites_geo %>%
  dplyr::bind_cols(idx_tibble) %>%
  dplyr::mutate(
    cell_id = stringr::str_glue('{i_py}_{j_py}')
  )

if (!isTRUE(MOBS_INCLUDE_MARINE_SITES)) {
  
  n_marine = sum(sites_geo$site_type == 'marine')
  
  sites_geo = sites_geo %>%
    dplyr::filter(site_type != 'marine')
  
  logger::log_info(
    'Excluded {n_marine} marine stations from MOBS collation'
  )
}

if (nrow(sites_geo) == 0) {
  stop('No stations remain after applying marine-site filtering.')
}

obs_data_pths = sites_geo %>%
  sf::st_drop_geometry() %>%
  dplyr::pull(output_pth) %>%
  purrr::map_chr(
    resolve_obs_file_path,
    obs_repo = OBS_REPO
  )

readr::write_rds(sites_geo, sites_pth)

paths = wrf_files
wrf_var_names = WRF_EXTRACT_VARS
codes = sites_geo$code
i_idx = idx_tibble$i_py
j_idx = idx_tibble$j_py


# EXTRACT WRF DATA --------------------------------------------------------

logger::log_info('Extracting WRF variables at selected sites')

future::plan(future::multisession, workers = collate_workers)

extract_opts = furrr::furrr_options(
  seed = TRUE,
  scheduling = Inf,
  packages = unique(c(
    'reticulate',
    'dplyr',
    'tibble',
    'tidyr',
    'checkmate',
    'purrr'
  )),
  globals = c(
    'extract_wrf_vars_from_file',
    'wrf_var_names',
    'codes',
    'i_idx',
    'j_idx'
  )
)

res_safed = furrr::future_map(
  paths,
  function(p) {
    purrr::safely(extract_wrf_vars_from_file)(
      wrf_file_pth = p,
      var_names = wrf_var_names,
      code = codes,
      i_idx = i_idx,
      j_idx = j_idx,
      index_level = NULL
    )
  },
  .options = extract_opts
)

future::plan(future::sequential)

res_split = purrr::transpose(res_safed)
errs = res_split$error
oks = res_split$result

fail_idx = which(!purrr::map_lgl(errs, is.null))

if (length(fail_idx)) {
  fail_tbl = tibble::tibble(
    wrf_file = paths[pmin(fail_idx, length(paths))],
    msg = purrr::map_chr(errs[fail_idx], conditionMessage)
  )
  
  print(fail_tbl, n = Inf)
  
  readr::write_csv(
    fail_tbl,
    fs::path(data_pth_out, 'wrf_mobs_extraction_failures.csv')
  )
}

wrf_long_all = oks %>% 
  purrr::compact()

wrf_long_all = if (length(wrf_long_all)) {
  dplyr::bind_rows(wrf_long_all)
} else {
  tibble::tibble()
}

if (nrow(wrf_long_all) == 0) {
  stop('No WRF data were extracted. Check wrf_mobs_extraction_failures.csv if present.')
}

wrf_long_all = wrf_long_all %>% 
  dplyr::arrange(code, date)

wrf_data_list = wrf_long_all %>% 
  split(.$code)


# PRECIPITATION -----------------------------------------------------------

logger::log_info('Calculating WRF precipitation')

future::plan(future::multisession, workers = collate_workers)

precip_opts = furrr::furrr_options(
  seed = TRUE,
  scheduling = Inf,
  packages = c(
    'dplyr',
    'tidyr',
    'purrr',
    'tibble',
    'lubridate'
  ),
  globals = c('calculate_wrf_precip')
)

wrf_precip = wrf_data_list %>% 
  furrr::future_map(
    calculate_wrf_precip,
    rate = TRUE,
    first_step = 'zero',
    .options = precip_opts
  ) %>% 
  dplyr::bind_rows()

future::plan(future::sequential)

wrf_data_list = wrf_data_list %>% 
  dplyr::bind_rows() %>%
  dplyr::filter(!var %in% c('RAINNC', 'RAINC')) %>%
  dplyr::filter(var != 'precip') %>%
  dplyr::bind_rows(wrf_precip) %>% 
  dplyr::mutate(scenario = 'mod') %>% 
  dplyr::arrange(code, .data$date) %>% 
  dplyr::relocate(file_id, .after = dplyr::last_col()) %>%
  split(.$code)

### for testing and debuggin write the extracted wrf data to save time
# readr::write_rds(
#   wrf_data_list,
#   fs::path(data_pth_out, 'wrf_data_list.rds')
# )

rm(wrf_long_all, wrf_precip)
gc()


# LOAD OBSERVATIONS -------------------------------------------------------

logger::log_info('Loading observation data')

# if precip is requested then replace WRF precip vars with 'precip'
obs_vars = c(
  setdiff(
    unname(WRF_EXTRACT_VARS),
    c('RAINNC', 'RAINC')
  ),
  'precip'
)

obs_data = obs_data_pths %>%
  purrr::map(readr::read_rds) %>%
  purrr::map('processed_data') %>%
  purrr::compact() %>%
  dplyr::bind_rows()

if (!'scenario' %in% names(obs_data)) {
  obs_data = obs_data %>%
    dplyr::mutate(scenario = 'obs')
}

obs_data = obs_data %>%
  tidyr::pivot_longer(
    cols = dplyr::any_of(obs_vars),
    names_to = 'var',
    values_to = 'value'
  )

wrf_data = wrf_data_list %>% 
  dplyr::bind_rows() %>% 
  dplyr::select(-dplyr::any_of(c('vertical_level', 'file_id')))

rm(wrf_data_list)
gc()


# COMBINE MODEL AND OBS ---------------------------------------------------

logger::log_info('Combining modelled and observed data')

mobs_list = dplyr::bind_rows(wrf_data, obs_data) %>% 
  split(.$code)

rm(wrf_data, obs_data)
gc()

future::plan(future::multisession, workers = collate_workers)

mobs_opts = furrr::furrr_options(
  seed = TRUE,
  scheduling = Inf,
  packages = c(
    'dplyr',
    'tidyr',
    'tibble'
  ),
  globals = c('add_subprecip_long')
)

mobs_list = mobs_list %>%
  furrr::future_map(
    function(df_code) {
      df_code %>%
        add_subprecip_long() %>% 
        dplyr::filter(.data$var != 'precip_code') %>% 
        tidyr::pivot_wider(
          id_cols = c('date', 'code', 'var'),
          names_from = 'scenario',
          values_from = 'value'
        ) %>% 
        dplyr::arrange(.data$date)
    },
    .options = mobs_opts
  )

future::plan(future::sequential)

names(mobs_list) = mobs_list %>% 
  purrr::map_chr(~ dplyr::pull(.x, .data$code) %>% unique())


# WRITE OUTPUT ------------------------------------------------------------

logger::log_info('Writing *_mod_obs.rds files')

future::plan(future::multisession, workers = collate_workers)

wide_opts = furrr::furrr_options(
  seed = TRUE,
  scheduling = Inf,
  packages = c('data.table')
)

mobs_wide = mobs_list %>%
  furrr::future_map(
    function(df) {
      dt = data.table::as.data.table(df)
      
      data.table::dcast(
        dt,
        date + code ~ var,
        value.var = c('obs', 'mod')
      )
    },
    .options = wide_opts
  )

future::plan(future::sequential)

mobs_data_pths = fs::path(
  data_pth_out,
  paste0(names(mobs_list), '_mod_obs'),
  ext = 'rds'
)

purrr::walk2(
  mobs_wide,
  mobs_data_pths,
  readr::write_rds
)

rm(mobs_wide)
gc()

logger::log_info('MOBS collation complete')
logger::log_info('Wrote {length(mobs_data_pths)} *_mod_obs.rds files to {data_pth_out}')