library(fs)
library(glue)
library(logger)
library(checkmate)


# CRS definitions ----------------------------------------------------------

MODEL_CRS_LONLAT = '+proj=longlat +R=6370000 +no_defs +type=crs'

MODEL_CRS_STEREO = paste(
  '+proj=stere',
  '+ellps=sphere',
  '+lat_0=90.0',
  '+lon_0=0.0',
  '+x_0=0.0',
  '+y_0=0.0',
  '+k_0=0.9330127018922193',
  '+no_defs',
  '+type=crs'
)


# ==========================================================================
# USER INPUT
# ==========================================================================

# Model CRS ----------------------------------------------------------------

TEST_CRS = MODEL_CRS_STEREO
REF_CRS = MODEL_CRS_STEREO


# Variable parameters file -------------------------------------------------

# Provide an absolute path.

WRF_VAR_PARAMS_FILE = NULL


# Python environment -------------------------------------------------------

# Python environment used for WRF extraction and summary calculations.
# Not required when generating the QAQC report from existing outputs.
# tomasliska: '~/miniforge3/envs/qaqc_2026a/bin/python'

WRF_QAQC_PYTHON = NULL


# NCL palettes -------------------------------------------------------------

# Directory containing NCL .rgb palette files.

NCL_PALETTE_DIR =
  '/mnt/ceph/polar_atmospheric/emep4uk/Tools/NCL/6.6.2/lib/ncarg/colormaps'

# Observation repository paths --------------------------------------------

# Repository containing processed GHCNh observations.

OBS_REPO = NULL

# Text file containing selected GHCNh station codes, one per line.
# Required when collating MOBS datasets.

SELECTED_SITES_FILE = NULL


# QAQC output directory and report filename --------------------------------

QAQC_DIR = NULL

REPORT_FNAME = 'QAQC_Report.html'


# Model inputs -------------------------------------------------------------

TEST_DIR = NULL
REF_DIR = NULL

# Directory containing the outputs from a previous WRF QAQC run.
# Used for Test-Reference comparisons of summary maps and MOBS data.
REF_QAQC_DIR = NULL

# Domains to evaluate, ordered from outermost to innermost.
# Names are used for reporting; values identify the WRF domains.
# At least one Test domain must be specified.
TEST_DOMAINS = c(UK_3km = 'd03')

# Labels used throughout to describe the input runs.

RUN_LABELS = c('Test', 'Reference')

# Optional descriptive labels for the model runs.

TEST_RUN_DESCRIPTOR = NULL
REF_RUN_DESCRIPTOR = NULL

# Report style -------------------------------------------------------------

REPORT_STYLE = 'static'
SAVE_STATIC_OUTPUTS = FALSE

# QAQC tasks ---------------------------------------------------------------

PRINT_WRF_SETTINGS = TRUE

COMPARE_SUMMARIES = FALSE # not yet implemented, keep FALSE

PLOT_STATIC_SUMMARY_MAPS = TRUE
PLOT_INTERACTIVE_SUMMARY_MAPS = FALSE

COLLATE_MOBS = FALSE

PLOT_MOBS_TSERIES = TRUE

PRINT_MODSTATS_TABLES = TRUE

PLOT_MOBS_SCATTER_PLOTS = TRUE

PLOT_MOBS_MODSTAT_MAPS = TRUE

SHOW_QAQC_RUN_INFO = FALSE


# ==========================================================================
# TASK-SPECIFIC INPUTS
# ==========================================================================

# WRF VARIABLE EXTRACTION --------------------------------------------------

# WRF variables extracted when collating model-observation datasets.
# RAINNC and RAINC are required for precipitation calculations.

WRF_EXTRACT_VARS = c(
  'T2',
  'td2',
  'rh2',
  'PSFC',
  'ws',
  'wd',
  'RAINNC',
  'RAINC'
)


# WRF SUMMARY GENERATION ---------------------------------------------------

# Variables for which WRF summary NetCDF files should be calculated.
# Used by summarise_wrf.py / wrf_summary_calc_py.sh.

WRF_SUMMARY_VARS = c(
  'T2',
  'td2',
  'rh2',
  'PSFC',
  'ws',
  'precip'
)

# Summary statistics calculated by the Python summariser.
# The available statistics depend on the summariser implementation.

WRF_SUMMARY_STATS = c('mean')

# the functionality below is WIP
# # SUMMARY COMPARISON -------------------------------------------------------
# 
# # Variables to compare.
# # Use 'all' for all supported variables and prefix variable names
# # with '-' to exclude them.
# 
# SUMMARY_VARS = 'all'
# 
# # Highlight Test-Reference differences exceeding this percentage.
# 
# SUMMARY_DIFF_THRESHOLD = 5


# SUMMARY MAPS -------------------------------------------------------------

# Variables to map.
# Use 'all' for all supported variables and prefix variable names
# with '-' to exclude them.

SUMMARY_MAP_VARS = 'all'

# Variables to display in the HTML report.
# These must be a subset of SUMMARY_MAP_VARS.

SUMMARY_MAP_VARS_REPORT = c(
  # 'T2',
  # 'td2',
  # 'rh2',
  # 'PSFC',
  # 'ws',
  'precip'
)

# Summary statistic to plot.
# Must match the statistic recorded in the summary NetCDF filename.

SUMMARY_MAP_TEST_STAT = 'mean'
SUMMARY_MAP_REF_STAT = 'mean'

# Optional date tags to distinguish summary files when multiple
# periods are available for the same variable, statistic and domain.

SUMMARY_MAP_TEST_DATE_TAG = NULL
SUMMARY_MAP_REF_DATE_TAG = NULL

# Optional spatial crop and mask applied to summary maps.

SUMMARY_MAP_DOMAIN_CROP = NULL
SUMMARY_MAP_DOMAIN_MASK = NULL

# Direction used when calculating Test-Reference differences.
# Options: 'test_minus_ref' or 'ref_minus_test'.

DIFF_DIRECTION = 'test_minus_ref'

# Default palettes for Test/Reference, absolute-difference and
# relative-difference maps. Variable-specific colours defined in
# WRF_VAR_PARAMS_LIST take precedence. 
#
# Palettes may be specified as an NCL palette file or a palette name.
# Supported palette namespaces are 'khroma::<palette>', 'scico::<palette>',
# 'viridis::<palette>', 'hcl::<palette>' and 'brewer::<palette>'.
TESTREF_PALETTE = fs::path(NCL_PALETTE_DIR, 'WhiteBlueGreenYellowRed.rgb')

ABSDIFF_PALETTE = fs::path(NCL_PALETTE_DIR, 'NCV_blu_red.rgb')

RELDIFF_PALETTE = fs::path(NCL_PALETTE_DIR, 'NCV_blu_red.rgb')


# STATIC SUMMARY MAPS ------------------------------------------------------

SUMMARY_MAPS_PDF_FNAME = 'WRF_summary_maps.pdf'

# Rasterise maps saved to PDF to reduce file size.

SUMMARY_MAP_PDF_RASTERISE = TRUE
SUMMARY_MAP_PDF_DPI = 300

# Resolution of static maps displayed in the HTML report.

SUMMARY_MAP_REPORT_DPI = 150

# Optional additional mapping layers.
# NULL uses Natural Earth country boundaries.

MAPPING_GEO_LIST = NULL

# Colourbar dimensions.

CBAR_WIDTH = NULL
CBAR_HEIGHT = grid::unit(2, 'mm')

# Colourbar label formatting.

CBAR_LABEL_ANGLE = 90
CBAR_LABEL_VJUST = 0.5
CBAR_LABEL_HJUST = 1

# Titles for Test, Reference, absolute-difference and
# relative-difference maps.

PLOT_TITLES = c(
  RUN_LABELS[1],
  RUN_LABELS[2],
  glue::glue('{RUN_LABELS[[1]]} - {RUN_LABELS[[2]]}'),
  glue::glue('{RUN_LABELS[[1]]} - {RUN_LABELS[[2]]} (%)')
)

PLOT_TITLE_SIZE = 10
LEGEND_TITLE_SIZE = 10
LEGEND_TEXT_SIZE = 8


# INTERACTIVE SUMMARY MAPS -------------------------------------------------

# Basemaps displayed in interactive maps.
# Options: 'world_topo', 'dark', 'grey', 'satellite', 'terrain'.

INTERACTIVE_SUMMARY_MAP_BASEMAP = c('world_topo', 'grey')

# By default, use the same crop and mask as the static maps.

INTERACTIVE_SUMMARY_MAP_DOMAIN_CROP = SUMMARY_MAP_DOMAIN_CROP
INTERACTIVE_SUMMARY_MAP_DOMAIN_MASK = SUMMARY_MAP_DOMAIN_MASK

# Map types to include.
# Options: 'test', 'ref', 'absdiff', 'reldiff'.

INTERACTIVE_SUMMARY_MAP_TYPES = c('test')

# Display map values as raster cells or points.
# Options: 'raster', 'points'.

INTERACTIVE_SUMMARY_MAP_DISPLAY = 'points'

# Maximum number of interactive maps included in the report.

INTERACTIVE_SUMMARY_MAP_MAX_MAPS = 4

INTERACTIVE_SUMMARY_MAP_OPACITY = 0.65

# Hover-point appearance.

INTERACTIVE_SUMMARY_MAP_HOVER_COLOUR = '#000000'
INTERACTIVE_SUMMARY_MAP_HOVER_RADIUS = 3

# Override variable-specific hover-value settings.
# NULL uses variable-specific settings.
# FALSE disables hover values.
# TRUE enables all values.

INTERACTIVE_SUMMARY_MAP_HOVER_VALUES_OVERRIDE = list(
  test = FALSE
)

INTERACTIVE_SUMMARY_MAP_HOVER_MAX_POINTS = 20000

INTERACTIVE_SUMMARY_MAP_LEGEND = TRUE


# MOBS ---------------------------------------------------------------------

# MOBS evaluation uses the innermost submitted Test domain.
# This is determined automatically from TEST_DOMAINS.

# MOBS COLLATION ----------------------------------------------------------

# For stations whose nearest WRF grid cell is water (XLAND = 2),
# extract meteorological data from the nearest land cell instead.
MOBS_COASTAL_LAND_CORRECTION = FALSE

# Maximum grid-cell neighbourhood distance for finding a land cell.
# 1 = immediate neighbours, including diagonals (3 x 3 neighbourhood).
# 2 = extended neighbourhood (5 x 5).
MAX_NDIST = 1

# Include stations that remain classified as marine.
MOBS_INCLUDE_MARINE_SITES = FALSE

# Optional spatial mask used when selecting monitoring sites.

MOBS_DOMAIN_MASK = NULL

# Existing Reference MOBS data. Set to NULL if no Reference run is available.
REF_MOBS_DIR = NULL

# MOBS EVALUATION ----------------------------------------------------------

MOBS_EVAL_VARS = c(
  'T2',
  'td2',
  'rh2',
  'PSFC',
  'ws',
  'subprecip'
)

# Minimum data capture (%) required for a site to be included
# in comparisons of model statistics between sites.

MOBS_THRESHOLD = 30

# Model statistics to calculate.
# Options: 'n', 'FAC2', 'MB', 'NMB', 'RMSE',
#          'r_spearman', 'r_pearson'.

MODSTATS_STATS = c(
  'n',
  'FAC2',
  'MB',
  'NMB',
  'RMSE',
  'r_spearman'
)

# Optional grouping used for model statistics and scatterplots.

MOBS_GROUPING_VAR = NULL
MOBS_GROUPING_VAR_LABEL = NULL

MOBS_GROUPING_VAR_COLOURS = NULL


# MOBS MODSTATS TABLES -----------------------------------------------------

# Fill colours for variables in model statistics tables.
# 'params_file' uses obs_fill from WRF_VAR_PARAMS_LIST.
# NULL uses no fill.

MOBS_MODSTATS_VAR_FILL = 'params_file'

# Filename for exported model statistics tables.

MOBS_STATS_FNAME = 'Mobs_modstats_annual.csv'


# MOBS TIME SERIES ---------------------------------------------------------

MOBS_TSERIES_VARS = c(
  'T2',
  'td2',
  'rh2',
  'PSFC',
  'ws',
  'wd',
  'precip'
)

# Time resolution and statistic used to summarise time series.

MOBS_TSERIES_SUMMARY_TIME = 'day'
MOBS_TSERIES_SUMMARY_STAT = 'mean'

MOBS_TSERIES_LABELS = c(
  obs = 'Observed',
  mod = 'Modelled',
  ref_mod = NA_character_
)

# Plot observations as a ribbon or line.
# Options: 'ribbon', 'line'.

MOBS_TSERIES_OBS_STYLE = 'ribbon'

# Transparency of observed ribbon.

MOBS_TSERIES_OBS_ALPHA = 0.7

# Plot Reference model run when available.

PLOT_REF_TSERIES = FALSE

# Reference-run line styling.

MOBS_REF_PLOT_PARAMS = list(
  colour = 'grey25',
  linetype = 'solid',
  linewidth = 0.7
)

# Plot time series in local time where available.
# FALSE uses UTC.

MOBS_USE_LOCAL_TIME = TRUE

# Geodata used to determine local time zones.

TIMEZONE_FILE_PTH =
  fs::path(  fs::path('/home/tomasliska/QAQC_ANCILLARY_FILES/Timezones/combined-shapefile-with-oceans.shp'))

# Plot all requested variables.
# FALSE plots only variables with observations.

PLOT_ALL_MOBS_VARS = TRUE

# Number of static MOBS time-series plots per PDF page.

PPP = 4

# Selected sites to show as interactive time series in the HTML report.
# Each entry must contain a site code and may optionally specify
# variables and data resolution.
#
# resolution = 'summary' uses MOBS_TSERIES_SUMMARY_TIME and
# MOBS_TSERIES_SUMMARY_STAT.
#
# resolution = 'native' plots the underlying data at native resolution.

MOBS_STATION_REPORT_TSERIES = list(
  ANDREWSFIELD = list(
    code = 'UKM00003684',
    resolution = 'summary'
  ),
  
  `EDINBURGH GOGARBANK` = list(
    code = 'UKM00003166',
    resolution = 'summary'
  ),
  
  CAMBORNE = list(
    code = 'UKM00003808',
    resolution = 'summary'
  ),
  
  `LOUGH FEA` = list(
    code = 'UKM00003911',
    resolution = 'summary'
  )
)

# NULL uses the complete available period.

MOBS_STATION_REPORT_TSERIES_PERIOD = NULL


# MOBS SCATTER PLOTS -------------------------------------------------------

# Point colours for scatterplots.
# Only used when MOBS_GROUPING_VAR is NULL.
#
# 'params_file' uses mobs$mod_colour from WRF_VAR_PARAMS_LIST.
# NULL uses the plotting default.

MOBS_SCATTER_POINT_COLOUR = 'params_file'


# MOBS MODSTATS MAPS -------------------------------------------------------

MOBS_MAP_VARS = c(
  'T2',
  'td2',
  'rh2',
  'PSFC',
  'ws',
  'subprecip'
)

MOBS_MAP_STATS = c(
  'MB',
  'NMB',
  'RMSE',
  'r_spearman'
)

# Optionally modify the statistics mapped for individual variables.
# INCLUDE adds statistics.
# EXCLUDE removes statistics.

MOBS_MAP_STATS_INCLUDE_BY_VAR = list(
  precip = c('MB', 'NMB'),
  subprecip = c('MB', 'NMB')
)

MOBS_MAP_STATS_EXCLUDE_BY_VAR = list(
  T2 = c('NMB', 'r_spearman', 'r_pearson'),
  td2 = c('NMB', 'r_spearman', 'r_pearson'),
  slp = c('NMB', 'r_spearman', 'r_pearson')
)

# Palettes used for MOBS model-statistic maps.

MOBS_MAP_MB_PALETTE = 'brewer::RdBu'
MOBS_MAP_NMB_PALETTE = 'brewer::RdBu'
MOBS_MAP_RMSE_PALETTE = 'viridis::magma'
MOBS_MAP_R_PALETTE = 'brewer::RdBu'


# LOGGER -------------------------------------------------------------------

LOGGER_FNAME = 'QAQC_log.txt'


# ==========================================================================
# INITIALISATION - DO NOT EDIT
# ==========================================================================

# Validate submitted Test domains.

if (is.null(TEST_DOMAINS) || length(TEST_DOMAINS) == 0) {
  stop("At least one Test WRF domain must be specified.")
}

if (anyDuplicated(TEST_DOMAINS)) {
  stop("TEST_DOMAINS contains duplicate domain identifiers.")
}

# Identify submitted Test domains.
submitted_test_domains = TEST_DOMAINS

# Identify the domain used for MOBS evaluation.
mobs_domain = unname(tail(TEST_DOMAINS, 1))

# Output directories -------------------------------------------------------
if (
  !is.character(QAQC_DIR) ||
  length(QAQC_DIR) != 1 ||
  is.na(QAQC_DIR) ||
  !nzchar(QAQC_DIR)
) {
  stop(
    'QAQC_DIR must be a non-empty directory path.',
    call. = FALSE
  )
}

qaqc_pth_out = if (
  !is.null(TEST_DIR) &&
  fs::path_norm(QAQC_DIR) == fs::path_norm(TEST_DIR)
) {
  fs::dir_create(fs::path(QAQC_DIR, 'QAQC'))
} else {
  fs::dir_create(QAQC_DIR)
}

report_pth_out = fs::dir_create(fs::path(qaqc_pth_out, 'Reports'))
maps_pth_out = fs::dir_create(fs::path(qaqc_pth_out, 'Maps'))
plots_pth_out = fs::dir_create(fs::path(qaqc_pth_out, 'Plots'))
tables_pth_out = fs::dir_create(fs::path(qaqc_pth_out, 'Tables'))
data_pth_out = fs::dir_create(fs::path(qaqc_pth_out, 'Data'))

# WRF summary directories --------------------------------------------------

test_summary_dir = fs::dir_create(
  fs::path(data_pth_out, 'WRF_summaries')
)

ref_summary_dir = if (!is.null(REF_QAQC_DIR)) {
  fs::path(REF_QAQC_DIR, 'Data', 'WRF_summaries')
} else {
  NULL
}

# Logger.

logger_pth = ifelse(
  LOGGER_FNAME == 'default',
  fs::path(
    report_pth_out,
    fs::path_ext_remove(REPORT_FNAME),
    ext = 'log'
  ),
  fs::path(report_pth_out, LOGGER_FNAME)
)

# USERS --------------------------------------------------------------------

USERS = c(
  tomlis65 = 'Tomas Liska',
  tomasliska = 'Tomas Liska',
  mvi = 'Massimo Vieno',
  jansch = 'Janice Scheffler',
  racbec = 'Rachel Beck',
  damtan = 'Damaris Tan',
  chrhoo = 'Christina Hood'
)