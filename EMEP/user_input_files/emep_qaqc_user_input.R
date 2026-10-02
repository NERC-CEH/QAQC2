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

EMEP_VAR_PARAMS_FILE = fs::path('var_params_files/emep_vars_parameters.R')

# NCL palettes -------------------------------------------------------------

# Directory containing NCL .rgb palette files.

NCL_PALETTE_DIR =
  '/mnt/ceph/polar_atmospheric/emep4uk/Tools/NCL/6.6.2/lib/ncarg/colormaps'


# Observation repository paths --------------------------------------------

# Observation repositories used to collate model-observation datasets.
# If multiple directories are supplied, their order defines dataset priority.
# core path: '/home/emep4uk/OBSERVATIONS'
TEST_OBS_DIR = NULL


# QAQC output directory and report filename --------------------------------

QAQC_DIR = NULL

REPORT_FNAME = 'QAQC_Report.html'

# Model inputs -------------------------------------------------------------

# At least one domain must be supplied in TEST_DIR
# If REF_DIR is not NULL, its submitted domains must correspond to TEST_DIR
TEST_DIR = list(
  outer = NULL,
  inner = NULL
)

REF_DIR = list(
  outer = NULL,
  inner = NULL
)

# config file path
CONFIG_FILE_PTH = NULL
RUN_SCRIPT_PTH = NULL

# Existing Reference MOBS data. Set to NULL if no Reference run is available.
REF_MOBS_DIR = NULL

# Labels used throughout to describe the input runs 
RUN_LABELS = c('Test', 'Reference')

# Optional override for non-standard EMEP output filenames. NOT WORKING YET!
TEST_EMEP_FILE_TAG = NULL
REF_EMEP_FILE_TAG = NULL

# Optional modelled periods. If NULL, infer from the EMEP output files.
# Periods use [START_DATE, END_DATE).
TEST_START_DATE = NULL
TEST_END_DATE = NULL

REF_START_DATE = NULL 
REF_END_DATE =  NULL


# If NULL, infer from the emission filenames.
TEST_EMISSION_YEAR = NULL
REF_EMISSION_YEAR = NULL


# Report style -------------------------------------------------------------

REPORT_STYLE = 'static'
SAVE_STATIC_OUTPUTS = TRUE

# QAQC tasks ---------------------------------------------------------------

INCLUDE_CONFIG_RUNSCRIPT_FILES = TRUE

CHECK_FILE_SIZE = TRUE

CHECK_EMISSIONS = TRUE

CHECK_EMISSION_TEMPORAL = TRUE
PLOT_NATURAL_EMISSION_TEMPORAL = FALSE

PLOT_TEMPORAL_PROFILES = FALSE

COMPARE_SUMMARIES = TRUE

COMPARE_MASS_BUDGET = TRUE

PLOT_STATIC_SUMMARY_MAPS = TRUE
PLOT_INTERACTIVE_SUMMARY_MAPS = FALSE

COLLATE_MOBS = TRUE

PLOT_MOBS_TSERIES = TRUE

PRINT_MODSTATS_TABLES = TRUE

PLOT_MOBS_SCATTER_PLOTS = TRUE

PLOT_MOBS_MODSTAT_MAPS = TRUE

SHOW_QAQC_RUN_INFO = TRUE


# ==========================================================================
# TASK-SPECIFIC INPUTS
# ==========================================================================

# EMISSION CHECKS ----------------------------------------------------------

# Region geodata used by the emission inventory and temporal checks.

REGION_GEODATA_DIR =
  '/mnt/ceph/polar_atmospheric/home/tomlis65/EMEP_QAQC/source_code/EMEP_QAQC/Area_masks/EMEP'
EMISSION_INVENTORY_MIN_COVERAGE = 0.95


# Emission inventory check -------------------------------------------------

# The inventory check uses the EMEP 'fullrun' output

EMISSION_INVENTORY_PTH = list(
  outer = '/mnt/ceph/polar_atmospheric/home/tomlis65/EMEP_QAQC/source_code/EMEP_QAQC/Emission_Inventory_files/EMEP/2025_update/country_totals_2022.txt',
  inner = NULL
)

# Emission inventory type for each supplied inventory.
# Currently supported: 'emep'
EMISSION_INVENTORY_TYPE = list(
  outer = 'emep',
  inner = 'naei'
)

# still in development and not currently used
EMISSION_NETCDF_FILES = NULL

# could be the sector number in the netcdf
# or 'all' or a combination c('all', '-1') to exclude sector 1
EMISSION_NETCDF_SECTORS = 'all'

EMISSION_CHECK_POLLS = c('sox', 'nox', 'co', 'voc', 'nh3', 'pm25', 'pmco')

EMISSION_CHECK_REGIONS = list(
  outer = 'all',
  inner = 'GB'
)

# Highlight differences exceeding this percentage in the report
EMISSION_COMPARISON_THRESHOLD = 5

# Emission temporal check --------------------------------------------------

EMISSION_TEMPORAL_REGIONS = list(
  outer = c('GB'),
  inner = c('GB')
)

EMISSION_TEMPORAL_POLLS = c(
  'sox', 'nox', 'co', 'voc', 'nh3', 'pm25', 'pmco'
)

EMISSION_TEMPORAL_FILE_TAG = 'month'

# NULL uses variable-specific colours where defined, with Viridis as the
# default. A supplied palette overrides variable-specific colours.
# Supported palette namespaces are 'khroma::<palette>', 'scico::<palette>',
# 'viridis::<palette>', 'hcl::<palette>' and 'brewer::<palette>'.
EMISSION_PALETTE = 'khroma::vibrant'

# TEMPORAL VARIATION CHECK -------------------------------------------------

# All variables to include in the temporal variation check must be listed
TEMPORAL_VARIATION_VARS = NULL
TEMPORAL_VARIATION_FILE_TAG = 'month'

# Optional plotting period. If NULL, use the complete model period.
# Dates refer to the test-run year; the equivalent calendar period is used
# for the reference run if the test and reference years differ.
TEMPORAL_VARIATION_PLOT_START = NULL
TEMPORAL_VARIATION_PLOT_END = NULL

# Regions must be specified by full file path. Use 'full_domain' to summarise
# the complete model domain
TEMPORAL_VARIATION_REGIONS = list(
  outer = NULL ,
  inner = NULL
)

TEMPORAL_VARIATION_REGION_MIN_COVERAGE = 0.95

# Points may be supplied either directly as longitude/latitude coordinates
# or using TEMPORAL_SUMMARY_POINT_FILE, but not both.
# Example: list(`US Embassy` = c(longitude = 106.9302, latitude = 47.9283))
TEMPORAL_VARIATION_POINTS = NULL

# Alternatively, supply the full file path to a point geofile
TEMPORAL_VARIATION_POINT_FILE = NULL

# Options: 'location' or 'variable'
TEMPORAL_VARIATION_GROUP_BY = 'variable'

# Values shown in Test and Reference temporal variation plots.
# Options: 'value' for actual values or 'fraction' for the fraction
# of the total represented by each period.
# Difference and ratio plots always use actual values.
TEMPORAL_VARIATION_VALUE_MODE = 'value'

# Controls how area-normalised mass variables (emissions, dry deposition and
# wet deposition) are represented in temporal variation plots for a grid point
#
# Options:
# 'area_normalised' = mass per unit area (e.g. mg m-2)
# 'total'           = total mass for the grid cell (e.g. Gg)
TEMPORAL_VARIATION_MASS_QUANTITY = 'area_normalised'

# NULL uses variable-specific colours where defined, with Viridis as the
# default. A supplied palette overrides variable-specific colours.
# Supported palette namespaces are 'khroma::<palette>', 'scico::<palette>',
# 'viridis::<palette>', 'hcl::<palette>' and 'brewer::<palette>'.
TEMPORAL_PALETTE = 'khroma::vibrant'

# SUMMARY COMPARISON -------------------------------------------------------

# Variables to compare. Use 'all' for all available variables and prefix
# variable names with '-' to exclude them from 'all'.
# Example:
# SUMMARY_VARS = c('all', '-SURF_ug_NO2')
SUMMARY_VARS = 'all'

# Directory containing the region geofiles used for the summary comparison.
SUMMARY_REGION_GEODATA_DIR = REGION_GEODATA_DIR

# Regions are specified using the final underscore-separated part of the
# geofile name, e.g. 'Mongolia.gpkg' -> 'Mongolia' and '..._GB.gpkg' -> 'GB'.
# Use 'all' for all available regions and prefix IDs with '-' to exclude them.
SUMMARY_REGIONS = list(
  outer = 'GB',
  inner = 'GB'
)

# Highlight test-reference differences exceeding this percentage in the report
SUMMARY_DIFF_THRESHOLD = 5

# MASS BUDGET COMPARISON ---------------------------------------------------

# Highlight test-reference differences exceeding this percentage in the report
MASS_BUDGET_DIFF_THRESHOLD = 5

# SUMMARY MAPS -------------------------------------------------------------

# Variables to map. Use 'all' for all supported variables and prefix variable
# names with '-' to exclude them from 'all' e.g '-SURF_ug_NO2'
SUMMARY_MAP_VARS = 'all'

# Variables to display in the HTML report. These must be a subset of
# SUMMARY_MAP_VARS and is capped at 10 vars
SUMMARY_MAP_VARS_REPORT = c(
  'SURF_ug_NO2',
  'SURF_ug_NH3',
  'SURF_ppb_O3',
  'SURF_ug_PM25_rh50'
)

# EMEP output files used for the Test and Reference maps
SUMMARY_MAP_TEST_FILE_TAG = 'fullrun'
SUMMARY_MAP_REF_FILE_TAG = 'fullrun'

# NULL uses all available time steps. Where more than one time step is
# available, the corresponding summary statistic is applied
SUMMARY_MAP_TEST_T_INDEX = NULL
SUMMARY_MAP_REF_T_INDEX = NULL

# NULL uses the variable-specific z_index from EMEP_VAR_PARAMS_LIST.
# Set an index here to override it for all mapped variables
SUMMARY_MAP_TEST_Z_INDEX = NULL
SUMMARY_MAP_REF_Z_INDEX = NULL

# Summary statistic applied where multiple time steps are selected.
# Options: 'mean', 'min', 'max', 'median' or a percentile such as 'p95'
SUMMARY_MAP_TEST_STAT = 'mean'
SUMMARY_MAP_REF_STAT = 'mean'

# Optional spatial crop and mask applied to all summary maps.
SUMMARY_MAP_DOMAIN_CROP = NULL # '/Users/tomasliska/Desktop/Work/EMEP_QAQC/Area_masks/WHO/Ubi_bbox.gpkg'
SUMMARY_MAP_DOMAIN_MASK = NULL

DIFF_DIRECTION = 'test_minus_ref'

# Default palettes for Test/Reference, absolute-difference and
# relative-difference maps. Variable-specific colours defined in
# EMEP_VAR_PARAMS_LIST take precedence. 
#
# Palettes may be specified as an NCL palette file or a palette name.
# Supported palette namespaces are 'khroma::<palette>', 'scico::<palette>',
# 'viridis::<palette>', 'hcl::<palette>' and 'brewer::<palette>'.
TESTREF_PALETTE = fs::path(NCL_PALETTE_DIR, 'WhiteBlueGreenYellowRed.rgb')

ABSDIFF_PALETTE = fs::path(NCL_PALETTE_DIR, 'NCV_blu_red.rgb')

RELDIFF_PALETTE = fs::path(NCL_PALETTE_DIR, 'NCV_blu_red.rgb')

# STATIC SUMMARY MAPS ------------------------------------------------------

SUMMARY_MAPS_PDF_FNAME = 'EMEP_summary_maps.pdf'

# Rasterise maps saved to PDF to reduce file size.
SUMMARY_MAP_PDF_RASTERISE = TRUE
SUMMARY_MAP_PDF_DPI = 300

# Resolution of static maps displayed in the HTML report.
SUMMARY_MAP_REPORT_DPI = 150

# Optional additional mapping layers. NULL uses Natural Earth country
# boundaries at medium resolution.
MAPPING_GEO_LIST = NULL

# Colourbar width. NULL uses the plotting default. For example,
# grid::unit(2.5, 'inches') works well for two maps side by side, while
# grid::unit(6, 'inches') works well for a single map.
CBAR_WIDTH = NULL

CBAR_HEIGHT = grid::unit(2, 'mm')

# Adjust label angle and justification if colourbar labels overlap.
CBAR_LABEL_ANGLE = 90
CBAR_LABEL_VJUST = 0.5
CBAR_LABEL_HJUST = 1

# Titles used for the Test, Reference, absolute-difference and
# relative-difference maps. Ensure the difference titles agree with
# DIFF_DIRECTION.
# can be c(NA, NA, NA, NA)
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

# Basemaps displayed in the interactive maps.
# Options: 'world_topo', 'dark', 'grey', 'satellite' and 'terrain'.
INTERACTIVE_SUMMARY_MAP_BASEMAP = c('world_topo', 'grey')

# By default, use the same crop and mask as the summary maps.
# These may be changed to focus the interactive maps on a smaller region.
INTERACTIVE_SUMMARY_MAP_DOMAIN_CROP = SUMMARY_MAP_DOMAIN_CROP
INTERACTIVE_SUMMARY_MAP_DOMAIN_MASK = SUMMARY_MAP_DOMAIN_MASK

# Map types to include.
# Options: 'test', 'ref', 'absdiff' and 'reldiff'.
INTERACTIVE_SUMMARY_MAP_TYPES = c(
  'test'
)

# Display map values as raster cells or points.
# Options: 'raster' or 'points'.
INTERACTIVE_SUMMARY_MAP_DISPLAY = 'points'

# Maximum number of interactive maps to include in the report. Point maps
# are substantially larger than raster maps (approximately 5 times larger
# in testing), so use a lower limit when INTERACTIVE_SUMMARY_MAP_DISPLAY
# is set to 'points'. 
INTERACTIVE_SUMMARY_MAP_MAX_MAPS = 4

INTERACTIVE_SUMMARY_MAP_OPACITY = 0.65

# Hover-point appearance.
INTERACTIVE_SUMMARY_MAP_HOVER_COLOUR = '#000000'
INTERACTIVE_SUMMARY_MAP_HOVER_RADIUS = 3

# Override variable-specific hover-value settings for interactive maps.
# NULL = use the variable-specific setting; FALSE = disable; TRUE = all values.
# Conditions can use percentiles (e.g. 'p95', '>p95'), values (e.g. '>5'),
# or multiple conditions combined with OR (e.g. c('<p5', '>p95')).
INTERACTIVE_SUMMARY_MAP_HOVER_VALUES_OVERRIDE = list(
  test = FALSE
)

INTERACTIVE_SUMMARY_MAP_HOVER_MAX_POINTS = 20000

INTERACTIVE_SUMMARY_MAP_LEGEND = TRUE

# MOBS --------------------------------------------------------------------

# Labels used for observation datasets in the report.
MOBS_SOURCE_LABELS = c(
  aurn = 'Automatic Urban and Rural Network (AURN)',
  saqn = 'Scottish Air Quality Network (SAQN)'
)

# Optional spatial mask used when selecting monitoring sites.
MOBS_DOMAIN_MASK = NULL

# Pollutants available for MOBS collation and evaluation.
OBSERVED_POLLS = c(
  'no', 'no2', 'o3', 'ox(ppb)', 'so2', 'pm10', 'pm2.5'
)

# Species with interval observations. Under development
# NONAUTO_SPECIES = c(
#   'Ca_p', 'Cl_p', 'HCl_g', 'HNO3_g', 'HONO_g', 'Mg_p', 'Na_p',
#   'NH3_alpha', 'NH3_delta', 'NH3_diffusion_tube', 'NH4_p', 'NO2_p',
#   'NO3_p', 'SO2_g', 'SO4_p', 'NH4_precip', 'NO3_precip',
#   'nmSO4_precip', 'rainfall'
# )

# Units used when calculating Ox.
# Options: 'ppb' or 'ug/m3'.
OX_UNITS = 'ppb'

# MOBS COLLATION -----------------------------------------------------------

# Link observed pollutants to the corresponding EMEP model variables.
OBSERVED_POLLS_EMEP_LINK = c(
  no = 'SURF_ug_NO',
  no2 = 'SURF_ug_NO2',
  o3 = 'SURF_ppb_O3',
  so2 = 'SURF_ug_SO2',
  pm10 = 'SURF_ug_PM10_rh50',
  pm2.5 = 'SURF_ug_PM25_rh50'
)

# MOBS EVALUATION ----------------------------------------------------------

MOBS_EVAL_VARS = OBSERVED_POLLS

# Minimum data capture (%) required for a site to be included in
# comparisons of model statistics between sites.
MOBS_THRESHOLD = 75

# Model statistics to calculate.
# Options: 'n', 'FAC2', 'MB', 'NMB', 'RMSE', 'r_spearman', 'r_pearson'.
MODSTATS_STATS = c(
  'n', 'FAC2', 'MB', 'NMB', 'RMSE', 'r_spearman'
)

# Optional grouping used for model statistics and scatter plots.
# for standard UK data this is 'site_type_grp' which is generated
# automatically when using datasets downloaded through openair
MOBS_GROUPING_VAR = 'site_type_grp'
MOBS_GROUPING_VAR_LABEL = 'Site type'

MOBS_GROUPING_VAR_COLOURS = c(
  Urban = '#7570b3',
  Rural = '#1b9e77',
  Industrial = '#d95f02',
  Road = '#e7298a',
  Unknown = '#666666'
)


# MOBS MODSTATS TABLES -----------------------------------------------------

# Fill colours for variables in the model statistics tables.
# 'params_file' uses obs_fill from EMEP_VAR_PARAMS_LIST.
# NULL uses no fill, or supply a single colour to use for all variables.
MOBS_MODSTATS_VAR_FILL = 'params_file'

# MOBS TIME SERIES ---------------------------------------------------------

MOBS_TSERIES_VARS = OBSERVED_POLLS

# Time resolution and statistic used to summarise the time series.
MOBS_TSERIES_SUMMARY_TIME = 'day'
MOBS_TSERIES_SUMMARY_STAT = 'mean'

MOBS_TSERIES_LABELS = c(
  obs = 'Observed',
  mod = 'Modelled',
  ref_mod = NA_character_
)

# Plot observations as either a ribbon or line.
# Options: 'ribbon' or 'line'.
MOBS_TSERIES_OBS_STYLE = 'ribbon'

# Transparency of the observed ribbon. Only used when
# MOBS_TSERIES_OBS_STYLE = 'ribbon'.
MOBS_TSERIES_OBS_ALPHA = 0.7

# Plot the reference model run in MOBS time-series plots when available.
PLOT_REF_TSERIES = FALSE

# Reference-run line styling applied to all MOBS time-series plots.
MOBS_REF_PLOT_PARAMS = list(
  colour = 'grey25',
  linetype = 'solid',
  linewidth = 0.7
)

# Plot time series in local time where available. FALSE uses UTC.
MOBS_USE_LOCAL_TIME = TRUE

# Geodata used to determine local time zones for MOBS observations.
TIMEZONE_FILE_PTH =
  fs::path('/mnt/ceph/polar_atmospheric/home/tomlis65/EMEP_QAQC/source_code/',
           'EMEP_QAQC/QAQC2/EMEP/utils/timezones/combined-shapefile-with-oceans.shp')
# Plot all requested variables. If FALSE, only variables with observations
# are plotted.
PLOT_ALL_MOBS_VARS = TRUE

# Number of static MOBS time-series plots per PDF page.
# Four plots per page generally gives the best layout.
PPP = 4

# Selected sites to show as interactive time series in the HTML report.
# Each entry must contain a site code and may optionally specify variables
# and the data resolution.
#
# resolution = 'summary' uses MOBS_TSERIES_SUMMARY_TIME and
# MOBS_TSERIES_SUMMARY_STAT.
# resolution = 'native' plots the underlying data at its native resolution.
# Native data are typically hourly and can substantially increase the size
# of the self-contained HTML report. Use 'native' sparingly and consider
# limiting the displayed period with MOBS_STATION_REPORT_TSERIES_PERIOD.
MOBS_STATION_REPORT_TSERIES = list(
  `Chilbolton` = list(
    code = 'CHBO',
    resolution = 'summary'
  ),
  
  `Auchencorth Moss` = list(
    code = 'ACTH',
    resolution = 'summary'
  ),

  `Bristol St Paul's` = list(
    code = 'BRS8',
    resolution = 'summary'
  ),

  `Glasgow Townhead` = list(
    code = 'GLKP',
    resolution = 'summary'
  ),

  `London North Kensington` = list(
    code = 'KC1',
    resolution = 'summary'
  ),

  `London Marylebone Rd` = list(
    code = 'MY1',
    resolution = 'summary'
  )
)

# NULL uses the complete available period.
# Example:
# MOBS_STATION_REPORT_TSERIES_PERIOD = c(
#   '2022-01-01 00:00',
#   '2022-01-31 23:59'
# )
MOBS_STATION_REPORT_TSERIES_PERIOD = NULL


# MOBS SCATTER PLOTS -------------------------------------------------------

# Point colours for MOBS scatter plots. Only used when MOBS_GROUPING_VAR is NULL.
# 'params_file' uses mobs$mod_colour from EMEP_VAR_PARAMS_LIST.
# NULL uses the plotting default, or supply a single colour for all variables.
MOBS_SCATTER_POINT_COLOUR = 'params_file'


# MOBS MODSTATS MAPS -------------------------------------------------------

MOBS_MAP_VARS = OBSERVED_POLLS

MOBS_MAP_STATS = c(
  'MB', 'NMB', 'RMSE', 'r_spearman'
)

# Optionally modify the statistics mapped for individual variables.
# INCLUDE adds statistics to MOBS_MAP_STATS for the specified variables.
# EXCLUDE removes statistics from MOBS_MAP_STATS for the specified variables.
# NULL applies MOBS_MAP_STATS unchanged to all variables.
MOBS_MAP_STATS_INCLUDE_BY_VAR = NULL
MOBS_MAP_STATS_EXCLUDE_BY_VAR = NULL

# Palettes used for MOBS model-statistic maps.
# Supported palette namespaces are 'khroma::<palette>', 'scico::<palette>',
# 'viridis::<palette>', 'hcl::<palette>' and 'brewer::<palette>'.
MOBS_MAP_MB_PALETTE = 'brewer::RdBu'
MOBS_MAP_NMB_PALETTE = 'brewer::RdBu'
MOBS_MAP_RMSE_PALETTE = 'viridis::magma'
MOBS_MAP_R_PALETTE = 'brewer::RdBu'


# LOGGER ------------------------------------------------------------------

LOGGER_FNAME = 'QAQC_log.txt'

# ==========================================================================
# INITIALISATION - DO NOT EDIT
# ==========================================================================

qaqc_pth_out = if (
  is.null(TEST_DIR$inner) ||
  QAQC_DIR != TEST_DIR$inner
) {
  fs::dir_create(QAQC_DIR)
} else {
  fs::dir_create(
    fs::path(QAQC_DIR, 'QAQC')
  )
}

report_pth_out = dir_create(path(qaqc_pth_out, 'Reports'))
maps_pth_out = dir_create(path(qaqc_pth_out, 'Maps'))
plots_pth_out = dir_create(path(qaqc_pth_out, 'Plots'))
tables_pth_out = dir_create(path(qaqc_pth_out, 'Tables'))
data_pth_out = dir_create(path(qaqc_pth_out, 'Data'))

#logger
logger_pth = ifelse(LOGGER_FNAME == 'default',
                    path(report_pth_out, path_ext_remove(REPORT_FNAME), ext = 'log'),
                    path(report_pth_out, LOGGER_FNAME))

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



