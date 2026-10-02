library(rmarkdown)
library(fs)
library(glue)
library(rlang)


# -------------------------------------------------------------------------
# User input file
# -------------------------------------------------------------------------

USER_INPUT_FILE = Sys.getenv(
  'EMEP_QAQC_USER_INPUT',
  unset = NA_character_
)

if (
  is.na(USER_INPUT_FILE) ||
  USER_INPUT_FILE == ''
) {
  stop(
    paste0(
      'EMEP_QAQC_USER_INPUT has not been set. ',
      'Submit the QAQC with a user input file.'
    ),
    call. = FALSE
  )
}

if (!fs::file_exists(USER_INPUT_FILE)) {
  stop(
    glue::glue(
      'User input file not found: {USER_INPUT_FILE}'
    ),
    call. = FALSE
  )
}

USER_INPUT_FILE = fs::path_abs(
  USER_INPUT_FILE
)

source(
  USER_INPUT_FILE
)


# -------------------------------------------------------------------------
# QAQC user
# -------------------------------------------------------------------------

qaqc_user_id = Sys.info()[['user']] %||% NA_character_

if (
  is.na(qaqc_user_id) ||
  !qaqc_user_id %in% names(USERS)
) {
  stop(
    glue::glue(
      'User not defined in USERS. ',
      'Please update USERS in {USER_INPUT_FILE}.'
    ),
    call. = FALSE
  )
}

qaqc_user = USERS[[qaqc_user_id]]


# -------------------------------------------------------------------------
# Render QAQC report
# -------------------------------------------------------------------------

REPORT_SOURCE_FILE = 'EMEP_QAQC_Report.Rmd'

if (!fs::file_exists(REPORT_SOURCE_FILE)) {
  stop(
    glue::glue(
      'EMEP QAQC report source file not found: {REPORT_SOURCE_FILE}'
    ),
    call. = FALSE
  )
}

rmarkdown::render(
  input = REPORT_SOURCE_FILE,
  output_file = fs::path(
    report_pth_out,
    REPORT_FNAME
  ),
  params = list(
    qaqc_user = qaqc_user
  )
)
