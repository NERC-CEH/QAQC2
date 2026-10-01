#utility functions

runtime_platform = function() {
  
  # determines what platform script is run on
  # for environment and parallelisation optimisation
  if (nzchar(Sys.getenv('SLURM_JOB_ID'))) {
    return('slurm-compute')
  }
  
  sys = Sys.info()[['sysname']]
  if (sys == 'Darwin')  return('mac')
  if (sys == 'Windows') return('windows')
  
  if (sys == 'Linux' && file.exists('/etc/slurm/slurm.conf')) {
    return('slurm-login') 
  }
  
  'unknown'
}

sanitise_string = function(
    x,
    case = c(
      'lower',
      'upper',
      'sentence',
      'title'
    ),
    separator = '_'
) {
  
  case = match.arg(case)
  
  x = x %>%
    stringi::stri_trans_general('Latin-ASCII') %>%
    stringr::str_replace_all("[’']", '') %>%
    stringr::str_replace_all('[^A-Za-z0-9]+', ' ') %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()
  
  words = stringr::str_split(x, ' +')
  
  x = purrr::map_chr(
    words,
    function(w) {
      
      if (case == 'upper') {
        w = stringr::str_to_upper(w)
      }
      
      if (case == 'sentence') {
        w[1] = stringr::str_to_title(w[1])
      }
      
      if (case == 'title') {
        w = stringr::str_to_title(w)
      }
      
      paste(w, collapse = separator)
    }
  )
  
  if (case == 'lower') {
    x = stringr::str_to_lower(x)
  }
  
  x
}