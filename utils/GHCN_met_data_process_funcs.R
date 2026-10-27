add_derived_rh_qc_status = function(
    qc_check,
    qc_status_rank = ghcnh_qc_status_rank,
    input_vars = c('air_temp', 'dew_point')
) {
  
  input_qc = qc_check %>%
    dplyr::filter(variable %in% input_vars) %>%
    dplyr::left_join(
      qc_status_rank,
      by = 'qc_status'
    ) %>%
    dplyr::group_by(row_id) %>%
    dplyr::summarise(
      rh_input_qc_rank = {
        valid_ranks = qc_rank[!is.na(qc_rank)]
        
        if (length(valid_ranks) == 0) {
          NA_integer_
        } else {
          max(valid_ranks)
        }
      },
      rh_input_qc_status = {
        valid_rows = !is.na(qc_rank)
        
        if (!any(valid_rows)) {
          NA_character_
        } else {
          qc_status[valid_rows][which.max(qc_rank[valid_rows])]
        }
      },
      rh_input_qc_meaning = {
        valid_rows = !is.na(qc_status)
        
        if (!any(valid_rows)) {
          NA_character_
        } else {
          paste(
            variable[valid_rows],
            qc_status[valid_rows],
            sep = ': ',
            collapse = '; '
          )
        }
      },
      .groups = 'drop'
    )
  
  qc_check %>%
    dplyr::left_join(
      input_qc,
      by = 'row_id'
    ) %>%
    dplyr::mutate(
      qc_status = dplyr::case_when(
        variable == 'rh' &
          qc_system == 'derived_from_inputs' &
          !is.na(value) &
          !is.na(rh_input_qc_status) ~ rh_input_qc_status,
        TRUE ~ qc_status
      ),
      qc_meaning = dplyr::case_when(
        variable == 'rh' &
          qc_system == 'derived_from_inputs' &
          !is.na(value) &
          !is.na(rh_input_qc_status) ~ stringr::str_c(
            'Relative humidity is derived from input variables; input QC: ',
            rh_input_qc_meaning
          ),
        variable == 'rh' &
          qc_system == 'derived_from_inputs' &
          !is.na(value) &
          is.na(rh_input_qc_status) ~
          'Relative humidity is derived from input variables, but input QC could not be assessed',
        TRUE ~ qc_meaning
      )
    ) %>%
    dplyr::select(
      -rh_input_qc_rank,
      -rh_input_qc_status,
      -rh_input_qc_meaning
    )
}

assess_precip_same_len_messiness_by_period = function(precip_oldstyle) {
  
  df = precip_oldstyle %>%
    dplyr::transmute(
      date = date,
      dur = suppressWarnings(as.integer(precip_code)),
      amt = suppressWarnings(as.numeric(precip_raw))
    ) %>%
    dplyr::filter(
      !is.na(dur),
      dur > 1,
      !is.na(amt)
    ) %>%
    dplyr::arrange(dur, date) %>%
    dplyr::mutate(
      start = date - lubridate::hours(dur) + lubridate::hours(1),
      end = date
    )
  
  if (nrow(df) == 0) {
    return(
      tibble::tibble(
        precip_code = integer(),
        n_total = integer(),
        same_len_conflicts = integer(),
        same_len_messiness_index = numeric()
      )
    )
  }
  
  purrr::map_dfr(
    sort(unique(df$dur)),
    function(this_dur) {
      
      this_df = df %>%
        dplyr::filter(dur == this_dur) %>%
        dplyr::arrange(date)
      
      if (nrow(this_df) < 2) {
        return(
          tibble::tibble(
            precip_code = this_dur,
            n_total = nrow(this_df),
            same_len_conflicts = 0L,
            same_len_messiness_index = 0
          )
        )
      }
      
      same_len_conflicts = 0L
      
      for (i in seq_len(nrow(this_df) - 1L)) {
        for (j in (i + 1L):nrow(this_df)) {
          
          si = this_df$start[i]
          ei = this_df$end[i]
          
          sj = this_df$start[j]
          ej = this_df$end[j]
          
          if (max(si, sj) > min(ei, ej)) {
            next
          }
          
          identical_window = (si == sj) && (ei == ej)
          
          if (!identical_window) {
            same_len_conflicts = same_len_conflicts + 1L
          }
        }
      }
      
      n_total = nrow(this_df)
      
      tibble::tibble(
        precip_code = this_dur,
        n_total = n_total,
        same_len_conflicts = same_len_conflicts,
        same_len_messiness_index = if (n_total > 0L) {
          same_len_conflicts / n_total
        } else {
          0
        }
      )
    }
  )
}

clean_precip_accum_windows = function(precip_consolidated) {
  
  df = precip_consolidated %>%
    dplyr::filter(
      !is.na(precip_code),
      precip_code > 1,
      !is.na(precip_raw)
    ) %>%
    dplyr::mutate(
      precip_code = as.integer(precip_code),
      precip_raw = as.numeric(precip_raw),
      window_start = date - lubridate::hours(precip_code) + lubridate::hours(1),
      window_end = date
    ) %>%
    dplyr::arrange(
      dplyr::desc(precip_code),
      dplyr::desc(date)
    )
  
  if (nrow(df) == 0) {
    return(df)
  }
  
  kept = df[0, ]
  
  for (i in seq_len(nrow(df))) {
    
    this_row = df[i, ]
    
    if (nrow(kept) == 0) {
      kept = dplyr::bind_rows(kept, this_row)
      next
    }
    
    overlaps_kept = any(
      this_row$window_start <= kept$window_end &
        this_row$window_end >= kept$window_start
    )
    
    if (!overlaps_kept) {
      kept = dplyr::bind_rows(kept, this_row)
    }
  }
  
  kept %>%
    dplyr::arrange(date) %>%
    dplyr::select(
      date,
      precip_code,
      precip_raw,
      precip_var,
      qc_status,
      qc_meaning,
      rt,
      sc,
      qc,
      si,
      window_start,
      window_end
    )
}

consolidate_ghcnh_precip_accum = function(
    precip_qc,
    keep_qc_status = 'passed',
    include_hourly = FALSE
) {
  
  out = precip_qc %>%
    dplyr::filter(
      !is.na(amount),
      qc_status %in% keep_qc_status
    )
  
  if (!include_hourly) {
    out = out %>%
      dplyr::filter(accum_hours > 1)
  }
  
  out %>%
    dplyr::transmute(
      date,
      precip_code = accum_hours,
      precip_raw = amount,
      precip_var,
      qc_status,
      qc_meaning,
      rt,
      sc,
      qc,
      si
    ) %>%
    dplyr::arrange(date, precip_code)
}

detect_ghcnh_precip_qc_system = function(
    precip_long,
    stop_on_precip_specific = TRUE,
    stop_on_ambiguous = FALSE,
    dominant_worldmet_code = '1',
    dominant_threshold = 0.5
) {
  
  precip_specific_unambiguous_codes = c(
    '6', '7', '8',
    'E', 'I', 'J', 'T'
  )
  
  qc_summary = precip_long %>%
    dplyr::filter(
      !is.na(amount),
      !is.na(qc)
    ) %>%
    dplyr::mutate(
      qc = as.character(qc)
    ) %>%
    dplyr::count(qc, name = 'n') %>%
    dplyr::mutate(
      prop = n / sum(n)
    ) %>%
    dplyr::arrange(
      dplyr::desc(n)
    )
  
  if (nrow(qc_summary) == 0) {
    return(
      list(
        summary = qc_summary,
        assumed_qc_system = 'no_qc_codes',
        stop_required = FALSE,
        message = 'No precipitation QC codes found.'
      )
    )
  }
  
  qc_codes = qc_summary$qc
  
  has_unambiguous_precip_codes = any(
    qc_codes %in% precip_specific_unambiguous_codes
  )
  
  if (has_unambiguous_precip_codes) {
    
    msg = paste0(
      'Precipitation appears to use precipitation-specific measurement codes. ',
      'This workflow currently expects the worldmet/GHCNh QC-style flags. ',
      'Found QC codes: ',
      paste(qc_codes, collapse = ', ')
    )
    
    if (isTRUE(stop_on_precip_specific)) {
      stop(msg)
    }
    
    return(
      list(
        summary = qc_summary,
        assumed_qc_system = 'precip_specific',
        stop_required = TRUE,
        message = msg
      )
    )
  }
  
  code_1_prop = qc_summary %>%
    dplyr::filter(qc == '1') %>%
    dplyr::pull(prop)
  
  code_1_prop = if (length(code_1_prop) == 0) {
    0
  } else {
    code_1_prop
  }
  
  only_1_to_5 = all(qc_codes %in% as.character(1:5))
  
  looks_like_worldmet_qc = only_1_to_5 &&
    code_1_prop > 0.5
  
  if (looks_like_worldmet_qc) {
    return(
      list(
        summary = qc_summary,
        assumed_qc_system = 'worldmet_ghcnh_qc',
        stop_required = FALSE,
        message = paste0(
          'Assuming worldmet/GHCNh QC-style flags because code 1 accounts for ',
          round(code_1_prop * 100, 1),
          '% of non-missing precipitation QC flags.'
        )
      )
    )
  }
  
  msg = paste0(
    'Precipitation QC code system is ambiguous. ',
    'Found QC codes: ',
    paste(qc_codes, collapse = ', '),
    '. Code 1 accounts for ',
    round(code_1_prop * 100, 1),
    '% of non-missing precipitation QC flags.'
  )
  
  if (isTRUE(stop_on_ambiguous)) {
    stop(msg)
  }
  
  list(
    summary = qc_summary,
    assumed_qc_system = 'ambiguous',
    stop_required = FALSE,
    message = msg
  )
}

distribute_precip_accum_hourly = function(precip_accum_clean) {
  
  if (nrow(precip_accum_clean) == 0) {
    return(
      tibble::tibble(
        date = as.POSIXct(character()),
        precip = numeric()
      )
    )
  }
  
  precip_accum_clean %>%
    dplyr::mutate(
      precip_code = as.integer(precip_code),
      precip_raw = as.numeric(precip_raw),
      window_start = date - lubridate::hours(precip_code) + lubridate::hours(1),
      window_end = date
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      hourly_data = list(
        tibble::tibble(
          date = seq.POSIXt(
            from = window_start,
            to = window_end,
            by = 'hour'
          ),
          precip = precip_raw / precip_code
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(hourly_data) %>%
    tidyr::unnest(hourly_data) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      precip = sum(precip, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::arrange(date)
}

filter_ghcn_report_type_records = function(
    data,
    met_vars = c('wd', 'ws', 'air_temp', 'dew_point', 'atmos_pres', 'rh'),
    report_type = 'FM12'
) {
  
  rt_cols = stringr::str_c(met_vars, '_rt')
  rt_cols = rt_cols[rt_cols %in% names(data)]
  
  if (length(rt_cols) == 0) {
    warning('No report-type columns found for requested met variables.')
    
    return(
      list(
        data = data,
        removed_records = tibble::tibble(),
        source_summary = tibble::tibble()
      )
    )
  }
  
  data_checked = data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      report_types_present = list(
        dplyr::c_across(dplyr::all_of(rt_cols)) %>%
          stats::na.omit() %>%
          unique()
      ),
      n_report_types = length(report_types_present),
      only_requested_report_type = (
        n_report_types == 0 |
          (
            n_report_types == 1 &&
              report_types_present[[1]] == report_type
          )
      ),
      report_type_issue = dplyr::case_when(
        n_report_types == 0 ~ NA_character_,
        n_report_types == 1 && report_types_present[[1]] == report_type ~ NA_character_,
        n_report_types == 1 && report_types_present[[1]] != report_type ~ 'wrong_report_type',
        n_report_types > 1 ~ 'mixed_report_types',
        TRUE ~ 'unknown_report_type_issue'
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      report_types_present = purrr::map_chr(
        report_types_present,
        ~ paste(.x, collapse = ', ')
      )
    )
  
  removed_records = data_checked %>%
    dplyr::filter(!only_requested_report_type)
  
  source_summary = removed_records %>%
    dplyr::count(
      station_id,
      station_name,
      report_type_issue,
      report_types_present,
      name = 'n_records'
    ) %>%
    dplyr::arrange(
      station_id,
      report_type_issue,
      dplyr::desc(n_records)
    )
  
  if (nrow(removed_records) > 0) {
    warning(
      nrow(removed_records),
      ' records were removed because their report types were not exclusively ',
      report_type,
      '. Inspect `removed_records` and `source_summary`.'
    )
  }
  
  data_filtered = data_checked %>%
    dplyr::filter(only_requested_report_type) %>%
    dplyr::select(
      -report_types_present,
      -n_report_types,
      -only_requested_report_type,
      -report_type_issue
    )
  
  list(
    data = data_filtered,
    removed_records = removed_records,
    source_summary = source_summary
  )
}

get_ghcnh_precip_long = function(data) {
  
  precip_cols = names(data) %>%
    stringr::str_subset('^precip($|_)')
  
  empty_out = tibble::tibble(
    row_id = integer(),
    date = as.POSIXct(character()),
    precip_var = character(),
    accum_hours = integer(),
    amount = numeric(),
    mc = character(),
    qc = character(),
    rt = character(),
    sc = character(),
    si = character()
  )
  
  if (length(precip_cols) == 0) {
    return(empty_out)
  }
  
  data_id = data %>%
    dplyr::mutate(row_id = dplyr::row_number())
  
  precip_value_cols = precip_cols[
    stringr::str_detect(precip_cols, '^precip(_\\d+)?$')
  ]
  
  precip_value_cols = precip_value_cols[
    purrr::map_lgl(data_id[precip_value_cols], ~ !all(is.na(.x)))
  ]
  
  if (length(precip_value_cols) == 0) {
    return(empty_out)
  }
  
  precip_long = data_id %>%
    dplyr::select(
      row_id,
      date,
      dplyr::all_of(precip_value_cols)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(precip_value_cols),
      names_to = 'precip_var',
      values_to = 'amount'
    ) %>%
    dplyr::mutate(
      accum_hours = stringr::str_extract(precip_var, '\\d+') %>%
        as.integer(),
      accum_hours = dplyr::if_else(
        precip_var == 'precip',
        1L,
        accum_hours
      )
    )
  
  get_precip_meta_long = function(suffix) {
    
    meta_cols = stringr::str_c(precip_value_cols, '_', suffix)
    meta_cols = meta_cols[meta_cols %in% names(data_id)]
    
    if (length(meta_cols) == 0) {
      return(
        precip_long %>%
          dplyr::select(row_id, precip_var) %>%
          dplyr::distinct() %>%
          dplyr::mutate(
            !!suffix := NA_character_
          )
      )
    }
    
    data_id %>%
      dplyr::select(
        row_id,
        dplyr::all_of(meta_cols)
      ) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(meta_cols),
        names_to = 'meta_col',
        values_to = 'meta_value'
      ) %>%
      dplyr::mutate(
        precip_var = stringr::str_remove(
          meta_col,
          stringr::str_c('_', suffix, '$')
        )
      ) %>%
      dplyr::select(
        row_id,
        precip_var,
        meta_value
      ) %>%
      dplyr::rename(
        !!suffix := meta_value
      )
  }
  
  precip_long %>%
    dplyr::left_join(
      get_precip_meta_long('mc'),
      by = c('row_id', 'precip_var')
    ) %>%
    dplyr::left_join(
      get_precip_meta_long('qc'),
      by = c('row_id', 'precip_var')
    ) %>%
    dplyr::left_join(
      get_precip_meta_long('rt'),
      by = c('row_id', 'precip_var')
    ) %>%
    dplyr::left_join(
      get_precip_meta_long('sc'),
      by = c('row_id', 'precip_var')
    ) %>%
    dplyr::left_join(
      get_precip_meta_long('si'),
      by = c('row_id', 'precip_var')
    ) %>%
    dplyr::mutate(
      amount = as.numeric(amount),
      mc = as.character(mc),
      qc = as.character(qc),
      rt = as.character(rt),
      sc = as.character(sc),
      si = as.character(si)
    ) %>%
    dplyr::arrange(
      row_id,
      accum_hours
    )
}

get_ghcnh_qc_long = function(
    data,
    met_vars = c(
      'wd',
      'ws',
      'air_temp',
      'dew_point',
      'atmos_pres',
      'rh'
    )
) {
  
  value_cols = met_vars[met_vars %in% names(data)]
  
  qc_cols = stringr::str_c(met_vars, '_qc')
  sc_cols = stringr::str_c(met_vars, '_sc')
  rt_cols = stringr::str_c(met_vars, '_rt')
  
  qc_cols = qc_cols[qc_cols %in% names(data)]
  sc_cols = sc_cols[sc_cols %in% names(data)]
  rt_cols = rt_cols[rt_cols %in% names(data)]
  
  if (length(value_cols) == 0) {
    return(
      tibble::tibble(
        row_id = integer(),
        station_id = character(),
        station_name = character(),
        date = as.POSIXct(character()),
        variable = character(),
        value = numeric(),
        qc = character(),
        source_code = character(),
        report_type = character()
      )
    )
  }
  
  data_id = data %>%
    dplyr::mutate(
      row_id = dplyr::row_number()
    )
  
  value_long = data_id %>%
    dplyr::select(
      dplyr::any_of(c(
        'row_id',
        'station_id',
        'station_name',
        'date',
        value_cols
      ))
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = 'variable',
      values_to = 'value'
    )
  
  qc_long = data_id %>%
    dplyr::select(
      row_id,
      dplyr::all_of(qc_cols)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(qc_cols),
      names_to = 'qc_col',
      values_to = 'qc'
    ) %>%
    dplyr::mutate(
      variable = stringr::str_remove(
        qc_col,
        '_qc$'
      )
    ) %>%
    dplyr::select(
      row_id,
      variable,
      qc
    )
  
  sc_long = data_id %>%
    dplyr::select(
      row_id,
      dplyr::all_of(sc_cols)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(sc_cols),
      names_to = 'sc_col',
      values_to = 'source_code'
    ) %>%
    dplyr::mutate(
      variable = stringr::str_remove(
        sc_col,
        '_sc$'
      )
    ) %>%
    dplyr::select(
      row_id,
      variable,
      source_code
    )
  
  rt_long = if (length(rt_cols) == 0) {
    
    tibble::tibble(
      row_id = integer(),
      variable = character(),
      report_type = character()
    )
    
  } else {
    
    data_id %>%
      dplyr::select(
        row_id,
        dplyr::all_of(rt_cols)
      ) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(rt_cols),
        names_to = 'rt_col',
        values_to = 'report_type'
      ) %>%
      dplyr::mutate(
        variable = stringr::str_remove(
          rt_col,
          '_rt$'
        )
      ) %>%
      dplyr::select(
        row_id,
        variable,
        report_type
      )
  }
  
  value_long %>%
    dplyr::left_join(
      qc_long,
      by = c('row_id', 'variable')
    ) %>%
    dplyr::left_join(
      sc_long,
      by = c('row_id', 'variable')
    ) %>%
    dplyr::left_join(
      rt_long,
      by = c('row_id', 'variable')
    )
}

import_ghcn_hourly_safe = function(
    station,
    year,
    hourly = FALSE,
    extra = TRUE,
    append_code = TRUE
) {
  
  warning_messages = character()
  
  out = purrr::safely(
    function(...) {
      
      withCallingHandlers(
        worldmet::import_ghcn_hourly(...),
        warning = function(w) {
          warning_messages <<- c(
            warning_messages,
            conditionMessage(w)
          )
          
          invokeRestart('muffleWarning')
        }
      )
    }
  )(
    station = station,
    year = year,
    hourly = hourly,
    extra = extra,
    append_code = append_code
  )
  
  error_message = if (is.null(out$error)) {
    NA_character_
  } else {
    conditionMessage(out$error)
  }
  
  warning_message = if (length(warning_messages) == 0) {
    NA_character_
  } else {
    paste(unique(warning_messages), collapse = '; ')
  }
  
  list(
    data = out$result,
    warning_message = warning_message,
    error_message = error_message
  )
}


import_process_save_ghcnh_one_station = function(
    station,
    year,
    output_pth = NULL,
    preferred_report_type = 'FM12',
    precip_report_types = 'FM12',
    met_vars = c(
      wd   = 'wd',
      ws   = 'ws',
      T2   = 'air_temp',
      td2  = 'dew_point',
      PSFC = 'atmos_pres',
      rh2  = 'rh'
    ),
    allow_off_hour_obs = TRUE,
    hourly_tie_method = c(
      'before',
      'after',
      'mean'
    ),
    met_keep_qc_status = 'passed',
    precip_keep_qc_status = 'passed',
    precip_output = c(
      'aggregated',
      'disaggregated_hourly'
    ),
    same_len_messiness_threshold = 0,
    stop_on_precip_specific_qc = TRUE,
    stop_on_ambiguous_precip_qc = FALSE,
    remove_messy_accum_periods = TRUE,
    return_precip_qc = FALSE,
    return_precip_consolidated = FALSE,
    return_precip_hourly_selected_records = FALSE
) {
  
  if (is.null(names(met_vars)) || any(names(met_vars) == '')) {
    stop(
      '`met_vars` must be a named character vector where names are QAQC ',
      'variable names and values are GHCNh variable names.'
    )
  }
  
  if (
    length(preferred_report_type) != 1 ||
    is.na(preferred_report_type) ||
    preferred_report_type == ''
  ) {
    stop(
      '`preferred_report_type` must contain exactly one valid report type.'
    )
  }
  
  met_var_map = met_vars
  met_vars = unname(met_var_map)
  
  source_to_qaqc_var = character()
  
  make_log_row = function(
    status,
    output_file = NA_character_,
    error_message = NA_character_
  ) {
    
    tibble::tibble(
      code = station,
      year = year,
      output_file = output_file,
      status = status,
      error_message = error_message
    )
  }
  
  empty_result = function(
    status,
    error_message
  ) {
    
    list(
      processing_log = make_log_row(
        status = status,
        error_message = error_message
      ),
      precip_summary = tibble::tibble(),
      data_capture_summary = tibble::tibble(),
      hourly_selection_summary = tibble::tibble()
    )
  }
  
  rename_summary_vars = function(x) {
    
    if (is.null(x) || !'variable' %in% names(x)) {
      return(x)
    }
    
    x %>%
      dplyr::mutate(
        variable = dplyr::recode(
          variable,
          !!!source_to_qaqc_var
        )
      )
  }
  
  extra_met_vars = c(
    'sea_pres',
    'wet_bulb',
    'wg'
  )
  
  extra = any(met_vars %in% extra_met_vars)
  
  import_result = import_ghcn_hourly_safe(
    station = station,
    year = year,
    hourly = FALSE,
    extra = extra,
    append_code = TRUE
  )
  
  raw_data = import_result$data
  
  if (!is.na(import_result$error_message)) {
    
    return(
      empty_result(
        status = 'import_failed',
        error_message = cli::ansi_strip(
          import_result$error_message
        )
      )
    )
  }
  
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    
    import_message = dplyr::coalesce(
      import_result$warning_message,
      'No data were returned'
    )
    
    return(
      empty_result(
        status = 'no_data_returned',
        error_message = cli::ansi_strip(
          import_message
        )
      )
    )
  }
  
  raw_data = raw_data %>%
    select_ghcnh_processing_columns(
      met_vars = met_vars
    )
  
  available_report_types = raw_data %>%
    dplyr::select(dplyr::ends_with('_rt')) %>%
    unlist(use.names = FALSE) %>%
    stats::na.omit() %>%
    unique() %>%
    sort()
  
  processed = raw_data %>%
    process_ghcnh_one_station(
      year = year,
      station_code = station,
      met_vars = met_vars,
      preferred_report_type = preferred_report_type,
      precip_report_types = precip_report_types,
      allow_off_hour_obs = allow_off_hour_obs,
      hourly_tie_method = hourly_tie_method,
      met_keep_qc_status = met_keep_qc_status,
      precip_keep_qc_status = precip_keep_qc_status,
      precip_output = precip_output,
      same_len_messiness_threshold =
        same_len_messiness_threshold,
      stop_on_precip_specific_qc =
        stop_on_precip_specific_qc,
      stop_on_ambiguous_precip_qc =
        stop_on_ambiguous_precip_qc,
      remove_messy_accum_periods =
        remove_messy_accum_periods,
      return_precip_qc =
        return_precip_qc,
      return_precip_consolidated =
        return_precip_consolidated,
      return_precip_hourly_selected_records =
        return_precip_hourly_selected_records
    )
  
  met_var_map_available = met_var_map[
    unname(met_var_map) %in% names(processed$processed_data)
  ]
  
  met_var_map_missing = met_var_map[
    !unname(met_var_map) %in% names(processed$processed_data)
  ]
  
  source_to_qaqc_var = purrr::set_names(
    names(met_var_map_available),
    unname(met_var_map_available)
  )
  
  met_data_cols = unname(met_var_map_available)
  
  has_met_data = length(met_data_cols) > 0 &&
    any(
      purrr::map_lgl(
        processed$processed_data[met_data_cols],
        ~ any(!is.na(.x))
      )
    )
  
  has_precip_data = 'precip' %in% names(processed$processed_data) &&
    any(!is.na(processed$processed_data$precip))
  
  if (!has_met_data && !has_precip_data) {
    return(
      empty_result(
        status = 'no_suitable_data',
        error_message = paste0(
          'No meteorological or precipitation data remained after ',
          'report-type filtering and QC. Missing requested variables: ',
          paste(unname(met_var_map_missing), collapse = ', ')
        )
      )
    )
  }
  
  processed$processed_data = processed$processed_data %>%
    dplyr::rename(
      dplyr::all_of(met_var_map_available)
    )
  
  processed = processed %>%
    purrr::modify_at(
      c(
        'precip_summary',
        'data_capture_summary',
        'hourly_selection_summary'
      ),
      rename_summary_vars
    )
  
  if (!is.null(output_pth)) {
    
    output_pth = fs::path(output_pth)
    
    if (fs::path_ext(output_pth) != 'rds') {
      stop('`output_pth` must be a complete file path ending in .rds')
    }
    
    fs::dir_create(
      fs::path_dir(output_pth)
    )
    
    readr::write_rds(
      processed,
      output_pth
    )
    
    output_file = as.character(output_pth)
    
  } else {
    
    output_file = NA_character_
  }
  
  result = list(
    processing_log = make_log_row(
      status = 'processed',
      output_file = output_file
    ),
    precip_summary = processed$precip_summary,
    data_capture_summary = processed$data_capture_summary,
    hourly_selection_summary = processed$hourly_selection_summary %>%
      dplyr::mutate(
        code = station,
        year = year,
        .before = 1
      )
  )
  
  if (is.null(output_pth)) {
    result$processed = processed %>%
      purrr::list_modify(
        precip_summary = rlang::zap(),
        data_capture_summary = rlang::zap(),
        hourly_selection_summary = rlang::zap()
      )
  }
  
  result
}

import_process_save_ghcnh_one_station_safe = function(
    station,
    year,
    output_pth = NULL,
    ...
) {
  
  out = purrr::safely(
    import_process_save_ghcnh_one_station
  )(
    station = station,
    year = year,
    output_pth = output_pth,
    ...
  )
  
  if (!is.null(out$error)) {
    
    status = if (
      inherits(out$error, 'ghcnh_invalid_processed_dates')
    ) {
      'invalid_processed_dates'
    } else {
      'failed'
    }
    
    return(
      list(
        processing_log = tibble::tibble(
          code = station,
          year = year,
          output_file = if (is.null(output_pth)) {
            NA_character_
          } else {
            as.character(output_pth)
          },
          status = status,
          error_message = cli::ansi_strip(
            conditionMessage(out$error)
          )
        ),
        precip_summary = tibble::tibble(),
        data_capture_summary = tibble::tibble(),
        hourly_selection_summary = tibble::tibble()
      )
    )
  }
  
  out$result
}

interpret_ghcnh_precip_qc = function(
    precip_long,
    legacy_qc_flags = ghcnh_legacy_qc_flags,
    stop_on_precip_specific_qc = TRUE,
    stop_on_ambiguous_precip_qc = FALSE
) {
  
  precip_qc_system_check = detect_ghcnh_precip_qc_system(
    precip_long = precip_long,
    stop_on_precip_specific = stop_on_precip_specific_qc,
    stop_on_ambiguous = stop_on_ambiguous_precip_qc
  )
  
  legacy_lookup = legacy_qc_flags %>%
    dplyr::select(
      source_code,
      qc,
      source_code_group,
      worldmet_qc_meaning = qc_meaning,
      worldmet_qc_status = qc_status
    )
  
  precip_long %>%
    dplyr::mutate(
      source_code = as.character(sc),
      qc = as.character(qc)
    ) %>%
    dplyr::left_join(
      legacy_lookup,
      by = c('source_code', 'qc')
    ) %>%
    dplyr::mutate(
      qc_system = dplyr::case_when(
        is.na(amount) & is.na(qc) ~ 'none',
        !is.na(worldmet_qc_status) ~ 'worldmet_ghcnh_qc',
        !is.na(amount) & is.na(qc) ~ 'missing_qc',
        TRUE ~ 'unknown'
      ),
      qc_status = dplyr::case_when(
        qc_system == 'none' ~ NA_character_,
        qc_system == 'missing_qc' ~ 'missing_qc',
        qc_system == 'worldmet_ghcnh_qc' ~ worldmet_qc_status,
        TRUE ~ 'unknown'
      ),
      qc_meaning = dplyr::case_when(
        qc_system == 'none' ~ NA_character_,
        qc_system == 'missing_qc' ~ 'Value present but QC flag missing',
        qc_system == 'worldmet_ghcnh_qc' ~ worldmet_qc_meaning,
        TRUE ~ NA_character_
      ),
      precip_qc_system_assumption = precip_qc_system_check$assumed_qc_system,
      precip_qc_system_message = precip_qc_system_check$message
    ) %>%
    dplyr::select(
      row_id,
      date,
      precip_var,
      accum_hours,
      amount,
      mc,
      qc,
      rt,
      sc,
      si,
      qc_system,
      qc_status,
      qc_meaning,
      source_code_group,
      precip_qc_system_assumption,
      precip_qc_system_message
    )
}

interpret_ghcnh_qc = function(
    qc_data,
    legacy_qc_flags = ghcnh_legacy_qc_flags,
    integrated_qc_flags = ghcnh_integrated_qc_flags,
    derived_qc_flags = ghcnh_derived_qc_flags
) {
  
  legacy_lookup = legacy_qc_flags %>%
    dplyr::select(
      source_code,
      qc,
      source_code_group,
      legacy_qc_meaning = qc_meaning,
      legacy_qc_status = qc_status
    )
  
  integrated_lookup = integrated_qc_flags %>%
    dplyr::select(
      qc,
      integrated_qc_meaning = qc_meaning,
      integrated_qc_status = qc_status
    )
  
  derived_lookup = derived_qc_flags %>%
    dplyr::select(
      qc,
      derived_qc_meaning = qc_meaning,
      derived_qc_status = qc_status
    )
  
  rh_derived_sources = legacy_qc_flags %>%
    dplyr::distinct(source_code) %>%
    dplyr::pull(source_code) %>%
    as.character()
  
  qc_data %>%
    dplyr::mutate(
      qc = as.character(qc),
      source_code = as.character(source_code),
      rh_is_derived = variable == 'rh' & source_code %in% rh_derived_sources
    ) %>%
    dplyr::left_join(
      legacy_lookup,
      by = c('source_code', 'qc')
    ) %>%
    dplyr::left_join(
      integrated_lookup,
      by = 'qc'
    ) %>%
    dplyr::left_join(
      derived_lookup,
      by = 'qc'
    ) %>%
    dplyr::mutate(
      qc_system = dplyr::case_when(
        is.na(value) & is.na(qc) ~ 'none',
        variable == 'rh' & rh_is_derived & !is.na(derived_qc_status) ~ 'derived_variable',
        variable == 'rh' & rh_is_derived & !is.na(value) & is.na(qc) ~ 'derived_from_inputs',
        !is.na(value) & is.na(qc) ~ 'missing_qc',
        !is.na(legacy_qc_status) ~ 'legacy_source',
        !is.na(integrated_qc_status) ~ 'integrated_ghcnh',
        TRUE ~ 'unknown'
      ),
      qc_status = dplyr::case_when(
        qc_system == 'none' ~ NA_character_,
        qc_system == 'missing_qc' ~ 'missing_qc',
        qc_system == 'derived_from_inputs' ~ 'input_qc_required',
        qc_system == 'legacy_source' ~ legacy_qc_status,
        qc_system == 'derived_variable' ~ derived_qc_status,
        qc_system == 'integrated_ghcnh' ~ integrated_qc_status,
        TRUE ~ 'unknown'
      ),
      qc_meaning = dplyr::case_when(
        qc_system == 'none' ~ NA_character_,
        qc_system == 'missing_qc' ~ 'Value present but QC flag missing',
        qc_system == 'derived_from_inputs' ~ 'Relative humidity is derived; assess QC from air_temp and dew_point',
        qc_system == 'legacy_source' ~ legacy_qc_meaning,
        qc_system == 'derived_variable' ~ derived_qc_meaning,
        qc_system == 'integrated_ghcnh' ~ integrated_qc_meaning,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(
      row_id,
      station_id,
      station_name,
      date,
      variable,
      value,
      report_type,
      source_code,
      qc,
      qc_system,
      qc_status,
      qc_meaning,
      source_code_group
    )
}

make_ghcnh_met_clean = function(
    qc_check,
    keep_qc_status = c(
      'passed',
      'passed_gross',
      'accepted',
      'calculated'
    )
) {
  
  qc_check %>%
    dplyr::mutate(
      value = dplyr::if_else(
        qc_status %in% keep_qc_status | is.na(value),
        value,
        NA_real_
      )
    ) %>%
    dplyr::select(
      code = station_id,
      station_name,
      date,
      variable,
      value,
      report_type,
      source_code
    ) %>%
    dplyr::arrange(
      date,
      variable
    )
}

select_ghcnh_processing_columns = function(
    data,
    met_vars = c(
      'wd',
      'ws',
      'air_temp',
      'dew_point',
      'atmos_pres',
      'rh'
    )
) {
  
  met_code_suffixes = c(
    '_mc',
    '_qc',
    '_rt',
    '_sc',
    '_si'
  )
  
  met_var_cols = purrr::map(
    met_vars,
    ~ c(
      .x,
      stringr::str_c(.x, met_code_suffixes)
    )
  ) %>%
    unlist()
  
  precip_cols = names(data) %>%
    stringr::str_subset('^precip($|_)')
  
  cols_keep = c(
    'station_id',
    'station_name',
    'date',
    'lat',
    'lng',
    'elev',
    met_var_cols,
    precip_cols
  )
  
  data %>%
    dplyr::select(
      dplyr::any_of(cols_keep)
    )
}

regularise_ghcnh_hourly_precip = function(
    data,
    allow_off_hour = TRUE,
    tie_method = c(
      'before',
      'after',
      'mean'
    )
) {
  
  tie_method = match.arg(tie_method)
  
  required_cols = c(
    'date',
    'amount',
    'rt'
  )
  
  if (!all(required_cols %in% names(data))) {
    stop(
      '`data` must contain: ',
      paste(required_cols, collapse = ', ')
    )
  }
  
  data_checked = data %>%
    dplyr::filter(
      !is.na(amount)
    ) %>%
    dplyr::mutate(
      original_date = date,
      assigned_hour = lubridate::round_date(
        date,
        unit = 'hour'
      ),
      time_offset_seconds = as.numeric(
        difftime(
          original_date,
          assigned_hour,
          units = 'secs'
        )
      ),
      abs_time_offset_seconds = abs(
        time_offset_seconds
      ),
      exact_hour = time_offset_seconds == 0
    )
  
  selected = data_checked %>%
    dplyr::group_by(
      rt,
      assigned_hour
    ) %>%
    dplyr::group_modify(
      function(.x, .y) {
        
        exact_records = .x %>%
          dplyr::filter(
            exact_hour
          )
        
        if (nrow(exact_records) > 0) {
          
          return(
            exact_records %>%
              dplyr::slice_head(n = 1) %>%
              dplyr::mutate(
                hourly_selection_method = 'exact_hour'
              )
          )
        }
        
        if (!isTRUE(allow_off_hour)) {
          
          return(
            .x[0, ] %>%
              dplyr::mutate(
                hourly_selection_method = character()
              )
          )
        }
        
        minimum_offset = min(
          .x$abs_time_offset_seconds,
          na.rm = TRUE
        )
        
        nearest_records = .x %>%
          dplyr::filter(
            abs_time_offset_seconds == minimum_offset
          )
        
        if (nrow(nearest_records) == 1) {
          
          return(
            nearest_records %>%
              dplyr::mutate(
                hourly_selection_method = 'nearest'
              )
          )
        }
        
        if (tie_method == 'before') {
          
          return(
            nearest_records %>%
              dplyr::arrange(
                time_offset_seconds
              ) %>%
              dplyr::slice_head(n = 1) %>%
              dplyr::mutate(
                hourly_selection_method = 'tie_before'
              )
          )
        }
        
        if (tie_method == 'after') {
          
          return(
            nearest_records %>%
              dplyr::arrange(
                dplyr::desc(time_offset_seconds)
              ) %>%
              dplyr::slice_head(n = 1) %>%
              dplyr::mutate(
                hourly_selection_method = 'tie_after'
              )
          )
        }
        
        nearest_records %>%
          dplyr::summarise(
            date = as.POSIXct(NA, tz = 'UTC'),
            amount = mean(
              amount,
              na.rm = TRUE
            ),
            mc = dplyr::first(mc),
            qc = dplyr::first(qc),
            sc = dplyr::first(sc),
            si = dplyr::first(si),
            qc_system = dplyr::first(qc_system),
            qc_status = dplyr::first(qc_status),
            qc_meaning = dplyr::first(qc_meaning),
            source_code_group = dplyr::first(source_code_group),
            precip_qc_system_assumption =
              dplyr::first(precip_qc_system_assumption),
            precip_qc_system_message =
              dplyr::first(precip_qc_system_message),
            original_date = as.POSIXct(NA, tz = 'UTC'),
            time_offset_seconds = 0,
            abs_time_offset_seconds = minimum_offset,
            exact_hour = FALSE,
            hourly_selection_method = 'tie_mean',
            .groups = 'drop'
          )
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      date = assigned_hour
    ) %>%
    dplyr::arrange(
      rt,
      date
    )
  
  list(
    data = selected,
    diagnostics = tibble::tibble(
      n_input_values = nrow(data_checked),
      n_output_values = nrow(selected),
      n_exact_hour = sum(
        selected$hourly_selection_method == 'exact_hour',
        na.rm = TRUE
      ),
      n_nearest = sum(
        selected$hourly_selection_method == 'nearest',
        na.rm = TRUE
      ),
      n_tie_before = sum(
        selected$hourly_selection_method == 'tie_before',
        na.rm = TRUE
      ),
      n_tie_after = sum(
        selected$hourly_selection_method == 'tie_after',
        na.rm = TRUE
      ),
      n_tie_mean = sum(
        selected$hourly_selection_method == 'tie_mean',
        na.rm = TRUE
      )
    )
  )
}

regularise_ghcnh_hourly_records = function(
    data,
    preferred_report_type = 'FM12',
    allow_off_hour = TRUE,
    tie_method = c(
      'before',
      'after',
      'mean'
    )
) {
  
  tie_method = match.arg(tie_method)
  
  required_cols = c(
    'date',
    'variable',
    'value',
    'report_type'
  )
  
  if (!all(required_cols %in% names(data))) {
    stop(
      '`data` must contain: ',
      paste(required_cols, collapse = ', ')
    )
  }
  
  data_checked = data %>%
    dplyr::filter(
      !is.na(value)
    ) %>%
    dplyr::mutate(
      original_date = date,
      assigned_hour = lubridate::round_date(
        date,
        unit = 'hour'
      ),
      time_offset_seconds = as.numeric(
        difftime(
          original_date,
          assigned_hour,
          units = 'secs'
        )
      ),
      abs_time_offset_seconds = abs(
        time_offset_seconds
      ),
      exact_hour = time_offset_seconds == 0,
      report_type_rank = dplyr::if_else(
        report_type == preferred_report_type,
        1L,
        2L,
        missing = 2L
      )
    )
  
  selected_long = data_checked %>%
    dplyr::group_by(
      assigned_hour,
      variable
    ) %>%
    dplyr::group_modify(
      function(.x, .y) {
        
        exact_records = .x %>%
          dplyr::filter(exact_hour)
        
        # Exact-hour records always take precedence. If several exist,
        # preferred report type is used
        if (nrow(exact_records) > 0) {
          
          return(
            exact_records %>%
              dplyr::arrange(
                report_type_rank
              ) %>%
              dplyr::slice_head(n = 1) %>%
              dplyr::mutate(
                hourly_selection_method = 'exact_hour'
              )
          )
        }
        
        if (!isTRUE(allow_off_hour)) {
          
          return(
            .x[0, ] %>%
              dplyr::mutate(
                hourly_selection_method = character()
              )
          )
        }
        
        minimum_offset = min(
          .x$abs_time_offset_seconds,
          na.rm = TRUE
        )
        
        nearest_records = .x %>%
          dplyr::filter(
            abs_time_offset_seconds == minimum_offset
          )
        
        # If equally close observations exist, prefer preferred_report_type
        if (any(nearest_records$report_type == preferred_report_type, na.rm = TRUE)) {
          
          nearest_records = nearest_records %>%
            dplyr::filter(
              report_type == preferred_report_type
            )
        }
        
        if (nrow(nearest_records) == 1) {
          
          return(
            nearest_records %>%
              dplyr::mutate(
                hourly_selection_method = 'nearest'
              )
          )
        }
        
        if (tie_method == 'before') {
          
          return(
            nearest_records %>%
              dplyr::arrange(
                time_offset_seconds
              ) %>%
              dplyr::slice_head(n = 1) %>%
              dplyr::mutate(
                hourly_selection_method = 'tie_before'
              )
          )
        }
        
        if (tie_method == 'after') {
          
          return(
            nearest_records %>%
              dplyr::arrange(
                dplyr::desc(time_offset_seconds)
              ) %>%
              dplyr::slice_head(n = 1) %>%
              dplyr::mutate(
                hourly_selection_method = 'tie_after'
              )
          )
        }
        
        # Average only the remaining equally close values for this variable.
        nearest_records %>%
          dplyr::summarise(
            code = dplyr::first(code),
            station_name = dplyr::first(station_name),
            original_date = as.POSIXct(
              NA,
              tz = attr(date, 'tzone') %||% 'UTC'
            ),
            value = mean(
              value,
              na.rm = TRUE
            ),
            report_type = paste(
              sort(
                unique(
                  stats::na.omit(report_type)
                )
              ),
              collapse = ', '
            ),
            source_code = paste(
              sort(
                unique(
                  stats::na.omit(source_code)
                )
              ),
              collapse = ', '
            ),
            time_offset_seconds = 0,
            abs_time_offset_seconds = minimum_offset,
            exact_hour = FALSE,
            report_type_rank = min(
              report_type_rank,
              na.rm = TRUE
            ),
            hourly_selection_method = 'tie_mean',
            .groups = 'drop'
          )
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      date = assigned_hour
    )
  
  offset_counts = selected_long %>%
    dplyr::filter(
      time_offset_seconds != 0
    ) %>%
    dplyr::mutate(
      offset_minutes = time_offset_seconds / 60
    ) %>%
    dplyr::count(
      offset_minutes,
      name = 'n'
    ) %>%
    dplyr::arrange(
      offset_minutes
    )
  
  diagnostics = tibble::tibble(
    allow_off_hour = allow_off_hour,
    tie_method = tie_method,
    n_input_records = nrow(data),
    n_input_values = nrow(data_checked),
    n_output_values = nrow(selected_long),
    n_exact_hour = sum(
      selected_long$hourly_selection_method == 'exact_hour',
      na.rm = TRUE
    ),
    n_nearest = sum(
      selected_long$hourly_selection_method == 'nearest',
      na.rm = TRUE
    ),
    n_tie_before = sum(
      selected_long$hourly_selection_method == 'tie_before',
      na.rm = TRUE
    ),
    n_tie_after = sum(
      selected_long$hourly_selection_method == 'tie_after',
      na.rm = TRUE
    ),
    n_tie_mean = sum(
      selected_long$hourly_selection_method == 'tie_mean',
      na.rm = TRUE
    ),
    preferred_report_type = preferred_report_type,
    n_preferred_report_type_selected = sum(
      selected_long$report_type == preferred_report_type,
      na.rm = TRUE
    ),
    n_other_report_type_selected = sum(
      !is.na(selected_long$report_type) &
        selected_long$report_type != preferred_report_type
    ),
    n_off_hour_rejected = if (isTRUE(allow_off_hour)) {
      0L
    } else {
      sum(!data_checked$exact_hour)
    },
    offset_counts = list(offset_counts)
  )
  
  selected_records = selected_long %>%
    dplyr::select(
      -report_type_rank
    ) %>%
    dplyr::arrange(
      date,
      variable
    )
  
  selected_wide = selected_records %>%
    dplyr::select(
      code,
      station_name,
      date,
      variable,
      value
    ) %>%
    tidyr::pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    dplyr::arrange(date)
  
  list(
    data = selected_wide,
    diagnostics = diagnostics,
    selected_records = selected_records
  )
}

summarise_ghcnh_met_capture = function(
    processed_met,
    year,
    met_vars,
    code,
    station_name,
    timezone = 'UTC'
) {
  
  year_start = as.POSIXct(
    paste0(year, '-01-01 00:00:00'),
    tz = timezone
  )
  
  next_year_start = as.POSIXct(
    paste0(year + 1L, '-01-01 00:00:00'),
    tz = timezone
  )
  
  current_hour = lubridate::now(tzone = timezone) %>%
    lubridate::floor_date('hour')
  
  expected_end = if (year == lubridate::year(current_hour)) {
    min(current_hour, next_year_start)
  } else {
    next_year_start
  }
  
  n_expected = as.integer(
    difftime(
      expected_end,
      year_start,
      units = 'hours'
    )
  )
  
  met_vars_present = intersect(
    met_vars,
    names(processed_met)
  )
  
  processed_met %>%
    dplyr::select(
      dplyr::all_of(met_vars_present)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = 'variable',
      values_to = 'value'
    ) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      n_observed = sum(!is.na(value)),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      code = code,
      station_name = station_name,
      year = year,
      n_expected = n_expected,
      data_capture = n_observed / n_expected,
      .before = 1
    ) %>%
    dplyr::arrange(variable)
}

summarise_ghcnh_precip_capture = function(
    processed_precip,
    precip_accum_clean,
    precip_method,
    year,
    code,
    station_name,
    timezone = 'UTC'
) {
  
  year_start = as.POSIXct(
    paste0(year, '-01-01 00:00:00'),
    tz = timezone
  )
  
  next_year_start = as.POSIXct(
    paste0(year + 1L, '-01-01 00:00:00'),
    tz = timezone
  )
  
  current_hour = lubridate::now(tzone = timezone) %>%
    lubridate::floor_date('hour')
  
  expected_end = if (year == lubridate::year(current_hour)) {
    min(current_hour, next_year_start)
  } else {
    next_year_start
  }
  
  n_expected = as.integer(
    difftime(
      expected_end,
      year_start,
      units = 'hours'
    )
  )
  
  n_observed = dplyr::case_when(
    precip_method %in% c(
      'observed_hourly',
      'disaggregated_hourly'
    ) ~ sum(!is.na(processed_precip$precip)),
    
    precip_method == 'aggregated' ~ sum(
      precip_accum_clean$precip_code,
      na.rm = TRUE
    ),
    
    TRUE ~ 0
  )
  
  n_observed = min(
    as.integer(n_observed),
    n_expected
  )
  
  tibble::tibble(
    code = code,
    station_name = station_name,
    year = year,
    variable = 'precip',
    n_observed = n_observed,
    n_expected = n_expected,
    data_capture = n_observed / n_expected
  )
}

summarise_ghcnh_precip_compact = function(
    data,
    precip_qc,
    precip_accum_clean,
    precip_method,
    messy_accum_periods,
    longest_accum_hours,
    longest_coverage_ratio,
    station_code = NA_character_
) {
  
  station_name = data %>%
    dplyr::pull(station_name) %>%
    stats::na.omit() %>%
    unique()
  
  station_name = if (length(station_name) == 0) {
    NA_character_
  } else {
    station_name[1]
  }
  
  yr = data %>%
    dplyr::pull(date) %>%
    lubridate::year() %>%
    stats::na.omit() %>%
    unique()
  
  yr = if (length(yr) == 0) {
    NA_integer_
  } else {
    yr[1]
  }
  
  qc_system_assumption = precip_qc %>%
    dplyr::pull(precip_qc_system_assumption) %>%
    stats::na.omit() %>%
    unique()
  
  qc_system_assumption = if (length(qc_system_assumption) == 0) {
    NA_character_
  } else {
    qc_system_assumption[1]
  }
  
  retained_accum_periods = precip_accum_clean %>%
    dplyr::pull(precip_code) %>%
    unique() %>%
    sort()
  
  tibble::tibble(
    code = station_code,
    station_name = station_name,
    year = yr,
    qc_system_assumption = qc_system_assumption,
    precip_method = precip_method,
    longest_accum_hours = longest_accum_hours,
    longest_coverage_ratio = longest_coverage_ratio,
    messy_accum_periods = paste(
      sort(unique(messy_accum_periods)),
      collapse = ', '
    ),
    retained_accum_periods = paste(
      retained_accum_periods,
      collapse = ', '
    ),
    n_precip_accum_clean_records = nrow(precip_accum_clean)
  )
}

summarise_ghcnh_precip_periods = function(precip_long) {
  
  date_min = min(precip_long$date, na.rm = TRUE)
  date_max = max(precip_long$date, na.rm = TRUE)
  
  precip_long %>%
    dplyr::group_by(precip_var, accum_hours) %>%
    dplyr::summarise(
      n_records = sum(!is.na(amount)),
      total_amount = sum(amount, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      n_expected = purrr::map_int(
        accum_hours,
        ~ length(seq.POSIXt(
          from = date_min,
          to = date_max,
          by = paste(.x, 'hours')
        ))
      ),
      coverage_ratio = n_records / n_expected
    ) %>%
    dplyr::select(
      precip_var,
      accum_hours,
      n_records,
      n_expected,
      coverage_ratio,
      total_amount
    ) %>%
    dplyr::arrange(accum_hours)
}

summarise_ghcnh_precip_period_diagnostics = function(
    precip_period_summary,
    same_len_messiness_by_period
) {
  
  precip_period_summary %>%
    dplyr::left_join(
      same_len_messiness_by_period %>%
        dplyr::rename(
          accum_hours = precip_code,
          n_total_qced = n_total
        ),
      by = 'accum_hours'
    ) %>%
    dplyr::mutate(
      n_total_qced = dplyr::coalesce(n_total_qced, 0L),
      same_len_conflicts = dplyr::coalesce(same_len_conflicts, 0L),
      same_len_messiness_index = dplyr::coalesce(same_len_messiness_index, 0)
    ) %>%
    dplyr::arrange(accum_hours)
}

process_ghcnh_one_station = function(
    data,
    year,
    station_code = NA_character_,
    met_vars = c(
      'wd',
      'ws',
      'air_temp',
      'dew_point',
      'atmos_pres',
      'rh'
    ),
    met_keep_qc_status = 'passed',
    preferred_report_type = 'FM12',
    precip_report_types = 'FM12',
    precip_keep_qc_status = 'passed',
    precip_output = c(
      'aggregated',
      'disaggregated_hourly'
    ),
    allow_off_hour_obs = TRUE,
    hourly_tie_method = c(
      'before',
      'after',
      'mean'
    ),
    same_len_messiness_threshold = 0,
    stop_on_precip_specific_qc = TRUE,
    stop_on_ambiguous_precip_qc = FALSE,
    remove_messy_accum_periods = TRUE,
    return_precip_qc = FALSE,
    return_precip_consolidated = FALSE,
    return_precip_hourly_selected_records = FALSE
) {
  
  precip_output = match.arg(precip_output)
  hourly_tie_method = match.arg(hourly_tie_method)
  
  code = data %>%
    dplyr::pull(station_id) %>%
    stats::na.omit() %>%
    dplyr::first()
  
  station_name = data %>%
    dplyr::pull(station_name) %>%
    stats::na.omit() %>%
    dplyr::first()
  
  qc_check = data %>%
    get_ghcnh_qc_long(
      met_vars = met_vars
    ) %>%
    interpret_ghcnh_qc() %>%
    add_derived_rh_qc_status()
  
  processed_met_unregularised = qc_check %>%
    make_ghcnh_met_clean(
      keep_qc_status = met_keep_qc_status
    )
  
  hourly_selection = processed_met_unregularised %>%
    regularise_ghcnh_hourly_records(
      preferred_report_type = preferred_report_type,
      allow_off_hour = allow_off_hour_obs,
      tie_method = hourly_tie_method
    )
  
  processed_met = hourly_selection$data

  met_capture_summary = summarise_ghcnh_met_capture(
    processed_met = processed_met,
    year = year,
    met_vars = met_vars,
    code = code,
    station_name = station_name
  )
  
  precip_result = process_ghcnh_precip_one_station(
    data = data,
    station_code = station_code,
    keep_qc_status = precip_keep_qc_status,
    precip_report_types = precip_report_types,
    allow_off_hour_obs = allow_off_hour_obs,
    hourly_tie_method = hourly_tie_method,
    precip_output = precip_output,
    same_len_messiness_threshold = same_len_messiness_threshold,
    stop_on_precip_specific_qc = stop_on_precip_specific_qc,
    stop_on_ambiguous_precip_qc = stop_on_ambiguous_precip_qc,
    remove_messy_accum_periods = remove_messy_accum_periods,
    return_precip_qc = return_precip_qc,
    return_precip_consolidated = return_precip_consolidated,
    return_precip_hourly_selected_records =
      return_precip_hourly_selected_records
  )
  
  precip_capture_summary = summarise_ghcnh_precip_capture(
    processed_precip = precip_result$processed_precip,
    precip_accum_clean = precip_result$precip_accum_clean_for_summary,
    precip_method = precip_result$precip_method,
    year = year,
    code = code,
    station_name = station_name
  )
  
  data_capture_summary = dplyr::bind_rows(
    met_capture_summary,
    precip_capture_summary
  )
  
  processed_data = processed_met %>%
    dplyr::full_join(
      precip_result$processed_precip,
      by = 'date'
    ) %>%
    dplyr::arrange(date)
  
  validate_ghcnh_processed_dates(
    processed_data
  )
  
  out = list(
    processed_data = processed_data,
    hourly_selection_summary = hourly_selection$diagnostics,
    precip_period_diagnostics = precip_result$precip_period_diagnostics,
    precip_summary = precip_result$precip_summary,
    precip_accum_clean =
      precip_result$precip_accum_clean,
    precip_hourly_selection_diagnostics =
      precip_result$precip_hourly_selection_diagnostics,
    data_capture_summary = data_capture_summary
  )

  if (isTRUE(return_precip_qc)) {
    out$precip_qc = precip_result$precip_qc
  }
  
  if (isTRUE(return_precip_consolidated)) {
    out$precip_consolidated = precip_result$precip_consolidated
  }
  
  if (
    'precip_hourly_selected_records' %in%
    names(precip_result)
  ) {
    out$precip_hourly_selected_records =
      precip_result$precip_hourly_selected_records
  }
  
  out
}

process_ghcnh_precip_one_station = function(
    data,
    station_code = NA_character_,
    keep_qc_status = 'passed',
    precip_report_types = 'FM12',
    allow_off_hour_obs = TRUE,
    hourly_tie_method = c(
      'before',
      'after',
      'mean'
    ),
    precip_output = c(
      'aggregated',
      'disaggregated_hourly'
    ),
    same_len_messiness_threshold = 0,
    stop_on_precip_specific_qc = TRUE,
    stop_on_ambiguous_precip_qc = FALSE,
    remove_messy_accum_periods = TRUE,
    return_precip_qc = FALSE,
    return_precip_hourly_selected_records = FALSE,
    return_precip_consolidated = FALSE
) {
  
  precip_output = match.arg(precip_output)
  hourly_tie_method = match.arg(hourly_tie_method)
  
  if (
    length(precip_report_types) == 0 ||
    any(is.na(precip_report_types)) ||
    any(precip_report_types == '')
  ) {
    stop(
      '`precip_report_types` must contain at least one valid report type.'
    )
  }
  
  precip_report_types = unique(
    as.character(precip_report_types)
  )
  
  empty_processed_precip = tibble::tibble(
    date = as.POSIXct(character()),
    precip = numeric()
  )
  
  empty_precip_accum = tibble::tibble(
    report_type = character(),
    selected_for_output = logical(),
    date = as.POSIXct(character()),
    precip_code = integer(),
    precip_raw = numeric()
  )
  
  precip_long = data %>%
    dplyr::select(
      date,
      dplyr::matches('^precip($|_)')
    ) %>%
    get_ghcnh_precip_long()
  
  if (nrow(precip_long) == 0) {
    
    precip_summary = tibble::tibble(
      code = station_code,
      station_name = dplyr::first(
        stats::na.omit(data$station_name)
      ),
      year = dplyr::first(
        stats::na.omit(
          lubridate::year(data$date)
        )
      ),
      report_type = NA_character_,
      selected_for_output = FALSE,
      qc_system_assumption = NA_character_,
      precip_method = 'none',
      longest_accum_hours = NA_integer_,
      longest_coverage_ratio = NA_real_,
      messy_accum_periods = NA_character_,
      retained_accum_periods = NA_character_,
      n_precip_accum_clean_records = 0L
    )
    
    return(
      list(
        processed_precip = empty_processed_precip,
        precip_method = 'none',
        precip_accum_clean_for_summary = empty_precip_accum,
        precip_period_diagnostics = tibble::tibble(),
        precip_summary = precip_summary,
        precip_hourly_selection_diagnostics = tibble::tibble(),
        precip_hourly_selected_records = tibble::tibble(),
        precip_hourly_report_type_overlap = tibble::tibble()
      )
    )
  }
  
  precip_qc = precip_long %>%
    interpret_ghcnh_precip_qc(
      stop_on_precip_specific_qc =
        stop_on_precip_specific_qc,
      stop_on_ambiguous_precip_qc =
        stop_on_ambiguous_precip_qc
    )
  
  available_precip_report_types = precip_qc %>%
    dplyr::filter(
      !is.na(amount),
      !is.na(rt)
    ) %>%
    dplyr::distinct(rt) %>%
    dplyr::pull(rt)
  
  # Process accumulated precipitation independently for each
  # available report type.
  precip_results_by_report_type = purrr::map(
    available_precip_report_types,
    function(report_type) {
      
      precip_qc_one_report_type = precip_qc %>%
        dplyr::filter(
          rt == report_type
        )
      
      precip_period_summary_one_report_type =
        precip_qc_one_report_type %>%
        summarise_ghcnh_precip_periods()
      
      precip_consolidated_one_report_type =
        precip_qc_one_report_type %>%
        consolidate_ghcnh_precip_accum(
          keep_qc_status = keep_qc_status,
          include_hourly = FALSE
        )
      
      same_len_messiness_one_report_type =
        precip_consolidated_one_report_type %>%
        assess_precip_same_len_messiness_by_period()
      
      precip_period_diagnostics_one_report_type =
        summarise_ghcnh_precip_period_diagnostics(
          precip_period_summary =
            precip_period_summary_one_report_type,
          same_len_messiness_by_period =
            same_len_messiness_one_report_type
        ) %>%
        dplyr::mutate(
          report_type = report_type,
          selected_for_output =
            report_type %in% precip_report_types,
          .before = precip_var
        )
      
      messy_accum_periods_one_report_type =
        precip_period_diagnostics_one_report_type %>%
        dplyr::filter(
          same_len_messiness_index >
            same_len_messiness_threshold
        ) %>%
        dplyr::pull(accum_hours) %>%
        unique()
      
      precip_consolidated_for_cleaning_one_report_type =
        precip_consolidated_one_report_type
      
      if (
        isTRUE(remove_messy_accum_periods) &&
        length(messy_accum_periods_one_report_type) > 0
      ) {
        
        precip_consolidated_for_cleaning_one_report_type =
          precip_consolidated_for_cleaning_one_report_type %>%
          dplyr::filter(
            !precip_code %in%
              messy_accum_periods_one_report_type
          )
      }
      
      precip_accum_clean_one_report_type =
        precip_consolidated_for_cleaning_one_report_type %>%
        clean_precip_accum_windows() %>%
        dplyr::mutate(
          report_type = report_type,
          .before = 1
        ) %>%
        dplyr::select(
          -dplyr::any_of('rt')
        )
      
      list(
        report_type = report_type,
        precip_qc = precip_qc_one_report_type,
        precip_period_summary =
          precip_period_summary_one_report_type,
        precip_consolidated =
          precip_consolidated_one_report_type,
        precip_period_diagnostics =
          precip_period_diagnostics_one_report_type,
        messy_accum_periods =
          messy_accum_periods_one_report_type,
        precip_accum_clean =
          precip_accum_clean_one_report_type
      )
    }
  )
  
  names(precip_results_by_report_type) =
    available_precip_report_types
  
  precip_period_diagnostics =
    precip_results_by_report_type %>%
    purrr::map_dfr(
      'precip_period_diagnostics'
    )
  
  precip_accum_clean =
    precip_results_by_report_type %>%
    purrr::map_dfr(
      'precip_accum_clean'
    )
  
  # Regularise one-hour observations independently by report type.
  precip_hourly_selection = precip_qc %>%
    dplyr::filter(
      accum_hours == 1,
      qc_status %in% keep_qc_status,
      !is.na(amount),
      rt %in% precip_report_types
    ) %>%
    regularise_ghcnh_hourly_precip(
      allow_off_hour = allow_off_hour_obs,
      tie_method = hourly_tie_method
    )
  
  # Selected hourly report types must not overlap after
  # regularisation.
  precip_hourly_report_type_overlap =
    precip_hourly_selection$data %>%
    dplyr::filter(
      !is.na(amount)
    ) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      n_report_types = dplyr::n_distinct(rt),
      report_types = paste(
        sort(
          unique(
            stats::na.omit(rt)
          )
        ),
        collapse = ', '
      ),
      .groups = 'drop'
    ) %>%
    dplyr::filter(
      n_report_types > 1
    )
  
  if (nrow(precip_hourly_report_type_overlap) > 0) {
    stop(
      'Selected precipitation report types overlap at ',
      nrow(precip_hourly_report_type_overlap),
      ' final hourly timestamp(s): ',
      paste(precip_report_types, collapse = ', '),
      '. Only non-overlapping report types may be combined.'
    )
  }
  
  precip_hourly_observed =
    precip_hourly_selection$data %>%
    dplyr::transmute(
      date,
      precip = amount
    ) %>%
    dplyr::arrange(date)
  
  # Accumulated data selected by the user. These remain separated
  # by report type in precip_accum_clean_for_summary.
  precip_accum_clean_selected =
    precip_accum_clean %>%
    dplyr::filter(
      report_type %in% precip_report_types
    )
  
  selected_accum_report_types =
    precip_accum_clean_selected %>%
    dplyr::filter(
      !is.na(precip_raw)
    ) %>%
    dplyr::distinct(report_type) %>%
    dplyr::pull(report_type)
  
  # We have not defined a valid procedure for combining accumulated
  # precipitation from multiple report types. Stop rather than merge
  # them silently if accumulated data are needed for the output.
  if (
    nrow(precip_hourly_observed) == 0 &&
    length(selected_accum_report_types) > 1
  ) {
    stop(
      'No usable observed-hourly precipitation was found, but ',
      'accumulated precipitation is available from multiple selected ',
      'report types: ',
      paste(selected_accum_report_types, collapse = ', '),
      '. Select one precipitation report type for the accumulated output.'
    )
  }
  
  if (nrow(precip_hourly_observed) > 0) {
    
    processed_precip = precip_hourly_observed
    precip_method = 'observed_hourly'
    
  } else if (
    precip_output == 'disaggregated_hourly' &&
    nrow(precip_accum_clean_selected) > 0
  ) {
    
    processed_precip =
      precip_accum_clean_selected %>%
      dplyr::select(
        -dplyr::any_of(
          c(
            'report_type',
            'selected_for_output'
          )
        )
      ) %>%
      distribute_precip_accum_hourly()
    
    precip_method = 'disaggregated_hourly'
    
  } else if (nrow(precip_accum_clean_selected) > 0) {
    
    processed_precip =
      precip_accum_clean_selected %>%
      dplyr::transmute(
        date,
        precip = precip_raw
      ) %>%
      dplyr::arrange(date)
    
    precip_method = 'aggregated'
    
  } else {
    
    processed_precip = empty_processed_precip
    precip_method = 'none'
  }
  
  # Produce one compact summary row for every available report type.
  precip_summary = purrr::map_dfr(
    available_precip_report_types,
    function(report_type) {
      
      result_one_report_type =
        precip_results_by_report_type[[report_type]]
      
      diagnostics_one_report_type =
        result_one_report_type$precip_period_diagnostics
      
      longest_accum_summary =
        diagnostics_one_report_type %>%
        dplyr::filter(
          accum_hours > 1
        ) %>%
        dplyr::slice_max(
          order_by = accum_hours,
          n = 1,
          with_ties = FALSE
        )
      
      longest_accum_hours =
        if (nrow(longest_accum_summary) == 0) {
          NA_integer_
        } else {
          longest_accum_summary$accum_hours[1]
        }
      
      longest_coverage_ratio =
        if (nrow(longest_accum_summary) == 0) {
          NA_real_
        } else {
          longest_accum_summary$coverage_ratio[1]
        }
      
      selected_for_output =
        report_type %in% precip_report_types
      
      hourly_available =
        precip_hourly_selection$data %>%
        dplyr::filter(
          rt == report_type,
          !is.na(amount)
        ) %>%
        nrow() > 0
      
      accum_available =
        result_one_report_type$precip_accum_clean %>%
        dplyr::filter(
          !is.na(precip_raw)
        ) %>%
        nrow() > 0
      
      report_type_method =
        if (!selected_for_output) {
          'not_selected'
        } else if (hourly_available) {
          'observed_hourly'
        } else if (
          precip_output == 'disaggregated_hourly' &&
          accum_available
        ) {
          'disaggregated_hourly'
        } else if (accum_available) {
          'aggregated'
        } else {
          'none'
        }
      
      summary_one_report_type =
        summarise_ghcnh_precip_compact(
          data = data,
          precip_qc =
            result_one_report_type$precip_qc,
          precip_accum_clean =
            result_one_report_type$precip_accum_clean,
          precip_method = report_type_method,
          messy_accum_periods =
            result_one_report_type$messy_accum_periods,
          longest_accum_hours =
            longest_accum_hours,
          longest_coverage_ratio =
            longest_coverage_ratio,
          station_code = station_code
        )
      
      summary_one_report_type %>%
        dplyr::mutate(
          report_type = report_type,
          selected_for_output =
            selected_for_output,
          .before = precip_method
        )
    }
  )
  
  out = list(
    processed_precip = processed_precip,
    precip_method = precip_method,
    precip_summary = precip_summary,
    precip_period_diagnostics =
      precip_period_diagnostics,
    precip_accum_clean =
      precip_accum_clean,
    precip_hourly_selection_diagnostics =
      precip_hourly_selection$diagnostics
  )
  
  if (isTRUE(return_precip_qc)) {
    out$precip_qc = precip_qc
  }
  
  if (isTRUE(return_precip_consolidated)) {
    
    out$precip_consolidated =
      precip_results_by_report_type %>%
      purrr::map_dfr(
        function(x) {
          
          x$precip_consolidated %>%
            dplyr::mutate(
              report_type = x$report_type,
              selected_for_output =
                x$report_type %in%
                precip_report_types,
              .before = 1
            )
        }
      )
  }
  
  if (isTRUE(return_precip_hourly_selected_records)) {
    out$precip_hourly_selected_records =
      precip_hourly_selection$data
  }
  
  out
}

validate_ghcnh_processed_dates = function(processed_data) {
  
  duplicate_dates = processed_data %>%
    dplyr::count(date, name = 'n') %>%
    dplyr::filter(n > 1)
  
  non_hourly_dates = processed_data %>%
    dplyr::filter(
      lubridate::minute(date) != 0 |
        lubridate::second(date) != 0
    ) %>%
    dplyr::distinct(date)
  
  if (nrow(duplicate_dates) > 0) {
    rlang::abort(
      paste0(
        'Processed data contain ',
        nrow(duplicate_dates),
        ' duplicated hourly timestamps. ',
        'Only one observation per hour is permitted.'
      ),
      class = 'ghcnh_invalid_processed_dates'
    )
  }
  
  if (nrow(non_hourly_dates) > 0) {
    rlang::abort(
      paste0(
        'Processed data contain ',
        nrow(non_hourly_dates),
        ' timestamps that are not at the top of the hour. ',
        'All output timestamps must have minute and second equal to zero.'
      ),
      class = 'ghcnh_invalid_processed_dates'
    )
  }
  
  invisible(processed_data)
}