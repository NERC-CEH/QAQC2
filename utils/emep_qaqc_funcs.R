
# UTILS -------------------------------------------------------------------

add_grob_outer_margin = function(grob,
                                 top = 0.05,
                                 right = 0.25,
                                 bottom = 0.10,
                                 left = 0.25,
                                 units = 'in') {
  
  gridExtra::arrangeGrob(
    grid::nullGrob(), grid::nullGrob(), grid::nullGrob(),
    grid::nullGrob(), grob,             grid::nullGrob(),
    grid::nullGrob(), grid::nullGrob(), grid::nullGrob(),
    nrow = 3,
    ncol = 3,
    widths = grid::unit.c(
      grid::unit(left, units),
      grid::unit(1, 'null'),
      grid::unit(right, units)
    ),
    heights = grid::unit.c(
      grid::unit(top, units),
      grid::unit(1, 'null'),
      grid::unit(bottom, units)
    )
  )
}

add_map_grob_title = function(grob, title) {
  ggpubr::annotate_figure(
    grob,
    top = grid::textGrob(
      title,
      gp = grid::gpar(fontface = 'bold', fontsize = 12)
    )
  )
}

add_map_notice = function(p, text, bbox, where = c("centre","top","bottom"),
                          size = 3.8, fill = "white", colour = "black") {
  where = match.arg(where)
  # bbox can be from st_bbox(stars/sf); must contain xmin,xmax,ymin,ymax
  xm = (bbox[["xmin"]] + bbox[["xmax"]]) / 2
  ym = (bbox[["ymin"]] + bbox[["ymax"]]) / 2
  if (where == "top")    ym = bbox[["ymax"]] - 0.05*(bbox[["ymax"]]-bbox[["ymin"]])
  if (where == "bottom") ym = bbox[["ymin"]] + 0.05*(bbox[["ymax"]]-bbox[["ymin"]])
  
  p + ggplot2::annotate(
    "label",
    x = xm, y = ym,
    label = text,
    size = size,      # text size in mm/pts depending on theme
    label.size = 0.25,
    label.padding = grid::unit(6, "pt"),
    label.r = grid::unit(3, "pt"),
    fill = fill, colour = colour
  )
}

add_ox = function(
    site_dframe,
    units = 'ug/m3'
) {
  
  if (!all(
    c('no2', 'o3') %in% unique(site_dframe$var)
  )) {
    return(site_dframe)
  }
  
  site_dframe_ox = site_dframe %>%
    dplyr::filter(
      var %in% c('no2', 'o3')
    ) %>%
    tidyr::pivot_wider(
      id_cols = c(
        date,
        code
      ),
      names_from = var,
      values_from = c(
        obs,
        mod
      )
    )
  
  if (units == 'ug/m3') {
    
    site_dframe_ox = site_dframe_ox %>%
      dplyr::mutate(
        var = 'ox',
        obs = obs_o3 + obs_no2,
        mod = mod_o3 + mod_no2
      )
    
  } else if (units == 'ppb') {
    
    site_dframe_ox = site_dframe_ox %>%
      dplyr::mutate(
        var = 'ox(ppb)',
        obs = obs_o3 / 2 + obs_no2 / 1.9125,
        mod = mod_o3 / 2 + mod_no2 / 1.9125
      )
    
  } else {
    
    stop(
      'Units for Ox calculation can either be "ppb" or "ug/m3".',
      call. = FALSE
    )
  }
  
  site_dframe_ox = site_dframe_ox %>%
    dplyr::select(
      date,
      code,
      var,
      obs,
      mod
    )
  
  site_dframe %>%
    dplyr::bind_rows(
      site_dframe_ox
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(
      date
    )
}

add_plotly_interactive_controls = function(
    p,
    plot_title = NULL,
    export_width = 1200,
    export_height = 600,
    export_scale = 1,
    title_y = NULL,
    title_yanchor = NULL
) {
  
  p %>%
    plotly::config(
      displaylogo = FALSE
    ) %>%
    htmlwidgets::onRender(
      '
      function(el, x, data) {
        
        el.addEventListener(
          "dblclick",
          function() {
            
            const hoverOff =
              el._qaqcHoverOff === true;
            
            Plotly.relayout(
              el,
              {
                hovermode: hoverOff
                  ? "closest"
                  : false
              }
            );
            
            el._qaqcHoverOff =
              !hoverOff;
          }
        );
        
        
        if (!data.plot_title) {
          return;
        }
        
        
        const addExportHandler = function() {
          
          const button = el.querySelector(
            ".modebar-btn[data-title=\'Download plot as a png\']"
          );
          
          if (
            !button ||
            button._qaqcExportHandler
          ) {
            return;
          }
          
          button._qaqcExportHandler = true;
          
          button.addEventListener(
            "click",
            async function(event) {
              
              event.preventDefault();
              event.stopImmediatePropagation();
              
              const oldTitle =
                el.layout.title || null;
              
              const oldTopMargin =
                el.layout.margin &&
                el.layout.margin.t !== undefined
                  ? el.layout.margin.t
                  : null;
              
              const titleUpdate = {
                "title.text": data.plot_title,
                "title.x": 0.5,
                "title.xanchor": "center",
                "margin.t": Math.max(
                  oldTopMargin || 0,
                  60
                )
              };
              
              if (data.title_y !== null) {
                titleUpdate["title.y"] =
                  data.title_y;
              }
              
              if (data.title_yanchor !== null) {
                titleUpdate["title.yanchor"] =
                  data.title_yanchor;
              }
              
              await Plotly.relayout(
                el,
                titleUpdate
              );
              
              try {
                
                await Plotly.downloadImage(
                  el,
                  {
                    format: "png",
                    width: data.export_width,
                    height: data.export_height,
                    scale: data.export_scale
                  }
                );
                
              } finally {
                
                const restore = {};
                
                if (
                  oldTitle &&
                  oldTitle.text
                ) {
                  
                  restore["title.text"] =
                    oldTitle.text;
                  
                  restore["title.x"] =
                    oldTitle.x;
                  
                  restore["title.xanchor"] =
                    oldTitle.xanchor;
                  
                  restore["title.y"] =
                    oldTitle.y;
                  
                  restore["title.yanchor"] =
                    oldTitle.yanchor;
                  
                } else {
                  
                  restore["title.text"] = "";
                  restore["title.y"] = null;
                  restore["title.yanchor"] = null;
                }
                
                if (oldTopMargin !== null) {
                  restore["margin.t"] =
                    oldTopMargin;
                }
                
                await Plotly.relayout(
                  el,
                  restore
                );
              }
            },
            true
          );
        };
        
        
        addExportHandler();
        
        el.on(
          "plotly_afterplot",
          addExportHandler
        );
      }
      ',
      data = list(
        plot_title = plot_title,
        export_width = export_width,
        export_height = export_height,
        export_scale = export_scale,
        title_y = title_y,
        title_yanchor = title_yanchor
      )
    )
}

add_subprecip_long = function(mobs_df) {
  
  obs_prec = mobs_df %>%
    dplyr::filter(
      var == 'precip',
      scenario == 'obs'
    ) %>%
    dplyr::select(
      code,
      date,
      obs_val = value
    )
  
  sub_obs = obs_prec %>%
    dplyr::transmute(
      code,
      date,
      var = 'subprecip',
      scenario = 'obs',
      value = obs_val
    )
  
  sub_mod = mobs_df %>%
    dplyr::filter(
      var == 'precip',
      scenario == 'mod'
    ) %>%
    dplyr::left_join(
      obs_prec,
      by = c('code', 'date')
    ) %>%
    dplyr::transmute(
      code,
      date,
      var = 'subprecip',
      scenario = 'mod',
      value = dplyr::if_else(
        !is.na(obs_val),
        value,
        NA_real_
      )
    )
  
  dplyr::bind_rows(
    mobs_df,
    sub_obs,
    sub_mod
  )
}

align_summary_grid = function(
    x,
    reference
) {
  
  same_crs =
    sf::st_crs(x) ==
    sf::st_crs(reference)
  
  grid_sig = function(s) {
    
    xy = names(
      stars::st_dimensions(s)[
        stars::st_dimensions(s)$raster$dimensions
      ]
    )
    
    d = stars::st_dimensions(s)[xy]
    
    tibble::tibble(
      from = purrr::map_dbl(
        d,
        'from'
      ),
      to = purrr::map_dbl(
        d,
        'to'
      ),
      delta = purrr::map_dbl(
        d,
        'delta'
      ),
      offset = purrr::map_dbl(
        d,
        'offset'
      ),
      point = purrr::map_lgl(
        d,
        ~ isTRUE(.x$point)
      )
    )
  }
  
  same_grid = isTRUE(
    all.equal(
      grid_sig(x),
      grid_sig(reference),
      check.attributes = FALSE
    )
  )
  
  if (
    !same_crs ||
    !same_grid
  ) {
    
    x = stars::st_warp(
      x,
      dest = reference,
      use_gdal = TRUE
    )
  }
  
  x
}

apply_domain_crop = function(stars_object,
                             domain_crop = NULL) {
  
  if (!is.null(domain_crop)) {
    
    domain_crop = as_domain_polygon_sf(
      domain_crop,
      filter_name = 'domain crop'
    )
    
    domain_crop = sf::st_transform(
      domain_crop,
      sf::st_crs(stars_object)
    )
    
    stars_object = sf::st_crop(
      stars_object,
      sf::st_bbox(domain_crop)
    )
  }
  
  stars_object
}

apply_domain_filter_sites = function(sites_geo,
                                     domain_crop = NULL,
                                     domain_mask = NULL) {
  
  if (!is.null(domain_crop)) {
    
    crop_sf = sf::st_transform(
      domain_crop,
      sf::st_crs(sites_geo)
    )
    
    sites_geo = sf::st_filter(
      sites_geo,
      crop_sf,
      .predicate = sf::st_intersects
    )
  }
  
  if (!is.null(domain_mask)) {
    
    mask_sf = sf::st_transform(
      domain_mask,
      sf::st_crs(sites_geo)
    )
    
    sites_geo = sf::st_filter(
      sites_geo,
      mask_sf,
      .predicate = sf::st_intersects
    )
  }
  
  sites_geo
}

apply_domain_filters_stars = function(x,
                                      domain_crop = NULL,
                                      domain_mask = NULL,
                                      max_depth = 4,
                                      .depth = 0) {
  
  if (.depth > max_depth) {
    stop(
      'Maximum recursion depth exceeded in apply_domain_filters_stars(). ',
      'Check that the input object is not unexpectedly deeply nested.'
    )
  }
  
  if (inherits(x, 'stars')) {
    
    x = apply_domain_crop(
      stars_object = x,
      domain_crop = domain_crop
    )
    
    x = apply_domain_mask(
      stars_object = x,
      domain_mask = domain_mask
    )
    
    return(x)
  }
  
  if (is.list(x)) {
    
    return(
      purrr::map(
        x,
        apply_domain_filters_stars,
        domain_crop = domain_crop,
        domain_mask = domain_mask,
        max_depth = max_depth,
        .depth = .depth + 1
      )
    )
  }
  
  x
}

apply_domain_mask = function(stars_object,
                           domain_mask = NULL) {
  
  if (!is.null(domain_mask)) {
    
    domain_mask = as_domain_polygon_sf(
      domain_mask,
      filter_name = 'domain mask'
    )
    
    domain_mask = sf::st_transform(
      domain_mask,
      sf::st_crs(stars_object)
    )
    
    stars_object = stars_object[domain_mask]
  }
  
  stars_object
}

as_domain_polygon_sf = function(x,
                                filter_name = 'domain filter') {
  
  if (is.null(x) || identical(x, NA) || identical(x, '')) {
    return(NULL)
  }
  
  if (is.character(x)) {
    
    if (length(x) != 1) {
      stop(
        stringr::str_to_sentence(filter_name),
        ' must be a single file path, an sf object, or NULL.'
      )
    }
    
    if (!fs::file_exists(x)) {
      stop(
        stringr::str_to_sentence(filter_name),
        ' file not found: ',
        x
      )
    }
    
    x = sf::st_read(
      x,
      quiet = TRUE
    )
  }
  
  if (!inherits(x, 'sf')) {
    stop(
      stringr::str_to_sentence(filter_name),
      ' must be an sf object, a path to a file readable by sf::st_read(), or NULL.'
    )
  }
  
  geom_types = sf::st_geometry_type(
    x,
    by_geometry = TRUE
  )
  
  valid_geom = geom_types %in% c(
    'POLYGON',
    'MULTIPOLYGON'
  )
  
  if (!all(valid_geom)) {
    stop(
      stringr::str_to_sentence(filter_name),
      ' must contain only POLYGON or MULTIPOLYGON geometries.'
    )
  }
  
  if (is.na(sf::st_crs(x))) {
    stop(
      stringr::str_to_sentence(filter_name),
      ' has no coordinate reference system.'
    )
  }
  
  x
}

archive_log = function(log_pth) {
  # moves superseded log files into 'Log_archive' directory
  # automatically numbers old files to prevent overwriting (up to 99 log files)
  
  archive_dir = fs::path_dir(log_pth) %>%
    fs::path('Log_archive') %>%
    fs::dir_create()
  
  old_log_files = fs::dir_ls(archive_dir)
  
  if (fs::file_exists(log_pth)) {
    if (length(old_log_files) == 0) {
      fs::file_move(log_pth, fs::path(archive_dir, paste0('00', fs::path_file(log_pth))))
    } else {
      n = old_log_files %>%
        fs::path_file() %>%
        stringr::str_extract('\\d*') %>%
        as.integer() %>%
        max(na.rm = TRUE) + 1
      
      n_string = n %>%
        stringr::str_pad(2, side = 'left', pad = '0')
      
      fs::file_move(log_pth, fs::path(archive_dir, paste0(n_string, fs::path_file(log_pth))))
    }
  }
}

arrange_comp_map_output = function(plot_obj) {
  
  n_plots = length(plot_obj$plots)
  n_cols = if (n_plots == 1) 1 else 2
  n_rows = ceiling(n_plots / n_cols)
  
  ggpubr::ggarrange(
    plotlist = plot_obj$plots,
    ncol = n_cols,
    nrow = n_rows
  )
}

bind_mobs_rows = function(
    mobs_list,
    interval = FALSE
) {
  
  mobs_df = mobs_list %>%
    purrr::compact() %>%
    dplyr::bind_rows()
  
  if (nrow(mobs_df) > 0) {
    return(mobs_df)
  }
  
  if (interval) {
    
    return(
      tibble::tibble(
        code = character(),
        start_date = as.POSIXct(
          character(),
          tz = 'UTC'
        ),
        end_date = as.POSIXct(
          character(),
          tz = 'UTC'
        ),
        var = character(),
        obs = double(),
        mod = double(),
        interval_hours = double()
      )
    )
  }
  
  tibble::tibble(
    code = character(),
    date = as.POSIXct(
      character(),
      tz = 'UTC'
    ),
    var = character(),
    obs = double(),
    mod = double()
  )
}

build_emep_diff_list = function(
    domain_setup,
    emep_var,
    test_crs,
    ref_crs = NULL,
    test_file_tag = 'fullrun',
    ref_file_tag = 'fullrun',
    time_index_test = NULL,
    time_index_ref = NULL,
    z_index_test = NULL,
    z_index_ref = NULL,
    test_summary_fun = 'mean',
    ref_summary_fun = 'mean',
    diff_direction = 'test_minus_ref',
    convert_O3_to_ug = FALSE,
    domain_masks = NULL
) {
  
  diff_list = purrr::pmap(
    domain_setup %>%
      dplyr::select(
        domain,
        test_dir,
        ref_dir
      ),
    function(domain, test_dir, ref_dir) {
      
      test_fname = select_emep_file_from_dir(
        EMEP_dir = test_dir,
        file_tag = test_file_tag
      )
      
      ref_fname = if (!is.null(ref_dir)) {
        select_emep_file_from_dir(
          EMEP_dir = ref_dir,
          file_tag = ref_file_tag
        )
      } else {
        NULL
      }
      
      mask = if (!is.null(domain_masks)) {
        domain_masks[[domain]]
      } else {
        NULL
      }
      
      calculate_emep_diff_pair(
        emep_var = emep_var,
        test_fname = test_fname,
        ref_fname = ref_fname,
        test_crs = test_crs,
        ref_crs = ref_crs,
        time_index_test = time_index_test,
        time_index_ref = time_index_ref,
        z_index_test = z_index_test,
        z_index_ref = z_index_ref,
        mask = mask,
        run_labels = c('Test', 'Reference'),
        diff_direction = diff_direction,
        convert_O3_to_ug = convert_O3_to_ug,
        test_summary_fun = test_summary_fun,
        ref_summary_fun = ref_summary_fun
      )
    }
  )
  
  names(diff_list) = domain_setup$domain
  
  diff_list
}

ceiling_dec = function(x, level=1) round(x + 5*10^(-level-1), level)

calculate_cumsum = function(x) {
  if (all(is.na(x))) {
    rep(NA_real_, length(x))
  } else {
    cumsum(tidyr::replace_na(x, 0))
  }
}

calculate_emep_diff_pair = function(
    emep_var,
    test_fname,
    ref_fname = NULL,
    test_crs,
    ref_crs = NULL,
    time_index_test = NULL,
    time_index_ref = NULL,
    z_index_test = NULL,
    z_index_ref = NULL,
    test_summary_fun = 'mean',
    ref_summary_fun = 'mean',
    mask = NULL,
    run_labels = c('Test', 'Reference'),
    diff_direction = c(
      'test_minus_ref',
      'ref_minus_test'
    ),
    convert_O3_to_ug = FALSE
) {
  
  # Identify the two spatial dimensions.
  
  pick_2d_dims = function(s) {
    
    dn = names(
      stars::st_dimensions(s)
    )
    
    dn_l = tolower(dn)
    
    has_pair = function(a, b) {
      all(
        c(a, b) %in% dn_l
      )
    }
    
    pick = function(a, b) {
      dn[
        match(
          c(a, b),
          dn_l
        )
      ]
    }
    
    if (has_pair('i', 'j')) {
      return(
        pick('i', 'j')
      )
    }
    
    if (has_pair('x', 'y')) {
      return(
        pick('x', 'y')
      )
    }
    
    if (has_pair('e', 'n')) {
      return(
        pick('e', 'n')
      )
    }
    
    if (has_pair('lon', 'lat')) {
      return(
        pick('lon', 'lat')
      )
    }
    
    if (has_pair('longitude', 'latitude')) {
      return(
        pick('longitude', 'latitude')
      )
    }
    
    if (has_pair('rlon', 'rlat')) {
      return(
        pick('rlon', 'rlat')
      )
    }
    
    NULL
  }
  
  
  # Normalise optional dimension indices.
  
  normalize_index = function(
    x,
    max_length = Inf,
    index_name = 'index'
  ) {
    
    if (is.null(x)) {
      return(NULL)
    }
    
    if (is.list(x)) {
      x = x[[1]]
    }
    
    if (
      length(x) == 0 ||
      all(is.na(x))
    ) {
      return(NULL)
    }
    
    x = as.integer(x)
    
    if (length(x) > max_length) {
      
      stop(
        paste0(
          index_name,
          " must contain no more than ",
          max_length,
          " value",
          ifelse(
            max_length == 1,
            '',
            's'
          ),
          "."
        ),
        call. = FALSE
      )
    }
    
    x
  }
  
  
  # Resolve the requested temporal summary statistic.
  
  resolve_summary_fun = function(fun) {
    
    if (
      !is.character(fun) ||
      length(fun) != 1 ||
      is.na(fun)
    ) {
      
      stop(
        "summary_fun must be a single character string.",
        call. = FALSE
      )
    }
    
    fun_lower = tolower(fun)
    
    if (fun_lower == 'mean') {
      
      return(
        function(x) {
          mean(
            x,
            na.rm = TRUE
          )
        }
      )
    }
    
    if (fun_lower == 'min') {
      
      return(
        function(x) {
          min(
            x,
            na.rm = TRUE
          )
        }
      )
    }
    
    if (fun_lower == 'max') {
      
      return(
        function(x) {
          max(
            x,
            na.rm = TRUE
          )
        }
      )
    }
    
    if (fun_lower == 'median') {
      
      return(
        function(x) {
          stats::median(
            x,
            na.rm = TRUE
          )
        }
      )
    }
    
    if (
      grepl(
        '^p\\d{1,3}$',
        fun_lower
      )
    ) {
      
      p = as.numeric(
        sub(
          '^p',
          '',
          fun_lower
        )
      )
      
      if (
        p < 0 ||
        p > 100
      ) {
        
        stop(
          "Percentile summary must be between p0 and p100.",
          call. = FALSE
        )
      }
      
      return(
        function(x) {
          stats::quantile(
            x,
            probs = p / 100,
            na.rm = TRUE,
            names = FALSE
          )
        }
      )
    }
    
    stop(
      paste0(
        "Unsupported summary function: ",
        fun
      ),
      call. = FALSE
    )
  }
  
  
  # Read one EMEP field and reduce selected time steps
  # to a 2D summary.
  
  read2d = function(
    f,
    crs,
    t_idx,
    z_idx,
    mask,
    summary_fun,
    var_name
  ) {
    
    if (
      is.null(f) ||
      length(f) == 0 ||
      is.na(f) ||
      !fs::file_exists(f)
    ) {
      return(NULL)
    }
    
    x = read_emep(
      emep_fname = f,
      emep_var = var_name,
      emep_crs = crs,
      proxy = FALSE,
      time_index = t_idx,
      z_index = z_idx
    )
    
    if (is.null(x)) {
      return(NULL)
    }
    
    if (!is.null(mask)) {
      
      x = tryCatch(
        apply_area_mask(
          x,
          mask
        ),
        error = function(e) {
          x
        }
      )
    }
    
    rd = stars::st_dimensions(x)$raster$dimensions
    
    if (
      is.null(rd) ||
      length(rd) != 2
    ) {
      
      rd = pick_2d_dims(x)
      
      if (is.null(rd)) {
        
        stop(
          paste(
            "Could not determine 2D spatial dimensions. Dimensions are:",
            paste(
              names(
                stars::st_dimensions(x)
              ),
              collapse = ', '
            )
          ),
          call. = FALSE
        )
      }
    }
    
    summary_fun = resolve_summary_fun(
      summary_fun
    )
    
    stars::st_apply(
      x,
      rd,
      summary_fun
    )
  }
  
  
  diff_direction = match.arg(
    diff_direction
  )
  
  time_index_test = normalize_index(
    time_index_test,
    index_name = 'time_index_test'
  )
  
  time_index_ref = normalize_index(
    time_index_ref,
    index_name = 'time_index_ref'
  )
  
  z_index_test = normalize_index(
    z_index_test,
    max_length = 1,
    index_name = 'z_index_test'
  )
  
  z_index_ref = normalize_index(
    z_index_ref,
    max_length = 1,
    index_name = 'z_index_ref'
  )
  
  
  # Determine comparison type.
  
  if (length(emep_var) == 1) {
    
    comparison_type = 'testref'
    
    emep_var_test = emep_var
    emep_var_ref = emep_var
    
  } else if (length(emep_var) == 2) {
    
    comparison_type = 'ratio'
    
    emep_var_test = emep_var[1]
    emep_var_ref = emep_var[2]
    
  } else {
    
    stop(
      paste0(
        "emep_var must contain one variable for a Test/Reference ",
        "comparison or two variables for a within-run ratio."
      ),
      call. = FALSE
    )
  }
  
  
  # For a ratio, both variables come from the Test run and use
  # the same temporal and vertical selections.
  
  if (comparison_type == 'ratio') {
    
    test_s = read2d(
      f = test_fname,
      crs = test_crs,
      t_idx = time_index_test,
      z_idx = z_index_test,
      mask = mask,
      summary_fun = test_summary_fun,
      var_name = emep_var_test
    )
    
    ref_s = read2d(
      f = test_fname,
      crs = test_crs,
      t_idx = time_index_test,
      z_idx = z_index_test,
      mask = mask,
      summary_fun = test_summary_fun,
      var_name = emep_var_ref
    )
    
  } else {
    
    test_s = read2d(
      f = test_fname,
      crs = test_crs,
      t_idx = time_index_test,
      z_idx = z_index_test,
      mask = mask,
      summary_fun = test_summary_fun,
      var_name = emep_var_test
    )
    
    ref_s = read2d(
      f = ref_fname,
      crs = ref_crs,
      t_idx = time_index_ref,
      z_idx = z_index_ref,
      mask = mask,
      summary_fun = ref_summary_fun,
      var_name = emep_var_ref
    )
  }
  
  
  # Optional ozone conversion.
  
  if (
    convert_O3_to_ug &&
    emep_var_test == 'SURF_ppb_O3' &&
    !is.null(test_s)
  ) {
    
    test_s[[1]] = test_s[[1]] * 2
  }
  
  if (
    convert_O3_to_ug &&
    emep_var_ref == 'SURF_ppb_O3' &&
    !is.null(ref_s)
  ) {
    
    ref_s[[1]] = ref_s[[1]] * 2
  }
  
  if (comparison_type == 'ratio') {
    
    out = calculate_summary_ratio(
      numerator = test_s,
      denominator = ref_s,
      numerator_name = emep_var_test,
      denominator_name = emep_var_ref
    )
    
  } else {
    
    out = calculate_summary_diff(
      test_s = test_s,
      ref_s = ref_s,
      run_labels = run_labels,
      diff_direction = diff_direction
    )
  }
  
  
  # Attach EMEP-specific metadata.
  
  if (!is.null(out)) {
    
    attr(
      out,
      'emep_var_test'
    ) = emep_var_test
    
    attr(
      out,
      'emep_var_ref'
    ) = emep_var_ref
    
    attr(
      out,
      'comparison_type'
    ) = comparison_type
  }
  
  out
}

calculate_modstats = function(dframe,
                              modstats = MODSTATS_STATS,
                              type = 'default',
                              pretty_format = TRUE) {
  
  if (!all(c('obs', 'mod') %in% names(dframe))) {
    stop("`dframe` must contain columns named 'obs' and 'mod'.")
  }
  
  if (is.null(type)) {
    type = 'default'
  }
  
  type = purrr::compact(type)
  
  cor_stats = intersect(
    modstats,
    c('r_pearson', 'r_spearman')
  )
  
  standard_stats = setdiff(
    modstats,
    c('r_pearson', 'r_spearman', 'r')
  )
  
  # openair::modStats() uses `r`; keep support for old configs where
  # MODSTATS_STATS may still contain plain 'r'.
  if ('r' %in% modstats && !'r_pearson' %in% cor_stats) {
    cor_stats = c(cor_stats, 'r_pearson')
  }
  
  join_modstats = function(x, y) {
    
    by_cols = intersect(names(x), names(y))
    
    by_cols = setdiff(
      by_cols,
      c(
        'n',
        'FAC2',
        'MB',
        'NMB',
        'RMSE',
        'r',
        'r_pearson',
        'r_spearman',
        'p',
        'P'
      )
    )
    
    if (length(by_cols) == 0) {
      return(
        dplyr::bind_cols(
          x,
          y %>%
            dplyr::select(-dplyr::any_of(names(x)))
        )
      )
    }
    
    dplyr::left_join(x, y, by = by_cols)
  }
  
  get_r_stats = function(method) {
    
    # Calculate n first so that correlations are only calculated for groups
    # with enough paired values. This avoids openair warnings/errors for small n.
    n_stats = openair::modStats(
      dframe,
      statistic = 'n',
      type = type
    )
    
    if (identical(type, 'default')) {
      
      if (!isTRUE(n_stats$n > 2)) {
        return(NULL)
      }
      
      openair::modStats(
        dframe,
        statistic = 'r',
        type = type,
        method = method
      )
      
    } else {
      
      keep_grp = n_stats %>%
        dplyr::filter(n > 2) %>%
        dplyr::select(dplyr::all_of(type)) %>%
        dplyr::mutate(dplyr::across(where(is.factor), as.character))
      
      if (nrow(keep_grp) == 0) {
        return(NULL)
      }
      
      dframe %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(type), as.character)) %>%
        dplyr::semi_join(keep_grp, by = type) %>%
        openair::modStats(
          statistic = 'r',
          type = type,
          method = method
        ) %>%
        dplyr::mutate(dplyr::across(where(is.factor), as.character))
    }
  }
  
  out_list = list()
  
  if (length(standard_stats) > 0) {
    out_list[['standard']] = openair::modStats(
      dframe,
      statistic = standard_stats,
      type = type
    )
  }
  
  if ('r_pearson' %in% cor_stats) {
    
    r_pearson = get_r_stats(method = 'pearson')
    
    if (!is.null(r_pearson)) {
      out_list[['r_pearson']] = r_pearson %>%
        dplyr::rename(r_pearson = r)
    }
  }
  
  if ('r_spearman' %in% cor_stats) {
    
    r_spearman = get_r_stats(method = 'spearman')
    
    if (!is.null(r_spearman)) {
      out_list[['r_spearman']] = r_spearman %>%
        dplyr::rename(r_spearman = r)
    }
  }
  
  if (length(out_list) == 0) {
    stop('No valid model statistics requested.')
  }
  
  mobs_stats = purrr::reduce(
    out_list,
    join_modstats
  )
  
  if ('NMB' %in% names(mobs_stats)) {
    mobs_stats = mobs_stats %>%
      dplyr::mutate(NMB = 100 * NMB)
  }
  
  if (isTRUE(pretty_format)) {
    mobs_stats = mobs_stats %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(c('FAC2', 'r_pearson', 'r_spearman')),
          \(x) round(x, 2)
        ),
        dplyr::across(
          dplyr::any_of(c('MB', 'NMB', 'RMSE')),
          \(x) round(x, 1)
        ),
        dplyr::across(
          dplyr::any_of(c('p', 'P')),
          \(x) round(x, 4)
        )
      )
  }
  
  mobs_stats
}

calculate_summary_diff = function(
    test_s,
    ref_s = NULL,
    run_labels = c('Test', 'Reference'),
    diff_direction = c(
      'test_minus_ref',
      'ref_minus_test'
    )
) {
  
  diff_direction = match.arg(
    diff_direction
  )
  
  if (
    is.null(test_s) &&
    is.null(ref_s)
  ) {
    return(NULL)
  }
  
  if (
    is.null(test_s) ||
    is.null(ref_s)
  ) {
    
    out = list()
    
    if (!is.null(test_s)) {
      out[[run_labels[[1]]]] = test_s
    }
    
    if (!is.null(ref_s)) {
      out[[run_labels[[2]]]] = ref_s
    }
    
    return(out)
  }
  
  ref_s = align_summary_grid(
    x = ref_s,
    reference = test_s
  )
  
  names(test_s) = run_labels[[1]]
  names(ref_s) = run_labels[[2]]
  
  if (diff_direction == 'test_minus_ref') {
    
    abs_diff = test_s - ref_s
    denom = ref_s
    
  } else {
    
    abs_diff = ref_s - test_s
    denom = test_s
  }
  
  names(abs_diff) = 'abs_diff'
  
  rel_vals =
    as.numeric(abs_diff[[1]]) /
    pmax(
      abs(
        as.numeric(denom[[1]])
      ),
      .Machine$double.eps
    ) *
    100
  
  rel_diff = test_s
  
  rel_diff[[1]] = array(
    rel_vals,
    dim = dim(
      abs_diff[[1]]
    )
  )
  
  names(rel_diff) = 'rel_diff'
  
  if (
    all(
      dplyr::near(
        as.numeric(abs_diff[[1]]),
        0
      )
    )
  ) {
    
    abs_diff[[1]][] = NA_real_
    rel_diff[[1]][] = NA_real_
  }
  
  out = list()
  
  out[[run_labels[[1]]]] = test_s
  out[[run_labels[[2]]]] = ref_s
  out$abs_diff = abs_diff
  out$rel_diff = rel_diff
  
  out
}

calculate_summary_ratio = function(
    numerator,
    denominator,
    numerator_name,
    denominator_name
) {
  
  if (
    is.null(numerator) &&
    is.null(denominator)
  ) {
    return(NULL)
  }
  
  if (
    is.null(numerator) ||
    is.null(denominator)
  ) {
    
    out = list()
    
    if (!is.null(numerator)) {
      out[[run_labels[[1]]]] = numerator
    }
    
    if (!is.null(denominator)) {
      out[[run_labels[[2]]]] = denominator
    }
    
    return(out)
  }
  
  denominator = align_summary_grid(
    x = denominator,
    reference = numerator
  )
  
  names(numerator) = numerator_name
  names(denominator) = denominator_name
  
  ratio_vals =
    as.numeric(numerator[[1]]) /
    pmax(
      as.numeric(denominator[[1]]),
      .Machine$double.eps
    ) *
    100
  
  ratio = numerator
  
  ratio[[1]] = array(
    ratio_vals,
    dim = dim(
      numerator[[1]]
    )
  )
  
  names(ratio) = 'ratio'
  
  out = list()
  
  out[[numerator_name]] = numerator
  out[[denominator_name]] = denominator
  out$ratio = ratio
  
  out
}

calc_wd_diff = function(mod,
                        obs) {
  
  ((mod - obs + 180) %% 360) - 180
}

calculate_wrf_precip = function(wrf_frame,
                                out_var = 'precip',
                                rate = F,
                                first_step = c("zero","na")) {
  #calculates hourly precip in mm from RAINC and RAINCC
  
  first_step = match.arg(first_step)
  stopifnot(all(c("code","date","var","value") %in% names(wrf_frame)))
  
  has_vlev = "vertical_level" %in% names(wrf_frame)
  has_fid   = "file_id" %in% names(wrf_frame)
  group_cols = c("code", if (has_vlev) "vertical_level")
  
  wrf_wide = wrf_frame %>% 
    filter(.data$var %in% c("RAINNC","RAINC")) %>% 
    select(any_of(c(group_cols, "file_id", "date", "var", "value"))) %>%
    pivot_wider(names_from = var, values_from = value, values_fill = 0) %>% 
    arrange(dplyr::across(all_of(group_cols)), date)
  
  if (nrow(wrf_wide) == 0L) {
    out = tibble(code = character(),
                 date = as.POSIXct(character()),
                 var = character(),
                 value = numeric())
    if (has_vlev) out$vertical_level = character()
    if (has_fid)  out$file_id        = character()
    return(out)
  }
  
  out = wrf_wide %>%
    group_by(across(all_of(group_cols))) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      cum   = dplyr::coalesce(.data$RAINNC, 0) + dplyr::coalesce(.data$RAINC, 0),
      inc_raw   = cum - lag(cum),
      # First step in each file (lag is NA): 0 or NA, your choice
      inc   = case_when(
        is.na(lag(cum)) ~ if (first_step == "zero") 0 else NA_real_,
        inc_raw < 0         ~ if (first_step == "zero") 0 else NA_real_, # counter reset
        TRUE            ~ inc_raw
      ),
      # Optional: convert to rate using actual dt (hours) within the group
      dt_hr = c(NA_real_, as.numeric(diff(date), units = "secs")) / 3600,
      inc   = if (rate) if_else(is.na(dt_hr) | dt_hr <= 0, NA_real_, inc / dt_hr) else inc
    ) %>%
    ungroup() %>%
    transmute(
      across(all_of(group_cols)),
      date,
      var = out_var,
      value   = inc,
      across(any_of(c("file_id")))
    )
  
  out
}  

calculate_wrf_summary_diff_pair = function(
    wrf_var,
    test_fname,
    ref_fname = NULL,
    test_crs,
    ref_crs = NULL,
    run_labels = c('Test', 'Reference'),
    diff_direction = c(
      'test_minus_ref',
      'ref_minus_test'
    )
) {
  
  read2d = function(
    f,
    crs
  ) {
    
    if (
      is.null(f) ||
      is.na(f) ||
      !fs::file_exists(f)
    ) {
      return(NULL)
    }
    
    read_wrf_summary_file(
      pth = f,
      crs = crs
    )
  }
  
  test_s = read2d(
    f = test_fname,
    crs = test_crs
  )
  
  ref_s = read2d(
    f = ref_fname,
    crs = ref_crs %||% test_crs
  )
  
  out = calculate_summary_diff(
    test_s = test_s,
    ref_s = ref_s,
    run_labels = run_labels,
    diff_direction = diff_direction
  )
  
  if (!is.null(out)) {
    
    attr(
      out,
      'wrf_var'
    ) = wrf_var
  }
  
  out
}


categorise_emep_vars = function(emep_vars) {
  
  # Split EMEP variables into emissions, surface concentrations,
  # 3-D concentrations, deposition and miscellaneous categories,
  # and return them as columns in a tibble.
  
  emiss_vars = stringr::str_subset(
    emep_vars,
    '^Emis'
  )
  
  surf_vars = stringr::str_subset(
    emep_vars,
    '^SURF'
  )
  
  d3_vars = stringr::str_subset(
    emep_vars,
    '^D3'
  )
  
  dep_vars = stringr::str_subset(
    emep_vars,
    '^(W|D)DEP'
  )
  
  misc_vars = base::setdiff(
    emep_vars,
    c(
      emiss_vars,
      surf_vars,
      d3_vars,
      dep_vars
    )
  )
  
  emep_var_table = tibble::tibble(
    emiss_vars = list(
      sort(emiss_vars)
    ),
    surf_vars = list(
      sort(surf_vars)
    ),
    d3_vars = list(
      sort(d3_vars)
    ),
    dep_vars = list(
      sort(dep_vars)
    ),
    misc_vars = list(
      sort(misc_vars)
    )
  ) %>%
    tidyr::pivot_longer(
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      value = purrr::map(
        value,
        `length<-`,
        max(
          lengths(value)
        )
      )
    ) %>%
    tidyr::pivot_wider(
      names_from = name,
      values_from = value
    ) %>%
    tidyr::unnest(
      dplyr::everything()
    )
  
  emep_var_table
}

check_emep_crs = function(emep_file, submitted_crs, input_name) {
  
  grid_type = get_emep_grid_type(emep_file)
  
  expected_crs = if (grid_type == 'stereo') {
    MODEL_CRS_STEREO
  } else {
    MODEL_CRS_LONLAT
  }
  
  if (sf::st_crs(submitted_crs) != sf::st_crs(expected_crs)) {
    stop(
      glue::glue(
        "{input_name} does not match the grid structure detected in:\n",
        "{emep_file}\n\n",
        "Detected grid type: {grid_type}"
      ),
      call. = FALSE
    )
  }
}

clean_wrf_run_dir = function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == '') {
    return(NA_character_)
  }
  
  # Drop final directory if it is called WRF
  if (fs::path_file(x) == 'WRF') {
    x = fs::path_dir(x)
  }
  
  x
}

collate_emep_mobs = function(
    nc_pth,
    site_code,
    i_index,
    j_index,
    obs_file,
    obs_var,
    emep_var,
    acceptable_model_coverage = 0.9
) {
  
  if (stringr::str_detect(
    emep_var,
    '^D3_'
  )) {
    
    stop(
      'D3 variables are not currently supported for MOBS collation.',
      call. = FALSE
    )
  }
  
  checkmate::assert_number(
    acceptable_model_coverage,
    lower = 0,
    upper = 1
  )
  
  # Model data.
  
  nc = ncdf4::nc_open(
    nc_pth
  )
  
  on.exit(
    ncdf4::nc_close(nc),
    add = TRUE
  )
  
  nc_date = get_emep_floordate(
    nc_pth
  )
  
  mod_data = ncdf4::ncvar_get(
    nc,
    varid = emep_var,
    start = c(
      i_index,
      j_index,
      1
    ),
    count = c(
      1,
      1,
      -1
    )
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      code = site_code,
      .before = value
    ) %>%
    dplyr::rename(
      mod = value
    ) %>%
    dplyr::bind_cols(
      nc_date,
      .
    )
  
  # EMEP O3 is in ppb; observation repository concentrations are ug m-3.
  
  if (stringr::str_detect(
    emep_var,
    'ppb_O3'
  )) {
    
    mod_data = mod_data %>%
      dplyr::mutate(
        mod = mod * 2
      )
  }
  
  # Observation data.
  
  obs_data = tryCatch(
    {
      
      read_processed_obs(
        obs_file
      )
      
    },
    error = function(e) {
      NULL
    }
  )
  
  if (is.null(obs_data)) {
    
    logger::log_warn(
      glue::glue(
        "No data for {obs_var} at {site_code} (file {obs_file})"
      )
    )
    
    return(
      mod_data %>%
        dplyr::mutate(
          code = site_code,
          var = obs_var,
          obs = NA_real_,
          .before = mod
        )
    )
  }
  
  has_date = 'date' %in% names(obs_data)
  has_start = 'start_date' %in% names(obs_data)
  has_end = 'end_date' %in% names(obs_data)
  
  if (has_start != has_end) {
    stop(
      'Observation data must contain both start_date and end_date ',
      'when either is supplied: ',
      obs_file,
      call. = FALSE
    )
  }
  
  if (!has_date && !has_start) {
    stop(
      'Observation data must contain either date or ',
      'start_date and end_date: ',
      obs_file,
      call. = FALSE
    )
  }
  
  obs_resolution = get_obs_file_time_resolution(
    obs_file
  )
  
  if (obs_resolution == 'year') {
    stop(
      'Annual observations are not supported for MOBS collation.',
      call. = FALSE
    )
  }
  
  
  # Convert monthly observations to explicit observation intervals.
  
  if (!has_start && obs_resolution == 'month') {
    
    obs_data = obs_data %>%
      dplyr::mutate(
        start_date = lubridate::floor_date(
          date,
          unit = 'month'
        ),
        end_date = start_date %m+%
          lubridate::period(months = 1)
      )
    
    has_start = TRUE
    has_end = TRUE
  }
  
  
  # Regular observations.
  
  if (!has_start) {
    
    obs_data = obs_data %>%
      dplyr::select(
        date,
        dplyr::all_of(obs_var)
      ) %>%
      dplyr::rename(
        obs = dplyr::all_of(obs_var)
      )
    
    both = mod_data %>%
      dplyr::left_join(
        obs_data,
        by = 'date'
      ) %>%
      dplyr::mutate(
        code = site_code,
        var = obs_var
      ) %>%
      dplyr::relocate(
        mod,
        .after = obs
      )
    
    return(both)
  }
  
  
  # Observation intervals.
  
  obs_data = obs_data %>%
    dplyr::select(
      start_date,
      end_date,
      dplyr::all_of(obs_var)
    ) %>%
    dplyr::rename(
      obs = dplyr::all_of(obs_var)
    )
  
  interval_data = obs_data %>%
    dplyr::mutate(
      interval_id = dplyr::row_number(),
      date = purrr::map2(
        start_date,
        end_date,
        function(start_date, end_date) {
          
          seq(
            from = start_date,
            to = end_date,
            by = obs_resolution
          ) %>%
            head(-1)
        }
      )
    ) %>%
    tidyr::unnest(
      date
    )
  
  # Match the temporary observation timestamps to the model data.
  
  interval_data = interval_data %>%
    dplyr::left_join(
      mod_data %>%
        dplyr::select(
          date,
          mod
        ),
      by = 'date'
    )
  
  # Return to the original observation intervals. The model value is only
  # retained when model data are available for the complete interval.
  
  both = interval_data %>%
    dplyr::group_by(
      interval_id,
      start_date,
      end_date
    ) %>%
    dplyr::summarise(
      obs = dplyr::first(obs),
      model_coverage = mean(is.finite(mod)),
      mod = if (
        model_coverage >= acceptable_model_coverage
      ) {
        mean(
          mod,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      code = site_code,
      var = obs_var
    ) %>%
    dplyr::select(
      start_date,
      end_date,
      code,
      obs,
      mod,
      var
    )
  
  both
}

compare_file_size = function(
    test_dir,
    ref_dir = NULL,
    valid_tags = c('fullrun', 'month', 'day', 'hour', 'hourInst')
) {
  if (is.null(test_dir) || !fs::dir_exists(test_dir)) return(NULL)
  
  dirs = list(test = test_dir)
  if (!is.null(ref_dir) && fs::dir_exists(ref_dir)) {
    dirs$ref = ref_dir
  }
  
  d_content = purrr::map_dfr(dirs, fs::dir_info, .id = 'run') %>%
    dplyr::select(run, path, size)
  
  d_content2 = d_content %>%
    dplyr::filter(stringr::str_detect(path, '\\.nc$')) %>%
    dplyr::mutate(
      fname = fs::path_file(path),
      fname = fs::path_ext_remove(fname),
      fname = stringr::word(fname, start = -1, sep = stringr::fixed('_')),
      size = as.double(size)
    ) %>%
    dplyr::filter(fname %in% valid_tags) %>%
    tidyr::pivot_wider(id_cols = fname, names_from = run, values_from = size)
  
  if (all(c('test', 'ref') %in% names(d_content2))) {
    d_content2 = d_content2 %>%
      dplyr::mutate(
        abs_diff = fs::fs_bytes(abs(test - ref)),
        rel_diff = round((test - ref) / ref * 100, 1),
        test = fs::fs_bytes(test),
        ref = fs::fs_bytes(ref)
      )
  } else {
    d_content2 = d_content2 %>%
      dplyr::mutate(test = fs::fs_bytes(test))
  }
  
  d_content2 %>%
    dplyr::mutate(fname = factor(fname, levels = valid_tags)) %>%
    dplyr::arrange(fname)
}

compare_MBS = function(
    test_MBS,
    ref_MBS
) {
  
  test_MBS %>%
    dplyr::select(
      species,
      test_mass = mass,
      test_unit = unit
    ) %>%
    dplyr::full_join(
      ref_MBS %>%
        dplyr::select(
          species,
          ref_mass = mass,
          ref_unit = unit
        ),
      by = 'species'
    ) %>%
    dplyr::mutate(
      unit = dplyr::coalesce(test_unit, ref_unit),
      abs_diff = test_mass - ref_mass,
      rel_diff = dplyr::if_else(
        ref_mass == 0,
        NA_real_,
        100 * (test_mass - ref_mass) / ref_mass
      )
    ) %>%
    dplyr::select(
      species,
      test_mass,
      ref_mass,
      unit,
      abs_diff,
      rel_diff
    ) %>%
    dplyr::arrange(species)
}

compare_reprojection_resolution = function(
    x,
    factors = c(1, 1.5, 2, 3, 4),
    crs = 3857
) {
  
  original_range = range(
    x[[1]],
    na.rm = TRUE
  )
  
  x_base = stars::st_warp(
    x,
    crs = crs,
    method = 'near',
    use_gdal = TRUE
  )
  
  base_bbox = sf::st_bbox(
    x_base
  )
  
  base_dim = dim(x_base)[1:2]
  
  purrr::map_dfr(
    factors,
    function(factor) {
      
      new_dim = round(
        base_dim * factor
      )
      
      dest = stars::st_as_stars(
        base_bbox,
        nx = new_dim[1],
        ny = new_dim[2]
      )
      
      x_reprojected = stars::st_warp(
        x,
        dest = dest,
        method = 'near',
        use_gdal = TRUE
      )
      
      reprojected_range = range(
        x_reprojected[[1]],
        na.rm = TRUE
      )
      
      tibble::tibble(
        factor = factor,
        nx = dim(x_reprojected)[1],
        ny = dim(x_reprojected)[2],
        cells = prod(dim(x_reprojected)),
        minimum = reprojected_range[1],
        maximum = reprojected_range[2],
        minimum_difference = reprojected_range[1] - original_range[1],
        maximum_difference = reprojected_range[2] - original_range[2]
      )
    }
  )
}

compare_reprojection_stats = function(
    x,
    crs = 3857,
    method = 'near'
) {
  
  x_reprojected = stars::st_warp(
    x,
    crs = crs,
    method = method,
    use_gdal = TRUE
  )
  
  get_stats = function(x) {
    
    vals = as.numeric(
      x[[1]]
    )
    
    vals = vals[
      is.finite(vals)
    ]
    
    c(
      min = min(vals),
      mean = mean(vals),
      max = max(vals)
    )
  }
  
  original = get_stats(x)
  reprojected = get_stats(x_reprojected)
  
  tibble::tibble(
    statistic = c('Minimum', 'Mean', 'Maximum'),
    original = original,
    reprojected = reprojected,
    difference = reprojected - original,
    percent_difference = 100 * difference / original
  )
}

convert_mass_from_mg = function(x, out_unit = 'Gg') {
  
  if (out_unit == 'mg') return(x)
  if (out_unit == 'g')  return(x / 1e3)
  if (out_unit == 'kg') return(x / 1e6)
  if (out_unit == 'Mg') return(x / 1e9)
  if (out_unit == 'Gg') return(x / 1e12)
  
  stop(
    "Unsupported output unit: ",
    out_unit
  )
}

create_blank_plot = function() {
  blank_plot = ggplot() +
    geom_blank(aes(1,1)) +
    theme(
      plot.background = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(), 
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )
  
}

decimal_count <- function(value) sapply(value, decimal_counter)
#from skgrange/threadr

# The worker
decimal_counter <- function(x) {
  
  # Check
  stopifnot(class(x) == "numeric")
  
  # If NA, return NA
  if (is.na(x)) {
    
    x <- NA
    
  } else {
    
    # If contains a period
    if (grepl("\\.", x)) {
      
      x <- stringr::str_replace(x, "0+$", "")
      x <- stringr::str_replace(x, "^.+[.]", "")
      x <- stringr::str_length(x)
      
    } else {
      
      # Otherwise return zero
      x <- 0
      
    }
    
  }
  
  # Return
  x
  
}

download_auto_site_year = function(
    site_code,
    site_name,
    year,
    network,
    poll_vec,
    out_root
) {
  
  message(
    glue::glue(
      "Processing {stringr::str_to_upper(network)}/{site_code} for {year}"
    )
  )
  
  dat = tryCatch(
    get_auto_data(
      site = site_code,
      network = network,
      pollutant = poll_vec,
      start_year = year,
      end_year = year
    ),
    error = function(e) {
      
      logger::log_warn(
        glue::glue(
          "Failed to download {network}/{site_code} for {year}: ",
          "{conditionMessage(e)}"
        )
      )
      
      NULL
    }
  )
  
  if (is.null(dat) || nrow(dat) == 0) {
    message("No data returned; site skipped.")
    return(NULL)
  }
  
  # openair returns CO in mg m-3; repository concentrations use µg m-3
  if ('co' %in% names(dat)) {
    dat = dat %>%
      dplyr::mutate(
        co = co * 1000
      )
  }
  
  network_id = stringr::str_to_upper(
    network
  )
  
  site_clean = sanitise_string(
    site_name,
    case = 'title',
    separator = '_'
  )
  
  output_pth = fs::path(
    out_root,
    year,
    network_id,
    'Data',
    paste0(
      site_clean,
      '_',
      site_code,
      '_',
      year,
      '_processed.rds'
    )
  )
  
  fs::dir_create(
    fs::path_dir(output_pth),
    recurse = TRUE
  )
  
  readr::write_rds(
    dat,
    output_pth
  )
  
  message("Saved.")
  
  output_pth
}

emep_file_has_temporal_dim = function(nc_file) {
  
  nc = ncdf4::nc_open(nc_file)
  
  on.exit(
    ncdf4::nc_close(nc),
    add = TRUE
  )
  
  if (!'time' %in% names(nc$dim)) {
    return(FALSE)
  }
  
  nc$dim$time$len > 1
}

extend_map_breaks_to_data_range = function(breaks,
                                           data_min,
                                           data_max) {
  
  breaks = as.numeric(unlist(breaks, use.names = FALSE))
  breaks = sort(unique(breaks[is.finite(breaks)]))
  
  if (length(breaks) < 2) {
    stop('At least two finite map breaks are required.')
  }
  
  break_precision = decimal_count(breaks)
  
  precision_l = if (length(breaks) >= 2) {
    break_precision[2]
  } else {
    break_precision[1]
  }
  
  precision_h = break_precision[length(break_precision)]
  
  if (is.finite(data_max) && data_max > dplyr::last(breaks)) {
    breaks[length(breaks)] = ceiling_dec(data_max, precision_h)
  }
  
  if (is.finite(data_min) && data_min < dplyr::first(breaks)) {
    breaks[1] = floor_dec(data_min, precision_l)
  }
  
  sort(unique(breaks))
}

extract_runlog_emissions = function(log_pth) {
  
  log_lines = readLines(
    log_pth,
    warn = FALSE
  )
  
  blocks = extract_runlog_emission_blocks(
    log_lines
  )
  
  if (length(blocks) == 0) {
    return(tibble::tibble())
  }
  
  combined_blocks = blocks[
    purrr::map_chr(
      blocks,
      'table_type'
    ) == 'combined'
  ]
  
  source_blocks = blocks[
    purrr::map_chr(
      blocks,
      'table_type'
    ) == 'source'
  ]
  
  # Preferred approach:
  # use EMEP's combined emissions table if available.
  if (length(combined_blocks) > 0) {
    
    return(
      combined_blocks %>%
        purrr::map_dfr(
          parse_combined_runlog_emission_block
        )
    )
  }
  
  # Fall back to the existing source-file parser.
  source_blocks %>%
    purrr::map_dfr(
      function(block) {
        parse_annual_runlog_emission_block(
          block$lines
        )
      }
    ) %>%
    dplyr::mutate(
      runlog_method = 'sum_source_tables'
    )
}

extract_runlog_emission_blocks = function(log_lines) {
  
  header_idx = which(
    stringr::str_detect(
      log_lines,
      '^#(?:EMTBL|EMSUM) Total emissions by countries'
    )
  )
  
  if (length(header_idx) == 0) {
    return(list())
  }
  
  block_end_idx = c(
    header_idx[-1] - 1,
    length(log_lines)
  )
  
  purrr::map2(
    header_idx,
    block_end_idx,
    function(start_idx, end_idx) {
      
      block = log_lines[start_idx:end_idx]
      header = block[1]
      
      list(
        lines = block,
        table_type = dplyr::if_else(
          stringr::str_detect(
            header,
            'all Emis_source files'
          ),
          'combined',
          'source'
        ),
        table_prefix = stringr::str_match(
          header,
          '^#(EMTBL|EMSUM)'
        )[, 2],
        header = header
      )
    }
  )
}

extract_temporal_emep_points = function(
    emep_file,
    area_file,
    emep_crs,
    configured_vars,
    points,
    var_params_list,
    mass_quantity = c(
      'area_normalised',
      'total'
    ),
    total_out_unit = 'Gg'
) {
  
  if (
    is.null(points) ||
    nrow(points) == 0
  ) {
    return(tibble::tibble())
  }
  
  mass_quantity = match.arg(
    mass_quantity
  )
  
  available_emep_vars = get_emep_vars(
    nc_file = emep_file,
    var_keywords = NULL
  )
  
  temporal_vars = intersect(
    available_emep_vars,
    configured_vars
  )
  
  if (length(temporal_vars) == 0) {
    return(tibble::tibble())
  }
  
  # Use the first requested variable to identify which points fall within
  # the model domain. For a 3-D variable, use its configured vertical level.
  
  first_var = temporal_vars[[1]]
  
  first_z_index = get_var_param(
    var = first_var,
    key = 'z_index',
    var_params_list = var_params_list,
    default = NULL
  )
  
  if (
    stringr::str_detect(
      first_var,
      '^D3_'
    ) &&
    is.null(first_z_index)
  ) {
    stop(
      "No z_index is defined for temporal variable '",
      first_var,
      "' in the variable parameters file.",
      call. = FALSE
    )
  }
  
  emep_2d_slice = read_emep(
    emep_fname = emep_file,
    emep_var = first_var,
    emep_crs = emep_crs,
    proxy = FALSE,
    driver = 'gdal'
  ) %>%
    get_emep_2d_slice(
      z_index = first_z_index %||% 1
    )
  
  points = get_sites_in_domain(
    site_geo = points,
    emep_2d_slice = emep_2d_slice
  )
  
  if (nrow(points) == 0) {
    return(tibble::tibble())
  }
  
  point_indexes = purrr::pmap_dfr(
    list(
      longitude = points$longitude,
      latitude = points$latitude
    ),
    get_emep_indexes,
    emep_pth = emep_file
  )
  
  points = points %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      i = point_indexes$row,
      j = point_indexes$col
    )
  
  # Read grid-cell areas from the fullrun file. These are required when
  # converting area-normalised emissions/deposition to total grid-cell mass.
  
  area_data = read_emep(
    emep_fname = area_file,
    emep_var = 'Area_Grid_km2',
    emep_crs = emep_crs,
    proxy = FALSE,
    driver = 'gdal'
  )
  
  area_values = area_data[['Area_Grid_km2']]
  
  nc = ncdf4::nc_open(
    emep_file
  )
  
  on.exit(
    ncdf4::nc_close(nc),
    add = TRUE
  )
  
  nc_date = get_emep_floordate(
    emep_file
  )
  
  var_units = get_nc_var_units(
    nc_file = emep_file,
    vars = temporal_vars
  )
  
  purrr::pmap_dfr(
    points,
    function(location_id, longitude, latitude, i, j, ...) {
      
      if (
        is.na(i) ||
        is.na(j)
      ) {
        return(tibble::tibble())
      }
      
      cell_area = area_values[
        i,
        j
      ]
      
      purrr::map_dfr(
        temporal_vars,
        function(var_name) {
          
          quantity_type = dplyr::case_when(
            stringr::str_starts(var_name, 'SURF_') ~ 'Concentration',
            stringr::str_starts(var_name, 'D3_') ~ 'Concentration',
            stringr::str_starts(var_name, 'Emis_') ~ 'Emissions',
            stringr::str_starts(var_name, 'DDEP_') ~ 'Dry deposition',
            stringr::str_starts(var_name, 'WDEP_') ~ 'Wet deposition',
            TRUE ~ 'Value'
          )
          
          is_mass_var = stringr::str_detect(
            var_name,
            '^(Emis|DDEP|WDEP)'
          )
          
          var_z_index = get_var_param(
            var = var_name,
            key = 'z_index',
            var_params_list = var_params_list,
            default = NULL
          )
          
          var_dims = nc$var[[var_name]]$dim
          
          dim_names = purrr::map_chr(
            var_dims,
            'name'
          )
          
          vertical_dim = intersect(
            dim_names,
            c('z', 'lev')
          )
          
          if (
            length(vertical_dim) > 0 &&
            is.null(var_z_index)
          ) {
            stop(
              "No z_index is defined for temporal variable '",
              var_name,
              "' in the variable parameters file.",
              call. = FALSE
            )
          }
          
          if (
            length(vertical_dim) > 0 &&
            (
              var_z_index < 1 ||
              var_z_index >
              var_dims[[match(vertical_dim[[1]], dim_names)]]$len
            )
          ) {
            stop(
              "z_index for temporal variable '",
              var_name,
              "' is outside the available vertical dimension.",
              call. = FALSE
            )
          }
          
          spatial_index = 0L
          
          start = purrr::map_int(
            dim_names,
            function(dim_name) {
              
              if (dim_name == 'time') {
                
                1L
                
              } else if (dim_name %in% c('z', 'lev')) {
                
                as.integer(
                  var_z_index
                )
                
              } else {
                
                spatial_index <<- spatial_index + 1L
                
                if (spatial_index == 1L) {
                  as.integer(i)
                } else if (spatial_index == 2L) {
                  as.integer(j)
                } else {
                  1L
                }
              }
            }
          )
          
          count = purrr::map_int(
            dim_names,
            function(dim_name) {
              
              if (dim_name == 'time') {
                -1L
              } else {
                1L
              }
            }
          )
          
          values = ncdf4::ncvar_get(
            nc,
            varid = var_name,
            start = start,
            count = count
          )
          
          var_unit = unname(
            var_units[var_name]
          )
          
          # For mass variables, optionally convert the area-normalised
          # value to total mass within the selected model grid cell.
          
          if (
            is_mass_var &&
            mass_quantity == 'total'
          ) {
            
            calc_unit = normalise_emep_dep_unit(
              var_unit
            )
            
            values = units::set_units(
              values,
              calc_unit,
              mode = 'standard'
            )
            
            cell_mass = values * cell_area
            
            cell_mass_mg = cell_mass %>%
              units::set_units('mg') %>%
              units::drop_units()
            
            values = convert_mass_from_mg(
              cell_mass_mg,
              out_unit = total_out_unit
            )
            
            dep_element = get_emep_dep_element(
              var_unit
            )
            
            var_unit = if (is.null(dep_element)) {
              total_out_unit
            } else {
              paste(
                total_out_unit,
                dep_element
              )
            }
          }
          
          tibble::tibble(
            location_id = location_id,
            location_type = 'point',
            summary_type = 'point',
            quantity_type = quantity_type,
            time = nc_date$date,
            variable = var_name,
            value = as.numeric(values),
            unit = var_unit
          )
        }
      )
    }
  )
}

extract_wrf_vars_from_file = function(
    wrf_file_pth,
    var_names,          # character vector, e.g. c("slp","T2","td2","ws","wd","RAINNC","RAINC")
    code,                # site codes (length N)
    i_idx,               # x (west_east), integer vector length N
    j_idx,               # y (south_north), integer vector length N
    index_level = NULL,  # NULL | integer-like | "pblh" (applies to vars that are 3-D)
    py_bin = Sys.getenv("RETICULATE_PYTHON")
) {
  # Bind Python (virtualenv/conda) and import inside the worker
  reticulate::use_python(py_bin, required = TRUE)
  ncpy = reticulate::import("netCDF4", convert = FALSE)
  wrf  = reticulate::import("wrf",      convert = FALSE)
  np   = reticulate::import("numpy",    convert = FALSE)
  
  # Open file once
  ds = ncpy$Dataset(wrf_file_pth)
  on.exit(try(ds$close(), silent = TRUE), add = TRUE)
  
  # Times -> POSIXct (robust)
  wrf_times = wrf$extract_times(ds, timeidx = wrf$ALL_TIMES)
  if (!inherits(wrf_times, "POSIXct")) {
    secs = wrf_times$astype("datetime64[s]")$astype("int64")$tolist() %>% 
      reticulate::py_to_r() %>% unlist(use.names = FALSE)
    wrf_times = as.POSIXct(secs, origin = "1970-01-01", tz = "UTC")
  }
  Tlen = length(wrf_times)
  N    = length(i_idx)
  stopifnot(length(j_idx) == N, length(code) == N)
  
  # Normalise vertical level input
  if (!is.null(index_level) && !identical(index_level, "pblh")) {
    checkmate::assert_number(as.numeric(index_level), lower = 0, finite = TRUE, na.ok = FALSE)
    index_level = as.integer(index_level)
  }
  vertical_lev_val = if (is.null(index_level)) "surface" else as.character(index_level)
  
  # Lazily cached helpers shared across variables
  pblh_cached = NULL
  z_cached    = NULL
  uvmet_cached = NULL  # for ws/wd so we don’t call getvar twice
  
  get_base_da = function(vname) {
    if (vname %in% c("ws", "wd")) {
      if (is.null(uvmet_cached)) {
        uvmet_cached <<- wrf$getvar(ds, "uvmet10_wspd_wdir", timeidx = wrf$ALL_TIMES)
      }
      sel_name = if (vname == "ws") "wspd" else "wdir"
      da = uvmet_cached$sel(wspd_wdir = sel_name)   # dims: time × south_north × west_east
      return(da)
    }
    
    da = wrf$getvar(ds, vname, timeidx = wrf$ALL_TIMES)
    dims = reticulate::py_to_r(da$dims)
    
    if (!is.null(index_level)) {
      if (identical(index_level, "pblh")) {
        if (is.null(pblh_cached)) pblh_cached <<- wrf$getvar(ds, "PBLH", timeidx = wrf$ALL_TIMES)
        if (is.null(z_cached))    z_cached    <<- wrf$getvar(ds, "z",    timeidx = wrf$ALL_TIMES)
        da = wrf$interplevel(da, z_cached, pblh_cached)  # => time × south_north × west_east
      } else {
        if ("bottom_top" %in% dims) {
          da = da$isel(bottom_top = index_level)
        } else if ("bottom_top_stag" %in% dims) {
          da = da$isel(bottom_top_stag = index_level)
        } else if ("low_mid_high" %in% dims) {
          da = da$isel(low_mid_high = index_level)
        } else if (any(c("bottom_top","bottom_top_stag","low_mid_high") %in% dims)) {
          stop("Variable has a vertical coordinate but no recognised dim name.")
        }
      }
    } else if (any(c("bottom_top","bottom_top_stag","low_mid_high") %in% dims)) {
      stop("Variable has a vertical coordinate. Provide numeric level or 'pblh'.")
    }
    da
  }
  
  # Slice a DataArray at all sites -> numeric matrix T×N
  da_to_matrix_TxN = function(da) {
    v_np = wrf$to_np(da)
    if (reticulate::py_has_attr(v_np, "mask")) v_np = np$ma$filled(v_np, np$nan)
    v_np = np$ascontiguousarray(v_np)$astype("float64")
    nd   = as.integer(reticulate::py_to_r(v_np$ndim))
    
    get_column = function(ix, jx) {
      if (nd == 3L) {
        v_np[, jx, ix]$tolist()
      } else if (nd == 2L) {
        sh = reticulate::py_to_r(v_np$shape)
        if (length(sh) == 2L && sh[[1]] == Tlen) {
          v_np[, jx]$tolist()       # time × site
        } else {
          list(v_np[jx, ix])        # single time
        }
      } else if (nd == 1L) {
        v_np$tolist()               # already time vector
      } else {
        stop("Unhandled array rank: ", nd)
      }
    }
    
    cols = lapply(seq_len(N), function(k) get_column(i_idx[k], j_idx[k]))
    cols_r = lapply(cols, function(x) unlist(reticulate::py_to_r(x), use.names = FALSE))
    cols_r = lapply(cols_r, function(x) if (length(x) == 1L && Tlen > 1L) rep(x, Tlen) else x)
    
    M = matrix(NA_real_, nrow = Tlen, ncol = N)
    for (k in seq_len(N)) {
      if (length(cols_r[[k]]) != Tlen) stop("Column length mismatch for site ", k)
      M[, k] = cols_r[[k]]
    }
    M
  }
  
  # Process all variables with a single open file
  out_list = vector("list", length(var_names))
  for (vv in seq_along(var_names)) {
    vname = var_names[vv]
    da = get_base_da(vname)
    M = da_to_matrix_TxN(da)
    
    # unit conversions
    if (vname == "T2") M = M - 273.15
    if (vname == 'PSFC') M = M / 100
    
    wide = tibble::as_tibble(M, .name_repair = "minimal")
    names(wide) = code
    
    df = tibble::tibble(date = wrf_times,
                        var   = rep(vname, Tlen),
                        file_id = basename(wrf_file_pth)) |>
      dplyr::bind_cols(wide) |>
      tidyr::pivot_longer(cols = -c(date, var, file_id),
                          names_to = "code",
                          values_to = "value") |>
      dplyr::mutate(vertical_level = vertical_lev_val) %>% 
      dplyr::relocate(file_id, .after = dplyr::last_col())
    
    out_list[[vv]] = df
  }
  
  dplyr::bind_rows(out_list)
}

file_has_site_code = function(file,
                              code) {
  
  stringr::str_detect(
    fs::path_file(file),
    paste0(
      '(^|_)',
      code,
      '(_|\\.)'
    )
  )
}

filter_mobs_tseries_period = function(df,
                                      period = mobs_report_tseries_period) {
  
  if (!'date' %in% names(df)) {
    return(df)
  }
  
  if (!is.na(period$start)) {
    df = df %>%
      dplyr::filter(date >= period$start)
  }
  
  if (!is.na(period$end)) {
    df = df %>%
      dplyr::filter(date <= period$end)
  }
  
  df
}

filter_region_geofiles_to_domain = function(region_geofiles, emep_file, emep_crs,
                                            min_coverage = 1,
                                            include_regions = NULL) {
  # filters region geofiles to those that are sufficiently covered by the
  # model domain of a representative EMEP file
  #
  # coverage is calculated by get_region_domain_coverage()
  #
  # regions are kept if either:
  # - their coverage proportion is greater than or equal to min_coverage, or
  # - their region_id is listed in include_regions
  #
  # this is useful when inventory-total comparisons should only be made for
  # regions that are mostly or fully contained within the model domain, while
  # still allowing specific regions to be force-included
  #
  # region_geofiles:
  #   tibble of region geofiles, typically with columns:
  #   - region_id
  #   - fpath
  #
  # emep_file:
  #   representative EMEP file for the domain, used to derive the model domain
  #   extent
  #
  # emep_crs:
  #   CRS string to assign to the EMEP data when reading emep_file
  #
  # min_coverage:
  #   minimum proportion of the region that must lie within the model domain
  #   for the region to be retained
  #   default is 1, i.e. only fully covered regions are kept
  #
  # include_regions:
  #   optional character vector of region_id values to always retain, regardless
  #   of coverage
  #
  # output:
  #   tibble returned by get_region_domain_coverage(), filtered to the selected
  #   regions
  
  coverage_tbl = get_region_domain_coverage(
    region_geofiles = region_geofiles,
    emep_file = emep_file,
    emep_crs = emep_crs
  )
  
  dplyr::filter(
    coverage_tbl,
    (
      coverage_prop > 0 &
        coverage_prop >= min_coverage
    ) |
      region_id %in% include_regions
  )
}

filter_map_values = function(
    x,
    plot_value_range = c(-Inf, Inf)
) {
  
  if (
    length(plot_value_range) != 2 ||
    plot_value_range[1] > plot_value_range[2]
  ) {
    stop(
      "plot_value_range must contain two ordered values."
    )
  }
  
  x[
    x < plot_value_range[1] |
      x > plot_value_range[2]
  ] = NA
  
  x
}

filter_report_site_vars = function(
    df,
    cfg,
    period,
    var_params_list,
    var_param_names = NULL,
    var_order = NULL
) {
  
  resolution = cfg$resolution %||% 'summary'
  
  if (!resolution %in% c('summary', 'raw')) {
    
    logger::log_warn(
      glue::glue(
        "Unknown time-series resolution '{resolution}'. Falling back to 'summary'."
      )
    )
    
    resolution = 'summary'
  }
  
  if (resolution == 'raw') {
    
    site_info = df %>%
      dplyr::ungroup() %>%
      dplyr::select(
        -dplyr::any_of(
          c(
            'data',
            'month',
            'month_start'
          )
        )
      ) %>%
      dplyr::distinct() %>%
      dplyr::slice(1)
    
    raw_data = df %>%
      dplyr::filter(
        month > 0
      ) %>%
      dplyr::pull(
        data
      ) %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(
        date
      ) %>%
      filter_mobs_tseries_period(
        period = period
      )
    
    if (nrow(raw_data) == 0) {
      
      logger::log_warn(
        glue::glue(
          "No raw time-series data found for site '{df$code[[1]]}' after period filtering."
        )
      )
      
      return(
        tibble::tibble()
      )
    }
    
    df = site_info %>%
      dplyr::mutate(
        month = -2L,
        month_start = as.POSIXct(NA),
        data = list(raw_data)
      )
    
  } else {
    
    df = df %>%
      dplyr::filter(
        month == -1
      ) %>%
      dplyr::mutate(
        data = purrr::map(
          data,
          filter_mobs_tseries_period,
          period = period
        )
      )
  }
  
  df %>%
    dplyr::mutate(
      resolution = resolution,
      vars = purrr::map(
        data,
        function(x) {
          
          vars_available = unique(x$var) %>%
            setdiff(
              'subprecip'
            )
          
          vars_available = vars_available[
            purrr::map_lgl(
              vars_available,
              function(var_nm) {
                
                var_param_nm = if (
                  !is.null(var_param_names) &&
                  var_nm %in% names(var_param_names)
                ) {
                  var_param_names[[var_nm]]
                } else {
                  var_nm
                }
                
                resolve_var_id(
                  var = var_param_nm,
                  var_params_list = var_params_list
                ) %in% names(var_params_list)
              }
            )
          ]
          
          vars_selected = if (is.null(cfg$vars)) {
            vars_available
          } else {
            intersect(
              vars_available,
              cfg$vars
            )
          }
          
          if (!is.null(var_order)) {
            
            vars_selected = c(
              var_order[
                var_order %in% vars_selected
              ],
              vars_selected[
                !vars_selected %in% var_order
              ]
            )
          }
          
          vars_selected
        }
      )
    ) %>%
    tidyr::unnest(
      cols = vars
    ) %>%
    dplyr::rename(
      var = vars
    ) %>%
    dplyr::mutate(
      var_order = if (!is.null(var_order)) {
        match(
          var,
          var_order
        )
      } else {
        get_var_order_index(
          var,
          var_params_list
        )
      }
    ) %>%
    dplyr::arrange(
      var_order,
      var
    ) %>%
    dplyr::select(
      -var_order
    )
}

filter_temporal_plot_period = function(
    temporal_data,
    plot_start = NULL,
    plot_end = NULL
) {
  
  if (
    is.null(temporal_data) ||
    nrow(temporal_data) == 0 ||
    (
      is.null(plot_start) &&
      is.null(plot_end)
    )
  ) {
    return(temporal_data)
  }
  
  test_years = temporal_data %>%
    dplyr::filter(
      run == 'test'
    ) %>%
    dplyr::pull(
      time
    ) %>%
    lubridate::year() %>%
    unique()
  
  test_years = test_years[
    !is.na(test_years)
  ]
  
  if (length(test_years) != 1) {
    stop(
      'Temporal plot period constraints require a single test-run year.',
      call. = FALSE
    )
  }
  
  test_year = test_years[[1]]
  
  parse_plot_datetime = function(x) {
    
    if (is.null(x)) {
      return(NULL)
    }
    
    parsed = lubridate::parse_date_time(
      x,
      orders = c(
        'Y-m-d H:M:S',
        'Y-m-d H:M',
        'Y-m-d'
      ),
      tz = 'UTC'
    )
    
    if (is.na(parsed)) {
      stop(
        "Could not parse temporal plot date/time: '",
        x,
        "'.",
        call. = FALSE
      )
    }
    
    parsed
  }
  
  plot_start = parse_plot_datetime(
    plot_start
  )
  
  plot_end = parse_plot_datetime(
    plot_end
  )
  
  if (
    !is.null(plot_start) &&
    lubridate::year(plot_start) != test_year
  ) {
    stop(
      'TEMPORAL_SUMMARY_PLOT_START must use the test-run year.',
      call. = FALSE
    )
  }
  
  if (
    !is.null(plot_end) &&
    lubridate::year(plot_end) != test_year
  ) {
    stop(
      'TEMPORAL_SUMMARY_PLOT_END must use the test-run year.',
      call. = FALSE
    )
  }
  
  if (
    !is.null(plot_start) &&
    !is.null(plot_end) &&
    plot_start > plot_end
  ) {
    stop(
      'TEMPORAL_SUMMARY_PLOT_START must not be after TEMPORAL_SUMMARY_PLOT_END.',
      call. = FALSE
    )
  }
  
  temporal_data %>%
    dplyr::group_by(
      run
    ) %>%
    dplyr::group_modify(
      function(.x, .y) {
        
        run_years = lubridate::year(
          .x$time
        ) %>%
          unique()
        
        run_years = run_years[
          !is.na(run_years)
        ]
        
        if (length(run_years) != 1) {
          stop(
            "Temporal plot period constraints require a single year for run '",
            .y$run,
            "'.",
            call. = FALSE
          )
        }
        
        run_year = run_years[[1]]
        
        run_start = if (!is.null(plot_start)) {
          update(
            plot_start,
            year = run_year
          )
        } else {
          NULL
        }
        
        run_end = if (!is.null(plot_end)) {
          update(
            plot_end,
            year = run_year
          )
        } else {
          NULL
        }
        
        if (!is.null(run_start)) {
          .x = .x %>%
            dplyr::filter(
              time >= run_start
            )
        }
        
        if (!is.null(run_end)) {
          .x = .x %>%
            dplyr::filter(
              time <= run_end
            )
        }
        
        .x
      }
    ) %>%
    dplyr::ungroup()
}

find_matching_emep_file = function(dir, file_tags, dir_name) {
  if (is.null(dir)) {
    return(NULL)
  }
  
  matches = purrr::map(
    file_tags,
    ~select_file_from_dir(dir, fname = .x, fail_out = 'na')
  ) %>%
    purrr::discard(is.na)
  
  if (length(matches) == 0) {
    msg = glue::glue(
      "No file matching any of {paste(file_tags, collapse = ', ')} was found in '{dir_name}' ('{dir}')."
    )
    logger::log_error(msg)
    stop(msg)
  }
  
  matches[[1]]
}

find_sites_used_file = function(data_dir) {
  
  candidates = fs::path(
    data_dir,
    c(
      'Sites_used.rds',
      '00Sites_used.rds'
    )
  )
  
  sites_used_pth = candidates[
    fs::file_exists(candidates)
  ]
  
  if (length(sites_used_pth) == 0) {
    return(NULL)
  }
  
  sites_used_pth[1]
}

floor_dec = function(x, level=1) round(x - 5*10^(-level-1), level)

format_elements_to_print = function(x,
                                    conjunction = 'and') {
  
  x = unique(x)
  x = x[!is.na(x) & x != '']
  
  if (length(x) == 0) {
    return('')
  }
  
  if (length(x) == 1) {
    return(x)
  }
  
  stringr::str_c(
    stringr::str_c(x[-length(x)], collapse = ', '),
    ' ',
    conjunction,
    ' ',
    x[length(x)]
  )
}

format_emep_maps_page_title = function(
    test_dir,
    ref_dir = NULL,
    stat,
    wrap_width = 50
) {
  
  stat_lab = stringr::str_to_upper(
    stat
  )
  
  format_run_line = function(
    label,
    run_dir
  ) {
    
    run_desc = stringr::str_glue(
      '{run_dir} - {stat_lab}'
    )
    
    stringr::str_c(
      label,
      ' ',
      stringr::str_wrap(
        run_desc,
        width = wrap_width,
        exdent = nchar(label) + 1
      )
    )
  }
  
  if (is.null(ref_dir)) {
    
    pg_title = format_run_line(
      label = 'Model run:',
      run_dir = test_dir
    )
    
  } else {
    
    pg_title = stringr::str_c(
      format_run_line(
        label = 'Test run:',
        run_dir = test_dir
      ),
      '\n',
      format_run_line(
        label = 'Reference run:',
        run_dir = ref_dir
      )
    )
  }
  
  stringr::str_c(
    pg_title,
    '\n'
  )
}

format_file_path = function(path) {
  stringr::str_replace_all(
    path,
    '/',
    '/<wbr>'
  )
}

format_file_size_table = function(file_size_df, run_labels = c('Test', 'Reference')) {
  
  if (is.null(file_size_df) || !is.data.frame(file_size_df) || nrow(file_size_df) == 0) {
    return(
      gt::gt(data.frame(note = "No test run data available")) %>%
        gt::cols_label(note = '') %>%
        gt::tab_options(
          data_row.padding = gt::px(3),
          table.align = 'left'
        )
    )
  }
  
  has_ref = 'ref' %in% names(file_size_df)
  
  gt_tbl = gt::gt(file_size_df) %>%
    gt::sub_missing(columns = tidyselect::everything()) %>%
    gt::cols_align(
      align = 'right',
      columns = -fname
    ) %>%
    gt::tab_options(
      data_row.padding = gt::px(3),
      table.align = 'left'
    )
  
  if (has_ref) {
    gt_tbl = gt_tbl %>%
      gt::tab_style(
        style = list(
          gt::cell_text(color = 'red')
        ),
        locations = gt::cells_body(
          columns = rel_diff,
          rows = abs(rel_diff) > 5
        )
      ) %>%
      gt::cols_label(
        fname = 'File',
        test = run_labels[1],
        ref = run_labels[2],
        abs_diff = 'Size difference (B)',
        rel_diff = 'Size difference (%)'
      )
  } else {
    gt_tbl = gt_tbl %>%
      gt::cols_label(
        fname = 'File',
        test = run_labels[1]
      ) %>%
      gt::tab_source_note(
        paste0(run_labels[2], " run not submitted: only ", run_labels[1], " run sizes shown")
      )
  }
  
  gt_tbl
}

format_inventory_pollutant = function(
    pollutant,
    output = c('plain', 'html', 'plotmath')
) {
  
  output = match.arg(output)
  
  pollutant_lab = dplyr::recode(
    pollutant,
    SOx = 'SO[x]',
    NOx = 'NO[x]',
    PMcoarse = 'PM[coarse]',
    .default = pollutant
  )
  
  switch(
    output,
    plain = format_lab_for_plain(pollutant_lab),
    html = format_lab_for_html(pollutant_lab),
    plotmath = pollutant_lab
  )
}

format_lab_for_html = function(x) {
  
  if (
    is.null(x) ||
    length(x) == 0 ||
    is.na(x) ||
    x == ''
  ) {
    return('')
  }
  
  x %>%
    format_lab_for_plotmath() %>%
    format_plotmath_for_html()
}

format_lab_for_plain = function(x) {
  
  if (
    is.null(x) ||
    length(x) == 0 ||
    is.na(x) ||
    x == ''
  ) {
    return('')
  }
  
  subscript_chars = c(
    '0' = '₀',
    '1' = '₁',
    '2' = '₂',
    '3' = '₃',
    '4' = '₄',
    '5' = '₅',
    '6' = '₆',
    '7' = '₇',
    '8' = '₈',
    '9' = '₉',
    '+' = '₊',
    '-' = '₋'
  )
  
  superscript_chars = c(
    '0' = '⁰',
    '1' = '¹',
    '2' = '²',
    '3' = '³',
    '4' = '⁴',
    '5' = '⁵',
    '6' = '⁶',
    '7' = '⁷',
    '8' = '⁸',
    '9' = '⁹',
    '+' = '⁺',
    '-' = '⁻'
  )
  
  convert_chars = function(x, lookup) {
    
    stringr::str_split(
      x,
      '',
      simplify = TRUE
    ) %>%
      as.character() %>%
      purrr::map_chr(
        function(char) {
          if (char %in% names(lookup)) {
            lookup[[char]]
          } else {
            char
          }
        }
      ) %>%
      paste0(
        collapse = ''
      )
  }
  
  x %>%
    format_lab_for_plotmath() %>%
    stringr::str_replace_all(
      '\\[([^]]+)\\]',
      function(z) {
        
        purrr::map_chr(
          z,
          function(this_z) {
            
            txt = stringr::str_remove_all(
              this_z,
              '^\\[|\\]$'
            )
            
            convert_chars(
              txt,
              subscript_chars
            )
          }
        )
      }
    ) %>%
    stringr::str_replace_all(
      '\\^\\{([^}]+)\\}',
      function(z) {
        
        purrr::map_chr(
          z,
          function(this_z) {
            
            txt = stringr::str_remove_all(
              this_z,
              '^\\^\\{|\\}$'
            )
            
            convert_chars(
              txt,
              superscript_chars
            )
          }
        )
      }
    ) %>%
    stringr::str_replace_all(
      '\\^([+-])',
      function(z) {
        
        purrr::map_chr(
          z,
          function(this_z) {
            
            txt = stringr::str_remove(
              this_z,
              '^\\^'
            )
            
            convert_chars(
              txt,
              superscript_chars
            )
          }
        )
      }
    ) %>%
    stringr::str_replace_all(
      '\\bmu\\b',
      'µ'
    ) %>%
    stringr::str_replace_all(
      '\\*',
      ''
    ) %>%
    stringr::str_replace_all(
      '~',
      ' '
    ) %>%
    stringr::str_replace_all(
      '"',
      ''
    )
}

format_lab_for_plotmath = function(x) {
  
  if (
    is.null(x) ||
    length(x) == 0 ||
    is.na(x) ||
    x == ''
  ) {
    return('')
  }
  
  dplyr::case_when(
    x == 'T2' ~ 'T[2]',
    x %in% c('Td2', 'td2') ~ 'T[d2]',
    x %in% c('RH2', 'rh2') ~ 'RH[2]',
    x == 'PM2.5' ~ 'PM[2.5]',
    x == 'PM10' ~ 'PM[10]',
    x == 'NO2' ~ 'NO[2]',
    x == 'NO3' ~ 'NO[3]',
    x == 'NH3' ~ 'NH[3]',
    x == 'NH4' ~ 'NH[4]',
    x == 'SO2' ~ 'SO[2]',
    x == 'SO4' ~ 'SO[4]',
    x == 'O3' ~ 'O[3]',
    x == 'N2O5' ~ 'N[2]O[5]',
    x == 'HNO3' ~ 'HNO[3]',
    x == 'HONO' ~ 'HONO',
    x == 'HO2NO2' ~ 'HO[2]NO[2]',
    x == 'CO2' ~ 'CO[2]',
    x == 'CH4' ~ 'CH[4]',
    x == 'C5H8' ~ 'C[5]H[8]',
    TRUE ~ x %>%
      stringr::str_replace_all(
        '\\]([A-Za-z])\\[',
        ']*\\1['
      ) %>%
      stringr::str_replace_all(
        '\\s+',
        '~'
      )
  )
}

format_mobs_group_colours = function(colours) {
  
  if (is.null(colours) || is.null(names(colours))) {
    return(colours)
  }
  
  names(colours) = names(colours) %>%
    stringr::str_replace_all('_', ' ') %>%
    stringr::str_to_sentence()
  
  colours
}

format_mobs_group_labels = function(df,
                                    grouping_var = MOBS_GROUPING_VAR,
                                    missing_label = 'All sites') {
  
  if (is.null(grouping_var) || !grouping_var %in% names(df)) {
    return(df)
  }
  
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(grouping_var),
        ~ format_mobs_group_values(
          .x,
          missing_label = missing_label
        )
      )
    )
}

format_mobs_group_values = function(x,
                                    missing_label = 'Unclassified') {
  
  x = as.character(x)
  
  dplyr::if_else(
    is.na(x) | x == '',
    missing_label,
    stringr::str_to_sentence(
      stringr::str_replace_all(x, '_', ' ')
    )
  )
}

format_mobs_period_label = function(cfg) {
  
  if (is.na(cfg$period_start) && is.na(cfg$period_end)) {
    return(NULL)
  }
  
  start_lab = if (is.na(cfg$period_start)) {
    'start'
  } else {
    format(cfg$period_start, '%Y-%m-%d %H:%M')
  }
  
  end_lab = if (is.na(cfg$period_end)) {
    'end'
  } else {
    format(cfg$period_end, '%Y-%m-%d %H:%M')
  }
  
  stringr::str_glue('{start_lab} to {end_lab}')
}

format_mobs_summary_label = function(summary_time = 'day',
                                     summary_stat = 'mean') {
  
  time_lab = dplyr::case_when(
    summary_time == 'hour' ~ 'hourly',
    summary_time == 'day' ~ 'daily',
    summary_time == 'month' ~ 'monthly',
    TRUE ~ summary_time
  )
  
  stat_lab = dplyr::case_when(
    summary_stat == 'mean' ~ 'mean',
    summary_stat == 'sum' ~ 'total',
    summary_stat == 'median' ~ 'median',
    summary_stat == 'max' ~ 'maximum',
    summary_stat == 'min' ~ 'minimum',
    TRUE ~ summary_stat
  )
  
  stringr::str_squish(stringr::str_c(time_lab, stat_lab, sep = ' '))
}

#' Format model-observation data for time-series plotting
#'
#' Prepare paired model-observation meteorological data for site-level
#' time-series plotting. The function creates a nested tibble containing summary
#' data for the full input period, stored with `month == -1`, and
#' native-resolution data split by consecutive calendar months, stored with
#' `month == 1, 2, 3, ...`.
#'
#' If a `ref_mod` column is present, it is retained and summarised alongside
#' `mod` so that reference-run values can be shown in static and interactive
#' time-series plots.
#'
#' @export
format_mobs_to_plot = function(mobs_lframe, ...) {
  
  stopifnot(is.data.frame(mobs_lframe))
  stopifnot(all(c('date', 'code', 'var', 'mod', 'obs') %in% names(mobs_lframe)))
  
  dots = list(...)
  avg_time = dots$avg_time %||% 'day'
  
  has_ref_mod = 'ref_mod' %in% names(mobs_lframe) &&
    any(is.finite(mobs_lframe$ref_mod))
  
  # Define consecutive month indices from the actual input period.
  month_lookup = mobs_lframe %>%
    dplyr::distinct(
      month_start = lubridate::floor_date(date, unit = 'month')
    ) %>%
    dplyr::arrange(.data$month_start) %>%
    dplyr::mutate(month = dplyr::row_number())
  
  summary_month_start = as.POSIXct(NA, tz = lubridate::tz(mobs_lframe$date))
  
  precip_vars = c('precip', 'subprecip')
  
  precip_data = mobs_lframe %>% 
    dplyr::filter(var %in% precip_vars)
  
  if (nrow(precip_data) > 0) {
    
    precip_value_cols = c('mod', 'obs')
    
    if (has_ref_mod) {
      precip_value_cols = c(precip_value_cols, 'ref_mod')
    }
    
    precip_data_wide = precip_data %>% 
      tidyr::pivot_wider(
        id_cols = c('date', 'code'),
        names_from = 'var',
        values_from = dplyr::all_of(precip_value_cols)
      )
    
    # Aggregated precipitation totals over the full input period, then
    # cumulative sum. The aggregation period follows avg_time.
    if (identical(avg_time, 'period')) {
      pdw_summary = precip_data_wide %>%
        dplyr::group_by(.data$code) %>%
        dplyr::summarise(
          date = min(.data$date, na.rm = TRUE),
          mod_precip = sum(.data$mod_precip, na.rm = TRUE),
          mod_subprecip = ifelse(
            all(is.na(.data$obs_precip)),
            NA_real_,
            sum(.data$mod_subprecip, na.rm = TRUE)
          ),
          obs_subprecip = ifelse(
            all(is.na(.data$obs_precip)),
            NA_real_,
            sum(.data$obs_subprecip, na.rm = TRUE)
          ),
          obs_precip = ifelse(
            all(is.na(obs_precip)),
            NA_real_,
            sum(obs_precip, na.rm = TRUE)
          ),
          .groups = 'drop'
        )
    } else {
      pdw_summary = precip_data_wide %>%
        dplyr::mutate(date = lubridate::floor_date(.data$date, unit = avg_time)) %>%
        dplyr::group_by(code, date) %>%
        dplyr::summarise(
          mod_precip = sum(.data$mod_precip, na.rm = TRUE),
          mod_subprecip = ifelse(
            all(is.na(.data$obs_precip)),
            NA_real_,
            sum(.data$mod_subprecip, na.rm = TRUE)
          ),
          obs_subprecip = ifelse(
            all(is.na(.data$obs_precip)),
            NA_real_,
            sum(.data$obs_subprecip, na.rm = TRUE)
          ),
          obs_precip = ifelse(
            all(is.na(.data$obs_precip)),
            NA_real_,
            sum(.data$obs_precip, na.rm = TRUE)
          ),
          .groups = 'drop'
        )
    }
    
    if (has_ref_mod) {
      ref_precip_summary = precip_data_wide
      
      if (identical(avg_time, 'period')) {
        ref_precip_summary = ref_precip_summary %>%
          dplyr::group_by(.data$code) %>%
          dplyr::summarise(
            date = min(.data$date, na.rm = TRUE),
            ref_mod_precip = sum(.data$ref_mod_precip, na.rm = TRUE),
            ref_mod_subprecip = ifelse(
              all(is.na(.data$obs_precip)),
              NA_real_,
              sum(.data$ref_mod_subprecip, na.rm = TRUE)
            ),
            .groups = 'drop'
          )
      } else {
        ref_precip_summary = ref_precip_summary %>%
          dplyr::mutate(date = lubridate::floor_date(date, unit = avg_time)) %>%
          dplyr::group_by(code, date) %>%
          dplyr::summarise(
            ref_mod_precip = sum(ref_mod_precip, na.rm = TRUE),
            ref_mod_subprecip = ifelse(
              all(is.na(obs_precip)),
              NA_real_,
              sum(ref_mod_subprecip, na.rm = TRUE)
            ),
            .groups = 'drop'
          )
      }
      
      pdw_summary = pdw_summary %>%
        dplyr::left_join(
          ref_precip_summary,
          by = c('code', 'date')
        )
    }
    
    precip_cols = intersect(
      c(
        'mod_precip', 'mod_subprecip',
        'obs_precip', 'obs_subprecip',
        'ref_mod_precip', 'ref_mod_subprecip'
      ),
      names(pdw_summary)
    )
    
    pdw_summary = pdw_summary %>%
      dplyr::arrange(.data$code, .data$date) %>%
      dplyr::group_by(.data$code) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(precip_cols),
          ~ calculate_cumsum(.x)
        ),
        month = -1L,
        month_start = summary_month_start
      ) %>%
      dplyr::ungroup()
    
    # Native-resolution precipitation data split by consecutive month, with
    # cumulative sums reset within each month.
    pdw_m = precip_data_wide %>%
      dplyr::mutate(
        month_start = lubridate::floor_date(.data$date, unit = 'month')
      ) %>%
      dplyr::left_join(month_lookup, by = 'month_start')
    
    precip_cols_m = intersect(
      c(
        'mod_precip', 'mod_subprecip',
        'obs_precip', 'obs_subprecip',
        'ref_mod_precip', 'ref_mod_subprecip'
      ),
      names(pdw_m)
    )
    
    pdw_m = pdw_m %>%
      dplyr::group_by(.data$code, .data$month, .data$month_start) %>% 
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(precip_cols_m),
          ~ calculate_cumsum(.x)
        )
      ) %>% 
      dplyr::ungroup()
    
    precip_data = dplyr::bind_rows(pdw_summary, pdw_m) %>% 
      tidyr::pivot_longer(
        cols = -c('code', 'date', 'month', 'month_start'),
        names_to = c('scenario', 'var'),
        names_pattern = '^(mod|obs|ref_mod)_(.*)$',
        values_to = 'value'
      ) %>% 
      tidyr::pivot_wider(
        names_from = 'scenario',
        values_from = 'value'
      )
    
    mobs_lframe = mobs_lframe %>% 
      dplyr::filter(!var %in% precip_vars)
    
  } else {
    precip_data = tibble::tibble()
  }
  
  data_summary = mobs_lframe %>% 
    summarise_mobs(...) %>% 
    dplyr::mutate(
      month = -1L,
      month_start = summary_month_start
    )
  
  mobs_out = mobs_lframe %>% 
    dplyr::mutate(
      month_start = lubridate::floor_date(.data$date, unit = 'month')
    ) %>%
    dplyr::left_join(month_lookup, by = 'month_start') %>%
    dplyr::bind_rows(data_summary, precip_data) %>%
    dplyr::group_by(.data$code, .data$month, .data$month_start) %>% 
    tidyr::nest() %>% 
    dplyr::arrange(.data$month) %>%
    dplyr::ungroup()
  
  mobs_out
}

format_mobs_map_popup_value = function(x,
                              digits = 4) {
  
  if (is.numeric(x)) {
    return(
      dplyr::if_else(
        is.na(x),
        '',
        format(
          round(x, digits),
          trim = TRUE,
          scientific = FALSE
        )
      )
    )
  }
  
  x = as.character(x)
  x[is.na(x)] = ''
  
  x
}

format_plotmath_for_html = function(x) {
  
  if (
    is.null(x) ||
    length(x) == 0 ||
    is.na(x) ||
    x == ''
  ) {
    return('')
  }
  
  x %>%
    stringr::str_replace_all(
      '\\[([^]]+)\\]',
      '<sub>\\1</sub>'
    ) %>%
    stringr::str_replace_all(
      '\\^\\{([^}]+)\\}',
      '<sup>\\1</sup>'
    ) %>%
    stringr::str_replace_all(
      '\\^([+-])',
      '<sup>\\1</sup>'
    ) %>%
    stringr::str_replace_all(
      '\\bdegree\\b',
      '&deg;'
    ) %>%
    stringr::str_replace_all(
      '\\bmu\\b',
      '&micro;'
    ) %>%
    stringr::str_replace_all(
      '\\*',
      ''
    ) %>%
    stringr::str_replace_all(
      '~',
      ' '
    ) %>%
    stringr::str_replace_all(
      '"',
      ''
    )
}

format_report_tseries_overview = function(report_tseries_config,
                                          report_mobs_plotlist,
                                          period_label = NULL,
                                          summary_label = NULL) {
  
  cfg_tbl = tibble::tibble(
    code = purrr::map_chr(report_tseries_config, 'code'),
    resolution = purrr::map_chr(report_tseries_config, 'resolution')
  )
  
  selected_sites = cfg_tbl %>%
    dplyr::pull(code) %>%
    unique()
  
  summary_sites = cfg_tbl %>%
    dplyr::filter(resolution == 'summary') %>%
    dplyr::pull(code) %>%
    unique()
  
  raw_sites = cfg_tbl %>%
    dplyr::filter(resolution == 'raw') %>%
    dplyr::pull(code) %>%
    unique()
  
  all_summary = length(summary_sites) > 0 &&
    setequal(summary_sites, selected_sites)
  
  all_raw = length(raw_sites) > 0 &&
    setequal(raw_sites, selected_sites)
  
  sentences = character()
  
  if (isTRUE(all_summary)) {
    
    sentences = c(
      sentences,
      stringr::str_glue(
        '{stringr::str_to_sentence(summary_label)} data are shown for all selected sites.'
      )
    )
    
  } else if (length(summary_sites) > 0) {
    
    sentences = c(
      sentences,
      stringr::str_glue(
        '{stringr::str_to_sentence(summary_label)} data are shown for site{if (length(summary_sites) > 1) "s" else ""} {format_elements_to_print(summary_sites)}.'
      )
    )
  }
  
  if (isTRUE(all_raw)) {
    
    sentences = c(
      sentences,
      'Original time resolution data are shown for all selected sites.'
    )
    
  } else if (length(raw_sites) > 0) {
    
    sentences = c(
      sentences,
      stringr::str_glue(
        'Original time resolution data are shown for site{if (length(raw_sites) > 1) "s" else ""} {format_elements_to_print(raw_sites)}.'
      )
    )
  }
  
  if (!is.null(period_label)) {
    sentences = c(
      sentences,
      stringr::str_glue(
        'The period {period_label} is shown.'
      )
    )
  }
  
  sentences = c(
    sentences,
    'Click a site name to show or hide the plots.'
  )
  
  stringr::str_c(sentences, collapse = ' ')
}

format_runtime = function(seconds) {
  
  days = floor(
    seconds / 86400
  )
  
  hours = floor(
    (seconds %% 86400) / 3600
  )
  
  minutes = floor(
    (seconds %% 3600) / 60
  )
  
  secs = floor(
    seconds %% 60
  )
  
  if (days > 0) {
    
    sprintf(
      '%d d %02d:%02d:%02d',
      days,
      hours,
      minutes,
      secs
    )
    
  } else {
    
    sprintf(
      '%02d:%02d:%02d',
      hours,
      minutes,
      secs
    )
  }
}

format_summary_map_period_label = function(date_tag = NULL) {
  
  if (is.null(date_tag) || length(date_tag) == 0 || is.na(date_tag) || date_tag == '') {
    return('the full modelled period')
  }
  
  date_parts = stringr::str_match(
    date_tag,
    '^([0-9]{2}[A-Za-z]{3}[0-9]{2})_([0-9]{2}[A-Za-z]{3}[0-9]{2})$'
  )
  
  if (all(!is.na(date_parts))) {
    
    start_date = as.Date(date_parts[, 2], format = '%d%b%y')
    end_date = as.Date(date_parts[, 3], format = '%d%b%y')
    
    if (!is.na(start_date) && !is.na(end_date)) {
      return(
        stringr::str_glue(
          'the period {format(start_date, "%d %B %Y")} to {format(end_date, "%d %B %Y")}'
        )
      )
    }
  }
  
  stringr::str_glue('the {date_tag} modelled period')
}

format_summary_stat = function(stat) {
  
  if (stat == 'mean') {
    
    'Mean'
    
  } else if (stat == 'median') {
    
    'Median'
    
  } else if (stat == 'min') {
    
    'Minimum'
    
  } else if (stat == 'max') {
    
    'Maximum'
    
  } else if (
    stringr::str_detect(
      stat,
      '^p\\d{1,3}$'
    )
  ) {
    
    paste0(
      stringr::str_remove(
        stat,
        '^p'
      ),
      'th percentile'
    )
    
  } else {
    
    stringr::str_to_title(
      stat
    )
  }
}

format_summary_stat_plural = function(stat) {
  
  if (stat == 'mean') {
    
    'means'
    
  } else if (stat == 'median') {
    
    'medians'
    
  } else if (stat == 'min') {
    
    'minima'
    
  } else if (stat == 'max') {
    
    'maxima'
    
  } else if (
    stringr::str_detect(
      stat,
      '^p\\d{1,3}$'
    )
  ) {
    
    paste0(
      stringr::str_remove(
        stat,
        '^p'
      ),
      'th percentiles'
    )
    
  } else {
    
    paste0(
      stat,
      's'
    )
  }
}

format_temporal_var_label = function(
    var,
    var_params_list,
    output = c('plain', 'html', 'plotmath'),
    include_units = TRUE
) {
  
  output = match.arg(
    output
  )
  
  var_lab = format_var_label_emep(
    var = var,
    var_params_list = var_params_list,
    output = output,
    include_units = include_units
  )
  
  if (!stringr::str_starts(var, 'D3_')) {
    return(var_lab)
  }
  
  z_index = get_var_param(
    var = var,
    key = 'z_index',
    var_params_list = var_params_list,
    default = NULL
  )
  
  if (is.null(z_index)) {
    return(var_lab)
  }
  
  if (output == 'plotmath') {
    
    paste0(
      var_lab,
      '~(sigma~level~',
      z_index,
      ')'
    )
    
  } else {
    
    paste0(
      var_lab,
      ' (σ level ',
      z_index,
      ')'
    )
  }
}

format_units_for_html = function(x) {
  
  if (is.null(x) || is.na(x) || x == '') {
    return('')
  }
  
  x %>%
    format_units_for_plotmath() %>%
    stringr::str_replace_all(
      '\\^\\{([^}]+)\\}',
      '<sup>\\1</sup>'
    ) %>%
    stringr::str_replace_all(
      '\\^(-?[0-9]+)',
      '<sup>\\1</sup>'
    ) %>%
    stringr::str_replace_all(
      '\\bdegree\\b',
      '&deg;'
    ) %>%
    stringr::str_replace_all(
      '\\bmu\\b',
      '&micro;'
    ) %>%
    stringr::str_replace_all(
      '\\*',
      ''
    ) %>%
    stringr::str_replace_all(
      '~',
      ' '
    ) %>%
    stringr::str_replace_all(
      '"',
      ''
    )
}

format_units_for_plain = function(x) {
  
  if (is.null(x) || is.na(x) || x == '') {
    return('')
  }
  
  x %>%
    format_units_for_plotmath() %>%
    stringr::str_replace_all(
      'degree',
      '°'
    ) %>%
    stringr::str_replace_all(
      'mu',
      'µ'
    ) %>%
    stringr::str_replace_all(
      '\\^\\{-1\\}|\\^-1',
      '⁻¹'
    ) %>%
    stringr::str_replace_all(
      '\\^\\{-2\\}|\\^-2',
      '⁻²'
    ) %>%
    stringr::str_replace_all(
      '\\^\\{-3\\}|\\^-3',
      '⁻³'
    ) %>%
    stringr::str_replace_all(
      '\\*',
      ''
    ) %>%
    stringr::str_replace_all(
      '~',
      ' '
    ) %>%
    stringr::str_replace_all(
      '"',
      ''
    )
}

format_units_for_plotmath = function(x) {
  
  if (is.null(x) || is.na(x) || x == '') {
    return('')
  }
  
  dplyr::case_when(
    x %in% c('°', 'degrees', 'degree') ~ 'degree',
    x %in% c('°C', 'degC') ~ 'degree*C',
    x == '%' ~ '"%"',
    x == 'hPa' ~ 'hPa',
    x == 'Pa' ~ 'Pa',
    x == 'K' ~ 'K',
    x == 'mm' ~ 'mm',
    x %in% c('m/s', 'm s-1', 'm s⁻¹') ~ 'm~s^{-1}',
    x %in% c('m/s2', 'm s-2', 'm s⁻²') ~ 'm~s^{-2}',
    x %in% c(
      'ug/m3',
      'ug m-3',
      'µg/m3',
      'µg m-3',
      'µg m⁻³'
    ) ~ 'mu*g~m^{-3}',
    x %in% c(
      'mg/m3',
      'mg m-3',
      'mg m⁻³'
    ) ~ 'mg~m^{-3}',
    x %in% c(
      'mg/m2',
      'mg m-2',
      'mg m⁻²'
    ) ~ 'mg~m^{-2}',
    x %in% c(
      'mgS/m2',
      'mgS m-2',
      'mgS m⁻²'
    ) ~ 'mg~S~m^{-2}',
    
    x %in% c(
      'mgN/m2',
      'mgN m-2',
      'mgN m⁻²'
    ) ~ 'mg~N~m^{-2}',
    TRUE ~ x
  )
}

format_var_label = function(
    var,
    var_params_list,
    label_type = c('short', 'verbose'),
    output = c('html', 'plotmath', 'plain'),
    include_units = TRUE
) {
  
  label_type = match.arg(label_type)
  output = match.arg(output)
  
  lab_key = if (label_type == 'short') {
    'short_lab'
  } else {
    'verbose_lab'
  }
  
  lab = get_var_param(
    var = var,
    key = lab_key,
    var_params_list = var_params_list,
    default = var
  )
  
  units = get_var_param(
    var = var,
    key = 'units',
    var_params_list = var_params_list,
    default = ''
  )
  
  if (output == 'html') {
    
    lab_out = format_lab_for_html(lab)
    units_out = format_units_for_html(units)
    
    if (
      isTRUE(include_units) &&
      units_out != ''
    ) {
      return(
        stringr::str_glue(
          '{lab_out} ({units_out})'
        )
      )
    }
    
    return(lab_out)
  }
  
  if (output == 'plotmath') {
    
    lab_out = format_lab_for_plotmath(lab)
    units_out = format_units_for_plotmath(units)
    
    if (
      isTRUE(include_units) &&
      units_out != ''
    ) {
      return(
        stringr::str_glue(
          '{lab_out}~({units_out})'
        )
      )
    }
    
    return(lab_out)
  }
  
  lab_out = format_lab_for_plain(lab)
  units_out = format_units_for_plain(units)
  
  if (
    isTRUE(include_units) &&
    units_out != ''
  ) {
    return(
      stringr::str_glue(
        '{lab_out} ({units_out})'
      )
    )
  }
  
  lab_out
}

format_var_label_emep = function(
    var,
    var_params_list,
    context = c('maps', 'temporal', 'mobs'),
    output = c('html', 'plotmath', 'plain'),
    include_units = TRUE
) {
  
  # temporary function, once wrf qaqc is updated to the same var params file style
  # rename to 'format_var_label'
  
  context = match.arg(context)
  output = match.arg(output)
  
  var_id = if (context == 'mobs') {
    
    resolve_mobs_var_id(
      var = var,
      var_params_list = var_params_list
    )
    
  } else {
    
    resolve_var_id(
      var = var,
      var_params_list = var_params_list
    )
  }
  
  lab = get_var_param(
    var = var_id,
    key = c(
      context,
      'lab'
    ),
    var_params_list = var_params_list,
    default = var_id
  )
  
  units = get_var_param(
    var = var_id,
    key = 'units',
    var_params_list = var_params_list,
    default = ''
  )
  
  if (output == 'html') {
    
    lab_out = format_lab_for_html(
      lab
    )
    
    units_out = format_units_for_html(
      units
    )
    
    if (
      isTRUE(include_units) &&
      units_out != ''
    ) {
      
      return(
        stringr::str_glue(
          '{lab_out} ({units_out})'
        )
      )
    }
    
    return(lab_out)
  }
  
  if (output == 'plotmath') {
    
    lab_out = format_lab_for_plotmath(
      lab
    )
    
    units_out = format_units_for_plotmath(
      units
    )
    
    if (
      isTRUE(include_units) &&
      units_out != ''
    ) {
      
      return(
        stringr::str_glue(
          '{lab_out}~({units_out})'
        )
      )
    }
    
    return(lab_out)
  }
  
  lab_out = format_lab_for_plain(
    lab
  )
  
  units_out = format_units_for_plain(
    units
  )
  
  if (
    isTRUE(include_units) &&
    units_out != ''
  ) {
    
    return(
      stringr::str_glue(
        '{lab_out} ({units_out})'
      )
    )
  }
  
  lab_out
}

format_wrf_attr_value = function(x) {
  
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_character_)
  }
  
  paste(as.character(x), collapse = ', ')
}

format_wrf_maps_page_title = function(test_dir,
                                      ref_dir = NULL,
                                      domain,
                                      stat,
                                      wrap_width = 50) {
  
  test_dir = clean_wrf_run_dir(test_dir)
  ref_dir = clean_wrf_run_dir(ref_dir)
  
  stat_lab = if (stat == 'mean') {
    'mean'
  } else if (stat == 'accum') {
    'accum'
  } else {
    stat
  }
  
  format_run_line = function(label, run_dir) {
    
    run_desc = stringr::str_glue(
      '{run_dir} - domain {domain} {str_to_upper(stat_lab)}'
    )
    
    stringr::str_c(
      label,
      ' ',
      stringr::str_wrap(
        run_desc,
        width = wrap_width,
        exdent = nchar(label) + 1
      )
    )
  }
  
  if (is.na(ref_dir)) {
    
    pg_title = format_run_line(
      label = 'Model run:',
      run_dir = test_dir
    )
    
  } else {
    
    pg_title = stringr::str_c(
      format_run_line(
        label = 'Test run:',
        run_dir = test_dir
      ),
      '\n',
      format_run_line(
        label = 'Reference run:',
        run_dir = ref_dir
      )
    )
  }
  
  stringr::str_c(pg_title, '\n')
}

get_auto_data = function(
    site,
    network,
    pollutant,
    start_year,
    end_year,
    data_type = 'hourly'
) {
  
  checkmate::assert_choice(
    network,
    c(
      'aurn',
      'aqe',
      'saqn',
      'waqn',
      'ni',
      'local'
    )
  )
  
  checkmate::assert_integerish(
    start_year,
    len = 1,
    lower = 1900
  )
  
  checkmate::assert_integerish(
    end_year,
    len = 1,
    lower = start_year
  )
  
  year_range = start_year:end_year
  
  obs = openair::importUKAQ(
    site = site,
    year = year_range,
    source = network,
    data_type = data_type,
    pollutant = pollutant,
    meteo = FALSE,
    to_narrow = FALSE,
    verbose = TRUE,
    progress = FALSE
  )
  
  if (is.null(obs) || nrow(obs) == 0) {
    
    logger::log_warn(
      glue::glue(
        "No {network} data returned for site {site} ",
        "for {start_year}-{end_year}."
      )
    )
    
    return(NULL)
  }
  
  obs %>%
    dplyr::mutate(
      code = as.character(code)
    )
}

get_auto_meta = function(
    network,
    pollutant,
    year
) {
  
  meta = openair::importMeta(
    source = network,
    all = TRUE,
    year = year
  ) %>%
    dplyr::filter(
      stringr::str_to_lower(variable) %in%
        stringr::str_to_lower(pollutant)
    )
  
  if (nrow(meta) == 0) {
    return(
      tibble::tibble()
    )
  }
  
  meta %>%
    dplyr::mutate(
      source = as.character(source),
      code = as.character(code),
      poll = stringr::str_to_lower(variable)
    ) %>%
    dplyr::group_by(
      source,
      code,
      site,
      site_type,
      latitude,
      longitude,
      zone,
      agglomeration,
      local_authority
    ) %>%
    tidyr::nest(
      poll_info = c(
        variable,
        Parameter_name,
        poll,
        start_date,
        end_date,
        start_year,
        end_year,
        ratified_to
      )
    ) %>%
    dplyr::mutate(
      site_type_grp = recode_site_type(
        site_type
      ),
      .after = site_type
    ) %>%
    dplyr::ungroup()
}

#from rcolors package
get_color <- function(col, n = NULL, show = FALSE) {
  cols = if (length(col) > 1) col else rcolors::rcolors[[col]]
  if (is.null(n)) n = length(cols)
  
  cols = colorRampPalette(cols)(n)
  if (show) show_col(cols)
  cols
}

get_common_dir = function(...) {
  dirs = c(...) %>%
    purrr::discard(is.null)
  
  if (length(dirs) > 0) fs::path_common(dirs) else NULL
}

get_config_substitutions = function(
    run_script,
    config_file = 'config_emep.nml'
) {
  
  run_lines = readLines(
    run_script,
    warn = FALSE
  )
  
  sed_lines = run_lines %>%
    stringr::str_subset(
      stringr::fixed('sed ')
    ) %>%
    stringr::str_subset(
      stringr::fixed(config_file)
    )
  
  if (length(sed_lines) == 0) {
    return(
      tibble::tibble(
        search = character(),
        replacement = character()
      )
    )
  }
  
  purrr::map_dfr(
    sed_lines,
    function(line) {
      
      match = stringr::str_match(
        line,
        's/([^/]+)/([^/]+)/'
      )
      
      if (is.na(match[1, 1])) {
        return(
          tibble::tibble()
        )
      }
      
      tibble::tibble(
        search = match[1, 2],
        replacement = match[1, 3]
      )
    }
  )
}

get_emep_2d_slice = function(
    emep_stars,
    z_index = 1,
    time_index = 1
) {
  
  dims = stars::st_dimensions(
    emep_stars
  )
  
  dim_names = names(dims)
  
  dim_indexes = purrr::map(
    dim_names,
    function(dim_name) {
      
      if (dim_name == 'time') {
        
        time_index
        
      } else if (dim_name %in% c('z', 'lev')) {
        
        z_index
        
      } else {
        
        TRUE
      }
    }
  )
  
  do.call(
    `[`,
    c(
      list(emep_stars),
      list(TRUE),
      dim_indexes,
      list(drop = TRUE)
    )
  )
}

get_emep_combined_emission_netcdf_total = function(
    region_nm,
    pollutant_nm,
    emission_netcdf_vars,
    sectors = 'all',
    domain_crop = NULL
) {
  
  vars_to_read = emission_netcdf_vars %>%
    dplyr::filter(
      region == region_nm,
      pollutant == pollutant_nm
    )
  
  if (nrow(vars_to_read) == 0) {
    return(
      tibble::tibble(
        region = region_nm,
        pollutant = pollutant_nm,
        emission_value = NA_real_,
        emission_unit = NA_character_
      )
    )
  }
  
  units = unique(
    vars_to_read$units
  )
  
  if (length(units) != 1) {
    stop(
      "Multiple emission units found for ",
      pollutant_nm,
      ' / ',
      region_nm,
      ': ',
      paste(
        units,
        collapse = ', '
      )
    )
  }
  
  source_values = purrr::pmap_dbl(
    vars_to_read,
    function(
    source_id,
    resolved_fpath,
    var_name,
    pollutant,
    region,
    units
    ) {
      
      emis = suppressWarnings(
        stars::read_stars(
          resolved_fpath,
          sub = var_name,
          proxy = FALSE
        )
      )
      
      sf::st_crs(emis) = 4326
      
      # Select emission sectors
      
      available_sectors = stars::st_get_dimension_values(
        emis,
        'sector'
      ) %>%
        as.character()
      
      selected_sectors = resolve_selection(
        selection = sectors,
        available = available_sectors,
        input_name = 'EMISSION_NETCDF_SECTORS'
      )
      
      sector_index = match(
        selected_sectors,
        available_sectors
      )
      
      emis = dplyr::slice(
        emis,
        'sector',
        sector_index
      )
      
      # Sum across sectors and other non-spatial dimensions
      
      emis_crs = sf::st_crs(
        emis
      )
      
      emis = stars::st_apply(
        emis,
        MARGIN = c(
          'x',
          'y'
        ),
        FUN = sum,
        na.rm = TRUE
      )
      
      sf::st_crs(emis) = emis_crs
      
      # Restrict to model domain
      
      emis = apply_domain_crop(
        stars_object = emis,
        domain_crop = domain_crop
      )
      
      sum(
        emis[[1]],
        na.rm = TRUE
      )
    }
  )
  
  tibble::tibble(
    region = region_nm,
    pollutant = pollutant_nm,
    emission_value = sum(
      source_values
    ),
    emission_unit = units
  )
}

get_emep_combined_emission_netcdf_vars = function(
    nc_file,
    pollutants = NULL,
    regions = NULL
) {
  
  nc = ncdf4::nc_open(
    nc_file
  )
  
  on.exit(
    ncdf4::nc_close(nc)
  )
  
  var_names = names(
    nc$var
  )
  
  emission_vars = purrr::map_dfr(
    var_names,
    function(var_name) {
      
      species = ncdf4::ncatt_get(
        nc,
        var_name,
        'species'
      )$value
      
      region = ncdf4::ncatt_get(
        nc,
        var_name,
        'country_ISO'
      )$value
      
      units = ncdf4::ncatt_get(
        nc,
        var_name,
        'units'
      )$value
      
      if (
        identical(species, 0) ||
        identical(region, 0)
      ) {
        return(
          tibble::tibble()
        )
      }
      
      tibble::tibble(
        var_name = var_name,
        pollutant = as.character(species),
        region = as.character(region),
        units = as.character(units)
      )
    }
  )
  
  if (!is.null(pollutants)) {
    
    emission_vars = emission_vars %>%
      dplyr::filter(
        pollutant %in% pollutants
      )
  }
  
  if (!is.null(regions)) {
    
    emission_vars = emission_vars %>%
      dplyr::filter(
        region %in% regions
      )
  }
  
  emission_vars
}

get_emep_dep_element = function(unit) {
  
  if (stringr::str_detect(unit, '^mgN/')) {
    return('N')
  }
  
  if (stringr::str_detect(unit, '^mgS/')) {
    return('S')
  }
  
  NULL
}

get_emep_domain_polygon = function(
    emep_file,
    emep_crs
) {
  
  domain_var = get_emep_vars(
    emep_file
  )[1]
  
  if (
    length(domain_var) == 0 ||
    is.na(domain_var)
  ) {
    
    msg = glue::glue(
      "Could not find a usable model variable in '{emep_file}' to derive the model domain extent."
    )
    
    logger::log_error(msg)
    
    stop(msg)
  }
  
  domain_grid = read_emep(
    emep_fname = emep_file,
    emep_var = domain_var,
    emep_crs = emep_crs,
    proxy = FALSE,
    time_index = 1,
    driver = 'gdal'
  )
  
  domain_polygon = sf::st_as_sfc(
    sf::st_bbox(domain_grid)
  )
  
  sf::st_crs(domain_polygon) = sf::st_crs(
    domain_grid
  )
  
  domain_polygon %>%
    sf::st_as_sf()
}

get_emep_emissions_year = function(emep_fname) {
  
  fs::path_file(emep_fname) %>%
    stringr::str_extract('(?<=emiss)\\d{4}') %>%
    as.integer()
}

get_emep_floordate = function(emep_fname) {
  # outputs emep_fname time dimension to the start of its aggreggating period
  # except for fullrun (where it's not clear what the aggreggated period is) 
  
  emep_fname_t_res = fs::path_ext_remove(emep_fname) %>% 
    stringr::str_extract('_[^_]+$') %>% 
    stringr::str_sub(2)
  
  if (!emep_fname_t_res %in% c('fullrun', 'month', 'day', 'hour')) {
    stop(
      "Could not infer EMEP temporal resolution from file name: ",
      emep_fname
    )
  }
  
  nc_date = get_nc_time(emep_fname)
  
  if (emep_fname_t_res != 'fullrun') {
    nc_date = nc_date %>%
      lubridate::floor_date(
        unit = emep_fname_t_res
      )
  }
  
  tibble::tibble(
    date = nc_date
  )
}

get_emep_grid_type = function(emep_file) {
  
  nc = ncdf4::nc_open(emep_file)
  on.exit(ncdf4::nc_close(nc))
  
  nc_dims = names(nc$dim)
  nc_vars = names(nc$var)
  
  # Regular lon/lat grid
  lon_dim = nc_dims[
    stringr::str_detect(
      stringr::str_to_lower(nc_dims),
      '(^lon$|longitude)'
    )
  ]
  
  lat_dim = nc_dims[
    stringr::str_detect(
      stringr::str_to_lower(nc_dims),
      '(^lat$|latitude)'
    )
  ]
  
  if (
    length(lon_dim) == 1 &&
    length(lat_dim) == 1 &&
    nc$dim[[lon_dim]]$units == 'degrees_east' &&
    nc$dim[[lat_dim]]$units == 'degrees_north'
  ) {
    return('latlon')
  }
  
  # Projected grid: identify 2-D latitude variable
  lat_var = nc_vars[
    stringr::str_detect(
      stringr::str_to_lower(nc_vars),
      '(^lat$|latitude)'
    )
  ]
  
  if (length(lat_var) == 0) {
    stop(
      glue::glue(
        "Could not identify the EMEP grid type in: {emep_file}"
      ),
      call. = FALSE
    )
  }
  
  if (length(lat_var) > 1) {
    stop(
      glue::glue(
        "Multiple possible latitude variables were found in: {emep_file}\n",
        "Candidates: {paste(lat_var, collapse = ', ')}"
      ),
      call. = FALSE
    )
  }
  
  lat_ndims = length(
    nc$var[[lat_var]]$dim
  )
  
  dplyr::case_when(
    lat_ndims == 1 ~ 'latlon',
    lat_ndims == 2 ~ 'stereo',
    TRUE ~ NA_character_
  )
}

get_emep_indexes = function(longitude, latitude, emep_pth) {
  #determines the grid x and y indexes of a point given the point longitude and a
  #matrix of longitudes in the NetCDF
  #!!! at present doesn't handle points outside of the modelled domain
  #so it's necessary to check prior using this function
  nc = nc_open(emep_pth)
  nc_lon = ncvar_get(nc, "lon")
  nc_lat = ncvar_get(nc, "lat")
  nc_close(nc)
  
  dist_lat = abs(nc_lat - latitude)
  dist_lon = abs(nc_lon - longitude)
  
  if(length(dim(nc_lon)) == 2){
    dist = sqrt(dist_lat ^ 2 + dist_lon ^ 2)
    index = which(dist == min(dist), arr.ind = TRUE)
    return(as_tibble(index))
  } else {
    return(tibble(row = which(dist_lon == min(dist_lon)),
                  col = which(dist_lat == min(dist_lat))))
  }
}

get_emep_time_range = function(
    emep_fname,
    fmt = '%Y-%m-%d %H:%M',
    tz = 'UTC'
) {
  
  nc_date = get_emep_floordate(
    emep_fname
  )
  
  if (
    is.null(nc_date) ||
    nrow(nc_date) == 0 ||
    !'date' %in% names(nc_date)
  ) {
    return(NULL)
  }
  
  first_time = min(
    nc_date$date,
    na.rm = TRUE
  )
  
  last_timestamp = max(
    nc_date$date,
    na.rm = TRUE
  )
  
  resolution = stringr::str_extract(
    fs::path_file(emep_fname),
    '(hour|day|month|fullrun)(?=\\.nc$)'
  )
  
  last_time = dplyr::case_when(
    resolution == 'hour' ~
      last_timestamp + lubridate::hours(1),
    
    resolution == 'day' ~
      last_timestamp + lubridate::days(1),
    
    resolution == 'month' ~
      last_timestamp %m+%
      lubridate::period(months = 1),
    
    TRUE ~
      last_timestamp
  )
  
  list(
    first_time = first_time,
    last_time = last_time,
    first_time_str = format(
      first_time,
      fmt,
      tz = tz
    ),
    last_time_str = format(
      last_time,
      fmt,
      tz = tz
    ),
    start_year = lubridate::year(
      first_time
    ),
    end_year = lubridate::year(
      last_time - lubridate::seconds(1)
    )
  )
}

get_emep_vars = function(nc_file,
                         var_keywords = c('SURF', 'Emis', 'DDEP', 'WDEP')) {
  # returns NetCDF variable names whose names start with one of the supplied
  # keywords
  #
  # if var_keywords is NULL, all NetCDF variables are returned
  #
  # variables are returned in the order implied by var_keywords
  #
  # nc_file:
  #   path to the NetCDF file to inspect
  #
  # var_keywords:
  #   character vector of variable-name prefixes to search for, in priority order
  #   if NULL, all variables are returned
  #
  # output:
  #   character vector of matching variable names
  #   returns character(0) if no matching variables are found
  
  nc = ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  
  nc_vars = names(nc$var)
  
  if (is.null(var_keywords)) {
    return(nc_vars)
  }
  
  out = purrr::map(
    var_keywords,
    function(keyword) {
      nc_vars[stringr::str_detect(nc_vars, paste0('^', keyword))]
    }
  ) %>%
    unlist(use.names = FALSE) %>%
    unique()
  
  return(out)
}

get_emep_z_slice = function(
    emep_stars,
    z_index
) {
  
  dims = stars::st_dimensions(
    emep_stars
  )
  
  dim_names = names(dims)
  
  if (!'z' %in% dim_names) {
    return(emep_stars)
  }
  
  if (is.null(z_index)) {
    stop(
      'A z_index must be supplied for EMEP data with a vertical dimension.',
      call. = FALSE
    )
  }
  
  z_length = dims[['z']]$to - dims[['z']]$from + 1
  
  if (
    z_index < 1 ||
    z_index > z_length
  ) {
    stop(
      'z_index is outside the available vertical dimension.',
      call. = FALSE
    )
  }
  
  dim_indexes = purrr::map(
    dim_names,
    function(dim_name) {
      
      if (dim_name == 'z') {
        z_index
      } else {
        TRUE
      }
    }
  )
  
  do.call(
    `[`,
    c(
      list(emep_stars),
      list(TRUE),
      dim_indexes,
      list(drop = TRUE)
    )
  )
}

get_emission_source_files = function(
    config_file
) {
  
  config_lines = readLines(
    config_file,
    warn = FALSE
  )
  
  matches = stringr::str_match(
    config_lines,
    paste0(
      "Emis_sourceFiles\\((\\d+)\\)%filename\\s*=\\s*",
      "['\"]([^'\"]+)['\"]"
    )
  )
  
  tibble::tibble(
    source_id = suppressWarnings(
      as.integer(matches[, 2])
    ),
    filename = matches[, 3]
  ) %>%
    dplyr::filter(
      !is.na(source_id),
      !is.na(filename)
    ) %>%
    dplyr::arrange(
      source_id
    )
}

get_comp_map_fig_height = function(map_stars,
                                   n_plots,
                                   fig_width = 7,
                                   aspect_pad = 1.1,
                                   cbar_pad = 1.0,
                                   min_height = 3.5,
                                   max_height = 12) {
  
  if (is.null(fig_width) || is.na(fig_width)) {
    fig_width = 7
  }
  
  dims = stars::st_dimensions(map_stars)
  
  x_dim = intersect(c('x', 'lon'), names(dims))[1]
  y_dim = intersect(c('y', 'lat'), names(dims))[1]
  
  if (is.na(x_dim) || is.na(y_dim)) {
    return(min(max_height, max(min_height, fig_width * 0.7)))
  }
  
  domain_dims = purrr::map_int(
    c(x_dim, y_dim),
    ~ length(stars::st_get_dimension_values(map_stars, which = .x))
  )
  
  names(domain_dims) = c('x', 'y')
  
  n_cols = if (n_plots == 1) {
    1
  } else {
    2
  }
  
  n_rows = ceiling(n_plots / n_cols)
  
  panel_width = fig_width / n_cols
  
  panel_height = panel_width *
    (domain_dims[['y']] / domain_dims[['x']]) *
    aspect_pad
  
  fig_height = n_rows * panel_height + cbar_pad
  
  max(
    min_height,
    min(max_height, fig_height)
  )
}

get_mobs_map_stats_for_var = function(var,
                                      stats,
                                      var_params_list,
                                      include_by_var = NULL,
                                      exclude_by_var = NULL) {
  
  var_id = resolve_var_id(
    var = var,
    var_params_list = var_params_list
  )
  
  stats_out = stats
  
  include_stats = unique(c(
    include_by_var[[var]],
    include_by_var[[var_id]]
  ))
  
  if (length(include_stats) > 0) {
    stats_out = intersect(stats_out, include_stats)
  }
  
  exclude_stats = unique(c(
    exclude_by_var[[var]],
    exclude_by_var[[var_id]]
  ))
  
  if (length(exclude_stats) > 0) {
    stats_out = setdiff(stats_out, exclude_stats)
  }
  
  stats_out
}

get_mobs_plot_aesthetics = function(
    var_nm,
    var_params_list = NULL,
    mod_colour_default = 'gray20',
    mod_fill_default = 'gray90',
    mod_linetype_default = 'solid',
    mod_linewidth_default = 0.7,
    obs_colour_default = 'black',
    obs_fill_default = 'gray80',
    obs_linetype_default = 'solid',
    obs_linewidth_default = 0.7,
    pointsize_default = 2
) {
  
  if (is.null(var_params_list)) {
    return(
      list(
        mod_colour = mod_colour_default,
        mod_fill = mod_fill_default,
        mod_linetype = mod_linetype_default,
        mod_linewidth = mod_linewidth_default,
        obs_colour = obs_colour_default,
        obs_fill = obs_fill_default,
        obs_linetype = obs_linetype_default,
        obs_linewidth = obs_linewidth_default,
        pointsize = pointsize_default
      )
    )
  }
  
  var_id = resolve_mobs_var_id(
    var = var_nm,
    var_params_list = var_params_list
  )
  
  mobs_params = var_params_list[[var_id]][['mobs']]
  
  list(
    mod_colour = mobs_params[['mod_colour']] %||%
      mod_colour_default,
    mod_fill = mobs_params[['mod_fill']] %||%
      mod_fill_default,
    mod_linetype = mobs_params[['mod_linetype']] %||%
      mod_linetype_default,
    mod_linewidth = mobs_params[['mod_linewidth']] %||%
      mod_linewidth_default,
    
    obs_colour = mobs_params[['obs_colour']] %||%
      obs_colour_default,
    obs_fill = mobs_params[['obs_fill']] %||%
      obs_fill_default,
    obs_linetype = mobs_params[['obs_linetype']] %||%
      obs_linetype_default,
    obs_linewidth = mobs_params[['obs_linewidth']] %||%
      obs_linewidth_default,
    
    pointsize = mobs_params[['pointsize']] %||%
      pointsize_default
  )
}

get_modstat_legend_formatter = function(stat) {
  
  if (stat %in% c('r', 'r_pearson', 'r_spearman')) {
    return(
      scales::label_number(
        accuracy = 0.1,
        drop0trailing = TRUE
      )
    )
  }
  
  if (stat %in% c('NMB')) {
    return(
      scales::label_number(
        accuracy = 1,
        drop0trailing = TRUE
      )
    )
  }
  
  scales::label_number(
    accuracy = NULL,
    drop0trailing = TRUE
  )
}

get_modstats_page_length = function(n_vars,
                                    target = 20,
                                    min_page_length = 10) {
  
  if (is.null(n_vars) || length(n_vars) == 0 || is.na(n_vars) || n_vars < 1) {
    return(target)
  }
  
  candidates = seq(n_vars, max(target * 2, n_vars), by = n_vars)
  
  candidates = candidates[candidates >= min_page_length]
  
  if (length(candidates) == 0) {
    return(n_vars)
  }
  
  candidates[which.min(abs(candidates - target))]
}

get_obs_file_time_resolution = function(obs_file) {
  
  obs = read_processed_obs(
    obs_file
  )
  
  has_date = 'date' %in% names(obs)
  has_start = 'start_date' %in% names(obs)
  has_end = 'end_date' %in% names(obs)
  
  if (has_start != has_end) {
    stop(
      'Observation data must contain both start_date and end_date ',
      'when either is supplied: ',
      obs_file,
      call. = FALSE
    )
  }
  
  if (!has_date && !has_start) {
    stop(
      'Observation data must contain either date or ',
      'start_date and end_date: ',
      obs_file,
      call. = FALSE
    )
  }
  
  # Explicit aggregation periods take precedence over date.
  
  if (has_start && has_end) {
    
    if (any(
      obs$end_date <= obs$start_date,
      na.rm = TRUE
    )) {
      
      stop(
        'Observation end_date must be later than start_date: ',
        obs_file,
        call. = FALSE
      )
    }
    
    period_boundaries = c(
      as.POSIXct(obs$start_date, tz = 'UTC'),
      as.POSIXct(obs$end_date, tz = 'UTC')
    )
    
    if (any(
      lubridate::minute(period_boundaries) != 0 |
      lubridate::second(period_boundaries) != 0
    )) {
      
      stop(
        'Observation interval boundaries must be hour-starting: ',
        obs_file,
        call. = FALSE
      )
    }
    
    if (any(
      lubridate::hour(period_boundaries) != 0
    )) {
      return('hour')
    }
    
    if (any(
      lubridate::day(period_boundaries) != 1
    )) {
      return('day')
    }
    
    return('month')
  }
  
  infer_obs_time_resolution(
    obs$date
  )
}

get_palette_package = function(x) {
  if (!stringr::str_detect(x, '^[[:alnum:]_.]+::[[:alnum:]_.]+$')) {
    return(NULL)
  }
  
  stringr::str_extract(x, '^[[:alnum:]_.]+')
}

get_nc_time = function(emep_fname, tz = 'UTC') {
  
  nc = ncdf4::nc_open(emep_fname)
  on.exit(ncdf4::nc_close(nc))
  
  if (!'time' %in% names(nc$dim) && !'time' %in% names(nc$var)) {
    stop("No 'time' coordinate found in: ", emep_fname)
  }
  
  time = ncdf4::ncvar_get(nc, 'time')
  
  time_units = ncdf4::ncatt_get(
    nc,
    'time',
    'units'
  )$value
  
  unit = stringr::str_extract(
    time_units,
    '^[A-Za-z]+'
  )
  
  origin = stringr::str_remove(
    time_units,
    '^[A-Za-z]+\\s+since\\s+'
  )
  
  origin = lubridate::ymd_hms(
    origin,
    tz = tz,
    quiet = TRUE
  )
  
  if (is.na(origin)) {
    stop(
      "Could not interpret NetCDF time origin: ",
      time_units
    )
  }
  
  multiplier = dplyr::case_when(
    unit %in% c('second', 'seconds') ~ 1,
    unit %in% c('minute', 'minutes') ~ 60,
    unit %in% c('hour', 'hours') ~ 3600,
    unit %in% c('day', 'days') ~ 86400,
    TRUE ~ NA_real_
  )
  
  if (is.na(multiplier)) {
    stop(
      "Unsupported NetCDF time unit: ",
      unit
    )
  }
  
  origin + lubridate::seconds(time * multiplier)
}

get_nc_var_units = function(nc_file, vars) {
  
  nc = ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  
  vars = intersect(
    vars,
    names(nc$var)
  )
  
  if (length(vars) == 0) {
    return(character(0))
  }
  
  units = purrr::map_chr(
    vars,
    function(var) {
      ncdf4::ncatt_get(
        nc,
        varid = var,
        attname = 'units'
      )$value
    }
  )
  
  stats::setNames(
    units,
    vars
  )
}

get_region_domain_coverage = function(
    region_geofiles,
    emep_file,
    emep_crs
) {
  
  # calculates the proportion of each region geofile that lies within the
  # model domain of a representative EMEP file
  #
  # if a geofile contains a 'frac_area' column, coverage is calculated using
  # the weighted grid-mask features
  # otherwise, coverage is calculated from polygon-area overlap
  
  domain_polygon = get_emep_domain_polygon(
    emep_file = emep_file,
    emep_crs = emep_crs
  )
  
  purrr::pmap_dfr(
    region_geofiles,
    function(region_id, fpath) {
      
      region_sf = sf::st_read(
        fpath,
        quiet = TRUE
      ) %>%
        sf::st_transform(
          sf::st_crs(domain_polygon)
        )
      
      if ('frac_area' %in% names(region_sf)) {
        
        # EMEP region geofiles contain frac_area, giving the fraction
        # of each EMEP grid cell belonging to the region
        
        inside = as.vector(
          sf::st_intersects(
            region_sf,
            domain_polygon,
            sparse = FALSE
          )
        )
        
        total_area = sum(
          region_sf$frac_area,
          na.rm = TRUE
        )
        
        covered_area = sum(
          region_sf$frac_area[inside],
          na.rm = TRUE
        )
        
      } else {
        
        # For general polygon geofiles, calculate coverage from the
        # geometric overlap with the model domain
        
        region_union = sf::st_union(
          region_sf
        )
        
        region_area = as.numeric(
          sf::st_area(
            region_union
          )
        )
        
        intersect_geom = suppressWarnings(
          sf::st_intersection(
            region_union,
            domain_polygon
          )
        )
        
        covered_area = if (length(intersect_geom) == 0) {
          0
        } else {
          as.numeric(
            sf::st_area(
              sf::st_union(
                intersect_geom
              )
            )
          )
        }
        
        total_area = region_area
      }
      
      tibble::tibble(
        region_id = region_id,
        fpath = fpath,
        total_area = total_area,
        covered_area = covered_area,
        coverage_prop = covered_area / total_area
      )
    }
  )
}

get_region_geofiles = function(
    geodata_dir,
    regions = 'all',
    exclude_files = NULL
) {
  
  # returns a table of geofiles
  #
  # geofiles are searched in geodata_dir only (no recursion)
  # for shapefiles, only the .shp file is retained
  #
  # region selection is controlled by regions:
  # - 'all' -> all available geofiles
  # - c('GB', 'IE', 'FR') -> only those region IDs
  # - c('all', '-RU', '-TR') -> all available geofiles except RU and TR
  #
  # region_id is defined as the string after the last underscore in the file stem
  # e.g.
  # 'EMEP_grid_01deg_shp_AL.gpkg' -> 'AL'
  #
  # exclude_files:
  # optional file path(s) to exclude from the available geofiles
  
  geofiles = fs::dir_ls(
    geodata_dir,
    recurse = FALSE,
    type = 'file'
  ) %>%
    purrr::keep(~ {
      ext = tolower(
        fs::path_ext(.x)
      )
      
      ext %in% c(
        'gpkg',
        'geojson',
        'shp'
      )
    })
  
  if (!is.null(exclude_files)) {
    
    geofiles = geofiles[
      !fs::path_abs(geofiles) %in%
        fs::path_abs(exclude_files)
    ]
  }
  
  out = tibble::tibble(
    fpath = geofiles
  ) %>%
    dplyr::mutate(
      stem = fs::path_ext_remove(
        fs::path_file(fpath)
      ),
      region_id = stringr::word(
        stem,
        start = -1,
        sep = stringr::fixed('_')
      )
    ) %>%
    dplyr::filter(
      !is.na(region_id),
      region_id != ''
    ) %>%
    dplyr::select(
      region_id,
      fpath
    ) %>%
    dplyr::distinct()
  
  selected_region_ids = resolve_selection(
    selection = regions,
    available = out$region_id,
    input_name = 'regions'
  )
  
  out %>%
    dplyr::filter(
      region_id %in% selected_region_ids
    ) %>%
    dplyr::arrange(
      match(
        region_id,
        selected_region_ids
      )
    )
}

get_run_script_var = function(
    run_script,
    var_name
) {
  
  run_lines = readLines(
    run_script,
    warn = FALSE
  )
  
  # Remove blank lines and full-line comments
  run_lines = run_lines %>%
    stringr::str_trim() %>%
    stringr::str_subset(
      '^[^#].*=.*$'
    )
  
  # Extract shell variable assignments
  assignments = stringr::str_match(
    run_lines,
    '^([A-Za-z_][A-Za-z0-9_]*)=(.*)$'
  )
  
  assignment_tbl = tibble::tibble(
    var = assignments[, 2],
    value = assignments[, 3]
  ) %>%
    dplyr::filter(
      !is.na(var)
    ) %>%
    dplyr::mutate(
      value = stringr::str_remove(
        value,
        '\\s+#.*$'
      ),
      value = stringr::str_trim(
        value
      ),
      value = stringr::str_remove(
        value,
        '^["\']'
      ),
      value = stringr::str_remove(
        value,
        '["\']$'
      )
    )
  
  if (!var_name %in% assignment_tbl$var) {
    
    stop(
      glue::glue(
        "Variable '{var_name}' was not found in run script:\n",
        "{run_script}"
      ),
      call. = FALSE
    )
  }
  
  resolve_var = function(
    current_var,
    seen = character()
  ) {
    
    if (current_var %in% seen) {
      
      stop(
        glue::glue(
          "Circular variable reference encountered while resolving ",
          "'{var_name}' in:\n",
          "{run_script}"
        ),
        call. = FALSE
      )
    }
    
    value = assignment_tbl %>%
      dplyr::filter(
        var == current_var
      ) %>%
      dplyr::pull(
        value
      ) %>%
      dplyr::last()
    
    if (length(value) == 0) {
      return(NA_character_)
    }
    
    refs = stringr::str_match_all(
      value,
      '\\$\\{([A-Za-z_][A-Za-z0-9_]*)\\}'
    )[[1]]
    
    if (nrow(refs) == 0) {
      return(value)
    }
    
    for (ref_var in refs[, 2]) {
      
      if (!ref_var %in% assignment_tbl$var) {
        next
      }
      
      ref_value = resolve_var(
        current_var = ref_var,
        seen = c(
          seen,
          current_var
        )
      )
      
      value = stringr::str_replace_all(
        value,
        stringr::fixed(
          paste0(
            '${',
            ref_var,
            '}'
          )
        ),
        ref_value
      )
    }
    
    value
  }
  
  resolved_value = resolve_var(
    current_var = var_name
  )
  
  if (
    stringr::str_detect(
      resolved_value,
      '^/'
    )
  ) {
    
    resolved_value =
      fs::path_norm(
        resolved_value
      )
  }
  
  resolved_value
}

get_sites_in_domain = function(
    site_geo,
    emep_2d_slice
) {
  
  site_geo_model_crs = site_geo %>%
    sf::st_transform(
      sf::st_crs(emep_2d_slice)
    )
  
  sites_in = stars::st_extract(
    emep_2d_slice,
    site_geo_model_crs
  ) %>%
    sf::st_drop_geometry() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      site_in_domain = !dplyr::if_all(
        dplyr::everything(),
        is.na
      )
    ) %>%
    dplyr::pull(
      site_in_domain
    )
  
  site_geo[
    sites_in,
  ]
}

get_stars_range = function(stars_list) {
  vals = map(stars_list, ~as.vector(.x[[1]])) %>% 
    flatten_dbl() %>% 
    discard(is.na)
  
  if (length(vals) == 0) {
    return(c(min = NA_real_, max = NA_real_))
  }
  
  c(min = min(vals), max = max(vals))
}

get_summary_map_cbar_width = function(n_plot_cols,
                                      fig_width = 8.4,
                                      cbar_width = NULL,
                                      width_fraction = 0.7,
                                      min_width = 2.4,
                                      max_width = 6.0) {
  
  if (!is.null(cbar_width)) {
    
    if (inherits(cbar_width, 'unit')) {
      return(cbar_width)
    }
    
    return(grid::unit(cbar_width, 'inches'))
  }
  
  width = fig_width / n_plot_cols * width_fraction
  
  width = max(min_width, width)
  width = min(max_width, width)
  
  grid::unit(width, 'inches')
}

get_temporal_comparison_period = function(
    time,
    resolution
) {
  
  resolution = unique(
    resolution
  )
  
  if (length(resolution) != 1) {
    stop(
      'Temporal comparison data must contain a single resolution.',
      call. = FALSE
    )
  }
  
  floored_time = dplyr::case_when(
    resolution == 'hour' ~ lubridate::floor_date(time, 'hour'),
    resolution == 'day' ~ lubridate::floor_date(time, 'day'),
    resolution == 'month' ~ lubridate::floor_date(time, 'month'),
    TRUE ~ time
  )
  
  dplyr::case_when(
    resolution == 'hour' ~ format(floored_time, '%m-%d %H:00'),
    resolution == 'day' ~ format(floored_time, '%m-%d'),
    resolution == 'month' ~ format(floored_time, '%m'),
    TRUE ~ format(floored_time, '%Y-%m-%d %H:%M:%S')
  )
}

get_temporal_param = function(
    var,
    key,
    var_params_list,
    common_value = NULL,
    default = NULL
) {
  
  var_value = get_var_param(
    var = var,
    key = c(
      'temporal',
      key
    ),
    var_params_list = var_params_list,
    default = NULL
  )
  
  if (!is.null(var_value)) {
    return(var_value)
  }
  
  if (!is.null(common_value)) {
    return(common_value)
  }
  
  default
}

get_years_from_emep = function(nc_file, time_var = 'time') {
  # extracts the start and end years from the NetCDF time variable
  # supports EMEP-style time units such as:
  # - 'days since 1900-1-1 0:0:0'
  # - 'hours since 1900-1-1 0:0:0'
  #
  # returns a list with:
  # - start_year
  # - end_year
  #
  # if the file spans multiple years, a warning is raised
  
  if (is.null(nc_file) || is.na(nc_file)) {
    return(NULL)
  }
  
  nc = ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  
  t_vals = ncdf4::ncvar_get(nc, time_var)
  
  if (length(t_vals) == 0 || all(is.na(t_vals))) {
    return(NULL)
  }
  
  t_units = ncdf4::ncatt_get(nc, time_var, 'units')$value
  origin_str = stringr::str_extract(t_units, '\\d{4}-\\d{1,2}-\\d{1,2}')
  
  if (is.na(origin_str)) {
    stop("Unknown time units format in NetCDF: ", t_units)
  }
  
  get_date_from_offset = function(t_val) {
    if (stringr::str_detect(t_units, '^days\\s+since')) {
      return(as.Date(origin_str) + t_val)
    }
    
    if (stringr::str_detect(t_units, '^hours\\s+since')) {
      origin_time = as.POSIXct(origin_str, tz = 'UTC')
      return(as.Date(origin_time + t_val * 3600, tz = 'UTC'))
    }
    
    stop(
      "Unsupported time-unit format: ", t_units,
      "\n  Expected 'days since ...' or 'hours since ...'."
    )
  }
  
  first_date = get_date_from_offset(t_vals[1])
  last_date = get_date_from_offset(t_vals[length(t_vals)])
  
  start_year = lubridate::year(first_date)
  end_year = lubridate::year(last_date)
  
  if (start_year != end_year) {
    warning(
      "NetCDF file spans multiple years: ",
      start_year, " to ", end_year, "."
    )
  }
  
  return(list(
    start_year = start_year,
    end_year = end_year
  ))
}

get_var_order_index = function(x,
                               var_params_list) {
  
  resolved = purrr::map_chr(
    as.character(x),
    resolve_var_id,
    var_params_list = var_params_list
  )
  
  match(
    resolved,
    names(var_params_list)
  )
}

get_var_param = function(
    var,
    key,
    var_params_list,
    default = NULL
) {
  
  var_id = resolve_var_id(
    var = var,
    var_params_list = var_params_list
  )
  
  x = var_params_list[[var_id]]
  
  for (this_key in key) {
    
    if (
      is.null(x) ||
      !this_key %in% names(x)
    ) {
      return(default)
    }
    
    x = x[[this_key]]
  }
  
  x
}

get_wrf_attr = function(attrs, nm, default = NA) {
  
  if (!nm %in% names(attrs)) {
    return(default)
  }
  
  val = attrs[[nm]]
  
  if (length(val) == 0) {
    return(default)
  }
  
  val
}

get_wrf_global_attrs = function(wrf_file) {
  
  nc = ncdf4::nc_open(wrf_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  
  ncdf4::ncatt_get(nc, varid = 0)
}

infer_obs_time_resolution = function(x) {
  
  if (!inherits(x, c('Date', 'POSIXct', 'POSIXt'))) {
    stop(
      'Time values must be Date or POSIX date-time values.',
      call. = FALSE
    )
  }
  
  x = x %>%
    stats::na.omit() %>%
    sort() %>%
    unique()
  
  if (length(x) < 2) {
    stop(
      'At least two observation timestamps are required to infer ',
      'temporal resolution.',
      call. = FALSE
    )
  }
  
  # Date-times must always be hour-starting.
  
  if (!inherits(x, 'Date')) {
    
    if (any(
      lubridate::minute(x) != 0 |
      lubridate::second(x) != 0
    )) {
      
      stop(
        'Observation timestamps must be hour-starting.',
        call. = FALSE
      )
    }
    
    # Any non-midnight period start requires hourly EMEP output.
    
    if (any(lubridate::hour(x) != 0)) {
      return('hour')
    }
  }
  
  x_date = as.Date(x)
  
  # More than one period start within a month requires daily output.
  
  if (anyDuplicated(
    lubridate::floor_date(
      x_date,
      'month'
    )
  )) {
    return('day')
  }
  
  # Monthly or annual data must start on the first day of the month.
  
  if (!all(lubridate::day(x_date) == 1)) {
    stop(
      'Observation timestamps appear to represent periods longer than ',
      'one day but do not start on the first day of the month.',
      call. = FALSE
    )
  }
  
  # More than one period start within a year requires monthly output.
  
  if (anyDuplicated(
    lubridate::floor_date(
      x_date,
      'year'
    )
  )) {
    return('month')
  }
  
  # Annual data must start on 1 January.
  
  if (!all(
    lubridate::month(x_date) == 1
  )) {
    
    stop(
      'Observation timestamps appear to represent annual periods but ',
      'do not start on 1 January.',
      call. = FALSE
    )
  }
  
  'year'
}

load_map_geo_features = function(geo_list) {
  # loads geographic features used for map overlays from a user-supplied
  # mapping list
  #
  # each entry in geo_list may define either:
  # - a file path in 'pth', which is read with sf::st_read()
  # - a Natural Earth dataset name in 'data', e.g. 'ne_countries'
  # - an already-loaded spatial object in 'data'
  #
  # for each valid entry, the function populates/updates the 'data' element and
  # adds an 'is_global' flag indicating whether the source is a global Natural
  # Earth layer
  #
  # entries where both 'pth' and 'data' are NULL are dropped
  #
  # geo_list:
  #   list of mapping feature definitions, typically from MAPPING_GEO_LIST
  #   each element is expected to contain some combination of:
  #   - 'pth': file path to a spatial layer
  #   - 'data': either a Natural Earth function name or an sf object
  #   - 'ne_scale': Natural Earth scale to use if 'data' is a Natural Earth layer
  #
  # output:
  #   list with the same structure as geo_list, but with spatial data loaded into
  #   the 'data' element where needed, and with an added logical 'is_global' flag
  
  if (is.null(geo_list)) return(NULL)
  
  list_out = geo_list
  
  for (i in seq_along(geo_list)) {
    entry = geo_list[[i]]
    
    if (is.null(entry[['pth']]) && is.null(entry[['data']])) {
      list_out[[i]] = NULL
      next
    }
    
    if (is.null(entry[['data']])) {
      # Data from a file path
      list_out[[i]][['data']] = sf::st_read(entry[['pth']])
      list_out[[i]][['is_global']] = FALSE
    } else if (is.character(entry[['data']]) && stringr::str_starts(entry[['data']], 'ne_')) {
      # NaturalEarth source
      func = get(entry[['data']], envir = asNamespace('rnaturalearth'))
      list_out[[i]][['data']] = func(scale = entry[['ne_scale']], returnclass = 'sf')
      list_out[[i]][['is_global']] = TRUE
    } else {
      # Already an sf object passed in
      list_out[[i]][['is_global']] = FALSE
    }
  }
  
  list_out
}

lookup_wrf_option = function(x, lookup) {
  
  x_chr = as.character(x)
  
  if (length(x_chr) == 0 || is.na(x_chr)) {
    return(NA_character_)
  }
  
  lab = purrr::pluck(
    lookup,
    x_chr,
    .default = NULL
  )
  
  if (is.null(lab)) {
    return(x_chr)
  }
  
  stringr::str_glue('{x_chr} - {lab}')
}

make_emep_summary_dt = function(
    df,
    run_labels,
    threshold = 5,
    caption = NULL,
    page_length = 25
) {
  
  if (is.null(df) || nrow(df) == 0) {
    return(
      htmltools::tags$p(
        'No EMEP summary results available.'
      )
    )
  }
  
  test_label = run_labels[1]
  ref_label = run_labels[2]
  
  has_ref = all(
    c(
      'ref_value',
      'ref_unit',
      'abs_diff',
      'rel_diff'
    ) %in% names(df)
  )
  
  # Check that test and reference units agree where both are available
  
  if (has_ref) {
    
    mismatched_units = df %>%
      dplyr::filter(
        !is.na(ref_value),
        !is.na(ref_unit),
        test_unit != ref_unit
      )
    
    if (nrow(mismatched_units) > 0) {
      stop(
        "Test and reference units do not match for the following variables: ",
        paste(
          mismatched_units$variable,
          collapse = ', '
        )
      )
    }
  }
  
  # Prepare display data
  
  df_out = df %>%
    dplyr::mutate(
      unit = test_unit
    ) %>%
    dplyr::arrange(
      variable
    )
  
  if (has_ref) {
    
    df_out = df_out %>%
      dplyr::mutate(
        test_value = round(test_value, 2),
        ref_value = round(ref_value, 2),
        abs_diff = round(abs_diff, 2),
        rel_diff = round(rel_diff, 1)
      ) %>%
      dplyr::select(
        variable,
        test_value,
        ref_value,
        unit,
        abs_diff,
        rel_diff
      )
    
  } else {
    
    df_out = df_out %>%
      dplyr::mutate(
        test_value = round(test_value, 2)
      ) %>%
      dplyr::select(
        variable,
        test_value,
        unit
      )
  }
  
  # Display column names
  
  display_names = names(df_out)
  
  display_names = dplyr::case_when(
    display_names == 'variable' ~ 'Variable',
    display_names == 'test_value' ~ test_label,
    display_names == 'ref_value' ~ ref_label,
    display_names == 'unit' ~ 'Unit',
    display_names == 'abs_diff' ~ 'Difference',
    display_names == 'rel_diff' ~ 'Relative difference (%)',
    TRUE ~ display_names
  )
  
  # Column alignment
  
  left_align_cols = which(
    names(df_out) == 'variable'
  ) - 1
  
  centre_align_cols = which(
    names(df_out) != 'variable'
  ) - 1
  
  column_defs = list()
  
  if (length(left_align_cols) > 0) {
    column_defs = c(
      column_defs,
      list(
        list(
          className = 'dt-left',
          targets = left_align_cols
        )
      )
    )
  }
  
  if (length(centre_align_cols) > 0) {
    column_defs = c(
      column_defs,
      list(
        list(
          className = 'dt-center',
          targets = centre_align_cols
        )
      )
    )
  }
  
  # Variable autocomplete/filter
  
  var_col_index = match(
    'variable',
    names(df_out)
  ) - 1
  
  var_values = df_out$variable %>%
    as.character() %>%
    unique() %>%
    sort()
  
  datalist_id = paste0(
    'summary-var-options-',
    sample.int(
      1e9,
      1
    )
  )
  
  filter_cells = purrr::map(
    names(df_out),
    function(col_nm) {
      
      if (col_nm == 'variable') {
        
        return(
          htmltools::tags$td(
            class = 'summary-filter-cell',
            htmltools::tags$input(
              type = 'text',
              class = 'summary-var-filter',
              list = datalist_id,
              placeholder = 'All variables',
              autocomplete = 'off'
            ),
            htmltools::tags$datalist(
              id = datalist_id,
              purrr::map(
                var_values,
                function(x) {
                  htmltools::tags$option(
                    value = x
                  )
                }
              )
            )
          )
        )
      }
      
      htmltools::tags$td('')
    }
  )
  
  table_container = htmltools::withTags(
    table(
      class = 'display',
      thead(
        tr(
          purrr::map2(
            display_names,
            names(df_out),
            function(display_name, col_name) {
              
              align_class = if (col_name == 'variable') {
                'dt-left'
              } else {
                'dt-center'
              }
              
              th(
                class = align_class,
                display_name
              )
            }
          )
        ),
        tr(
          class = 'summary-filter-row',
          filter_cells
        )
      )
    )
  )
  
  # Variable filter callback
  
  filter_callback = htmlwidgets::JS(
    paste0(
      '
      var container = $(table.table().container());

      container
        .find(".summary-var-filter")
        .off(".summary")
        .on(
          "click.summary mousedown.summary",
          function(e) {
            e.stopPropagation();
          }
        )
        .on(
          "keyup.summary change.summary search.summary",
          function() {

            var val = this.value;

            if (
              table.column(', var_col_index, ').search() !== val
            ) {

              table
                .column(', var_col_index, ')
                .search(
                  val,
                  false,
                  true
                )
                .draw();
            }
          }
        );
      '
    )
  )
  
  # Base DT options
  
  dt_options = list(
    pageLength = page_length,
    scrollX = FALSE,
    dom = 'rtip',
    autoWidth = TRUE,
    columnDefs = column_defs
  )
  
  # Add exceedance filter when a reference run is available
  
  if (has_ref) {
    
    rel_diff_col_index = match(
      'rel_diff',
      names(df_out)
    ) - 1
    
    dt_options$initComplete = DT::JS(
      sprintf(
        "
        function(settings, json) {

          var api = this.api();
          var tableNode = api.table().node();

          var wrapper = $(api.table().container())
            .closest('.dataTables_wrapper');

          var control = $(
            '<div class=\"emep-exceed-control\">' +
              '<label>' +
                '<input type=\"checkbox\" class=\"emep-exceed-only\"> ' +
                'Show exceedances only' +
              '</label>' +
            '</div>'
          );

          wrapper.prepend(control);

          $.fn.dataTable.ext.search.push(
            function(settings, data, dataIndex) {

              if (settings.nTable !== tableNode) {
                return true;
              }

              var checked = control
                .find('.emep-exceed-only')
                .prop('checked');

              if (!checked) {
                return true;
              }

              var value = parseFloat(
                data[%d]
              );

              return !isNaN(value) &&
                Math.abs(value) > %f;
            }
          );

          control
            .find('.emep-exceed-only')
            .on('change', function() {
              api.draw();
            });
        }
        ",
        rel_diff_col_index,
        threshold
      )
    )
  }
  
  # Create table
  
  dt = DT::datatable(
    df_out,
    rownames = FALSE,
    filter = 'none',
    container = table_container,
    caption = caption,
    escape = FALSE,
    callback = filter_callback,
    options = dt_options,
    class = 'compact stripe hover'
  ) %>%
    DT::formatStyle(
      columns = names(df_out),
      `font-size` = '12px'
    ) %>%
    DT::formatStyle(
      columns = 'variable',
      `text-align` = 'left'
    )
  
  # Highlight relative differences exceeding the threshold
  
  if (has_ref) {
    
    dt = dt %>%
      DT::formatStyle(
        'rel_diff',
        backgroundColor = DT::styleInterval(
          c(
            -threshold,
            threshold
          ),
          c(
            '#f4cccc',
            NA,
            '#f4cccc'
          )
        )
      )
  }
  
  # Common DT styling
  
  dt = htmlwidgets::prependContent(
    dt,
    htmltools::tags$style(
      htmltools::HTML(
        '
        table.dataTable thead th {
          background-color: #eeeeee !important;
          color: #333333 !important;
          font-weight: 600 !important;
          border-bottom: 1px solid #cccccc !important;
        }

        table.dataTable thead tr.summary-filter-row td {
          height: 29px !important;
          padding: 2px 4px !important;
          background-color: #f5f5f5 !important;
        }

        table.dataTable thead tr.summary-filter-row
        td.summary-filter-cell::before,
        table.dataTable thead tr.summary-filter-row
        td.summary-filter-cell::after {
          display: none !important;
        }

        table.dataTable thead tr.summary-filter-row
        td.summary-filter-cell {
          text-align: left !important;
        }

        table.dataTable thead tr.summary-filter-row
        input.summary-var-filter {
          width: 275px !important;
          max-width: 275px !important;
          height: 25px !important;
          box-sizing: border-box !important;
          font-size: 12px !important;
          text-align: left !important;
        }

        table.dataTable {
          width: 100% !important;
        }

        .dataTables_wrapper {
          font-size: 12px;
        }

        .emep-exceed-control {
          display: flex;
          justify-content: flex-end;
          margin-bottom: 6px;
          font-size: 12px;
          font-weight: normal;
        }

        .emep-exceed-control input {
          vertical-align: middle;
          margin-right: 3px;
        }
        '
      )
    )
  )
  
  dt
}

make_emep_summary_gt = function(
    df,
    run_labels,
    threshold = 5,
    caption = NULL
) {
  
  if (is.null(df) || nrow(df) == 0) {
    return(
      gt::gt(
        data.frame(
          Message = 'No EMEP summary results available.'
        )
      )
    )
  }
  
  test_label = run_labels[1]
  ref_label = run_labels[2]
  
  has_ref = all(
    c(
      'ref_value',
      'ref_unit',
      'abs_diff',
      'rel_diff'
    ) %in% names(df)
  )
  
  # Check that test and reference units agree where both are available
  
  if (has_ref) {
    
    mismatched_units = df %>%
      dplyr::filter(
        !is.na(ref_value),
        !is.na(ref_unit),
        test_unit != ref_unit
      )
    
    if (nrow(mismatched_units) > 0) {
      
      stop(
        "Test and reference units do not match for the following variables: ",
        paste(
          mismatched_units$variable,
          collapse = ', '
        )
      )
    }
  }
  
  # Prepare display data
  
  df_out = df %>%
    dplyr::mutate(
      unit = purrr::map_chr(
        test_unit,
        format_units_for_html
      )
    ) %>%
    dplyr::arrange(
      variable
    )
  
  if (has_ref) {
    
    df_out = df_out %>%
      dplyr::mutate(
        test_value = round(test_value, 2),
        ref_value = round(ref_value, 2),
        abs_diff = round(abs_diff, 2),
        rel_diff = round(rel_diff, 1)
      ) %>%
      dplyr::select(
        variable,
        test_value,
        ref_value,
        unit,
        abs_diff,
        rel_diff
      )
    
  } else {
    
    df_out = df_out %>%
      dplyr::mutate(
        test_value = round(test_value, 2)
      ) %>%
      dplyr::select(
        variable,
        test_value,
        unit
      )
  }
  
  # Create table
  
  summary_gt = df_out %>%
    gt::gt(
      caption = caption
    ) %>%
    gt::fmt_markdown(
      columns = unit
    ) %>%
    gt::cols_label(
      variable = 'Variable',
      test_value = test_label,
      unit = 'Unit'
    ) %>%
    gt::cols_align(
      align = 'left',
      columns = variable
    ) %>%
    gt::cols_align(
      align = 'center',
      columns = -variable
    ) %>%
    gt::tab_options(
      table.font.size = gt::px(12),
      column_labels.background.color = '#eeeeee',
      column_labels.font.weight = '600'
    )
  
  if (has_ref) {
    
    summary_gt = summary_gt %>%
      gt::cols_label(
        ref_value = ref_label,
        abs_diff = 'Difference',
        rel_diff = 'Relative difference (%)'
      ) %>%
      gt::tab_style(
        style = gt::cell_fill(
          color = '#f4cccc'
        ),
        locations = gt::cells_body(
          columns = rel_diff,
          rows = !is.na(rel_diff) &
            abs(rel_diff) > threshold
        )
      )
  }
  
  summary_gt
}

make_leaflet_bin_legend_html = function(
    title,
    breaks,
    colours,
    formatter,
    breaks_labs = NULL,
    alpha = 1,
    font_size = 10,
    title_size = 11,
    swatch_width = 13,
    swatch_height = 11,
    line_height = 12
) {
  
  n_bins = length(breaks) - 1
  
  if (n_bins < 1) {
    return(NULL)
  }
  
  if (!is.null(breaks_labs)) {
    
    expected_n_labs = length(breaks) - 2
    
    if (length(breaks_labs) != expected_n_labs) {
      
      stop(
        glue::glue(
          'breaks_labs must have exactly {expected_n_labs} labels ',
          'for {length(breaks)} breaks.'
        )
      )
    }
    
    bin_labs = c(
      paste0(
        '< ',
        breaks_labs[1]
      ),
      purrr::map2_chr(
        breaks_labs[-length(breaks_labs)],
        breaks_labs[-1],
        function(lower, upper) {
          
          paste0(
            lower,
            ' – ',
            upper
          )
        }
      ),
      paste0(
        '≥ ',
        breaks_labs[length(breaks_labs)]
      )
    )
    
  } else {
    
    bin_labs = purrr::map_chr(
      seq_len(n_bins),
      function(i) {
        
        paste0(
          formatter(breaks[i]),
          ' – ',
          formatter(breaks[i + 1])
        )
      }
    )
  }
  
  colours = scales::alpha(
    colours,
    alpha = alpha
  )
  
  bin_ids = rev(
    seq_len(n_bins)
  )
  
  rows = purrr::map_chr(
    bin_ids,
    function(i) {
      
      lab = bin_labs[i]
      
      stringr::str_glue(
        '<div style="display:flex; align-items:center; line-height:{line_height}px; margin:1px 0;">
           <span style="display:inline-block; width:{swatch_width}px; height:{swatch_height}px; margin-right:5px; background:{colours[i]}; border:1px solid #777;"></span>
           <span>{htmltools::htmlEscape(lab)}</span>
         </div>'
      )
    }
  )
  
  htmltools::HTML(
    stringr::str_glue(
      '<div style="background:white; padding:5px 7px; border:1px solid #aaa; border-radius:3px; font-size:{font_size}px; box-shadow:0 1px 5px rgba(0,0,0,0.25);">
         <div style="font-size:{title_size}px; font-weight:600; margin-bottom:4px;">{title}</div>
         {paste(rows, collapse = "\n")}
       </div>'
    )
  )
}

make_map_label_formatter = function(
    var,
    break_key,
    var_params_list,
    default_scale_cut = c(
      0,
      'k' = 1e3,
      'M' = 1e6,
      'G' = 1e9,
      'T' = 1e12
    ),
    default_accuracy = NULL,
    drop0trailing = TRUE
) {
  
  key_parent = head(
    break_key,
    -1
  )
  
  key_name = tail(
    break_key,
    1
  )
  
  scale_cut_key = c(
    key_parent,
    paste0(
      key_name,
      '_scale_cut'
    )
  )
  
  accuracy_key = c(
    key_parent,
    paste0(
      key_name,
      '_accuracy'
    )
  )
  
  scale_cut = get_var_param(
    var = var,
    key = scale_cut_key,
    var_params_list = var_params_list,
    default = default_scale_cut
  )
  
  accuracy = get_var_param(
    var = var,
    key = accuracy_key,
    var_params_list = var_params_list,
    default = default_accuracy
  )
  
  if (identical(scale_cut, FALSE)) {
    scale_cut = NULL
  }
  
  scales::label_number(
    accuracy = accuracy,
    scale_cut = scale_cut,
    drop0trailing = drop0trailing
  )
}

make_MBS_dt = function(
    df,
    run_labels,
    threshold = 5,
    page_length = 25
) {
  
  if (is.null(df) || nrow(df) == 0) {
    return(
      htmltools::tags$p(
        'No mass budget comparison results available.'
      )
    )
  }
  
  # Prepare display data
  
  table_data = df %>%
    dplyr::select(
      species,
      test_mass,
      ref_mass,
      unit,
      abs_diff,
      rel_diff
    )
  
  # Display column names
  
  display_names = names(table_data)
  
  display_names = dplyr::case_when(
    display_names == 'species' ~ 'Species',
    display_names == 'test_mass' ~ run_labels[1],
    display_names == 'ref_mass' ~ run_labels[2],
    display_names == 'unit' ~ 'Unit',
    display_names == 'abs_diff' ~ 'Absolute difference',
    display_names == 'rel_diff' ~ 'Relative difference (%)',
    TRUE ~ display_names
  )
  
  # Column alignment
  
  left_align_cols = which(
    names(table_data) == 'species'
  ) - 1
  
  centre_align_cols = which(
    names(table_data) != 'species'
  ) - 1
  
  column_defs = list()
  
  if (length(left_align_cols) > 0) {
    column_defs = c(
      column_defs,
      list(
        list(
          className = 'dt-left',
          targets = left_align_cols
        )
      )
    )
  }
  
  if (length(centre_align_cols) > 0) {
    column_defs = c(
      column_defs,
      list(
        list(
          className = 'dt-center',
          targets = centre_align_cols
        )
      )
    )
  }
  
  # Species autocomplete/filter
  
  species_col_index = match(
    'species',
    names(table_data)
  ) - 1
  
  species_values = table_data$species %>%
    as.character() %>%
    unique() %>%
    sort()
  
  datalist_id = paste0(
    'mbs-species-options-',
    sample.int(
      1e9,
      1
    )
  )
  
  filter_cells = purrr::map(
    names(table_data),
    function(col_nm) {
      
      if (col_nm == 'species') {
        
        return(
          htmltools::tags$td(
            class = 'mbs-filter-cell',
            htmltools::tags$input(
              type = 'text',
              class = 'mbs-species-filter',
              list = datalist_id,
              placeholder = 'All species',
              autocomplete = 'off'
            ),
            htmltools::tags$datalist(
              id = datalist_id,
              purrr::map(
                species_values,
                function(x) {
                  htmltools::tags$option(
                    value = x
                  )
                }
              )
            )
          )
        )
      }
      
      htmltools::tags$td('')
    }
  )
  
  table_container = htmltools::withTags(
    table(
      class = 'display',
      thead(
        tr(
          purrr::map2(
            display_names,
            names(table_data),
            function(display_name, col_name) {
              
              align_class = if (col_name == 'species') {
                'dt-left'
              } else {
                'dt-center'
              }
              
              th(
                class = align_class,
                display_name
              )
            }
          )
        ),
        tr(
          class = 'mbs-filter-row',
          filter_cells
        )
      )
    )
  )
  
  # Species filter callback
  
  filter_callback = htmlwidgets::JS(
    paste0(
      '
      var container = $(table.table().container());

      container
        .find(".mbs-species-filter")
        .off(".mbs")
        .on(
          "click.mbs mousedown.mbs",
          function(e) {
            e.stopPropagation();
          }
        )
        .on(
          "keyup.mbs change.mbs search.mbs",
          function() {

            var val = this.value;

            if (
              table.column(', species_col_index, ').search() !== val
            ) {

              table
                .column(', species_col_index, ')
                .search(
                  val,
                  false,
                  true
                )
                .draw();
            }
          }
        );
      '
    )
  )
  
  # Base DT options
  
  dt_options = list(
    pageLength = page_length,
    scrollX = FALSE,
    dom = 'rtip',
    autoWidth = TRUE,
    columnDefs = column_defs
  )
  
  # Filter directly on relative difference
  
  rel_diff_col_index = match(
    'rel_diff',
    names(table_data)
  ) - 1
  
  dt_options$initComplete = DT::JS(
    sprintf(
      "
      function(settings, json) {

        var api = this.api();
        var tableNode = api.table().node();

        var wrapper = $(api.table().container())
          .closest('.dataTables_wrapper');

        var control = $(
          '<div class=\"mbs-exceed-control\">' +
            '<label>' +
              '<input type=\"checkbox\" class=\"mbs-exceed-only\"> ' +
              'Show exceedances only' +
            '</label>' +
          '</div>'
        );

        wrapper.prepend(control);

        $.fn.dataTable.ext.search.push(
          function(settings, data, dataIndex) {

            if (settings.nTable !== tableNode) {
              return true;
            }

            var checked = control
              .find('.mbs-exceed-only')
              .prop('checked');

            if (!checked) {
              return true;
            }

            var value = parseFloat(
              data[%d]
            );

            return !isNaN(value) &&
              Math.abs(value) > %f;
          }
        );

        control
          .find('.mbs-exceed-only')
          .on('change', function() {
            api.draw();
          });
      }
      ",
      rel_diff_col_index,
      threshold
    )
  )
  
  # Create table
  
  dt = DT::datatable(
    table_data,
    rownames = FALSE,
    filter = 'none',
    container = table_container,
    escape = FALSE,
    callback = filter_callback,
    options = dt_options,
    class = 'compact stripe hover'
  ) %>%
    DT::formatStyle(
      columns = names(table_data),
      `font-size` = '12px'
    ) %>%
    DT::formatStyle(
      columns = 'species',
      `text-align` = 'left'
    ) %>%
    DT::formatSignif(
      columns = c(
        'test_mass',
        'ref_mass',
        'abs_diff'
      ),
      digits = 5
    ) %>%
    DT::formatRound(
      columns = 'rel_diff',
      digits = 2
    ) %>%
    DT::formatStyle(
      'rel_diff',
      backgroundColor = DT::styleInterval(
        c(
          -threshold,
          threshold
        ),
        c(
          '#f8d7da',
          NA,
          '#f8d7da'
        )
      )
    )
  
  # Common DT styling
  
  dt = htmlwidgets::prependContent(
    dt,
    htmltools::tags$style(
      htmltools::HTML(
        '
        table.dataTable thead th {
          background-color: #eeeeee !important;
          color: #333333 !important;
          font-weight: 600 !important;
          border-bottom: 1px solid #cccccc !important;
        }

        table.dataTable thead tr.mbs-filter-row td {
          height: 29px !important;
          padding: 2px 4px !important;
          background-color: #f5f5f5 !important;
        }

        table.dataTable thead tr.mbs-filter-row
        td.mbs-filter-cell::before,
        table.dataTable thead tr.mbs-filter-row
        td.mbs-filter-cell::after {
          display: none !important;
        }

        table.dataTable thead tr.mbs-filter-row
        td.mbs-filter-cell {
          text-align: left !important;
        }

        table.dataTable thead tr.mbs-filter-row
        input.mbs-species-filter {
          width: 275px !important;
          max-width: 275px !important;
          height: 25px !important;
          box-sizing: border-box !important;
          font-size: 12px !important;
          text-align: left !important;
        }

        table.dataTable {
          width: 100% !important;
        }

        .dataTables_wrapper {
          font-size: 12px;
        }

        .mbs-exceed-control {
          display: flex;
          justify-content: flex-end;
          margin-bottom: 6px;
          font-size: 12px;
          font-weight: normal;
        }

        .mbs-exceed-control input {
          vertical-align: middle;
          margin-right: 3px;
        }
        '
      )
    )
  )
  
  dt
}

make_MBS_gt = function(
    df,
    run_labels,
    threshold = 5
) {
  
  if (is.null(df) || nrow(df) == 0) {
    return(
      gt::gt(
        data.frame(
          Message = 'No mass budget comparison results available.'
        )
      )
    )
  }
  
  # Prepare display data
  
  table_data = df %>%
    dplyr::select(
      species,
      test_mass,
      ref_mass,
      unit,
      abs_diff,
      rel_diff
    ) %>%
    dplyr::mutate(
      unit = purrr::map_chr(
        unit,
        format_units_for_html
      )
    )
  
  # Create table
  
  mbs_gt = table_data %>%
    gt::gt() %>%
    gt::fmt_markdown(
      columns = unit
    ) %>%
    gt::cols_label(
      species = 'Species',
      test_mass = run_labels[1],
      ref_mass = run_labels[2],
      unit = 'Unit',
      abs_diff = 'Absolute difference',
      rel_diff = 'Relative difference (%)'
    ) %>%
    gt::fmt_number(
      columns = c(
        test_mass,
        ref_mass,
        abs_diff
      ),
      n_sigfig = 5
    ) %>%
    gt::fmt_number(
      columns = rel_diff,
      decimals = 2
    ) %>%
    gt::cols_align(
      align = 'left',
      columns = species
    ) %>%
    gt::cols_align(
      align = 'center',
      columns = -species
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(
        color = '#f8d7da'
      ),
      locations = gt::cells_body(
        columns = rel_diff,
        rows = !is.na(rel_diff) &
          abs(rel_diff) > threshold
      )
    ) %>%
    gt::tab_options(
      table.font.size = gt::px(12),
      column_labels.background.color = '#eeeeee',
      column_labels.font.weight = '600'
    )
  
  mbs_gt
}

make_mobs_map_container = function(
    map,
    stat,
    var_nm
) {
  
  htmltools::tags$div(
    id = make_mobs_map_id(
      stat = stat,
      var_nm = var_nm
    ),
    class = paste0(
      'mobs-map-panel mobs-map-var-',
      var_nm %>%
        stringr::str_replace_all(
          '[^A-Za-z0-9_-]',
          '-'
        )
    ),
    map
  )
}

make_mobs_map_id = function(
    stat,
    var_nm
) {
  
  paste0(
    'mobs-map-',
    stat,
    '-',
    var_nm
  ) %>%
    stringr::str_replace_all(
      '[^A-Za-z0-9_-]',
      '-'
    )
}

make_mobs_plotly_xaxis = function(x_resolution = c('summary', 'raw'),
                                  raw_tick_hours = 6) {
  
  x_resolution = rlang::arg_match(x_resolution)
  
  one_hour = 1000 * 60 * 60
  one_day = one_hour * 24
  
  if (x_resolution == 'raw') {
    return(
      list(
        title = '',
        type = 'date',
        tickformatstops = list(
          list(
            dtickrange = list(NULL, one_day * 3),
            value = '%H:%M<br>%d %b'
          ),
          list(
            dtickrange = list(one_day * 3, one_day * 31),
            value = '%d %b'
          ),
          list(
            dtickrange = list(one_day * 31, one_day * 366),
            value = '%b'
          ),
          list(
            dtickrange = list(one_day * 366, NULL),
            value = '%Y'
          )
        )
      )
    )
  }
  
  list(
    title = '',
    type = 'date',
    tickformat = '%b',
    tickformatstops = list(
      list(
        dtickrange = list(NULL, one_day * 3),
        value = '%H:%M<br>%d %b'
      ),
      list(
        dtickrange = list(one_day * 3, one_day * 31),
        value = '%d %b'
      ),
      list(
        dtickrange = list(one_day * 31, one_day * 366),
        value = '%b'
      ),
      list(
        dtickrange = list(one_day * 366, NULL),
        value = '%Y'
      )
    )
  )
}

make_modstats_dt = function(
    df,
    var_params_list,
    var_param_names = NULL,
    caption = NULL,
    page_length = NULL,
    include_run = TRUE,
    grouping_var = NULL,
    grouping_var_label = NULL,
    var_fill = NULL,
    row_fill_alpha = 0.5
) {
  
  if (is.null(df) || nrow(df) == 0) {
    return(
      htmltools::tags$p(
        'No model evaluation statistics available.'
      )
    )
  }
  
  if (is.null(page_length)) {
    
    n_vars = if ('var' %in% names(df)) {
      dplyr::n_distinct(df$var)
    } else {
      1
    }
    
    page_length = get_modstats_page_length(
      n_vars = n_vars,
      target = 20
    )
  }
  
  drop_cols = c(
    'p',
    'P',
    'site',
    'station'
  )
  
  if (!isTRUE(include_run)) {
    drop_cols = c(
      drop_cols,
      'run'
    )
  }
  
  front_cols = c(
    'run',
    'code',
    if (!is.null(grouping_var)) {
      grouping_var
    } else {
      character()
    },
    'var',
    'n',
    'FAC2',
    'MB',
    'NMB',
    'RMSE',
    'r_pearson',
    'r_spearman'
  )
  
  sort_cols = c(
    if (!is.null(grouping_var)) {
      grouping_var
    } else {
      character()
    },
    'code',
    'site',
    'station',
    'var'
  )
  
  df_out = df %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -dplyr::any_of(drop_cols)
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(
          c(
            'FAC2',
            'NMB',
            'r_pearson',
            'r_spearman'
          )
        ),
        ~ round(.x, 2)
      ),
      dplyr::across(
        dplyr::any_of(
          c(
            'MB',
            'RMSE'
          )
        ),
        ~ round(.x, 1)
      )
    ) %>%
    dplyr::relocate(
      dplyr::any_of(front_cols)
    ) %>%
    dplyr::arrange(
      dplyr::across(
        dplyr::any_of(sort_cols)
      )
    )
  
  if ('var' %in% names(df_out)) {
    
    df_out = df_out %>%
      dplyr::mutate(
        var_raw = var,
        var_param_nm = if (is.null(var_param_names)) {
          var_raw
        } else {
          dplyr::coalesce(
            unname(
              var_param_names[var_raw]
            ),
            var_raw
          )
        },
        var_id = purrr::map_chr(
          var_param_nm,
          resolve_mobs_var_id,
          var_params_list = var_params_list
        ),
        var = purrr::map_chr(
          var_param_nm,
          format_var_label_emep,
          var_params_list = var_params_list,
          context = 'mobs',
          output = 'html',
          include_units = FALSE
        )
      ) %>%
      dplyr::select(
        -var_param_nm
      )
  }
  
  display_names = names(df_out)
  
  display_names = dplyr::case_when(
    display_names == 'run' ~ 'Run',
    display_names == 'code' ~ 'Site Code',
    display_names == 'var' ~ 'Variable',
    display_names == 'var_raw' ~ 'Variable raw',
    display_names == 'var_id' ~ 'Variable ID',
    display_names == 'NMB' ~ 'NMB (%)',
    display_names == 'r_pearson' ~ 'Pearson r',
    display_names == 'r_spearman' ~ 'Spearman ρ',
    display_names == 'n' ~ 'n',
    TRUE ~ display_names
  )
  
  if (
    !is.null(grouping_var) &&
    length(grouping_var) == 1 &&
    grouping_var %in% names(df_out)
  ) {
    
    this_grouping_label = if (!is.null(grouping_var_label)) {
      grouping_var_label
    } else {
      stringr::str_to_sentence(
        grouping_var
      )
    }
    
    display_names[
      names(df_out) == grouping_var
    ] = this_grouping_label
  }
  
  hidden_cols = which(
    names(df_out) %in%
      c(
        'var_raw',
        'var_id'
      )
  ) - 1
  
  left_align_cols = which(
    names(df_out) %in%
      c(
        'run',
        'code',
        grouping_var,
        'var'
      )
  ) - 1
  
  right_align_cols = which(
    !names(df_out) %in%
      c(
        'run',
        'code',
        grouping_var,
        'var',
        'var_raw',
        'var_id'
      )
  ) - 1
  
  column_defs = list()
  
  if (length(left_align_cols) > 0) {
    column_defs = c(
      column_defs,
      list(
        list(
          className = 'dt-left',
          targets = left_align_cols
        )
      )
    )
  }
  
  if (length(right_align_cols) > 0) {
    column_defs = c(
      column_defs,
      list(
        list(
          className = 'dt-right',
          targets = right_align_cols
        )
      )
    )
  }
  
  if (length(hidden_cols) > 0) {
    
    column_defs = c(
      column_defs,
      list(
        list(
          visible = FALSE,
          targets = hidden_cols
        )
      )
    )
  }
  
  code_col_index = if ('code' %in% names(df_out)) {
    match(
      'code',
      names(df_out)
    ) - 1
  } else {
    NULL
  }
  
  if (!is.null(code_col_index)) {
    column_defs = c(
      column_defs,
      list(
        list(
          width = '110px',
          targets = code_col_index
        )
      )
    )
  }
  
  var_col_index = if ('var' %in% names(df_out)) {
    match(
      'var',
      names(df_out)
    ) - 1
  } else {
    NULL
  }
  
  var_raw_col_index = if ('var_raw' %in% names(df_out)) {
    match(
      'var_raw',
      names(df_out)
    ) - 1
  } else {
    NULL
  }
  
  grouping_col_index = if (
    !is.null(grouping_var) &&
    grouping_var %in% names(df_out)
  ) {
    match(
      grouping_var,
      names(df_out)
    ) - 1
  } else {
    NULL
  }
  
  # Native HTML select elements cannot render HTML subscripts,
  # so convert the formatted variable labels to Unicode subscripts.
  var_options = NULL
  
  if (
    'var' %in% names(df_out) &&
    'var_raw' %in% names(df_out)
  ) {
    
    var_options = df_out %>%
      dplyr::select(
        var_raw,
        var_id
      ) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        var_select_label = purrr::map_chr(
          var_id,
          format_var_label_emep,
          var_params_list = var_params_list,
          context = 'mobs',
          output = 'plain',
          include_units = FALSE
        )
      )
  }
  
  grouping_values = NULL
  
  if (!is.null(grouping_col_index)) {
    
    grouping_values = df_out[[grouping_var]] %>%
      as.character() %>%
      unique()
    
    grouping_values = grouping_values[
      !is.na(grouping_values)
    ]
    
    grouping_values = sort(
      grouping_values
    )
  }
  
  filter_cells = purrr::map(
    names(df_out),
    function(col_nm) {
      
      if (col_nm == 'code') {
        
        return(
          htmltools::tags$td(
            class = 'modstats-filter-cell',
            htmltools::tags$input(
              type = 'text',
              class = 'modstats-code-filter',
              placeholder = 'Search'
            )
          )
        )
      }
      
      if (
        !is.null(grouping_var) &&
        col_nm == grouping_var
      ) {
        
        return(
          htmltools::tags$td(
            class = 'modstats-filter-cell',
            htmltools::tags$select(
              class = 'modstats-grouping-filter',
              htmltools::tags$option(
                value = '',
                'All'
              ),
              purrr::map(
                grouping_values,
                function(x) {
                  htmltools::tags$option(
                    value = x,
                    x
                  )
                }
              )
            )
          )
        )
      }
      
      if (
        col_nm == 'var' &&
        !is.null(var_options)
      ) {
        
        return(
          htmltools::tags$td(
            class = 'modstats-filter-cell',
            htmltools::tags$select(
              class = 'modstats-var-filter',
              htmltools::tags$option(
                value = '',
                'All'
              ),
              purrr::map2(
                var_options$var_raw,
                var_options$var_select_label,
                function(var_raw, var_label) {
                  htmltools::tags$option(
                    value = var_raw,
                    var_label
                  )
                }
              )
            )
          )
        )
      }
      
      htmltools::tags$td('')
    }
  )
  
  table_container = htmltools::withTags(
    table(
      class = 'display',
      thead(
        tr(
          purrr::map2(
            display_names,
            names(df_out),
            function(display_name, col_name) {
              
              align_class = if (
                col_name %in%
                c(
                  'run',
                  'code',
                  grouping_var,
                  'var'
                )
              ) {
                'dt-left'
              } else {
                'dt-right'
              }
              
              th(
                class = align_class,
                display_name
              )
            }
          )
        ),
        tr(
          class = 'modstats-filter-row',
          filter_cells
        )
      )
    )
  )
  
  callback_parts = c(
    '
    var container = $(table.table().container());
    '
  )
  
  if (!is.null(code_col_index)) {
    
    callback_parts = c(
      callback_parts,
      paste0(
        '
        container
          .find(".modstats-code-filter")
          .off(".modstats")
          .on("click.modstats mousedown.modstats", function(e) {
            e.stopPropagation();
          })
          .on("keyup.modstats change.modstats clear.modstats", function() {

            var val = this.value;

            if (
              table.column(', code_col_index, ').search() !== val
            ) {

              table
                .column(', code_col_index, ')
                .search(val)
                .draw();
            }
          });
        '
      )
    )
  }
  
  if (!is.null(grouping_col_index)) {
    
    callback_parts = c(
      callback_parts,
      paste0(
        '
        container
          .find(".modstats-grouping-filter")
          .off(".modstats")
          .on("click.modstats mousedown.modstats", function(e) {
            e.stopPropagation();
          })
          .on("change.modstats", function() {

            var val = $.fn.dataTable.util.escapeRegex(
              $(this).val()
            );

            table
              .column(', grouping_col_index, ')
              .search(
                val ? "^" + val + "$" : "",
                true,
                false
              )
              .draw();
          });
        '
      )
    )
  }
  
  if (
    !is.null(var_col_index) &&
    !is.null(var_raw_col_index)
  ) {
    
    callback_parts = c(
      callback_parts,
      paste0(
        '
        container
          .find(".modstats-var-filter")
          .off(".modstats")
          .on("click.modstats mousedown.modstats", function(e) {
            e.stopPropagation();
          })
          .on("change.modstats", function() {

            var val = $.fn.dataTable.util.escapeRegex(
              $(this).val()
            );

            table
              .column(', var_raw_col_index, ')
              .search(
                val ? "^" + val + "$" : "",
                true,
                false
              )
              .draw();
          });
        '
      )
    )
  }
  
  filter_callback = htmlwidgets::JS(
    stringr::str_c(
      callback_parts,
      collapse = '\n'
    )
  )
  
  dt = DT::datatable(
    df_out,
    rownames = FALSE,
    filter = 'none',
    container = table_container,
    caption = caption,
    escape = FALSE,
    callback = filter_callback,
    options = list(
      pageLength = page_length,
      scrollX = FALSE,
      dom = 'rtip',
      autoWidth = TRUE,
      columnDefs = column_defs
    ),
    class = 'compact stripe hover'
  ) %>%
    DT::formatStyle(
      columns = names(df_out),
      `font-size` = '12px'
    )
  
  dt = htmlwidgets::prependContent(
    dt,
    htmltools::tags$style(
      htmltools::HTML(
        '
        table.dataTable thead th {
          background-color: #eeeeee !important;
          color: #333333 !important;
          font-weight: 600 !important;
          border-bottom: 1px solid #cccccc !important;
        }

        table.dataTable thead tr.modstats-filter-row td {
          height: 29px !important;
          padding: 2px 4px !important;
          background-color: #f5f5f5 !important;
        }

        table.dataTable thead tr.modstats-filter-row
        td.modstats-filter-cell::before,
        table.dataTable thead tr.modstats-filter-row
        td.modstats-filter-cell::after {
          display: none !important;
        }

        table.dataTable thead tr.modstats-filter-row input,
        table.dataTable thead tr.modstats-filter-row select {
          width: 100% !important;
          height: 25px !important;
          box-sizing: border-box !important;
          font-size: 12px !important;
        }
        
        table.dataTable thead tr.modstats-filter-row
        td.modstats-filter-cell {
          text-align: left !important;
        }
        
        table.dataTable thead tr.modstats-filter-row input,
        table.dataTable thead tr.modstats-filter-row select {
          text-align: left !important;
        }

        table.dataTable {
          width: 100% !important;
        }

        .dataTables_wrapper {
          font-size: 12px;
        }
        '
      )
    )
  )
  
  if (
    !is.null(var_fill) &&
    'var_raw' %in% names(df_out)
  ) {
    
    fill_vars = unique(
      df_out$var_raw
    )
    
    if (identical(var_fill, 'params_file')) {
      
      fill_values = purrr::map_chr(
        fill_vars,
        function(var_nm) {
          
          var_param_nm = if (is.null(var_param_names)) {
            
            var_nm
            
          } else {
            
            mapped_var = unname(
              var_param_names[var_nm]
            )
            
            if (is.na(mapped_var)) {
              var_nm
            } else {
              mapped_var
            }
          }
          
          get_mobs_plot_aesthetics(
            var_nm = var_param_nm,
            var_params_list = var_params_list
          )$obs_fill
        }
      )
      
    } else {
      
      fill_values = rep(
        var_fill,
        length(fill_vars)
      )
    }
    
    names(fill_values) = fill_vars
    
    use_fill = !is.na(fill_values) &
      nzchar(fill_values)
    
    fill_vars = fill_vars[
      use_fill
    ]
    
    fill_values = fill_values[
      use_fill
    ]
    
    if (length(fill_vars) > 0) {
      
      dt = dt %>%
        DT::formatStyle(
          columns = 'var_raw',
          target = 'row',
          backgroundColor = DT::styleEqual(
            levels = fill_vars,
            values = scales::alpha(
              unname(fill_values),
              row_fill_alpha
            )
          )
        )
    }
  }
  
  dt = dt %>%
    DT::formatRound(
      columns = intersect(
        c(
          'FAC2',
          'r_pearson',
          'r_spearman'
        ),
        names(df_out)
      ),
      digits = 2
    ) %>%
    DT::formatRound(
      columns = intersect(
        c(
          'MB',
          'NMB',
          'RMSE'
        ),
        names(df_out)
      ),
      digits = 1
    ) %>%
    DT::formatRound(
      columns = intersect(
        c(
          'p',
          'P'
        ),
        names(df_out)
      ),
      digits = 4
    )
  
  dt
}

make_modstats_gt = function(
    df,
    var_params_list,
    var_param_names = NULL,
    caption = NULL,
    include_run = TRUE,
    grouping_var = NULL,
    grouping_var_label = NULL,
    var_fill = NULL,
    row_fill_alpha = 0.5
) {
  
  if (is.null(df) || nrow(df) == 0) {
    
    return(
      htmltools::tags$p(
        'No model evaluation statistics available.'
      )
    )
  }
  
  drop_cols = c(
    'p',
    'P',
    'site',
    'station'
  )
  
  if (!isTRUE(include_run)) {
    
    drop_cols = c(
      drop_cols,
      'run'
    )
  }
  
  front_cols = c(
    'run',
    'code',
    if (!is.null(grouping_var)) {
      grouping_var
    } else {
      character()
    },
    'var',
    'n',
    'FAC2',
    'MB',
    'NMB',
    'RMSE',
    'r_pearson',
    'r_spearman'
  )
  
  sort_cols = c(
    if (!is.null(grouping_var)) {
      grouping_var
    } else {
      character()
    },
    'code',
    'site',
    'station',
    'var'
  )
  
  df_out = df %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -dplyr::any_of(drop_cols)
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(
          c(
            'FAC2',
            'NMB',
            'r_pearson',
            'r_spearman'
          )
        ),
        ~ round(.x, 2)
      ),
      dplyr::across(
        dplyr::any_of(
          c(
            'MB',
            'RMSE'
          )
        ),
        ~ round(.x, 1)
      )
    ) %>%
    dplyr::relocate(
      dplyr::any_of(front_cols)
    ) %>%
    dplyr::arrange(
      dplyr::across(
        dplyr::any_of(sort_cols)
      )
    )
  
  if ('var' %in% names(df_out)) {
    
    df_out = df_out %>%
      dplyr::mutate(
        var_raw = var,
        var_param_nm = if (is.null(var_param_names)) {
          var_raw
        } else {
          dplyr::coalesce(
            unname(
              var_param_names[var_raw]
            ),
            var_raw
          )
        },
        var_id = purrr::map_chr(
          var_param_nm,
          resolve_mobs_var_id,
          var_params_list = var_params_list
        ),
        var = purrr::map_chr(
          var_param_nm,
          format_var_label_emep,
          var_params_list = var_params_list,
          context = 'mobs',
          output = 'html',
          include_units = FALSE
        )
      ) %>%
      dplyr::select(
        -var_param_nm
      )
  }
  
  display_names = names(df_out)
  
  display_names = dplyr::case_when(
    display_names == 'run' ~ 'Run',
    display_names == 'code' ~ 'Site Code',
    display_names == 'var' ~ 'Variable',
    display_names == 'var_raw' ~ 'Variable raw',
    display_names == 'var_id' ~ 'Variable ID',
    display_names == 'NMB' ~ 'NMB (%)',
    display_names == 'r_pearson' ~ 'Pearson r',
    display_names == 'r_spearman' ~ 'Spearman ρ',
    display_names == 'n' ~ 'n',
    TRUE ~ display_names
  )
  
  if (
    !is.null(grouping_var) &&
    length(grouping_var) == 1 &&
    grouping_var %in% names(df_out)
  ) {
    
    this_grouping_label = if (!is.null(grouping_var_label)) {
      
      grouping_var_label
      
    } else {
      
      stringr::str_to_sentence(
        grouping_var
      )
    }
    
    display_names[
      names(df_out) == grouping_var
    ] = this_grouping_label
  }
  
  gt_tbl = df_out %>%
    gt::gt() %>%
    gt::cols_label(
      .list = stats::setNames(
        display_names,
        names(df_out)
      )
    ) %>%
    gt::fmt_markdown(
      columns = dplyr::any_of(
        'var'
      )
    ) %>%
    gt::cols_hide(
      columns = dplyr::any_of(
        c(
          'var_raw',
          'var_id'
        )
      )
    ) %>%
    gt::cols_align(
      align = 'left',
      columns = dplyr::any_of(
        c(
          'run',
          'code',
          grouping_var,
          'var'
        )
      )
    ) %>%
    gt::cols_align(
      align = 'right',
      columns = dplyr::where(is.numeric)
    ) %>%
    gt::fmt_number(
      columns = dplyr::any_of(
        c(
          'FAC2',
          'r_pearson',
          'r_spearman'
        )
      ),
      decimals = 2
    ) %>%
    gt::fmt_number(
      columns = dplyr::any_of(
        c(
          'MB',
          'NMB',
          'RMSE'
        )
      ),
      decimals = 1
    ) %>%
    gt::sub_missing(
      columns = dplyr::everything(),
      missing_text = ''
    ) %>%
    gt::tab_options(
      data_row.padding = gt::px(3),
      table.align = 'left'
    )
  
  if ('var' %in% names(df_out)) {
    
    gt_tbl = gt_tbl %>%
      gt::text_transform(
        locations = gt::cells_body(
          columns = var
        ),
        fn = function(x) {
          purrr::map_chr(
            x,
            ~ as.character(
              htmltools::HTML(.x)
            )
          )
        }
      )
  }
  
  if (!is.null(caption)) {
    
    gt_tbl = gt_tbl %>%
      gt::tab_caption(
        caption
      )
  }
  
  if (
    !is.null(var_fill) &&
    'var_raw' %in% names(df_out)
  ) {
    
    fill_vars = unique(
      df_out$var_raw
    )
    
    if (identical(var_fill, 'params_file')) {
      
      fill_values = purrr::map_chr(
        fill_vars,
        function(var_nm) {
          
          var_param_nm = if (is.null(var_param_names)) {
            
            var_nm
            
          } else {
            
            mapped_var = unname(
              var_param_names[var_nm]
            )
            
            if (is.na(mapped_var)) {
              var_nm
            } else {
              mapped_var
            }
          }
          
          get_mobs_plot_aesthetics(
            var_nm = var_param_nm,
            var_params_list = var_params_list
          )$obs_fill
        }
      )
      
    } else {
      
      fill_values = rep(
        var_fill,
        length(fill_vars)
      )
    }
    
    names(fill_values) = fill_vars
    
    use_fill =
      !is.na(fill_values) &
      nzchar(fill_values)
    
    fill_vars = fill_vars[
      use_fill
    ]
    
    fill_values = fill_values[
      use_fill
    ]
    
    if (length(fill_vars) > 0) {
      
      purrr::walk2(
        fill_vars,
        fill_values,
        function(var_nm, fill_colour) {
          
          gt_tbl <<- gt_tbl %>%
            gt::tab_style(
              style = gt::cell_fill(
                color = scales::alpha(
                  fill_colour,
                  row_fill_alpha
                )
              ),
              locations = gt::cells_body(
                rows = var_raw == var_nm
              )
            )
        }
      )
    }
  }
  
  gt_tbl
}

make_obs_filename = function(
    code,
    site_name,
    year
) {
  
  site_name_file = sanitise_string(
    site_name,
    case = 'title',
    separator = '_'
  )
  
  paste0(
    code,
    '_',
    site_name_file,
    '_',
    year,
    '_processed.rds'
  )
}

make_site_popup_content = function(
    df,
    group_column = NULL,
    grouping_var_label = NULL,
    missing_label = 'Not available'
) {
  
  popup_value = function(x) {
    x = as.character(x)
    x[is.na(x) | x == ''] = missing_label
    x
  }
  
  elev_col = intersect(
    names(df),
    c(
      'elev(m)',
      'elev_m',
      'elevation_m',
      'elev',
      'elevation'
    )
  )[1]
  
  info_vars = c(
    'code',
    'station',
    'site',
    if (
      !is.null(group_column) &&
      'leaflet_group' %in% names(df)
    ) {
      'leaflet_group'
    } else {
      NULL
    },
    'network',
    elev_col,
    'year'
  )
  
  info_vars = info_vars %>%
    purrr::discard(is.na) %>%
    intersect(names(df)) %>%
    unique()
  
  info_labels = stringr::str_to_sentence(
    info_vars
  )
  
  names(info_labels) = info_vars
  
  if (
    !is.null(group_column) &&
    'leaflet_group' %in% names(info_labels)
  ) {
    
    info_labels[['leaflet_group']] = if (
      !is.null(grouping_var_label) &&
      grouping_var_label != ''
    ) {
      
      grouping_var_label
      
    } else {
      
      stringr::str_to_sentence(
        stringr::str_replace_all(
          group_column,
          '_',
          ' '
        )
      )
    }
  }
  
  if (
    !is.na(elev_col) &&
    elev_col %in% names(info_labels)
  ) {
    info_labels[[elev_col]] = 'Elevation'
  }
  
  purrr::map2(
    info_labels,
    names(info_labels),
    ~ paste0(
      '<b>',
      .x,
      ':</b> ',
      popup_value(df[[.y]])
    )
  ) %>%
    purrr::transpose() %>%
    stringi::stri_join_list(
      sep = '<br/>'
    )
}

make_var_lookup_table = function(vars,
                                 var_params_list,
                                 include_variable_id = FALSE) {
  
  vars = unique(vars)
  
  out = tibble::tibble(
    variable = vars,
    Variable = purrr::map_chr(
      vars,
      format_var_label,
      var_params_list = var_params_list,
      label_type = 'verbose',
      output = 'html',
      include_units = FALSE
    ),
    Unit = purrr::map_chr(
      vars,
      ~ format_units_for_html(
        get_var_param(
          var = .x,
          key = 'units',
          var_params_list = var_params_list,
          default = ''
        )
      )
    ),
    Label = purrr::map_chr(
      vars,
      format_var_label,
      var_params_list = var_params_list,
      label_type = 'short',
      output = 'html',
      include_units = TRUE
    )
  )
  
  if (!isTRUE(include_variable_id)) {
    out = out %>%
      dplyr::select(-variable)
  }
  
  out
}

make_wd_yaxis = function() {
  
  list(
    title = '',
    range = c(-180, 180),
    rangemode = 'normal',
    tickmode = 'array',
    tickvals = c(-180, -90, 0, 90, 180),
    ticktext = c('-180', '-90', '0', '90', '180')
  )
}

make_year_dirs = function(yr) {
  
  fs::path(
    base_obs_dir,
    yr,
    'AURN',
    'Data'
  ) %>%
    fs::dir_create(
      recurse = TRUE
    )
}

match_emep_area_dims = function(area_stars, emep_stars) {
  
  emep_dims = names(
    stars::st_dimensions(emep_stars)
  )
  
  area_dims = names(
    stars::st_dimensions(area_stars)
  )
  
  if ('time' %in% emep_dims && !'time' %in% area_dims) {
    
    times = stars::st_get_dimension_values(
      emep_stars,
      'time'
    )
    
    area_vals = replicate(
      length(times),
      area_stars[['Area_Grid_km2']],
      simplify = 'array'
    )
    
    names(dim(area_vals)) = c(
      'x',
      'y',
      'time'
    )
    
    # replicate() drops the units, so restore the Area_Grid_km2 units
    area_vals = units::set_units(
      area_vals,
      'km^2',
      mode = 'standard'
    )
    
    area_stars = stars::st_as_stars(
      list(
        Area_Grid_km2 = area_vals
      ),
      dimensions = stars::st_dimensions(emep_stars)[
        c('x', 'y', 'time')
      ]
    )
  }
  
  area_stars
}

mobs_dframe_to_long = function(
    mobs_dframe,
    time_cols
) {
  
  mobs_dframe %>%
    tidyr::pivot_longer(
      cols = -dplyr::all_of(
        c(
          time_cols,
          'code'
        )
      ),
      names_to = 'var',
      values_to = 'value'
    ) %>%
    tidyr::separate(
      var,
      into = c(
        'scenario',
        'var'
      ),
      sep = '_',
      extra = 'merge',
      fill = 'right'
    ) %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(
        c(
          time_cols,
          'code',
          'var'
        )
      ),
      names_from = scenario,
      values_from = value
    ) %>%
    dplyr::arrange(
      code,
      dplyr::across(
        dplyr::all_of(time_cols)
      ),
      var
    )
}

mobs_map_stat_is_enabled = function(
    var_nm,
    stat,
    include_by_var = NULL,
    exclude_by_var = NULL
) {
  
  if (
    !is.null(include_by_var) &&
    var_nm %in% names(include_by_var)
  ) {
    
    include_stats = include_by_var[[var_nm]]
    
    if (
      !is.null(include_stats) &&
      !stat %in% include_stats
    ) {
      return(FALSE)
    }
  }
  
  if (
    !is.null(exclude_by_var) &&
    var_nm %in% names(exclude_by_var)
  ) {
    
    exclude_stats = exclude_by_var[[var_nm]]
    
    if (
      !is.null(exclude_stats) &&
      stat %in% exclude_stats
    ) {
      return(FALSE)
    }
  }
  
  TRUE
}

mobs_to_long = function(mobs) {
  
  # Legacy MOBS files are plain data frames and are always regular.
  if (inherits(mobs, 'data.frame')) {
    
    return(
      list(
        regular = mobs_dframe_to_long(
          mobs,
          time_cols = 'date'
        ),
        interval = NULL
      )
    )
  }
  
  if (!is.list(mobs)) {
    stop(
      'MOBS data must be a data frame or a regular/interval list.',
      call. = FALSE
    )
  }
  
  regular = if (!is.null(mobs$regular)) {
    
    mobs_dframe_to_long(
      mobs$regular,
      time_cols = 'date'
    )
    
  } else {
    
    NULL
  }
  
  interval = if (!is.null(mobs$interval)) {
    
    mobs_dframe_to_long(
      mobs$interval,
      time_cols = c(
        'start_date',
        'end_date'
      )
    )
    
  } else {
    
    NULL
  }
  
  list(
    regular = regular,
    interval = interval
  )
}

mobs_tseries_to_pdf = function(
    mobs_tbl,
    var_params_list,
    var_param_names = NULL,
    var_order = NULL,
    out_dir = getwd(),
    fname_out = NULL,
    run_title_info = NULL,
    summary_time = 'day',
    summary_stat = 'mean',
    obs_style = c('ribbon', 'line'),
    obs_alpha = 0.9,
    ref_colour = 'grey25',
    ref_linetype = 'solid',
    ref_linewidth = 0.7,
    ppp = 4,
    plot_all_vars = TRUE,
    legend_labels = c(
      mod = 'Test',
      ref_mod = 'Reference',
      obs = 'Observed'
    )
) {
  
  # Plots time series from a MOBS dataframe for one site and saves the plots
  # in a PDF.
  #
  # mobs_tbl should be the nested output from format_mobs_to_plot(), with:
  #
  # month == -1 for the summary period
  #
  # month >= 1 for consecutive months in the input period
  #
  # var_param_names optionally maps MOBS variable names to the corresponding
  # variable names in var_params_list.
  #
  # fname_out, if not given, is constructed from site name, station name,
  # code and year/period.
  #
  # run_title_info is a string added to PDF page titles.
  #
  # obs_style controls whether observations are plotted as a ribbon or line.
  #
  # ppp = plots per page.
  #
  # plot_all_vars = TRUE plots all modelled variables.
  #
  # plot_all_vars = FALSE plots only variables for which observations are
  # available somewhere during the period represented by mobs_tbl.
  
  obs_style = match.arg(
    obs_style
  )
  
  
  format_run_title_info = function(
    run_title_info,
    wrap_width = 80
  ) {
    
    if (
      is.null(run_title_info) ||
      length(run_title_info) == 0 ||
      is.na(run_title_info) ||
      run_title_info == ''
    ) {
      return(NULL)
    }
    
    # If the title already contains a newline, preserve the first line
    # and wrap only the remaining text.
    
    title_parts = stringr::str_split(
      run_title_info,
      '\n',
      n = 2
    )[[1]]
    
    if (length(title_parts) == 1) {
      
      return(
        stringr::str_wrap(
          run_title_info,
          width = wrap_width
        )
      )
    }
    
    title_label = title_parts[[1]]
    title_body = title_parts[[2]]
    
    stringr::str_c(
      title_label,
      '\n',
      stringr::str_wrap(
        title_body,
        width = wrap_width
      )
    )
  }
  
  
  title_exclude_cols = c(
    'data',
    'month',
    'month_start',
    'timezone'
  )
  
  info_tbl = mobs_tbl %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -dplyr::any_of(
        title_exclude_cols
      )
    ) %>%
    dplyr::distinct()
  
  if ('geometry' %in% names(info_tbl)) {
    
    info_tbl = info_tbl %>%
      sf::st_as_sf() %>%
      dplyr::mutate(
        longitude = stringr::str_c(
          'lon: ',
          round(
            sf::st_coordinates(.)[, 1],
            4
          )
        ),
        latitude = stringr::str_c(
          'lat: ',
          round(
            sf::st_coordinates(.)[, 2],
            4
          )
        )
      ) %>%
      sf::st_drop_geometry()
  }
  
  
  mobs_dates = mobs_tbl %>%
    tidyr::unnest(
      cols = 'data'
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      !is.na(.data$date)
    ) %>%
    dplyr::pull(
      .data$date
    )
  
  mobs_years = lubridate::year(
    mobs_dates
  ) %>%
    unique() %>%
    sort()
  
  mobs_year_lab = if (length(mobs_years) == 0) {
    
    NA_character_
    
  } else if (length(mobs_years) == 1) {
    
    as.character(
      mobs_years
    )
    
  } else {
    
    paste0(
      min(mobs_years),
      '-',
      max(mobs_years)
    )
  }
  
  if (!'year' %in% names(info_tbl)) {
    
    info_tbl = info_tbl %>%
      dplyr::mutate(
        year = mobs_year_lab
      )
  }
  
  
  # Determine which variables should be included in the PDF.
  #
  # subprecip is not plotted separately. It is the precip series with
  # modelled values retained only where precipitation observations exist,
  # and is therefore included in the precip plot.
  
  plot_vars_tbl = mobs_tbl %>%
    tidyr::unnest(
      cols = 'data'
    ) %>%
    dplyr::mutate(
      plot_var = dplyr::if_else(
        var == 'subprecip',
        'precip',
        var
      )
    )
  
  if (isFALSE(plot_all_vars)) {
    
    # Keep only variables for which observations are available somewhere
    # during the period represented by this site's MOBS data.
    
    plot_vars_tbl = plot_vars_tbl %>%
      dplyr::group_by(
        plot_var
      ) %>%
      dplyr::filter(
        any(
          is.finite(obs)
        )
      ) %>%
      dplyr::ungroup()
  }
  
  plot_vars = plot_vars_tbl %>%
    dplyr::distinct(
      plot_var
    ) %>%
    dplyr::pull(
      plot_var
    )
  
  if (!is.null(var_order)) {
    
    plot_vars = c(
      var_order[
        var_order %in% plot_vars
      ],
      plot_vars[
        !plot_vars %in% var_order
      ]
    )
    
  } else {
    
    plot_vars = plot_vars[
      order(
        get_var_order_index(
          plot_vars,
          var_params_list
        ),
        plot_vars
      )
    ]
  }
  
  if (length(plot_vars) == 0) {
    
    logger::log_warn(
      glue::glue(
        "No variables available for MOBS time-series PDF for site '{info_tbl[['code']][[1]]}'."
      )
    )
    
    return(
      invisible(NULL)
    )
  }
  
  
  # Create one plot for each variable within each summary/monthly period.
  
  mobs_tbl = mobs_tbl %>%
    dplyr::mutate(
      var = purrr::map(
        data,
        ~ plot_vars
      )
    ) %>%
    tidyr::unnest(
      cols = var
    ) %>%
    dplyr::mutate(
      plots = purrr::map2(
        data,
        var,
        ~ plot_mobs_tseries(
          dframe = .x,
          var_nm = .y,
          var_param_nm = if (
            !is.null(var_param_names) &&
            .y %in% names(var_param_names)
          ) {
            var_param_names[[.y]]
          } else {
            .y
          },
          var_params_list = var_params_list,
          obs_style = obs_style,
          obs_alpha = obs_alpha,
          ref_colour = ref_colour,
          ref_linetype = ref_linetype,
          ref_linewidth = ref_linewidth,
          legend_labels = legend_labels
        )
      ),
      var_order = match(
        var,
        plot_vars
      )
    ) %>%
    dplyr::arrange(
      month,
      var_order
    ) %>%
    dplyr::select(
      -var_order
    ) %>%
    dplyr::ungroup()
  
  
  # Construct output filename.
  
  if (is.null(fname_out)) {
    
    site_part = if ('site' %in% names(info_tbl)) {
      
      textclean::replace_non_ascii(
        info_tbl[['site']]
      )
      
    } else {
      
      ''
    }
    
    station_part = if ('station' %in% names(info_tbl)) {
      
      textclean::replace_non_ascii(
        info_tbl[['station']]
      )
      
    } else {
      
      ''
    }
    
    fname_out = stringr::str_c(
      c(
        stringr::str_replace_all(
          site_part,
          '[^[:alnum:]]',
          ''
        ),
        stringr::str_replace_all(
          station_part,
          '[^[:alnum:]]',
          ''
        ),
        info_tbl[['code']],
        info_tbl[['year']]
      ),
      collapse = ' '
    ) %>%
      stringr::str_squish() %>%
      stringr::str_replace_all(
        ' ',
        '_'
      )
    
    if (isFALSE(plot_all_vars)) {
      
      fname_out = paste0(
        fname_out,
        '_p'
      )
    }
  }
  
  plot_pth_out = fs::path(
    out_dir,
    fname_out,
    ext = 'pdf'
  )
  
  
  # Construct site information for page titles.
  
  extra_info_tbl = info_tbl %>%
    dplyr::select(
      -dplyr::any_of(
        c(
          'code',
          'site',
          'station',
          'year'
        )
      )
    ) %>%
    purrr::discard(
      ~ all(
        is.na(.x)
      )
    )
  
  if (length(extra_info_tbl) > 0) {
    
    extra_info = extra_info_tbl %>%
      tidyr::unite(
        col = 'dummy',
        sep = ', '
      ) %>%
      dplyr::pull(
        .data$dummy
      ) %>%
      na.omit()
    
    extra_info = stringr::str_glue(
      '({extra_info})'
    )
    
  } else {
    
    extra_info = ''
  }
  
  if (!any(
    c(
      'site',
      'station'
    ) %in% names(info_tbl)
  )) {
    
    s_name = info_tbl[['code']]
    
  } else {
    
    s_name = NULL
  }
  
  site_title_part = c(
    if ('site' %in% names(info_tbl)) {
      textclean::replace_non_ascii(
        info_tbl[['site']]
      )
    } else {
      NULL
    },
    if ('station' %in% names(info_tbl)) {
      textclean::replace_non_ascii(
        info_tbl[['station']]
      )
    } else {
      NULL
    },
    s_name,
    extra_info
  )
  
  
  # Construct summary page title.
  
  summary_label = format_mobs_summary_label(
    summary_time = summary_time,
    summary_stat = summary_stat
  )
  
  page_summary_title = stringr::str_c(
    c(
      stringr::str_to_sentence(
        summary_label
      ),
      'at',
      site_title_part,
      'in',
      info_tbl[['year']]
    ),
    collapse = ' '
  ) %>%
    stringr::str_squish() %>%
    stringr::str_wrap(
      width = 80
    )
  
  
  # Construct monthly page titles.
  
  month_info = mobs_tbl %>%
    dplyr::filter(
      .data$month > 0
    ) %>%
    dplyr::distinct(
      .data$month,
      .data$month_start
    ) %>%
    dplyr::arrange(
      .data$month
    )
  
  page_month_titles = month_info %>%
    dplyr::mutate(
      month_lab = format(
        .data$month_start,
        '%B %Y'
      ),
      title = purrr::map_chr(
        .data$month_lab,
        ~ stringr::str_c(
          c(
            'Hourly data at',
            site_title_part,
            'in',
            .x
          ),
          collapse = ' '
        ) %>%
          stringr::str_squish() %>%
          stringr::str_wrap(
            width = 80
          )
      )
    ) %>%
    dplyr::select(
      .data$month,
      .data$title
    )
  
  
  # Add run information to page titles.
  
  if (!is.null(run_title_info)) {
    
    run_title_info = format_run_title_info(
      run_title_info = run_title_info,
      wrap_width = 80
    )
    
    page_summary_title = stringr::str_c(
      run_title_info,
      '\n\n',
      page_summary_title
    )
    
    page_month_titles = page_month_titles %>%
      dplyr::mutate(
        title = stringr::str_c(
          run_title_info,
          '\n\n',
          title
        )
      )
  }
  
  
  # Pad the variable vector so that each summary/monthly section occupies
  # a whole number of pages.
  
  n_vars = length(
    plot_vars
  )
  
  if (n_vars %% ppp == 0) {
    
    base_var_vector = plot_vars
    
  } else {
    
    base_var_vector = c(
      plot_vars,
      rep(
        NA_character_,
        times = ppp * (
          n_vars %/% ppp + 1
        ) - n_vars
      )
    )
  }
  
  
  # Build plot list in the intended order:
  #
  # 1. summary panel, month == -1
  #
  # 2. consecutive monthly panels, month >= 1
  #
  # Within each period, variables retain the ordering defined by
  # var_params_list.
  
  plot_list = mobs_tbl %>%
    split(
      .$month
    ) %>%
    purrr::map(
      ~ dplyr::right_join(
        .x,
        tibble::tibble(
          var = base_var_vector
        ),
        by = 'var'
      )
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::pull(
      .data$plots
    )
  
  plot_list = plot_list %>%
    purrr::map(
      ~ if (is.null(.x)) {
        create_blank_plot()
      } else {
        .x
      }
    )
  
  
  # Construct page titles.
  
  pages_per_section = (
    n_vars - 1
  ) %/% ppp + 1
  
  month_titles_vec = page_month_titles$title
  
  page_titles = c(
    rep(
      page_summary_title,
      pages_per_section
    ),
    rep(
      month_titles_vec,
      each = pages_per_section
    )
  )
  
  
  # Export PDF.
  
  export = gridExtra::marrangeGrob(
    grobs = plot_list,
    nrow = ppp,
    ncol = 1,
    top = substitute(
      page_titles[g]
    )
  )
  
  ggplot2::ggsave(
    filename = plot_pth_out,
    plot = export,
    paper = 'a4',
    height = 10,
    width = 7
  )
  
  invisible(
    plot_pth_out
  )
}

normalise_emep_dep_unit = function(unit) {
  
  unit %>%
    stringr::str_replace('mgN/', 'mg/') %>%
    stringr::str_replace('mgS/', 'mg/')
}

normalise_mobs_report_tseries_config = function(x) {
  
  if (!is.list(x) || is.null(x[['code']])) {
    stop(
      'Each entry in MOBS_STATION_REPORT_TSERIES must be a list containing at least code.'
    )
  }
  
  supported_resolutions = c('summary', 'raw')
  
  out = list(
    code = x[['code']],
    vars = x[['vars']] %||% NULL,
    resolution = x[['resolution']] %||% 'summary'
  )
  
  if (!out$resolution %in% supported_resolutions) {
    stop(
      "Unsupported MOBS report time-series resolution for site '",
      out$code,
      "': ",
      out$resolution,
      '. Currently supported: ',
      stringr::str_c(
        stringr::str_glue("'{supported_resolutions}'"),
        collapse = ', '
      ),
      '.'
    )
  }
  
  out
}

override_emep_time_range = function(
    time_range,
    start_date = NULL,
    end_date = NULL,
    fmt = '%Y-%m-%d %H:%M',
    tz = 'UTC'
) {
  
  if (
    is.null(start_date) &&
    is.null(end_date)
  ) {
    return(time_range)
  }
  
  first_time = as.POSIXct(
    start_date,
    tz = tz
  )
  
  last_time = as.POSIXct(
    end_date,
    tz = tz
  )
  
  if (
    is.na(first_time) ||
    is.na(last_time)
  ) {
    stop(
      'Unable to parse the supplied EMEP model period.',
      call. = FALSE
    )
  }
  
  if (first_time >= last_time) {
    stop(
      'The EMEP model period start must be earlier than its end.',
      call. = FALSE
    )
  }
  
  list(
    first_time = first_time,
    last_time = last_time,
    first_time_str = format(
      first_time,
      fmt,
      tz = tz
    ),
    last_time_str = format(
      last_time,
      fmt,
      tz = tz
    ),
    start_year = lubridate::year(
      first_time
    ),
    end_year = lubridate::year(
      last_time - lubridate::seconds(1)
    )
  )
}

parse_annual_runlog_emission_block = function(block) {
  
  marker = block[[1]]
  
  emission_file = stringr::str_match(
    marker,
    'for\\s+(.+\\.nc)\\s+\\('
  )[, 2]
  
  unit = stringr::str_match(
    marker,
    '\\(([^()]*)\\)\\s*$'
  )[, 2]
  
  table_lines = block[-1]
  
  header_id = which(
    stringr::str_detect(
      table_lines,
      '^EMTBL\\s+.*\\bLand\\b'
    )
  )
  
  if (length(header_id) == 0) {
    return(NULL)
  }
  
  header_id = header_id[[1]]
  
  header = table_lines[[header_id]] %>%
    stringr::str_remove(
      '^EMTBL\\s+'
    ) %>%
    stringr::str_squish() %>%
    stringr::str_split(
      '\\s+'
    ) %>%
    purrr::pluck(1)
  
  data_lines = table_lines[
    seq.int(
      header_id + 1,
      length(table_lines)
    )
  ]
  
  data_lines = data_lines[
    stringr::str_detect(
      data_lines,
      '^EMTBL\\s+'
    )
  ] %>%
    stringr::str_remove(
      '^EMTBL\\s+'
    )
  
  split_rows = stringr::str_split(
    stringr::str_squish(
      data_lines
    ),
    '\\s+'
  )
  
  expected_n = length(header)
  
  good_rows = lengths(
    split_rows
  ) == expected_n
  
  if (any(!good_rows)) {
    
    warning(
      paste0(
        sum(!good_rows),
        ' EMTBL row(s) did not contain the expected ',
        expected_n,
        ' fields in ',
        emission_file,
        '.'
      )
    )
  }
  
  split_rows = split_rows[
    good_rows
  ]
  
  if (length(split_rows) == 0) {
    return(NULL)
  }
  
  tbl = split_rows %>%
    purrr::map(
      ~ stats::setNames(
        .x,
        header
      )
    ) %>%
    purrr::map_dfr(
      tibble::as_tibble_row
    )
  
  names(tbl) = names(tbl) %>%
    stringr::str_to_lower()
  
  pollutant_cols = intersect(
    c(
      'sox',
      'nox',
      'co',
      'voc',
      'nh3',
      'pm25',
      'pmco'
    ),
    names(tbl)
  )
  
  tbl %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(
        pollutant_cols
      ),
      names_to = 'pollutant',
      values_to = 'emission_text'
    ) %>%
    dplyr::transmute(
      table_type = 'source',
      emission_file = emission_file,
      unit = unit,
      month = NA_integer_,
      region_id = suppressWarnings(
        as.integer(cc)
      ),
      region = land,
      pollutant = pollutant,
      emission_text = emission_text,
      status = dplyr::case_when(
        stringr::str_detect(
          emission_text,
          '\\*'
        ) ~ 'overflow',
        stringr::str_detect(
          emission_text,
          '^[+-]?[0-9]+(?:\\.[0-9]*)?(?:[Ee][+-]?[0-9]+)?$'
        ) ~ 'ok',
        TRUE ~ 'parse_error'
      ),
      emission = dplyr::if_else(
        status == 'ok',
        suppressWarnings(
          as.numeric(
            emission_text
          )
        ),
        NA_real_
      )
    )
}

parse_combined_runlog_emission_block = function(block) {
  
  pollutant_names = c(
    'sox',
    'nox',
    'co',
    'voc',
    'nh3',
    'pm25',
    'pmco'
  )
  
  data_lines = block$lines %>%
    stringr::str_subset(
      '^(?:EMTBL|EMSUM)\\s+\\d+\\s+\\d+\\s+\\S+'
    )
  
  if (length(data_lines) == 0) {
    return(tibble::tibble())
  }
  
  purrr::map_dfr(
    data_lines,
    function(line) {
      
      fields = line %>%
        stringr::str_squish() %>%
        stringr::str_split(
          '\\s+',
          simplify = TRUE
        ) %>%
        as.character()
      
      # prefix, NCalls, region_id, region, then 7 pollutants
      if (length(fields) < 11) {
        return(tibble::tibble())
      }
      
      values = fields[5:11]
      
      tibble::tibble(
        region_id = as.integer(fields[3]),
        region = fields[4],
        pollutant = pollutant_names,
        emission_text = values,
        emission = suppressWarnings(
          as.numeric(values)
        ),
        status = dplyr::if_else(
          is.na(emission) &
            !is.na(emission_text),
          'bad',
          'ok'
        ),
        runlog_method = 'combined_table'
      )
    }
  ) %>%
    dplyr::filter(
      region != 'TOTAL'
    )
}

parse_mobs_tseries_period = function(period,
                                     timezone = 'UTC') {
  
  if (is.null(period)) {
    return(
      list(
        start = as.POSIXct(NA),
        end = as.POSIXct(NA)
      )
    )
  }
  
  if (length(period) != 2) {
    stop('MOBS_STATION_REPORT_TSERIES_PERIOD must be NULL or a vector of length 2.')
  }
  
  parse_one = function(x) {
    
    if (is.null(x) || length(x) == 0 || is.na(x) || x == '') {
      return(as.POSIXct(NA))
    }
    
    if (inherits(x, 'POSIXt')) {
      return(as.POSIXct(x, tz = timezone))
    }
    
    if (inherits(x, 'Date')) {
      return(as.POSIXct(x, tz = timezone))
    }
    
    parsed = lubridate::parse_date_time(
      as.character(x),
      orders = c(
        'ymd HMS',
        'ymd HM',
        'ymd H',
        'ymd'
      ),
      tz = timezone
    )
    
    as.POSIXct(parsed, tz = timezone)
  }
  
  out = list(
    start = parse_one(period[[1]]),
    end = parse_one(period[[2]])
  )
  
  if (!is.na(out$start) && !is.na(out$end) && out$start > out$end) {
    stop('MOBS_STATION_REPORT_TSERIES_PERIOD start is after end.')
  }
  
  out
}

parse_monthly_runlog_emission_block = function(
    block,
    month
) {
  
  marker = block[[1]]
  
  emission_file = stringr::str_match(
    marker,
    'for\\s+(.+\\.nc)\\s+\\('
  )[, 2]
  
  unit = stringr::str_match(
    marker,
    '\\(([^()]*)\\)\\s*$'
  )[, 2]
  
  target_pollutant = stringr::str_match(
    emission_file,
    '^(sox|nox|co|voc|nh3|pm25|pmco)_'
  )[, 2]
  
  if (is.na(target_pollutant)) {
    return(NULL)
  }
  
  pollutant_positions = tibble::tribble(
    ~pollutant, ~start, ~end,
    'sox',          18L,   35L,
    'nox',          36L,   53L,
    'co',           54L,   71L,
    'voc',          72L,   89L,
    'nh3',          90L,  107L,
    'pm25',        108L,  125L,
    'pmco',        126L,  143L
  )
  
  target_position = pollutant_positions %>%
    dplyr::filter(
      pollutant == target_pollutant
    )
  
  table_lines = block[-1]
  
  header_id = which(
    stringr::str_detect(
      table_lines,
      '^EMTBL\\s+.*\\bLand\\b'
    )
  )
  
  if (length(header_id) == 0) {
    return(NULL)
  }
  
  data_lines = table_lines[
    seq.int(
      header_id[[1]] + 1,
      length(table_lines)
    )
  ]
  
  data_lines = data_lines[
    stringr::str_detect(
      data_lines,
      '^EMTBL\\s+'
    )
  ] %>%
    stringr::str_remove(
      '^EMTBL'
    )
  
  region_info = stringr::str_sub(
    data_lines,
    start = 1,
    end = 17
  ) %>%
    stringr::str_match(
      '^\\s*(\\d+)\\s+(\\S+)'
    )
  
  emission_text = stringr::str_sub(
    data_lines,
    start = target_position$start[[1]],
    end = target_position$end[[1]]
  ) %>%
    stringr::str_trim()
  
  tibble::tibble(
    table_type = 'source',
    emission_file = emission_file,
    unit = unit,
    month = month,
    region_id = suppressWarnings(
      as.integer(
        region_info[, 2]
      )
    ),
    region = region_info[, 3],
    pollutant = target_pollutant,
    emission_text = emission_text
  ) %>%
    dplyr::mutate(
      status = dplyr::case_when(
        stringr::str_detect(
          emission_text,
          '\\*'
        ) ~ 'overflow',
        stringr::str_detect(
          emission_text,
          '^[+-]?[0-9]+(?:\\.[0-9]*)?(?:[Ee][+-]?[0-9]+)?$'
        ) ~ 'ok',
        TRUE ~ 'parse_error'
      ),
      emission = dplyr::if_else(
        status == 'ok',
        suppressWarnings(
          as.numeric(
            emission_text
          )
        ),
        NA_real_
      )
    )
}

plot_emission_comparison_diff = function(
    emiss_data,
    comparison = c(
      'inventory',
      'runlog',
      'netcdf'
    ),
    threshold = 5,
    relative_plot_limit = 50,
    bar_fill = 'grey70',
    threshold_colour = 'red',
    threshold_linewidth = 0.5,
    interactive = TRUE
) {
  
  comparison = match.arg(comparison)
  
  comparison_params = switch(
    comparison,
    inventory = list(
      value_var = 'inventory_value',
      unit_var = 'inventory_unit',
      label = 'Inventory'
    ),
    runlog = list(
      value_var = 'runlog_value',
      unit_var = NULL,
      label = 'RunLog'
    ),
    netcdf = list(
      value_var = 'netcdf_value',
      unit_var = 'netcdf_unit',
      label = 'NetCDF'
    )
  )
  
  comparison_value_var =
    comparison_params$value_var
  
  comparison_unit_var =
    comparison_params$unit_var
  
  comparison_label =
    comparison_params$label
  
  
  # Plot one difference
  
  plot_one_diff = function(
    plot_data,
    value_var,
    axis_label,
    threshold = NULL,
    apply_cap = FALSE
  ) {
    
    if (apply_cap) {
      
      plot_data = plot_data %>%
        dplyr::mutate(
          denominator_zero =
            is.na(rel_diff) &
            !is.na(comparison_value) &
            comparison_value == 0,
          plot_value = dplyr::case_when(
            denominator_zero ~
              0,
            TRUE ~
              pmax(
                pmin(
                  .data[[value_var]],
                  relative_plot_limit
                ),
                -relative_plot_limit
              )
          ),
          plot_label = dplyr::case_when(
            denominator_zero ~
              'undefined',
            .data[[value_var]] > relative_plot_limit ~
              paste0(
                round(
                  .data[[value_var]],
                  1
                ),
                '%'
              ),
            .data[[value_var]] < -relative_plot_limit ~
              paste0(
                round(
                  .data[[value_var]],
                  1
                ),
                '%'
              ),
            TRUE ~
              NA_character_
          ),
          cap_note = dplyr::case_when(
            denominator_zero ~
              '<br>Relative difference undefined because denominator = 0',
            abs(.data[[value_var]]) > relative_plot_limit ~
              glue::glue(
                '<br>Bar capped at ±{relative_plot_limit}% for display'
              ),
            TRUE ~
              ''
          )
        )
      
      axis_expand = ggplot2::expansion(
        mult = c(
          0,
          0
        )
      )
      
    } else {
      
      plot_data = plot_data %>%
        dplyr::mutate(
          plot_value =
            .data[[value_var]],
          plot_label =
            NA_character_,
          cap_note =
            ''
        )
      
      axis_expand = ggplot2::expansion(
        mult = c(
          0.05,
          0.05
        )
      )
    }
    
    
    # Plot dimensions
    
    n_regions =
      nrow(plot_data)
    
    plot_height = max(
      220,
      20 * n_regions + 100
    )
    
    bar_width = dplyr::case_when(
      n_regions == 1 ~
        0.25,
      n_regions == 2 ~
        0.4,
      TRUE ~
        0.6
    )
    
    
    # Comparison units
    
    if (
      !is.null(comparison_unit_var) &&
      comparison_unit_var %in% names(plot_data)
    ) {
      
      plot_data = plot_data %>%
        dplyr::mutate(
          comparison_unit =
            .data[[comparison_unit_var]]
        )
      
    } else {
      
      plot_data = plot_data %>%
        dplyr::mutate(
          comparison_unit =
            model_unit
        )
    }
    
    
    # Hover text
    
    plot_data = plot_data %>%
      dplyr::mutate(
        hover_text = glue::glue(
          "Region: {region}<br>",
          "Model: {round(model_value, 2)} {model_unit}<br>",
          "{comparison_label}: {round(comparison_value, 2)} ",
          "{comparison_unit}<br>",
          "Difference: {round(abs_diff, 2)} {model_unit}<br>",
          "Relative difference: ",
          "{dplyr::if_else(
            is.na(rel_diff),
            'undefined',
            paste0(
              round(rel_diff, 1),
              '%'
            )
          )}",
          "{cap_note}"
        )
      )
    
    label_position = if (interactive) {
      0.83
    } else {
      0.99
    }
    
    
    # Base plot
    
    p = ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = region,
        y = plot_value
      )
    ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linewidth = 0.3
      ) +
      ggplot2::geom_col(
        width = bar_width,
        fill = bar_fill
      )
    
    
    # Relative difference
    
    if (apply_cap) {
      
      p = p +
        ggplot2::geom_hline(
          yintercept = c(
            -threshold,
            threshold
          ),
          colour = threshold_colour,
          linetype = 'dashed',
          linewidth = threshold_linewidth
        ) +
        
        # Positive capped values and undefined relative
        # differences.
        
        ggplot2::geom_text(
          data = plot_data %>%
            dplyr::filter(
              denominator_zero |
                .data[[value_var]] >
                relative_plot_limit
            ),
          ggplot2::aes(
            y =
              relative_plot_limit * label_position,
            label =
              plot_label
          ),
          hjust = 1,
          size = 3
        ) +
        
        # Negative capped values.
        
        ggplot2::geom_text(
          data = plot_data %>%
            dplyr::filter(
              !denominator_zero,
              .data[[value_var]] <
                -relative_plot_limit
            ),
          ggplot2::aes(
            y =
              -relative_plot_limit * 0.8,
            label =
              plot_label
          ),
          hjust = 0,
          size = 3
        ) +
        
        ggplot2::scale_y_continuous(
          limits = c(
            -relative_plot_limit,
            relative_plot_limit
          ),
          expand = axis_expand
        )
      
    } else {
      
      p = p +
        ggplot2::scale_y_continuous(
          expand = axis_expand
        )
    }
    
    
    # Formatting
    
    p = p +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = NULL,
        y = axis_label
      ) +
      ggplot2::theme_bw()
    
    
    # Static output
    
    if (!interactive) {
      return(p)
    }
    
    
    # Interactive output
    
    p = p +
      ggplot2::aes(
        text = hover_text
      )
    
    p = plotly::ggplotly(
      p,
      tooltip = 'text',
      height = plot_height
    ) %>%
      plotly::layout(
        showlegend = FALSE
      )
    
    p
  }
  
  
  # Split by pollutant
  
  pollutants =
    unique(
      emiss_data$pollutant
    )
  
  pollutants %>%
    purrr::set_names() %>%
    purrr::map(
      function(poll) {
        
        plot_data = emiss_data %>%
          dplyr::filter(
            pollutant == poll
          ) %>%
          dplyr::mutate(
            comparison_value =
              .data[[comparison_value_var]],
            region = factor(
              region,
              levels = sort(
                unique(region),
                decreasing = TRUE
              )
            )
          )
        
        list(
          relative = plot_one_diff(
            plot_data = plot_data,
            value_var = 'rel_diff',
            axis_label = glue::glue(
              'Model - {comparison_label} (%)'
            ),
            threshold = threshold,
            apply_cap = TRUE
          ),
          absolute = plot_one_diff(
            plot_data = plot_data,
            value_var = 'abs_diff',
            axis_label = glue::glue(
              'Model - {comparison_label} (Gg)'
            ),
            threshold = threshold
          )
        )
      }
    )
}

plot_mobs_scatter = function(
    mobs_data,
    var_params_list,
    var_param_nm = NULL,
    colours = NULL,
    point_colour = NULL,
    pointsize = NULL,
    group_column = NULL,
    facet = TRUE,
    title_prefix = NULL,
    legend_title = 'Site Type'
) {
  
  stopifnot(
    is.data.frame(mobs_data)
  )
  
  stopifnot(
    all(
      c(
        'obs',
        'mod',
        'var'
      ) %in% names(mobs_data)
    )
  )
  
  if (!is.null(group_column)) {
    
    checkmate::assert_string(
      group_column
    )
    
    if (!group_column %in% names(mobs_data)) {
      stop(
        "Grouping column '",
        group_column,
        "' not found in mobs_data."
      )
    }
  }
  
  var_name = unique(
    mobs_data$var
  )
  
  if (length(var_name) != 1L) {
    stop(
      'mobs_data must contain exactly one unique variable.'
    )
  }
  
  if (is.null(var_param_nm)) {
    var_param_nm = var_name
  }
  
  var_id = resolve_mobs_var_id(
    var = var_param_nm,
    var_params_list = var_params_list
  )
  
  if (!var_id %in% names(var_params_list)) {
    stop(
      "Variable '",
      var_param_nm,
      "' not found in var_params_list or aliases."
    )
  }
  
  plot_aes = get_mobs_plot_aesthetics(
    var_nm = var_id,
    var_params_list = var_params_list
  )
  
  if (!is.null(pointsize)) {
    plot_aes$pointsize = pointsize
  }
  
  rng_vals = mobs_data %>%
    tidyr::pivot_longer(
      cols = c(
        'mod',
        'obs'
      ),
      names_to = 'src',
      values_to = 'val'
    ) %>%
    dplyr::summarise(
      min_val = min(
        val,
        na.rm = TRUE
      ),
      max_val = max(
        val,
        na.rm = TRUE
      ),
      .groups = 'drop'
    )
  
  if (
    !is.finite(rng_vals$min_val) ||
    !is.finite(rng_vals$max_val)
  ) {
    
    rng_min = 0
    rng_max = 1
    
  } else {
    
    pad = 0.02
    
    rng_range =
      rng_vals$max_val -
      rng_vals$min_val
    
    if (rng_range == 0) {
      rng_range = abs(
        rng_vals$max_val
      )
    }
    
    if (rng_range == 0) {
      rng_range = 1
    }
    
    rng_min =
      rng_vals$min_val -
      rng_range * pad
    
    rng_max =
      rng_vals$max_val +
      rng_range * pad
  }
  
  lab_unit = format_var_label_emep(
    var = var_param_nm,
    var_params_list = var_params_list,
    context = 'mobs',
    output = 'plotmath',
    include_units = TRUE
  )
  
  title_text = if (
    is.null(title_prefix) ||
    title_prefix == ''
  ) {
    
    lab_unit
    
  } else {
    
    paste0(
      stringr::str_replace_all(
        title_prefix,
        ' ',
        '~'
      ),
      '~',
      lab_unit
    )
  }
  
  p_title = parse(
    text = title_text
  )
  
  base_plot = ggplot2::ggplot()
  
  if (!var_id %in% c('T2', 'td2')) {
    
    base_plot = base_plot +
      ggplot2::geom_abline(
        slope = 0.5,
        intercept = 0,
        linetype = 'longdash',
        linewidth = 0.3
      ) +
      ggplot2::geom_abline(
        slope = 2,
        intercept = 0,
        linetype = 'longdash',
        linewidth = 0.3
      )
  }
  
  base_plot = base_plot +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linewidth = 0.3
    ) +
    ggplot2::scale_x_continuous(
      limits = c(
        rng_min,
        rng_max
      )
    ) +
    ggplot2::scale_y_continuous(
      limits = c(
        rng_min,
        rng_max
      )
    ) +
    ggplot2::labs(
      title = p_title,
      x = 'Observed',
      y = 'Modelled',
      colour = legend_title
    ) +
    ggplot2::theme_bw(
      base_size = 9
    ) +
    ggplot2::theme(
      aspect.ratio = 1,
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = 11
      ),
      panel.grid.major = ggplot2::element_line(
        linewidth = 0.2
      ),
      panel.grid.minor = ggplot2::element_line(
        linewidth = 0.1
      )
    )
  
  if (is.null(group_column)) {
    
    this_point_colour = if (
      is.null(point_colour) ||
      identical(
        point_colour,
        'params_file'
      )
    ) {
      
      plot_aes$mod_colour
      
    } else {
      
      point_colour
    }
    
    p = base_plot +
      ggplot2::geom_point(
        data = mobs_data,
        ggplot2::aes(
          obs,
          mod
        ),
        colour = this_point_colour,
        shape = 1,
        alpha = 0.8,
        size = plot_aes$pointsize
      ) +
      ggplot2::theme(
        legend.position = 'none'
      )
    
  } else {
    
    if (is.null(colours)) {
      stop(
        'colours must be supplied when group_column is used.'
      )
    }
    
    mobs_data = mobs_data %>%
      dplyr::mutate(
        "{group_column}" := factor(
          .data[[group_column]],
          levels = names(colours),
          ordered = TRUE
        )
      )
    
    p = base_plot +
      ggplot2::geom_point(
        data = mobs_data,
        ggplot2::aes(
          obs,
          mod,
          colour = .data[[group_column]]
        ),
        shape = 1,
        alpha = 0.8,
        size = plot_aes$pointsize
      ) +
      ggplot2::scale_color_manual(
        values = colours,
        guide = ggplot2::guide_legend(
          override.aes = list(
            alpha = 1,
            size = plot_aes$pointsize
          )
        )
      )
    
    if (isTRUE(facet)) {
      
      p = p +
        ggplot2::facet_wrap(
          ggplot2::vars(
            .data[[group_column]]
          ),
          ncol = 4
        )
    }
  }
  
  p
}

plot_mobs_scatter_interactive = function(
    mobs_data,
    var_name,
    var_params_list,
    var_param_nm = NULL,
    group_col = NULL,
    group_label = NULL,
    colours = NULL,
    point_colour = NULL,
    pointsize = NULL,
    title_prefix = NULL,
    height = 520
) {
  
  stopifnot(
    is.data.frame(mobs_data)
  )
  
  stopifnot(
    all(
      c(
        'obs',
        'mod',
        'var'
      ) %in% names(mobs_data)
    )
  )
  
  checkmate::assert_string(
    var_name
  )
  
  if (is.null(var_param_nm)) {
    var_param_nm = var_name
  }
  
  checkmate::assert_string(
    var_param_nm
  )
  
  var_id = resolve_mobs_var_id(
    var = var_param_nm,
    var_params_list = var_params_list
  )
  
  if (!var_id %in% names(var_params_list)) {
    stop(
      "Variable '",
      var_param_nm,
      "' not found in var_params_list or aliases."
    )
  }
  
  plot_aes = get_mobs_plot_aesthetics(
    var_nm = var_id,
    var_params_list = var_params_list
  )
  
  if (!is.null(pointsize)) {
    plot_aes$pointsize = pointsize
  }
  
  plotly_pointsize = plot_aes$pointsize * 3.5
  
  if (!is.null(group_col)) {
    
    checkmate::assert_string(
      group_col
    )
    
    if (!group_col %in% names(mobs_data)) {
      stop(
        "Grouping column '",
        group_col,
        "' not found in mobs_data."
      )
    }
    
    if (is.null(colours)) {
      stop(
        'colours must be supplied when group_col is used.'
      )
    }
  }
  
  mobs_data = mobs_data %>%
    dplyr::filter(
      is.finite(obs),
      is.finite(mod)
    )
  
  if (nrow(mobs_data) == 0) {
    
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          annotations = list(
            text = paste(
              'No finite model-observation pairs for',
              var_name
            ),
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            xref = 'paper',
            yref = 'paper'
          ),
          xaxis = list(
            visible = FALSE
          ),
          yaxis = list(
            visible = FALSE
          ),
          height = height
        )
    )
  }
  
  rng = range(
    c(
      mobs_data$obs,
      mobs_data$mod
    ),
    na.rm = TRUE
  )
  
  if (
    !is.finite(rng[1]) ||
    !is.finite(rng[2]) ||
    rng[1] == rng[2]
  ) {
    rng = c(
      0,
      1
    )
  }
  
  pad = 0.1 * diff(rng)
  
  if (
    !is.finite(pad) ||
    pad == 0
  ) {
    pad = 0.1
  }
  
  rng_exp = c(
    rng[1] - pad,
    rng[2] + pad
  )
  
  xseq = seq(
    rng_exp[1],
    rng_exp[2],
    length.out = 100
  )
  
  hover_parts = list()
  
  if ('code' %in% names(mobs_data)) {
    
    hover_parts = c(
      hover_parts,
      list(
        paste0(
          'code: ',
          mobs_data$code
        )
      )
    )
  }
  
  if (
    'station' %in% names(mobs_data) &&
    any(!is.na(mobs_data$station))
  ) {
    
    hover_parts = c(
      hover_parts,
      list(
        paste0(
          'station: ',
          ifelse(
            is.na(mobs_data$station),
            'NA',
            mobs_data$station
          )
        )
      )
    )
    
  } else if (
    'site' %in% names(mobs_data) &&
    any(!is.na(mobs_data$site))
  ) {
    
    hover_parts = c(
      hover_parts,
      list(
        paste0(
          'site: ',
          ifelse(
            is.na(mobs_data$site),
            'NA',
            mobs_data$site
          )
        )
      )
    )
  }
  
  if ('date' %in% names(mobs_data)) {
    
    hover_parts = c(
      hover_parts,
      list(
        paste0(
          'date: ',
          mobs_data$date
        )
      )
    )
  }
  
  if (!is.null(group_col)) {
    
    hover_parts = c(
      hover_parts,
      list(
        paste0(
          if (is.null(group_label)) {
            group_col
          } else {
            group_label
          },
          ': ',
          ifelse(
            is.na(mobs_data[[group_col]]),
            'NA',
            mobs_data[[group_col]]
          )
        )
      )
    )
  }
  
  if (length(hover_parts) == 0) {
    
    base_text = rep(
      '',
      nrow(mobs_data)
    )
    
  } else {
    
    base_text = purrr::pmap_chr(
      as.data.frame(
        hover_parts,
        stringsAsFactors = FALSE
      ),
      ~ paste(
        c(...),
        collapse = '<br>'
      )
    )
  }
  
  hovertemplate = paste0(
    '%{text}',
    '<br>Observed: %{x:.2f}',
    '<br>Modelled: %{y:.2f}',
    '<extra></extra>'
  )
  
  if (!is.null(group_col)) {
    
    p = plotly::plot_ly(
      mobs_data,
      x = ~obs,
      y = ~mod,
      type = 'scatter',
      mode = 'markers',
      color = mobs_data[[group_col]],
      colors = colours,
      marker = list(
        size = plotly_pointsize
      ),
      text = base_text,
      hovertemplate = hovertemplate
    )
    
  } else {
    
    this_point_colour = if (
      is.null(point_colour) ||
      identical(
        point_colour,
        'params_file'
      )
    ) {
      
      plot_aes$mod_colour
      
    } else {
      
      point_colour
    }
    
    p = plotly::plot_ly(
      mobs_data,
      x = ~obs,
      y = ~mod,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = plotly_pointsize,
        color = this_point_colour
      ),
      text = base_text,
      hovertemplate = hovertemplate
    )
  }
  
  ref_col = 'rgba(80,80,80,0.9)'
  
  p = p %>%
    plotly::add_lines(
      x = xseq,
      y = xseq,
      line = list(
        color = ref_col,
        dash = 'solid',
        width = 1.5
      ),
      hoverinfo = 'skip',
      showlegend = FALSE,
      inherit = FALSE
    )
  
  if (!var_id %in% c('T2', 'td2')) {
    
    p = p %>%
      plotly::add_lines(
        x = xseq,
        y = 2 * xseq,
        line = list(
          color = ref_col,
          dash = 'dash',
          width = 1
        ),
        hoverinfo = 'skip',
        showlegend = FALSE,
        inherit = FALSE
      ) %>%
      plotly::add_lines(
        x = xseq,
        y = 0.5 * xseq,
        line = list(
          color = ref_col,
          dash = 'dash',
          width = 1
        ),
        hoverinfo = 'skip',
        showlegend = FALSE,
        inherit = FALSE
      )
  }
  
  layout_args = list(
    xaxis = list(
      title = 'Observed',
      range = rng_exp,
      autorange = FALSE,
      constrain = 'domain'
    ),
    yaxis = list(
      title = 'Modelled',
      range = rng_exp,
      autorange = FALSE,
      scaleanchor = 'x',
      scaleratio = 1,
      constrain = 'domain'
    ),
    margin = list(
      l = 60,
      r = 20,
      t = 10,
      b = 50
    ),
    height = height
  )
  
  if (
    !is.null(group_col) &&
    !is.null(group_label)
  ) {
    
    layout_args$legend = list(
      title = list(
        text = group_label
      )
    )
  }
  
  p = do.call(
    plotly::layout,
    c(
      list(
        p = p
      ),
      layout_args
    )
  )
  
  var_title = format_var_label_emep(
    var = var_id,
    var_params_list = var_params_list,
    context = 'mobs',
    output = 'plain',
    include_units = TRUE
  )
  
  plot_title = if (
    is.null(title_prefix) ||
    title_prefix == ''
  ) {
    
    var_title
    
  } else {
    
    stringr::str_squish(
      paste(
        title_prefix,
        var_title
      )
    )
  }
  
  p = add_plotly_interactive_controls(
    p = p,
    plot_title = plot_title,
    export_width = 1200,
    export_height = 1000,
    export_scale = 2,
    title_y = 0.75,
    title_yanchor = 'bottom'
  )
  
  p
}

plot_mobs_sites_map2 = function(
    sites_df,
    basemap = c(
      'world_topo',
      'satellite',
      'terrain'
    ),
    colours = '#7570b3',
    group_column = NULL,
    legend_label = '',
    legend_title = 'Legend',
    grouping_var_label = NULL,
    show_legend = TRUE,
    highlight_sites_df = NULL,
    highlight_label = 'Sites shown in report'
) {
  
  allowed_basemaps = c(
    'world_topo',
    'satellite',
    'terrain'
  )
  
  if (!all(basemap %in% allowed_basemaps)) {
    stop(
      'basemap must contain only: ',
      paste(
        allowed_basemaps,
        collapse = ', '
      ),
      '.'
    )
  }
  
  if (!is.null(group_column)) {
    
    checkmate::assert_string(
      group_column
    )
    
    if (!group_column %in% names(sites_df)) {
      
      stop(
        "Grouping column '",
        group_column,
        "' not found in sites_df."
      )
    }
  }
  
  if (!is.null(grouping_var_label)) {
    checkmate::assert_string(
      grouping_var_label
    )
  }
  
  sites_df = sites_df %>%
    dplyr::mutate(
      leaflet_group = if (!is.null(group_column)) {
        
        format_mobs_group_values(
          .data[[group_column]],
          missing_label = 'Unclassified'
        )
        
      } else {
        
        legend_label
      },
      leaflet_group = dplyr::if_else(
        is.na(leaflet_group) |
          leaflet_group == '',
        'Unclassified',
        leaflet_group
      )
    )
  
  add_lon_lat = function(x) {
    
    if (inherits(x, 'sf')) {
      
      coords = sf::st_coordinates(x)
      
      x$longitude = coords[, 1]
      x$latitude = coords[, 2]
      
    } else {
      
      lon_col = intersect(
        names(x),
        c(
          'longitude',
          'lon',
          'long',
          'LONGITUDE',
          'Lon'
        )
      )[1]
      
      lat_col = intersect(
        names(x),
        c(
          'latitude',
          'lat',
          'LATITUDE',
          'Lat'
        )
      )[1]
      
      if (
        !is.na(lon_col) &&
        !is.na(lat_col)
      ) {
        
        x = dplyr::rename(
          x,
          longitude = !!rlang::sym(lon_col),
          latitude = !!rlang::sym(lat_col)
        )
        
      } else {
        
        stop(
          'Need either sf geometry or longitude/latitude columns.'
        )
      }
    }
    
    x
  }
  
  sites_df = add_lon_lat(
    sites_df
  )
  
  content = make_site_popup_content(
    df = sites_df,
    group_column = group_column,
    grouping_var_label = grouping_var_label
  )
  
  m = leaflet::leaflet(
    sites_df
  )
  
  basemap_labels = c(
    world_topo = 'World Topographic',
    satellite = 'Satellite',
    terrain = 'Terrain'
  )
  
  if ('world_topo' %in% basemap) {
    
    m = m %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldTopoMap,
        group = basemap_labels[['world_topo']]
      )
  }
  
  if ('satellite' %in% basemap) {
    
    m = m %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery,
        group = basemap_labels[['satellite']]
      )
  }
  
  if ('terrain' %in% basemap) {
    
    m = m %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldTerrain,
        group = basemap_labels[['terrain']]
      )
  }
  
  if (is.null(group_column)) {
    
    m = m %>%
      leaflet::addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = colours[1],
        opacity = 0.9,
        fill = FALSE,
        weight = 3,
        radius = 5,
        popup = content
      )
    
    if (isTRUE(show_legend)) {
      
      m = m %>%
        leaflet::addLegend(
          position = 'topright',
          colors = colours[1],
          labels = unique(
            sites_df$leaflet_group
          ),
          opacity = 1,
          title = legend_title
        )
    }
    
  } else {
    
    if (
      !is.null(names(colours)) &&
      !any(names(colours) == '')
    ) {
      
      names(colours) = format_mobs_group_values(
        names(colours),
        missing_label = 'Unclassified'
      )
    }
    
    lvls_present = sites_df$leaflet_group %>%
      unique() %>%
      stats::na.omit() %>%
      as.character()
    
    if (
      is.null(names(colours)) ||
      any(names(colours) == '')
    ) {
      
      if (length(colours) == 1L) {
        
        colours = stats::setNames(
          rep(
            colours,
            length(lvls_present)
          ),
          lvls_present
        )
        
      } else if (
        length(colours) >=
        length(lvls_present)
      ) {
        
        colours = stats::setNames(
          colours[
            seq_along(lvls_present)
          ],
          lvls_present
        )
        
      } else {
        
        stop(
          'For grouped maps, colours should be either a named vector, ',
          'a single colour, or have at least as many colours as groups.'
        )
      }
    }
    
    missing_lvls = setdiff(
      lvls_present,
      names(colours)
    )
    
    if (length(missing_lvls) > 0) {
      
      extra_cols = stats::setNames(
        scales::hue_pal()(
          length(missing_lvls)
        ),
        missing_lvls
      )
      
      colours = c(
        colours,
        extra_cols
      )
    }
    
    pal = leaflet::colorFactor(
      palette = colours,
      domain = names(colours),
      ordered = TRUE
    )
    
    m = m %>%
      leaflet::addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = ~ pal(leaflet_group),
        opacity = 0.9,
        fill = FALSE,
        weight = 3,
        radius = 5,
        popup = content
      )
    
    if (isTRUE(show_legend)) {
      
      map_legend_title = if (
        !is.null(grouping_var_label) &&
        grouping_var_label != ''
      ) {
        grouping_var_label
      } else {
        legend_title
      }
      
      m = m %>%
        leaflet::addLegend(
          position = 'topright',
          pal = pal,
          values = sites_df$leaflet_group,
          opacity = 1,
          title = map_legend_title
        )
    }
  }
  
  if (
    !is.null(highlight_sites_df) &&
    nrow(highlight_sites_df) > 0
  ) {
    
    if (
      !is.null(group_column) &&
      group_column %in%
      names(highlight_sites_df)
    ) {
      
      highlight_sites_df = highlight_sites_df %>%
        dplyr::mutate(
          leaflet_group = format_mobs_group_values(
            .data[[group_column]],
            missing_label = 'Unclassified'
          ),
          leaflet_group = dplyr::if_else(
            is.na(leaflet_group) |
              leaflet_group == '',
            'Unclassified',
            leaflet_group
          )
        )
      
    } else {
      
      highlight_sites_df = highlight_sites_df %>%
        dplyr::mutate(
          leaflet_group = highlight_label
        )
    }
    
    highlight_sites_df = add_lon_lat(
      highlight_sites_df
    )
    
    highlight_content = make_site_popup_content(
      df = highlight_sites_df,
      group_column = group_column,
      grouping_var_label = grouping_var_label
    )
    
    # Neutral halo around sites shown in report
    m = m %>%
      leaflet::addCircleMarkers(
        data = highlight_sites_df,
        lng = ~longitude,
        lat = ~latitude,
        color = 'black',
        opacity = 1,
        fill = TRUE,
        fillColor = 'white',
        fillOpacity = 1,
        weight = 2,
        radius = 8
      )
    
    # Redraw selected sites in their original group colour
    if (
      !is.null(group_column) &&
      exists('pal')
    ) {
      
      m = m %>%
        leaflet::addCircleMarkers(
          data = highlight_sites_df,
          lng = ~longitude,
          lat = ~latitude,
          color = ~ pal(leaflet_group),
          opacity = 1,
          fill = FALSE,
          weight = 3,
          radius = 5,
          popup = highlight_content
        )
      
    } else {
      
      m = m %>%
        leaflet::addCircleMarkers(
          data = highlight_sites_df,
          lng = ~longitude,
          lat = ~latitude,
          color = colours[1],
          opacity = 1,
          fill = FALSE,
          weight = 3,
          radius = 5,
          popup = highlight_content
        )
    }
  }
  
  if (length(basemap) > 1) {
    
    m = m %>%
      leaflet::addLayersControl(
        baseGroups = unname(
          basemap_labels[basemap]
        ),
        options = leaflet::layersControlOptions(
          collapsed = TRUE
        )
      )
  }
  
  m
}

plot_mobs_stat_map = function(
    sites_df,
    stat,
    var_nm,
    var_param_nm,
    var_params_list,
    stat_lookup,
    breaks = NULL,
    palette = NULL,
    basemap = c(
      'satellite',
      'world_topo'
    ),
    radius = 7,
    stroke = TRUE,
    weight = 0.8,
    fill_opacity = 0.9,
    digits = 4,
    legend_position = 'topright',
    legend_font_size = 10,
    legend_title_size = 11,
    legend_swatch_width = 13,
    legend_swatch_height = 11,
    legend_line_height = 12
) {
  
  basemap = match.arg(
    basemap,
    choices = c(
      'world_topo',
      'satellite'
    ),
    several.ok = TRUE
  )
  
  if (!inherits(sites_df, 'data.frame')) {
    stop(
      'sites_df must be a data frame or sf object.'
    )
  }
  
  req_cols = c(
    'code',
    'var',
    stat
  )
  
  if (!inherits(sites_df, 'sf')) {
    req_cols = c(
      req_cols,
      'longitude',
      'latitude'
    )
  }
  
  missing_cols = setdiff(
    req_cols,
    names(sites_df)
  )
  
  if (length(missing_cols) > 0) {
    stop(
      'sites_df is missing required column(s): ',
      paste(
        missing_cols,
        collapse = ', '
      )
    )
  }
  
  # Common map extent based on all available MOBS sites.
  
  if (inherits(sites_df, 'sf')) {
    
    map_bbox = sites_df %>%
      sf::st_transform(
        4326
      ) %>%
      sf::st_bbox()
    
  } else {
    
    map_bbox = c(
      xmin = min(
        sites_df$longitude,
        na.rm = TRUE
      ),
      ymin = min(
        sites_df$latitude,
        na.rm = TRUE
      ),
      xmax = max(
        sites_df$longitude,
        na.rm = TRUE
      ),
      ymax = max(
        sites_df$latitude,
        na.rm = TRUE
      )
    )
  }
  
  sites_df = sites_df %>%
    dplyr::filter(
      var == var_nm
    )
  
  if (nrow(sites_df) == 0) {
    stop(
      "No data found for variable '",
      var_nm,
      "'."
    )
  }
  
  if (!any(is.finite(sites_df[[stat]]))) {
    stop(
      "Statistic '",
      stat,
      "' contains no finite values for variable '",
      var_nm,
      "'."
    )
  }
  
  
  # Labels.
  
  stat_label = unname(
    stat_lookup[stat] %||% stat
  )
  
  var_label = get_var_param(
    var = var_param_nm,
    key = 'short_lab',
    var_params_list = var_params_list,
    default = var_nm
  )
  
  
  # Breaks and palette.
  
  if (is.null(breaks)) {
    
    breaks = resolve_modstat_map_breaks(
      stat = stat,
      var_nm = var_param_nm,
      sites_df = sites_df,
      var_params_list = var_params_list
    )
  }
  
  x = sites_df[[stat]]
  x = x[is.finite(x)]
  
  breaks = extend_map_breaks_to_data_range(
    breaks = breaks,
    data_min = min(
      x,
      na.rm = TRUE
    ),
    data_max = max(
      x,
      na.rm = TRUE
    )
  )
  
  pal_cols = resolve_palette(
    pal_spec = palette,
    n = length(breaks) - 1
  )
  
  pal = leaflet::colorBin(
    palette = pal_cols,
    domain = sites_df[[stat]],
    bins = breaks,
    na.color = '#BDBDBD',
    right = FALSE
  )
  
  sites_df = sites_df %>%
    dplyr::mutate(
      marker_colour = pal(
        .data[[stat]]
      )
    )
  
  
  # Popup.
  
  popup_value = function(x) {
    
    if (is.numeric(x)) {
      
      out = format(
        round(
          x,
          digits
        ),
        trim = TRUE,
        scientific = FALSE,
        nsmall = 0
      )
      
      out[is.na(x)] = ''
      
      return(out)
    }
    
    x = as.character(x)
    x[is.na(x)] = ''
    
    x
  }
  
  info_vars = intersect(
    c(
      'code',
      'station',
      'site',
      'network',
      'elev(m)',
      'year'
    ),
    names(sites_df)
  )
  
  popup_vars = c(
    info_vars,
    stat
  )
  
  popup_labels = c(
    stringr::str_to_sentence(
      info_vars
    ),
    stat_label
  ) %>%
    stats::setNames(
      popup_vars
    )
  
  popup_content = purrr::map2(
    popup_labels,
    names(popup_labels),
    function(lbl, nm) {
      
      stringr::str_c(
        '<b>',
        lbl,
        ':</b> ',
        popup_value(
          sites_df[[nm]]
        )
      )
    }
  ) %>%
    purrr::transpose() %>%
    stringi::stri_join_list(
      sep = '<br/>'
    )
  
  sites_df = sites_df %>%
    dplyr::mutate(
      popup_content = popup_content
    )
  
  
  # Basemaps.
  
  basemap_lookup = c(
    world_topo = 'Esri.WorldTopoMap',
    satellite = 'Esri.WorldImagery'
  )
  
  basemap_labels = c(
    world_topo = 'World topo',
    satellite = 'Satellite'
  )
  
  basemap_groups = unname(
    basemap_labels[basemap]
  )
  
  m = leaflet::leaflet()
  
  for (this_basemap in basemap) {
    
    m = m %>%
      leaflet::addProviderTiles(
        provider = basemap_lookup[[this_basemap]],
        group = basemap_labels[[this_basemap]]
      )
  }
  
  if (length(basemap_groups) > 1) {
    m = m %>%
      leaflet::hideGroup(
        basemap_groups[-1]
      )
  }
  
  # Site markers.
  
  if (inherits(sites_df, 'sf')) {
    
    m = m %>%
      leaflet::addCircleMarkers(
        data = sites_df,
        radius = radius,
        stroke = stroke,
        weight = weight,
        color = ~marker_colour,
        fillColor = ~marker_colour,
        fillOpacity = fill_opacity,
        popup = ~popup_content
      )
    
  } else {
    
    m = m %>%
      leaflet::addCircleMarkers(
        data = sites_df,
        lng = ~longitude,
        lat = ~latitude,
        radius = radius,
        stroke = stroke,
        weight = weight,
        color = ~marker_colour,
        fillColor = ~marker_colour,
        fillOpacity = fill_opacity,
        popup = ~popup_content
      )
  }
  
  
  # Legend.
  
  legend_html = make_leaflet_bin_legend_html(
    title = stat_label,
    breaks = breaks,
    colours = pal_cols,
    formatter = get_modstat_legend_formatter(
      stat
    ),
    alpha = 1,
    font_size = legend_font_size,
    title_size = legend_title_size,
    swatch_width = legend_swatch_width,
    swatch_height = legend_swatch_height,
    line_height = legend_line_height
  )
  
  if (!is.null(legend_html)) {
    
    m = m %>%
      leaflet::addControl(
        html = legend_html,
        position = legend_position
      )
  }
  
  
  # Basemap control only.
  
  if (length(basemap_groups) > 1) {
    
    m = m %>%
      leaflet::addLayersControl(
        baseGroups = basemap_groups,
        position = 'topleft',
        options = leaflet::layersControlOptions(
          collapsed = TRUE
        )
      )
  }
  
  # Use the same initial extent for all pollutant/statistic maps.
  
  m = m %>%
    leaflet::fitBounds(
      lng1 = unname(map_bbox[['xmin']]),
      lat1 = unname(map_bbox[['ymin']]),
      lng2 = unname(map_bbox[['xmax']]),
      lat2 = unname(map_bbox[['ymax']])
    )
  
  m
}

plot_mobs_stat_map_layers = function(
    sites_df,
    stat,
    var_params_list,
    stat_lookup,
    var_param_names = NULL,
    breaks_by_var = NULL,
    palette_by_var = NULL,
    palette_default = fs::path(
      PALETTE_DIR,
      ABSDIFF_PALETTE
    ),
    basemap = c(
      'world_topo',
      'satellite'
    ),
    radius = 7,
    stroke = TRUE,
    weight = 0.8,
    fill_opacity = 0.9,
    digits = 4,
    legend_position = 'topright',
    legend_font_size = 10,
    legend_title_size = 11,
    legend_swatch_width = 13,
    legend_swatch_height = 11,
    legend_line_height = 12
) {
  
  basemap = match.arg(
    basemap,
    choices = c(
      'world_topo',
      'satellite'
    ),
    several.ok = TRUE
  )
  
  if (!inherits(sites_df, 'data.frame')) {
    stop(
      'sites_df must be a data frame or sf object.'
    )
  }
  
  req_cols = c(
    'code',
    'var',
    stat
  )
  
  if (!inherits(sites_df, 'sf')) {
    req_cols = c(
      req_cols,
      'longitude',
      'latitude'
    )
  }
  
  missing_cols = setdiff(
    req_cols,
    names(sites_df)
  )
  
  if (length(missing_cols) > 0) {
    stop(
      'sites_df is missing required column(s): ',
      paste(
        missing_cols,
        collapse = ', '
      )
    )
  }
  
  
  # Helper functions.
  
  popup_value = function(x) {
    
    if (is.numeric(x)) {
      
      out = format(
        round(
          x,
          digits
        ),
        trim = TRUE,
        scientific = FALSE,
        nsmall = 0
      )
      
      out[is.na(x)] = ''
      
      return(out)
    }
    
    x = as.character(x)
    x[is.na(x)] = ''
    
    x
  }
  
  
  stat_label = function(x) {
    unname(
      stat_lookup[x] %||% x
    )
  }
  
  
  get_var_param_nm = function(var_nm) {
    
    if (is.null(var_param_names)) {
      return(var_nm)
    }
    
    mapped_var = unname(
      var_param_names[var_nm]
    )
    
    if (
      length(mapped_var) == 0 ||
      is.na(mapped_var)
    ) {
      var_nm
    } else {
      mapped_var
    }
  }
  
  
  get_var_label = function(var_nm) {
    
    var_param_nm = get_var_param_nm(
      var_nm
    )
    
    get_var_param(
      var = var_param_nm,
      key = 'short_lab',
      var_params_list = var_params_list,
      default = var_nm
    )
  }
  
  
  get_palette_spec = function(var_nm) {
    
    if (
      !is.null(palette_by_var) &&
      var_nm %in% names(palette_by_var)
    ) {
      
      palette_by_var[[var_nm]]
      
    } else {
      
      palette_default
    }
  }
  
  
  get_breaks = function(
    var_nm,
    df_var
  ) {
    
    if (
      !is.null(breaks_by_var) &&
      var_nm %in% names(breaks_by_var)
    ) {
      
      breaks = breaks_by_var[[var_nm]]
      
    } else {
      
      breaks = resolve_modstat_map_breaks(
        stat = stat,
        var_nm = get_var_param_nm(
          var_nm
        ),
        sites_df = df_var,
        var_params_list = var_params_list
      )
    }
    
    x = df_var[[stat]]
    x = x[is.finite(x)]
    
    if (length(x) > 0) {
      
      breaks = extend_map_breaks_to_data_range(
        breaks = breaks,
        data_min = min(
          x,
          na.rm = TRUE
        ),
        data_max = max(
          x,
          na.rm = TRUE
        )
      )
    }
    
    breaks
  }
  
  
  # Retain only variables with finite values for this statistic.
  
  var_names = unique(
    sites_df$var
  )
  
  var_names = var_names[
    purrr::map_lgl(
      var_names,
      function(var_nm) {
        
        x = sites_df[
          sites_df$var == var_nm,
          stat
        ]
        
        if (inherits(x, 'data.frame')) {
          x = x[[1]]
        }
        
        any(
          is.finite(x)
        )
      }
    )
  ]
  
  if (length(var_names) == 0) {
    stop(
      "Statistic '",
      stat,
      "' contains no finite values."
    )
  }
  
  
  # Set up basemaps.
  
  basemap_lookup = c(
    world_topo = 'Esri.WorldTopoMap',
    satellite = 'Esri.WorldImagery'
  )
  
  basemap_labels = c(
    world_topo = 'World topo',
    satellite = 'Satellite'
  )
  
  basemap_groups = unname(
    basemap_labels[basemap]
  )
  
  m = leaflet::leaflet()
  
  for (this_basemap in basemap) {
    
    m = m %>%
      leaflet::addProviderTiles(
        provider = basemap_lookup[[this_basemap]],
        group = basemap_labels[[this_basemap]]
      )
  }
  
  
  # Add one layer and one legend for each variable.
  
  group_names = purrr::map_chr(
    var_names,
    get_var_label
  )
  
  legend_ids = paste0(
    'var-',
    seq_along(var_names)
  )
  
  names(legend_ids) = group_names
  
  for (i in seq_along(var_names)) {
    
    var_nm = var_names[[i]]
    group_nm = group_names[[i]]
    
    df_var = sites_df %>%
      dplyr::filter(
        var == var_nm
      )
    
    if (nrow(df_var) == 0) {
      next
    }
    
    these_breaks = get_breaks(
      var_nm = var_nm,
      df_var = df_var
    )
    
    pal_cols = resolve_palette(
      pal_spec = get_palette_spec(
        var_nm
      ),
      n = length(these_breaks) - 1
    )
    
    pal = leaflet::colorBin(
      palette = pal_cols,
      domain = df_var[[stat]],
      bins = these_breaks,
      na.color = '#BDBDBD',
      right = FALSE
    )
    
    df_var = df_var %>%
      dplyr::mutate(
        marker_colour = pal(
          .data[[stat]]
        )
      )
    
    
    # Popup.
    
    info_vars0 = c(
      'code',
      'station',
      'site',
      'network',
      'elev(m)',
      'year'
    )
    
    info_vars = intersect(
      info_vars0,
      names(df_var)
    )
    
    popup_labels = c(
      stringr::str_to_sentence(
        info_vars
      ),
      stat_label(stat)
    )
    
    popup_vars = c(
      info_vars,
      stat
    )
    
    names(popup_labels) = popup_vars
    
    popup_content = purrr::map2(
      popup_labels,
      names(popup_labels),
      function(lbl, nm) {
        
        stringr::str_c(
          '<b>',
          lbl,
          ':</b> ',
          popup_value(
            df_var[[nm]]
          )
        )
      }
    ) %>%
      purrr::transpose() %>%
      stringi::stri_join_list(
        sep = '<br/>'
      )
    
    df_var = df_var %>%
      dplyr::mutate(
        popup_content = popup_content
      )
    
    
    # Markers.
    
    if (inherits(df_var, 'sf')) {
      
      m = m %>%
        leaflet::addCircleMarkers(
          data = df_var,
          radius = radius,
          stroke = stroke,
          weight = weight,
          color = ~marker_colour,
          fillColor = ~marker_colour,
          fillOpacity = fill_opacity,
          popup = ~popup_content,
          group = group_nm
        )
      
    } else {
      
      m = m %>%
        leaflet::addCircleMarkers(
          data = df_var,
          lng = ~longitude,
          lat = ~latitude,
          radius = radius,
          stroke = stroke,
          weight = weight,
          color = ~marker_colour,
          fillColor = ~marker_colour,
          fillOpacity = fill_opacity,
          popup = ~popup_content,
          group = group_nm
        )
    }
    
    
    # Variable-specific legend.
    
    legend_id = legend_ids[[group_nm]]
    
    legend_class = paste(
      'mobs-var-legend',
      paste0(
        'mobs-var-legend-',
        legend_id
      ),
      if (i > 1) {
        'mobs-var-legend-hidden'
      } else {
        ''
      }
    )
    
    stat_formatter = get_modstat_legend_formatter(
      stat
    )
    
    legend_html = make_leaflet_bin_legend_html(
      title = paste0(
        group_nm,
        ' - ',
        stat_label(stat)
      ),
      breaks = these_breaks,
      colours = pal_cols,
      formatter = stat_formatter,
      alpha = 1,
      font_size = legend_font_size,
      title_size = legend_title_size,
      swatch_width = legend_swatch_width,
      swatch_height = legend_swatch_height,
      line_height = legend_line_height
    )
    
    if (!is.null(legend_html)) {
      
      m = m %>%
        leaflet::addControl(
          html = legend_html,
          position = legend_position,
          className = legend_class
        )
    }
  }
  
  
  # Initially show only the first variable.
  
  if (length(group_names) > 1) {
    
    m = m %>%
      leaflet::hideGroup(
        group_names[-1]
      )
  }
  
  
  # Layer controls.
  
  m = m %>%
    htmlwidgets::prependContent(
      htmltools::tags$style(
        htmltools::HTML(
          '
          .mobs-var-legend-hidden {
            display: none !important;
          }
          '
        )
      )
    ) %>%
    leaflet::addLayersControl(
      baseGroups = basemap_groups,
      overlayGroups = group_names,
      position = 'topleft',
      options = leaflet::layersControlOptions(
        collapsed = FALSE
      )
    )
  
  
  # Switch legend when the selected variable changes.
  
  if (length(group_names) > 1) {
    
    group_names_json = jsonlite::toJSON(
      group_names,
      auto_unbox = TRUE
    )
    
    legend_id_lookup_json = jsonlite::toJSON(
      as.list(legend_ids),
      auto_unbox = TRUE
    )
    
    m = m %>%
      htmlwidgets::onRender(
        glue::glue(
          .open = '<<',
          .close = '>>',
          '
function(el, x) {

  var groups = <<group_names_json>>;
  var legendIds = <<legend_id_lookup_json>>;

  function setLegend(activeGroup) {

    var legends = el.querySelectorAll(
      ".mobs-var-legend"
    );

    legends.forEach(function(leg) {
      leg.classList.add(
        "mobs-var-legend-hidden"
      );
      leg.style.display = "none";
    });

    var activeLegendId = legendIds[
      activeGroup
    ];

    if (activeLegendId) {

      var activeLegends = el.querySelectorAll(
        ".mobs-var-legend-" +
        activeLegendId
      );

      activeLegends.forEach(function(leg) {
        leg.classList.remove(
          "mobs-var-legend-hidden"
        );
        leg.style.display = "block";
      });
    }
  }

  setTimeout(function() {

    setLegend(groups[0]);

    this.on(
      "overlayadd",
      function(e) {

        if (
          groups.indexOf(e.name) < 0
        ) {
          return;
        }

        groups.forEach(function(groupName) {

          if (groupName === e.name) {
            return;
          }

          var layer = this.layerManager
            .getLayerGroup(
              "marker",
              groupName
            );

          if (
            layer &&
            this.hasLayer(layer)
          ) {
            this.removeLayer(layer);
          }

        }.bind(this));

        setLegend(e.name);
      }
    );

  }.bind(this), 250);
}
          '
        )
      )
  }

m
}

plot_mobs_testref_scatter_interactive = function(
    mobs_data,
    var_name,
    var_params_list,
    var_param_nm = NULL,
    group_col = NULL,
    group_label = NULL,
    colours = NULL,
    point_colour = NULL,
    pointsize = NULL,
    test_label = 'Test',
    ref_label = 'Reference',
    title_prefix = NULL,
    height = 520
) {
  
  stopifnot(
    is.data.frame(mobs_data)
  )
  
  stopifnot(
    all(
      c(
        'obs',
        'ref_mod',
        'test_mod',
        'var'
      ) %in% names(mobs_data)
    )
  )
  
  checkmate::assert_string(
    var_name
  )
  
  checkmate::assert_string(
    test_label
  )
  
  checkmate::assert_string(
    ref_label
  )
  
  if (is.null(var_param_nm)) {
    var_param_nm = var_name
  }
  
  checkmate::assert_string(
    var_param_nm
  )
  
  var_id = resolve_mobs_var_id(
    var = var_param_nm,
    var_params_list = var_params_list
  )
  
  if (!var_id %in% names(var_params_list)) {
    stop(
      "Variable '",
      var_param_nm,
      "' not found in var_params_list or aliases."
    )
  }
  
  plot_aes = get_mobs_plot_aesthetics(
    var_nm = var_id,
    var_params_list = var_params_list
  )
  
  if (!is.null(pointsize)) {
    plot_aes$pointsize = pointsize
  }
  
  plotly_pointsize = plot_aes$pointsize * 3.5
  
  if (!is.null(group_col)) {
    
    checkmate::assert_string(
      group_col
    )
    
    if (!group_col %in% names(mobs_data)) {
      stop(
        "Grouping column '",
        group_col,
        "' not found in mobs_data."
      )
    }
    
    if (is.null(colours)) {
      stop(
        'colours must be supplied when group_col is used.'
      )
    }
  }
  
  mobs_data = mobs_data %>%
    dplyr::filter(
      var == var_name,
      is.finite(obs),
      is.finite(ref_mod),
      is.finite(test_mod)
    )
  
  if (nrow(mobs_data) == 0) {
    
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          annotations = list(
            text = paste(
              'No finite Test/Reference pairs for',
              var_name
            ),
            x = 0.5,
            y = 0.5,
            showarrow = FALSE,
            xref = 'paper',
            yref = 'paper'
          ),
          xaxis = list(
            visible = FALSE
          ),
          yaxis = list(
            visible = FALSE
          ),
          height = height
        )
    )
  }
  
  rng = range(
    c(
      mobs_data$ref_mod,
      mobs_data$test_mod
    ),
    na.rm = TRUE
  )
  
  if (
    !is.finite(rng[1]) ||
    !is.finite(rng[2]) ||
    rng[1] == rng[2]
  ) {
    rng = c(
      0,
      1
    )
  }
  
  pad = 0.1 * diff(rng)
  
  if (
    !is.finite(pad) ||
    pad == 0
  ) {
    pad = 0.1
  }
  
  rng_exp = c(
    rng[1] - pad,
    rng[2] + pad
  )
  
  xseq = seq(
    rng_exp[1],
    rng_exp[2],
    length.out = 100
  )
  
  mobs_data = mobs_data %>%
    dplyr::mutate(
      ref_error = ref_mod - obs,
      test_error = test_mod - obs
    )
  
  hover_parts = list()
  
  if ('code' %in% names(mobs_data)) {
    
    hover_parts = c(
      hover_parts,
      list(
        paste0(
          'code: ',
          mobs_data$code
        )
      )
    )
  }
  
  if (
    'station' %in% names(mobs_data) &&
    any(!is.na(mobs_data$station))
  ) {
    
    hover_parts = c(
      hover_parts,
      list(
        paste0(
          'station: ',
          ifelse(
            is.na(mobs_data$station),
            'NA',
            mobs_data$station
          )
        )
      )
    )
    
  } else if (
    'site' %in% names(mobs_data) &&
    any(!is.na(mobs_data$site))
  ) {
    
    hover_parts = c(
      hover_parts,
      list(
        paste0(
          'site: ',
          ifelse(
            is.na(mobs_data$site),
            'NA',
            mobs_data$site
          )
        )
      )
    )
  }
  
  if (!is.null(group_col)) {
    
    hover_parts = c(
      hover_parts,
      list(
        paste0(
          if (is.null(group_label)) {
            group_col
          } else {
            group_label
          },
          ': ',
          ifelse(
            is.na(mobs_data[[group_col]]),
            'NA',
            mobs_data[[group_col]]
          )
        )
      )
    )
  }
  
  if (length(hover_parts) == 0) {
    
    base_text = rep(
      '',
      nrow(mobs_data)
    )
    
  } else {
    
    base_text = purrr::pmap_chr(
      as.data.frame(
        hover_parts,
        stringsAsFactors = FALSE
      ),
      ~ paste(
        c(...),
        collapse = '<br>'
      )
    )
  }
  
  base_text = paste0(
    base_text,
    '<br>Observed: ',
    sprintf(
      '%.2f',
      mobs_data$obs
    ),
    '<br>',
    ref_label,
    ': ',
    sprintf(
      '%.2f',
      mobs_data$ref_mod
    ),
    '<br>',
    test_label,
    ': ',
    sprintf(
      '%.2f',
      mobs_data$test_mod
    ),
    '<br>',
    ref_label,
    ' error: ',
    sprintf(
      '%.2f',
      mobs_data$ref_error
    ),
    '<br>',
    test_label,
    ' error: ',
    sprintf(
      '%.2f',
      mobs_data$test_error
    )
  )
  
  hovertemplate = paste0(
    '%{text}',
    '<extra></extra>'
  )
  
  if (!is.null(group_col)) {
    
    p = plotly::plot_ly(
      mobs_data,
      x = ~ref_mod,
      y = ~test_mod,
      type = 'scatter',
      mode = 'markers',
      color = mobs_data[[group_col]],
      colors = colours,
      marker = list(
        size = plotly_pointsize
      ),
      text = base_text,
      hovertemplate = hovertemplate
    )
    
  } else {
    
    this_point_colour = if (
      is.null(point_colour) ||
      identical(
        point_colour,
        'params_file'
      )
    ) {
      
      plot_aes$mod_colour
      
    } else {
      
      point_colour
    }
    
    p = plotly::plot_ly(
      mobs_data,
      x = ~ref_mod,
      y = ~test_mod,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = plotly_pointsize,
        color = this_point_colour
      ),
      text = base_text,
      hovertemplate = hovertemplate,
      showlegend = FALSE
    )
  }
  
  p = p %>%
    plotly::add_lines(
      x = xseq,
      y = xseq,
      line = list(
        color = 'black',
        dash = 'solid',
        width = 1.5
      ),
      hoverinfo = 'skip',
      showlegend = FALSE,
      inherit = FALSE
    )
  
  layout_args = list(
    xaxis = list(
      title = ref_label,
      range = rng_exp,
      autorange = FALSE,
      constrain = 'domain'
    ),
    yaxis = list(
      title = test_label,
      range = rng_exp,
      autorange = FALSE,
      scaleanchor = 'x',
      scaleratio = 1,
      constrain = 'domain'
    ),
    margin = list(
      l = 60,
      r = 20,
      t = 10,
      b = 50
    ),
    height = height
  )
  
  if (
    !is.null(group_col) &&
    !is.null(group_label)
  ) {
    
    layout_args$legend = list(
      title = list(
        text = group_label
      )
    )
  }
  
  p = do.call(
    plotly::layout,
    c(
      list(
        p = p
      ),
      layout_args
    )
  )
  
  var_title = format_var_label_emep(
    var = var_id,
    var_params_list = var_params_list,
    context = 'mobs',
    output = 'plain',
    include_units = TRUE
  )
  
  plot_title = if (
    is.null(title_prefix) ||
    title_prefix == ''
  ) {
    
    var_title
    
  } else {
    
    stringr::str_squish(
      paste(
        title_prefix,
        var_title
      )
    )
  }
  
  p = add_plotly_interactive_controls(
    p = p,
    plot_title = plot_title,
    export_width = 1200,
    export_height = 1000,
    export_scale = 2,
    title_y = 0.75,
    title_yanchor = 'bottom'
  )
  
  p
}

plot_mobs_tseries = function(
    dframe,
    var_nm,
    var_param_nm = NULL,
    var_params_list = NULL,
    var_label = NULL,
    mod_colour = NULL,
    mod_linetype = NULL,
    mod_linewidth = NULL,
    obs_colour = NULL,
    obs_fill = NULL,
    obs_linetype = NULL,
    obs_linewidth = NULL,
    obs_style = c('ribbon', 'line'),
    ref_colour = 'grey25',
    ref_linetype = 'solid',
    ref_linewidth = 0.7,
    obs_alpha = 0.5,
    legend_labels = c(
      obs = 'Observed',
      mod = 'Test run',
      ref_mod = 'Reference run'
    )
) {
  
  # Plots a time series for one variable from a tibble with obs and mod
  # columns.
  #
  # Optionally plots reference model values if a ref_mod column is present.
  #
  # For precip, precip and subprecip are plotted together.
  #
  # var_nm identifies the variable in the MOBS data. var_param_nm optionally
  # identifies the variable whose plotting parameters should be used.
  #
  # Plotting aesthetics can be supplied directly or obtained from
  # var_params_list. Directly supplied values take precedence.
  #
  # Observations can be plotted either as a ribbon or as a line.
  
  obs_style = match.arg(obs_style)
  
  
  legend_lab = function(key, default) {
    
    x = legend_labels[key]
    
    if (
      length(x) == 0 ||
      is.na(x) ||
      x == ''
    ) {
      return(default)
    }
    
    unname(x)
  }
  
  
  # Variable plotting parameters
  
  if (is.null(var_param_nm)) {
    var_param_nm = var_nm
  }
  
  if (!is.null(var_params_list)) {
    
    var_id = resolve_mobs_var_id(
      var = var_param_nm,
      var_params_list = var_params_list
    )
    
  } else {
    
    var_id = var_param_nm
  }
  
  is_precip_var = var_id == 'precip' ||
    var_nm == 'precip'
  
  
  # Plot aesthetics
  
  plot_aes = get_mobs_plot_aesthetics(
    var_nm = var_id,
    var_params_list = var_params_list
  )
  
  if (!is.null(mod_colour)) {
    plot_aes$mod_colour = mod_colour
  }
  
  if (!is.null(mod_linetype)) {
    plot_aes$mod_linetype = mod_linetype
  }
  
  if (!is.null(mod_linewidth)) {
    plot_aes$mod_linewidth = mod_linewidth
  }
  
  if (!is.null(obs_colour)) {
    plot_aes$obs_colour = obs_colour
  }
  
  if (!is.null(obs_fill)) {
    plot_aes$obs_fill = obs_fill
  }
  
  if (!is.null(obs_linetype)) {
    plot_aes$obs_linetype = obs_linetype
  }
  
  if (!is.null(obs_linewidth)) {
    plot_aes$obs_linewidth = obs_linewidth
  }
  
  
  # Select variable data
  
  if (isTRUE(is_precip_var)) {
    
    tbl_sub = dframe %>%
      dplyr::filter(
        stringr::str_detect(
          var,
          'precip'
        )
      )
    
  } else {
    
    tbl_sub = dframe %>%
      dplyr::filter(
        var == var_nm
      )
  }
  
  has_ref_mod_this = 'ref_mod' %in% names(tbl_sub) &&
    any(is.finite(tbl_sub$ref_mod))
  
  dc_obs = sum(
    !is.na(tbl_sub$obs)
  )
  
  
  # Determine time resolution and span
  
  dates = tbl_sub %>%
    dplyr::distinct(date) %>%
    dplyr::pull(date) %>%
    sort()
  
  if (
    length(dates) < 2 ||
    all(!is.finite(dates))
  ) {
    
    data_res = Inf
    data_span = Inf
    date_breaks = '1 month'
    date_labels = '%b'
    
  } else {
    
    step_secs = as.numeric(
      diff(dates),
      units = 'secs'
    )
    
    if (
      length(step_secs) == 0 ||
      all(!is.finite(step_secs))
    ) {
      
      data_res = Inf
      
    } else {
      
      data_res = stats::median(
        step_secs,
        na.rm = TRUE
      )
    }
    
    data_span = as.numeric(
      difftime(
        max(
          dates,
          na.rm = TRUE
        ),
        min(
          dates,
          na.rm = TRUE
        ),
        units = 'secs'
      )
    )
    
    if (
      is.finite(data_res) &&
      data_res <= 3600
    ) {
      
      if (data_span <= 86400 * 7) {
        
        date_breaks = '12 hours'
        date_labels = '%e %b %H:%M'
        
      } else if (data_span <= 86400 * 14) {
        
        date_breaks = '1 day'
        date_labels = '%e %b'
        
      } else if (data_span <= 86400 * 32) {
        
        date_breaks = '2 days'
        date_labels = '%e'
        
      } else {
        
        date_breaks = '1 week'
        date_labels = '%e %b'
      }
      
    } else {
      
      date_breaks = '1 month'
      date_labels = '%b'
    }
  }
  
  
  # Convert modelled and observed values to long format
  
  value_cols = c(
    'mod',
    'obs'
  )
  
  if (has_ref_mod_this) {
    
    value_cols = c(
      value_cols,
      'ref_mod'
    )
  }
  
  tbl_sub2 = tbl_sub %>%
    dplyr::mutate(
      var = as.character(var)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = 'scenario',
      values_to = 'conc'
    )
  
  
  # Variable label
  
  if (!is.null(var_label)) {
    
    label_vector = var_label %>%
      purrr::set_names(
        var_nm
      )
    
  } else if (!is.null(var_params_list)) {
    
    label_vector = format_var_label_emep(
      var = var_id,
      var_params_list = var_params_list,
      context = 'mobs',
      output = 'plotmath',
      include_units = TRUE
    ) %>%
      purrr::set_names(
        var_nm
      )
    
  } else {
    
    label_vector = var_nm %>%
      purrr::set_names(
        var_nm
      )
  }
  
  
  # Observations
  
  if (dc_obs == 0) {
    
    p = ggplot2::ggplot()
    
  } else {
    
    obs_tbl = tbl_sub2 %>%
      dplyr::filter(
        scenario == 'obs'
      ) %>%
      dplyr::distinct(
        date,
        .keep_all = TRUE
      )
    
    
    if (obs_style == 'ribbon') {
      
      # geom_ribbon() requires ymin <= ymax. For variables with negative
      # values, keep the ribbon base at zero but flip ymin/ymax as needed.
      #
      # For SLP, avoid filling from zero to ~1000 hPa and instead use the
      # local minimum of the plotted values as the ribbon baseline.
      
      if (var_nm %in% c('slp', 'PSFC')) {
        
        slp_vals = c(
          tbl_sub$mod,
          tbl_sub$obs
        )
        
        if (has_ref_mod_this) {
          
          slp_vals = c(
            slp_vals,
            tbl_sub$ref_mod
          )
        }
        
        slp_ymin = min(
          slp_vals,
          na.rm = TRUE
        )
        
        obs_tbl = obs_tbl %>%
          dplyr::mutate(
            ribbon_ymin = slp_ymin,
            ribbon_ymax = conc
          )
        
      } else {
        
        obs_tbl = obs_tbl %>%
          dplyr::mutate(
            ribbon_ymin = pmin(
              0,
              conc
            ),
            ribbon_ymax = pmax(
              0,
              conc
            )
          )
      }
      
      p = ggplot2::ggplot() +
        ggplot2::geom_ribbon(
          data = obs_tbl,
          ggplot2::aes(
            x = date,
            ymin = ribbon_ymin,
            ymax = ribbon_ymax,
            fill = scenario
          ),
          colour = NA,
          alpha = obs_alpha
        ) +
        ggplot2::scale_fill_manual(
          values = c(
            obs = unname(
              plot_aes$obs_fill
            )
          ),
          labels = c(
            obs = legend_lab(
              'obs',
              'Observed'
            )
          ),
          guide = ggplot2::guide_legend(
            override.aes = list(
              alpha = obs_alpha
            )
          )
        )
      
    } else {
      
      p = ggplot2::ggplot() +
        ggplot2::geom_line(
          data = obs_tbl,
          ggplot2::aes(
            x = date,
            y = conc,
            colour = scenario
          ),
          linetype = plot_aes$obs_linetype,
          linewidth = plot_aes$obs_linewidth
        )
    }
  }
  
  
  # Test-run modelled values
  
  mod_tbl = tbl_sub2 %>%
    dplyr::filter(
      scenario == 'mod'
    ) %>%
    dplyr::mutate(
      var2 = var,
      var = var_nm
    )
  
  
  # Reference-run modelled values
  
  if (has_ref_mod_this) {
    
    ref_mod_tbl = tbl_sub2 %>%
      dplyr::filter(
        scenario == 'ref_mod'
      ) %>%
      dplyr::mutate(
        var = var_nm
      )
  }
  
  
  # Test-run linetypes.
  #
  # precip/subprecip retain their distinction. Otherwise mod_linetype is
  # simply used for the modelled variable.
  
  linetype_vals = c(
    plot_var = plot_aes$mod_linetype,
    sub_plot_var = 'dashed'
  ) %>%
    stats::setNames(
      nm = c(
        var_nm,
        paste0(
          'sub',
          var_nm
        )
      )
    )
  
  
  # Colour scale
  
  colour_values = c(
    mod = unname(
      plot_aes$mod_colour
    )
  )
  
  colour_labels = c(
    mod = legend_lab(
      'mod',
      'Modelled'
    )
  )
  
  if (has_ref_mod_this) {
    
    colour_values = c(
      colour_values,
      ref_mod = ref_colour
    )
    
    colour_labels = c(
      colour_labels,
      ref_mod = legend_lab(
        'ref_mod',
        'Reference run'
      )
    )
  }
  
  if (
    obs_style == 'line' &&
    dc_obs > 0
  ) {
    
    colour_values = c(
      colour_values,
      obs = unname(
        plot_aes$obs_colour
      )
    )
    
    colour_labels = c(
      colour_labels,
      obs = legend_lab(
        'obs',
        'Observed'
      )
    )
  }
  
  colour_breaks = c(
    'mod',
    if (has_ref_mod_this) 'ref_mod',
    if (
      obs_style == 'line' &&
      dc_obs > 0
    ) 'obs'
  )
  
  
  # Legend line aesthetics
  
  legend_linetypes = c(
    mod = plot_aes$mod_linetype,
    ref_mod = ref_linetype,
    obs = plot_aes$obs_linetype
  )
  
  legend_linewidths = c(
    mod = plot_aes$mod_linewidth,
    ref_mod = ref_linewidth,
    obs = plot_aes$obs_linewidth
  )
  
  
  # X-axis limits
  
  x_limits = if (length(dates) >= 1) {
    
    range(
      dates,
      na.rm = TRUE
    )
    
  } else {
    
    NULL
  }
  
  
  # Strip appearance
  
  strip_background = if (obs_style == 'ribbon') {
    
    ggplot2::element_rect(
      fill = unname(
        plot_aes$obs_fill
      )
    )
    
  } else {
    
    ggplot2::element_rect(
      fill = 'white',
      colour = 'white'
    )
  }
  
  
  # Test-run modelled values
  
  p = p +
    ggplot2::geom_line(
      data = mod_tbl,
      ggplot2::aes(
        x = date,
        y = conc,
        colour = scenario,
        linetype = var2
      ),
      linewidth = plot_aes$mod_linewidth
    )
  
  
  # Reference-run modelled values
  
  if (has_ref_mod_this) {
    
    p = p +
      ggplot2::geom_line(
        data = ref_mod_tbl,
        ggplot2::aes(
          x = date,
          y = conc,
          colour = scenario
        ),
        linetype = ref_linetype,
        linewidth = ref_linewidth
      )
  }
  
  
  # Main plot formatting
  
  p = p +
    ggplot2::scale_x_datetime(
      date_breaks = date_breaks,
      date_labels = date_labels,
      expand = ggplot2::expansion(
        c(0, 0)
      ),
      limits = x_limits
    ) +
    ggplot2::scale_color_manual(
      values = colour_values,
      breaks = colour_breaks,
      labels = colour_labels,
      guide = ggplot2::guide_legend(
        override.aes = list(
          linetype = unname(
            legend_linetypes[colour_breaks]
          ),
          linewidth = unname(
            legend_linewidths[colour_breaks]
          )
        )
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = linetype_vals
    ) +
    ggplot2::guides(
      linetype = 'none'
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::facet_wrap(
      ~var,
      strip.position = 'left',
      labeller = ggplot2::as_labeller(
        label_vector,
        default = ggplot2::label_parsed
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.placement = 'outside',
      strip.background = strip_background,
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        linewidth = 0.1
      ),
      axis.ticks = ggplot2::element_line(
        linewidth = 0.1
      ),
      legend.title = ggplot2::element_blank(),
      legend.direction = 'horizontal',
      legend.position = c(
        0.5,
        0.9
      ),
      legend.box = 'horizontal',
      legend.box.background = ggplot2::element_rect(
        fill = 'transparent',
        colour = 'transparent'
      ),
      legend.background = ggplot2::element_rect(
        fill = 'transparent'
      ),
      legend.key = ggplot2::element_rect(
        fill = 'transparent'
      ),
      legend.key.height = grid::unit(
        0.5,
        'lines'
      )
    )
  
  
  # Y-axis parameters
  
  y_params_default = list(
    breaks = ggplot2::waiver(),
    labels = function(x) {
      stringr::str_pad(
        x,
        width = 4,
        side = 'left',
        pad = ' '
      )
    },
    expand = ggplot2::expansion(
      mult = c(
        0,
        0.15
      ),
      add = c(
        0,
        0
      )
    ),
    limits = NULL
  )
  
  y_params_var = list(
    wd = list(
      breaks = c(
        0,
        90,
        180,
        270,
        360
      ),
      labels = c(
        '0',
        '90',
        '180',
        '270',
        '360'
      ),
      expand = ggplot2::expansion(
        mult = c(
          0,
          0
        ),
        add = c(
          0,
          5
        )
      ),
      limits = c(
        0,
        390
      )
    ),
    rh2 = list(
      breaks = seq(
        0,
        100,
        20
      ),
      limits = c(
        0,
        110
      )
    )
  )
  
  if (var_nm %in% names(y_params_var)) {
    
    y_params_intersect = intersect(
      names(y_params_default),
      names(
        y_params_var[[var_nm]]
      )
    )
    
    y_params = y_params_default
    
    for (j in seq_along(y_params_intersect)) {
      
      y_params[[y_params_intersect[j]]] =
        y_params_var[[var_nm]][[y_params_intersect[j]]]
    }
    
  } else {
    
    y_params = y_params_default
  }
  
  
  # Observation availability and y-axis
  
  if (dc_obs == 0) {
    
    p = p +
      ggplot2::labs(
        caption = '* Insufficient or no observations during the period shown.'
      ) +
      ggplot2::theme(
        legend.position = c(
          0.4,
          0.9
        ),
        plot.caption = ggplot2::element_text(
          size = 8
        )
      )
    
    modelled_min = tbl_sub2 %>%
      dplyr::filter(
        scenario %in% c(
          'mod',
          'ref_mod'
        )
      ) %>%
      dplyr::summarise(
        mod_min = min(
          conc,
          na.rm = TRUE
        )
      ) %>%
      dplyr::pull(
        mod_min
      )
    
    if (modelled_min > 0) {
      
      if (var_nm %in% c('slp', 'PSFC')) {
        
        # Keep default y scaling for SLP when only modelled data are present.
        
      } else {
        
        p = p +
          ggplot2::scale_y_continuous(
            breaks = y_params[['breaks']],
            labels = y_params[['labels']],
            expand = y_params[['expand']]
          )
        
        if (!is.null(y_params[['limits']])) {
          
          p = p +
            ggplot2::coord_cartesian(
              ylim = y_params[['limits']]
            )
        }
      }
      
    } else {
      
      p = p +
        ggplot2::scale_y_continuous(
          breaks = y_params[['breaks']],
          labels = y_params[['labels']],
          expand = y_params[['expand']]
        )
      
      if (!is.null(y_params[['limits']])) {
        
        p = p +
          ggplot2::coord_cartesian(
            ylim = y_params[['limits']]
          )
      }
    }
    
  } else {
    
    p = p +
      ggplot2::scale_y_continuous(
        breaks = y_params[['breaks']],
        labels = y_params[['labels']],
        expand = y_params[['expand']],
        limits = y_params[['limits']]
      )
    
    if (!isTRUE(is_precip_var)) {
      
      p = p +
        ggplot2::labs(
          caption = 'Insufficient or no observations during the period shown.'
        ) +
        ggplot2::theme(
          plot.caption = ggplot2::element_text(
            colour = 'transparent',
            size = 8
          )
        )
      
    } else {
      
      p = p +
        ggplot2::labs(
          caption = '* Dashed line shows modelled data only when observations exist.'
        ) +
        ggplot2::theme(
          plot.caption = ggplot2::element_text(
            size = 8
          )
        )
    }
  }
  
  p
}

plot_mobs_tseries_interactive = function(
    df,
    site_name,
    var_nm,
    var_param_nm = NULL,
    var_params_list = NULL,
    mod_colour = NULL,
    mod_linetype = NULL,
    mod_linewidth = NULL,
    obs_colour = NULL,
    obs_fill = NULL,
    obs_linetype = NULL,
    obs_linewidth = NULL,
    obs_style = c('ribbon', 'line'),
    x_resolution = c('summary', 'raw'),
    height = 280,
    obs_alpha = 0.55,
    ref_colour = 'grey25',
    ref_linetype = 'solid',
    ref_linewidth = 0.7,
    legend_labels = c(
      mod = 'Test',
      ref_mod = 'Reference',
      obs = 'Observed'
    )
) {
  
  x_resolution = rlang::arg_match(
    x_resolution
  )
  
  obs_style = rlang::arg_match(
    obs_style
  )
  
  if (is.null(var_param_nm)) {
    var_param_nm = var_nm
  }
  
  var_id = if (!is.null(var_params_list)) {
    
    resolve_mobs_var_id(
      var = var_param_nm,
      var_params_list = var_params_list
    )
    
  } else {
    
    var_nm
  }
  
  
  # Convert ggplot2 linetype names to Plotly dash names.
  
  as_plotly_dash = function(x) {
    
    dplyr::recode(
      x,
      dashed = 'dash',
      dotted = 'dot',
      dotdash = 'dashdot',
      longdash = 'longdash',
      twodash = 'longdashdot',
      .default = x
    )
  }
  
  
  # Variable plotting parameters.
  
  var_title = format_var_label_emep(
    var = var_id,
    var_params_list = var_params_list,
    context = 'mobs',
    output = 'plain',
    include_units = TRUE
  )
  
  plot_title = stringr::str_glue(
    '{site_name} — {var_title}'
  )
  
  is_precip_var =
    var_id == 'precip' ||
    var_nm == 'precip'
  
  is_wd_var =
    var_id == 'wd' ||
    var_nm == 'wd'
  
  legend_lab = function(key, default) {
    
    x = legend_labels[key]
    
    if (
      length(x) == 0 ||
      is.na(x) ||
      x == ''
    ) {
      return(default)
    }
    
    unname(x)
  }
  
  
  # Plot aesthetics.
  
  plot_aes = get_mobs_plot_aesthetics(
    var_nm = var_id,
    var_params_list = var_params_list
  )
  
  if (!is.null(mod_colour)) {
    plot_aes$mod_colour = mod_colour
  }
  
  if (!is.null(mod_linetype)) {
    plot_aes$mod_linetype = mod_linetype
  }
  
  if (!is.null(mod_linewidth)) {
    plot_aes$mod_linewidth = mod_linewidth
  }
  
  if (!is.null(obs_colour)) {
    plot_aes$obs_colour = obs_colour
  }
  
  if (!is.null(obs_fill)) {
    plot_aes$obs_fill = obs_fill
  }
  
  if (!is.null(obs_linetype)) {
    plot_aes$obs_linetype = obs_linetype
  }
  
  if (!is.null(obs_linewidth)) {
    plot_aes$obs_linewidth = obs_linewidth
  }
  
  mod_dash = as_plotly_dash(
    plot_aes$mod_linetype
  )
  
  obs_dash = as_plotly_dash(
    plot_aes$obs_linetype
  )
  
  ref_dash = as_plotly_dash(
    ref_linetype
  )
  
  obs_fill_alpha = grDevices::adjustcolor(
    plot_aes$obs_fill,
    alpha.f = obs_alpha
  )
  
  df = df %>%
    dplyr::arrange(
      date
    )
  
  
  # Select variable data.
  
  if (isTRUE(is_precip_var)) {
    
    df_plot = df %>%
      dplyr::filter(
        var %in% c(
          'precip',
          'subprecip'
        )
      )
    
  } else {
    
    df_plot = df %>%
      dplyr::filter(
        var == var_nm
      )
  }
  
  value_cols = c(
    'obs',
    'mod'
  )
  
  if ('ref_mod' %in% names(df_plot)) {
    
    value_cols = c(
      value_cols,
      'ref_mod'
    )
  }
  
  finite_vals = df_plot %>%
    dplyr::select(
      dplyr::any_of(
        value_cols
      )
    ) %>%
    unlist(
      use.names = FALSE
    )
  
  finite_vals = finite_vals[
    is.finite(finite_vals)
  ]
  
  if (length(finite_vals) == 0) {
    
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          height = height
        )
    )
  }
  
  
  # Y-axis range.
  
  y_rng = range(
    finite_vals,
    na.rm = TRUE
  )
  
  y_pad = diff(y_rng) * 0.05
  
  if (
    !is.finite(y_pad) ||
    y_pad == 0
  ) {
    
    y_pad = abs(
      y_rng[1]
    ) * 0.05
  }
  
  if (
    !is.finite(y_pad) ||
    y_pad == 0
  ) {
    y_pad = 1
  }
  
  y_min = y_rng[1] - y_pad
  y_max = y_rng[2] + y_pad
  
  non_zero_baseline_vars = c(
    'slp',
    'PSFC'
  )
  
  use_zero_baseline =
    !var_nm %in% non_zero_baseline_vars
  
  ribbon_base = if (use_zero_baseline) {
    
    0
    
  } else {
    
    y_min
  }
  
  if (
    use_zero_baseline &&
    y_min > 0
  ) {
    y_min = 0
  }
  
  p = plotly::plot_ly()
  
  
  # Observation availability and caption.
  
  dc_obs = if ('obs' %in% names(df_plot)) {
    
    sum(
      !is.na(df_plot$obs)
    )
    
  } else {
    
    0
  }
  
  caption_text = ''
  
  if (
    isTRUE(is_wd_var) &&
    dc_obs > 0
  ) {
    
    caption_text = paste(
      'Wind direction is shown as modelled minus observed wind direction.',
      'Hover over the plotted values to see the actual wind direction.'
    )
    
  } else if (dc_obs == 0) {
    
    caption_text =
      '* Insufficient or no observations during the period shown.'
    
  } else if (isTRUE(is_precip_var)) {
    
    caption_text =
      '* Dashed line shows modelled data only when observations exist.'
  }
  
  
  # Prepare groups, including DST breaks for raw hourly plots.
  
  prepare_plot_groups = function(d) {
    
    d = d %>%
      dplyr::arrange(
        date
      ) %>%
      dplyr::mutate(
        date_hover = format(
          date,
          '%d %b %Y %H:%M %Z'
        ),
        tz_offset = format(
          date,
          '%z'
        )
      )
    
    if (x_resolution == 'raw') {
      
      d = d %>%
        dplyr::mutate(
          dst_break = tz_offset != dplyr::lag(
            tz_offset,
            default = dplyr::first(
              tz_offset
            )
          )
        )
      
    } else {
      
      d = d %>%
        dplyr::mutate(
          dst_break = FALSE
        )
    }
    
    d %>%
      dplyr::mutate(
        dst_group = cumsum(
          dplyr::coalesce(
            dst_break,
            FALSE
          )
        )
      )
  }
  
  
  # Add observations as a ribbon.
  
  add_obs_ribbon = function(
    p,
    d,
    nm = legend_lab(
      'obs',
      'Observed'
    )
  ) {
    
    if (
      !'obs' %in% names(d) ||
      !any(is.finite(d$obs))
    ) {
      return(p)
    }
    
    # Wind-direction plots display model-observation differences instead.
    
    if (isTRUE(is_wd_var)) {
      return(p)
    }
    
    d = d %>%
      prepare_plot_groups() %>%
      dplyr::mutate(
        obs_group = cumsum(
          !is.finite(obs) |
            dst_break
        )
      ) %>%
      dplyr::filter(
        is.finite(obs)
      )
    
    obs_groups = d %>%
      dplyr::group_split(
        obs_group
      )
    
    for (i in seq_along(obs_groups)) {
      
      d_group = obs_groups[[i]]
      
      if (isTRUE(use_zero_baseline)) {
        
        p = p %>%
          plotly::add_trace(
            data = d_group,
            x = ~date,
            y = ~obs,
            type = 'scatter',
            mode = 'lines',
            name = nm,
            showlegend = i == 1,
            legendrank = 3,
            fill = 'tozeroy',
            fillcolor = obs_fill_alpha,
            line = list(
              width = 0,
              color = obs_fill_alpha
            ),
            customdata = ~date_hover,
            hovertemplate = paste0(
              'date: %{customdata}<br>',
              'value: %{y:.2f}<extra>',
              nm,
              '</extra>'
            )
          )
        
      } else {
        
        d_group = d_group %>%
          dplyr::mutate(
            ribbon_base = ribbon_base
          )
        
        p = p %>%
          plotly::add_trace(
            data = d_group,
            x = ~date,
            y = ~ribbon_base,
            type = 'scatter',
            mode = 'lines',
            line = list(
              width = 0,
              color = obs_fill_alpha
            ),
            hoverinfo = 'skip',
            showlegend = FALSE
          ) %>%
          plotly::add_trace(
            data = d_group,
            x = ~date,
            y = ~obs,
            type = 'scatter',
            mode = 'lines',
            name = nm,
            showlegend = i == 1,
            legendrank = 3,
            fill = 'tonexty',
            fillcolor = obs_fill_alpha,
            line = list(
              width = 0,
              color = obs_fill_alpha
            ),
            customdata = ~date_hover,
            hovertemplate = paste0(
              'date: %{customdata}<br>',
              'value: %{y:.2f}<extra>',
              nm,
              '</extra>'
            )
          )
      }
    }
    
    p
  }
  
  
  # Add observations as a line.
  
  add_obs_line = function(
    p,
    d,
    nm = legend_lab(
      'obs',
      'Observed'
    )
  ) {
    
    if (
      !'obs' %in% names(d) ||
      !any(is.finite(d$obs))
    ) {
      return(p)
    }
    
    # Wind-direction plots display model-observation differences instead.
    
    if (isTRUE(is_wd_var)) {
      return(p)
    }
    
    d = d %>%
      prepare_plot_groups()
    
    obs_groups = d %>%
      dplyr::group_split(
        dst_group
      )
    
    for (i in seq_along(obs_groups)) {
      
      d_group = obs_groups[[i]]
      
      p = p %>%
        plotly::add_trace(
          data = d_group,
          x = ~date,
          y = ~obs,
          type = 'scatter',
          mode = 'lines',
          connectgaps = FALSE,
          name = nm,
          showlegend = i == 1,
          legendrank = 3,
          line = list(
            color = plot_aes$obs_colour,
            dash = obs_dash,
            width = plot_aes$obs_linewidth
          ),
          customdata = ~date_hover,
          hovertemplate = paste0(
            'date: %{customdata}<br>',
            'value: %{y:.2f}<extra>',
            nm,
            '</extra>'
          )
        )
    }
    
    p
  }
  
  
  # Add test-run model line.
  
  add_mod_line = function(
    p,
    d,
    nm = legend_lab(
      'mod',
      'Test'
    ),
    dash = mod_dash,
    showlegend = TRUE
  ) {
    
    if (
      !'mod' %in% names(d) ||
      !any(is.finite(d$mod))
    ) {
      return(p)
    }
    
    d = d %>%
      prepare_plot_groups()
    
    mod_groups = d %>%
      dplyr::group_split(
        dst_group
      )
    
    if (isTRUE(is_wd_var)) {
      
      for (i in seq_along(mod_groups)) {
        
        d_group = mod_groups[[i]] %>%
          dplyr::mutate(
            wd_diff = dplyr::if_else(
              is.finite(mod) &
                is.finite(obs),
              calc_wd_diff(
                mod,
                obs
              ),
              NA_real_
            ),
            hover_txt = dplyr::if_else(
              is.finite(mod) &
                is.finite(obs),
              sprintf(
                paste0(
                  'modelled wd: %.1f°<br>',
                  'observed wd: %.1f°<br>',
                  'difference: %.1f°'
                ),
                mod,
                obs,
                wd_diff
              ),
              NA_character_
            )
          )
        
        if (!any(is.finite(d_group$wd_diff))) {
          next
        }
        
        p = p %>%
          plotly::add_trace(
            data = d_group,
            x = ~date,
            y = ~wd_diff,
            type = 'scatter',
            mode = 'lines',
            connectgaps = FALSE,
            name = nm,
            showlegend = showlegend &&
              i == 1,
            legendrank = 1,
            line = list(
              color = plot_aes$mod_colour,
              dash = dash,
              width = plot_aes$mod_linewidth
            ),
            text = ~hover_txt,
            customdata = ~date_hover,
            hovertemplate = paste0(
              'date: %{customdata}<br>',
              '%{text}<extra>',
              nm,
              '</extra>'
            )
          )
      }
      
      return(p)
    }
    
    for (i in seq_along(mod_groups)) {
      
      d_group = mod_groups[[i]]
      
      p = p %>%
        plotly::add_trace(
          data = d_group,
          x = ~date,
          y = ~mod,
          type = 'scatter',
          mode = 'lines',
          connectgaps = FALSE,
          name = nm,
          showlegend = showlegend &&
            i == 1,
          legendrank = 1,
          line = list(
            color = plot_aes$mod_colour,
            dash = dash,
            width = plot_aes$mod_linewidth
          ),
          customdata = ~date_hover,
          hovertemplate = paste0(
            'date: %{customdata}<br>',
            'value: %{y:.2f}<extra>',
            nm,
            '</extra>'
          )
        )
    }
    
    p
  }
  
  
  # Add reference-run model line.
  
  add_ref_line = function(
    p,
    d,
    nm = legend_lab(
      'ref_mod',
      'Reference'
    ),
    dash = ref_dash
  ) {
    
    if (
      !'ref_mod' %in% names(d) ||
      !any(is.finite(d$ref_mod))
    ) {
      return(p)
    }
    
    d = d %>%
      prepare_plot_groups()
    
    ref_groups = d %>%
      dplyr::group_split(
        dst_group
      )
    
    if (isTRUE(is_wd_var)) {
      
      for (i in seq_along(ref_groups)) {
        
        d_group = ref_groups[[i]] %>%
          dplyr::mutate(
            wd_diff = dplyr::if_else(
              is.finite(ref_mod) &
                is.finite(obs),
              calc_wd_diff(
                ref_mod,
                obs
              ),
              NA_real_
            ),
            hover_txt = dplyr::if_else(
              is.finite(ref_mod) &
                is.finite(obs),
              sprintf(
                paste0(
                  'reference wd: %.1f°<br>',
                  'observed wd: %.1f°<br>',
                  'difference: %.1f°'
                ),
                ref_mod,
                obs,
                wd_diff
              ),
              NA_character_
            )
          )
        
        if (!any(is.finite(d_group$wd_diff))) {
          next
        }
        
        p = p %>%
          plotly::add_trace(
            data = d_group,
            x = ~date,
            y = ~wd_diff,
            type = 'scatter',
            mode = 'lines',
            connectgaps = FALSE,
            name = nm,
            showlegend = i == 1,
            legendrank = 2,
            line = list(
              color = ref_colour,
              dash = dash,
              width = ref_linewidth
            ),
            text = ~hover_txt,
            customdata = ~date_hover,
            hovertemplate = paste0(
              'date: %{customdata}<br>',
              '%{text}<extra>',
              nm,
              '</extra>'
            )
          )
      }
      
      return(p)
    }
    
    for (i in seq_along(ref_groups)) {
      
      d_group = ref_groups[[i]]
      
      p = p %>%
        plotly::add_trace(
          data = d_group,
          x = ~date,
          y = ~ref_mod,
          type = 'scatter',
          mode = 'lines',
          connectgaps = FALSE,
          name = nm,
          showlegend = i == 1,
          legendrank = 2,
          line = list(
            color = ref_colour,
            dash = dash,
            width = ref_linewidth
          ),
          customdata = ~date_hover,
          hovertemplate = paste0(
            'date: %{customdata}<br>',
            'value: %{y:.2f}<extra>',
            nm,
            '</extra>'
          )
        )
    }
    
    p
  }
  
  
  # Add observation trace using the selected style.
  
  add_obs = function(
    p,
    d,
    nm = legend_lab(
      'obs',
      'Observed'
    )
  ) {
    
    if (obs_style == 'ribbon') {
      
      add_obs_ribbon(
        p = p,
        d = d,
        nm = nm
      )
      
    } else {
      
      add_obs_line(
        p = p,
        d = d,
        nm = nm
      )
    }
  }
  
  
  # Build traces.
  #
  # Observation traces are deliberately added first so that ribbons sit
  # behind model lines. legendrank controls the displayed legend order:
  # Test, Reference, Observed.
  
  if (isTRUE(is_precip_var)) {
    
    precip_df = df_plot %>%
      dplyr::filter(
        var == 'precip'
      )
    
    subprecip_df = df_plot %>%
      dplyr::filter(
        var == 'subprecip'
      )
    
    p = p %>%
      add_obs(
        precip_df,
        nm = legend_lab(
          'obs',
          'Observed'
        )
      ) %>%
      add_mod_line(
        precip_df,
        nm = legend_lab(
          'mod',
          'Test'
        ),
        dash = mod_dash
      ) %>%
      add_mod_line(
        subprecip_df,
        nm = legend_lab(
          'mod',
          'Test'
        ),
        dash = 'dash',
        showlegend = FALSE
      ) %>%
      add_ref_line(
        precip_df,
        nm = legend_lab(
          'ref_mod',
          'Reference'
        ),
        dash = ref_dash
      )
    
  } else {
    
    p = p %>%
      add_obs(
        df_plot,
        nm = legend_lab(
          'obs',
          'Observed'
        )
      ) %>%
      add_mod_line(
        df_plot,
        nm = legend_lab(
          'mod',
          'Test'
        ),
        dash = mod_dash
      ) %>%
      add_ref_line(
        df_plot,
        nm = legend_lab(
          'ref_mod',
          'Reference'
        ),
        dash = ref_dash
      )
  }
  
  
  # Caption.
  
  caption_y = if (x_resolution == 'raw') {
    
    -0.12
    
  } else {
    
    -0.22
  }
  
  caption_annotation = if (caption_text != '') {
    
    list(
      list(
        text = caption_text,
        x = 0,
        y = caption_y,
        xref = 'paper',
        yref = 'paper',
        xanchor = 'left',
        yanchor = 'top',
        showarrow = FALSE,
        font = list(
          size = 11
        )
      )
    )
    
  } else {
    
    list()
  }
  
  top_margin = if (x_resolution == 'raw') {
    
    35
    
  } else {
    
    5
  }
  
  bottom_margin = if (caption_text != '') {
    
    if (x_resolution == 'raw') {
      
      85
      
    } else {
      
      65
    }
    
  } else {
    
    45
  }
  
  
  # Final layout and interactive controls.
  
  p = p %>%
    plotly::layout(
      height = height,
      hovermode = FALSE,
      xaxis = make_mobs_plotly_xaxis(
        x_resolution = x_resolution,
        raw_tick_hours = 6
      ),
      yaxis = if (isTRUE(is_wd_var)) {
        
        make_wd_yaxis()
        
      } else {
        
        list(
          title = '',
          range = c(
            y_min,
            y_max
          ),
          rangemode = 'normal'
        )
      },
      showlegend = TRUE,
      legend = list(
        orientation = 'h',
        x = 0,
        y = 1.04,
        traceorder = 'normal'
      ),
      annotations = caption_annotation,
      margin = list(
        l = 45,
        r = 10,
        t = top_margin,
        b = bottom_margin
      )
    )
  
  p = add_plotly_interactive_controls(
    p = p,
    plot_title = plot_title,
    export_width = 1200,
    export_height = 600,
    export_scale = 1
  )
  
  p
}

plot_summary_maps = function(
    diff_list,
    emep_var,
    var_params_list = NULL,
    var_unit = NULL,
    map_geo_params_list = NULL,
    testref_pal_pth = NULL,
    testref_colours = NULL,
    testref_breaks = NULL,
    testref_plot_value_range = NULL,
    reverse_testref_pal = FALSE,
    absdiff_pal_pth = NULL,
    absdiff_colours = NULL,
    absdiff_breaks = NULL,
    absdiff_plot_value_range = NULL,
    reverse_absdiff_pal = FALSE,
    reldiff_pal_pth = NULL,
    reldiff_colours = NULL,
    reldiff_breaks = NULL,
    reldiff_plot_value_range = NULL,
    reverse_reldiff_pal = FALSE,
    ratio_pal_pth = NULL,
    ratio_colours = NULL,
    ratio_breaks = NULL,
    ratio_cbar_tlabels = NULL,
    ratio_cbar_title = NULL,
    ratio_plot_value_range = NULL,
    reverse_ratio_pal = FALSE,
    alpha_stars = 1.0,
    na_fill_colour = 'gray95',
    testref_cbar_tlabels = NULL,
    absdiff_cbar_tlabels = NULL,
    reldiff_cbar_tlabels = NULL,
    cbar_ranges = NULL,
    test_cbar_title = NULL,
    ref_cbar_title = NULL,
    absdiff_cbar_title = NULL,
    reldiff_cbar_title = NULL,
    cbar_width = unit(3.0, 'inches'),
    cbar_height = unit(1, 'mm'),
    cbar_label_angle = 90,
    cbar_label_vjust = 0.5,
    cbar_label_hjust = 1,
    label_type = 'short',
    plot_titles = c(NA, NA, NA, NA),
    plot_title_size = 14,
    legend_title_size = 10,
    legend_text_size = 9,
    panel_background_fill = NULL,
    rasterise_map_layers = FALSE,
    raster_dpi = 300
) {
  
  # helpers
  
  `%|||%` = function(x, y) {
    
    if (is.null(x)) {
      return(y)
    }
    
    if (length(x) == 0) {
      return(y)
    }
    
    if (
      is.atomic(x) &&
      length(x) == 1 &&
      is.na(x)
    ) {
      return(y)
    }
    
    x
  }
  
  vp = function(
    var,
    key,
    default = NULL
  ) {
    
    if (is.null(var_params_list)) {
      return(default)
    }
    
    get_var_param(
      var = var,
      key = key,
      var_params_list = var_params_list,
      default = default
    )
  }
  
  parse_plotmath_label = function(x) {
    
    if (
      is.null(x) ||
      length(x) == 0 ||
      is.na(x) ||
      x == ''
    ) {
      return(NULL)
    }
    
    tryCatch(
      parse(
        text = x
      )[[1]],
      error = function(e) {
        x
      }
    )
  }
  
  make_summary_formatter = function(
    var,
    break_key
  ) {
    
    if (!is.null(var_params_list)) {
      
      return(
        make_map_label_formatter(
          var = var,
          break_key = break_key,
          var_params_list = var_params_list,
          default_scale_cut = summary_map_default_scale_cut,
          default_accuracy = NULL,
          drop0trailing = TRUE
        )
      )
    }
    
    scales::label_number(
      scale_cut = summary_map_default_scale_cut,
      drop0trailing = TRUE
    )
  }
  
  masked_labeler = function(
    all_breaks,
    mask_or_labels,
    formatter
  ) {
    
    force(all_breaks)
    force(mask_or_labels)
    force(formatter)
    
    n_breaks = length(all_breaks)
    
    inner_breaks = all_breaks[
      -c(
        1,
        n_breaks
      )
    ]
    
    if (
      is.character(mask_or_labels) &&
      length(mask_or_labels) ==
      length(inner_breaks)
    ) {
      
      warning(
        "Using character vector for colourbar labels; only inner breaks will be labelled."
      )
      
      return(
        function(used_uppers) {
          
          idx = match(
            used_uppers,
            inner_breaks
          )
          
          ifelse(
            is.na(idx),
            '',
            mask_or_labels[idx]
          )
        }
      )
    }
    
    if (
      is.logical(mask_or_labels) &&
      length(mask_or_labels) ==
      length(all_breaks)
    ) {
      
      all_labs = formatter(
        all_breaks
      )
      
      all_labs[
        !mask_or_labels
      ] = ''
      
      return(
        function(used_uppers) {
          
          idx = match(
            used_uppers,
            all_breaks
          )
          
          ifelse(
            is.na(idx),
            formatter(
              used_uppers
            ),
            all_labs[idx]
          )
        }
      )
    }
    
    if (!is.null(mask_or_labels)) {
      
      warning(
        glue::glue(
          'Invalid colourbar labels: must be character vector of length ',
          '{length(all_breaks) - 2} or logical vector of length ',
          '{length(all_breaks)}. Ignoring.'
        )
      )
    }
    
    function(used_uppers) {
      formatter(
        used_uppers
      )
    }
  }
  
  title_or_null = function(x) {
    
    if (
      is.null(x) ||
      length(x) == 0 ||
      is.na(x)
    ) {
      return(NULL)
    }
    
    x
  }
  
  add_sf_overlays = function(
    p,
    overlays,
    rasterise = FALSE,
    raster_dpi = 300
  ) {
    
    if (
      is.null(overlays) ||
      length(overlays) == 0
    ) {
      return(p)
    }
    
    allowed = c(
      'data',
      'colour',
      'fill',
      'linewidth',
      'alpha',
      'size',
      'linetype'
    )
    
    for (i in seq_along(overlays)) {
      
      params = overlays[[i]]
      
      if (is.null(params[['data']])) {
        next
      }
      
      params = params[
        intersect(
          names(params),
          allowed
        )
      ]
      
      sf_layer = rlang::exec(
        ggplot2::geom_sf,
        !!!params
      )
      
      if (isTRUE(rasterise)) {
        
        sf_layer = ggrastr::rasterise(
          sf_layer,
          dpi = raster_dpi,
          dev = 'ragg'
        )
      }
      
      p = p +
        sf_layer
    }
    
    p
  }
  
  resolve_summary_breaks = function(
    breaks,
    var,
    key,
    data_min,
    data_max
  ) {
    
    if (!is.null(var_params_list)) {
      
      return(
        resolve_map_breaks(
          breaks = breaks,
          var = var,
          key = key,
          var_params_list = var_params_list,
          data_min = data_min,
          data_max = data_max
        )
      )
    }
    
    if (is.null(breaks)) {
      
      key_lab = paste(
        key,
        collapse = '$'
      )
      
      stop(
        "No breaks supplied for ",
        key_lab,
        ". Either provide `",
        key_lab,
        "` through var_params_list or pass explicit breaks."
      )
    }
    
    extend_map_breaks_to_data_range(
      breaks = breaks,
      data_min = data_min,
      data_max = data_max
    )
  }
  
  format_map_lab = function(
    var,
    include_units = TRUE,
    output = 'plotmath'
  ) {
    
    if (!is.null(var_params_list)) {
      
      return(
        format_var_label_emep(
          var = var,
          var_params_list = var_params_list,
          context = 'maps',
          output = output,
          include_units = include_units
        )
      )
    }
    
    lab_out = switch(
      output,
      plotmath = format_lab_for_plotmath(var),
      html = format_lab_for_html(var),
      plain = format_lab_for_plain(var),
      stop(
        "output must be one of 'plotmath', 'html', or 'plain'."
      )
    )
    
    if (
      !isTRUE(include_units) ||
      is.null(var_unit) ||
      var_unit == ''
    ) {
      return(lab_out)
    }
    
    units_out = switch(
      output,
      plotmath = format_units_for_plotmath(var_unit),
      html = format_units_for_html(var_unit),
      plain = format_units_for_plain(var_unit)
    )
    
    if (output == 'plotmath') {
      return(
        glue::glue(
          '{lab_out}~({units_out})'
        )
      )
    }
    
    glue::glue(
      '{lab_out} ({units_out})'
    )
  }
  
  # variable setup
  
  if (length(emep_var) == 1) {
    
    comparison_type = 'testref'
    
    var_test = emep_var
    var_ref = emep_var
    
  } else if (length(emep_var) == 2) {
    
    comparison_type = 'ratio'
    
    var_test = emep_var[1]
    var_ref = emep_var[2]
    
  } else {
    
    stop(
      "emep_var must contain one or two variable names."
    )
  }
  
  base_name = if (
    comparison_type == 'testref'
  ) {
    
    var_test
    
  } else {
    
    paste0(
      var_test,
      '_vs_',
      var_ref
    )
  }
  
  if (is.null(var_params_list)) {
    
    var_test_params = var_test
    var_ref_params = var_ref
    var_comparison_params = base_name
    
  } else {
    
    var_test_params = resolve_var_id(
      var = var_test,
      var_params_list = var_params_list
    )
    
    var_ref_params = resolve_var_id(
      var = var_ref,
      var_params_list = var_params_list
    )
    
    var_comparison_params = resolve_var_id(
      var = base_name,
      var_params_list = var_params_list
    )
    
    if (
      comparison_type == 'testref' &&
      !var_comparison_params %in%
      names(var_params_list)
    ) {
      
      var_comparison_params =
        var_test_params
    }
  }
  
  # plotting value ranges
  
  test_plot_value_range =
    testref_plot_value_range %|||%
    vp(
      var_test_params,
      c(
        'maps',
        'testref_value_range'
      ),
      default = c(
        -Inf,
        Inf
      )
    ) %|||%
    c(
      -Inf,
      Inf
    )
  
  ref_plot_value_range =
    testref_plot_value_range %|||%
    vp(
      var_ref_params,
      c(
        'maps',
        'testref_value_range'
      ),
      default = c(
        -Inf,
        Inf
      )
    ) %|||%
    c(
      -Inf,
      Inf
    )
  
  absdiff_plot_value_range =
    absdiff_plot_value_range %|||%
    vp(
      var_comparison_params,
      c(
        'maps',
        'absdiff_value_range'
      ),
      default = c(
        -Inf,
        Inf
      )
    ) %|||%
    c(
      -Inf,
      Inf
    )
  
  if (comparison_type == 'ratio') {
    
    ratio_plot_value_range =
      ratio_plot_value_range %|||%
      vp(
        var_comparison_params,
        c(
          'maps',
          'ratio_value_range'
        ),
        default = c(
          -Inf,
          Inf
        )
      ) %|||%
      c(
        -Inf,
        Inf
      )
    
  } else {
    
    reldiff_plot_value_range =
      reldiff_plot_value_range %|||%
      vp(
        var_comparison_params,
        c(
          'maps',
          'reldiff_value_range'
        ),
        default = c(
          -Inf,
          Inf
        )
      ) %|||%
      c(
        -Inf,
        Inf
      )
  }
  
  # variable-specific colours
  
  test_colours_resolved =
    testref_colours %|||%
    vp(
      var_test_params,
      c(
        'maps',
        'testref_colours'
      )
    )
  
  ref_colours_resolved =
    testref_colours %|||%
    vp(
      var_ref_params,
      c(
        'maps',
        'testref_colours'
      )
    )
  
  absdiff_colours_resolved =
    absdiff_colours %|||%
    vp(
      var_comparison_params,
      c(
        'maps',
        'absdiff_colours'
      )
    )
  
  reldiff_colours_resolved =
    reldiff_colours %|||%
    vp(
      var_comparison_params,
      c(
        'maps',
        'reldiff_colours'
      )
    )
  
  ratio_colours_resolved =
    ratio_colours %|||%
    vp(
      var_comparison_params,
      c(
        'maps',
        'ratio_colours'
      )
    )
  
  # extract map layers
  
  if (comparison_type == 'testref') {
    
    p1_list = diff_list %>%
      purrr::map('Test') %>%
      purrr::compact()
    
    p2_list = diff_list %>%
      purrr::map('Reference') %>%
      purrr::compact()
    
    p3_list = diff_list %>%
      purrr::map('abs_diff') %>%
      purrr::compact()
    
    p4_list = diff_list %>%
      purrr::map('rel_diff') %>%
      purrr::compact()
    
    if (length(p1_list) == 0) {
      
      stop(
        "No 'Test' data found in diff_list."
      )
    }
    
    has_ref = length(p2_list) > 0
    
  } else {
    
    p1_list = diff_list %>%
      purrr::map(var_test) %>%
      purrr::compact()
    
    p2_list = diff_list %>%
      purrr::map(var_ref) %>%
      purrr::compact()
    
    p3_list = list()
    
    p4_list = diff_list %>%
      purrr::map('ratio') %>%
      purrr::compact()
    
    if (length(p1_list) == 0) {
      
      stop(
        paste0(
          "No numerator data found for '",
          var_test,
          "' in diff_list."
        )
      )
    }
    
    if (length(p2_list) == 0) {
      
      stop(
        paste0(
          "No denominator data found for '",
          var_ref,
          "' in diff_list."
        )
      )
    }
    
    if (length(p4_list) == 0) {
      
      stop(
        "No 'ratio' data found in diff_list."
      )
    }
    
    has_ref = TRUE
  }
  
  # first supplied Test domain determines plot extent
  
  domain_base = p1_list[[1]]
  
  domain_bbox = sf::st_bbox(
    domain_base
  )
  
  # spatial overlays
  
  if (is.null(map_geo_params_list)) {
    
    map_geo_params_list = list(
      countries = list(
        data = rnaturalearth::ne_countries(
          scale = 'medium',
          returnclass = 'sf'
        ),
        colour = 'black',
        fill = NA,
        linewidth = 0.2
      )
    )
  }
  
  map_geo_params_list = purrr::map(
    map_geo_params_list,
    function(geo_params) {
      
      if (is.null(geo_params[['data']])) {
        return(geo_params)
      }
      
      geo_params[['data']] =
        prepare_map_geo(
          geo = geo_params[['data']],
          domain_base = domain_base
        )
      
      geo_params
    }
  )
  
  # data ranges
  
  if (
    !is.null(cbar_ranges) &&
    var_comparison_params %in%
    cbar_ranges$variable
  ) {
    
    row = cbar_ranges %>%
      dplyr::filter(
        variable ==
          var_comparison_params
      )
    
    testref_min_data =
      row$testref_min
    
    testref_max_data =
      row$testref_max
    
    abs_min_data =
      row$abs_min
    
    abs_max_data =
      row$abs_max
    
    rel_min_data =
      row$rel_min
    
    rel_max_data =
      row$rel_max
    
  } else {
    
    if (comparison_type == 'testref') {
      
      testref_range = get_stars_range(
        purrr::compact(
          c(
            p1_list,
            p2_list
          )
        )
      )
      
      test_range = testref_range
      ref_range = testref_range
      
    } else {
      
      test_range = get_stars_range(
        p1_list
      )
      
      ref_range = get_stars_range(
        p2_list
      )
    }
    
    abs_diff_range = get_stars_range(
      p3_list
    )
    
    rel_diff_range = get_stars_range(
      p4_list
    )
    
    test_min_data =
      test_range['min']
    
    test_max_data =
      test_range['max']
    
    ref_min_data =
      ref_range['min']
    
    ref_max_data =
      ref_range['max']
    
    abs_min_data =
      abs_diff_range['min']
    
    abs_max_data =
      abs_diff_range['max']
    
    rel_min_data =
      rel_diff_range['min']
    
    rel_max_data =
      rel_diff_range['max']
  }
  
  if (
    !is.null(cbar_ranges) &&
    var_comparison_params %in%
    cbar_ranges$variable
  ) {
    
    test_min_data =
      testref_min_data
    
    test_max_data =
      testref_max_data
    
    ref_min_data =
      testref_min_data
    
    ref_max_data =
      testref_max_data
  }
  
  # Test / Reference scales
  
  summary_map_default_scale_cut = c(
    0,
    'k' = 1e3,
    'M' = 1e6,
    'G' = 1e9,
    'T' = 1e12
  )
  
  test_breaks = resolve_summary_breaks(
    breaks = testref_breaks,
    var = var_test_params,
    key = c(
      'maps',
      'testref_breaks'
    ),
    data_min = test_min_data,
    data_max = test_max_data
  )
  
  test_formatter = make_summary_formatter(
    var = var_test_params,
    break_key = c(
      'maps',
      'testref_breaks'
    )
  )
  
  test_cbar_tlabels_resolved =
    testref_cbar_tlabels %|||%
    vp(
      var_test_params,
      c(
        'maps',
        'testref_breaks_labs'
      )
    )
  
  test_labels_fun = masked_labeler(
    test_breaks,
    test_cbar_tlabels_resolved,
    test_formatter
  )
  
  if (comparison_type == 'testref') {
    
    ref_breaks = test_breaks
    
    ref_formatter = test_formatter
    
    ref_cbar_tlabels_resolved =
      test_cbar_tlabels_resolved
    
    ref_labels_fun =
      test_labels_fun
    
  } else {
    
    ref_breaks = resolve_summary_breaks(
      breaks = testref_breaks,
      var = var_ref_params,
      key = c(
        'maps',
        'testref_breaks'
      ),
      data_min = ref_min_data,
      data_max = ref_max_data
    )
    
    ref_formatter = make_summary_formatter(
      var = var_ref_params,
      break_key = c(
        'maps',
        'testref_breaks'
      )
    )
    
    ref_cbar_tlabels_resolved =
      testref_cbar_tlabels %|||%
      vp(
        var_ref_params,
        c(
          'maps',
          'testref_breaks_labs'
        )
      )
    
    ref_labels_fun = masked_labeler(
      ref_breaks,
      ref_cbar_tlabels_resolved,
      ref_formatter
    )
  }
  
  # Test palette
  
  if (!is.null(test_colours_resolved)) {
    
    expected_n_colours =
      length(test_breaks) - 1
    
    if (
      length(test_colours_resolved) !=
      expected_n_colours
    ) {
      
      stop(
        glue::glue(
          'testref_colours must have exactly ',
          '{expected_n_colours} colours to match ',
          'length(test_breaks) = {length(test_breaks)}'
        )
      )
    }
    
    test_pal =
      test_colours_resolved
    
  } else {
    
    test_pal = resolve_palette(
      pal_spec = testref_pal_pth,
      n = length(test_breaks) - 1,
      default_hcl = 'Viridis'
    )
  }
  
  if (reverse_testref_pal) {
    test_pal = rev(
      test_pal
    )
  }
  
  test_pal = scales::alpha(
    test_pal,
    alpha = alpha_stars
  )
  
  # Reference palette
  
  if (comparison_type == 'testref') {
    
    ref_pal = test_pal
    
  } else if (!is.null(ref_colours_resolved)) {
    
    expected_n_colours =
      length(ref_breaks) - 1
    
    if (
      length(ref_colours_resolved) !=
      expected_n_colours
    ) {
      
      stop(
        glue::glue(
          'testref_colours must have exactly ',
          '{expected_n_colours} colours to match ',
          'length(ref_breaks) = {length(ref_breaks)}'
        )
      )
    }
    
    ref_pal =
      ref_colours_resolved
    
    if (reverse_testref_pal) {
      ref_pal = rev(
        ref_pal
      )
    }
    
    ref_pal = scales::alpha(
      ref_pal,
      alpha = alpha_stars
    )
    
  } else {
    
    ref_pal = resolve_palette(
      pal_spec = testref_pal_pth,
      n = length(ref_breaks) - 1,
      default_hcl = 'Viridis'
    )
    
    if (reverse_testref_pal) {
      ref_pal = rev(
        ref_pal
      )
    }
    
    ref_pal = scales::alpha(
      ref_pal,
      alpha = alpha_stars
    )
  }
  
  # labels
  
  name_for_title_test = format_map_lab(
    var_test,
    include_units = FALSE,
    output = 'plotmath'
  )
  
  name_for_title_ref = format_map_lab(
    var_ref,
    include_units = FALSE,
    output = 'plotmath'
  )
  
  name_for_title = if (
    var_test != var_ref
  ) {
    
    glue::glue(
      '{name_for_title_test}/{name_for_title_ref}'
    )
    
  } else {
    
    name_for_title_test
  }
  
  resolved_cbar_title_test =
    test_cbar_title %|||%
    vp(
      var_test_params,
      c(
        'maps',
        'testref_cbar_title'
      )
    ) %|||%
    format_map_lab(
      var_test,
      include_units = TRUE,
      output = 'plotmath'
    )
  
  resolved_cbar_title_ref =
    ref_cbar_title %|||%
    vp(
      var_ref_params,
      c(
        'maps',
        'testref_cbar_title'
      )
    ) %|||%
    format_map_lab(
      var_ref,
      include_units = TRUE,
      output = 'plotmath'
    )
  
  resolved_cbar_title_abs =
    absdiff_cbar_title %|||%
    vp(
      var_comparison_params,
      c(
        'maps',
        'absdiff_cbar_title'
      )
    ) %|||%
    if (
      var_test != var_ref
    ) {
      
      glue::glue(
        '{name_for_title_test} - ',
        '{format_map_lab(var_ref, include_units = TRUE, output = "plotmath")}'
      )
      
    } else {
      
      glue::glue(
        'Delta~',
        '{format_map_lab(var_test, include_units = TRUE, output = "plotmath")}'
      )
    }
  
  resolved_cbar_title_rel =
    if (
      comparison_type == 'ratio'
    ) {
      
      ratio_cbar_title %|||%
        vp(
          var_comparison_params,
          c(
            'maps',
            'ratio_cbar_title'
          )
        ) %|||%
        glue::glue(
          '{name_for_title_test} : ',
          '{name_for_title_ref}~("%")'
        )
      
    } else {
      
      reldiff_cbar_title %|||%
        vp(
          var_comparison_params,
          c(
            'maps',
            'reldiff_cbar_title'
          )
        ) %|||%
        glue::glue(
          'Delta~',
          '{name_for_title_test}~("%")'
        )
    }
  
  fill_labs = list(
    parse_plotmath_label(
      resolved_cbar_title_test
    ),
    parse_plotmath_label(
      resolved_cbar_title_ref
    ),
    parse_plotmath_label(
      resolved_cbar_title_abs
    ),
    parse_plotmath_label(
      resolved_cbar_title_rel
    )
  )
  
  top_title = myquickText(
    name_for_title
  )
  
  # individual plot titles
  
  if (comparison_type == 'testref') {
    
    p1_title = plot_titles[1]
    p2_title = plot_titles[2]
    p3_title = plot_titles[3]
    p4_title = plot_titles[4]
    
  } else {
    
    p1_title = plot_titles[1]
    p2_title = plot_titles[2]
    p3_title = NA
    p4_title = plot_titles[3]
  }
  
  # Test
  
  p1 = ggplot2::ggplot()
  
  for (i in seq_along(p1_list)) {
    
    stars_layer = stars::geom_stars(
      data = cut(
        filter_map_values(
          p1_list[[i]],
          plot_value_range =
            test_plot_value_range
        ),
        breaks = test_breaks,
        include.lowest = TRUE
      ),
      alpha = alpha_stars
    )
    
    if (isTRUE(rasterise_map_layers)) {
      
      stars_layer = ggrastr::rasterise(
        stars_layer,
        dpi = raster_dpi,
        dev = 'ragg'
      )
    }
    
    p1 = p1 +
      stars_layer
  }
  
  p1 = add_sf_overlays(
    p1,
    map_geo_params_list,
    rasterise = rasterise_map_layers,
    raster_dpi = raster_dpi
  )
  
  p1 = p1 +
    ggplot2::scale_fill_manual(
      values = test_pal,
      drop = FALSE,
      labels = test_labels_fun,
      na.value = na_fill_colour,
      guide = ggplot2::guide_coloursteps(
        show.limits = TRUE,
        title.position = 'top',
        title.hjust = 0.5,
        frame.colour = 'black',
        frame.linewidth = 0.1,
        barwidth = cbar_width,
        barheight = cbar_height
      )
    ) +
    ggplot2::labs(
      title = title_or_null(
        p1_title
      ),
      fill = fill_labs[[1]]
    ) +
    ggplot2::coord_sf(
      xlim = domain_bbox[
        c(
          'xmin',
          'xmax'
        )
      ],
      ylim = domain_bbox[
        c(
          'ymin',
          'ymax'
        )
      ],
      expand = FALSE
    )
  
  p1 = p1 %>%
    theme_emep_diffmap(
      plot_title_size = plot_title_size,
      legend_title_size = legend_title_size,
      legend_text_size = legend_text_size,
      cbar_label_angle = cbar_label_angle,
      cbar_label_hjust = cbar_label_hjust,
      cbar_label_vjust = cbar_label_vjust
    ) +
    ggplot2::theme(
      panel.background =
        ggplot2::element_rect(
          fill = panel_background_fill,
          colour = NA
        )
    )
  
  # Reference
  
  if (has_ref) {
    
    p2 = ggplot2::ggplot()
    
    for (i in seq_along(p2_list)) {
      
      stars_layer = stars::geom_stars(
        data = cut(
          filter_map_values(
            p2_list[[i]],
            plot_value_range =
              ref_plot_value_range
          ),
          breaks = ref_breaks,
          include.lowest = TRUE
        ),
        alpha = alpha_stars
      )
      
      if (isTRUE(rasterise_map_layers)) {
        
        stars_layer = ggrastr::rasterise(
          stars_layer,
          dpi = raster_dpi,
          dev = 'ragg'
        )
      }
      
      p2 = p2 +
        stars_layer
    }
    
    p2 = add_sf_overlays(
      p2,
      map_geo_params_list,
      rasterise = rasterise_map_layers,
      raster_dpi = raster_dpi
    )
    
    p2 = p2 +
      ggplot2::scale_fill_manual(
        values = ref_pal,
        drop = FALSE,
        labels = ref_labels_fun,
        na.value = na_fill_colour,
        guide = ggplot2::guide_coloursteps(
          show.limits = TRUE,
          title.position = 'top',
          title.hjust = 0.5,
          frame.colour = 'black',
          frame.linewidth = 0.1,
          barwidth = cbar_width,
          barheight = cbar_height
        )
      ) +
      ggplot2::labs(
        title = title_or_null(
          p2_title
        ),
        fill = fill_labs[[2]]
      ) +
      ggplot2::coord_sf(
        xlim = domain_bbox[
          c(
            'xmin',
            'xmax'
          )
        ],
        ylim = domain_bbox[
          c(
            'ymin',
            'ymax'
          )
        ],
        expand = FALSE
      )
    
    p2 = p2 %>%
      theme_emep_diffmap(
        plot_title_size = plot_title_size,
        legend_title_size = legend_title_size,
        legend_text_size = legend_text_size,
        cbar_label_angle = cbar_label_angle,
        cbar_label_hjust = cbar_label_hjust,
        cbar_label_vjust = cbar_label_vjust
      ) +
      ggplot2::theme(
        panel.background =
          ggplot2::element_rect(
            fill = panel_background_fill,
            colour = NA
          )
      )
    
  } else {
    
    p2 = NULL
  }
  
  # Absolute difference
  
  if (!has_ref) {
    
    p3 = NULL
    
  } else {
    
    has_abs_layer =
      length(p3_list) > 0 &&
      !is.null(p3_list[[1]])
    
    identical_abs =
      has_abs_layer &&
      all(
        purrr::map_lgl(
          p3_list,
          ~ {
            tb = as_tibble(.x) %>%
              tidyr::drop_na()
            
            nrow(tb) == 0
          }
        )
      )
    
    abs_has_data =
      has_abs_layer &&
      is.finite(
        as.numeric(
          abs_min_data
        )
      ) &&
      is.finite(
        as.numeric(
          abs_max_data
        )
      )
    
    if (identical_abs) {
      
      p3 = ggplot2::ggplot()
      
      p3 = add_sf_overlays(
        p3,
        map_geo_params_list,
        rasterise =
          rasterise_map_layers,
        raster_dpi =
          raster_dpi
      )
      
      p3 = p3 +
        ggplot2::coord_sf(
          xlim = domain_bbox[
            c(
              'xmin',
              'xmax'
            )
          ],
          ylim = domain_bbox[
            c(
              'ymin',
              'ymax'
            )
          ],
          expand = FALSE
        ) +
        ggplot2::labs(
          title = title_or_null(
            p3_title
          ),
          fill = fill_labs[[3]]
        )
      
      p3 = p3 %>%
        theme_emep_diffmap(
          plot_title_size =
            plot_title_size,
          legend_title_size =
            legend_title_size,
          legend_text_size =
            legend_text_size,
          cbar_label_angle =
            cbar_label_angle,
          cbar_label_hjust =
            cbar_label_hjust,
          cbar_label_vjust =
            cbar_label_vjust
        ) +
        ggplot2::theme(
          legend.position = 'none',
          panel.background =
            ggplot2::element_rect(
              fill =
                panel_background_fill,
              colour = NA
            )
        )
      
      p3 = add_map_notice(
        p3,
        'Data are identical',
        bbox = domain_bbox,
        where = 'centre'
      )
      
    } else if (!abs_has_data) {
      
      p3 = ggplot2::ggplot()
      
      p3 = add_sf_overlays(
        p3,
        map_geo_params_list,
        rasterise =
          rasterise_map_layers,
        raster_dpi =
          raster_dpi
      )
      
      p3 = p3 +
        ggplot2::coord_sf(
          xlim = domain_bbox[
            c(
              'xmin',
              'xmax'
            )
          ],
          ylim = domain_bbox[
            c(
              'ymin',
              'ymax'
            )
          ],
          expand = FALSE
        ) +
        ggplot2::labs(
          title = title_or_null(
            p3_title
          ),
          fill = fill_labs[[3]]
        )
      
      p3 = p3 %>%
        theme_emep_diffmap(
          plot_title_size =
            plot_title_size,
          legend_title_size =
            legend_title_size,
          legend_text_size =
            legend_text_size,
          cbar_label_angle =
            cbar_label_angle,
          cbar_label_hjust =
            cbar_label_hjust,
          cbar_label_vjust =
            cbar_label_vjust
        ) +
        ggplot2::theme(
          legend.position = 'none',
          panel.background =
            ggplot2::element_rect(
              fill =
                panel_background_fill,
              colour = NA
            )
        )
      
      p3 = add_map_notice(
        p3,
        'Absolute difference\nnot calculable',
        bbox = domain_bbox,
        where = 'centre'
      )
      
    } else {
      
      absdiff_breaks =
        resolve_summary_breaks(
          breaks = absdiff_breaks,
          var = var_comparison_params,
          key = c(
            'maps',
            'absdiff_breaks'
          ),
          data_min = abs_min_data,
          data_max = abs_max_data
        )
      
      absdiff_formatter =
        make_summary_formatter(
          var = var_comparison_params,
          break_key = c(
            'maps',
            'absdiff_breaks'
          )
        )
      
      absdiff_cbar_tlabels =
        absdiff_cbar_tlabels %|||%
        vp(
          var_comparison_params,
          c(
            'maps',
            'absdiff_breaks_labs'
          )
        )
      
      abs_labels_fun =
        masked_labeler(
          absdiff_breaks,
          absdiff_cbar_tlabels,
          absdiff_formatter
        )
      
      p3 = ggplot2::ggplot()
      
      for (i in seq_along(p3_list)) {
        
        stars_layer =
          stars::geom_stars(
            data = cut(
              filter_map_values(
                p3_list[[i]],
                plot_value_range =
                  absdiff_plot_value_range
              ),
              breaks =
                absdiff_breaks,
              include.lowest = TRUE
            ),
            alpha = alpha_stars
          )
        
        if (
          isTRUE(
            rasterise_map_layers
          )
        ) {
          
          stars_layer =
            ggrastr::rasterise(
              stars_layer,
              dpi = raster_dpi,
              dev = 'ragg'
            )
        }
        
        p3 = p3 +
          stars_layer
      }
      
      p3 = add_sf_overlays(
        p3,
        map_geo_params_list,
        rasterise =
          rasterise_map_layers,
        raster_dpi =
          raster_dpi
      )
      
      if (
        !is.null(
          absdiff_colours_resolved
        )
      ) {
        
        expected_n_colours =
          length(absdiff_breaks) - 1
        
        if (
          length(
            absdiff_colours_resolved
          ) !=
          expected_n_colours
        ) {
          
          stop(
            glue::glue(
              'absdiff_colours must have exactly ',
              '{expected_n_colours} colours to match ',
              'length(absdiff_breaks) = {length(absdiff_breaks)}'
            )
          )
        }
        
        absdiff_pal =
          absdiff_colours_resolved
        
      } else {
        
        absdiff_pal =
          resolve_palette(
            pal_spec =
              absdiff_pal_pth,
            n =
              length(absdiff_breaks) - 1,
            default_hcl =
              'Blue-Red'
          )
      }
      
      if (reverse_absdiff_pal) {
        
        absdiff_pal = rev(
          absdiff_pal
        )
      }
      
      absdiff_pal =
        scales::alpha(
          absdiff_pal,
          alpha = alpha_stars
        )
      
      p3 = p3 +
        ggplot2::scale_fill_manual(
          values = absdiff_pal,
          drop = FALSE,
          labels = abs_labels_fun,
          na.value = na_fill_colour,
          guide =
            ggplot2::guide_coloursteps(
              show.limits = TRUE,
              title.position = 'top',
              direction = 'horizontal',
              title.hjust = 0.5,
              frame.colour = 'black',
              frame.linewidth = 0.1,
              barwidth = cbar_width,
              barheight = cbar_height
            )
        ) +
        ggplot2::labs(
          title = title_or_null(
            p3_title
          ),
          fill = fill_labs[[3]]
        ) +
        ggplot2::coord_sf(
          xlim = domain_bbox[
            c(
              'xmin',
              'xmax'
            )
          ],
          ylim = domain_bbox[
            c(
              'ymin',
              'ymax'
            )
          ],
          expand = FALSE
        )
      
      p3 = p3 %>%
        theme_emep_diffmap(
          plot_title_size =
            plot_title_size,
          legend_title_size =
            legend_title_size,
          legend_text_size =
            legend_text_size,
          cbar_label_angle =
            cbar_label_angle,
          cbar_label_hjust =
            cbar_label_hjust,
          cbar_label_vjust =
            cbar_label_vjust
        ) +
        ggplot2::theme(
          panel.background =
            ggplot2::element_rect(
              fill =
                panel_background_fill,
              colour = NA
            )
        )
    }
  }
  
  # Relative difference / ratio
  
  if (
    comparison_type == 'testref' &&
    !has_ref
  ) {
    
    p4 = NULL
    
  } else {
    
    has_rel_layer =
      length(p4_list) > 0 &&
      !is.null(p4_list[[1]])
    
    identical_rel =
      has_rel_layer &&
      all(
        purrr::map_lgl(
          p4_list,
          ~ {
            tb = as_tibble(.x) %>%
              tidyr::drop_na()
            
            nrow(tb) == 0
          }
        )
      )
    
    rel_has_data =
      has_rel_layer &&
      is.finite(
        as.numeric(
          rel_min_data
        )
      ) &&
      is.finite(
        as.numeric(
          rel_max_data
        )
      )
    
    if (identical_rel) {
      
      p4 = ggplot2::ggplot()
      
      p4 = add_sf_overlays(
        p4,
        map_geo_params_list,
        rasterise =
          rasterise_map_layers,
        raster_dpi =
          raster_dpi
      )
      
      p4 = p4 +
        ggplot2::coord_sf(
          xlim = domain_bbox[
            c(
              'xmin',
              'xmax'
            )
          ],
          ylim = domain_bbox[
            c(
              'ymin',
              'ymax'
            )
          ],
          expand = FALSE
        ) +
        ggplot2::labs(
          title = title_or_null(
            p4_title
          ),
          fill = fill_labs[[4]]
        )
      
      p4 = p4 %>%
        theme_emep_diffmap(
          plot_title_size =
            plot_title_size,
          legend_title_size =
            legend_title_size,
          legend_text_size =
            legend_text_size,
          cbar_label_angle =
            cbar_label_angle,
          cbar_label_hjust =
            cbar_label_hjust,
          cbar_label_vjust =
            cbar_label_vjust
        ) +
        ggplot2::theme(
          legend.position = 'none',
          panel.background =
            ggplot2::element_rect(
              fill =
                panel_background_fill,
              colour = NA
            )
        )
      
      p4 = add_map_notice(
        p4,
        if (
          comparison_type == 'ratio'
        ) {
          'Ratio\nnot calculable'
        } else {
          'Relative difference\nnot calculable'
        },
        bbox = domain_bbox,
        where = 'centre'
      )
      
    } else if (!rel_has_data) {
      
      p4 = ggplot2::ggplot()
      
      p4 = add_sf_overlays(
        p4,
        map_geo_params_list,
        rasterise =
          rasterise_map_layers,
        raster_dpi =
          raster_dpi
      )
      
      p4 = p4 +
        ggplot2::coord_sf(
          xlim = domain_bbox[
            c(
              'xmin',
              'xmax'
            )
          ],
          ylim = domain_bbox[
            c(
              'ymin',
              'ymax'
            )
          ],
          expand = FALSE
        ) +
        ggplot2::labs(
          title = title_or_null(
            p4_title
          ),
          fill = fill_labs[[4]]
        )
      
      p4 = p4 %>%
        theme_emep_diffmap(
          plot_title_size =
            plot_title_size,
          legend_title_size =
            legend_title_size,
          legend_text_size =
            legend_text_size,
          cbar_label_angle =
            cbar_label_angle,
          cbar_label_hjust =
            cbar_label_hjust,
          cbar_label_vjust =
            cbar_label_vjust
        ) +
        ggplot2::theme(
          legend.position = 'none',
          panel.background =
            ggplot2::element_rect(
              fill =
                panel_background_fill,
              colour = NA
            )
        )
      
      p4 = add_map_notice(
        p4,
        if (
          comparison_type == 'ratio'
        ) {
          'Ratio\nnot calculable'
        } else {
          'Relative difference\nnot calculable'
        },
        bbox = domain_bbox,
        where = 'centre'
      )
      
    } else {
      
      p4_break_key = if (
        comparison_type == 'ratio'
      ) {
        
        c(
          'maps',
          'ratio_breaks'
        )
        
      } else {
        
        c(
          'maps',
          'reldiff_breaks'
        )
      }
      
      p4_breaks =
        resolve_summary_breaks(
          breaks = if (
            comparison_type == 'ratio'
          ) {
            ratio_breaks
          } else {
            reldiff_breaks
          },
          var = var_comparison_params,
          key = p4_break_key,
          data_min = rel_min_data,
          data_max = rel_max_data
        )
      
      p4_formatter =
        make_summary_formatter(
          var = var_comparison_params,
          break_key = p4_break_key
        )
      
      p4_breaks_labs_key = c(
        head(
          p4_break_key,
          -1
        ),
        paste0(
          tail(
            p4_break_key,
            1
          ),
          '_labs'
        )
      )
      
      p4_cbar_tlabels_resolved =
        if (
          comparison_type == 'ratio'
        ) {
          
          ratio_cbar_tlabels %|||%
            vp(
              var_comparison_params,
              p4_breaks_labs_key
            )
          
        } else {
          
          reldiff_cbar_tlabels %|||%
            vp(
              var_comparison_params,
              p4_breaks_labs_key
            )
        }
      
      p4_labels_fun =
        masked_labeler(
          p4_breaks,
          p4_cbar_tlabels_resolved,
          p4_formatter
        )
      
      p4 = ggplot2::ggplot()
      
      for (i in seq_along(p4_list)) {
        
        stars_layer =
          stars::geom_stars(
            data = cut(
              filter_map_values(
                p4_list[[i]],
                plot_value_range =
                  if (
                    comparison_type ==
                    'ratio'
                  ) {
                    ratio_plot_value_range
                  } else {
                    reldiff_plot_value_range
                  }
              ),
              breaks = p4_breaks,
              include.lowest = TRUE
            ),
            alpha = alpha_stars
          )
        
        if (
          isTRUE(
            rasterise_map_layers
          )
        ) {
          
          stars_layer =
            ggrastr::rasterise(
              stars_layer,
              dpi = raster_dpi,
              dev = 'ragg'
            )
        }
        
        p4 = p4 +
          stars_layer
      }
      
      p4 = add_sf_overlays(
        p4,
        map_geo_params_list,
        rasterise =
          rasterise_map_layers,
        raster_dpi =
          raster_dpi
      )
      
      if (comparison_type == 'ratio') {
        
        if (
          !is.null(
            ratio_colours_resolved
          )
        ) {
          
          expected_n_colours =
            length(p4_breaks) - 1
          
          if (
            length(
              ratio_colours_resolved
            ) !=
            expected_n_colours
          ) {
            
            stop(
              glue::glue(
                'ratio_colours must have exactly ',
                '{expected_n_colours} colours to match ',
                'length(p4_breaks) = {length(p4_breaks)}'
              )
            )
          }
          
          p4_pal =
            ratio_colours_resolved
          
        } else {
          
          p4_pal =
            resolve_palette(
              pal_spec =
                ratio_pal_pth,
              n =
                length(p4_breaks) - 1,
              default_hcl =
                'Viridis'
            )
        }
        
        if (reverse_ratio_pal) {
          
          p4_pal = rev(
            p4_pal
          )
        }
        
      } else {
        
        if (
          !is.null(
            reldiff_colours_resolved
          )
        ) {
          
          expected_n_colours =
            length(p4_breaks) - 1
          
          if (
            length(
              reldiff_colours_resolved
            ) !=
            expected_n_colours
          ) {
            
            stop(
              glue::glue(
                'reldiff_colours must have exactly ',
                '{expected_n_colours} colours to match ',
                'length(p4_breaks) = {length(p4_breaks)}'
              )
            )
          }
          
          p4_pal =
            reldiff_colours_resolved
          
        } else {
          
          p4_pal =
            resolve_palette(
              pal_spec =
                reldiff_pal_pth,
              n =
                length(p4_breaks) - 1,
              default_hcl =
                'Blue-Red 3'
            )
        }
        
        if (reverse_reldiff_pal) {
          
          p4_pal = rev(
            p4_pal
          )
        }
      }
      
      p4_pal = scales::alpha(
        p4_pal,
        alpha = alpha_stars
      )
      
      p4 = p4 +
        ggplot2::scale_fill_manual(
          values = p4_pal,
          drop = FALSE,
          labels = p4_labels_fun,
          na.value = na_fill_colour,
          guide =
            ggplot2::guide_coloursteps(
              show.limits = TRUE,
              title.position = 'top',
              title.hjust = 0.5,
              frame.colour = 'black',
              frame.linewidth = 0.1,
              barwidth = cbar_width,
              barheight = cbar_height
            )
        ) +
        ggplot2::labs(
          title = title_or_null(
            p4_title
          ),
          fill = fill_labs[[4]]
        ) +
        ggplot2::coord_sf(
          xlim = domain_bbox[
            c(
              'xmin',
              'xmax'
            )
          ],
          ylim = domain_bbox[
            c(
              'ymin',
              'ymax'
            )
          ],
          expand = FALSE
        )
      
      p4 = p4 %>%
        theme_emep_diffmap(
          plot_title_size =
            plot_title_size,
          legend_title_size =
            legend_title_size,
          legend_text_size =
            legend_text_size,
          cbar_label_angle =
            cbar_label_angle,
          cbar_label_hjust =
            cbar_label_hjust,
          cbar_label_vjust =
            cbar_label_vjust
        ) +
        ggplot2::theme(
          panel.background =
            ggplot2::element_rect(
              fill =
                panel_background_fill,
              colour = NA
            )
        )
    }
  }
  
  # output
  
  if (is.null(p2)) {
    
    return(
      list(
        plots = list(
          p1
        ),
        top_title = top_title
      )
    )
  }
  
  if (comparison_type == 'ratio') {
    
    return(
      list(
        plots = list(
          p1,
          p2,
          p4
        ),
        top_title = top_title
      )
    )
  }
  
  list(
    plots = list(
      p1,
      p2,
      p3,
      p4
    ),
    top_title = top_title
  )
}

plot_summary_maps_leaflet = function(
    diff_list,
    var_nm,
    map_types = c('test'),
    var_params_list = NULL,
    var_unit = NULL,
    label_type = 'short',
    testref_pal_pth = NULL,
    testref_colours = NULL,
    absdiff_pal_pth = NULL,
    absdiff_colours = NULL,
    reldiff_pal_pth = NULL,
    reldiff_colours = NULL,
    ratio_pal_pth = NULL,
    ratio_colours = NULL,
    testref_breaks = NULL,
    testref_plot_value_range = NULL,
    absdiff_breaks = NULL,
    absdiff_plot_value_range = NULL,
    reldiff_breaks = NULL,
    reldiff_plot_value_range = NULL,
    ratio_breaks = NULL,
    ratio_plot_value_range = NULL,
    basemap = c('world_topo', 'satellite', 'terrain'),
    display = c('raster', 'points'),
    opacity = 0.7,
    hover_values = NULL,
    hover_colour = '#EE7733',
    hover_radius = 3,
    hover_max_points = 20000,
    show_leaflet_legend = FALSE,
    max_cells = 250000,
    digits = 2,
    height = 420
) {
  
  `%||%` = function(x, y) {
    if (is.null(x)) y else x
  }
  
  vp = function(
    var,
    key,
    default = NULL
  ) {
    
    if (is.null(var_params_list)) {
      return(default)
    }
    
    get_var_param(
      var = var,
      key = key,
      var_params_list = var_params_list,
      default = default
    )
  }
  
  
  # Variable setup
  
  if (length(var_nm) == 1) {
    
    comparison_type = 'testref'
    
    var_test = var_nm
    var_ref = var_nm
    
  } else if (length(var_nm) == 2) {
    
    comparison_type = 'ratio'
    
    var_test = var_nm[1]
    var_ref = var_nm[2]
    
  } else {
    
    stop(
      "var_nm must contain one or two variable names."
    )
  }
  
  base_name = if (
    comparison_type == 'testref'
  ) {
    
    var_test
    
  } else {
    
    paste0(
      var_test,
      '_vs_',
      var_ref
    )
  }
  
  
  # Resolve variable parameter IDs
  
  if (is.null(var_params_list)) {
    
    var_test_params = var_test
    var_ref_params = var_ref
    var_comparison_params = base_name
    
  } else {
    
    var_test_params = resolve_var_id(
      var = var_test,
      var_params_list = var_params_list
    )
    
    var_ref_params = resolve_var_id(
      var = var_ref,
      var_params_list = var_params_list
    )
    
    var_comparison_params = resolve_var_id(
      var = base_name,
      var_params_list = var_params_list
    )
    
    if (
      comparison_type == 'testref' &&
      !var_comparison_params %in%
      names(var_params_list)
    ) {
      
      var_comparison_params =
        var_test_params
    }
  }
  
  
  # General helpers
  
  normalise_map_type = function(x) {
    
    x = stringr::str_to_lower(x)
    
    x = stringr::str_replace_all(
      x,
      '-',
      '_'
    )
    
    dplyr::case_when(
      x %in% c(
        'test',
        'numerator'
      ) ~ 'test',
      x %in% c(
        'ref',
        'reference',
        'denominator'
      ) ~ 'ref',
      x %in% c(
        'absdiff',
        'abs_diff',
        'absolute_difference'
      ) ~ 'absdiff',
      x %in% c(
        'reldiff',
        'rel_diff',
        'relative_difference'
      ) ~ 'reldiff',
      x %in% c(
        'ratio'
      ) ~ 'ratio',
      TRUE ~ x
    )
  }
  
  get_map_name = function(map_type) {
    
    if (comparison_type == 'testref') {
      
      return(
        dplyr::case_when(
          map_type == 'test' ~ 'Test',
          map_type == 'ref' ~ 'Reference',
          map_type == 'absdiff' ~ 'abs_diff',
          map_type == 'reldiff' ~ 'rel_diff',
          TRUE ~ NA_character_
        )
      )
    }
    
    dplyr::case_when(
      map_type == 'test' ~ var_test,
      map_type == 'ref' ~ var_ref,
      map_type == 'ratio' ~ 'ratio',
      TRUE ~ NA_character_
    )
  }
  
  get_map_list = function(
    map_type,
    diff_list
  ) {
    
    map_name = get_map_name(
      map_type
    )
    
    if (
      is.na(map_name) ||
      is.null(map_name)
    ) {
      return(list())
    }
    
    diff_list %>%
      purrr::map(
        ~ .x[[map_name]]
      ) %>%
      purrr::compact()
  }
  
  get_map_var = function(map_type) {
    
    if (map_type == 'ref') {
      return(var_ref)
    }
    
    if (
      map_type %in% c(
        'absdiff',
        'reldiff',
        'ratio'
      )
    ) {
      return(var_comparison_params)
    }
    
    var_test
  }
  
  get_var_for_breaks = function(map_type) {
    
    if (map_type == 'test') {
      return(var_test_params)
    }
    
    if (map_type == 'ref') {
      
      if (comparison_type == 'testref') {
        return(var_test_params)
      }
      
      return(var_ref_params)
    }
    
    var_comparison_params
  }
  
  get_break_key = function(map_type) {
    
    if (
      map_type %in% c(
        'test',
        'ref'
      )
    ) {
      
      return(
        c(
          'maps',
          'testref_breaks'
        )
      )
    }
    
    if (map_type == 'absdiff') {
      
      return(
        c(
          'maps',
          'absdiff_breaks'
        )
      )
    }
    
    if (map_type == 'reldiff') {
      
      return(
        c(
          'maps',
          'reldiff_breaks'
        )
      )
    }
    
    if (map_type == 'ratio') {
      
      return(
        c(
          'maps',
          'ratio_breaks'
        )
      )
    }
    
    c(
      'maps',
      'testref_breaks'
    )
  }
  
  get_breaks_arg = function(map_type) {
    
    if (
      map_type %in% c(
        'test',
        'ref'
      )
    ) {
      return(testref_breaks)
    }
    
    if (map_type == 'absdiff') {
      return(absdiff_breaks)
    }
    
    if (map_type == 'reldiff') {
      return(reldiff_breaks)
    }
    
    if (map_type == 'ratio') {
      return(ratio_breaks)
    }
    
    NULL
  }
  
  get_colours_arg = function(map_type) {
    
    if (
      map_type %in% c(
        'test',
        'ref'
      )
    ) {
      return(testref_colours)
    }
    
    if (map_type == 'absdiff') {
      return(absdiff_colours)
    }
    
    if (map_type == 'reldiff') {
      return(reldiff_colours)
    }
    
    if (map_type == 'ratio') {
      return(ratio_colours)
    }
    
    NULL
  }
  
  get_break_labs_key = function(map_type) {
    
    if (map_type %in% c('test', 'ref')) {
      return(
        c(
          'maps',
          'testref_breaks_labs'
        )
      )
    }
    
    if (map_type == 'absdiff') {
      return(
        c(
          'maps',
          'absdiff_breaks_labs'
        )
      )
    }
    
    if (map_type == 'reldiff') {
      return(
        c(
          'maps',
          'reldiff_breaks_labs'
        )
      )
    }
    
    if (map_type == 'ratio') {
      return(
        c(
          'maps',
          'ratio_breaks_labs'
        )
      )
    }
    
    NULL
  }
  
  get_cbar_title_key = function(map_type) {
    
    if (map_type %in% c('test', 'ref')) {
      return(
        c(
          'maps',
          'testref_cbar_title'
        )
      )
    }
    
    if (map_type == 'absdiff') {
      return(
        c(
          'maps',
          'absdiff_cbar_title'
        )
      )
    }
    
    if (map_type == 'reldiff') {
      return(
        c(
          'maps',
          'reldiff_cbar_title'
        )
      )
    }
    
    if (map_type == 'ratio') {
      return(
        c(
          'maps',
          'ratio_cbar_title'
        )
      )
    }
    
    NULL
  }
  
  get_colour_key = function(map_type) {
    
    if (
      map_type %in% c(
        'test',
        'ref'
      )
    ) {
      
      return(
        c(
          'maps',
          'testref_colours'
        )
      )
    }
    
    if (map_type == 'absdiff') {
      
      return(
        c(
          'maps',
          'absdiff_colours'
        )
      )
    }
    
    if (map_type == 'reldiff') {
      
      return(
        c(
          'maps',
          'reldiff_colours'
        )
      )
    }
    
    if (map_type == 'ratio') {
      
      return(
        c(
          'maps',
          'ratio_colours'
        )
      )
    }
    
    NULL
  }
  
  get_palette_spec = function(
    map_type,
    var_for_breaks
  ) {
    
    explicit_colours = switch(
      map_type,
      test = testref_colours,
      ref = testref_colours,
      absdiff = absdiff_colours,
      reldiff = reldiff_colours,
      ratio = ratio_colours
    )
    
    if (!is.null(explicit_colours)) {
      return(explicit_colours)
    }
    
    colour_key = switch(
      map_type,
      test = 'testref_colours',
      ref = 'testref_colours',
      absdiff = 'absdiff_colours',
      reldiff = 'reldiff_colours',
      ratio = 'ratio_colours'
    )
    
    vp(
      var = var_for_breaks,
      key = c(
        'maps',
        colour_key
      ),
      default = NULL
    )
  }
  
  get_palette_path = function(map_type) {
    
    switch(
      map_type,
      test = testref_pal_pth,
      ref = testref_pal_pth,
      absdiff = absdiff_pal_pth,
      reldiff = reldiff_pal_pth,
      ratio = ratio_pal_pth
    )
  }
  
  get_default_hcl = function(map_type) {
    
    if (
      map_type %in% c(
        'test',
        'ref',
        'ratio'
      )
    ) {
      return('Viridis')
    }
    
    'Blue-Red'
  }
  
  get_total_stars_cells = function(x) {
    prod(dim(x))
  }
  
  get_finite_stars_cells = function(x) {
    
    value_name = names(x)[1]
    
    vals = as.vector(
      x[[value_name]]
    )
    
    sum(
      is.finite(vals)
    )
  }
  
  get_map_values = function(x) {
    
    if (length(x) == 0) {
      return(numeric())
    }
    
    x %>%
      purrr::map(
        function(map_obj) {
          
          value_name = names(map_obj)[1]
          
          as.numeric(
            map_obj[[value_name]]
          )
        }
      ) %>%
      unlist(
        use.names = FALSE
      )
  }
  
  get_plot_value_range_key = function(map_type) {
    
    if (
      map_type %in% c(
        'test',
        'ref'
      )
    ) {
      
      return(
        c(
          'maps',
          'testref_value_range'
        )
      )
    }
    
    if (map_type == 'absdiff') {
      
      return(
        c(
          'maps',
          'absdiff_value_range'
        )
      )
    }
    
    if (map_type == 'reldiff') {
      
      return(
        c(
          'maps',
          'reldiff_value_range'
        )
      )
    }
    
    if (map_type == 'ratio') {
      
      return(
        c(
          'maps',
          'ratio_value_range'
        )
      )
    }
    
    NULL
  }
  
  resolve_plot_value_range = function(
    map_type,
    var_for_breaks
  ) {
    
    explicit_range = switch(
      map_type,
      test = testref_plot_value_range,
      ref = testref_plot_value_range,
      absdiff = absdiff_plot_value_range,
      reldiff = reldiff_plot_value_range,
      ratio = ratio_plot_value_range
    )
    
    if (!is.null(explicit_range)) {
      return(explicit_range)
    }
    
    range_key = switch(
      map_type,
      test = 'testref_value_range',
      ref = 'testref_value_range',
      absdiff = 'absdiff_value_range',
      reldiff = 'reldiff_value_range',
      ratio = 'ratio_value_range'
    )
    
    if (
      is.null(var_params_list) ||
      is.null(range_key)
    ) {
      
      return(
        c(
          -Inf,
          Inf
        )
      )
    }
    
    get_var_param(
      var = var_for_breaks,
      key = c(
        'maps',
        range_key
      ),
      var_params_list = var_params_list,
      default = c(
        -Inf,
        Inf
      )
    )
  }
  
  get_hover_key = function(map_type) {
    
    c(
      'maps',
      paste0(
        map_type,
        '_hover_values'
      )
    )
  }
  
  resolve_hover_values = function(
    map_type,
    var_for_breaks
  ) {
    
    # Global user-input override by map type.
    #
    # NULL for a map type means fall back to the
    # variable-specific setting.
    if (is.list(hover_values)) {
      
      if (
        map_type %in% names(hover_values)
      ) {
        
        this_override =
          hover_values[[map_type]]
        
        if (!is.null(this_override)) {
          return(this_override)
        }
      }
      
    } else if (!is.null(hover_values)) {
      
      # Retain support for a single global override
      # applying to all map types.
      return(hover_values)
    }
    
    # Fall back to variable-specific setting.
    vp(
      var = var_for_breaks,
      key = get_hover_key(
        map_type
      ),
      default = FALSE
    )
  }
  
  format_hover_condition = function(condition) {
    
    if (
      isFALSE(condition) ||
      is.null(condition)
    ) {
      return(NULL)
    }
    
    if (isTRUE(condition)) {
      return('all values')
    }
    
    condition = condition %>%
      stringr::str_remove_all(
        '\\s+'
      ) %>%
      stringr::str_to_lower()
    
    percentile_match =
      stringr::str_match(
        condition,
        '^(<=|>=|<|>)?p([0-9]+(?:\\.[0-9]+)?)$'
      )
    
    if (!is.na(
      percentile_match[1, 1]
    )) {
      
      operator =
        percentile_match[1, 2]
      
      percentile =
        percentile_match[1, 3]
      
      if (
        is.na(operator) ||
        operator == ''
      ) {
        
        operator = if (
          as.numeric(percentile) <= 50
        ) {
          '<='
        } else {
          '>='
        }
      }
      
      operator_lab = dplyr::case_when(
        operator == '<=' ~ '\u2264',
        operator == '>=' ~ '\u2265',
        TRUE ~ operator
      )
      
      return(
        paste0(
          operator_lab,
          ' p',
          percentile
        )
      )
    }
    
    value_match =
      stringr::str_match(
        condition,
        '^(<=|>=|<|>)([-+]?(?:[0-9]*\\.?[0-9]+)(?:e[-+]?[0-9]+)?)$'
      )
    
    if (!is.na(
      value_match[1, 1]
    )) {
      
      operator =
        value_match[1, 2]
      
      threshold =
        value_match[1, 3]
      
      operator_lab = dplyr::case_when(
        operator == '<=' ~ '\u2264',
        operator == '>=' ~ '\u2265',
        TRUE ~ operator
      )
      
      return(
        paste0(
          operator_lab,
          ' ',
          threshold
        )
      )
    }
    
    condition
  }
  
  make_hover_group_name = function(
    this_hover_values
  ) {
    
    if (isTRUE(this_hover_values)) {
      return('Native grid values: all')
    }
    
    if (
      isFALSE(this_hover_values) ||
      is.null(this_hover_values)
    ) {
      return('Native grid values')
    }
    
    hover_labs =
      purrr::map_chr(
        this_hover_values,
        format_hover_condition
      )
    
    paste0(
      'Native grid values: ',
      paste(
        hover_labs,
        collapse = ' or '
      )
    )
  }
  
  select_hover_values = function(
    values,
    hover_values
  ) {
    
    finite = is.finite(values)
    
    if (
      isFALSE(hover_values) ||
      is.null(hover_values)
    ) {
      
      return(
        rep(
          FALSE,
          length(values)
        )
      )
    }
    
    if (isTRUE(hover_values)) {
      return(finite)
    }
    
    if (!is.character(hover_values)) {
      
      stop(
        "hover_values must be FALSE, TRUE, or a character vector ",
        "such as c('<p5', 'p95') or c('>=5', '<=10')."
      )
    }
    
    finite_values = values[
      finite
    ]
    
    if (length(finite_values) == 0) {
      
      return(
        rep(
          FALSE,
          length(values)
        )
      )
    }
    
    keep = rep(
      FALSE,
      length(values)
    )
    
    for (condition in hover_values) {
      
      condition = condition %>%
        stringr::str_remove_all(
          '\\s+'
        ) %>%
        stringr::str_to_lower()
      
      
      # Percentile condition
      
      percentile_match =
        stringr::str_match(
          condition,
          '^(<=|>=|<|>)?p([0-9]+(?:\\.[0-9]+)?)$'
        )
      
      if (!is.na(
        percentile_match[1, 1]
      )) {
        
        operator =
          percentile_match[1, 2]
        
        percentile = as.numeric(
          percentile_match[1, 3]
        )
        
        if (
          percentile < 0 ||
          percentile > 100
        ) {
          
          stop(
            "Percentiles in hover_values must be between p0 and p100."
          )
        }
        
        if (
          is.na(operator) ||
          operator == ''
        ) {
          
          operator = if (
            percentile <= 50
          ) {
            '<='
          } else {
            '>='
          }
        }
        
        threshold = as.numeric(
          stats::quantile(
            finite_values,
            probs = percentile / 100,
            na.rm = TRUE,
            names = FALSE
          )
        )
        
      } else {
        
        
        # Absolute-value condition
        
        value_match =
          stringr::str_match(
            condition,
            '^(<=|>=|<|>)([-+]?(?:[0-9]*\\.?[0-9]+)(?:e[-+]?[0-9]+)?)$'
          )
        
        if (is.na(
          value_match[1, 1]
        )) {
          
          stop(
            "Could not interpret hover_values condition '",
            condition,
            "'. Use expressions such as '<p5', 'p95', '>=5', or '<=10'."
          )
        }
        
        operator =
          value_match[1, 2]
        
        threshold = as.numeric(
          value_match[1, 3]
        )
      }
      
      this_keep = switch(
        operator,
        '<' = values < threshold,
        '<=' = values <= threshold,
        '>' = values > threshold,
        '>=' = values >= threshold
      )
      
      this_keep[
        !finite
      ] = FALSE
      
      keep = keep | this_keep
    }
    
    keep
  }
  
  format_hover_value = function(
    x,
    digits = 2
  ) {
    
    format(
      round(
        x,
        digits = digits
      ),
      trim = TRUE,
      scientific = FALSE,
      nsmall = 0
    )
  }
  
  build_hover_sf = function(
    x,
    value_name,
    hover_values,
    plot_value_range = c(-Inf, Inf),
    digits = 2
  ) {
    
    pts = as.data.frame(
      x,
      xy = TRUE,
      na.rm = FALSE
    )
    
    if (!nrow(pts)) {
      return(NULL)
    }
    
    if (
      all(
        c(
          'x',
          'y'
        ) %in%
        names(pts)
      )
    ) {
      
      coord_cols = c(
        'x',
        'y'
      )
      
    } else if (
      all(
        c(
          'lon',
          'lat'
        ) %in%
        names(pts)
      )
    ) {
      
      coord_cols = c(
        'lon',
        'lat'
      )
      
    } else {
      
      return(NULL)
    }
    
    values =
      pts[[value_name]]
    
    in_plot_range =
      is.finite(values) &
      values >=
      plot_value_range[1] &
      values <=
      plot_value_range[2]
    
    hover_values_input =
      values
    
    hover_values_input[
      !in_plot_range
    ] = NA
    
    in_hover_range =
      select_hover_values(
        values = hover_values_input,
        hover_values = hover_values
      )
    
    pts = pts[
      in_plot_range &
        in_hover_range,
      ,
      drop = FALSE
    ]
    
    if (!nrow(pts)) {
      return(NULL)
    }
    
    pts = pts %>%
      dplyr::mutate(
        hover_value =
          format_hover_value(
            .data[[value_name]],
            digits = digits
          )
      )
    
    pts_sf = sf::st_as_sf(
      pts,
      coords = coord_cols,
      crs = sf::st_crs(x),
      remove = FALSE
    )
    
    if (
      !isTRUE(
        sf::st_is_longlat(
          pts_sf
        )
      )
    ) {
      
      pts_sf = sf::st_transform(
        pts_sf,
        4326
      )
    }
    
    pts_sf
  }
  
  
  # Breaks and label helpers
  
  summary_map_default_scale_cut = c(0, 'k' = 1e3, 'M' = 1e6, 'G' = 1e9, 'T' = 1e12)
  
  resolve_summary_leaflet_breaks = function(
    breaks,
    var,
    key,
    x
  ) {
    
    x = as.numeric(x)
    
    x = x[
      is.finite(x)
    ]
    
    data_min = if (
      length(x) > 0
    ) {
      
      min(
        x,
        na.rm = TRUE
      )
      
    } else {
      
      NA_real_
    }
    
    data_max = if (
      length(x) > 0
    ) {
      
      max(
        x,
        na.rm = TRUE
      )
      
    } else {
      
      NA_real_
    }
    
    if (!is.null(var_params_list)) {
      
      return(
        resolve_map_breaks(
          breaks = breaks,
          var = var,
          key = key,
          var_params_list = var_params_list,
          data_min = data_min,
          data_max = data_max
        )
      )
    }
    
    if (is.null(breaks)) {
      
      key_lab = paste(
        key,
        collapse = '$'
      )
      
      stop(
        "No breaks supplied for ",
        key_lab,
        ". Either provide `",
        key_lab,
        "` through var_params_list or pass explicit breaks."
      )
    }
    
    extend_map_breaks_to_data_range(
      breaks = breaks,
      data_min = data_min,
      data_max = data_max
    )
  }
  
  make_summary_leaflet_formatter = function(
    var,
    break_key,
    breaks
  ) {
    
    finite_breaks = breaks[
      is.finite(breaks)
    ]
    
    default_accuracy = if (
      length(finite_breaks) > 0
    ) {
      
      10^(
        -max(
          decimal_count(
            finite_breaks
          ),
          na.rm = TRUE
        )
      )
      
    } else {
      
      NULL
    }
    
    if (!is.null(var_params_list)) {
      
      return(
        make_map_label_formatter(
          var = var,
          break_key = break_key,
          var_params_list = var_params_list,
          default_scale_cut = summary_map_default_scale_cut,
          default_accuracy = default_accuracy,
          drop0trailing = TRUE
        )
      )
    }
    
    scales::label_number(
      accuracy = default_accuracy,
      scale_cut = summary_map_default_scale_cut,
      drop0trailing = TRUE
    )
  }
  
  format_leaflet_var_label = function(
    var,
    include_units = TRUE
  ) {
    
    if (!is.null(var_params_list)) {
      
      return(
        format_var_label_emep(
          var = var,
          var_params_list = var_params_list,
          context = 'maps',
          output = 'html',
          include_units = include_units
        )
      )
    }
    
    lab_out = format_lab_for_html(
      var
    )
    
    if (
      !isTRUE(include_units) ||
      is.null(var_unit) ||
      var_unit == ''
    ) {
      return(lab_out)
    }
    
    units_out = format_units_for_html(
      var_unit
    )
    
    glue::glue(
      '{lab_out} ({units_out})'
    )
  }
  
  get_var_unit = function(var) {
    
    if (!is.null(var_params_list)) {
      
      return(
        get_var_param(
          var = var,
          key = 'units',
          var_params_list = var_params_list,
          default = ''
        )
      )
    }
    
    var_unit %||%
      ''
  }
  
  make_leaflet_bin_lab_format = function(
    formatter
  ) {
    
    function(
    type,
    cuts,
    p
    ) {
      
      if (type == 'bin') {
        
        n_cuts = length(cuts)
        
        if (n_cuts < 2) {
          
          return(
            formatter(cuts)
          )
        }
        
        lower_labs = formatter(
          cuts[-n_cuts]
        )
        
        upper_labs = formatter(
          cuts[-1]
        )
        
        return(
          paste0(
            lower_labs,
            ' – ',
            upper_labs
          )
        )
      }
      
      formatter(cuts)
    }
  }
  
  make_summary_leaflet_legend_title = function(
    map_type
  ) {
    
    test_lab_no_units =
      format_leaflet_var_label(
        var = var_test,
        include_units = FALSE
      )
    
    ref_lab_no_units =
      format_leaflet_var_label(
        var = var_ref,
        include_units = FALSE
      )
    
    test_unit =
      get_var_unit(
        var_test
      )
    
    ref_unit =
      get_var_unit(
        var_ref
      )
    
    test_unit_html = if (
      !is.null(test_unit) &&
      !is.na(test_unit) &&
      test_unit != ''
    ) {
      
      format_units_for_html(
        test_unit
      )
      
    } else {
      
      ''
    }
    
    ref_unit_html = if (
      !is.null(ref_unit) &&
      !is.na(ref_unit) &&
      ref_unit != ''
    ) {
      
      format_units_for_html(
        ref_unit
      )
      
    } else {
      
      ''
    }
    
    test_lab = if (
      nzchar(test_unit_html)
    ) {
      
      glue::glue(
        '{test_lab_no_units} ({test_unit_html})'
      )
      
    } else {
      
      test_lab_no_units
    }
    
    ref_lab = if (
      nzchar(ref_unit_html)
    ) {
      
      glue::glue(
        '{ref_lab_no_units} ({ref_unit_html})'
      )
      
    } else {
      
      ref_lab_no_units
    }
    
    if (map_type == 'test') {
      return(test_lab)
    }
    
    if (map_type == 'ref') {
      return(ref_lab)
    }
    
    if (map_type == 'absdiff') {
      
      return(
        glue::glue(
          '&Delta; {test_lab}'
        )
      )
    }
    
    if (map_type == 'reldiff') {
      
      return(
        glue::glue(
          '&Delta; {test_lab_no_units} (%)'
        )
      )
    }
    
    if (map_type == 'ratio') {
      
      return(
        glue::glue(
          '{test_lab_no_units} : ',
          '{ref_lab_no_units} (%)'
        )
      )
    }
    
    test_lab
  }
  
  make_summary_hover_title = function(
    map_type
  ) {
    
    test_lab =
      format_leaflet_var_label(
        var = var_test,
        include_units = FALSE
      )
    
    ref_lab =
      format_leaflet_var_label(
        var = var_ref,
        include_units = FALSE
      )
    
    if (map_type == 'test') {
      return(test_lab)
    }
    
    if (map_type == 'ref') {
      return(ref_lab)
    }
    
    if (map_type == 'absdiff') {
      
      return(
        glue::glue(
          '{test_lab} difference'
        )
      )
    }
    
    if (map_type == 'reldiff') {
      
      return(
        glue::glue(
          '{test_lab} relative difference'
        )
      )
    }
    
    if (map_type == 'ratio') {
      
      return(
        glue::glue(
          '{test_lab} : {ref_lab}'
        )
      )
    }
    
    test_lab
  }
  
  get_value_suffix = function(
    map_type
  ) {
    
    if (
      map_type %in% c(
        'reldiff',
        'ratio'
      )
    ) {
      return('%')
    }
    
    this_var =
      get_map_var(
        map_type
      )
    
    this_unit =
      get_var_unit(
        this_var
      )
    
    if (
      is.null(this_unit) ||
      is.na(this_unit) ||
      this_unit == ''
    ) {
      return('')
    }
    
    format_units_for_html(
      this_unit
    )
  }
  
  
  # Input checks
  
  map_types = unique(
    normalise_map_type(
      map_types
    )
  )
  
  allowed_types = if (
    comparison_type == 'testref'
  ) {
    
    c(
      'test',
      'ref',
      'absdiff',
      'reldiff'
    )
    
  } else {
    
    c(
      'test',
      'ref',
      'ratio'
    )
  }
  
  bad_types = setdiff(
    map_types,
    allowed_types
  )
  
  if (length(bad_types) > 0) {
    
    stop(
      'Unsupported map type(s) for ',
      comparison_type,
      ' comparison: ',
      paste(
        bad_types,
        collapse = ', '
      ),
      '. Allowed types are: ',
      paste(
        allowed_types,
        collapse = ', '
      ),
      '.'
    )
  }
  
  
  # Extract all available map layers once
  
  map_lists = list(
    test = get_map_list(
      map_type = 'test',
      diff_list = diff_list
    ),
    ref = get_map_list(
      map_type = 'ref',
      diff_list = diff_list
    ),
    absdiff = get_map_list(
      map_type = 'absdiff',
      diff_list = diff_list
    ),
    reldiff = get_map_list(
      map_type = 'reldiff',
      diff_list = diff_list
    ),
    ratio = get_map_list(
      map_type = 'ratio',
      diff_list = diff_list
    )
  )
  
  if (length(map_lists$test) == 0) {
    
    stop(
      "No data were found for the first map variable."
    )
  }
  
  
  # Use the first Test/numerator domain
  # to set the initial map extent.
  
  domain_base =
    map_lists$test[[1]]
  
  domain_base_4326 = if (
    isTRUE(
      sf::st_is_longlat(
        domain_base
      )
    )
  ) {
    
    domain_base
    
  } else {
    
    sf::st_transform(
      domain_base,
      4326
    )
  }
  
  domain_bbox =
    sf::st_bbox(
      domain_base_4326
    )
  
  
  # Data ranges
  
  test_values =
    get_map_values(
      map_lists$test
    )
  
  ref_values =
    get_map_values(
      map_lists$ref
    )
  
  absdiff_values =
    get_map_values(
      map_lists$absdiff
    )
  
  reldiff_values =
    get_map_values(
      map_lists$reldiff
    )
  
  ratio_values =
    get_map_values(
      map_lists$ratio
    )
  
  
  # Ordinary Test and Reference maps share
  # one common scale. Ratio numerator and
  # denominator maps use their own scales.
  
  testref_values = if (
    comparison_type == 'testref'
  ) {
    
    c(
      test_values,
      ref_values
    )
    
  } else {
    
    numeric()
  }
  
  get_values_for_breaks = function(
    map_type
  ) {
    
    if (
      comparison_type == 'testref' &&
      map_type %in% c(
        'test',
        'ref'
      )
    ) {
      return(testref_values)
    }
    
    if (map_type == 'test') {
      return(test_values)
    }
    
    if (map_type == 'ref') {
      return(ref_values)
    }
    
    if (map_type == 'absdiff') {
      return(absdiff_values)
    }
    
    if (map_type == 'reldiff') {
      return(reldiff_values)
    }
    
    if (map_type == 'ratio') {
      return(ratio_values)
    }
    
    numeric()
  }
  
  
  # Display model as points or raster
  
  display =
    match.arg(
      display
    )
  
  
  # Basemaps
  
  basemap_providers = list(
    world_topo = leaflet::providers$Esri.WorldTopoMap,
    dark = leaflet::providers$Stadia.AlidadeSmoothDark,
    grey = leaflet::providers$Esri.WorldGrayCanvas,
    satellite = leaflet::providers$Esri.WorldImagery,
    terrain = leaflet::providers$Esri.WorldTerrain
  )
  
  invalid_basemaps =
    setdiff(
      basemap,
      names(basemap_providers)
    )
  
  if (length(invalid_basemaps) > 0) {
    
    stop(
      "Unknown basemap(s): ",
      paste(
        invalid_basemaps,
        collapse = ', '
      )
    )
  }
  
  selected_basemaps =
    basemap_providers[
      basemap
    ]
  
  
  # Build maps
  
  out = purrr::map(
    map_types,
    function(map_type) {
      
      this_maps =
        map_lists[[map_type]]
      
      if (length(this_maps) == 0) {
        
        warning(
          "Map type '",
          map_type,
          "' is not available for variable '",
          paste(
            var_nm,
            collapse = ' vs '
          ),
          "'."
        )
        
        return(NULL)
      }
      
      n_cells = this_maps %>%
        purrr::map_dbl(
          get_total_stars_cells
        ) %>%
        sum()
      
      if (n_cells > max_cells) {
        
        stop(
          "Interactive map for variable '",
          paste(
            var_nm,
            collapse = ' vs '
          ),
          "' and type '",
          map_type,
          "' has ",
          format(
            n_cells,
            big.mark = ','
          ),
          " cells across all domains, which exceeds max_cells = ",
          format(
            max_cells,
            big.mark = ','
          ),
          "."
        )
      }
      
      this_break_key =
        get_break_key(
          map_type
        )
      
      this_breaks_raw =
        get_breaks_arg(
          map_type
        )
      
      var_for_breaks =
        get_var_for_breaks(
          map_type
        )
      
      breaks_labs = vp(
        var = var_for_breaks,
        key = get_break_labs_key(
          map_type
        ),
        default = NULL
      )
      
      cbar_title = vp(
        var = var_for_breaks,
        key = get_cbar_title_key(
          map_type
        ),
        default = NULL
      )
      
      this_plot_value_range =
        resolve_plot_value_range(
          map_type = map_type,
          var_for_breaks = var_for_breaks
        )
      
      this_hover_values =
        resolve_hover_values(
          map_type = map_type,
          var_for_breaks = var_for_breaks
        )
      
      values_for_breaks =
        get_values_for_breaks(
          map_type
        )
      
      this_breaks =
        resolve_summary_leaflet_breaks(
          breaks = this_breaks_raw,
          var = var_for_breaks,
          key = this_break_key,
          x = values_for_breaks
        )
      
      this_formatter =
        make_summary_leaflet_formatter(
          var = var_for_breaks,
          break_key = this_break_key,
          breaks = this_breaks
        )
      
      cols =
        get_palette_spec(
          map_type = map_type,
          var_for_breaks = var_for_breaks
        )
      
      if (!is.null(cols)) {
        
        expected_n_colours =
          length(this_breaks) - 1
        
        if (
          length(cols) !=
          expected_n_colours
        ) {
          
          stop(
            glue::glue(
              'maps${switch(
          map_type,
          test = "testref_colours",
          ref = "testref_colours",
          absdiff = "absdiff_colours",
          reldiff = "reldiff_colours",
          ratio = "ratio_colours"
        )} must have exactly {expected_n_colours} colours to match ',
              'length(this_breaks) = {length(this_breaks)}.'
            )
          )
        }
        
      } else {
        
        cols = resolve_palette(
          pal_spec = get_palette_path(
            map_type
          ),
          n = length(this_breaks) - 1,
          default_hcl = get_default_hcl(
            map_type
          )
        )
      }
      
      pal = leaflet::colorBin(
        palette = cols,
        domain = values_for_breaks,
        bins = this_breaks,
        na.color = 'transparent',
        right = FALSE
      )
      
      
      # Initialise map
      
      m = leaflet::leaflet(
        options =
          leaflet::leafletOptions(
            zoomControl = TRUE,
            minZoom = 2
          ),
        width = '100%',
        height = height
      )
      
      for (
        basemap_nm in
        names(selected_basemaps)
      ) {
        
        m = m %>%
          leaflet::addProviderTiles(
            provider =
              selected_basemaps[[basemap_nm]],
            group =
              basemap_nm
          )
      }
      
      # Show the first supplied basemap by default
      
      if (length(selected_basemaps) > 1) {
        
        m = m %>%
          leaflet::hideGroup(
            names(selected_basemaps)[-1]
          )
      }
      
      
      # Add model domains in supplied order.
      #
      # An inner high-resolution domain added
      # after an outer domain therefore
      # overplots the outer domain.
      
      native_maps = list()
      
      for (
        domain in
        names(this_maps)
      ) {
        
        this_map =
          this_maps[[domain]]
        
        if (
          length(
            names(this_map)
          ) == 0
        ) {
          
          stop(
            "Could not determine attribute name for the stars object ",
            "for domain '",
            domain,
            "'."
          )
        }
        
        native_maps[[domain]] =
          this_map
        
        display_map =
          filter_map_values(
            this_map,
            plot_value_range =
              this_plot_value_range
          )
        
        if (display == 'raster') {
          
          m = m %>%
            leafem::addStarsImage(
              display_map,
              colors = pal,
              opacity = opacity,
              project = TRUE,
              method = 'near',
              group = map_type,
              layerId = paste0(
                map_type,
                '_',
                domain
              ),
              autozoom = FALSE
            )
          
        } else {
          
          native_points = display_map %>%
            sf::st_as_sf(
              as_points = TRUE,
              na.rm = TRUE
            ) %>%
            sf::st_transform(
              4326
            )
          
          value_col =
            names(display_map)[1]
          
          native_points$plot_value =
            native_points[[value_col]]
          
          m = m %>%
            leaflet::addCircleMarkers(
              data = native_points,
              radius = 2,
              stroke = FALSE,
              fillColor =
                ~pal(plot_value),
              fillOpacity = 1,
              group = map_type,
              layerId = paste0(
                map_type,
                '_',
                domain,
                '_',
                seq_len(
                  nrow(native_points)
                )
              ),
              options =
                leaflet::pathOptions(
                  className =
                    'native-value-point'
                )
            )
        }
      }
      
      
      # Set map extent
      
      m = m %>%
        leaflet::fitBounds(
          lng1 =
            domain_bbox[['xmin']],
          lat1 =
            domain_bbox[['ymin']],
          lng2 =
            domain_bbox[['xmax']],
          lat2 =
            domain_bbox[['ymax']]
        )
      
      
      # Legend
      
      if (
        isTRUE(
          show_leaflet_legend
        )
      ) {
        
        cbar_title_key = switch(
          map_type,
          test = 'testref_cbar_title',
          ref = 'testref_cbar_title',
          absdiff = 'absdiff_cbar_title',
          reldiff = 'reldiff_cbar_title',
          ratio = 'ratio_cbar_title'
        )
        
        legend_title = vp(
          var = var_for_breaks,
          key = c(
            'maps',
            cbar_title_key
          ),
          default = NULL
        )
        
        breaks_labs_key = switch(
          map_type,
          test = 'testref_breaks_labs',
          ref = 'testref_breaks_labs',
          absdiff = 'absdiff_breaks_labs',
          reldiff = 'reldiff_breaks_labs',
          ratio = 'ratio_breaks_labs'
        )
        
        breaks_labs = vp(
          var = var_for_breaks,
          key = c(
            'maps',
            breaks_labs_key
          ),
          default = NULL
        )
        
        if (is.null(legend_title)) {
          
          legend_title =
            make_summary_leaflet_legend_title(
              map_type = map_type
            )
        }
        
        legend_title = format_plotmath_for_html(
          legend_title
        )
        
        legend_alpha = if (
          display == 'points'
        ) {
          
          1
          
        } else {
          
          opacity
        }
        
        legend_html =
          make_leaflet_bin_legend_html(
            title = legend_title,
            breaks = this_breaks,
            colours = cols,
            formatter = this_formatter,
            breaks_labs = breaks_labs,
            alpha = legend_alpha,
            font_size = 10,
            title_size = 11,
            swatch_width = 13,
            swatch_height = 11,
            line_height = 12
          )
        
        if (!is.null(
          legend_html
        )) {
          
          m = m %>%
            leaflet::addControl(
              html = legend_html,
              position = 'topright',
              className =
                'summary-map-legend'
            )
        }
      }
      
      # Native-grid hover values
      
      native_grid_values_added =
        FALSE
      
      hover_group =
        make_hover_group_name(
          this_hover_values
        )
      
      if (
        !isFALSE(this_hover_values) &&
        !is.null(this_hover_values)
      ) {
        
        hover_sf_list =
          purrr::imap(
            native_maps,
            function(
    native_map,
    domain
            ) {
              
              value_name =
                names(native_map)[1]
              
              build_hover_sf(
                x = native_map,
                value_name =
                  value_name,
                hover_values =
                  this_hover_values,
                plot_value_range =
                  this_plot_value_range,
                digits = digits
              )
            }
          ) %>%
          purrr::compact()
        
        n_hover_points =
          hover_sf_list %>%
          purrr::map_dbl(
            nrow
          ) %>%
          sum()
        
        if (
          n_hover_points >
          hover_max_points
        ) {
          
          warning(
            "Skipping hover values for variable '",
            paste(
              var_nm,
              collapse = ' vs '
            ),
            "' and type '",
            map_type,
            "' because ",
            format(
              n_hover_points,
              big.mark = ','
            ),
            " cells satisfy hover_values, which exceeds hover_max_points = ",
            format(
              hover_max_points,
              big.mark = ','
            ),
            "."
          )
          
        } else if (
          n_hover_points > 0
        ) {
          
          value_suffix =
            get_value_suffix(
              map_type
            )
          
          for (
            domain in
            names(hover_sf_list)
          ) {
            
            hover_sf =
              hover_sf_list[[domain]]
            
            hover_sf = hover_sf %>%
              dplyr::mutate(
                label_text =
                  paste0(
                    '<b>',
                    make_summary_hover_title(
                      map_type =
                        map_type
                    ),
                    '</b><br/>',
                    'Value: ',
                    hover_value,
                    if (
                      nzchar(
                        value_suffix
                      )
                    ) {
                      
                      paste0(
                        ' ',
                        value_suffix
                      )
                      
                    } else {
                      
                      ''
                    }
                  )
              )
            
            m = m %>%
              leaflet::addCircleMarkers(
                data = hover_sf,
                radius = hover_radius,
                stroke = TRUE,
                weight = 1,
                color = hover_colour,
                fillOpacity = 0,
                opacity = 1,
                label = ~lapply(
                  label_text,
                  htmltools::HTML
                ),
                group = hover_group
              )
          }
          
          native_grid_values_added =
            TRUE
          
          m = m %>%
            leaflet::hideGroup(
              hover_group
            )
        }
      }
      
      
      # Basemap and overlay controls
      
      overlay_groups =
        map_type
      
      if (native_grid_values_added) {
        
        overlay_groups = c(
          overlay_groups,
          hover_group
        )
      }
      
      m = m %>%
        leaflet::addLayersControl(
          baseGroups =
            names(selected_basemaps),
          overlayGroups =
            overlay_groups,
          position = 'bottomleft',
          options =
            leaflet::layersControlOptions(
              collapsed = TRUE
            )
        ) %>%
        htmlwidgets::prependContent(
          htmltools::tags$style(
            htmltools::HTML(
              '
              .leaflet-bottom.leaflet-left .leaflet-control-layers {
                margin-bottom: 35px;
              }

              .leaflet-control-layers-overlays label:first-child {
                display: none;
              }
              '
            )
          )
        )
      
      if (display == 'points') {
        
        m = m %>%
          htmlwidgets::onRender(
            '
            function(el, x) {

              var map = this;

              function updateNativePointRadius() {

                var zoom = map.getZoom();

                var radius;

                if (zoom <= 5) {
                  radius = 1;
                } else if (zoom == 6) {
                  radius = 2;
                } else if (zoom == 7) {
                  radius = 3;
                } else {
                  radius = 4;
                }

                map.eachLayer(function(layer) {

                  if (
                    layer.options &&
                    layer.options.className === "native-value-point" &&
                    typeof layer.setRadius === "function"
                  ) {
                    layer.setRadius(radius);
                  }
                });
              }

              updateNativePointRadius();

              map.on(
                "zoomend",
                updateNativePointRadius
              );
            }
            '
          )
      }
      
      m
    }
  )
  
  names(out) =
    map_types
  
  out = purrr::discard(
    out,
    is.null
  )
  
  out
}

plot_temporal_profiles = function(
    temporal_data,
    var_params_list,
    run_labels,
    var_labels = NULL,
    palette = NULL,
    value_label = 'Value',
    plot_type = c('test', 'ref', 'absdiff', 'reldiff', 'ratio'),
    value_mode = c('value', 'fraction'),
    group_var = 'variable',
    region_label = NULL,
    year = NULL,
    interactive = TRUE
) {
  
  plot_type = match.arg(
    plot_type
  )
  
  value_mode = match.arg(
    value_mode
  )
  
  if (
    is.null(temporal_data) ||
    nrow(temporal_data) == 0
  ) {
    return(NULL)
  }
  
  if (length(run_labels) != 2) {
    stop(
      'run_labels must contain exactly two labels: test and reference.',
      call. = FALSE
    )
  }
  
  test_label = run_labels[[1]]
  ref_label = run_labels[[2]]
  
  if (!group_var %in% names(temporal_data)) {
    stop(
      'Temporal plot grouping column not found: ',
      group_var,
      call. = FALSE
    )
  }
  
  if (
    plot_type %in% c(
      'absdiff',
      'reldiff',
      'ratio'
    ) &&
    value_mode == 'fraction'
  ) {
    stop(
      "Temporal comparison plots cannot use value_mode = 'fraction'.",
      call. = FALSE
    )
  }
  
  if (plot_type %in% c('test', 'ref')) {
    
    if (!'run' %in% names(temporal_data)) {
      stop(
        'Temporal test/reference plots require a run column.',
        call. = FALSE
      )
    }
    
    plot_data = temporal_data %>%
      dplyr::filter(
        run == plot_type
      ) %>%
      dplyr::mutate(
        plot_value = value
      )
    
  } else {
    
    if (!plot_type %in% names(temporal_data)) {
      stop(
        'Temporal comparison column not found: ',
        plot_type,
        call. = FALSE
      )
    }
    
    plot_data = temporal_data %>%
      dplyr::mutate(
        plot_value = .data[[plot_type]]
      )
  }
  
  if (nrow(plot_data) == 0) {
    return(NULL)
  }
  
  plot_data = plot_data %>%
    dplyr::mutate(
      variable_label = purrr::map_chr(
        variable,
        function(var_name) {
          
          if (
            !is.null(var_labels) &&
            var_name %in% names(var_labels)
          ) {
            
            var_lab = unname(
              var_labels[[var_name]]
            )
            
          } else {
            
            var_lab = format_var_label_emep(
              var = var_name,
              var_params_list = var_params_list,
              context = 'temporal',
              output = 'plain'
            )
          }
          
          if (stringr::str_starts(var_name, 'D3_')) {
            
            z_index = get_var_param(
              var = var_name,
              key = 'z_index',
              var_params_list = var_params_list,
              default = NULL
            )
            
            if (!is.null(z_index)) {
              var_lab = paste0(
                var_lab,
                ' (sigma level ',
                z_index,
                ')'
              )
            }
          }
          
          var_lab
        }
      ),
      group_id = as.character(
        .data[[group_var]]
      )
    )
  
  if (group_var == 'variable') {
    
    plot_data = plot_data %>%
      dplyr::mutate(
        group_label = variable_label
      )
    
  } else {
    
    plot_data = plot_data %>%
      dplyr::mutate(
        group_label = group_id
      )
  }
  
  if (
    all(
      c(
        'summary_type',
        'quantity_type'
      ) %in% names(plot_data)
    )
  ) {
    
    plot_data = plot_data %>%
      dplyr::mutate(
        value_descriptor = dplyr::case_when(
          summary_type == 'mean' ~ paste(
            'Mean',
            stringr::str_to_lower(quantity_type)
          ),
          summary_type == 'total' ~ paste(
            'Total',
            stringr::str_to_lower(quantity_type)
          ),
          summary_type == 'point' ~ quantity_type,
          TRUE ~ quantity_type
        )
      )
    
  } else {
    
    plot_data = plot_data %>%
      dplyr::mutate(
        value_descriptor = value_label
      )
  }
  
  if (value_mode == 'fraction') {
    
    plot_data = plot_data %>%
      dplyr::group_by(
        group_id
      ) %>%
      dplyr::mutate(
        period_fraction = 100 * plot_value / sum(
          plot_value,
          na.rm = TRUE
        ),
        plot_value = period_fraction
      ) %>%
      dplyr::ungroup()
    
  } else {
    
    plot_data = plot_data %>%
      dplyr::mutate(
        period_fraction = NA_real_
      )
  }
  
  group_info = plot_data %>%
    dplyr::distinct(
      group_id,
      group_label
    )
  
  n_groups = nrow(
    group_info
  )
  
  fallback_colours = resolve_palette(
    pal_spec = NULL,
    n = n_groups
  )
  
  if (
    !is.null(palette) &&
    !is.null(names(palette)) &&
    all(group_info$group_id %in% names(palette))
  ) {
    
    plot_colours = unname(
      palette[
        group_info$group_id
      ]
    )
    
  } else if (!is.null(palette)) {
    
    plot_colours = resolve_palette(
      pal_spec = palette,
      n = n_groups
    )
    
  } else if (group_var == 'variable') {
    
    plot_colours = purrr::map2_chr(
      group_info$group_id,
      fallback_colours,
      function(var_name, fallback_colour) {
        
        test_colour = get_var_param(
          var = var_name,
          key = c(
            'temporal',
            'test_colour'
          ),
          var_params_list = var_params_list,
          default = NULL
        )
        
        if (
          is.null(test_colour) ||
          is.na(test_colour) ||
          test_colour == ''
        ) {
          test_colour = fallback_colour
        }
        
        if (plot_type == 'ref') {
          
          ref_colour = get_var_param(
            var = var_name,
            key = c(
              'temporal',
              'ref_colour'
            ),
            var_params_list = var_params_list,
            default = NULL
          )
          
          if (
            is.null(ref_colour) ||
            is.na(ref_colour) ||
            ref_colour == ''
          ) {
            test_colour
          } else {
            ref_colour
          }
          
        } else {
          
          test_colour
        }
      }
    )
    
  } else {
    
    plot_colours = fallback_colours
  }
  
  plot_colours = purrr::set_names(
    plot_colours,
    group_info$group_label
  )
  
  if (group_var == 'variable') {
    
    plot_linewidths = purrr::map_dbl(
      group_info$group_id,
      function(var_name) {
        
        test_linewidth = get_var_param(
          var = var_name,
          key = c(
            'temporal',
            'test_linewidth'
          ),
          var_params_list = var_params_list,
          default = NULL
        )
        
        if (is.null(test_linewidth)) {
          test_linewidth = 0.5
        }
        
        if (plot_type == 'ref') {
          
          ref_linewidth = get_var_param(
            var = var_name,
            key = c(
              'temporal',
              'ref_linewidth'
            ),
            var_params_list = var_params_list,
            default = NULL
          )
          
          if (is.null(ref_linewidth)) {
            test_linewidth
          } else {
            ref_linewidth
          }
          
        } else {
          
          test_linewidth
        }
      }
    )
    
    plot_linetypes = purrr::map_chr(
      group_info$group_id,
      function(var_name) {
        
        test_linetype = get_var_param(
          var = var_name,
          key = c(
            'temporal',
            'test_linetype'
          ),
          var_params_list = var_params_list,
          default = NULL
        )
        
        if (
          is.null(test_linetype) ||
          is.na(test_linetype) ||
          test_linetype == ''
        ) {
          test_linetype = 'solid'
        }
        
        if (plot_type == 'ref') {
          
          ref_linetype = get_var_param(
            var = var_name,
            key = c(
              'temporal',
              'ref_linetype'
            ),
            var_params_list = var_params_list,
            default = NULL
          )
          
          if (
            is.null(ref_linetype) ||
            is.na(ref_linetype) ||
            ref_linetype == ''
          ) {
            test_linetype
          } else {
            ref_linetype
          }
          
        } else {
          
          test_linetype
        }
      }
    )
    
  } else {
    
    plot_linewidths = rep(
      0.5,
      n_groups
    )
    
    plot_linetypes = rep(
      'solid',
      n_groups
    )
  }
  
  plot_linewidths = stats::setNames(
    plot_linewidths,
    group_info$group_label
  )
  
  plot_linetypes = stats::setNames(
    plot_linetypes,
    group_info$group_label
  )
  
  plot_descriptors = plot_data %>%
    dplyr::filter(
      !is.na(value_descriptor),
      value_descriptor != ''
    ) %>%
    dplyr::distinct(
      value_descriptor
    ) %>%
    dplyr::pull(
      value_descriptor
    )
  
  plot_value_label = if (length(plot_descriptors) == 1) {
    plot_descriptors[[1]]
  } else {
    value_label
  }
  
  if (value_mode == 'fraction') {
    
    y_label = 'Share of period total (%)'
    
    plot_data = plot_data %>%
      dplyr::mutate(
        unit_html = purrr::map_chr(
          unit,
          format_units_for_html
        ),
        hover_text = glue::glue(
          '{group_label}<br>',
          'Variable: {variable_label}<br>',
          'Date: {format(time, "%Y-%m-%d")}<br>',
          '{value_descriptor}: {round(value, 2)} {unit_html}<br>',
          'Share of period total: {round(period_fraction, 1)}%'
        )
      )
    
  } else if (plot_type == 'reldiff') {
    
    y_label = glue::glue(
      '{test_label} - {ref_label} (%)'
    )
    
    plot_data = plot_data %>%
      dplyr::mutate(
        hover_text = glue::glue(
          '{group_label}<br>',
          'Variable: {variable_label}<br>',
          'Period: {comparison_period}<br>',
          'Relative difference: {round(plot_value, 2)}%'
        )
      )
    
  } else if (plot_type == 'ratio') {
    
    y_label = glue::glue(
      '{test_label} / {ref_label} ratio'
    )
    
    plot_data = plot_data %>%
      dplyr::mutate(
        hover_text = glue::glue(
          '{group_label}<br>',
          'Variable: {variable_label}<br>',
          'Period: {comparison_period}<br>',
          'Ratio: {round(plot_value, 3)}'
        )
      )
    
  } else {
    
    plot_units = plot_data %>%
      dplyr::filter(
        !is.na(unit),
        unit != ''
      ) %>%
      dplyr::distinct(
        unit
      ) %>%
      dplyr::pull(
        unit
      )
    
    if (plot_type == 'absdiff') {
      
      y_label = if (length(plot_units) == 1) {
        
        glue::glue(
          '{test_label} - {ref_label} ',
          '({format_units_for_plain(plot_units[[1]])})'
        )
        
      } else {
        
        glue::glue(
          '{test_label} - {ref_label}'
        )
      }
      
    } else {
      
      y_label = if (length(plot_units) == 1) {
        
        glue::glue(
          '{plot_value_label} ',
          '({format_units_for_plain(plot_units[[1]])})'
        )
        
      } else {
        
        plot_value_label
      }
    }
    
    plot_data = plot_data %>%
      dplyr::mutate(
        unit_html = purrr::map_chr(
          unit,
          format_units_for_html
        )
      )
    
    if (plot_type == 'absdiff') {
      
      plot_data = plot_data %>%
        dplyr::mutate(
          hover_text = glue::glue(
            '{group_label}<br>',
            'Variable: {variable_label}<br>',
            'Period: {comparison_period}<br>',
            'Absolute difference in ',
            '{stringr::str_to_lower(value_descriptor)}: ',
            '{round(plot_value, 2)} {unit_html}'
          )
        )
      
    } else {
      
      run_label = if (plot_type == 'test') {
        test_label
      } else {
        ref_label
      }
      
      plot_data = plot_data %>%
        dplyr::mutate(
          hover_text = glue::glue(
            '{group_label}<br>',
            'Variable: {variable_label}<br>',
            'Run: {run_label}<br>',
            'Date: {format(time, "%Y-%m-%d")}<br>',
            '{value_descriptor}: ',
            '{round(plot_value, 2)} {unit_html}'
          )
        )
    }
  }
  
  comparison_note = NULL
  
  if (
    plot_type %in% c(
      'absdiff',
      'reldiff',
      'ratio'
    )
  ) {
    
    test_years = unique(
      lubridate::year(
        plot_data$time_test
      )
    )
    
    ref_years = unique(
      lubridate::year(
        plot_data$time_ref
      )
    )
    
    test_years = test_years[
      !is.na(test_years)
    ]
    
    ref_years = ref_years[
      !is.na(ref_years)
    ]
    
    if (
      length(test_years) == 1 &&
      length(ref_years) == 1 &&
      test_years != ref_years
    ) {
      comparison_note = glue::glue(
        '{ref_label} year: {ref_years}'
      )
    }
  }
  
  if (plot_type %in% c('test', 'ref')) {
    
    x_var = 'time'
    
  } else {
    
    x_var = 'time_test'
  }
  
  plot_type_title = dplyr::recode(
    plot_type,
    test = test_label,
    ref = ref_label,
    absdiff = 'Absolute difference',
    reldiff = 'Relative difference',
    ratio = 'Ratio'
  )
  
  p = ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[[x_var]],
      y = plot_value,
      colour = group_label,
      linewidth = group_label,
      linetype = group_label,
      group = group_label,
      text = hover_text
    )
  ) +
    ggplot2::geom_line()
  
  if (plot_type %in% c('absdiff', 'reldiff')) {
    
    p = p +
      ggplot2::geom_hline(
        yintercept = 0,
        linewidth = 0.4
      )
    
  } else if (plot_type == 'ratio') {
    
    p = p +
      ggplot2::geom_hline(
        yintercept = 1,
        linewidth = 0.4
      )
  }
  
  p = p +
    ggplot2::scale_colour_manual(
      values = plot_colours
    ) +
    ggplot2::scale_linewidth_manual(
      values = plot_linewidths,
      guide = 'none'
    ) +
    ggplot2::scale_linetype_manual(
      values = plot_linetypes,
      guide = 'none'
    ) +
    ggplot2::scale_x_datetime(
      expand = ggplot2::expansion(
        mult = c(0.01, 0.03)
      )
    ) +
    ggplot2::labs(
      title = plot_type_title,
      x = NULL,
      y = y_label,
      colour = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = 'bottom',
      plot.margin = ggplot2::margin(
        t = 5.5,
        r = 15,
        b = 5.5,
        l = 5.5
      )
    )
  
  if (!interactive) {
    return(p)
  }
  
  p = plotly::ggplotly(
    p,
    tooltip = 'text',
    height = 300
  ) %>%
    plotly::layout(
      hovermode = FALSE,
      legend = list(
        orientation = 'h',
        x = 0.5,
        xanchor = 'center',
        y = -0.2,
        yanchor = 'top'
      ),
      margin = list(
        b = 90
      )
    )
  
  if (!is.null(comparison_note)) {
    
    p = plotly::layout(
      p,
      annotations = list(
        list(
          text = comparison_note,
          x = 1,
          y = 1,
          xref = 'paper',
          yref = 'paper',
          xanchor = 'right',
          yanchor = 'bottom',
          showarrow = FALSE,
          font = list(
            size = 11
          )
        )
      )
    )
  }
  
  export_title = NULL
  
  if (
    !is.null(region_label) &&
    !is.null(year)
  ) {
    
    export_title = glue::glue(
      '{format_lab_for_plain(region_label)} — {year}'
    )
    
  } else if (!is.null(region_label)) {
    
    export_title = format_lab_for_plain(
      region_label
    )
  }
  
  p = add_plotly_interactive_controls(
    p = p,
    plot_title = export_title,
    export_width = 1200,
    export_height = 600
  )
  
  p
}

prepare_map_geo = function(
    geo,
    domain_base
) {
  
  domain_crs = sf::st_crs(domain_base)
  domain_bbox = sf::st_bbox(domain_base)
  
  geo = sf::st_transform(
    geo,
    domain_crs
  )
  
  if (
    sf::st_is_longlat(domain_crs) &&
    domain_bbox[['xmax']] > 180
  ) {
    
    geo = geo %>%
      sf::st_break_antimeridian(
        lon_0 = 180
      ) %>%
      sf::st_shift_longitude()
  }
  
  geo
}

prepare_temporal_comparison_data = function(
    temporal_data
) {
  
  if (
    is.null(temporal_data) ||
    nrow(temporal_data) == 0
  ) {
    return(
      tibble::tibble()
    )
  }
  
  runs_available = unique(
    temporal_data$run
  )
  
  if (
    !all(
      c('test', 'ref') %in% runs_available
    )
  ) {
    return(
      tibble::tibble()
    )
  }
  
  temporal_data %>%
    dplyr::group_by(
      resolution
    ) %>%
    dplyr::mutate(
      comparison_period = get_temporal_comparison_period(
        time = time,
        resolution = resolution
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      dplyr::any_of(
        c(
          'domain',
          'domain_name',
          'location_id',
          'location_type',
          'resolution',
          'comparison_period',
          'variable',
          'unit'
        )
      ),
      time,
      run,
      value
    ) %>%
    tidyr::pivot_wider(
      names_from = run,
      values_from = c(
        time,
        value
      )
    ) %>%
    dplyr::filter(
      !is.na(value_test),
      !is.na(value_ref)
    ) %>%
    dplyr::mutate(
      absdiff = value_test - value_ref,
      reldiff = dplyr::if_else(
        value_ref == 0,
        NA_real_,
        100 * (value_test - value_ref) / value_ref
      ),
      ratio = dplyr::if_else(
        value_ref == 0,
        NA_real_,
        value_test / value_ref
      )
    )
}

prepare_temporal_emission_difference_data = function(emiss_data) {
  
  emiss_data %>%
    prepare_temporal_emission_plot_data() %>%
    dplyr::select(
      resolution,
      region,
      calendar_month,
      month_label,
      variable,
      run,
      value
    ) %>%
    tidyr::pivot_wider(
      names_from = run,
      values_from = value
    ) %>%
    dplyr::filter(
      !is.na(test),
      !is.na(ref)
    ) %>%
    dplyr::mutate(
      rel_diff = dplyr::if_else(
        ref == 0,
        NA_real_,
        100 * (test - ref) / ref
      )
    )
}

prepare_temporal_emission_plot_data = function(emiss_data) {
  
  emiss_data %>%
    dplyr::mutate(
      calendar_month = lubridate::month(time),
      month_label = factor(
        month.abb[calendar_month],
        levels = month.abb
      )
    )
}

read_domain_filter = function(path,
                              filter_name = 'domain filter') {
  
  if (is.null(path) || is.na(path) || path == '') {
    return(NULL)
  }
  
  if (!fs::file_exists(path)) {
    stop(
      stringr::str_to_sentence(filter_name),
      ' file not found: ',
      path
    )
  }
  
  sf::st_read(
    path,
    quiet = TRUE
  )
}

read_emep = function(emep_fname, emep_var, emep_crs, proxy = TRUE,
                     x_index = NULL, y_index = NULL,
                     z_index = NULL, time_index = NULL,
                     driver = c('gdal', 'ncdf'),
                     suppress_gdal_warnings = TRUE) {
  # reads one or more EMEP variables from a NetCDF file and returns them as a
  # single stars object where possible
  #
  # variables are first read lazily so that optional slicing can be applied
  # before data are realised in memory
  #
  # driver:
  #   'gdal' uses stars::read_stars(sub = ...) and is generally much faster
  #   than ncdf
  #   'ncdf' uses stars::read_ncdf(var = ...)
  #
  # when driver = 'gdal', z_index is applied only to variables containing a
  # 'lev' or 'ilev' dimension. For these variables, values are read directly
  # from the NetCDF file using an ncdf4 hyperslab. This avoids a stars/GDAL
  # proxy problem with slicing multidimensional variables containing a
  # vertical dimension. GDAL is still used to obtain the spatial grid
  # geometry.
  #
  # the hyperslab route reads only the requested vertical level(s), and only
  # the requested time step(s) when time_index is also supplied. This avoids
  # materialising all vertical levels from large EMEP files.
  #
  # if z_index is supplied for a variable without a vertical dimension, it is
  # ignored and the normal reader is used
  #
  # if multiple variables are requested and they have identical dimensions,
  # they are combined into one stars object
  # if dimensions differ, a named list of stars objects is returned instead
  #
  # emep_crs is assigned to the output because the EMEP CRS cannot be
  # reliably determined by stars when reading the NetCDF file
  #
  # emep_fname:
  #   path to the EMEP NetCDF file
  #
  # emep_var:
  #   character vector of EMEP variable names to read
  #
  # proxy:
  #   logical; if TRUE, keep the result as a proxy object where possible
  #   the z_index hyperslab route necessarily returns a realised stars object
  #
  # x_index, y_index, z_index, time_index:
  #   optional integer indices used to subset the corresponding dimensions
  #   if NULL, the full dimension is retained
  #
  # output:
  #   - combined stars object if all requested variables have identical dimensions
  #   - named list of stars objects if dimensions differ
  #   - NULL if no variables could be read
  
  driver = match.arg(
    driver
  )
  
  if (is.null(emep_fname) || is.na(emep_fname)) {
    return(NULL)
  }
  
  if (length(emep_var) == 0) {
    stop(
      "No variables supplied to read_emep()."
    )
  }
  
  
  reverse_array_dim = function(x, dim_pos) {
    
    idx = purrr::map(
      dim(x),
      seq_len
    )
    
    idx[[dim_pos]] = rev(
      idx[[dim_pos]]
    )
    
    do.call(
      '[',
      c(
        list(x),
        idx,
        list(
          drop = FALSE
        )
      )
    )
  }
  
  
  read_one_var = function(var_name) {

    # Read the GDAL proxy once.
    #
    # For ordinary variables this is the object used by the standard
    # GDAL route. For variables with a vertical dimension it supplies
    # the spatial grid geometry for the ncdf4 hyperslab route.
    
    emep_proxy = NULL
    
    if (driver == 'gdal') {
      
      emep_proxy = tryCatch(
        {
          if (suppress_gdal_warnings) {
            
            suppressWarnings(
              stars::read_stars(
                emep_fname,
                sub = var_name,
                proxy = TRUE,
                quiet = TRUE
              )
            )
            
          } else {
            
            stars::read_stars(
              emep_fname,
              sub = var_name,
              proxy = TRUE,
              quiet = TRUE
            )
          }
        },
        error = function(e) {
          
          msg = glue::glue(
            "read_stars failed for variable '{var_name}' in file '{emep_fname}': {e$message}"
          )
          
          logger::log_error(
            msg
          )
          
          stop(
            msg
          )
        }
      )
    }
    
    
    proxy_dims = if (!is.null(emep_proxy)) {
      
      names(
        dim(emep_proxy)
      )
      
    } else {
      
      character()
    }
    
    
    has_vertical_dim = any(
      c(
        'lev',
        'ilev'
      ) %in% proxy_dims
    )

    # Special route for variables where a vertical level is selected.
    #
    # stars/GDAL can correctly describe the multidimensional proxy,
    # but fails to materialise a selected vertical level correctly.
    # Read the requested hyperslab with ncdf4 and use GDAL only for
    # the grid geometry.
    #
    # A global z_index can therefore safely be supplied: variables
    # without lev/ilev simply continue through the standard route.
    
    if (
      driver == 'gdal' &&
      !is.null(z_index) &&
      has_vertical_dim
    ) {
      
      nc = ncdf4::nc_open(
        emep_fname
      )
      
      on.exit(
        ncdf4::nc_close(
          nc
        ),
        add = TRUE
      )
      
      
      if (is.null(nc$var[[var_name]])) {
        stop(
          glue::glue(
            "Variable '{var_name}' was not found in '{emep_fname}'."
          )
        )
      }
      
      
      var_dims = nc$var[[var_name]]$dim
      
      dim_names = purrr::map_chr(
        var_dims,
        'name'
      )
      
      dim_lengths = purrr::map_int(
        var_dims,
        'len'
      )
      
      
      x_dim = if ('i' %in% dim_names) {
        'i'
      } else if ('lon' %in% dim_names) {
        'lon'
      } else {
        NA_character_
      }
      
      y_dim = if ('j' %in% dim_names) {
        'j'
      } else if ('lat' %in% dim_names) {
        'lat'
      } else {
        NA_character_
      }
      
      lev_dim = if ('lev' %in% dim_names) {
        'lev'
      } else if ('ilev' %in% dim_names) {
        'ilev'
      } else {
        NA_character_
      }
      
      
      requested_index = stats::setNames(
        vector(
          'list',
          length(dim_names)
        ),
        dim_names
      )
      
      
      if (!is.na(x_dim)) {
        requested_index[[x_dim]] = x_index
      }
      
      if (!is.na(y_dim)) {
        requested_index[[y_dim]] = y_index
      }
      
      requested_index[[lev_dim]] = z_index
      
      if ('time' %in% dim_names) {
        requested_index[['time']] = time_index
      }
      
      
      # Construct the smallest contiguous hyperslab containing the
      # requested indices.
      
      start = rep(
        1L,
        length(dim_names)
      )
      
      count = rep(
        -1L,
        length(dim_names)
      )
      
      local_index = vector(
        'list',
        length(dim_names)
      )
      
      
      for (i in seq_along(dim_names)) {
        
        this_dim = dim_names[[i]]
        this_index = requested_index[[this_dim]]
        
        if (is.null(this_index)) {
          
          local_index[[i]] = seq_len(
            dim_lengths[[i]]
          )
          
        } else {
          
          this_index = as.integer(
            this_index
          )
          
          if (
            anyNA(this_index) ||
            any(this_index < 1L) ||
            any(this_index > dim_lengths[[i]])
          ) {
            
            stop(
              glue::glue(
                "Invalid index supplied for dimension '{this_dim}' ",
                "of variable '{var_name}'. Valid indices are ",
                "1:{dim_lengths[[i]]}."
              )
            )
          }
          
          start[[i]] = min(
            this_index
          )
          
          count[[i]] =
            max(this_index) -
            min(this_index) +
            1L
          
          local_index[[i]] =
            this_index -
            start[[i]] +
            1L
        }
      }
      
      
      values = ncdf4::ncvar_get(
        nc,
        var_name,
        start = start,
        count = count,
        collapse_degen = FALSE
      )
      
      
      # Apply the exact requested indices. This allows non-contiguous
      # index vectors while keeping the NetCDF read itself contiguous.
      
      values = do.call(
        '[',
        c(
          list(values),
          local_index,
          list(
            drop = FALSE
          )
        )
      )

      # Obtain the corresponding GDAL dimensions for the requested
      # spatial subset. Only proxy metadata are used here.
      
      gdal_dims = stars::st_dimensions(
        emep_proxy
      )
      
      
      if (
        !is.null(x_index) &&
        'x' %in% names(gdal_dims)
      ) {
        
        x_index = as.integer(
          x_index
        )
        
        gdal_dims[['x']]$from = 1L
        gdal_dims[['x']]$to = length(
          x_index
        )
        
        gdal_dims[['x']]$offset =
          gdal_dims[['x']]$offset +
          (min(x_index) - 1L) *
          gdal_dims[['x']]$delta
      }
      
      
      if (
        !is.null(y_index) &&
        'y' %in% names(gdal_dims)
      ) {
        
        y_index = as.integer(
          y_index
        )
        
        gdal_dims[['y']]$from = 1L
        gdal_dims[['y']]$to = length(
          y_index
        )
        
        gdal_dims[['y']]$offset =
          gdal_dims[['y']]$offset +
          (min(y_index) - 1L) *
          gdal_dims[['y']]$delta
      }

      # ncdf4 returns the native NetCDF i/j orientation, whereas GDAL
      # may expose one or both spatial axes in the opposite direction.
      #
      # Compare the signs of the native coordinate increments with
      # the GDAL x/y deltas and reverse the values where required.
      
      if (
        !is.na(x_dim) &&
        'x' %in% names(gdal_dims)
      ) {
        
        x_values = ncdf4::ncvar_get(
          nc,
          x_dim
        )
        
        if (length(x_values) > 1) {
          
          flip_x =
            sign(diff(x_values)[1]) !=
            sign(gdal_dims[['x']]$delta)
          
          if (flip_x) {
            
            x_pos = match(
              x_dim,
              dim_names
            )
            
            values = reverse_array_dim(
              values,
              x_pos
            )
          }
        }
      }
      
      
      if (
        !is.na(y_dim) &&
        'y' %in% names(gdal_dims)
      ) {
        
        y_values = ncdf4::ncvar_get(
          nc,
          y_dim
        )
        
        if (length(y_values) > 1) {
          
          flip_y =
            sign(diff(y_values)[1]) !=
            sign(gdal_dims[['y']]$delta)
          
          if (flip_y) {
            
            y_pos = match(
              y_dim,
              dim_names
            )
            
            values = reverse_array_dim(
              values,
              y_pos
            )
          }
        }
      }

      # Drop dimensions that have explicitly been reduced to one
      # selected index. Spatial dimensions are retained.
      
      drop_dims = character()
      
      
      if (
        !is.null(z_index) &&
        length(z_index) == 1
      ) {
        
        drop_dims = c(
          drop_dims,
          lev_dim
        )
      }
      
      
      if (
        !is.null(time_index) &&
        length(time_index) == 1 &&
        'time' %in% dim_names
      ) {
        
        drop_dims = c(
          drop_dims,
          'time'
        )
      }
      
      
      retained_dim_names = setdiff(
        dim_names,
        drop_dims
      )
      
      retained_dim_pos = match(
        retained_dim_names,
        dim_names
      )
      
      retained_dim_lengths = dim(values)[
        retained_dim_pos
      ]
      
      values = array(
        values,
        dim = retained_dim_lengths
      )
      
      
      # Convert native NetCDF spatial dimension names to the
      # corresponding GDAL/stars names.
      
      output_dim_names = retained_dim_names
      
      if (!is.na(x_dim)) {
        
        output_dim_names[
          output_dim_names == x_dim
        ] = 'x'
      }
      
      if (!is.na(y_dim)) {
        
        output_dim_names[
          output_dim_names == y_dim
        ] = 'y'
      }
      
      
      # Retain the GDAL spatial dimensions and any remaining
      # non-spatial dimensions from the original proxy.
      
      output_dims = stars::st_dimensions(
        emep_proxy
      )
      
      output_dims = output_dims[
        output_dim_names
      ]
      
      
      # Preserve the NetCDF variable units where possible.
      
      var_units = ncdf4::ncatt_get(
        nc,
        var_name,
        'units'
      )
      
      if (
        isTRUE(var_units$hasatt) &&
        length(var_units$value) == 1 &&
        !is.na(var_units$value) &&
        nzchar(var_units$value)
      ) {
        
        values = tryCatch(
          units::set_units(
            values,
            var_units$value,
            mode = 'standard'
          ),
          error = function(e) {
            values
          }
        )
      }
      
      
      emep_data = stars::st_as_stars(
        stats::setNames(
          list(values),
          var_name
        ),
        dimensions = output_dims
      )
      
      emep_data = sf::st_set_crs(
        emep_data,
        emep_crs
      )
      
      
      return(
        emep_data
      )
    }

    # Standard reader.
    #
    # This is used for ordinary surface variables and whenever z_index
    # is NULL. Therefore a global z_index is harmless for variables
    # without a vertical dimension.
    
    emep_data = if (driver == 'gdal') {
      
      emep_proxy
      
    } else {
      
      tryCatch(
        {
          stars::read_ncdf(
            emep_fname,
            var = var_name,
            proxy = TRUE,
            quiet = TRUE
          )
        },
        error = function(e) {
          
          msg = glue::glue(
            "read_ncdf failed for variable '{var_name}' ",
            "in file '{emep_fname}': {e$message}"
          )
          
          logger::log_error(
            msg
          )
          
          stop(
            msg
          )
        }
      )
    }
    
    
    dims = names(
      dim(emep_data)
    )
    
    dims_len = dim(
      emep_data
    )
    
    
    idx = purrr::map(
      dims,
      function(d) {
        seq_len(
          dims_len[[d]]
        )
      }
    )
    
    names(idx) = dims
    
    
    slice_dim = function(idx, dim_name, index) {
      
      if (
        !is.null(index) &&
        !is.na(dim_name) &&
        dim_name %in% names(idx)
      ) {
        
        idx[[dim_name]] = as.integer(
          index
        )
      }
      
      idx
    }
    
    
    # 'x' and 'y' when using GDAL, 'i/lon' and 'j/lat' when using ncdf
    
    x_dim = if ('x' %in% dims) {
      'x'
    } else if ('i' %in% dims) {
      'i'
    } else if ('lon' %in% dims) {
      'lon'
    } else {
      NA_character_
    }
    
    y_dim = if ('y' %in% dims) {
      'y'
    } else if ('j' %in% dims) {
      'j'
    } else if ('lat' %in% dims) {
      'lat'
    } else {
      NA_character_
    }
    
    lev_dim = if ('lev' %in% dims) {
      'lev'
    } else if ('ilev' %in% dims) {
      'ilev'
    } else {
      NA_character_
    }
    
    
    idx = slice_dim(
      idx,
      x_dim,
      x_index
    )
    
    idx = slice_dim(
      idx,
      y_dim,
      y_index
    )
    
    # Only apply z_index here when a vertical dimension actually exists.
    
    if (!is.na(lev_dim)) {
      
      idx = slice_dim(
        idx,
        lev_dim,
        z_index
      )
    }
    
    idx = slice_dim(
      idx,
      'time',
      time_index
    )
    
    
    subset_args = c(
      list(emep_data),
      list(
        names(emep_data)
      ),
      unname(
        idx[dims]
      ),
      list(
        drop = FALSE
      )
    )
    
    emep_data = do.call(
      '[',
      subset_args
    )
    
    
    if (!proxy) {
      
      emep_data = if (
        driver == 'gdal' &&
        suppress_gdal_warnings
      ) {
        
        suppressWarnings(
          stars::st_as_stars(
            emep_data,
            curvilinear = NULL
          )
        )
        
      } else {
        
        stars::st_as_stars(
          emep_data,
          curvilinear = NULL
        )
      }
    }
    
    
    emep_data
  }
  
  
  var_list = purrr::map(
    emep_var,
    read_one_var
  )
  
  names(var_list) = emep_var
  
  
  dims_equal = all(
    purrr::map_lgl(
      var_list[-1],
      ~ identical(
        stars::st_dimensions(.x),
        stars::st_dimensions(
          var_list[[1]]
        )
      )
    )
  )
  
  
  if (dims_equal) {
    
    emep_data = do.call(
      c,
      var_list
    )
    
  } else {
    
    warning(
      "Not all variables have identical dimensions. Returning as list instead of combined stars object."
    )
    
    return(
      var_list
    )
  }
  
  
  emep_data = sf::st_set_crs(
    emep_data,
    emep_crs
  )
  
  
  emep_data
}

read_emep_country_inventory_file = function(fpath) {
  # reads an EMEP/WebDab country emission inventory text file
  #
  # expected file structure:
  # - comment/header lines starting with '#'
  # - semicolon-separated data rows with columns:
  #   ISO2;YEAR;SECTOR;POLLUTANT;UNIT;NUMBER/FLAG
  #
  # the function reads all data rows and returns a tidy tibble with the
  # original unit retained
  #
  # fpath:
  #   path to the EMEP/WebDab country inventory text file
  #
  # output:
  #   tibble with columns:
  #   - iso2
  #   - year
  #   - sector
  #   - pollutant
  #   - unit
  #   - value
  
  if (!fs::file_exists(fpath)) {
    stop(
      "EMEP emission inventory file not found: ",
      fpath
    )
  }
  
  readr::read_delim(
    file = fpath,
    delim = ';',
    comment = '#',
    col_names = c('iso2', 'year', 'sector', 'pollutant', 'unit', 'value'),
    col_types = readr::cols(
      iso2 = readr::col_character(),
      year = readr::col_integer(),
      sector = readr::col_character(),
      pollutant = readr::col_character(),
      unit = readr::col_character(),
      value = readr::col_double()
    ),
    trim_ws = TRUE,
    show_col_types = FALSE
  )
}

read_emep_forecast = function(forecast_file,
                              forecast_vars,
                              emep_crs,
                              proxy = FALSE) {
  
  read_emep(
    emep_fname = forecast_file,
    emep_var = forecast_vars,
    emep_crs = emep_crs,
    proxy = proxy
  )
}

read_obs_sites_available = function(
    obs_dirs,
    year
) {
  
  purrr::map_dfr(
    seq_along(obs_dirs),
    function(i) {
      
      obs_dir = obs_dirs[i]
      
      sites_available_pth = fs::path(
        obs_dir,
        paste0(
          'Sites_available_',
          year,
          '.rds'
        )
      )
      
      checkmate::assert_file_exists(
        sites_available_pth
      )
      
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
      
      sites_available %>%
        dplyr::mutate(
          obs_repo = obs_dir,
          obs_priority = i
        )
    }
  )
}

read_processed_naei_inventory_file = function(
    fpath,
    inventory_year = NULL
) {
  
  if (!fs::file_exists(fpath)) {
    stop(
      "Processed NAEI emission inventory file not found: ",
      fpath
    )
  }
  
  out = readr::read_rds(fpath)
  
  required_columns = c(
    'year',
    'pollutant',
    'unit',
    'value'
  )
  
  missing_columns = setdiff(
    required_columns,
    names(out)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns in processed NAEI inventory: ",
      paste(missing_columns, collapse = ', ')
    )
  }
  
  if (!is.null(inventory_year)) {
    out = out %>%
      dplyr::filter(year %in% inventory_year)
  }
  
  out
}

read_processed_obs = function(obs_file) {
  
  obs = readr::read_rds(
    obs_file
  )
  
  if (
    is.list(obs) &&
    'processed_data' %in% names(obs)
  ) {
    
    obs = obs$processed_data
  }
  
  if (!inherits(obs, 'data.frame')) {
    stop(
      'Processed observation data must be a data frame or contain ',
      'a processed_data data frame: ',
      obs_file,
      call. = FALSE
    )
  }
  
  obs
}

recode_site_type = function(site_type) {
  
  site_type = site_type %>%
    stringr::str_to_lower() %>%
    stringr::str_trim()
  
  dplyr::case_when(
    is.na(site_type) | site_type == '' ~ 'Unknown',
    
    site_type %in% c(
      'urban traffic',
      'roadside',
      'kerbside',
      'traffic_urban'
    ) ~ 'Road',
    
    site_type %in% c(
      'urban background',
      'suburban background',
      'urban',
      'urban_background',
      'suburban',
      'urban_centre',
      'background_urban',
      'background_suburban'
    ) ~ 'Urban',
    
    site_type %in% c(
      'rural',
      'rural background',
      'background_rural',
      'background_rural_regional'
    ) ~ 'Rural',
    
    site_type %in% c(
      'suburban industrial',
      'urban industrial',
      'urban_industrial',
      'rural industrial',
      'industrial',
      'industrial_suburban',
      'airport'
    ) ~ 'Industrial',
    
    TRUE ~ 'Unknown'
  ) %>%
    factor(
      levels = c(
        'Urban',
        'Road',
        'Industrial',
        'Rural',
        'Unknown'
      ),
      ordered = TRUE
    )
}

region_list_requires_geodata = function(
    region_list,
    allow_full_domain = FALSE
) {
  
  if (is.null(region_list)) {
    return(FALSE)
  }
  
  selections = region_list[
    !purrr::map_lgl(region_list, is.null)
  ]
  
  if (length(selections) == 0) {
    return(FALSE)
  }
  
  if (!allow_full_domain) {
    return(TRUE)
  }
  
  any(
    purrr::map_lgl(
      selections,
      ~ any(.x != 'full_domain')
    )
  )
}

resolve_emission_source_file = function(
    filename,
    emis_dir,
    run_script,
    config_file = 'config_emep.nml'
) {
  
  literal_fpath = fs::path(
    emis_dir,
    filename
  )
  
  if (fs::file_exists(literal_fpath)) {
    
    return(
      tibble::tibble(
        configured_filename = filename,
        resolved_filename = filename,
        resolved_fpath = literal_fpath
      )
    )
  }
  
  substitutions = get_config_substitutions(
    run_script = run_script,
    config_file = config_file
  )
  
  resolved_filename =
    filename
  
  if (nrow(substitutions) > 0) {
    
    for (i in seq_len(nrow(substitutions))) {
      
      search =
        substitutions$search[i]
      
      replacement =
        resolve_run_script_value(
          value = substitutions$replacement[i],
          run_script = run_script
        )
      
      resolved_filename =
        stringr::str_replace_all(
          resolved_filename,
          stringr::fixed(search),
          replacement
        )
    }
  }
  
  resolved_fpath = fs::path(
    emis_dir,
    resolved_filename
  )
  
  tibble::tibble(
    configured_filename = filename,
    resolved_filename = resolved_filename,
    resolved_fpath = resolved_fpath
  )
}

resolve_map_breaks = function(
    breaks = NULL,
    var,
    key,
    var_params_list,
    data_min,
    data_max
) {
  
  resolved_breaks = breaks %||%
    get_var_param(
      var = var,
      key = key,
      var_params_list = var_params_list,
      default = NULL
    )
  
  if (is.null(resolved_breaks)) {
    
    key_lab = paste(
      key,
      collapse = '$'
    )
    
    stop(
      "No map breaks found for variable '",
      var,
      "' and key '",
      key_lab,
      "'."
    )
  }
  
  extend_map_breaks_to_data_range(
    breaks = resolved_breaks,
    data_min = data_min,
    data_max = data_max
  )
}

resolve_mobs_plot_tzone = function(site_code,
                                   sites_meta,
                                   mobs_tzone = c('utc', 'local'),
                                   timezone_col = 'timezone') {
  
  mobs_tzone = match.arg(tolower(mobs_tzone), choices = c('utc', 'local'))
  
  if (mobs_tzone == 'utc') {
    return('UTC')
  }
  
  if (!timezone_col %in% names(sites_meta)) {
    logger::log_warn(
      glue::glue(
        "mobs_tzone = 'local' but column '{timezone_col}' was not found in sites_meta. ",
        "Falling back to UTC for site {site_code}."
      )
    )
    
    return('UTC')
  }
  
  site_tzone = sites_meta %>%
    dplyr::filter(.data$code == site_code) %>%
    dplyr::pull(.data[[timezone_col]]) %>%
    unique()
  
  site_tzone = site_tzone[!is.na(site_tzone) & site_tzone != '']
  
  if (length(site_tzone) == 0) {
    logger::log_warn(
      glue::glue(
        "No timezone found for site {site_code}. Falling back to UTC."
      )
    )
    
    return('UTC')
  }
  
  if (length(site_tzone) > 1) {
    logger::log_warn(
      glue::glue(
        "Multiple timezones found for site {site_code}: ",
        "{paste(site_tzone, collapse = ', ')}. Falling back to UTC."
      )
    )
    
    return('UTC')
  }
  
  if (!site_tzone %in% OlsonNames()) {
    logger::log_warn(
      glue::glue(
        "Invalid timezone for site {site_code}: {site_tzone}. ",
        "Falling back to UTC."
      )
    )
    
    return('UTC')
  }
  
  site_tzone
}

resolve_modstat_map_breaks = function(stat,
                                      var_nm,
                                      sites_df,
                                      var_params_list) {
  
  var_id = resolve_var_id(
    var = var_nm,
    var_params_list = var_params_list
  )
  
  vp = var_params_list[[var_id]] %||% list()
  
  get_stat_vals = function(stat) {
    if (!stat %in% names(sites_df)) {
      return(numeric())
    }
    
    x = sites_df[[stat]]
    x[is.finite(x)]
  }
  
  make_signed_breaks_from_data = function(x,
                                          n = 8) {
    
    if (length(x) == 0) {
      return(c(-1, 1))
    }
    
    edge = max(abs(x), na.rm = TRUE)
    
    if (!is.finite(edge) || edge == 0) {
      edge = 1
    }
    
    pretty(c(-edge, edge), n = n)
  }
  
  make_positive_breaks_from_data = function(x,
                                            n = 6) {
    
    if (length(x) == 0) {
      return(c(0, 1))
    }
    
    upper = max(x, na.rm = TRUE)
    
    if (!is.finite(upper) || upper == 0) {
      upper = 1
    }
    
    pretty(c(0, upper), n = n)
  }
  
  mb_breaks = vp[['map_mb_breaks']] %||%
    c(seq(-10, -2, 2), seq(2, 10, 2))
  
  nmb_breaks = vp[['map_nmb_breaks']] %||%
    c(
      seq(-100, -20, 20),
      -10,
      -5,
      5,
      10,
      seq(20, 100, 20)
    )
  
  rmse_breaks = vp[['map_rmse_breaks']] %||%
    abs(mb_breaks)
  
  rmse_breaks = sort(
    unique(
      c(
        0,
        rmse_breaks[is.finite(rmse_breaks)]
      )
    )
  )
  
  r_breaks = vp[['map_r_breaks']] %||%
    seq(-1, 1, by = 0.2)
  
  out = switch(
    stat,
    
    MB = mb_breaks,
    
    NMB = nmb_breaks,
    
    RMSE = {
      if (length(rmse_breaks) >= 2) {
        rmse_breaks
      } else {
        make_positive_breaks_from_data(
          get_stat_vals('RMSE')
        )
      }
    },
    
    r = r_breaks,
    r_pearson = r_breaks,
    r_spearman = r_breaks,
    
    FAC2 = c(0, 0.5, 1, 1.5, 2, 3),
    
    p = c(0, 0.01, 0.05, 0.1, 0.5, 1),
    P = c(0, 0.01, 0.05, 0.1, 0.5, 1),
    
    n = make_positive_breaks_from_data(get_stat_vals('n')),
    
    {
      x = get_stat_vals(stat)
      
      if (length(x) == 0) {
        c(0, 1)
      } else if (min(x, na.rm = TRUE) < 0 && max(x, na.rm = TRUE) > 0) {
        make_signed_breaks_from_data(x)
      } else {
        make_positive_breaks_from_data(x)
      }
    }
  )
  
  out = sort(unique(as.numeric(out[is.finite(out)])))
  
  if (length(out) < 2) {
    out = c(0, 1)
  }
  
  x = get_stat_vals(stat)
  
  if (length(x) > 0) {
    
    x_min = min(x, na.rm = TRUE)
    x_max = max(x, na.rm = TRUE)
    
    if (is.finite(x_min) && x_min < out[1]) {
      out[1] = floor(x_min)
    }
    
    if (is.finite(x_max) && x_max > out[length(out)]) {
      out[length(out)] = ceiling(x_max)
    }
  }
  
  out = sort(unique(out))
  
  if (length(out) < 2) {
    out = c(0, 1)
  }
  
  out
}

resolve_mobs_var_id = function(var,
                               var_params_list) {
  
  var_id = resolve_var_id(
    var = var,
    var_params_list = var_params_list
  )
  
  mobs_alias = var_params_list[[var_id]][['mobs_alias']]
  
  if (!is.null(mobs_alias)) {
    return(
      resolve_var_id(
        var = mobs_alias,
        var_params_list = var_params_list
      )
    )
  }
  
  var_id
}

resolve_obs_file_path = function(
    output_pth,
    obs_repo
) {
  
  if (fs::file_exists(output_pth)) {
    return(output_pth)
  }
  
  path_parts = fs::path_split(
    output_pth
  ) %>%
    purrr::pluck(1)
  
  data_index = which(
    path_parts == 'Data'
  )
  
  if (length(data_index) != 1) {
    stop(
      'Could not resolve observation file path because output_pth ',
      'does not contain a unique Data directory: ',
      output_pth,
      call. = FALSE
    )
  }
  
  relative_pth = do.call(
    fs::path,
    as.list(
      path_parts[
        data_index:length(path_parts)
      ]
    )
  )
  
  resolved_pth = fs::path(
    obs_repo,
    relative_pth
  )
  
  if (!fs::file_exists(resolved_pth)) {
    stop(
      'Processed observation file was not found at either:\n  ',
      output_pth,
      '\n  ',
      resolved_pth,
      call. = FALSE
    )
  }
  
  resolved_pth
}

resolve_palette = function(
    pal_spec,
    n,
    default_hcl = 'Viridis'
) {
  
  # pal_spec can be:
  # - file path to NCL rgb
  # - 'pkg::pal'
  # - 'pal'
  # - a function(n) -> colours
  # - a character vector of colours
  
  if (is.null(pal_spec)) {
    return(
      grDevices::hcl.colors(n, default_hcl)
    )
  }
  
  if (
    is.character(pal_spec) &&
    length(pal_spec) > 1
  ) {
    return(
      grDevices::colorRampPalette(pal_spec)(n)
    )
  }
  
  if (is.function(pal_spec)) {
    return(
      pal_spec(n)
    )
  }
  
  pal_spec = as.character(pal_spec)
  
  if (fs::file_exists(pal_spec)) {
    return(
      get_color(
        read_color(pal_spec),
        n = n
      )
    )
  }
  
  pal_key = fs::path_file(pal_spec)
  
  if (
    grepl(
      '^[A-Za-z0-9_.]+::.+$',
      pal_key
    )
  ) {
    pkg = sub('::.*$', '', pal_key)
    pal = sub('^[^:]+::', '', pal_key)
    
  } else {
    pkg = NA_character_
    pal = pal_key
  }
  
  # khroma
  
  if (
    is.na(pkg) ||
    pkg == 'khroma'
  ) {
    
    if (requireNamespace('khroma', quietly = TRUE)) {
      out = tryCatch(
        khroma::colour(pal)(n),
        error = function(e) NULL
      )
      
      if (!is.null(out)) {
        return(out)
      }
    }
    
    if (!is.na(pkg) && pkg == 'khroma') {
      stop(
        "khroma palette not found: ",
        pal
      )
    }
  }
  
  # scico
  
  if (
    is.na(pkg) ||
    pkg == 'scico'
  ) {
    
    if (requireNamespace('scico', quietly = TRUE)) {
      out = tryCatch(
        scico::scico(
          n = n,
          palette = pal
        ),
        error = function(e) NULL
      )
      
      if (!is.null(out)) {
        return(out)
      }
    }
    
    if (!is.na(pkg) && pkg == 'scico') {
      stop(
        "scico palette not found: ",
        pal
      )
    }
  }
  
  # viridisLite
  
  if (
    is.na(pkg) ||
    pkg == 'viridis'
  ) {
    
    if (requireNamespace('viridisLite', quietly = TRUE)) {
      valid_palettes = c(
        'viridis',
        'magma',
        'inferno',
        'plasma',
        'cividis',
        'turbo'
      )
      
      if (pal %in% valid_palettes) {
        return(
          viridisLite::viridis(
            n,
            option = pal
          )
        )
      }
    }
    
    if (!is.na(pkg) && pkg == 'viridis') {
      stop(
        "viridis option not found: ",
        pal
      )
    }
  }
  
  # HCL
  
  if (
    is.na(pkg) ||
    pkg == 'hcl'
  ) {
    
    out = tryCatch(
      grDevices::hcl.colors(n, pal),
      error = function(e) NULL
    )
    
    if (!is.null(out)) {
      return(out)
    }
    
    if (!is.na(pkg) && pkg == 'hcl') {
      stop(
        "hcl.colors palette not found: ",
        pal
      )
    }
  }
  
  # RColorBrewer
  
  if (
    is.na(pkg) ||
    pkg == 'brewer'
  ) {
    
    if (requireNamespace('RColorBrewer', quietly = TRUE)) {
      
      if (
        pal %in%
        rownames(RColorBrewer::brewer.pal.info)
      ) {
        max_n =
          RColorBrewer::brewer.pal.info[
            pal,
            'maxcolors'
          ]
        
        base_cols =
          RColorBrewer::brewer.pal(
            min(
              max_n,
              max(3, min(n, max_n))
            ),
            pal
          )
        
        return(
          grDevices::colorRampPalette(
            base_cols
          )(n)
        )
      }
    }
    
    if (!is.na(pkg) && pkg == 'brewer') {
      stop(
        "RColorBrewer palette not found: ",
        pal
      )
    }
  }
  
  stop(
    "Could not resolve palette spec: '",
    pal_spec,
    "'. Tried file, khroma, scico, viridisLite, ",
    "hcl.colors, RColorBrewer."
  )
}

resolve_run_script_value = function(
    value,
    run_script
) {
  
  var_matches = stringr::str_match_all(
    value,
    '\\$\\{([A-Za-z_][A-Za-z0-9_]*)\\}'
  )[[1]]
  
  if (nrow(var_matches) == 0) {
    return(value)
  }
  
  for (var_name in var_matches[, 2]) {
    
    var_value = get_run_script_var(
      run_script = run_script,
      var_name = var_name
    )
    
    value = stringr::str_replace_all(
      value,
      stringr::fixed(
        paste0(
          '${',
          var_name,
          '}'
        )
      ),
      var_value
    )
  }
  
  value
}

resolve_selection = function(
    selection,
    available,
    input_name = 'selection'
) {
  
  if (is.null(selection)) {
    return(character())
  }
  
  if (!is.character(selection) ||
      length(selection) == 0 ||
      anyNA(selection) ||
      any(selection == '')) {
    
    stop(
      glue::glue(
        "{input_name} must be NULL or a non-empty character vector."
      ),
      call. = FALSE
    )
  }
  
  include_all = 'all' %in% selection
  
  exclusions = selection %>%
    stringr::str_subset('^-') %>%
    stringr::str_remove('^-')
  
  inclusions = selection %>%
    stringr::str_subset('^[^-]') %>%
    setdiff('all')
  
  if (length(exclusions) > 0 &&
      !include_all &&
      length(inclusions) == 0) {
    
    stop(
      glue::glue(
        "{input_name}: exclusions must be used with 'all' or with explicitly included items."
      ),
      call. = FALSE
    )
  }
  
  invalid_inclusions = setdiff(
    inclusions,
    available
  )
  
  invalid_exclusions = setdiff(
    exclusions,
    available
  )
  
  if (length(invalid_inclusions) > 0) {
    stop(
      paste0(
        input_name,
        " contains unavailable requested items:\n  ",
        paste(invalid_inclusions, collapse = '\n  ')
      ),
      call. = FALSE
    )
  }
  
  if (length(invalid_exclusions) > 0) {
    stop(
      paste0(
        input_name,
        " contains unavailable excluded items:\n  ",
        paste(invalid_exclusions, collapse = '\n  ')
      ),
      call. = FALSE
    )
  }
  
  selected = if (include_all) {
    available
  } else {
    inclusions
  }
  
  setdiff(
    selected,
    exclusions
  )
}

resolve_summary_map_vars = function(
    selection,
    test_file,
    var_params_list,
    input_name = 'SUMMARY_MAP_VARS',
    available = NULL
) {
  
  if (is.null(selection)) {
    stop(
      paste0(
        input_name,
        ' cannot be NULL.'
      ),
      call. = FALSE
    )
  }
  
  available_file_vars = get_emep_vars(
    nc_file = test_file,
    var_keywords = NULL
  )
  
  param_vars = names(
    var_params_list
  )
  
  ratio_param_vars = param_vars %>%
    purrr::keep(
      ~ stringr::str_detect(
        .x,
        '_vs_'
      )
    )
  
  testref_param_vars = param_vars %>%
    purrr::keep(
      function(var) {
        !is.null(
          get_var_param(
            var = var,
            key = c(
              'maps',
              'testref_breaks'
            ),
            var_params_list = var_params_list,
            default = NULL
          )
        )
      }
    )
  
  ratio_mappable_param_vars = ratio_param_vars %>%
    purrr::keep(
      function(var) {
        !is.null(
          get_var_param(
            var = var,
            key = c(
              'maps',
              'ratio_breaks'
            ),
            var_params_list = var_params_list,
            default = NULL
          )
        )
      }
    )
  
  
  # Ordinary variables must themselves exist in the EMEP file.
  
  available_testref_vars = available_file_vars %>%
    intersect(
      testref_param_vars
    )
  
  
  # Ratio entries are derived variables. The ratio name itself does not
  # occur in the EMEP file; both variables forming the ratio must occur.
  
  available_ratio_vars = ratio_mappable_param_vars %>%
    purrr::keep(
      function(var) {
        
        ratio_vars = stringr::str_split(
          var,
          '_vs_',
          simplify = TRUE
        )
        
        ncol(ratio_vars) == 2 &&
          all(
            ratio_vars %in%
              available_file_vars
          )
      }
    )
  
  
  # If a narrower set of available variables has been supplied,
  # restrict both ordinary and ratio variables to that set.
  
  if (!is.null(available)) {
    
    available_testref_vars =
      available_testref_vars %>%
      intersect(
        available
      )
    
    available_ratio_vars =
      available_ratio_vars %>%
      intersect(
        available
      )
  }
  
  
  # 'all' is a shortcut for all configured ordinary variables only.
  # Ratio variables must always be explicitly requested.
  
  if (identical(selection, 'all')) {
    
    return(
      resolve_selection(
        selection = selection,
        available = available_testref_vars,
        input_name = input_name
      )
    )
  }
  
  
  # Explicit selections may contain both ordinary and ratio variables.
  
  available_map_vars = c(
    available_testref_vars,
    available_ratio_vars
  ) %>%
    unique()
  
  missing_param_vars = selection %>%
    setdiff(
      param_vars
    )
  
  requested_testref_vars = selection %>%
    setdiff(
      ratio_param_vars
    )
  
  requested_ratio_vars = selection %>%
    intersect(
      ratio_param_vars
    )
  
  missing_file_vars = requested_testref_vars %>%
    setdiff(
      available_file_vars
    )
  
  missing_ratio_file_vars = requested_ratio_vars %>%
    purrr::map(
      ~ stringr::str_split(
        .x,
        '_vs_',
        simplify = TRUE
      )
    ) %>%
    unlist(
      use.names = FALSE
    ) %>%
    setdiff(
      available_file_vars
    ) %>%
    unique()
  
  missing_testref_map_params = requested_testref_vars %>%
    intersect(
      param_vars
    ) %>%
    setdiff(
      testref_param_vars
    )
  
  missing_ratio_map_params = requested_ratio_vars %>%
    setdiff(
      ratio_mappable_param_vars
    )
  
  unavailable_vars = selection %>%
    intersect(
      param_vars
    ) %>%
    setdiff(
      available_map_vars
    )
  
  if (length(missing_param_vars) > 0) {
    stop(
      paste0(
        'The following variables requested in ',
        input_name,
        ' are not defined in EMEP_VAR_PARAMS_LIST:\n  ',
        paste(
          missing_param_vars,
          collapse = '\n  '
        )
      ),
      call. = FALSE
    )
  }
  
  if (length(missing_file_vars) > 0) {
    stop(
      paste0(
        'The following variables requested in ',
        input_name,
        ' are not available in the selected EMEP file:\n  ',
        paste(
          missing_file_vars,
          collapse = '\n  '
        )
      ),
      call. = FALSE
    )
  }
  
  if (length(missing_ratio_file_vars) > 0) {
    stop(
      paste0(
        'The following variables required for ratio maps requested in ',
        input_name,
        ' are not available in the selected EMEP file:\n  ',
        paste(
          missing_ratio_file_vars,
          collapse = '\n  '
        )
      ),
      call. = FALSE
    )
  }
  
  if (length(missing_testref_map_params) > 0) {
    stop(
      paste0(
        'The following variables requested in ',
        input_name,
        ' do not have maps$testref_breaks defined in ',
        'EMEP_VAR_PARAMS_LIST:\n  ',
        paste(
          missing_testref_map_params,
          collapse = '\n  '
        )
      ),
      call. = FALSE
    )
  }
  
  if (length(missing_ratio_map_params) > 0) {
    stop(
      paste0(
        'The following ratio maps requested in ',
        input_name,
        ' do not have maps$ratio_breaks defined in ',
        'EMEP_VAR_PARAMS_LIST:\n  ',
        paste(
          missing_ratio_map_params,
          collapse = '\n  '
        )
      ),
      call. = FALSE
    )
  }
  
  if (length(unavailable_vars) > 0) {
    stop(
      paste0(
        'The following variables requested in ',
        input_name,
        ' are not included in the available map variables:\n  ',
        paste(
          unavailable_vars,
          collapse = '\n  '
        )
      ),
      call. = FALSE
    )
  }
  
  resolve_selection(
    selection = selection,
    available = available_map_vars,
    input_name = input_name
  )
}

resolve_summary_vars = function(
    summary_vars,
    test_file
) {
  
  if (is.null(summary_vars)) {
    stop(
      paste0(
        "SUMMARY_VARS cannot be NULL when COMPARE_SUMMARIES is TRUE. ",
        "Use 'all' to summarise all supported EMEP variables."
      ),
      call. = FALSE
    )
  }
  
  available_summary_vars = get_emep_vars(
    nc_file = test_file,
    var_keywords = NULL
  ) %>%
    stringr::str_subset(
      '^(Emis|DDEP|WDEP|SURF)'
    ) %>%
    setdiff('WDEP_PREC')
  
  resolve_selection(
    selection = summary_vars,
    available = available_summary_vars,
    input_name = 'SUMMARY_VARS'
  )
}

resolve_temporal_points = function(
    points = NULL,
    point_file = NULL,
    location_id_col = NULL
) {
  
  if (
    is.null(points) &&
    is.null(point_file)
  ) {
    return(NULL)
  }
  
  if (
    !is.null(points) &&
    !is.null(point_file)
  ) {
    stop(
      "Supply either 'points' or 'point_file', not both.",
      call. = FALSE
    )
  }
  
  if (!is.null(point_file)) {
    
    if (!fs::file_exists(point_file)) {
      stop(
        glue::glue(
          "Temporal point geofile does not exist: {point_file}"
        ),
        call. = FALSE
      )
    }
    
    points_sf = sf::st_read(
      point_file,
      quiet = TRUE
    )
    
    geom_types = unique(
      as.character(
        sf::st_geometry_type(
          points_sf
        )
      )
    )
    
    if (
      length(geom_types) != 1 ||
      !geom_types %in% 'POINT'
    ) {
      stop(
        "Temporal point geofile must contain POINT geometries only.",
        call. = FALSE
      )
    }
    
    if (!is.null(location_id_col)) {
      
      if (!location_id_col %in% names(points_sf)) {
        stop(
          glue::glue(
            "location_id_col '{location_id_col}' is not present in the point geofile."
          ),
          call. = FALSE
        )
      }
      
      points_sf = points_sf %>%
        dplyr::mutate(
          location_id = as.character(
            .data[[location_id_col]]
          )
        )
      
    } else if ('location_id' %in% names(points_sf)) {
      
      points_sf = points_sf %>%
        dplyr::mutate(
          location_id = as.character(
            location_id
          )
        )
      
    } else {
      
      points_sf = points_sf %>%
        dplyr::mutate(
          location_id = paste0(
            'Point ',
            dplyr::row_number()
          )
        )
    }
    
  } else {
    
    if (!is.list(points)) {
      stop(
        "'points' must be a named list of coordinate vectors.",
        call. = FALSE
      )
    }
    
    if (
      is.null(names(points)) ||
      any(names(points) == '')
    ) {
      stop(
        "All entries in 'points' must have names to use as location_id.",
        call. = FALSE
      )
    }
    
    point_data = purrr::imap_dfr(
      points,
      function(point_coords, point_name) {
        
        if (
          !all(
            c(
              'longitude',
              'latitude'
            ) %in% names(point_coords)
          )
        ) {
          stop(
            glue::glue(
              "Point '{point_name}' must contain named 'longitude' and 'latitude' values."
            ),
            call. = FALSE
          )
        }
        
        tibble::tibble(
          location_id = point_name,
          longitude = as.numeric(
            point_coords[['longitude']]
          ),
          latitude = as.numeric(
            point_coords[['latitude']]
          )
        )
      }
    )
    
    points_sf = sf::st_as_sf(
      point_data,
      coords = c(
        'longitude',
        'latitude'
      ),
      crs = 4326,
      remove = FALSE
    )
  }
  
  if (is.na(sf::st_crs(points_sf))) {
    stop(
      'Temporal point data must have a defined CRS.',
      call. = FALSE
    )
  }
  
  points_sf = sf::st_transform(
    points_sf,
    4326
  )
  
  if (
    anyNA(points_sf$location_id) ||
    any(points_sf$location_id == '')
  ) {
    stop(
      'All temporal points must have a non-empty location_id.',
      call. = FALSE
    )
  }
  
  if (anyDuplicated(points_sf$location_id)) {
    stop(
      'Temporal point location_id values must be unique.',
      call. = FALSE
    )
  }
  
  points_sf %>%
    dplyr::select(
      location_id,
      dplyr::everything()
    )
}

resolve_temporal_regions = function(
    regions = NULL
) {
  
  if (is.null(regions)) {
    
    return(
      list(
        include_full_domain = FALSE,
        region_geofiles = tibble::tibble(
          region = character(),
          fpath = character()
        )
      )
    )
  }
  
  if (!is.character(regions)) {
    stop(
      "'regions' must be a character vector containing 'full_domain' and/or polygon geofile paths.",
      call. = FALSE
    )
  }
  
  include_full_domain =
    'full_domain' %in% regions
  
  region_files = setdiff(
    regions,
    'full_domain'
  )
  
  if (length(region_files) == 0) {
    
    return(
      list(
        include_full_domain = include_full_domain,
        region_geofiles = tibble::tibble(
          region = character(),
          fpath = character()
        )
      )
    )
  }
  
  missing_files = region_files[
    !fs::file_exists(
      region_files
    )
  ]
  
  if (length(missing_files) > 0) {
    stop(
      paste0(
        'The following temporal-summary region geofile(s) do not exist:\n',
        paste(
          missing_files,
          collapse = '\n'
        )
      ),
      call. = FALSE
    )
  }
  
  region_geofiles = purrr::map_dfr(
    region_files,
    function(fpath) {
      
      region_sf = sf::st_read(
        fpath,
        quiet = TRUE
      )
      
      geom_types = unique(
        as.character(
          sf::st_geometry_type(
            region_sf
          )
        )
      )
      
      if (
        any(
          !geom_types %in% c(
            'POLYGON',
            'MULTIPOLYGON'
          )
        )
      ) {
        stop(
          glue::glue(
            "Temporal-summary region geofile must contain POLYGON or MULTIPOLYGON geometries: {fpath}"
          ),
          call. = FALSE
        )
      }
      
      if (nrow(region_sf) == 0) {
        stop(
          glue::glue(
            "Temporal-summary region geofile contains no features: {fpath}"
          ),
          call. = FALSE
        )
      }
      
      if (is.na(sf::st_crs(region_sf))) {
        stop(
          glue::glue(
            "Temporal-summary region geofile has no defined CRS: {fpath}"
          ),
          call. = FALSE
        )
      }
      
      region = fs::path_ext_remove(
        fs::path_file(
          fpath
        )
      )
      
      tibble::tibble(
        region = region,
        fpath = fpath
      )
    }
  )
  
  if (anyDuplicated(region_geofiles$region)) {
    stop(
      'Temporal-summary region names derived from geofile names must be unique.',
      call. = FALSE
    )
  }
  
  list(
    include_full_domain = include_full_domain,
    region_geofiles = region_geofiles
  )
}

resolve_temporal_style = function(
    var,
    key,
    var_params_list,
    common_test = NULL,
    common_ref = NULL,
    default_test = NULL,
    use_var_test = TRUE
) {
  
  var_test = if (isTRUE(use_var_test)) {
    get_var_param(
      var = var,
      key = c(
        'temporal',
        paste0(
          'test_',
          key
        )
      ),
      var_params_list = var_params_list,
      default = NULL
    )
  } else {
    NULL
  }
  
  var_ref = get_var_param(
    var = var,
    key = c(
      'temporal',
      paste0(
        'ref_',
        key
      )
    ),
    var_params_list = var_params_list,
    default = NULL
  )
  
  test_value = if (!is.null(common_test)) {
    common_test
  } else if (!is.null(var_test)) {
    var_test
  } else {
    default_test
  }
  
  ref_value = if (!is.null(common_ref)) {
    common_ref
  } else if (!is.null(var_ref)) {
    var_ref
  } else {
    test_value
  }
  
  list(
    test = test_value,
    ref = ref_value
  )
}

resolve_var_id = function(var,
                          var_params_list) {
  
  var = as.character(var)
  
  if (var %in% names(var_params_list)) {
    return(var)
  }
  
  hits = names(var_params_list)[
    purrr::map_lgl(
      var_params_list,
      ~ var %in% (.x[['aliases']] %||% character())
    )
  ]
  
  if (length(hits) == 1) {
    return(hits)
  }
  
  if (length(hits) > 1) {
    stop(
      "Variable alias '",
      var,
      "' matches multiple entries in var_params_list: ",
      paste(hits, collapse = ', ')
    )
  }
  
  var
}

#from rcolors package
read_color <- function(file) {
  tryCatch({
    d = read.table(file, skip = 1) %>% set_names(c("r", "g", "b"))
    max_value = max(sapply(d, max))
    max_value = ifelse(max_value <= 1, 1, 255)
    # pmax(d$r, d$g, d$b) %>% print()
    colors = with(d, rgb(r, g, b, maxColorValue = max_value))
    colors
  }, error = function(e){
    # print(file)
    message(sprintf("[%s]: %s", basename(file), e$message))
  })
  # colors
}

read_MBS = function(domain_dir) {
  
  budget_file = fs::path(
    domain_dir,
    'MassBudgetSummary.txt'
  )
  
  if (!fs::file_exists(budget_file)) {
    stop(
      glue::glue(
        "MassBudgetSummary.txt not found in '{domain_dir}'."
      ),
      call. = FALSE
    )
  }
  
  read.table(
    budget_file,
    comment.char = '#',
    header = FALSE,
    blank.lines.skip = TRUE,
    col.names = c(
      'n',
      'species',
      'usedMW',
      'emis',
      'ddep',
      'wdep',
      'init',
      'sum_mass',
      'fluxout',
      'fluxin',
      'frac_mass'
    )
  ) %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      species,
      mass = sum_mass,
      unit = 'kg'
    )
}

read_wrf_summary_file = function(pth,
                                 crs) {
  
  is_lonlat_crs = function(crs) {
    crs_txt = as.character(crs)
    stringr::str_detect(crs_txt, '\\+proj=longlat')
  }
  
  x = stars::read_stars(pth)
  
  if (is_lonlat_crs(crs)) {
    if (!all(c('XLAT', 'XLONG') %in% names(x))) {
      stop('Expected XLAT and XLONG for lonlat WRF summary file: ', pth)
    }
    
    var_nm = setdiff(names(x), c('XLAT', 'XLONG'))
    
    if (length(var_nm) != 1) {
      stop(
        'Expected exactly one data variable plus XLAT/XLONG in WRF summary file: ',
        pth
      )
    }
    
    lon_mat = as.array(x[['XLONG']])
    lat_mat = as.array(x[['XLAT']])
    val_arr = as.array(x[[var_nm]])
    
    lon_vals = lon_mat[, 1]
    lat_vals = lat_mat[1, ]
    
    if (length(lon_vals) != dim(val_arr)[1]) {
      lon_vals = lon_mat[1, ]
    }
    
    if (length(lat_vals) != dim(val_arr)[2]) {
      lat_vals = lat_mat[, 1]
    }
    
    x = x[var_nm]
    
    lon_vals = as.numeric(lon_vals)
    lat_vals = as.numeric(lat_vals)
    
    lon_delta = stats::median(diff(lon_vals), na.rm = TRUE)
    lat_delta = stats::median(diff(lat_vals), na.rm = TRUE)
    
    x = stars::st_set_dimensions(
      x,
      which = 'x',
      offset = lon_vals[[1]],
      delta = lon_delta,
      refsys = sf::st_crs(crs),
      point = FALSE,
      names = 'lon'
    )
    
    x = stars::st_set_dimensions(
      x,
      which = 'y',
      offset = lat_vals[[1]],
      delta = lat_delta,
      refsys = sf::st_crs(crs),
      point = FALSE,
      names = 'lat'
    )
    
  } else {
    x = x[setdiff(names(x), c('XLAT', 'XLONG'))]
  }
  
  sf::st_crs(x) = crs
  
  x
}

read_wrf_times = function(wrf_file) {
  nc = ncdf4::nc_open(wrf_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  
  if (!'Times' %in% names(nc$var)) {
    stop("Variable 'Times' not found in WRF file: ", wrf_file)
  }
  
  times_raw = ncdf4::ncvar_get(nc, 'Times')
  
  if (!is.character(times_raw) || length(dim(times_raw)) > 1) {
    stop(
      "Expected WRF 'Times' to be returned as a character vector, but got:\n",
      "  class: ", paste(class(times_raw), collapse = ', '), "\n",
      "  dim: ", paste(dim(times_raw), collapse = ' x '), "\n",
      "File: ", wrf_file
    )
  }
  
  times_chr = as.character(times_raw) %>%
    stringr::str_trim() %>%
    stringr::str_replace('_', ' ')
  
  out = lubridate::ymd_hms(times_chr, tz = 'UTC', quiet = TRUE)
  
  if (all(is.na(out))) {
    stop(
      "Could not parse WRF Times. First few strings were:\n",
      paste(utils::head(times_chr, 5), collapse = '\n')
    )
  }
  
  out
}

select_emep_file_from_dir = function(
    EMEP_dir,
    file_tag = 'fullrun',
    fail_out = c('null', 'na'),
    most_resolved = FALSE,
    least_resolved = FALSE
) {
  
  fail_list = list(
    'null' = NULL,
    'na' = NA_character_
  )
  
  fail_value = fail_list[[match.arg(fail_out)]]
  
  if (is.null(EMEP_dir)) {
    return(fail_value)
  }
  
  if (most_resolved && least_resolved) {
    stop(
      "Only one of 'most_resolved' and 'least_resolved' can be TRUE.",
      call. = FALSE
    )
  }
  
  # List NetCDF files only
  
  nc_files = fs::dir_ls(
    EMEP_dir,
    recurse = FALSE,
    regexp = '\\.nc$'
  )
  
  if (most_resolved || least_resolved) {
    
    resolution = c(
      'hour',
      'day',
      'month',
      'fullrun'
    )
    
    if (least_resolved) {
      resolution = rev(resolution)
    }
    
    for (res in resolution) {
      
      matches = nc_files %>%
        stringr::str_subset(
          paste0('_', res, '\\.nc$')
        )
      
      if (length(matches) > 1) {
        stop(
          paste0(
            "More than one EMEP NetCDF file matched temporal resolution '",
            res,
            "' in directory:\n  ",
            EMEP_dir,
            "\n\nMatched files:\n  ",
            paste(
              fs::path_file(matches),
              collapse = '\n  '
            )
          ),
          call. = FALSE
        )
      }
      
      if (length(matches) == 1) {
        return(matches)
      }
    }
    
    return(fail_value)
  }
  
  matches = nc_files %>%
    stringr::str_subset(
      paste0('_', file_tag, '\\.nc$')
    )
  
  if (length(matches) > 1) {
    stop(
      paste0(
        "More than one EMEP NetCDF file matched file tag '",
        file_tag,
        "' in directory:\n  ",
        EMEP_dir,
        "\n\nMatched files:\n  ",
        paste(
          fs::path_file(matches),
          collapse = '\n  '
        )
      ),
      call. = FALSE
    )
  }
  
  if (length(matches) == 0) {
    return(fail_value)
  }
  
  matches
}

select_emep_summary_vars = function(
    available_vars,
    summary_vars
) {
  
  summariseable_vars = available_vars %>%
    stringr::str_subset(
      '^(Emis|DDEP|WDEP|SURF|D3)'
    ) %>%
    setdiff('WDEP_PREC')
  
  if (identical(summary_vars, 'all')) {
    return(summariseable_vars)
  }
  
  if (length(summary_vars) == 1 &&
      !summary_vars %in% available_vars) {
    
    return(
      summariseable_vars %>%
        stringr::str_subset(summary_vars)
    )
  }
  
  intersect(
    summary_vars,
    summariseable_vars
  )
}

select_wrf_summary_file = function(summary_dir,
                                   domain,
                                   var,
                                   stat,
                                   date_tag = NULL) {
  
  files = fs::dir_ls(summary_dir, regexp = '\\.nc$')
  
  file_names = fs::path_file(files)
  
  keep = rep(TRUE, length(files))
  
  keep = keep & stringr::str_detect(
    file_names,
    stringr::str_c('(^|_)', domain, '(_|$)')
  )
  
  keep = keep & stringr::str_detect(
    file_names,
    stringr::str_c('(^|_)', var, '(_|$)')
  )
  
  keep = keep & stringr::str_detect(
    file_names,
    stringr::str_c('(^|_)', stat, '(_|$)')
  )
  
  if (!is.null(date_tag)) {
    keep = keep & stringr::str_detect(
      file_names,
      stringr::fixed(date_tag)
    )
  }
  
  matches = files[keep]
  
  if (length(matches) == 0) {
    stop(
      'No WRF summary file found for domain = ', domain,
      ', var = ', var,
      ', stat = ', stat,
      if (!is.null(date_tag)) paste0(', date_tag = ', date_tag) else '',
      '\nDirectory: ', summary_dir,
      '\nAvailable files:\n',
      paste(file_names, collapse = '\n')
    )
  }
  
  if (length(matches) > 1) {
    stop(
      'Multiple WRF summary files found for domain = ', domain,
      ', var = ', var,
      ', stat = ', stat,
      if (!is.null(date_tag)) paste0(', date_tag = ', date_tag) else '',
      ':\n',
      paste(matches, collapse = '\n')
    )
  }
  
  matches[[1]]
}

set_parallel_plan = function(workers_requested = workers,
                             max_workers = Inf,
                             prefer_multicore = TRUE) {
  
  n_workers = min(
    workers_requested,
    max_workers,
    parallelly::availableCores(),
    na.rm = TRUE
  )
  
  n_workers = as.integer(max(1, n_workers))
  
  if (platform == 'slurm-login') {
    n_workers = min(2L, n_workers)
    prefer_multicore = FALSE
  }
  
  if (n_workers <= 1) {
    future::plan(future::sequential)
    return(invisible(n_workers))
  }
  
  if (
    platform == 'slurm-compute' &&
    isTRUE(prefer_multicore) &&
    future::supportsMulticore()
  ) {
    future::plan(future::multicore, workers = n_workers)
  } else {
    future::plan(future::multisession, workers = n_workers)
  }
  
  invisible(n_workers)
}

show_cols <- function(colors_list, margin = 8, fontsize = 1, family = NULL) {
  if (!is.list(colors_list)) colors_list = list(colors_list)
  names = names(colors_list)
  n <- length(colors_list)
  if (is.null(names)) {
    names = if (n == 1) {
      margin = 3
      ""
    } else seq_len(n)
  }
  
  # par(mfrow = c(n, 1), mar = rep(0.25, 4))
  height <- 0.05
  old <- par(mfrow = c(n, 1), mar = c(height, 0.25, height, margin), mgp = c(0, 0, 0))
  on.exit(par(old))
  
  suppressWarnings({
    for(i in seq_along(colors_list)) {
      color = colors_list[[i]]
      name  = names[i]
      
      ncol <- length(color)
      barplot(rep(1, ncol),
              yaxt = "n",
              space = c(0, 0), border = NA,
              col = color,
              xaxs = "i"
              # xlim = c(2, ncol)
              # , ylab = name
      )
      if (n > 10) abline(v = ncol * .05, col = "white", lwd = 0.7)
      # , family = "Times"
      title = sprintf(" %s (n = %s)", paste0(rep(" ", 0), name), ncol)
      mtext(title, 4, las = 1, cex = fontsize, adj = 0, family = family) # , family = "Arial"
      # text(-4, 0.5, name, adj = c(0, 0.5))
      # sprintf("\n\n%s", name)
    }
  })
  invisible()
}

summarise_emep_var = function(
    emep_stars,
    area_stars,
    var_name,
    var_unit,
    area_mask_sf = NULL,
    total_out_unit = 'Gg'
) {
  # spatially summarises one EMEP variable
  #
  # totals are calculated for Emis_*, DDEP_* and WDEP_* variables
  # area-weighted means are calculated for SURF_* and D3_* variables
  #
  # if a time dimension is present, the spatial summary is calculated
  # independently for each time step; the function does not aggregate over time
  
  if (!var_name %in% names(emep_stars)) {
    stop(
      glue::glue(
        "Variable '{var_name}' is missing from the supplied EMEP data."
      )
    )
  }
  
  if (!'Area_Grid_km2' %in% names(area_stars)) {
    stop(
      "summarise_emep_var(): 'Area_Grid_km2' is missing from area_stars."
    )
  }
  
  if (stringr::str_detect(var_name, '^(Emis|DDEP|WDEP)')) {
    summary_method = 'total'
  } else if (stringr::str_detect(var_name, '^(SURF|D3)')) {
    summary_method = 'mean'
  } else {
    stop(
      glue::glue(
        "Could not infer summary method for EMEP variable '{var_name}'."
      )
    )
  }
  
  if (!is.null(area_mask_sf)) {
    
    emep_stars = apply_domain_mask(
      stars_object = emep_stars,
      domain_mask = area_mask_sf
    )
    
    area_stars = apply_domain_mask(
      stars_object = area_stars,
      domain_mask = area_mask_sf
    )
  }
  
  values = emep_stars[[var_name]]
  area_km2 = area_stars[['Area_Grid_km2']]
  
  dep_element = get_emep_dep_element(
    var_unit
  )
  
  calc_unit = if (summary_method == 'total') {
    normalise_emep_dep_unit(var_unit)
  } else {
    var_unit
  }
  
  if (summary_method == 'total') {
    
    values = units::set_units(
      values,
      calc_unit,
      mode = 'standard'
    )
  }
  
  summarise_one = function(x, area) {
    
    if (summary_method == 'total') {
      
      total_mass = sum(
        x * area,
        na.rm = TRUE
      )
      
      total_mg = total_mass %>%
        units::set_units('mg') %>%
        units::drop_units()
      
      out_value = convert_mass_from_mg(
        total_mg,
        out_unit = total_out_unit
      )
      
      out_unit = if (is.null(dep_element)) {
        total_out_unit
      } else {
        paste(total_out_unit, dep_element)
      }
      
      return(
        list(
          value = out_value,
          unit = out_unit
        )
      )
    }
    
    mean_value =
      sum(
        x * area,
        na.rm = TRUE
      ) /
      sum(
        area[!is.na(x)],
        na.rm = TRUE
      )
    
    mean_value = units::drop_units(mean_value)
    
    list(
      value = mean_value,
      unit = var_unit
    )
  }
  
  has_time = 'time' %in% names(dim(values))
  
  if (!has_time) {
    
    summary = summarise_one(
      values,
      area_km2
    )
    
    return(
      tibble::tibble(
        variable = var_name,
        value = summary$value,
        unit = summary$unit
      )
    )
  }
  
  time_values = stars::st_get_dimension_values(
    emep_stars,
    'time'
  )
  
  purrr::map_dfr(
    seq_along(time_values),
    function(time_index) {
      
      summary = summarise_one(
        values[, , time_index],
        area_km2[, , time_index]
      )
      
      tibble::tibble(
        time = time_values[time_index],
        variable = var_name,
        value = summary$value,
        unit = summary$unit
      )
    }
  )
}

summarise_mobs = function(
    mobs_lframe,
    var = 'all',
    avg_time = c('hour', 'day', 'month', 'year', 'period'),
    summary_stat = c('mean', 'median', 'max', 'min', 'sum'),
    data_thresh = 75,
    drop_na_mod = TRUE,
    start_date = NULL,
    end_date = NULL,
    native_time = 'hour'
) {
  
  avg_time = match.arg(avg_time)
  summary_stat = match.arg(summary_stat)
  
  
  # Internal helpers
  
  summarise_period_vector = function(x, stat) {
    
    if (all(is.na(x))) {
      return(NA_real_)
    }
    
    switch(
      stat,
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      sum = sum(x, na.rm = TRUE)
    )
  }
  
  
  normalise_start_date = function(x, tz) {
    
    if (is.null(x)) {
      return(NULL)
    }
    
    if (inherits(x, 'Date')) {
      
      return(
        as.POSIXct(
          x,
          tz = tz
        )
      )
    }
    
    as.POSIXct(
      x,
      tz = tz
    )
  }
  
  
  normalise_end_date = function(x, tz) {
    
    if (is.null(x)) {
      return(NULL)
    }
    
    if (inherits(x, 'Date')) {
      
      return(
        list(
          value = as.POSIXct(
            x + 1,
            tz = tz
          ),
          inclusive = FALSE
        )
      )
    }
    
    list(
      value = as.POSIXct(
        x,
        tz = tz
      ),
      inclusive = TRUE
    )
  }
  
  
  # Prepare data
  
  mobs = mobs_lframe
  
  if (var != 'all') {
    
    mobs = mobs %>%
      dplyr::filter(
        var %in% var
      )
  }
  
  if (nrow(mobs) == 0) {
    return(mobs)
  }
  
  has_ref_mod = 'ref_mod' %in% names(mobs) &&
    any(is.finite(mobs$ref_mod))
  
  mobs_tz = lubridate::tz(
    mobs$date
  )
  
  start_date_norm = normalise_start_date(
    start_date,
    tz = mobs_tz
  )
  
  end_date_norm = normalise_end_date(
    end_date,
    tz = mobs_tz
  )
  
  
  # Restrict requested period
  
  if (!is.null(start_date_norm)) {
    
    mobs = mobs %>%
      dplyr::filter(
        date >= start_date_norm
      )
  }
  
  if (!is.null(end_date_norm)) {
    
    if (isTRUE(end_date_norm$inclusive)) {
      
      mobs = mobs %>%
        dplyr::filter(
          date <= end_date_norm$value
        )
      
    } else {
      
      mobs = mobs %>%
        dplyr::filter(
          date < end_date_norm$value
        )
    }
  }
  
  if (isTRUE(drop_na_mod)) {
    
    mobs = mobs %>%
      dplyr::filter(
        !is.na(mod)
      )
  }
  
  if (nrow(mobs) == 0) {
    return(mobs)
  }
  
  
  # Whole-period summary
  
  if (avg_time == 'period') {
    
    mobs_avg = mobs %>%
      dplyr::group_by(
        code,
        var
      ) %>%
      dplyr::summarise(
        obs = summarise_period_vector(
          obs,
          summary_stat
        ),
        mod = summarise_period_vector(
          mod,
          summary_stat
        ),
        .groups = 'drop'
      )
    
    if (has_ref_mod) {
      
      ref_avg = mobs %>%
        dplyr::group_by(
          code,
          var
        ) %>%
        dplyr::summarise(
          ref_mod = summarise_period_vector(
            ref_mod,
            summary_stat
          ),
          .groups = 'drop'
        )
      
      mobs_avg = mobs_avg %>%
        dplyr::left_join(
          ref_avg,
          by = c(
            'code',
            'var'
          )
        )
    }
    
    return(mobs_avg)
  }
  
  
  # Calculate expected data capture
  #
  # Workaround for openair::timeAverage():
  # supplying start.date/end.date can incorrectly trigger a duplicate-date
  # error even when mydata$date contains unique timestamps.
  
  mobs = mobs %>%
    dplyr::mutate(
      summary_date = lubridate::floor_date(
        date,
        unit = avg_time
      )
    )
  
  
  # Use explicitly requested bounds where supplied. Otherwise use the
  # averaging periods containing the first and last available observations.
  
  expected_start = if (!is.null(start_date_norm)) {
    
    lubridate::floor_date(
      start_date_norm,
      unit = avg_time
    )
    
  } else {
    
    lubridate::floor_date(
      min(
        mobs$date,
        na.rm = TRUE
      ),
      unit = avg_time
    )
  }
  
  expected_end = if (!is.null(end_date_norm)) {
    
    lubridate::ceiling_date(
      end_date_norm$value,
      unit = avg_time
    )
    
  } else {
    
    lubridate::ceiling_date(
      max(
        mobs$date,
        na.rm = TRUE
      ),
      unit = avg_time
    )
  }
  
  
  # Construct a complete native-resolution time sequence so that the expected
  # number of observations is independent of missing values in the input.
  
  expected_dates = tibble::tibble(
    date = seq(
      from = expected_start,
      to = expected_end,
      by = native_time
    )
  ) %>%
    dplyr::filter(
      date < expected_end
    ) %>%
    dplyr::mutate(
      summary_date = lubridate::floor_date(
        date,
        unit = avg_time
      )
    )
  
  expected_counts = expected_dates %>%
    dplyr::count(
      summary_date,
      name = 'n_expected'
    )
  
  
  # Calculate observation and model data capture independently.
  
  data_capture = mobs %>%
    dplyr::group_by(
      code,
      var,
      summary_date
    ) %>%
    dplyr::summarise(
      n_obs = sum(
        !is.na(obs)
      ),
      n_mod = sum(
        !is.na(mod)
      ),
      .groups = 'drop'
    )
  
  if (has_ref_mod) {
    
    ref_capture = mobs %>%
      dplyr::group_by(
        code,
        var,
        summary_date
      ) %>%
      dplyr::summarise(
        n_ref_mod = sum(
          !is.na(ref_mod)
        ),
        .groups = 'drop'
      )
    
    data_capture = data_capture %>%
      dplyr::left_join(
        ref_capture,
        by = c(
          'code',
          'var',
          'summary_date'
        )
      )
  }
  
  data_capture = data_capture %>%
    dplyr::left_join(
      expected_counts,
      by = 'summary_date'
    ) %>%
    dplyr::mutate(
      obs_capture = 100 * n_obs / n_expected,
      mod_capture = 100 * n_mod / n_expected
    )
  
  if (has_ref_mod) {
    
    data_capture = data_capture %>%
      dplyr::mutate(
        ref_mod_capture = 100 * n_ref_mod / n_expected
      )
  }
  
  
  # Aggregate with openair
  #
  # data.thresh is deliberately disabled here. Data capture is applied
  # explicitly below because start.date/end.date cannot currently be supplied.
  
  mobs_avg = openair::timeAverage(
    mydata = mobs %>%
      dplyr::select(
        -summary_date
      ),
    avg.time = avg_time,
    data.thresh = 0,
    type = c(
      'code',
      'var'
    ),
    statistic = summary_stat
  ) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.factor),
        as.character
      )
    ) %>%
    dplyr::left_join(
      data_capture,
      by = c(
        'code',
        'var',
        'date' = 'summary_date'
      )
    ) %>%
    dplyr::mutate(
      obs = dplyr::if_else(
        obs_capture >= data_thresh,
        obs,
        NA_real_
      ),
      mod = dplyr::if_else(
        mod_capture >= data_thresh,
        mod,
        NA_real_
      )
    )
  
  if (has_ref_mod) {
    
    mobs_avg = mobs_avg %>%
      dplyr::mutate(
        ref_mod = dplyr::if_else(
          ref_mod_capture >= data_thresh,
          ref_mod,
          NA_real_
        )
      )
  }
  
  mobs_avg = mobs_avg %>%
    dplyr::select(
      -dplyr::any_of(
        c(
          'n_obs',
          'n_mod',
          'n_ref_mod',
          'n_expected',
          'obs_capture',
          'mod_capture',
          'ref_mod_capture'
        )
      )
    )
  
  mobs_avg
}

# openair bug, please do not use until fixed
summarise_mobs_with_a_bug = function(
    mobs_lframe,
    var = 'all',
    avg_time = c('hour', 'day', 'month', 'year', 'period'),
    summary_stat = c('mean', 'median', 'max', 'min'),
    data_thresh = 75,
    drop_na_mod = TRUE,
    start_date = NULL,
    end_date = NULL
) {
  
  summarise_period_vector = function(
    x,
    summary_stat,
    data_thresh
  ) {
    
    valid = is.finite(x)
    data_capture = mean(valid) * 100
    
    if (
      is.nan(data_capture) ||
      data_capture < data_thresh
    ) {
      return(NA_real_)
    }
    
    x = x[valid]
    
    switch(
      summary_stat,
      mean = mean(x),
      median = stats::median(x),
      max = max(x),
      min = min(x),
      stop("Unsupported summary_stat: ", summary_stat)
    )
  }
  
  normalise_start_date = function(x) {
    
    if (is.null(x)) {
      return(NULL)
    }
    
    as.POSIXct(
      x,
      tz = 'UTC'
    )
  }
  
  normalise_end_date = function(x) {
    
    if (is.null(x)) {
      return(NULL)
    }
    
    if (inherits(x, 'Date')) {
      
      return(
        list(
          value = as.POSIXct(
            x + 1,
            tz = 'UTC'
          ),
          inclusive = FALSE
        )
      )
    }
    
    if (
      is.character(x) &&
      grepl(
        '^\\d{4}-\\d{2}-\\d{2}$',
        x
      )
    ) {
      
      return(
        list(
          value = as.POSIXct(
            as.Date(x) + 1,
            tz = 'UTC'
          ),
          inclusive = FALSE
        )
      )
    }
    
    list(
      value = as.POSIXct(
        x,
        tz = 'UTC'
      ),
      inclusive = TRUE
    )
  }
  
  stopifnot(
    is.data.frame(mobs_lframe)
  )
  
  stopifnot(
    all(
      c(
        'date',
        'obs',
        'mod',
        'var',
        'code'
      ) %in% names(mobs_lframe)
    )
  )
  
  avg_time = match.arg(
    avg_time
  )
  
  summary_stat = match.arg(
    summary_stat
  )
  
  start_date_norm = normalise_start_date(
    start_date
  )
  
  end_date_norm = normalise_end_date(
    end_date
  )
  
  mobs = mobs_lframe
  
  if (!is.null(start_date_norm)) {
    
    mobs = dplyr::filter(
      mobs,
      .data$date >= start_date_norm
    )
  }
  
  if (!is.null(end_date_norm)) {
    
    if (isTRUE(end_date_norm$inclusive)) {
      
      mobs = dplyr::filter(
        mobs,
        .data$date <= end_date_norm$value
      )
      
    } else {
      
      mobs = dplyr::filter(
        mobs,
        .data$date < end_date_norm$value
      )
    }
  }
  
  if (isTRUE(drop_na_mod)) {
    
    mobs = dplyr::filter(
      mobs,
      !is.na(.data$mod)
    )
  }
  
  # rng_start = if (is.null(start_date_norm)) {
  #   
  #   suppressWarnings(
  #     min(
  #       mobs$date,
  #       na.rm = TRUE
  #     )
  #   )
  #   
  # } else {
  #   
  #   start_date_norm
  # }
  # 
  # rng_end = if (is.null(end_date_norm)) {
  #   
  #   suppressWarnings(
  #     max(
  #       mobs$date,
  #       na.rm = TRUE
  #     )
  #   )
  #   
  # } else {
  #   
  #   end_date_norm
  # }
  
  if (avg_time == 'period') {
    
    mobs_avg = mobs %>%
      dplyr::group_by(
        .data$code,
        .data$var
      ) %>%
      dplyr::summarise(
        date = suppressWarnings(
          min(
            .data$date,
            na.rm = TRUE
          )
        ),
        obs = summarise_period_vector(
          .data$obs,
          summary_stat = summary_stat,
          data_thresh = data_thresh
        ),
        mod = summarise_period_vector(
          .data$mod,
          summary_stat = summary_stat,
          data_thresh = data_thresh
        ),
        .groups = 'drop'
      )
    
  } else {
    
    # Workaround for openair::timeAverage() bug:
    # supplying start.date/end.date can incorrectly trigger a
    # duplicate-date error even when mydata$date is unique.
    mobs_avg = openair::timeAverage(
      mydata = mobs,
      avg.time = avg_time,
      data.thresh = data_thresh,
      type = c(
        'code',
        'var'
      ),
      statistic = summary_stat
    ) %>%
      dplyr::mutate(
        dplyr::across(
          where(is.factor),
          as.character
        )
      )
  }
  
  if (!identical(var, 'all')) {
    
    mobs_avg = dplyr::filter(
      mobs_avg,
      .data$var %in% var
    )
  }
  
  mobs_avg
}

summarise_runlog_emissions = function(runlog_emissions) {
  
  runlog_emissions %>%
    dplyr::group_by(
      region_id,
      region,
      pollutant,
      runlog_method
    ) %>%
    dplyr::summarise(
      n_bad = sum(
        status != 'ok'
      ),
      emission = dplyr::case_when(
        any(status != 'ok') ~ NA_real_,
        runlog_method == 'combined_table' ~ dplyr::first(emission),
        TRUE ~ sum(emission)
      ),
      .groups = 'drop'
    )
}

summarise_temporal_emep = function(
    emep_file,
    area_file,
    emep_crs,
    configured_vars,
    var_params_list,
    region_geofiles = NULL,
    include_full_domain = FALSE,
    region_min_coverage = 0.95,
    total_out_unit = 'Gg'
) {
  
  available_emep_vars = get_emep_vars(
    nc_file = emep_file,
    var_keywords = NULL
  )
  
  temporal_vars = intersect(
    available_emep_vars,
    configured_vars
  )
  
  if (length(temporal_vars) == 0) {
    return(tibble::tibble())
  }
  
  nc_date = get_emep_floordate(
    emep_file
  )
  
  area_data = read_emep(
    emep_fname = area_file,
    emep_var = 'Area_Grid_km2',
    emep_crs = emep_crs,
    proxy = FALSE,
    driver = 'gdal'
  )
  
  var_units = get_nc_var_units(
    nc_file = emep_file,
    vars = temporal_vars
  )
  
  region_geofiles_in_domain = region_geofiles
  
  if (
    !is.null(region_geofiles_in_domain) &&
    nrow(region_geofiles_in_domain) > 0
  ) {
    
    region_geofiles_in_domain = filter_region_geofiles_to_domain(
      region_geofiles = region_geofiles_in_domain,
      emep_file = emep_file,
      emep_crs = emep_crs,
      min_coverage = region_min_coverage
    )
  }
  
  purrr::map_dfr(
    temporal_vars,
    function(var_name) {
      
      summary_type = dplyr::case_when(
        stringr::str_starts(var_name, 'SURF_') ~ 'mean',
        stringr::str_starts(var_name, 'D3_') ~ 'mean',
        stringr::str_starts(var_name, 'Emis_') ~ 'total',
        stringr::str_starts(var_name, 'DDEP_') ~ 'total',
        stringr::str_starts(var_name, 'WDEP_') ~ 'total',
        TRUE ~ 'value'
      )
      
      quantity_type = dplyr::case_when(
        stringr::str_starts(var_name, 'SURF_') ~ 'Concentration',
        stringr::str_starts(var_name, 'D3_') ~ 'Concentration',
        stringr::str_starts(var_name, 'Emis_') ~ 'Emissions',
        stringr::str_starts(var_name, 'DDEP_') ~ 'Dry deposition',
        stringr::str_starts(var_name, 'WDEP_') ~ 'Wet deposition',
        TRUE ~ 'Value'
      )
      
      var_data = read_emep(
        emep_fname = emep_file,
        emep_var = var_name,
        emep_crs = emep_crs,
        proxy = FALSE,
        driver = 'gdal'
      )
      
      var_dims = stars::st_dimensions(
        var_data
      )
      
      if ('z' %in% names(var_dims)) {
        
        var_z_index = get_var_param(
          var = var_name,
          key = 'z_index',
          var_params_list = var_params_list,
          default = NULL
        )
        
        if (is.null(var_z_index)) {
          stop(
            "No z_index is defined for temporal variable '",
            var_name,
            "' in the variable parameters file.",
            call. = FALSE
          )
        }
        
        var_data = get_emep_z_slice(
          emep_stars = var_data,
          z_index = var_z_index
        )
      }
      
      var_area_data = match_emep_area_dims(
        area_stars = area_data,
        emep_stars = var_data
      )
      
      full_domain_summary = tibble::tibble()
      
      if (isTRUE(include_full_domain)) {
        
        full_domain_summary = summarise_emep_var(
          emep_stars = var_data,
          area_stars = var_area_data,
          var_name = var_name,
          var_unit = unname(
            var_units[var_name]
          ),
          total_out_unit = total_out_unit
        )
        
        if (nrow(full_domain_summary) != nrow(nc_date)) {
          stop(
            "Number of full-domain temporal summary rows does not match ",
            "the EMEP time dimension."
          )
        }
        
        full_domain_summary = full_domain_summary %>%
          dplyr::mutate(
            location_id = 'Full domain',
            location_type = 'region',
            summary_type = summary_type,
            quantity_type = quantity_type,
            time = nc_date$date,
            .before = 1
          )
      }
      
      regional_summary = tibble::tibble()
      
      if (
        !is.null(region_geofiles_in_domain) &&
        nrow(region_geofiles_in_domain) > 0
      ) {
        
        regional_summary = purrr::pmap_dfr(
          region_geofiles_in_domain,
          function(region_id, fpath, ...) {
            
            area_mask_sf = sf::st_read(
              fpath,
              quiet = TRUE
            )
            
            masked_var_data = apply_domain_mask(
              stars_object = var_data,
              domain_mask = area_mask_sf
            )
            
            masked_area_data = apply_domain_mask(
              stars_object = var_area_data,
              domain_mask = area_mask_sf
            )
            
            this_summary = summarise_emep_var(
              emep_stars = masked_var_data,
              area_stars = masked_area_data,
              var_name = var_name,
              var_unit = unname(
                var_units[var_name]
              ),
              total_out_unit = total_out_unit
            )
            
            if (nrow(this_summary) != nrow(nc_date)) {
              stop(
                "Number of temporal summary rows for region '",
                region_id,
                "' does not match the EMEP time dimension."
              )
            }
            
            this_summary %>%
              dplyr::mutate(
                location_id = region_id,
                location_type = 'region',
                summary_type = summary_type,
                quantity_type = quantity_type,
                time = nc_date$date,
                .before = 1
              )
          }
        )
      }
      
      dplyr::bind_rows(
        full_domain_summary,
        regional_summary
      )
    }
  )
}
summarise_wrf_run_settings = function(first_file,
                                      last_file = NULL,
                                      physics_lookup = WRF_PHYSICS_LOOKUP) {
  
  attrs = get_wrf_global_attrs(first_file)
  
  get = function(nm) {
    get_wrf_attr(attrs, nm)
  }
  
  physics_rows = tibble::tribble(
    ~setting, ~attribute, ~lookup_name,
    'Microphysics', 'MP_PHYSICS', 'mp_physics',
    'Longwave radiation', 'RA_LW_PHYSICS', 'ra_lw_physics',
    'Shortwave radiation', 'RA_SW_PHYSICS', 'ra_sw_physics',
    'Surface layer', 'SF_SFCLAY_PHYSICS', 'sf_sfclay_physics',
    'Land-surface model', 'SF_SURFACE_PHYSICS', 'sf_surface_physics',
    'PBL scheme', 'BL_PBL_PHYSICS', 'bl_pbl_physics',
    'Cumulus scheme', 'CU_PHYSICS', 'cu_physics'
  ) %>%
    dplyr::mutate(
      value_raw = purrr::map(attribute, get),
      value = purrr::map2_chr(
        value_raw,
        lookup_name,
        function(x, lookup_name) {
          
          lookup = physics_lookup[[lookup_name]]
          
          if (is.null(lookup)) {
            return(format_wrf_attr_value(x))
          }
          
          purrr::map_chr(
            x,
            lookup_wrf_option,
            lookup = lookup
          ) %>%
            paste(collapse = ', ')
        }
      )
    ) %>%
    dplyr::select(setting, attribute, value)
  
  timing_rows = tibble::tribble(
    ~setting, ~attribute, ~value,
    'Simulation start date', 'SIMULATION_START_DATE', format_wrf_attr_value(get('SIMULATION_START_DATE')),
    'File start date', 'START_DATE', format_wrf_attr_value(get('START_DATE')),
    'History interval', 'HISTORY_INTERVAL', format_wrf_attr_value(get('HISTORY_INTERVAL')),
    'Time step', 'DT', format_wrf_attr_value(get('DT')),
    'Radiation time step', 'RADT', format_wrf_attr_value(get('RADT')),
    'PBL time step', 'BLDT', format_wrf_attr_value(get('BLDT')),
    'Cumulus time step', 'CUDT', format_wrf_attr_value(get('CUDT'))
  )
  
  domain_rows = tibble::tribble(
    ~setting, ~attribute, ~value,
    'Grid ID', 'GRID_ID', format_wrf_attr_value(get('GRID_ID')),
    'Parent ID', 'PARENT_ID', format_wrf_attr_value(get('PARENT_ID')),
    'Parent grid ratio', 'PARENT_GRID_RATIO', format_wrf_attr_value(get('PARENT_GRID_RATIO')),
    'DX', 'DX', format_wrf_attr_value(get('DX')),
    'DY', 'DY', format_wrf_attr_value(get('DY')),
    'Map projection', 'MAP_PROJ', format_wrf_attr_value(get('MAP_PROJ')),
    'Central latitude', 'CEN_LAT', format_wrf_attr_value(get('CEN_LAT')),
    'Central longitude', 'CEN_LON', format_wrf_attr_value(get('CEN_LON')),
    'True latitude 1', 'TRUELAT1', format_wrf_attr_value(get('TRUELAT1')),
    'True latitude 2', 'TRUELAT2', format_wrf_attr_value(get('TRUELAT2')),
    'Grid size west-east', 'WEST-EAST_GRID_DIMENSION', format_wrf_attr_value(get('WEST-EAST_GRID_DIMENSION')),
    'Grid size south-north', 'SOUTH-NORTH_GRID_DIMENSION', format_wrf_attr_value(get('SOUTH-NORTH_GRID_DIMENSION')),
    'Vertical levels', 'BOTTOM-TOP_GRID_DIMENSION', format_wrf_attr_value(get('BOTTOM-TOP_GRID_DIMENSION'))
  )
  
  dplyr::bind_rows(
    physics_rows %>% dplyr::mutate(section = 'Physics options'),
    timing_rows %>% dplyr::mutate(section = 'Timing'),
    domain_rows %>% dplyr::mutate(section = 'Domain')
  ) %>%
    dplyr::relocate(section)
}

summary_regions_require_geodata = function(regions) {
  
  if (is.null(regions)) {
    return(FALSE)
  }
  
  any(regions != 'full_domain')
}

theme_emep_diffmap = function(emep_plot, 
                              plot_title_size = 10,
                              legend_title_size = 10,
                              legend_text_size = 8,
                              cbar_label_angle = 90,
                              cbar_label_vjust = 0.5,
                              cbar_label_hjust = 1) {
  
  emep_plot +
    scale_y_continuous(expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0)) + 
    theme_void() +
    theme(
      legend.position = 'bottom',
      legend.direction = 'horizontal',
      legend.key.height = unit(3, 'mm'),
      legend.title = element_text(size = legend_title_size),
      legend.text = element_text(size = legend_text_size, angle = cbar_label_angle, 
                                 vjust = cbar_label_vjust, hjust = cbar_label_hjust),
      panel.background = element_rect(fill = 'white', color = NA),
      panel.border = element_rect(color = 'black', linewidth = 0.3, fill = NA),
      plot.title = element_text(size = plot_title_size, hjust = 0.5, face = 'bold'),
      plot.margin = margin(8, 5.5, 5.5, 5.5, unit = 'pt')
    )
}

validate_domain_region_input = function(
    region_list,
    input_name,
    submitted_domains,
    allow_full_domain = FALSE,
    allow_null = FALSE
) {
  
  if (
    !is.list(region_list) ||
    is.null(names(region_list)) ||
    any(names(region_list) == '')
  ) {
    stop(
      paste0(
        input_name,
        " must be a named list of domain-specific region selections."
      ),
      call. = FALSE
    )
  }
  
  submitted_region_list = region_list[
    intersect(
      names(region_list),
      submitted_domains
    )
  ]
  
  if (length(submitted_region_list) == 0) {
    stop(
      paste0(
        input_name,
        " does not contain a region selection for any submitted domain."
      ),
      call. = FALSE
    )
  }
  
  if (
    !allow_null &&
    all(
      purrr::map_lgl(
        submitted_region_list,
        is.null
      )
    )
  ) {
    stop(
      paste0(
        input_name,
        " does not contain a region selection for any submitted domain."
      ),
      call. = FALSE
    )
  }
  
  purrr::iwalk(
    submitted_region_list,
    function(regions, domain) {
      
      if (is.null(regions)) {
        
        if (!allow_null) {
          return(invisible(NULL))
        }
        
        return(invisible(NULL))
      }
      
      checkmate::assert_character(
        regions,
        min.len = 1,
        any.missing = FALSE,
        min.chars = 1,
        .var.name = paste0(
          input_name,
          '$',
          domain
        )
      )
      
      invalid_full_domain = regions[
        stringr::str_to_lower(regions) == 'full_domain' &
          regions != 'full_domain'
      ]
      
      if (length(invalid_full_domain) > 0) {
        stop(
          paste0(
            input_name,
            '$',
            domain,
            " contains an invalid full-domain selector:\n  ",
            paste(
              invalid_full_domain,
              collapse = '\n  '
            ),
            "\n\nUse exactly:\n  full_domain"
          ),
          call. = FALSE
        )
      }
      
      if (
        !allow_full_domain &&
        'full_domain' %in% regions
      ) {
        stop(
          paste0(
            input_name,
            '$',
            domain,
            " cannot contain 'full_domain'."
          ),
          call. = FALSE
        )
      }
    }
  )
}

validate_emep_time_range_override = function(
    time_range,
    emep_file,
    run_label
) {
  
  file_time_range = get_emep_time_range(
    emep_file
  )
  
  file_time = file_time_range$first_time
  
  if (
    file_time < time_range$first_time ||
    file_time >= time_range$last_time
  ) {
    
    stop(
      glue::glue(
        "{run_label} model output timestamp ({file_time}) does not fall within the user-specified modelled period ({time_range$first_time} to {time_range$last_time}). Check the supplied {run_label} start and end dates."
      ),
      call. = FALSE
    )
  }
  
  time_range
}

validate_palette = function(
    palette,
    input_name
) {
  
  if (is.null(palette)) {
    return(invisible(NULL))
  }
  
  tryCatch(
    {
      resolve_palette(
        pal_spec = palette,
        n = 3
      )
      
      invisible(NULL)
    },
    error = function(e) {
      stop(
        paste0(
          input_name,
          " could not be resolved:\n  ",
          paste(palette, collapse = ', '),
          "\n\n",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

validate_vars_available = function(
    requested_vars,
    available_vars,
    task,
    run_label,
    file = NULL
) {
  
  missing_vars = setdiff(
    requested_vars,
    available_vars
  )
  
  if (length(missing_vars) == 0) {
    return(invisible(TRUE))
  }
  
  message = paste0(
    task,
    " validation failed.\n\n",
    "The following requested variables are not available in ",
    run_label,
    ":\n  ",
    paste(missing_vars, collapse = '\n  ')
  )
  
  if (!is.null(file)) {
    message = paste0(
      message,
      "\n\nVariables were checked in:\n  ",
      file
    )
  }
  
  stop(
    message,
    call. = FALSE
  )
}

weighted_mean_finite = function(x, w) {
  
  keep = is.finite(x) &
    is.finite(w) &
    w > 0
  
  if (!any(keep)) {
    return(NA_real_)
  }
  
  stats::weighted.mean(
    x[keep],
    w[keep]
  )
}

# source('emep_qaqc_funcs2_WIP_tester.R')
# source('emep_qaqc_user_input2_WIP_tester.R')
# TEST_INNER_DIR = list(Test = '/hood/chrhoo/EMEP/EMEP_user_5.5/output/SCOTLAND/processed/AQC_10084')
# 
# TEST_TIME_RES = list('policy2_totalPM')


