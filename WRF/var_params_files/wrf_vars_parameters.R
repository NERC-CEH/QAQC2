# ==========================================================================
# WRF VARIABLE PARAMETERS
# ==========================================================================

MAP_TEMPLATE_BREAKS = c(
  0, 0.1, 0.2, 0.3, 0.5, 1, 2, 3, 4, 5,
  10, 15, 20, 25, 30, 40, 50, 100, 200
)

MAP_TEMPLATE_DIFF_BREAKS = c(seq(-10, -1, 1), -0.5, 0.5, seq(1, 10, 1))

MAP_TEMPLATE_MB_BREAKS = c(seq(-10, -2, 2), -1, 1, seq(2, 10, 2))
MAP_TEMPLATE_RMSE_BREAKS = c(0, 1, 2, 3, 5, 8, 10)
MAP_TEMPLATE_NMB_BREAKS = c(-100, -50, -25, -10, 10, 25, 50, 100)
MAP_TEMPLATE_R_BREAKS = c(seq(-1, -0.2, 0.2), -0.05, 0.05, seq(0.2, 1, 0.2))


WRF_VAR_PARAMS_LIST = list(
  
  T2 = list(
    aliases = c('t2'),
    description = 'Temperature at 2 m',
    units = '°C',
    
    maps = list(
      lab = 'T[2]',
      testref_breaks = seq(-16, 16, 2),
      absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
      reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS,
      plot_reldiff = FALSE
    ),
    
    temporal = list(
      lab = 'T[2]'
    ),
    
    mobs = list(
      lab = 'T[2]',
      mod_colour = '#e41a1c',
      mod_linewidth = 1,
      mod_linetype = 'solid',
      obs_fill = '#fbb4ae',
      obs_colour = 'gray10',
      pointsize = 2,
      map_mb_breaks = MAP_TEMPLATE_MB_BREAKS,
      map_rmse_breaks = MAP_TEMPLATE_RMSE_BREAKS,
      map_nmb_breaks = MAP_TEMPLATE_NMB_BREAKS,
      map_r_breaks = MAP_TEMPLATE_R_BREAKS
    )
  ),

  td2 = list(
    aliases = c('Td2'),
    description = 'Dew-point temperature at 2 m',
    units = '°C',
    
    maps = list(
      lab = 'T[d2]',
      testref_breaks = seq(-20, 12, 2),
      absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
      reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS,
      plot_reldiff = FALSE
    ),
    
    temporal = list(
      lab = 'T[d2]'
    ),
    
    mobs = list(
      lab = 'T[d2]',
      mod_colour = '#984ea3',
      mod_linewidth = 1,
      mod_linetype = 'solid',
      obs_fill = '#decbe4',
      obs_colour = 'gray10',
      pointsize = 2,
      map_mb_breaks = MAP_TEMPLATE_MB_BREAKS,
      map_rmse_breaks = MAP_TEMPLATE_RMSE_BREAKS,
      map_nmb_breaks = MAP_TEMPLATE_NMB_BREAKS,
      map_r_breaks = MAP_TEMPLATE_R_BREAKS
    )
  ),

  slp = list(
    aliases = c('SLP'),
    description = 'Sea-level pressure',
    units = 'hPa',
    
    maps = list(
      lab = 'SLP',
      testref_breaks = seq(992, 1032, 4),
      absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
      reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS,
      testref_breaks_scale_cut = FALSE,
      testref_breaks_accuracy = 1,
      plot_reldiff = FALSE
    ),
    
    temporal = list(
      lab = 'SLP'
    ),
    
    mobs = list(
      lab = 'SLP',
      mod_colour = '#f781bf',
      mod_linewidth = 1,
      mod_linetype = 'solid',
      obs_fill = '#fddaec',
      obs_colour = 'gray10',
      pointsize = 2,
      map_mb_breaks = MAP_TEMPLATE_MB_BREAKS,
      map_rmse_breaks = MAP_TEMPLATE_RMSE_BREAKS,
      map_nmb_breaks = MAP_TEMPLATE_NMB_BREAKS,
      map_r_breaks = MAP_TEMPLATE_R_BREAKS
    )
  ),

  PSFC = list(
    aliases = c('PSFC'),
    description = 'Surface pressure',
    units = 'hPa',
    
    maps = list(
      lab = 'PSFC',
      testref_breaks = seq(992, 1032, 4),
      absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
      reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS,
      testref_breaks_scale_cut = FALSE,
      testref_breaks_accuracy = 1,
      plot_reldiff = FALSE
    ),
    
    temporal = list(
      lab = 'PSFC'
    ),
    
    mobs = list(
      lab = 'PSFC',
      mod_colour = '#f781bf',
      mod_linewidth = 1,
      mod_linetype = 'solid',
      obs_fill = '#fddaec',
      obs_colour = 'gray10',
      pointsize = 2,
      map_mb_breaks = MAP_TEMPLATE_MB_BREAKS,
      map_rmse_breaks = MAP_TEMPLATE_RMSE_BREAKS,
      map_nmb_breaks = MAP_TEMPLATE_NMB_BREAKS,
      map_r_breaks = MAP_TEMPLATE_R_BREAKS
    )
  ),

  rh2 = list(
    aliases = c('RH2'),
    description = 'Relative humidity at 2 m',
    units = '%',
    
    maps = list(
      lab = 'RH[2]',
      testref_breaks = seq(0, 100, 5),
      absdiff_breaks = c(seq(-30, -5, 5), seq(5, 30, 5)),
      reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS,
      plot_reldiff = FALSE
    ),
    
    temporal = list(
      lab = 'RH[2]'
    ),
    
    mobs = list(
      lab = 'RH[2]',
      mod_colour = '#008b8b',
      mod_linewidth = 1,
      mod_linetype = 'solid',
      obs_fill = '#D1EEEE',
      obs_colour = 'gray10',
      pointsize = 2,
      map_mb_breaks = MAP_TEMPLATE_MB_BREAKS,
      map_rmse_breaks = MAP_TEMPLATE_RMSE_BREAKS,
      map_nmb_breaks = MAP_TEMPLATE_NMB_BREAKS,
      map_r_breaks = MAP_TEMPLATE_R_BREAKS
    )
  ),
  
  ws = list(
    aliases = c('WS'),
    description = 'Wind speed at 10 m',
    units = 'm s-1',
    
    maps = list(
      lab = 'WS',
      testref_breaks = seq(0, 10, 1),
      absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
      reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS,
      plot_reldiff = FALSE
    ),
    
    temporal = list(
      lab = 'WS'
    ),
    
    mobs = list(
      lab = 'WS',
      mod_colour = '#ff7f00',
      mod_linewidth = 1,
      mod_linetype = 'solid',
      obs_fill = '#fed9a6',
      obs_colour = 'gray10',
      pointsize = 2,
      map_mb_breaks = MAP_TEMPLATE_MB_BREAKS,
      map_rmse_breaks = MAP_TEMPLATE_RMSE_BREAKS,
      map_nmb_breaks = MAP_TEMPLATE_NMB_BREAKS,
      map_r_breaks = MAP_TEMPLATE_R_BREAKS
    )
  ),

  wd = list(
    aliases = c('WD'),
    description = 'Wind direction',
    units = '°',
    
    maps = list(
      lab = 'WD',
      plot_reldiff = FALSE
    ),
    
    temporal = list(
      lab = 'WD'
    ),
    
    mobs = list(
      lab = 'WD',
      mod_colour = '#a65628',
      mod_linewidth = 1,
      mod_linetype = 'solid',
      obs_fill = '#e5d8bd',
      obs_colour = 'gray10',
      pointsize = 2,
      map_mb_breaks = c(seq(-50, -10, 10), seq(10, 50, 10)),
      map_rmse_breaks = NULL,
      map_nmb_breaks = NULL,
      map_r_breaks = MAP_TEMPLATE_R_BREAKS
    )
  ),

  precip = list(
    aliases = c('subprecip'),
    description = 'Accumulated precipitation',
    units = 'mm',
    
    maps = list(
      lab = 'Precipitation',
      testref_breaks = c(0, 25, 50, 100, seq(250, 2000, 250)),
      absdiff_breaks = c(seq(-100, -20, 20), -10, -5, 5, 10, seq(20, 100, 20)),
      reldiff_breaks = c(seq(-100, -20, 20), -10, -5, 5, 10, seq(20, 100, 20)),
      testref_breaks_scale_cut = FALSE,
      testref_breaks_accuracy = 1,
      absdiff_breaks_scale_cut = FALSE,
      reldiff_breaks_scale_cut = FALSE,
      plot_reldiff = TRUE
    ),
    
    temporal = list(
      lab = 'Precipitation'
    ),
    
    mobs = list(
      lab = 'Precipitation',
      mod_colour = '#377eb8',
      mod_linewidth = 1,
      mod_linetype = 'solid',
      obs_fill = '#b3cde3',
      obs_colour = 'gray10',
      pointsize = 2,
      map_mb_breaks = c(-200, -100, -50, -25, -10, 10, 25, 50, 100, 200),
      map_rmse_breaks = c(0, 10, 25, 50, 100, 200, 500),
      map_nmb_breaks = c(-200, -100, -50, -25, -10, 10, 25, 50, 100, 200),
      map_r_breaks = MAP_TEMPLATE_R_BREAKS
    )
  )
  
)
