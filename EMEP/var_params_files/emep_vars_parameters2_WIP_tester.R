# ==========================================================================
# VARIABLE PARAMETER EXAMPLE
# ==========================================================================
#
# The example below shows the available variable parameters. Parameters that
# are not required for a variable can generally be omitted or set to NULL.
#
# Labels use R plotmath-style notation where formatting is required.
#
# Common examples:
#
#   'NO[2]'          -> NO₂
#   'SO[4]^{2-}'     -> SO₄²⁻
#   'NH[4]^+'        -> NH₄⁺
#   'O[3]'           -> O₃
#
# Common Greek letters:
#
#   'alpha'          -> α
#   'beta'           -> β
#   'gamma'          -> γ
#   'delta'          -> δ
#   'mu'             -> μ
#   'sigma'          -> σ
#
# Text, symbols and units can be combined, for example:
#
#   'NO[2]~"concentration"~(mu*g~m^{-3})'
#
# See ?plotmath for further notation.
#
# Example:
#
# SURF_ug_NO2 = list(
#   mobs_alias = 'SURF_ug_NO2',
#   units = 'ug/m3',
#
#   # Vertical index to extract for variables with a vertical dimension.
#   z_index = NULL,
#
#   maps = list(
#     lab = 'NO[2]',
# 
#     testref_breaks = c(0, 5, 10, 20, 30, 40, 60, 80, 100, 150, 200),
#     absdiff_breaks = c(-100, -50, -25, -10, -5, 0, 5, 10, 25, 50, 100),
#     reldiff_breaks = c(-100, -50, -25, -10, -5, 0, 5, 10, 25, 50, 100),
# 
#     # Optional formatting controls for break labels.
#     # *_breaks_accuracy controls numeric rounding.
#     # *_breaks_scale_cut controls large-number abbreviation
#     # (k, M, G, T); FALSE disables abbreviation.
#     testref_breaks_accuracy = NULL,
#     absdiff_breaks_accuracy = NULL,
#     reldiff_breaks_accuracy = NULL,
#     ratio_breaks_accuracy = NULL,
#
#     testref_breaks_scale_cut = NULL,
#     absdiff_breaks_scale_cut = NULL,
#     reldiff_breaks_scale_cut = NULL,
#     ratio_breaks_scale_cut = NULL,
#
#     testref_colours = c('#ffffcc', '#ffeda0', '#fed976', '#feb24c', '#fd8d3c', '#fc4e2a', '#e31a1c', '#bd0026', '#800026', '#4d0018'),
#     absdiff_colours = c('#053061', '#2166ac', '#4393c3', '#92c5de', '#d1e5f0', '#fddbc7', '#f4a582', '#d6604d', '#b2182b', '#67001f'),
#     reldiff_colours = c('#053061', '#2166ac', '#4393c3', '#92c5de', '#d1e5f0', '#fddbc7', '#f4a582', '#d6604d', '#b2182b', '#67001f'),
#
#     testref_breaks_labs = c('5', '10', '20', '30', '40', '60', '80', '100', '150'),
#     absdiff_breaks_labs = c('-50', '-25', '-10', '-5', '0', '5', '10', '25', '50'),
#     reldiff_breaks_labs = c('-50%', '-25%', '-10%', '-5%', '0%', '5%', '10%', '25%', '50%'),
#
#     # NULL uses the automatically generated colour-bar title.
#     testref_cbar_title = NULL,
#     absdiff_cbar_title = NULL,
#     reldiff_cbar_title = NULL,
#
#     # Value ranges to include in the plotted map
#     # Use c(-Inf, Inf) when no value filtering is required.
#     testref_value_range = c(-Inf, Inf),
#     absdiff_value_range = c(-Inf, Inf),
#     reldiff_value_range = c(-Inf, Inf),
#
#     # Interactive-map hover values.
#     # FALSE disables hover values; TRUE shows all finite values.
#     # Conditions can use percentiles (e.g. 'p95', '>p95'),
#     # absolute values (e.g. '>5'), or multiple conditions combined
#     # with OR (e.g. c('<p5', '>p95')).
#     
#     test_hover_values = FALSE,
#     ref_hover_values = FALSE,
#     absdiff_hover_values = FALSE,
#     reldiff_hover_values = FALSE,
#
#     plot_reldiff = TRUE
#   ),
#
#   temporal = list(
#     lab = 'NO[2]',
# 
#     test_colour = NULL,
#     ref_colour = NULL,
#     test_linewidth = NULL,
#     ref_linewidth = NULL,
#     test_linetype = NULL,
#     ref_linetype = NULL
#   ),
#
#   mobs = list(
#     lab = 'NO[2]',
# 
#     mod_colour = '#377eb8',
#     mod_linewidth = 1,
#     mod_linetype = 'solid',
#
#     obs_fill = '#b3cde3',
#     obs_colour = 'gray10',
#
#     pointsize = 2,
#
#     map_mb_breaks = MAP_TEMPLATE_MB_BREAKS,
#     map_rmse_breaks = MAP_TEMPLATE_RMSE_BREAKS,
#     map_nmb_breaks = MAP_TEMPLATE_NMB_BREAKS,
#     map_r_breaks = MAP_TEMPLATE_R_BREAKS
#   )
# )





MAP_TEMPLATE_BREAKS = c(
  0, 0.1, 0.2, 0.3, 0.5, 1, 2, 3, 4, 5,
  10, 15, 20, 25, 30, 40, 50, 100, 200
)

MAP_TEMPLATE_DIFF_BREAKS = c(seq(-10, -1, 1), -0.5, 0.5, seq(1, 10, 1))

MAP_TEMPLATE_MB_BREAKS = c(seq(-10, -2, 2), -1, 1, seq(2, 10, 2))
MAP_TEMPLATE_RMSE_BREAKS = c(0, 1, 2, 3, 5, 8, 10)
MAP_TEMPLATE_NMB_BREAKS = c(-100, -50, -25, -10, 10, 25, 50, 100)
MAP_TEMPLATE_R_BREAKS = c(seq(-1,-0.2,0.2), -0.05, 0.05, seq(0.2, 1, 0.2))

EMEP_VAR_PARAMS_LIST = list(
  
  DDEP_OXN_m2Conif = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  DDEP_OXN_m2Grid = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  DDEP_OXN_m2Seminat = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  DDEP_RDN_m2Conif = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  DDEP_RDN_m2Grid = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  DDEP_RDN_m2Seminat = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  DDEP_SOX_m2Conif = list(
    units = 'mgS/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  DDEP_SOX_m2Grid = list(
    units = 'mgS/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  DDEP_SOX_m2Seminat = list(
    units = 'mgS/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  D3_ppb_NO2 = list(
    mobs_alias = 'SURF_ug_NO2',
    units = 'ppb',
    
    z_index = 21,
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  Emis_mgm2_BioNatC5H8 = list(
    units = 'mg/m2',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  Emis_mgm2_BioNatNO = list(
    units = 'mg/m2',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
    ),
  
  Emis_mgm2_BioNatTERP = list(
    units = 'mg/m2',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  Emis_mgm2_co = list(
    units = 'mg/m2',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  Emis_mgm2_nh3 = list(
    units = 'mg/m2',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  Emis_mgm2_nox = list(
    units = 'mg/m2',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  Emis_mgm2_pmco = list(
    units = 'mg/m2',

    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  Emis_mgm2_pm25 = list(
    units = 'mg/m2',
    
    maps = list(testref_breaks = c(0, 50, 100, 200, 400, 800, 1200,1600),
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS,
                
                testref_breaks_accuracy = 0.01
                )
  ),
  
  Emis_mgm2_sox = list(
    units = 'mg/m2',

    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  Emis_mgm2_voc = list(
    units = 'mg/m2',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  # derived var from O3 and NO2
  ox = list(
    units = 'ug/m3',
    
    mobs = list(lab = 'O[x]',
                
                mod_colour = '#008b8b',
                mod_linewidth = 0.5,
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
  
  # derived var from O3 and NO2
  `ox(ppb)` = list(
    mobs_alias = 'ox',
    units = 'ppb'
  ),
  
  SURF_ppb_NO2 = list(
    units = 'ppb',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  SURF_ppb_O3 = list(
    mobs_alias = 'SURF_ug_O3',
    units = 'ppb',
    
    maps = list(testref_breaks = seq(10, 50, 4),
                absdiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  SURF_ug_DUST_WB_C = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
    )
  ),
  
  SURF_ug_DUST_WB_F = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  SURF_ug_ECFINE = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  SURF_ug_HNO3 = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.1 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.01 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  
  SURF_ug_NH3 = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                ),
    
    mobs = list(lab = 'NH[3]',
                
                mod_colour = '#e41a1c',
                mod_linewidth = 0.5,
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
  
  SURF_ug_NH4_F = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.4 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.05 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  SURF_ug_NO = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                ),
    
    mobs = list(lab = 'NO',
                
                mod_colour = '#984ea3',
                mod_linewidth = 0.5,
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
  
  SURF_ug_NO2 = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = MAP_TEMPLATE_BREAKS,
                absdiff_breaks = MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 10 * MAP_TEMPLATE_DIFF_BREAKS
                ),
    
    mobs = list(lab = 'NO[2]',
                
                mod_colour = '#377eb8',
                mod_linewidth = 0.5,
                mod_linetype = 'solid',
                
                obs_fill = '#b3cde3',
                obs_colour = 'gray10',
                
                pointsize = 2,
                
                map_mb_breaks = MAP_TEMPLATE_MB_BREAKS,
                map_rmse_breaks = MAP_TEMPLATE_RMSE_BREAKS,
                map_nmb_breaks = MAP_TEMPLATE_NMB_BREAKS,
                map_r_breaks = MAP_TEMPLATE_R_BREAKS
                )
    
  ),
  
  SURF_ug_NO3_C = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.1 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.01 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  SURF_ug_NO3_F = list(
    short_lab = 'NO[3] F',
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.1 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.01 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  SURF_ug_O3 = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = seq(20, 50, 2),
                absdiff_breaks = 20 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = 2 * MAP_TEMPLATE_DIFF_BREAKS
                ),
    
    mobs = list(lab = 'O[3]',
                
                mod_colour = '#4daf4a',
                mod_linewidth = 0.5,
                mod_linetype = 'solid',
                
                obs_fill = '#ccebc5',
                obs_colour = 'gray10',
                
                pointsize = 2,
                
                map_mb_breaks = MAP_TEMPLATE_MB_BREAKS,
                map_rmse_breaks = MAP_TEMPLATE_RMSE_BREAKS,
                map_nmb_breaks = MAP_TEMPLATE_NMB_BREAKS,
                map_r_breaks = MAP_TEMPLATE_R_BREAKS
                )
  ),

  SURF_ug_PM25_rh50 = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.3 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                ),
    
    mobs = list(lab = 'PM[2.5]',
                
                mod_colour = '#ff7f00',
                mod_linewidth = 0.5,
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
  
  SURF_ug_PM10_rh50 = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.5 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                ),
    
    mobs = list(lab = 'PM[10]',
                
                mod_colour = '#a65628',
                mod_linewidth = 0.5,
                mod_linetype = 'solid',
                
                obs_fill = '#e5d8bd',
                obs_colour = 'gray10',
                
                pointsize = 2,
                
                map_mb_breaks = MAP_TEMPLATE_MB_BREAKS,
                map_rmse_breaks = MAP_TEMPLATE_RMSE_BREAKS,
                map_nmb_breaks = MAP_TEMPLATE_NMB_BREAKS,
                map_r_breaks = MAP_TEMPLATE_R_BREAKS
                )
  ),
  
  SURF_ug_PPM25 = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.3 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  SURF_ug_PM_ASOA = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.3 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  SURF_ug_PM_BSOA = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.3 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  SURF_ug_SEASALT_C = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.3 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  SURF_ug_SEASALT_F = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.3 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  SURF_ug_SO2 = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.2 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                ),
    
    mobs = list(lab = 'SO[2]',
                
                mod_colour = '#f781bf',
                mod_linewidth = 0.5,
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
  
  SURF_ug_SO4 = list(
    units = 'ug/m3',
    
    maps = list(testref_breaks = 0.2 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.02 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  WDEP_HNO3 = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  WDEP_HONO = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  WDEP_N2O5 = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  WDEP_NH3 = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  WDEP_NH4_F = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  WDEP_NO3_C = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  WDEP_NO3_F = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  WDEP_OXN = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  WDEP_PREC = list(
    units = 'mm',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  WDEP_RDN = list(
    units = 'mgN/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  WDEP_SO2 = list(
    units = 'mgS/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  WDEP_SOX = list(
    units = 'mgS/m2',
    
    maps = list(testref_breaks = 20 * MAP_TEMPLATE_BREAKS,
                absdiff_breaks = 0.1 * MAP_TEMPLATE_DIFF_BREAKS,
                reldiff_breaks = MAP_TEMPLATE_DIFF_BREAKS
                )
  ),
  
  # ratio variables
  
  SURF_ug_ECFINE_vs_SURF_ug_PM25_rh50 = list(
    maps = list(
      ratio_breaks = seq(0, 100, 10)
    )
  ),
  
  SURF_ug_PM25_rh50_vs_SURF_ug_PM10_rh50 = list(
    maps = list(
      ratio_breaks = seq(0, 100, 20)
    )
  ),
  
  SURF_ug_DUST_WB_F_vs_SURF_ug_PM25_rh50 = list(
    maps = list(
      ratio_breaks = seq(0, 100, 10)
    )
  ),
  
  SURF_ug_PPM25_vs_SURF_ug_PM25_rh50 = list(
    maps = list(
      ratio_breaks = seq(0, 100, 10)
    )
  )

)