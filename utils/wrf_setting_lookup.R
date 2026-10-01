WRF_PHYSICS_LOOKUP = list(
  mp_physics = c(
    `0` = 'No microphysics',
    `1` = 'Kessler',
    `2` = 'Lin et al.',
    `3` = 'WRF Single-Moment 3-class',
    `4` = 'WRF Single-Moment 5-class',
    `5` = 'Ferrier / Eta',
    `6` = 'WRF Single-Moment 6-class',
    `7` = 'Goddard',
    `8` = 'Thompson',
    `10` = 'Morrison double-moment',
    `16` = 'WRF Double-Moment 6-class'
  ),
  ra_lw_physics = c(
    `0` = 'No longwave radiation',
    `1` = 'RRTM',
    `3` = 'CAM',
    `4` = 'RRTMG',
    `24` = 'RRTMG-K'
  ),
  ra_sw_physics = c(
    `0` = 'No shortwave radiation',
    `1` = 'Dudhia',
    `2` = 'Goddard',
    `3` = 'CAM',
    `4` = 'RRTMG',
    `24` = 'RRTMG-K'
  ),
  sf_sfclay_physics = c(
    `0` = 'No surface layer scheme',
    `1` = 'Revised MM5 Monin-Obukhov',
    `2` = 'Monin-Obukhov / Janjic Eta',
    `5` = 'MYNN surface layer'
  ),
  sf_surface_physics = c(
    `0` = 'No land-surface model',
    `1` = 'Thermal diffusion',
    `2` = 'Noah land-surface model',
    `3` = 'RUC land-surface model',
    `4` = 'Noah-MP land-surface model'
  ),
  bl_pbl_physics = c(
    `0` = 'No PBL scheme',
    `1` = 'YSU',
    `2` = 'Mellor-Yamada-Janjic',
    `5` = 'MYNN 2.5',
    `6` = 'MYNN 3',
    `7` = 'ACM2',
    `8` = 'BouLac',
    `9` = 'UW',
    `11` = 'Shin-Hong'
  ),
  cu_physics = c(
    `0` = 'No cumulus scheme',
    `1` = 'Kain-Fritsch',
    `2` = 'Betts-Miller-Janjic',
    `3` = 'Grell-Freitas',
    `5` = 'Grell 3D ensemble',
    `6` = 'Tiedtke',
    `11` = 'Multi-scale Kain-Fritsch',
    `14` = 'New Tiedtke',
    `16` = 'New SAS'
  )
)