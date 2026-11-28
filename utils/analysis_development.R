group_sites = function(
    sites,
    method,
    group_col,
    ...
) {
  
  checkmate::assert_class(sites, 'sf')
  checkmate::assert_choice(method, c('distance', 'polygon', 'numeric'))
  checkmate::assert_string(group_col, min.chars = 1)
  
  if (group_col == attr(sites, 'sf_column')) {
    stop("The grouping column cannot replace the geometry column.")
  }
  
  switch(
    method,
    distance = group_sites_by_distance(
      sites = sites,
      group_col = group_col,
      ...
    ),
    polygon = group_sites_by_polygon(
      sites = sites,
      group_col = group_col,
      ...
    ),
    numeric = group_sites_by_numeric(
      sites = sites,
      group_col = group_col,
      ...
    )
  )
}

group_sites_by_distance = function(
    sites,
    group_col,
    geofeature,
    threshold_km,
    within_label = 'within',
    outside_label = 'outside',
    distance_col = NULL
) {
  
  checkmate::assert_number(threshold_km, lower = 0, finite = TRUE)
  checkmate::assert_string(within_label)
  checkmate::assert_string(outside_label)
  checkmate::assert_string(distance_col, null.ok = TRUE)
  
  if (is.na(sf::st_crs(sites))) {
    stop("The sites have no defined CRS.")
  }
  
  if (!is.null(distance_col) &&
      distance_col %in% c(group_col, attr(sites, 'sf_column'))) {
    stop("The distance column conflicts with the grouping or geometry column.")
  }
  
  geofeature = prepare_grouping_geofeature(geofeature)
  
  sites_geom = sf::st_transform(sf::st_geometry(sites), 4326)
  feature_geom = sf::st_transform(sf::st_geometry(geofeature), 4326)
  
  previous_s2 = sf::sf_use_s2()
  on.exit(sf::sf_use_s2(previous_s2), add = TRUE)
  sf::sf_use_s2(TRUE)
  
  distance_km = rep(NA_real_, nrow(sites))
  valid_sites = which(!sf::st_is_empty(sites_geom))
  
  if (length(valid_sites) > 0) {
    
    nearest_feature = sf::st_nearest_feature(
      sites_geom[valid_sites],
      feature_geom
    )
    
    distance_km[valid_sites] = sf::st_distance(
      sites_geom[valid_sites],
      feature_geom[nearest_feature],
      by_element = TRUE
    ) %>%
      units::set_units('km') %>%
      as.numeric()
  }
  
  sites[[group_col]] = dplyr::case_when(
    distance_km <= threshold_km ~ within_label,
    distance_km > threshold_km ~ outside_label,
    TRUE ~ NA_character_
  )
  
  if (!is.null(distance_col)) {
    sites[[distance_col]] = distance_km
  }
  
  sites
}

group_sites_by_numeric = function(
    sites,
    group_col,
    value_col,
    breaks,
    labels = NULL,
    right = FALSE,
    include_lowest = TRUE
) {
  
  checkmate::assert_string(value_col)
  checkmate::assert_choice(value_col, names(sites))
  checkmate::assert_numeric(sites[[value_col]])
  
  checkmate::assert_numeric(
    breaks,
    min.len = 2,
    any.missing = FALSE,
    unique = TRUE,
    sorted = TRUE
  )
  
  checkmate::assert_character(
    labels,
    len = length(breaks) - 1,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE
  )
  
  checkmate::assert_flag(right)
  checkmate::assert_flag(include_lowest)
  
  sites[[group_col]] = cut(
    sites[[value_col]],
    breaks = breaks,
    labels = labels,
    right = right,
    include.lowest = include_lowest,
    ordered_result = TRUE
  )
  
  sites
}

group_sites_by_polygon = function(
    sites,
    group_col,
    geofeature,
    feature_col,
    outside_label = NA_character_
) {
  
  checkmate::assert_string(feature_col)
  checkmate::assert_string(outside_label, na.ok = TRUE)
  
  if (is.na(sf::st_crs(sites))) {
    stop("The sites have no defined CRS.")
  }
  
  geofeature = prepare_grouping_geofeature(geofeature)
  
  checkmate::assert_choice(feature_col, names(geofeature))
  
  geometry_types = as.character(sf::st_geometry_type(geofeature))
  
  if (!all(geometry_types %in% c('POLYGON', 'MULTIPOLYGON'))) {
    stop("Polygon-based grouping requires polygon geometries.")
  }
  
  feature_groups = as.character(geofeature[[feature_col]])
  
  if (anyNA(feature_groups)) {
    stop("The polygon grouping attribute contains missing values.")
  }
  
  sites_geom = sf::st_transform(sf::st_geometry(sites), 4326)
  feature_geom = sf::st_transform(sf::st_geometry(geofeature), 4326)
  
  previous_s2 = sf::sf_use_s2()
  on.exit(sf::sf_use_s2(previous_s2), add = TRUE)
  sf::sf_use_s2(TRUE)
  
  intersections = sf::st_intersects(
    sites_geom,
    feature_geom,
    model = 'closed'
  )
  
  matched_groups = purrr::map(
    intersections,
    function(idx) unique(feature_groups[idx])
  )
  
  multiple_groups = which(lengths(matched_groups) > 1)
  
  if (length(multiple_groups) > 0) {
    
    site_ids = if ('code' %in% names(sites)) {
      sites$code[multiple_groups]
    } else {
      multiple_groups
    }
    
    stop(
      "The following sites intersect polygons belonging to multiple groups: ",
      paste(site_ids, collapse = ', ')
    )
  }
  
  sites[[group_col]] = purrr::map_chr(
    matched_groups,
    function(groups) {
      if (length(groups) == 0) outside_label else groups[1]
    }
  )
  
  sites[[group_col]][sf::st_is_empty(sites_geom)] = NA_character_
  
  sites
}

prepare_grouping_geofeature = function(geofeature) {
  
  if (is.character(geofeature)) {
    
    checkmate::assert_string(geofeature)
    checkmate::assert_file_exists(geofeature)
    
    geofeature = sf::st_read(
      geofeature,
      quiet = TRUE
    )
  }
  
  checkmate::assert_class(geofeature, 'sf')
  
  if (is.na(sf::st_crs(geofeature))) {
    stop("The geographical feature has no defined CRS.")
  }
  
  geofeature = geofeature[!sf::st_is_empty(geofeature), ]
  
  if (nrow(geofeature) == 0) {
    stop("No non-empty geometries found in geofeature.")
  }
  
  geofeature
}