
validate_parameters <- function(type, parsed_url, location, date_begin, date_end, names) {
  if (!is.null(location)) {

    if (!type %in% c("event", "shapes"))
      warning(paste0("'date_begin' not valid for type '", type, "', ignoring"))

    if (requireNamespace("geojsonlint", quietly = TRUE)) {
      if(!isTRUE(geojsonlint::geojson_validate(location)))
        stop("Invalid geojson polygon! Use `geojsonlint::geojson_validate` and see http://docs.plenar.io/#space-filtering for more.")
    } else
      warning("`geojsonlint` not installed, skipping check of 'location' geojson polygon")

    parsed_url$query <- c(parsed_url$query, list(location_geom__within = location))
  } #/ end null location check

  if (!is.null(date_begin)) {

    if (!type %in% c("event"))
      warning(paste0("'date_begin' not valid for type '", type, "', ignoring"))

    if (is.na(as.Date(date_begin, format = "%Y-%m-%d")))
      stop("Invalid `date_begin` format! Must be YYYY-MM-DD.")

    parsed_url$query <- c(parsed_url$query, list(obs_date__ge = date_begin))
  } #/ end null date_begin check

  if (!is.null(date_end)) {

    if (!type %in% c("event"))
      warning(paste0("'date_begin' not valid for type '", type, "', ignoring"))

    if (is.na(as.Date(date_end, format = "%Y-%m-%d")))
      stop("Invalid `date_end` format! Must be YYYY-MM-DD.")

    parsed_url$query <- c(parsed_url$query, list(obs_date__le = date_end))
  } #/ end null date_end check

  if (!is.null(names)) {

    if (!type %in% c("event"))
      warning(paste0("'names' not valid for type '", type, "', ignoring"))

    parsed_url$query <- c(parsed_url$query, list(dataset_name__in = jsonlite::toJSON(names)))
  } # end/ null names check

  return(parsed_url)
}
