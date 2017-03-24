
validate_parameters <- function(type,
                                parsed_url,
                                location = NULL,
                                date_begin = NULL,
                                date_end = NULL,
                                names = NULL,
                                name = NULL) {
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
  } else {
    if (!is.null(date_begin)) {
      date_end <- Sys.Date()
      parsed_url$query <- c(parsed_url$query, list(obs_date__le = date_end))
    } else {
     warning("When no dates are specified, the date range defaults to the past 90 days")
    }
  } #/ end null date_end check

  if (!is.null(name)) {

    if (!type %in% c("event"))
      warning(paste0("'names' not valid for type '", type, "', ignoring"))

    parsed_url$query <- c(parsed_url$query, list(dataset_name = name))
  } # end/ null name check

  return(parsed_url)
}

validate_json_param <- function(param, value, parsed_url) {
  if (!is.null(value)) {
    if (jsonlite::validate(value)) {
      param_list <- list(value)
      names(param_list) <- param

      parsed_url$query <- c(parsed_url$query, param_list)
    } else {
      stop(sprintf("Invalid json argument to '%s'!", param))
    }
  }
  return(parsed_url)
}
