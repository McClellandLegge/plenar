#' Query API for available Event, Shape or Sensor Network Data
#'
#' @param type A character string, one of `event`, `shape` or `sensor`
#' @param names (`type = "event"` only) filter data sets by "human" name
#' @param location (`type = "event"` and `type = "shape"` only) filter data sets with a valid geojson polygon
#' @param date_begin (`type = "event"` only) filter data sets for those that begin on or after this date specified in `YYYY-MM-DD` format
#' @param date_end (`type = "event"` only) filter data sets for those that end on or before this date specified in `YYYY-MM-DD` format
#' @param ... Additional arguments passed to \code{\link{get_response}}
#'
#' @return A list object
#' @export
#' @importFrom httr parse_url build_url
#' @importFrom jsonlite toJSON
#' @examples
#' event <- available_data(type = "event", content_only = TRUE)
#' shapes <- available_data(type = "shape", content_only = TRUE)
#' sensor <- available_data(type = "sensor", content_only = TRUE)
available_data <- function(type, names = NULL, location = NULL, date_begin = NULL, date_end = NULL, ...) {

  parsed_url <- httr::parse_url(plenar::plenar_api)

  if (!type %in% c("event", "shape", "sensor"))
    stop("'type' must be one of 'event', 'shape' or 'sensor'")

  qtype <- switch(type,
                 event = "datasets",
                 shape = "shapes",
                 sensor = "sensor-networks")

  parsed_url$path <- file.path(parsed_url$path, qtype)

  if (!is.null(location)) {

    if (!type %in% c("event", "shapes"))
      warning(paste0("'date_begin' not valid for type '", type, "', ignoring"))

    if (requireNamespace("geojsonlint", quietly = TRUE)) {
      if(!isTRUE(geojsonlint::geojson_validate(location)))
        stop("Invalid geojson polygon! Use `geojsonlint::geojson_validate` and see http://docs.plenar.io/#space-filtering for more.")
    } else
      warning("`geojsonlint` not installed, skipping check of 'location' geojson polygon")

    parsed_url$query <- c(parsed_url$query, list(location_geom__within = location))
  }

  if (!is.null(date_begin)) {

    if (!type %in% c("event"))
      warning(paste0("'date_begin' not valid for type '", type, "', ignoring"))

    if (is.na(as.Date(date_begin, format = "%Y-%m-%d")))
      stop("Invalid `date_begin` format! Must be YYYY-MM-DD.")

    parsed_url$query <- c(parsed_url$query, list(obs_date__ge = date_begin))
  }

  if (!is.null(date_end)) {

    if (!type %in% c("event"))
      warning(paste0("'date_begin' not valid for type '", type, "', ignoring"))

    if (is.na(as.Date(date_end, format = "%Y-%m-%d")))
      stop("Invalid `date_end` format! Must be YYYY-MM-DD.")

    parsed_url$query <- c(parsed_url$query, list(obs_date__le = date_end))
  }

  if (!is.null(names)) {

    if (!type %in% c("event"))
      warning(paste0("'date_begin' not valid for type '", type, "', ignoring"))

    parsed_url$query <- c(parsed_url$query, list(dataset_name__in = jsonlite::toJSON(names)))
  }

  query_url <- plenar::validate_url(httr::build_url(parsed_url))

  plenar::get_response(query_url, ...)
}
