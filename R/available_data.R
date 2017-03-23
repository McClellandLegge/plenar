#' Query API for available Event, Shape or Sensor Network Data
#'
#' @param type A character string, one of `event`, `shape` or `sensor`
#' @param names (`type = "event"` only) filter data sets by "human" name
#' @param location (`type = "event"` and `type = "shape"` only) filter data sets with a valid geojson polygon
#' @param date_begin (`type = "event"` only) filter data sets for those that begin on or after this date specified in `YYYY-MM-DD` format
#' @param date_end (`type = "event"` only) filter data sets for those that end on or before this date specified in `YYYY-MM-DD` format
#' @param ... Additional arguments passed to \code{\link{get_response}}
#'
#' @return A data.table object
#' @export
#' @importFrom httr parse_url build_url
#' @importFrom jsonlite toJSON
#' @import data.table
#' @examples
#' # can use date filters
#' event <- available_data(type = "event", date_begin = "2016-10-20")
#'
#' # can use name filters
#' sets <- c("Land Use Permits", "LAPD Crime and Collision Raw Data for 2013")
#' shapes <- available_data(type = "shape", names = sets)
#'
#' sensor <- available_data(type = "sensor")
available_data <- function(type, names = NULL, location = NULL, date_begin = NULL, date_end = NULL, ...) {

  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("`jsonlite` needed for this function to work. Please install it.", call. = FALSE)

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
      warning(paste0("'date_begin' not valid for type '", type, "', ignoring"))

    parsed_url$query <- c(parsed_url$query, list(dataset_name__in = jsonlite::toJSON(names)))
  } # end/ null names check

  # build the query and send call
  query_url <- plenar::validate_url(httr::build_url(parsed_url))
  response <- plenar::get_response(query_url, ...)

  # format the nested list
  format_available(response, type = type)
}


format_available <- function(x, type) {

  # specify the name of the element by type
  if (type %in% c("event", "shape")) {
    objects <- get("objects", x)
  } else if (type == "sensor") {
    objects <- get("data", x)
  } else {
    stop("Invalid 'type' argument to format")
  }

  dt_list <- lapply(objects, function(x) {
    # list_cols <- which(sapply(x, class) == "list")
    simple_cols <- which(sapply(x, class) != "list")

    # flattened <- sapply(x[list_cols], paste, collapse = "|",
                        # simplify = FALSE, USE.NAMES = TRUE)

    # data.table::as.data.table(c(x[simple_cols], flattened))
    data.table::as.data.table(x[simple_cols])
  })

  out <- data.table::rbindlist(dt_list, fill = TRUE)
  return(out)
}
