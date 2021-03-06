#' Query API for available Event, Shape or Sensor Network Data
#'
#' @param type A character string, one of `event`, `shape` or `sensor`
#' @param location (`type = "event"` and `type = "shape"` only) filter data sets with a valid geojson polygon
#' @param date_begin (`type = "event"` only) filter data sets for those that begin on or after this date specified in `YYYY-MM-DD` format
#' @param date_end (`type = "event"` only) filter data sets for those that end on or before this date specified in `YYYY-MM-DD` format
#'
#' @return A data.table object
#' @export
#' @importFrom httr parse_url build_url
#' @importFrom jsonlite toJSON
#' @import data.table
#' @examples
#' # can use filters
#' loc = '{"type":"Polygon",
#'   "coordinates":[
#'     [[-87.58695602416992,41.79224063145134],
#'     [-87.58695602416992,41.7996633276003],
#'     [-87.5745964050293,41.7996633276003],
#'     [-87.5745964050293,41.79224063145134],
#'     [-87.58695602416992,41.79224063145134]]]}'
#' event <- available_data(type = "event", date_begin = "2015-10-20", location = loc)
#'
#' shapes <- available_data(type = "shape")
#'
#' sensor <- available_data(type = "sensor")
available_data <- function(type, location = NULL, date_begin = NULL, date_end = NULL) {

  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("`jsonlite` needed for this function to work. Please install it.", call. = FALSE)

  parsed_url <- httr::parse_url(plenar::plenar_api)

  if (!type %in% c("event", "shape", "sensor"))
    stop("'type' must be one of 'event', 'shape' or 'sensor'")

  qtype <- switch(type,
                 event = "datasets",
                 shape = "shapes",
                 sensor = "sensor-networks")

  # append the type to specify the correct route
  parsed_url$path <- file.path(parsed_url$path, qtype)

  # check the parameters against the rules for the type and format
  parsed_url <- validate_parameters(type, parsed_url,
                                    location = location,
                                    date_begin = date_begin,
                                    date_end = date_end)

  # build the query and send call
  query_url <- plenar::validate_url(httr::build_url(parsed_url))
  response <- plenar::get_response(query_url)

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
