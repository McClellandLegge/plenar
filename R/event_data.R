#' Dump all records for an event data set
#'
#' @inheritParams available_data
#' @param name The name of the dataset to extract
#' @param filter Attribute filtering, see the [documentation](http://docs.plenar.io/#attribute-filtering)
#'
#' @return A data.table
#' @importFrom data.table as.data.table
#' @importFrom httr parse_url build_url
#' @importFrom jsonlite validate fromJSON
#' @export
#' @examples
#' x <- event_data(name = "crimes_2001_to_present",
#'   filter = '{"op": "eq", "col": "primary_type", "val": "HOMICIDE"}',
#'   date_begin = "2016-12-01", date_end = "2016-12-31")
event_data <- function(name = NULL,
                       location = NULL,
                       date_begin = NULL,
                       date_end = NULL,
                       filter = NULL) {

  # parse the api url
  parsed_url <- httr::parse_url(plenar::plenar_api)

  # append the relevant route
  parsed_url$path <- paste0(parsed_url$path, "/datadump/")

  # validate the parameters passed
  parsed_url <- validate_parameters("event", parsed_url,
                                    location = location,
                                    date_begin = date_begin,
                                    date_end = date_end,
                                    name = name)

  # validate the data set specific filter
  filter_name <- paste0(name, "__filter")
  parsed_url <- validate_json_param(filter_name, filter, parsed_url)

  # build the query and send call
  query_url <- plenar::validate_url(httr::build_url(parsed_url))
  response <- plenar::get_response(query_url)

  # convert from JSON
  data.table::as.data.table(jsonlite::fromJSON(response)$features$properties)
}
