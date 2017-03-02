#' Check a GET verb call for errors
#'
#' @param url A query to the plenar.io API in the form of a character string
#' @param content_only A
#' @return httr response object
#' @importFrom httr http_status GET content stop_for_status
#' @importFrom jsonlite fromJSON
#' @seealso \code{\link{validate_url}}
#' @export
get_response <- function(url, content_only = TRUE){

  url <- plenar::validate_url(url)

  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("`jsonlite` needed for this function to work. Please install it.", call. = FALSE)

  response <- httr::GET(url)

  # raise any HTTP errors
  httr::stop_for_status(response)

  if (isTRUE(content_only)) {
    return(httr::content(response))
  } else {
    return(response)
  }
}
