#' Check a GET verb call for errors
#'
#' @param url A query to the plenar.io API in the form of a character string
#' @param content_only Only include the content, discard completion info
#' @return httr response object
#' @importFrom httr http_status GET content stop_for_status
#' @seealso \code{\link{validate_url}}
#' @export
get_response <- function(url, content_only = TRUE){

  url <- plenar::validate_url(url)
  response <- httr::GET(url)

  # raise any HTTP errors
  httr::stop_for_status(response)

  if (isTRUE(content_only)) {
    return(httr::content(response))
  } else {
    return(response)
  }
}
