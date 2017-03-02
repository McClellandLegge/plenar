#' Basic quality checks on URL(s)
#'
#' @param urls A character vector of URL(s)
#' @return The original URL(s)
#' @importFrom httr parse_url build_url
#' @export
validate_url <- function(urls) {
  if (!requireNamespace("httr", quietly = TRUE)) stop("`httr` needed for this function to work. Please install it.", call. = FALSE)

  urls <- as.character(urls)
  sapply(urls, function(url){
    parsed_url <- httr::parse_url(url)
    if(is.null(parsed_url$scheme) | is.null(parsed_url$hostname) | is.null(parsed_url$path))
      stop(url, " does not appear to be a valid URL.")
    httr::build_url(parsed_url)
  }, USE.NAMES = FALSE)
}
