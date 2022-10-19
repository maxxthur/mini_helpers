#' Title
#'
#' @param date
#' @param origin
#'
#' @return
#' @export
#'
#' @examples
sqlite_date <- function(date, origin = "1970-01-01") {
  as.Date(date, origin = origin)
}
