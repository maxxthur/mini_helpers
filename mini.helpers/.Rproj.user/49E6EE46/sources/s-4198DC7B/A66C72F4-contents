#' Title
#'
#' @param connection
#'
#' @return
#' @export
#'
#' @examples
clean_db_environment <- function(connection = con) {

  dbDisconnect(connection, shutdown = T)

  rm(list = ls())
}
