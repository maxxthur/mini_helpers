#' Detect Rows with NA Values
#'
#' This function filters rows in a data frame that contain at least one NA value.
#'
#' @param x A data frame or tibble.
#'
#' @return A filtered data frame containing only rows with at least one NA.
#' @export
#'
#' @examples
#' df <- tibble::tibble(a = c(1, NA, 3), b = c(4, 5, NA))
#' na_detect(df)
na_detect <- function(x) {
  dplyr::filter(x, dplyr::if_any(dplyr::everything(), is.na))
}

