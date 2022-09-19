#' Title
#'
#' @param dir_path
#' @param ext_filter
#'
#' @return
#' @export
#'
#' @examples
infer_paths <- function(dir_path, ext_filter = NULL) {

  regex <- paste(ext_filter, collapse = "|")

  paths <- paste0(dir_path, "/", dir(dir_path)) %>%
   str_subset(pattern = regex)

  return(paths)
}
