#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
get_excel_structure <- function(path) {

  paths <- as.list(path)

  names(paths) <- str_subset(
    unlist(
      str_split(unlist(paths), pattern = "/")
      ),
    pattern = ".xlsx"
    )

  for(i in 1:length(paths)) {
    paths[[i]][2] <- list(excel_sheets(paths[[i]]))
  }

  for(i in 1:length(paths)) {

    list_head <- list()

    for(j in 1:length(paths[[i]][[2]])) {
      sheet_name <- paths[[i]][[2]][j]

      path <- paths[[i]][[1]]

      list_head[[j]] <- colnames(
        read_excel(
          path,
          sheet = sheet_name,
          col_names = T,
          n_max = 1))
    }

    names(list_head) <- paths[[i]][[2]]

    paths[[i]][[3]] <- list_head

    names(paths[[i]]) <- c("path", "sheets", "header")

  }

  return(paths)
}
