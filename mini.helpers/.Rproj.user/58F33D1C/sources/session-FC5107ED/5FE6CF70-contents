#' Title
#'
#' @param x
#' @param group_by
#'
#' @return
#' @export
#'
#' @examples
na_summary <- function(x, group_by = NULL) {

  # counting
  if(is.null(group_by)) {
    nas <- x %>%
      mutate(across(.fns = is.na)) %>%
      summarise(across(.fns = sum))
  } else {
    nas <- x %>%
      group_by(across(all_of(group_by))) %>%
      mutate(across(.fns = is.na)) %>%
      summarise(across(.fns = sum))
  }

  # plot

  if(is.null(group_by)) {
    plot <- nas %>%
      pivot_longer(cols = everything(),
                   names_to = "var",
                   values_to = "value") %>%
      ggplot(aes(x = var, y = value)) +
      geom_col(fill = "steelblue") +
      labs(y = "number of NA") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, face = "bold"),
            strip.text = element_text(face = "bold"))
  } else {
    rhs <- paste(group_by, collapse = "+")

    form <- paste0(". ~", rhs)

    plot <- nas %>%
      pivot_longer(cols = -all_of(group_by),
                   names_to = "var",
                   values_to = "value") %>%
      ggplot(aes(x = var, y = value)) +
      geom_col(fill = "steelblue") +
      labs(y = "number of NA") +
      facet_wrap(form, scales = "free_x") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, face = "bold"),
            strip.text = element_text(face = "bold"))

  }

  print(plot)
  return(nas)

}
