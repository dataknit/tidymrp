#' Get dependent variables from a brms model
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
get_independent_variables <- function(model) {
  dependent_variable <- model$formula[[1]][[2]] |>
    as.character()
  all_variables <- names(model$data)
  independent_variables <- all_variables |>
    purrr::keep(function(x) !stringr::str_detect(x, dependent_variable))
  return(independent_variables)
}
