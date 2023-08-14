#' Check frame-model alignment
#'
#' @param model
#' @param frame
#'
#' @return
#' @export
#'
#' @examples
check_frame_model_alignment <- function(model, frame) {
  variable_names <- check_variable_names(model, frame)
  variable_values <- check_character_variable_values(model, frame)
  if (variable_names == FALSE | variable_values == FALSE) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Check variable names
#'
#' @param model
#' @param frame
#'
#' @return
#' @export
#'
#' @examples
check_variable_names <- function(model, frame) {
  model_independent_variables <- get_independent_variables(model)
  frame_variables <- names(frame)
  variables_missing_in_frame <- setdiff(model_independent_variables, frame_variables)

  if (length(variables_missing_in_frame) == 0) {
    message("All variable names found in frame.")
    return(TRUE)
  } else {
    message(paste0("The following variable names are missing in frame: ", paste0(variables_missing_in_frame, collapse = ", "), "."))
    return(FALSE)
  }
}

#' Check character variable values
#'
#' @param model
#' @param frame
#'
#' @return
#' @export
#'
#' @examples
check_character_variable_values <- function(model, frame) {
  result <- get_character_variable_values(model, frame)

  if(anyNA(result)) {
    return(FALSE)
  } else {
    return(TRUE)
  }

}


#' Get character variable values
#'
#' @param model
#' @param frame
#'
#' @return
#' @export
#'
#' @examples
get_character_variable_values <- function(model, frame) {
  character_variables_in_model <- get_all_character_variables(model)

  character_variables_in_frame <- frame |>
    dplyr::select(names(character_variables_in_model))

  col_names <- colnames(character_variables_in_model)

  result <- tibble::tibble(column_name = character(),
                           value_in_model = character(),
                           value_in_frame = character())

  for (col_name in col_names) {
    model_values <- character_variables_in_model[[col_name]] |>
      unique()
    frame_values <- character_variables_in_frame[[col_name]] |>
      unique()
    all_values <- append(model_values, frame_values) |>
      unique()

    for (value in all_values) {
      result <- result |>
        dplyr::add_row(column_name = col_name,
                       value_in_model = dplyr::if_else(value %in% model_values, value, NA),
                       value_in_frame = dplyr::if_else(value %in% frame_values, value, NA))
    }

  }

  return(result)
}

get_all_character_variables <- function(model) {
  model$data |>
    dplyr::select(where(is.character))
}
