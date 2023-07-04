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
  variable_values <- check_grouping_variable_values(model, frame)
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
  missing_variables <- setdiff(model_independent_variables, names(frame))

  if (length(missing_variables) == 0) {
    message("All variable names found in frame.")
    return(TRUE)
  } else {
    message(paste("The following variable names are missing in frame:", missing_variables))
    return(FALSE)
  }
}

#' Check grouping variable values
#'
#' @param model
#' @param frame
#'
#' @return
#' @export
#'
#' @examples
check_grouping_variable_values <- function(model, frame) {
  random_effects <- model |>
    brms::ranef() |>
    names()

  data <- model$data

  invalid_values <- c()
  # check if frame contains any values that are not present in data[[effect]]
  for (effect in random_effects) {
    effect_levels <- levels(data[[effect]])
    frame_values <- frame[[effect]]
    if (any(!frame_values %in% effect_levels)) {
      invalid_values <- frame_values[!frame_values %in% effect_levels]
      message(paste0('Invalid values found in variable ', effect, ': ', invalid_values |> unique()))
    }
  }

  if(length(invalid_values) > 0) {
    return(FALSE)
  }
  else {
    message("All grouping values found in frame.")
    return(TRUE)
  }
}
