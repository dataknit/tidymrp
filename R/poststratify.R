#' Get poststratified estimates from a model
#'
#' @param model
#' @param estimates_by
#' @param weight_column
#' @param lower_confidence
#' @param upper_confidence
#' @param poststratification_frame
#' @param allow_new_levels
#' @param draws predicted (default) or fitted. predicted incorporates all uncertainty.
#'
#' @return
#' @export
#'
#' @examples
poststratify <- function(model, poststratification_frame, estimates_by, weight_column = "n", lower_confidence = 0.025, upper_confidence = 1-lower_confidence, allow_new_levels = FALSE,
                         draws = "predicted") {

  model_dependent_variables <- names(model$data) %>%
    str_remove(model$formula[[1]][[2]] %>%
                 as.character()
    ) %>%
    .[. != ""]

  # warning for estimates_by not being in the model
  # if(setdiff(as.vector({{ estimates_by }}), model_dependent_variables) %>% length() >= 1) {
  #   stop("estimates_by is not a dependent variable of in the model")
  # }

  # aggregate away any unused variables in poststratification frame
  reduced_frame <- poststratification_frame %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(model_dependent_variables)), dplyr::across({{ estimates_by }})) %>%
    dplyr::summarise(n = sum({{ weight_column }}),
                     .groups = "drop")

  if(draws == "predicted") {
    draws <- model %>%
      tidybayes::add_predicted_draws(newdata = reduced_frame,
                                     value = "strata_prediction")
  } else if(draws == "fitted") {
    draws <- model %>%
      tidybayes::add_fitted_draws(newdata = reduced_frame,
                                  value = "strata_prediction")
  } else {
    stop("'draws' parameter is invalid. It must be 'predicted' or 'fitted'.")
  }

  if(any(is.na(draws$strata_prediction))) {
    warning("Draws contain NA values.")
  }

  poststratified_estimates <- draws |>
    collect_draws_by_strata(strata_prediction, {{ estimates_by }}) %>%
    dplyr::group_by(dplyr::across({{ estimates_by }})) %>%
    dplyr::summarise(
      # x = quantile(pop_prediction, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75),
      estimate_sum = mean(division_prediction, na.rm = TRUE),
      estimate_sum_lower = stats::quantile(division_prediction, 0.025, na.rm = TRUE),
      estimate_sum_upper = stats::quantile(division_prediction, 0.975, na.rm = TRUE),
      .groups = "drop") %>%
    dplyr::left_join(reduced_frame %>%
                       dplyr::group_by(dplyr::across({{ estimates_by }})) %>%
                       dplyr::summarise(n = sum(n),
                                        .groups = "drop")) %>%
    dplyr::mutate(estimate_mean = estimate_sum/n,
                  estimate_mean_lower = estimate_sum_lower/n,
                  estimate_mean_upper = estimate_sum_upper/n)

  poststratified_estimates %>%
    dplyr::relocate(n, .after = tidyselect::last_col())
}

#' Collect draws by strata
#'
#' Usually used internally by poststratify. Provides draws, grouped by
#'
#' @param draws
#' @param strata_prediction
#' @param estimates_by
#'
#' @return
#' @export
#'
#' @examples
collect_draws_by_strata <- function(draws, strata_prediction, estimates_by) {
  draws %>%
    dplyr::mutate(strata_prediction = strata_prediction*n) %>%
    dplyr::group_by(dplyr::across({{ estimates_by }}), .draw) %>%
    dplyr::summarise(division_prediction = sum(strata_prediction),
                     .groups = "drop")
}

#' Create poststratification frame from a population level survey such as a census
#'
#' @param census
#' @param strata_variables
#' @param weight_column
#'
#' @return
#' @export
#'
#' @examples
create_poststratification_frame <- function(census, strata_variables, weight_column = 1) {
  census %>%
    dplyr::mutate(weight = {{ weight_column }}) %>%
    dplyr::group_by(dplyr::across({{ strata_variables }})) %>%
    dplyr::summarise(n = sum(weight),
                     .groups = "drop")
}


#' Adds a population_proportion column to a poststratification frame.
#'
#' @param poststratification_frame
#' @param model_variables
#' @param estimates_by
#' @param weight_column
#'
#' @return
#' @export
#'
#' @examples
add_proportion <- function(poststratification_frame, model_variables, estimates_by, weight_column) {

  # variable_group <- poststratification_frame %>%
  #   dplyr::select( {{ model_variables }} ) %>%
  #   dplyr::select( -{{ estimates_by }} ) %>%
  #   names()

  # dataset %>%
  #   ungroup() %>%
  #   dplyr::group_by(dplyr::across({{ variable_group_3 }})) %>%
  #   dplyr::summarise(result = max(col_4))

  results_by_totals <- poststratification_frame %>%
    dplyr::group_by(dplyr::across({{ estimates_by }})) %>%
    dplyr::summarise(population_total_sum = sum({{ weight_column }}),
                     .groups = "drop")

  poststratification_frame %>%
    dplyr::left_join(results_by_totals) %>%
    dplyr::group_by(dplyr::across({{ estimates_by }}), dplyr::across({{ model_variables }}), population_total_sum) %>%
    dplyr::summarise(population_total = sum({{ weight_column }}), .groups = "drop") %>%
    dplyr::mutate(strata_proportion = population_total/population_total_sum) %>%
    select(-population_total_sum)
  # dplyr::mutate(population_proportion = population_total/population_total_sum) %>%
  # population
  # dplyr::group_by()
  #           population_proportion = {{ weight_column }}/population_total_sum,
  #           .groups = "drop")
}
