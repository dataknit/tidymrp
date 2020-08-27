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
    dplyr::summarise(population_total = sum(weight),
                     .groups = "drop")
}


#' Create post-stratified draws from a model
#'
#' @param model
#' @param poststratification_frame
#'
#' @return
#' @export
#'
#' @examples
get_poststratified_draws <- function(model, new_data, weight_column = population_total) {
  poststratified_draws <- model %>%
    tidybayes::add_predicted_draws(newdata = new_data) %>%
    # rename(prediction = .prediction) %>%
    mutate(prediction_sum = .prediction * {{ weight_column }}) %>%
    ungroup()

  poststratified_draws
}


get_strata_estimates <- function(model, new_data, group_variables, weight_column = population_total, lower_confidence = 0.025, upper_confidence = 1-lower_confidence) {

  model %>%
    tidybayes::add_predicted_draws(newdata = new_data) %>%
    ungroup() %>%
    dplyr::group_by(dplyr::across({{ group_variables }}), .draw) %>%
    dplyr::summarise(.prediction = mean(.prediction), .groups = "drop") %>%
    ungroup() %>%
    dplyr::group_by(dplyr::across({{ group_variables }})) %>%
    dplyr::summarise(
      mean = mean(.prediction),
      lower = quantile(.prediction, lower_confidence),
      upper = quantile(.prediction, upper_confidence),
      .groups = "drop") %>%
    ungroup()
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
add_proportion <- function(poststratification_frame, model_variables, estimates_by, weight_column = population_total) {

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
    dplyr::group_by(dplyr::across({{ model_variables }}), population_total_sum) %>%
    dplyr::summarise(population_total = sum(population_total)) %>%
    dplyr::mutate(strata_proportion = population_total/population_total_sum) %>%
    select(-population_total_sum)
    # dplyr::mutate(population_proportion = population_total/population_total_sum) %>%
    # population
    # dplyr::group_by()
    #           population_proportion = {{ weight_column }}/population_total_sum,
    #           .groups = "drop")
}

get_poststratified_estimates <- function(model, new_data, model_variables, estimates_by, weight_column = population_total, lower_confidence = 0.025, upper_confidence = 1-lower_confidence) {

  # add way to automatically get model variables from model

  poststratification_frame <- add_proportion(new_data, {{ model_variables }}, {{ estimates_by }})

  model %>%
    tidybayes::add_predicted_draws(newdata = poststratification_frame) %>%
    ungroup() %>%
    rename(strata_prediction = .prediction) %>%
    dplyr::mutate(contributing_prediction = strata_prediction*strata_proportion) %>%
    dplyr::group_by(dplyr::across({{ estimates_by }}), .draw) %>%
    dplyr::summarise(pop_prediction = sum(contributing_prediction),
                     .groups = "drop") %>%
    ungroup() %>%
    dplyr::group_by(dplyr::across({{ estimates_by }})) %>%
    dplyr::summarise(
      mean_estimate = mean(pop_prediction),
      lower_estimate = quantile(pop_prediction, lower_confidence),
      upper_estimate = quantile(pop_prediction, upper_confidence),
      .groups = "drop") %>%
    ungroup()

}

# poststratification_frame_2 <- add_proportion(poststratification_frame,
#                                              model_variables = c(age_group, region),
#                                              estimates_by = region)
#
# hi <- binary_model_1 %>%
#   tidybayes::add_predicted_draws(newdata = poststratification_frame_2) %>%
#   ungroup() %>%
#   rename(strata_prediction = .prediction) %>%
#   dplyr::mutate(contributing_prediction = strata_prediction*strata_proportion) %>%
#   dplyr::group_by(region, .draw) %>%
#   dplyr::summarise(pop_prediction = sum(contributing_prediction), .groups = "drop")
