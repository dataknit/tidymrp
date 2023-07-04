#' Get poststratified estimates from a model
#'
#' @param model
#' @param estimates_by
#' @param weight_column
#' @param lower_confidence
#' @param upper_confidence
#' @param poststratification_frame
#' @param draws predicted (default) or fitted. Predicted incorporates all uncertainty.
#' @param large_frame
#' @param ... Extra arguments for adding draws from tidybayes. See tidybayes::add_predicted_draws() for details.
#'
#' @return
#' @export
#'
#' @examples
poststratify <- function(model, poststratification_frame, estimates_by, weight_column = n, lower_confidence = 0.025, upper_confidence = 1-lower_confidence,
                         draws = "predicted", large_frame = FALSE, progress = FALSE, ...) {

  # prepare
  model_independent_variables <- get_independent_variables({{ model }})

  # aggregate away any unused variables in poststratification frame
  reduced_frame <- {{ poststratification_frame }} %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(model_independent_variables)), dplyr::across({{ estimates_by }})) %>%
    dplyr::summarise(n = sum({{ weight_column }}),
                     .groups = "drop")

  # do poststratification
  if (!large_frame) {
    poststratified_estimates <- perform_poststratification({{ model }}, reduced_frame, {{ estimates_by }},
                                                           {{ lower_confidence }}, {{ upper_confidence }},
                                                           {{ draws }}, ...)
  }
  else {
    poststratified_estimates <- tibble()
    values <- reduced_frame |>
      distinct(across({{ estimates_by }}))
    for(i in 1:nrow(values)) {
      if(progress) { message(paste0(round(100*i/nrow(values), 2), "%")) }
      current_values <- values[i,]
      print(paste("Poststratifying for", paste(current_values, collapse = " and ")))
      current_frame <- current_values |>
        left_join(reduced_frame, by = join_by({{ estimates_by }}))
      poststratified_estimates <- bind_rows(poststratified_estimates,
                                            perform_poststratification({{ model }}, current_frame, {{ estimates_by }},
                                                                       {{ lower_confidence }}, {{ upper_confidence }},
                                                                       {{ draws }}, ...))
    }
  }

  # clean up
  poststratified_estimates %>%
    dplyr::relocate(n, .after = tidyselect::last_col())
}


#' Perform poststratification
#'
#' @param estimate_value
#'
#' @return
#' @export
#'
#' @examples
perform_poststratification <- function(model, reduced_frame, estimates_by,
                                       lower_confidence, upper_confidence,
                                       draws, ...) {
  if(draws == "predicted") {
    draws <- tidybayes::add_predicted_draws(newdata = reduced_frame,
                                            object = model,
                                            value = "strata_prediction",
                                            ...)
  } else if(draws == "fitted") {
    draws <- tidybayes::add_epred_draws(newdata = reduced_frame,
                                        object = model,
                                        value = "strata_prediction",
                                        ...)
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
      estimate_sum_lower = stats::quantile(division_prediction, lower_confidence, na.rm = TRUE),
      estimate_sum_upper = stats::quantile(division_prediction, upper_confidence, na.rm = TRUE),
      .groups = "drop") %>%
    dplyr::left_join(reduced_frame %>%
                       dplyr::group_by(dplyr::across({{ estimates_by }})) %>%
                       dplyr::summarise(n = sum(n),
                                        .groups = "drop"),
                     by = join_by({{ estimates_by }})) %>%
    dplyr::mutate(estimate_mean = estimate_sum/n,
                  estimate_mean_lower = estimate_sum_lower/n,
                  estimate_mean_upper = estimate_sum_upper/n)

  return(poststratified_estimates)
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
    dplyr::left_join(results_by_totals, by = join_by({{ estimates_by }})) %>%
    dplyr::group_by(dplyr::across({{ estimates_by }}), dplyr::across({{ model_variables }}), population_total_sum) %>%
    dplyr::summarise(population_total = sum({{ weight_column }}), .groups = "drop") %>%
    dplyr::mutate(strata_proportion = population_total/population_total_sum) %>%
    dplyr::select(-population_total_sum)
  # dplyr::mutate(population_proportion = population_total/population_total_sum) %>%
  # population
  # dplyr::group_by()
  #           population_proportion = {{ weight_column }}/population_total_sum,
  #           .groups = "drop")
}
