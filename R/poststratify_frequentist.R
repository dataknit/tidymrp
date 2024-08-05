#' Poststratify frequentist
#'
#' @param model
#' @param poststratification_frame
#' @param estimates_by
#' @param weight_column
#' @param lower_confidence
#' @param upper_confidence
#' @param draws
#'
#' @return
#' @export
#'
#' @examples
poststratify_frequentist <- function(model,
                                     poststratification_frame,
                                     estimates_by,
                                     weight_column = n,
                                     lower_confidence = 0.025,
                                     upper_confidence = 1-lower_confidence,
                                     draws = "predicted") {

  draws_mean <- get_results_for_all_strata(model, poststratification_frame)

  end_results <- all_strata_results_to_more_results(draws_mean,
                                                    poststratification_frame,
                                                    estimates_by)

  return(end_results)
}

#' get_results_for_all_strata
#'
#' @param model
#' @param poststratification_frame
#'
#' @return
#' @export
#'
#' @examples
get_results_for_all_strata <- function(model,
                                       poststratification_frame,
                                       slice_size = NULL,
                                       ...) {
  model_independent_variables <- get_independent_variables({{ model }})

  reduced_frame <- poststratification_frame |>
    dtplyr::lazy_dt() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(model_independent_variables))) |>
    dplyr::summarise(n = sum(n),
                     .groups = "drop") |>
    tibble::as_tibble()


  if (is.null(slice_size)) {
    draws <- tidybayes::add_predicted_draws(newdata = reduced_frame,
                                            object = model,
                                            value = "strata_prediction",
                                            ...)
    draws_mean <- draws |>
      dplyr::group_by(dplyr::across(dplyr::all_of(model_independent_variables))) |>
      dplyr::summarise(estimate_mean = mean(strata_prediction, na.rm = TRUE),
                       estimate_sum_lower = stats::quantile(strata_prediction, 0.05, na.rm = TRUE),
                       estimate_sum_upper = stats::quantile(strata_prediction, 0.95, na.rm = TRUE),
                       strata_sd = sd(strata_prediction, na.rm = TRUE),
                       .groups = "drop")
  } else {
    draws_mean <- tibble::tibble()
    num_rows <- nrow(reduced_frame)

    for (group_start in seq(1, num_rows, by = slice_size)) {
      group_end <- min(group_start + slice_size - 1, num_rows)

      message(glue::glue("{group_start} to {group_end}: {round(100 * (group_start - 1) / num_rows, 2)}%."))

      draws_new <- reduced_frame |>
        dplyr::slice(group_start:group_end) |>
        tidybayes::add_predicted_draws(object = model,
                                       value = "strata_prediction",
                                       ...)

      draws_mean_new <- draws_new |>
        dplyr::group_by(dplyr::across(dplyr::all_of(model_independent_variables))) |>
        dplyr::summarise(estimate_mean = mean(strata_prediction, na.rm = TRUE),
                         estimate_sum_lower = stats::quantile(strata_prediction, 0.05, na.rm = TRUE),
                         estimate_sum_upper = stats::quantile(strata_prediction, 0.95, na.rm = TRUE),
                         strata_sd = sd(strata_prediction, na.rm = TRUE),
                         .groups = "drop")

      draws_mean <- draws_mean |>
        dplyr::bind_rows(draws_mean_new)
    }
  }

  return(draws_mean)

}

#' all_strata_results_to_more_results
#'
#' @param strata_results
#' @param frame
#' @param estimates_by
#'
#' @return
#' @export
#'
#' @examples
all_strata_results_to_more_results <- function(strata_results, frame, estimates_by) {
  end_results <- frame |>
    dplyr::left_join(strata_results) |>
    dplyr::group_by(dplyr::across({{ estimates_by }})) |>
    dplyr::summarise(strata_prediction = sum(estimate_mean * n)/sum(n),
                     standard_error = sqrt(sum(n^2 * strata_sd^2))/sum(n),
                     .groups = "drop") |>
    dplyr::mutate(strata_prediction_lower = strata_prediction - 1.96 * standard_error,
                  strata_prediction_upper = strata_prediction + 1.96 * standard_error)

  return(end_results)
}
