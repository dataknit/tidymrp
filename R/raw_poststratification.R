#' Calculate survey strata weights
#'
#' @param survey
#' @param strata
#' @param population_weight_column
#' @param population_dataset
#'
#' @return
#' @export
#'
#' @examples
calculate_survey_strata_weights <- function(survey, population_dataset, strata, population_weight_column) {
  survey_strata_size <- calculate_survey_strata_size(survey, {{strata}})
  population_strata_weights <- calculate_population_strata_weights(population_dataset, {{strata}}, {{population_weight_column}})
  dplyr::inner_join(survey_strata_size, population_strata_weights) |>
    dplyr::mutate(survey_strata_weight = population_in_strata/survey_in_strata,
           survey_inclusion_probability = 1/survey_strata_weight)
}

calculate_survey_strata_size <- function(survey, strata) {
  survey |>
    dplyr::group_by({{strata}}) |>
    dplyr::summarise(survey_in_strata = n()) |>
    dplyr::ungroup()
}

calculate_population_strata_weights <- function(population, strata, population_weight_column) {
  population |>
    dplyr::group_by({{strata}}) |>
    dplyr::summarise(
      population_in_strata = sum({{population_weight_column}})
    ) |>
    dplyr::ungroup()
}

#' Calculate raised totals
#'
#' @param survey
#' @param survey_strata_weights
#' @param strata
#' @param results_by
#' @param dependent_variable
#'
#' @return
#' @export
#'
#' @examples
calculate_raised_totals <- function(survey, survey_strata_weights, strata, results_by, dependent_variable) {
  survey |>
    group_by({{strata}}, {{results_by}}) |>
    summarise(dependent_variable = sum({{dependent_variable}})) |>
    ungroup() |>
    left_join(survey_strata_weights |>
                select({{strata}}, survey_strata_weight)) |>
    group_by({{strata}}, {{results_by}}) |>
    mutate(dependent_variable_raised = dependent_variable * survey_strata_weight) |>
    select(-survey_strata_weight) |>
    group_by({{results_by}}) |>
    summarise("{{dependent_variable}}_raised" := sum(dependent_variable_raised, na.rm = TRUE))
}

calculate_population_errors <- function(population_dataset, strata, bootstrap_times, population_dataset_effective_sample_size) {
  # calculate population values: mean, se, sd, variance

  ## se, sd and variance
  strata <- rlang::sym(strata)

  bootstraps <- population_dataset |>
    # group_by(!!strata) |>
    # summarise(weight = sum(weight_gross)) |>
    rsample::bootstraps(weights = weight, times = bootstrap_times)

  population_errors <- bootstraps |>
    mutate(model = map(splits, ~ summarise_on_bootstrap(.x, { strata }))) |>
    unnest(model) |>
    summarise_if(is.numeric, list(~sd(., na.rm = TRUE))) |>
    transpose_df() |>
    rename(!!strata := rowname,
           strata_se = `1`) |>
    mutate(strata_sd = strata_se * sqrt(population_dataset_effective_sample_size),
           strata_variance = strata_sd^2)

  population_errors
}
