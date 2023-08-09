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

calculate_survey_errors <- function(survey, strata, average_catch_totals, results_by, assume_further_grouping_independent) {
  # by_ = c(set_names(quo_name(strata)), set_names(quo_name(results_by)))

  average_catch_variance <-
    survey %>%
    group_by(diarist_id, !!strata, !!!results_by) %>%
    summarise(diarist_kept_weight = sum(kept_session_weight_g*(10^(-6))),
              diarists_in_group = unique(diarists_in_group),
              diarists_in_group_with_species_catch = unique(diarists_in_group_with_species_catch)) %>%
    # left_join(average_catch_totals, by = c("strata_15", "species")) %>%
    # inner_join(average_catch_totals, by = by_, suffix = c("",".y")) %>%
    inner_join(average_catch_totals, suffix = c("",".y")) %>%
    # add_group_numbers(!!!results_by, !!strata) %>%
    group_by(!!strata, !!!results_by) %>%
    summarise(kept_weight_var_1 = sum(diarist_kept_weight-kept_weight_diarist_average)^2,
              number_unaccounted_diarists = unique(diarists_in_group)-unique(diarists_in_group_with_species_catch),
              kept_weight_var_2 = number_unaccounted_diarists * (unique(kept_weight_diarist_average)^2),
              kept_weight_var = (kept_weight_var_1 + kept_weight_var_2)/unique(diarists_in_group),
              diarists_in_group = unique(diarists_in_group),
              diarists_in_group_with_species_catch = unique(diarists_in_group_with_species_catch)
    ) %>%
    select(!!strata, !!!results_by, ends_with("var"), starts_with("diarists_in_"))

    average_catch_variance %>%
      mutate_if(is.numeric , replace_na, replace = 0) %>%
      mutate(kept_weight_sd = sqrt(kept_weight_var)) %>%
      join_by_quosure(data_right = diarists_strata_weights, var_to_impute = diarists_in_strata, left_index = !!strata, right_index = strata) %>%
      group_by(!!strata, !!!results_by) %>%
      mutate(kept_weight_se = kept_weight_sd/sqrt(diarists_in_strata)) %>%
      select(!!strata, !!!results_by, sort(tidyselect::peek_vars()), diarists_in_strata)
}

calculate_errors <- function(strata_errors, strata, results_by) {
  strata_errors %>%
    mutate(
      kept_weight_var = strata_variance * (kept_weight_diarist_average ^ 2) +
        + (strata_total^2) * kept_weight_var,
      kept_weight_var_2 = (strata_se^2) * (kept_weight_diarist_average ^ 2) +
        (strata_total^2) * (kept_weight_se ^ 2)
    ) %>%
    group_by(!!!results_by) %>%
    select(!!strata,
           kept_weight_var, kept_weight_var_2) %>%
    summarise_if(is.numeric, funs(sum(., na.rm = TRUE))) %>%
    mutate(
      kept_weight_sd = sqrt(kept_weight_var),
      kept_weight_se = sqrt(kept_weight_var_2)
    )
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
