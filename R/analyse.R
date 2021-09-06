#' Create poststratified draws from a model
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
