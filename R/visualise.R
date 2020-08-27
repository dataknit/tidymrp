visualise_model_effect <- function(data, post_stratified_estimates) {
  post_stratified_estimates %>%
    ggplot(aes(y = mean, x = forcats::fct_inorder(state), color = "MRP estimate")) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
    geom_point(data = example_poll %>%
                 group_by(state, supports_ALP) %>%
                 summarise(n = n()) %>%
                 group_by(state) %>%
                 mutate(prop = n/sum(n)) %>%
                 filter(supports_ALP==1),
               aes(state, prop, color = "Raw data"))
}
