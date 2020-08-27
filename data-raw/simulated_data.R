# example_census <- read.csv("https://github.com/RohanAlexander/mrp_workshop/blob/master/outputs/data/census_data.csv")

region_options <- c("A", "B", "C", "D", "E")
age_group_options <- c("16_to_24", "25_to_34", "35_to_44", "45_to_54", "55_to_64", "65_plus")
age_options <- 16:80
ethnicity_options <- 1:5
gender_options <- c("male", "female", "other")


census_size <- 1000000

example_census <- tibble::tibble(
  region = sample(region_options, size = census_size, replace = TRUE),
  age_group = sample(age_group_options, size = census_size, replace = TRUE),
  ethnicity = sample(ethnicity_options, size = census_size, replace = TRUE),
  gender = sample(gender_options, size = census_size, replace = TRUE, prob = c(0.48, 0.48, 0.04))
) %>%
  dplyr::group_by(region, age_group, ethnicity, gender) %>%
  dplyr::summarise(population_total = dplyr::n(),
                  .groups = "drop")

usethis::use_data(example_census, overwrite = TRUE)




survey_size <- 17000


example_survey <- tibble::tibble(
  region = sample(region_options, size = survey_size, replace = TRUE),
  age = sample(age_options, size = survey_size, replace = TRUE),
  ethnicity = sample(ethnicity_options, size = survey_size, replace = TRUE),
  gender = sample(gender_options, size = survey_size, replace = TRUE, prob = c(0.48, 0.48, 0.04)) %>%
    forcats::as_factor(),
  non_negative_response = rgamma(
    n = survey_size,
    shape = 1*age/max(age_options),
    scale = 1),
  binary_response = rbinom(
    size = 1,
    n = survey_size,
    p = age/max(age_options)) %>%
    as.numeric()
)


usethis::use_data(example_survey, overwrite = TRUE)
