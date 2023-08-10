test_that("check_variable_names correctly identifies variable name alignment", {
  dat <- data.frame(a = letters[1:10], b = rnorm(10), c = rnorm(10), d = rnorm(10),
                    g = rep(1:5, 2),
                    y = rnorm(10))
  mock_model <- brms::brm(y ~ a + b*c + s(d) + (1 | g), dat,
                          mock_fit = 1, backend = "mock", rename = FALSE)

  expect_true(check_variable_names(mock_model, dat))
  expect_false(check_variable_names(mock_model, dat |> dplyr::select(-a)))
  expect_true(check_variable_names(mock_model, dat |> dplyr::mutate(e = 1)))
})

test_that("check_character_variable_values correctly identifies invalid values", {
  dat <- data.frame(a = letters[1:10], b = rnorm(10), c = rnorm(10), d = rnorm(10),
                    g = letters[10:1],
                    y = rnorm(10))
  model <- brms::brm(y ~ a + b*c + s(d) + (1 | g), dat, iter = 100,
                     backend = "cmdstanr", chains = 2, threads = 5, rename = FALSE)

  expect_true(check_character_variable_values(model, dat))

  invalid_dat <- dat
  invalid_dat$g[3] <- "invalid"

  expect_false(check_character_variable_values(model, invalid_dat))
})
