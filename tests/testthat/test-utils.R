test_that("get_independent_variables returns correct independent variables", {

  dat <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10), d = rnorm(10),
                    g = rep(1:5, 2),
                    y = rnorm(10))
  mock_fit <- brms::brm(y ~ a + b*c + s(d) + (1 | g), dat,
                        mock_fit = 1, backend = "mock", rename = FALSE)
  result <- get_independent_variables(mock_fit)
  expected_result <- c("a", "b", "c", "g", "d")

  expect_equal(result, expected_result)
})
