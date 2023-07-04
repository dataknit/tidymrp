test_that("get_independent_variables returns correct independent variables", {

  dat <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10), d = rnorm(10),
                    g = rep(1:5, 2),
                    y = rnorm(10))

  # Create a mock model object for testing
  mock_fit <- brms::brm(y ~ a + b*c + s(d) + (1 | g), dat, mock_fit = 1, backend = "mock", rename = FALSE)

  # Call the function and get the result
  result <- get_independent_variables(mock_fit)

  # Define the expected result
  expected_result <- c("a", "b", "c", "g", "d")

  # Assert that the result matches the expected result
  expect_equal(result, expected_result)
})
