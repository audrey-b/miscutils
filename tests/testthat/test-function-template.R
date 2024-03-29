context("function-template")

test_that("function_template returns TRUE by default", {
  expect_true(function_template())
})

test_that("function_template returns FALSE if x = FALSE", {
  expect_false(function_template(x = FALSE))
})

test_that("function_template errors x is not a FLAG", {
  expect_error(function_template(1), "^`x` must be a flag [(]TRUE or FALSE[)][.]$")
})
