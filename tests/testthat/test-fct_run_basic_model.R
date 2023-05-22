test_that("GAM models work", {
  default_inputs <- parse_inputs()
  expect_no_error( run_basic_model(default_inputs) )
})
