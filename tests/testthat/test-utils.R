
test_that("parse_units works", {

  expect_error(parse_units(0, '5yr', 'foo'))
  expect_warning(parse_units(0, 'foo', 'ui2psa'))

  # back-and-forth conversion: exponential-parameter <-> 5-year-survival
  x <- c(0.7, 0.8, 0.9)
  expect_equal(x, parse_units(parse_units(x, "5yr", "ui2psa"), "5yr", "psa2ui") )

  expect_equal(x, parse_units(x, "mm", "ui2psa"))
})

# NOTE: `load_*_gam`, `input_list`, and `model_input_names` are tested
# (indirectly) in test-data.R

test_that("model_input_names", {
  expect_equal(c("PSA_foo","PSA_bar"),model_input_names(c("foo","bar")))
})

test_that("custom_round", {
  expect_error(custom_round(seq(1,3),seq(1,2)))
  expect_no_error( custom_round(seq(1,3), 1) )
  expect_no_error( custom_round(seq(1,3), step = NULL) )
  expect_no_error( custom_round(seq(1,3), c(NA,1,0.1)) )

  expect_equal(custom_round(1.234, op = round, digits = 2), 1.2)
  expect_equal(custom_round(1.8, op = floor, digits = 1), 1)
  expect_equal(custom_round(1.234, op = ceiling, digits = 2), 1.3)

  expect_equal(custom_round(1.234, op = round, digits = 2), 1.2)
})
