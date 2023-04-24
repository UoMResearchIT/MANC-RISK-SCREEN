
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

  data("cost_model_obj")
  mod_vars <- names(cost_model_obj$var.summary)

  ui_vars <- input_list("basic")
  mapped_vars <- model_input_names(ui_vars)

  mod_vars <- setdiff(mapped_vars, mod_vars)

  # (Possibly more elaborate rules here in the future)

  conflicts <- setdiff(mapped_vars, mod_vars)
  if ( !rlang::is_empty(conflicts) ) {
    stop("Variables in GAM missing from basic inputs: ",
         stringr::str_flatten_comma(conflicts))
  }

})
