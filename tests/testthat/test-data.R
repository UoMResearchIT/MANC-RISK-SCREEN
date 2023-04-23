test_that("input_config_table loads ok", {

  data("input_config_table")
  expect_s3_class(input_config_table,"data.frame")

  expect_no_error( load_input_config(NULL, dryrun = T) )
})

test_that("input_config_table works well with input_list", {

  expect_no_error( input_list("basic") )
  expect_no_error( input_list("advanced") )
  expect_no_error( input_list("fixed") )
})

test_that("PSA_config table loads ok", {

  data("PSA_config")
  expect_s3_class(PSA_config,"data.frame")
})

test_that("PSA_config matches input_list(basic)", {

  data("PSA_config")

  basic_inputs <- input_list("basic")
  var_names <- colnames(PSA_config)

  conflicts <- setdiff(basic_inputs, var_names)
  if (!rlang::is_empty(conflicts)) {
    warning("PSA inputs missing from PSA config: ", stringr::str_flatten_comma(conflicts))
  }

  conflicts <- setdiff(var_names, basic_inputs)
  if (!rlang::is_empty(conflicts)) {
    warning("Variables in PSA config missing from basic inputs: ", stringr::str_flatten_comma(conflicts))
  }

  expect_setequal( input_list("basic"), colnames(PSA_config) )
})

test_that("QUALY GAM model-object loads ok",{
  modQ <- load_gam_model()
  expect_s3_class(modQ,"gam")
})

test_that("QUALY GAM model-object matches PSA_config",{

  IMPLICIT <- c("alternative") # added by run_basic_model

  modQ <- load_GAM_model()
  mod_vars <- names(modQ$var.summary)

  data("PSA_config")
  psa_vars <- colnames(PSA_config)

  mapped_vars <- paste("PSA", psa_vars, sep = "_")

  conflicts <- setdiff(mod_vars, c(mapped_vars,IMPLICIT))
  if (!rlang::is_empty(conflicts)) {
    stop("Variables in GAM missing from basic inputs: ", stringr::str_flatten_comma(conflicts))
  }
  mapped_vars <- intersect(mapped_vars, psa_vars)

})

