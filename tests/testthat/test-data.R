test_that("input_config_table loads ok", {

  input_config_table <- .pkgenv$input_config_table
  expect_s3_class(input_config_table,"data.frame")

  expect_no_error( load_input_config(NULL, dryrun = T) )
})

test_that("input_config_table works well with input_list", {

  expect_no_error( input_list("basic") )
  expect_no_error( input_list("advanced") )
  expect_no_error( input_list("fixed") )
})

test_that("PSA_config table loads ok", {

  PSA_config <- .pkgenv$PSA_config
  expect_s3_class(PSA_config,"data.frame")
})

test_that("PSA_config matches input_list(basic)", {

  UNUSED = c("mcid")

  PSA_config <- .pkgenv$PSA_config

  basic_inputs <- input_list("basic")
  psa_variables <- setdiff(colnames(PSA_config),UNUSED)

  # Does not print correctly, for some reason:
  # expect_setequal( input_list("basic"), psa_variables )

  expect_subset_of(input_list("basic"), psa_variables)
  expect_subset_of(psa_variables, input_list("basic"))
})

test_that("QUALY GAM model-object loads ok",{
  qualy_model_obj <- .pkgenv$qualy_model_obj
  expect_s3_class(qualy_model_obj,"gam")
})

test_that("COSTS GAM model-object loads ok",{
  cost_model_obj <- .pkgenv$cost_model_obj
  expect_s3_class(cost_model_obj,"gam")
})

test_that("GAM model-objects match PSA_config",{

  IMPLICIT <- c("alternative") # added by run_basic_model

  qualy_model_obj <- .pkgenv$qualy_model_obj
  qualy_model_vars <- names(qualy_model_obj$var.summary)

  cost_model_obj <- .pkgenv$cost_model_obj
  cost_model_vars <- names(cost_model_obj$var.summary)

  PSA_config <- .pkgenv$PSA_config
  psa_vars <- colnames(PSA_config)
  psa_vars <- model_input_names(psa_vars) # add PSA_ prefix
  psa_vars <- append(psa_vars, IMPLICIT)

  expect_subset_of(qualy_model_vars, psa_vars)
  expect_subset_of(cost_model_vars, psa_vars)
})

