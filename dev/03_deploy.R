
UPDATE_TSV = T
UPDATE_CONFIG = T
CHECK = F
DEPLOY = F

# Read boolean UPDATE_TSV, UPDATE_CONFIG, CHECK, DEPLOY
if (file.exists("dev/deploy_config.R")) source("dev/deploy_config.R")

golem::detach_all_attached()
if ( UPDATE_CONFIG ) { file.remove("R/auto_generated_ui.R") }
devtools::load_all()

# Update csv copy of shared "inputs.xlsx"
if ( UPDATE_TSV ) {
  config_table <- readxl::read_excel("../UoM/P086_Manc_Risk_Screen/dev/inputs.xlsx", sheet = "inputs")
  write.table(config_table, "data-raw/input_config.tsv", row.names = F, sep = "\t", na = "", quote = F)
}

# Read input configuration file, updates global `input_config_table`
if ( UPDATE_CONFIG ) {

  # Update UI based on `data-raw/input_config.tsv`
  source("data-raw/parse_ui_table.R")

  # Update PSA_config table (used to set soft bounds for basic inputs)

  source("data-raw/PSA_config.R")
  draw_psa_runs(version = '1.1', 1000, psa_prefix = FALSE, write_out=TRUE )
  # batch-run model, fit GAM?

  # Update basic input soft bounds based on PSA_config, update `input_config_table`
  source("data-raw/get_PSA_input_limits.R")

  # PROVISIONAL?:
  qualy_model_obj <- readRDS("Model Core Files/QALYmodelslim.RDS")
  usethis::use_data(qualy_model_obj, internal = F, overwrite = T)

  cost_model_obj <- readRDS("Model Core Files/costmodelslim.RDS")
  usethis::use_data(cost_model_obj, internal = F, overwrite = T)
}

# Run checks
if ( CHECK ) {
  devtools::check()
  # rhub::check_for_cran()
}

if ( DEPLOY ) {

  # Deploy

  # ## Local, CRAN or Package Manager ----
  # ## This will build a tar.gz that can be installed locally,
  # ## sent to CRAN, or to a package manager
  # devtools::build()
  #
  # ## RStudio ----
  # ## If you want to deploy on RStudio related platforms
  # golem::add_rstudioconnect_file()
  # golem::add_shinyappsio_file()
  # golem::add_shinyserver_file()
  #
  # ## Docker ----
  # ## If you want to deploy via a generic Dockerfile
  # golem::add_dockerfile_with_renv()
  #
  # ## If you want to deploy to ShinyProxy
  # golem::add_dockerfile_with_renv_shinyproxy()
  #
  #
  # # Deploy to Posit Connect or ShinyApps.io
  # # In command line.
  # rsconnect::deployApp(
  #   appName = desc::desc_get_field("Package"),
  #   appTitle = desc::desc_get_field("Package"),
  #   appFiles = c(
  #     # Add any additional files unique to your app here.
  #     "R/",
  #     "inst/",
  #     "data/",
  #     "NAMESPACE",
  #     "DESCRIPTION",
  #     "app.R"
  #   ),
  #   appId = rsconnect::deployments(".")$appID,
  #   lint = FALSE,
  #   forceUpdate = TRUE
  # )

  shinysender::ss_uploadAddin()
}

