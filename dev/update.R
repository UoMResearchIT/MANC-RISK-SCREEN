# Updates UI and run-time configuration tables based on `data-raw/input_config.tsv`

golem::detach_all_attached()
devtools::load_all()

# Update UI based on `data-raw/input_config.tsv`
source("dev/parse_ui_table.R")

# Update PSA_config table (used to set soft bounds for basic inputs)
# TODO: make sure generated runs are identical to those actually used to train the GAM's
source("data-raw/PSA_config.R")
draw_psa_runs(version = '1.1', 10000, psa_prefix = FALSE, write_out=TRUE )

# Update basic input soft bounds based on PSA_config, update `input_config_table`
source("dev/get_PSA_input_limits.R")

# TODO: at some point both forks should strive to keep a coherent structuer
qualy_model_obj <- readRDS("Model Core Files/QALYmodelslim.RDS")
usethis::use_data(qualy_model_obj, internal = F, overwrite = T)

cost_model_obj <- readRDS("Model Core Files/costmodelslim.RDS")
usethis::use_data(cost_model_obj, internal = F, overwrite = T)
