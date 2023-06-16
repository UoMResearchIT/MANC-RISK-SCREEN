## Model generated data

-   `PSA_config.R` (formerly `Model Core Files/Create PSA values.R`) defines parameters for sensitivity analysis, and writes down the table of model runs `data/PSA_config.rda`

- TODO: code that fits GAM model and writes `QALYmodelslim.RDS` should be moved here.

## UI configuration workflow

-   `input_config.tsv` holds the (editable) list of model inputs, input-types, limits, and their position within input groups.

-   `parse_ui_table.R` reads this table, and saves a parsed version in `data/input_config_table.rda`. In the process, it generates code for the UI (`R/auto_generated_ui.R`), that matches the provided input configuration.

> NOTE: `R/auto_generated_ui.R` will probably be replaced by a set of module scripts, the idea is that some (if not all) of these can later be customized.

-   `get_PSA_input_limits.R` looks at the model runs `PSA_config.R` and uses them to set soft bounds on those variables, results are written back to `input_config_table.rda`
