
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MancRiskScreenUI

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

MancRiskScreenUI is a shiny interface for the Manchester Breast Cancer
Model, developed by Ewan Gray, Anna Donten, and Katherine Payne and
validated by Stuart Wright, Gabriel Rogers, Katherine Payne, and Rob
Hainsworth at the Manchester Centre for Health Economics.

The full model is hosted in Stuart Wright’s parent
[repo](https://github.com/stuwrighthealthecon/MANC-RISK-SCREEN). This
fork is dedicated to a `Shiny` app that actually runs a couple of
meta-models: `data/cost_model_obj.rda`, and `data/qualy_model_obj.rda`.
These are Generalized Additive Model (GAM) objects, trained on a set
(`data/PSA_config.rda`) of Monte Carlo runs on the actual model.

## Installation (for development)

After cloning the repo, open the `MancRiskScreenUI.Rproj` project in
`Rstudio`, then:

``` r
# Install package dependencies
packrat::restore()

# Test
source("dev/run_dev.R", echo = TRUE)
```

## App structure

The bulk of the UI is written programatically by
[`parse_ui_table`](dev/parse_ui_table.R), based on the *UI configuration
table* [`input_config.tsv`](data-raw/input_config_table.R). The results
are the code at the core of the UI
[`auto_generated_ui.R`](R/auto_generated_ui.R), and the parsed run-time
configuration table `data/input_config_table.rda`.

### UI configuration table

The UI configuration table holds the complete list of model inputs,
along with their details, and how they are divided into input-groups
(parsed as tabs):

- `group`: rows with a non-empty `group` and `description` will act as
  *group headers* and include all input *elements* below until the next
  header[^1].

- `id`: unique input element ID, passed e.g. to `numericInput("id",...)`

- `type`: one of {`numeric`, `text`, `checkbox`, `slider`, `matrix`,
  …}[^2].

- `basic`: a boolean option (ticked `x` for `TRUE`). The table contains
  a list of all inputs required to run the complete model, but the GAMs
  are trained only on a subset of *basic inputs*. [^3]

- `fixed`: similar to the `basic`, aimed to filter out input elements
  from the advanced model (currently has no effect).

- `default`: default element value. Note that in some cases (e.g. matrix
  inputs) the value is written as `R` code, that will be evaluated
  during parsing.

- `rel_min`, `rel_max`: used to specify rank ordering among inputs,
  e.g. if input `A` has `B` as `rel_min`, a warning will be raised if at
  any point `A` \< `B`. During parsing, these relations are encoded in
  the boolean matrix `relative_matrix.rda` and the fields in
  `input_config_table` cleared. These are later repopulated by
  [`get_PSA_input_limits`](dev/get_PSA_input_limits.R) to work as soft
  limits beyond which an “extrapolating-beyond-data” warning is issued.

- `abs_min`, `abs_max`: hard limits for slider inputs.

- `step`: increment size for slider inputs.

- `unit`: character keys used to have UI variable representations which
  are different from those of the underlying model. See `parse_units` in
  [`utils.R`](R/utils.R). Recognized keys include `5yr` (5 year survival
  rate), `rel` (relative increments to default value), and `pm`
  (promille).

- `description`: displayed element label.

- `notes`: printed as comments in the
  [`auto_generated_ui.R`](R/auto_generated_ui.R).

## Updating the app

\[!WARNING\] The app has no hard dependencies on the parent repo (the
code is too poorly structured to be reusable, and to make
change-tracking meaningful). So relevant changes in the
`Model Core Files` directory have to be tracked down (by hand) and
replicated into the app code. These include:

- Monte Carlo parameter variations found in files:

  - `Model Core Files/MANC-RISK-SCREEN GAM Version [..].R`
  - `Model Core Files/MANC-RISK-SCREEN PSA Standard Version [..].R`
  - `Model Core Files/MANC-RISK-SCREEN PSA Wide Version [..].R`

  Must be mirrored in [`PSA_config.R`](data-raw/PSA_config.R) and in the
  call to `draw_psa_runs` in [`update.R`](dev/update.R). This call
  generates the `PSA_config.rda` table, that should (ideally) be
  identical to the one used to train the GAMs. The table is used by
  `get_PSA_input_limits.R` to set soft bounds on the input variables,
  which are written back to `input_config_table.rda`

- [`update.R`](dev/update.R) should take care of copying GAM model
  objects `Model Core Files/*.RDS` as corresponding `data/*.rda` files.
  However, this will not account for changes in the model input
  parameters. Run the tests in
  [`test-data.R`](tests/testthat/test-data.R) (or a general
  `devtools::check()`) to verify that all required inputs are listed as
  *basic* and will be available in the UI (see above).

  If changes on the UI configuration table are required
  [`input_config.tsv`](data-raw/input_config_table.R) these will **not**
  be reflected in the app, until [`update.R`](dev/update.R) is used to
  parse the table and update the UI code.

- [`costs_laudicella.R`](R/costs_laudicella.R) contains code that is
  repeated in all (?) model variations. It is **not** used for the
  *basic* model.

The process to merge changes from the parent repo is thus the following:

1.  Fetch and merge the latest changes:

``` sh
git remote add upstream git@github.com:stuwrighthealthecon/MANC-RISK-SCREEN.git
git fetch upstream
git merge upstream/main
```

2.  Resolve any conflicts manually, and `git rebase --continue`, if
    required.

3.  Update `data/*.rda` GAM model copies from the (updated)
    `Model Core Files/*.RDS` using:

``` r
source("dev/update.R", echo = TRUE)
```

4.  Manually track down and apply the changes outlined above.

5.  Update the UI again, and test

``` r
source("dev/update.R", echo = TRUE)
devtools::check()
source("dev/run_dev.R", echo = TRUE)
```

[^1]: Input elements within the `#Other` group (and in fact any group
    with a name starting with `#`) will not be written to the
    `auto_generated_ui.R`, but will still be listed in the run-time
    configuration table. The idea is that corresponding UI elements can
    be placed freely (manually) in the [app_ui](R/app_ui.R).

[^2]: all *basic* (i.e. currently used) inputs are numeric, so even
    though parsing / rendering should work for other input types, some
    work might be necessary to actually get these types working as model
    inputs.

[^3]: Currently all input elements are *generated* (i.e. written to the
    UI) but all except for those ticked as *basic* are hidden on
    runtime. The idea is to one day have an *advanced* UI mode that runs
    the actual model for a full set of inputs. If performance (refresh
    rate) becomes an issue, this could be changed, e.g. using `renderUI`
    to include only the elements used for the current model.
