library(gsubfn)

#' Use bounds from `PSA_config` to define soft limits in `input_config_table`
#'
#' @return Overwrites columns `rel_min` and `rel_max` of `input_config_table`
#'
get_PSA_input_limits <- function(.MIN_REL_STEPS = 10,
                                 .MIN_ABS_STEPS = 50) {

  input_config_table <- .pkgenv$input_config_table
  PSA_config <- .pkgenv$PSA_config

  var_names <- colnames(PSA_config)
  basic_inputs <- input_list("basic")
  var_names <- intersect(var_names, basic_inputs)

  #idx <- match(var_names, input_config_table$id)
  #rownames(input_config_table)
  # PSA_col_idx <- match(var_names, var_names)

  # Set limits as PSA_config column's min/max
  lo <- apply(PSA_config[, var_names], 2, min)
  hi <- apply(PSA_config[, var_names], 2, max)

  def <- input_config_table[var_names, "default"]
  abs.lo <- input_config_table[var_names, "abs_min"]
  abs.hi <- input_config_table[var_names, "abs_max"]

  # Handle unit-conversion cases (mismatch between UI and PSA_config)
  units <- input_config_table[var_names, "unit"]
  lo <- parse_units(lo, units, direction = 'psa2ui', def)
  hi <- parse_units(hi, units, direction = 'psa2ui', def)

  # Make sure lo < hi (might not be the case after unit conversion)
  list[lo, hi] <- list( pmin(lo, hi), pmax(lo, hi) )

  abs.lo <- pmin(lo, abs.lo)
  abs.hi <- pmax(hi, abs.hi)

  out_of_bounds <- !(def > abs.lo & def < abs.hi)
  if ( any(out_of_bounds) ) {
    warning('Inconsistent PSA range for ',
            stringr::str_flatten_comma(var_names[out_of_bounds]))
  }

  step <- input_config_table[var_names, "step"]
  names(step) <- var_names

  nice_step <- function(x) {10^(floor(log(x, base = 10)))}
  step <- nice_step(step)

  min_step <- pmin((hi - lo) / .MIN_REL_STEPS, (abs.hi - abs.lo) / .MIN_ABS_STEPS, na.rm = TRUE)
  min_step <- nice_step(step)

  steps_too_small <- is.na(step) | (min_step < step)

  if ( any(steps_too_small) ) {
    step[steps_too_small] <- min_step[steps_too_small]

    warning('Reduced step size for ',
            stringr::str_flatten_comma(var_names[steps_too_small]))
  }

  # Round to nice values
  lo <- mapply(custom_round, lo, step, MoreArgs = list(op = ceiling))
  hi <- mapply(custom_round, hi, step, MoreArgs = list(op = floor))

  input_config_table$rel_min = as.numeric(input_config_table$rel_min)
  input_config_table$rel_max = as.numeric(input_config_table$rel_max)

  input_config_table[var_names, "rel_min"] <- lo
  input_config_table[var_names, "rel_max"] <- hi
  input_config_table[var_names, "step"] <- step

  usethis::use_data(input_config_table, internal = FALSE, overwrite = TRUE)
  data("input_config_table", envir = .pkgenv)
}
get_PSA_input_limits()
