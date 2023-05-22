#' Use bounds from `PSA_config` to define soft limits in `input_config_table`
#'
#' @return Overwrites columns `rel_min` and `rel_max` of `input_config_table`
#'
get_PSA_input_limits <- function(.MIN_STEPS = 10) {

  data("input_config_table")
  data("PSA_config")

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
  out_of_bounds <- !(def > lo & def < hi)

  if ( any(out_of_bounds) ) {
    warning('Inconsistent PSA range for ',
            stringr::str_flatten_comma(var_names[out_of_bounds]))
  }

  # Handle unit-conversion cases (mismatch between UI and PSA_config)
  units <- input_config_table[var_names, "unit"]
  lo <- parse_units(lo, units, direction = 'psa2ui')
  hi <- parse_units(hi, units, direction = 'psa2ui')

  # Make sure lo < hi (might not be the case after unit conversion)
  list[lo, hi] <- list( pmin(lo, hi), pmax(lo, hi) )

  step <- input_config_table[var_names, "step"]

  steps_too_small <- (hi - lo) / step < .MIN_STEPS
  steps_too_small[is.na(steps_too_small)] <- FALSE

  if ( any(steps_too_small) ) {
    dx <- (hi - lo)[steps_too_small] / .MIN_STEPS
    step[steps_too_small] <- 10^(ceiling(log10(dx) - 1))

    warning('Reduced step size for ',
            stringr::str_flatten_comma(var_names[steps_too_small]))
  }

  # Round to nice values
  lo <- mapply(custom_round, lo, step, MoreArgs = list(op = ceiling))
  hi <- mapply(custom_round, hi, step, MoreArgs = list(op = floor))

  input_config_table[var_names, "rel_min"] <- lo
  input_config_table[var_names, "rel_max"] <- hi

  usethis::use_data(input_config_table, internal = FALSE, overwrite = TRUE)
}
get_PSA_input_limits()
