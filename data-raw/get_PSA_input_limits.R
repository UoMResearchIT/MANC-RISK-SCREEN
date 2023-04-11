#' Use bounds from `PSA_config` to define soft limits in `input_config_table`
#'
#' @return Overwrites columns `rel_min` and `rel_max` of `input_config_table`
#'
get_PSA_input_limits <- function() {
  data("input_config_table")
  data("PSA_config")

  stopifnot(is.data.frame(PSA_config))
  stopifnot(is.data.frame(input_config_table))

  # Read input config table from INPUT_FILE
  COL_NAMES <- c("id", "type", "basic")
  stopifnot(rlang::is_empty(setdiff(COL_NAMES, colnames(input_config_table))))

  var_names <- colnames(PSA_config)

  basic_inputs <- input_list("basic")
  advanced_inputs <- input_list("advanced")

  conflicts <- setdiff(basic_inputs, var_names)
  if (!rlang::is_empty(conflicts)) {
    stop("basic inputs missing from PSA config: ", stringr::str_flatten_comma(conflicts))
  }

  conflicts <- setdiff(var_names, basic_inputs)
  if (!rlang::is_empty(conflicts)) {
    warning("Variables in PSA config missing from basic inputs: ", stringr::str_flatten_comma(conflicts))
  }

  var_names <- intersect(var_names, basic_inputs)

  idx <- match(var_names, input_config_table$id)
  rownames(input_config_table)

  PSA_col_idx <- match(var_names, var_names)

  lo <- apply(PSA_config[, var_names], 2, min)
  hi <- apply(PSA_config[, var_names], 2, max)
  step <- input_config_table[var_names, "step"]


  # Handle unit-conversion cases
  units <- as.factor(input_config_table[var_names, "unit"])
  units <- addNA(units)

  KNOWN <- c("5yr","mm","1/yr",NA)

  uk <- setdiff(units,KNOWN)
  if ( !is.empty(uk) ){
    warning("Unknown unit(s):",stringr::str_flatten_comma(dQuote(uk)))
  }

  parse_units <- function(x) {
    x <- dplyr::case_when(
      units == "5yr" ~ exp(-5*exp(x)),
      .default = x
    )
  }

  lo <- parse_units(lo)
  hi <- parse_units(hi)

  list[lo,hi] <- list(pmin(lo,hi),pmax(lo,hi))

  lo <- mapply(custom.round, lo, step, MoreArgs = list(op = ceiling))
  hi <- mapply(custom.round, hi, step, MoreArgs = list(op = floor))

  input_config_table[var_names, "rel_min"] <- lo
  input_config_table[var_names, "rel_max"] <- hi

  usethis::use_data(input_config_table, internal = F, overwrite = T)
}
get_PSA_input_limits()
