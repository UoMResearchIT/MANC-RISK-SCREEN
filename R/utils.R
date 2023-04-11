#' Returns configurable subsets of inputs, to control app "modes"
#'
#' Relies on columns "basic", "valid", and "fixed" of internal data table
#' `input_config_table`, exported by `dev/parse_ui_table.R`
#'
#' @param key named subset of inputs: {"basic","advanced","fixed"}
#' @return list of input ids in the corresponding list
input_list <- function(key) {
  subset <- switch(key,
    "basic" = input_config_table$valid & input_config_table$basic,
    "advanced" = input_config_table$valid & !input_config_table$fixed & !input_config_table$basic,
    "fixed" = input_config_table$valid & input_config_table$fixed
  )
  return(input_config_table$id[subset])
}


custom.round <- function(x, step, op = round, digits = 4) {
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(step))

  if (is.na(x) || x == 0) {
    return(x)
  }

  if (is.na(step)) {
    step <- 10^(ceiling(log10(abs(x))) - digits)
  }
  stopifnot(step > 0)

  return(op(x / step) * step)
}
