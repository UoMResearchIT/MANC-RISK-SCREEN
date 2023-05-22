#' Returns configurable subsets of inputs, to control app "modes"
#'
#' Relies on columns "basic", "valid", and "fixed" of internal data table
#' `input_config_table`, exported by `dev/parse_ui_table.R`
#'
#' @param key named subset of inputs: {"basic","advanced","fixed"}
#' @return list of input ids in the corresponding list
#'
#' @noRd
input_list <- function(key) {
  subset <- switch(key,
    "basic" = input_config_table$valid & input_config_table$basic,
    "advanced" = input_config_table$valid & !input_config_table$fixed & !input_config_table$basic,
    "fixed" = input_config_table$valid & input_config_table$fixed
  )
  return(input_config_table$id[subset])
}

#' map input IDs to model variable names
#' @param ui_vars list of input ID's, e.g. input_list("basic")
#' @return variable names as used in GAM model
#'
#' @noRd
model_input_names <- function(ui_vars) {

  mapped_vars <- paste("PSA", ui_vars, sep = "_")

  # (Possibly more elaborate rules here in the future)

  return(mapped_vars)
}

#' Convert UI values to PSAmodel variables, and vice-versa
#'
#' @param x vector of input values
#' @param units {"5yr","mm","1/yr",NA} of length(x) (or scalar)
#' @param direction {'psa2ui' or 'ui2psa'}
#'
#' @return vector of converted values
#'
#' @noRd
parse_units <- function(x, units, direction) {

  stopifnot( direction %in% c('psa2ui','ui2psa') )

  KNOWN <- c("5yr", "mm", "1/yr", "\\u00a3", "NA")

  units[ is.na(units) ] <- "NA"
  units <- droplevels( as.factor(units) )
  x0 <- unlist(x)

  if ( length(units) == 1 && length(x) > 1) {
    units <- rep_len(units, length(x))
  } else {
    stopifnot(length(units) == length(x))
  }

  uk <- setdiff(units, KNOWN)
  if ( !rlang::is_empty(uk) ) {
    warning("Unknown unit(s):", stringr::str_flatten_comma(dQuote(uk)))
  }

  for ( u in levels(units) ) {

    if ( !(u %in% KNOWN) ) next

    idx <- (units == u)
    if ( !any(idx) ) next

    if ( u == "5yr" ) {
      x[idx] <- switch( direction,
                        'psa2ui' = exp( -5 * exp(x0[idx]) ),
                        'ui2psa' = log( -log(x0[idx]) / 5 ) )

    } # else if ( u == "some other unit" ) {
      #   x[idx] <- ...
  }

  return(x)
}

custom_round <- function(x, step = NULL, op = round, digits = 4) {

  stopifnot(is.numeric(x))
  stopifnot(digits > 0)

  trivial <- is.na(x) | x == 0
  if ( all(trivial) ) return(x)

  if ( is.null(step) ) step <- NA
  if ( length(x) > 1 && length(step) == 1 ) {
    step <- rep_len(step, length(x))
  } else {
    stopifnot(length(step) == length(x))
  }

  def <- is.na(step)
  if ( any(def) ) {
    step[def] <- 10^(ceiling(log10(abs(x[def]))) - digits)
  }
  stopifnot(is.numeric(step))
  stopifnot( all(step > 0) )

  return(op(x / step) * step)
}
