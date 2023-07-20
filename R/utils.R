#' Returns configurable subsets of inputs, to control app "modes"
#'
#' Relies on columns "basic", "valid", and "fixed" of `input_config_table`
#'
#' @param key named subset of inputs: {"basic","advanced","fixed"}
#' @return list of input ids in the corresponding list
#'
#' @noRd
input_list <- function(key) {
  input_config_table <- .pkgenv$input_config_table
  subset <- switch(key,
    "basic" = input_config_table$valid & input_config_table$basic,
    "advanced" = input_config_table$valid & !input_config_table$fixed & !input_config_table$basic,
    "fixed" = input_config_table$valid & input_config_table$fixed
  )
  return(input_config_table$id[subset])
}

#' Returns configurable subsets of inputs, to control app "modes"
#'
#' Relies on columns "basic", "valid", and "fixed" of internal data table
#' `input_config_table`, exported by `dev/parse_ui_table.R`
#'
#' @param key named subset of inputs: {"basic","advanced","fixed"}
#' @return list of input ids in the corresponding list
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr summarise group_by
#' @noRd
input_groups <- function(key) {

  grp <- .pkgenv$input_config_table %>%
    group_by(.data$group) %>%
    summarise(basic = any(.data$basic & .data$valid), fixed = all( .data$fixed | !.data$valid))

  subset <- switch(key,
                   "basic" = grp$basic,
                   "advanced" = !grp$basic & !grp$fixed,
                   "fixed" = grp$fixed
  )
  return(grp$group[subset])
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
#' @param units {"5yr","rel","pm", NA} of length(x) (or scalar)
#' @param direction {'psa2ui' or 'ui2psa'}
#' @param y vector of reference values
#'
#' @return vector of converted values
#'
#' @noRd
parse_units <- function(x, units, direction, y = NULL) {

  stopifnot( direction %in% c('psa2ui','ui2psa') )

  KNOWN <- c("5yr", "rel", "pm", "NA")

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

    } else if ( u == "pm" ) {
      x[idx] <- switch( direction,
                        'psa2ui' = x0[idx]*1000,
                        'ui2psa' = x0[idx]/1000 )

    } else if ( u == "rel" ) {

      stopifnot(!is.null(y))
      y = unlist(y)
      if ( length(y) == 1 && length(x) > 1) {
        y <- rep_len(y, length(x))
      } else {
        stopifnot(length(y) == length(x))
      }

      x[idx] <- switch( direction,
                        'psa2ui' = y[idx]*( 1 + x0[idx] ),
                        'ui2psa' = x0[idx]/y[idx] - 1 )
    }
  }

  return(x)
}

#' `custom_round` - round vector `x` to units of `step`
#'
#' @param x value(s) to be rounded
#' @param step defaults to `10^(ceiling(log10(abs(x))) - digits)`
#' @param op use custom operation (e.g. `ceiling` or `floor`) instead of `round`
#' @param digits significant digits (for default `step` only)
#'
#' @return `op(x / step) * step`
#'
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

#' Extract an out-of-range check function and error messages from `input_config_table` soft limits
#'
#' @param vars list of (selected) inputs, e.g. from `input_list(KEY)`
#' @return `list(msg,check)` where `check` is a function, that returns a boolean out-of-range value,
#'  given a vector of values for variables `vars`; and `msg` is a list of out-of-range error messages
parse_soft_limits <- function(vars) {

  limits <- .pkgenv$input_config_table[vars,c("rel_min","rel_max")]
  rownames(limits) <- vars

  # Prepare warning messages for out-of-range variables
  msg <- lapply(vars,
    function(id)
      paste0("Extrapolating beyond data [", limits[id, "rel_min"], ", ", limits[id, "rel_max"], "]")
  )

  check <- function(val){
    out_of_range <- (val > limits$rel_max) | (val < limits$rel_min)
    out_of_range[is.na(out_of_range)] <- FALSE
    return(out_of_range)
  }

  return(list(check = check, msg = msg))
}

#' Extract an out-of-range check function and error messages from the `relative_limits` matrix.
#'
#' @param vars list of (selected) inputs, e.g. from `input_list(KEY)`
#' @return `list(msg,check)` where `check` is a function, that returns a boolean out-of-range value,
#'  given a vector of values for variables `vars`; and `msg` is a list of out-of-range error messages
parse_relative_limits <- function(vars) {

  rel_matrix <- .pkgenv$relative_limits[vars, vars]

  # relative_limits[i,j] == 1 implies that input[i] > input[j]
  has_relative_min <- (rowSums(rel_matrix) > 0)
  has_relative_max <- (colSums(rel_matrix) > 0)

  # Prepare out of range messages
  msg <- rep("",length(vars))
  names(msg) <- vars
  for (j in which(has_relative_min | has_relative_max)) {

    if (any(rel_matrix[j,] > 0)) {
      rel.min <- stringr::str_flatten_comma(vars[which(rel_matrix[j,] > 0)], last = " and ")
    } else {
      rel.min <- NULL
    }

    if (any(rel_matrix[,j] > 0)) {
      rel.max <- stringr::str_flatten_comma(vars[which(rel_matrix[,j] > 0)], last = " and ")
    } else {
      rel.max <- NULL
    }

    if (!(is.null(rel.min))) {
      if (!is.null(rel.max)) {
        msg[j] <- paste("Expected", rel.min, "<", vars[j], "<", rel.max)
      } else {
        msg[j] <- paste("Expected", rel.min, "<", vars[j])
      }
    } else {
      msg[j] <- paste("Expected", vars[j], "<", rel.max)
    }
  }

  check = function(val) {
    v <- matrix(as.numeric(val), ncol = 1)
    rel.min <- rel_matrix[has_relative_min, ] %*% v
    rel.max <- t(rel_matrix[, has_relative_max]) %*% v

    out_of_range <- has_relative_min | has_relative_max
    out_of_range[has_relative_min] <- (val[has_relative_min] < rel.min)
    out_of_range[has_relative_max] <- (val[has_relative_max] > rel.max)

    out_of_range[is.na(out_of_range)] <- FALSE
    return(out_of_range)
  }

  return(list(check = check, msg = msg))
}

