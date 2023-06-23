#' @description set input limits based on PSA config
#'
#' @details reads rel_min and `rel_max` fields from `input_config_table`
#' (set by `get_PSA_input_limits`), and calls required functions
#' `updateSliderInput`, `updateNumericInput`, etc.
#'
#' @noRd
load_input_config <- function(session, dryrun = F) {

  input_config_table <- .pkgenv$input_config_table

  COL_NAMES <- c("id", "type", "fixed", "default", "rel_min", "rel_max", "abs_min", "abs_max", "description", "step")
  stopifnot(is.data.frame(input_config_table) &&
             rlang::is_empty(setdiff(COL_NAMES, colnames(input_config_table))))

  ok <- input_config_table$valid & !input_config_table$fixed

  # disp.str <- function(x) paste(capture.output(str(x)), collapse = "\n")

  for (j in which(ok)) {

    line <- as.list(input_config_table[j, COL_NAMES])
    out <- parse_line(line)

    if ( !rlang::is_empty(out$args) & !dryrun ) {

      out$args$inputId <- line$id
      out$args$session <- session

      do.call(out$fun, out$args)
    }
  }
}

parse_line <- function(line) {

  typ = parse_type(line$type)

  args = list()

  if ( match("value",typ$opts) && !is.na(line$default) ) {

    args$value <- unlist(line$default)
    if ( line$type == "matrix" ) {
      args$value <- line$default[[1]] # see GOTCHA in parse_ui_table
    } else {
       args$value = line$default
    }
  }

  if ( match("min",typ$opts) && !is.na(line$rel_min) ) {
    args$min <- line$rel_min
  }

  if ( match("max",typ$opts) && !is.na(line$rel_max) ) {
    args$max <- line$rel_max
  }

  if ( match("step",typ$opts) && !is.na(line$step) ) {
    args$step <- line$step
  }

  return( list(fun = typ$fun, args = args) )
}

#' @import shiny
parse_type <- function(type) {
  typ <- switch(type,
    "slider" = list(
      fun = updateSliderInput,
      opts = c("value", "label", "min", "max", "step")
    ),
    "numeric" = list(
      fun = updateNumericInput,
      opts = c("value", "label", "min", "max", "step")
    ),
    "text" = list(
      fun = updateTextInput,
      opts = c("value", "label", "placeholder")
    ),
    "file" = list(
      fun = updateTextInput,
      opts = c("value", "label", "placeholder")
    ),
    "matrix" = list(
      fun = shinyMatrix::updateMatrixInput,
      opts = c("value")
    ),
    "checkbox" = list(
      fun = updateCheckboxInput,
      opts = c("value", "label")
    ),
    NULL
  )
  return( typ )
}
