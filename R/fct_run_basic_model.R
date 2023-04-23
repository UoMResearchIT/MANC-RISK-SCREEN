#' run_basic_model
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
run_basic_model <- function(input = NULL) {

  modQ <- load_gam_model()
  data("input_config_table")

  ui_vars <- input_list("basic")

  if ( is.null(input) ){
    input_vector <- input_config_table[ui_vsars,"default"]
  } else {
    input_vector <- reactiveValuesToList(input)[ui_vars]
  }

  # Resolve change of units, e.g. 5-year-survival to exponential param.
  units <- input_config_table[ui_vars,'unit']
  input_vector <- parse_units(input_vector,units,'ui2psa')

  # Resolve change of names
  mapped_vars <- model_input_names(ui_vars)
  names(input_vector) <- mapped_vars

  # replicate for all strategies
  strategies <- as.factor(levels(modQ$var.summary$alternative))
  df <- data.frame(alternative = strategies)
  df[,mapped_vars] <- input_vector

  #Predict the QALYs for each strategy
  #This is not giving sensible values at the moment so I need to fix
  #Should be ok for now
  qualys <- mgcv::predict.bam(modQ,df)
  names(qualys) <- strategies

  return( qualys )
}
