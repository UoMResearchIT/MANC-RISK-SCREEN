#' run_basic_model
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
run_basic_model <- function(input = NULL) {

  modQ <- load_qualy_gam()
  modC <- load_cost_gam()

  data("input_config_table")

  ui_vars <- input_list("basic")

  if ( is.null(input) ) {
    input_vector <- input_config_table[ui_vars,"default"]
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
  input_df <- data.frame(alternative = strategies)
  input_df[,mapped_vars] <- input_vector

  #Predict the QALYs and costs for each strategy
  qualys <- mgcv::predict.bam(modQ,input_df)
  cost <- mgcv::predict.bam(modC,input_df)

  output_df <- data.frame(qualy = qualys,cost = cost)
  colnames(output_df) <- strategies

  return( output_df )
}
