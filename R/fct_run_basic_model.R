#' run_basic_model
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
run_basic_model <- function(input = NULL) {

  data("qualy_model_obj")
  data("cost_model_obj")
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
  strategies <- as.factor(levels(qualy_model_obj$var.summary$alternative))
  input_df <- data.frame(alternative = strategies)
  input_df[,mapped_vars] <- input_vector

  #Predict the QALYs and costs for each strategy
  qualys <- mgcv::predict.bam(qualy_model_obj,input_df)
  cost <- mgcv::predict.bam(cost_model_obj,input_df)

  output_df <- data.frame(qualy = qualys,cost = cost)
  colnames(output_df) <- strategies

  return( output_df )
}
