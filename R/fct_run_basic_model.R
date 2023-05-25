#' `run_basic_model` evaluate GAM objects with current `input` for all strategies
#'
#' @param input reactive UI input (if missing, default values are returned).
#' @return two-column data frame, rows for strategies, cols. for model
#' @noRd
run_basic_model <- function(input_vector) {

  data("qualy_model_obj")
  data("cost_model_obj")

  # replicate for all strategies
  strategies <- as.factor(levels(qualy_model_obj$var.summary$alternative))
  input_df <- data.frame(alternative = strategies)
  input_df[,names(input_vector)] <- input_vector

  #Predict the QALYs and costs for each strategy
  qualys <- mgcv::predict.bam(qualy_model_obj,input_df)
  cost <- mgcv::predict.bam(cost_model_obj,input_df)

  output_df <- data.frame(qualy = qualys, cost = cost)
  rownames(output_df) <- strategies

  output_df[,"incQALYS"]<-c(output_df$qualy-output_df["noscreening","qualy"])
  output_df[,"incCost"]<-c(output_df$cost-output_df["noscreening","cost"])
  output_df[,"ICER"]<-c(output_df$incCost/output_df$incQALYS)
  output_df[,"NB20k"]<-c((output_df$incQALYS*20000)-output_df$incCost)
  output_df[,"NB30k"]<-c((output_df$incQALYS*30000)-output_df$incCost)

  return( output_df )
}

#' `parse_inputs` - compile inputs into a list, names and units matching those
#'    expected by cost & qualy's GAM objects.
#'
#' @param input reactive UI input (if missing, default values are returned).
#' @return named list of inputs.
#' @noRd
parse_inputs <- function(input = NULL) {

  data("input_config_table")

  ui_vars <- input_list("basic")
  defaults <- input_config_table[ui_vars,"default"]

  if ( is.null(input) ) {
    input_vector <- defaults
  } else {
    input_vector <- reactiveValuesToList(input)[ui_vars]
  }

  # Resolve change of units, e.g. 5-year-survival to exponential param.
  units <- input_config_table[ui_vars,'unit']
  input_vector <- parse_units(input_vector,units,'ui2psa',defaults)

  # Resolve change of names
  mapped_vars <- model_input_names(ui_vars)
  names(input_vector) <- mapped_vars

  return( input_vector )
}

