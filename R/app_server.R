#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinipsum
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # PROVISIONAL? Code-generated list of advanced_inputs
  source("dev/auto_generated_server.R",local=T)

  lapply(advanced_inputs,shinyjs::disable)

  # output$data_table <- DT::renderDT({
  #   random_DT(10, 5)
  # })
  # output$image <- renderImage({
  #   random_image()
  # }, deleteFile = TRUE)

  output$text <- renderText({
    random_text(nwords = 50)
  })

  output$plot <- renderPlot({
    random_ggplot()
  })
  # output$print <- renderPrint({
  #   random_print("model")
  # })
  # output$table <- renderTable({
  #   random_table(10, 5)
  # })

}
