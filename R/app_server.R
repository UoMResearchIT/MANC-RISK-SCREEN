#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinipsum
#' @import ggplot2
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  basic_inputs <- input_list("basic")
  advanced_inputs <- input_list("advanced")
  fixed_inputs <- input_list("fixed")

  load_input_config(session)

  #lapply(advanced_inputs,shinyjs::disable)
  lapply(advanced_inputs,shinyjs::hide)

  lapply(fixed_inputs,shinyjs::hide)

  # half <- reactive({
  #   even <- input$n %% 2 == 0
  #   shinyFeedback::feedbackWarning("n", !even, "Please select an even number")
  #   input$n / 2
  # })

  # output$data_table <- DT::renderDT({
  #   random_DT(10, 5)
  # })
  # output$image <- renderImage({
  #   random_image()
  # }, deleteFile = TRUE)

  output$intro_text <- renderText(random_text(nwords = 50))

  output$qualy_plot <- renderPlot({

    basic_output <- run_basic_model(input)

    ggplot(basic_output, aes(x = row.names(basic_output), y = "qualy")) +
      geom_bar(stat = "identity") +
      labs(x = 'strategy')
  })

  output$cost_plot <- renderPlot({

    basic_output <- run_basic_model(input)

    ggplot(basic_output, aes(x = row.names(basic_output), y = "cost")) +
      geom_bar(stat = "identity") +
      labs(x = 'strategy')
  })

  output$status <- renderPrint({
    # do.call('req',reactiveValuesToList(values)[basic_inputs])
    print(reactiveValuesToList(input)[basic_inputs])
  })

  # output$print <- renderPrint({
  #   random_print("model")
  # })
  # output$table <- renderTable({
  #   random_table(10, 5)
  # })

}
