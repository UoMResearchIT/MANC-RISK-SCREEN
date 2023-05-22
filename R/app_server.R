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

  mdl_inputs <- reactive( parse_inputs(input) )
  mdl_output <- reactive( run_basic_model(mdl_inputs()))

  output$qualy_plot <- renderPlot({
    df <- mdl_output()
    ggplot(data = df, aes(x = row.names(df), y = qualy)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(x = 'strategy') +
      geom_text(aes(label=custom_round(df$qualy)), vjust=-0.3, size=3.5) +
      theme_minimal()
  })

  output$cost_plot <- renderPlot({
    df <- mdl_output()
    ggplot(data = df, aes(x = row.names(df), y = cost)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(x = 'strategy') +
      geom_text(aes(label=custom_round(df$cost)), vjust=-0.3, size=3.5) +
      theme_minimal()
  })

  output$status <- renderPrint({
    # do.call('req',reactiveValuesToList(values)[basic_inputs])
    # print(reactiveValuesToList(input)[basic_inputs])
    print(t(data.frame(mdl_inputs())))
    print(mdl_output())
  })

  # output$print <- renderPrint({
  #   random_print("model")
  # })
  # output$table <- renderTable({
  #   random_table(10, 5)
  # })

}
