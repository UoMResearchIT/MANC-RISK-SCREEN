#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinipsum
#' @import ggplot2
#' @importFrom dampack calculate_icers
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  basic_inputs <- input_list("basic")
  advanced_inputs <- input_list("advanced")
  fixed_inputs <- input_list("fixed")

  load_input_config(session)

  defaults <- .pkgenv$input_config_table[basic_inputs,"default"]
  names(defaults) <- basic_inputs

  #lapply(advanced_inputs,shinyjs::disable)
  lapply(advanced_inputs,shinyjs::hide)
  lapply(fixed_inputs,shinyjs::hide)

  for ( tab in input_groups("advanced") ) {
    hideTab(inputId = "tabs", target = tab)
  }
  for ( tab in input_groups("fixed") ) {
    hideTab(inputId = "tabs", target = tab)
  }

  output$intro_text <- renderText(random_text(nwords = 50))

  mdl_inputs <- reactive( parse_inputs(input) )
  mdl_output <- reactive( run_basic_model(mdl_inputs()))

  output$table <- renderTable(
    mdl_output(),
    rownames = TRUE
  )

  output$icer_plot <- renderPlot({
    df <- mdl_output()
    icer_strat <- dampack::calculate_icers(cost = df$cost,
                                           effect = df$qualy,
                                           strategies = row.names(df))
    plot(icer_strat,currency = "\uA3", label = "all")
  })

  # Required by mod_save_load_reset_server:
  # call shinyjs::reset at startup, to populate input$`shinyjs-resettable-`
  shinyjs::reset()
  saved_inputs <- mod_save_load_reset_server("menu",
                                             main_session = session,
                                             defaults = defaults,
                                             ext = "yml",
                                             downloader = NULL,
                                             parser = NULL,
                                             .filename = "dummy_session",
                                             .bookmark = c("main_tab", "tabs"))

  # output$status <- renderPrint({
  #   print(t(data.frame(mdl_inputs())))
  #   # print(mdl_output())
  # })
}
