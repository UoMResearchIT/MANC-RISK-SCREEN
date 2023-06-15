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

  # defaults might become reactive, if advanced mode is enabled
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

  used_inputs <- reactive( reactiveValuesToList(input)[basic_inputs] )
  mdl_inputs <- reactive( parse_inputs(used_inputs()) )
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

  # Custom downloader: include commented mdl_output table after as.yaml(used_inputs)
  pkg = packageName()
  downloader <- downloadHandler(
    filename = paste0(pkg, "_session.", "yml"),
    content = function(file) {
      write(c(paste("#", pkg, packageVersion(pkg)),
              paste0("# ",date()),
              '',
              '# Input:',
              yaml::as.yaml(used_inputs()),
              '',
              "# Output:",
              paste0("# ",capture.output(mdl_output()))
      ), file)
    }
  )

  saved_inputs <- mod_save_load_reset_server("menu",
                                             main_session = session,
                                             defaults = defaults,
                                             ext = "yml",
                                             downloader = downloader,
                                             parser = NULL,
                                             .bookmark = c("main_tab", "tabs"))

  # output$status <- renderPrint({
  #   print(t(data.frame(mdl_inputs())))
  #   # print(mdl_output())
  # })
}
