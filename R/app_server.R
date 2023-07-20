#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom utils packageName packageVersion capture.output
#' @noRd
app_server <- function(input, output, session) {

  # Load input lists on startup
  basic_inputs <- input_list("basic")
  advanced_inputs <- input_list("advanced")
  fixed_inputs <- input_list("fixed")

  # Prepare out-of-range check functions and error messages for basic inputs
  abs_lim <- parse_soft_limits(basic_inputs)
  rel_lim <- parse_relative_limits(basic_inputs)

# Configure UI ----------------------------------------------------------------------------------------------------
# currently "basic" mode is hard-coded
# defaults might become reactive, if advanced mode is (dynamically) enabled

  defaults <- .pkgenv$input_config_table[basic_inputs,"default"]
  names(defaults) <- basic_inputs

  # Add any inputs that should be included in save/restore, but are not basic_inputs
  defaults$wtp <- .pkgenv$input_config_table["wtp","default"]

  # lapply(advanced_inputs,shinyjs::disable)
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
  mdl_output <- reactive( run_basic_model(mdl_inputs(),input$wtp*1000))

# Basic output ----------------------------------------------------------------------------------------------------

  output$table <- gt::render_gt(
    pretty_incCU_table(mdl_output()) %>% gt::tab_options(table.font.size = 11)
  )

  output$icer_plot <- renderPlot({
    print(plot_ce_table(mdl_output()))
  })

  output$explanation <- renderUI({

    IncCU <- mdl_output()
    req(out_of_range)

    best_strategy  <- IncCU$StratName[IncCU$rank == 1]

    msg <- paste0('<p>With the specified inputs, the strategy that provides best value for money ',
                  '(greatest net benefit) is <b>', best_strategy, '</b>.</p>')

    if (any(out_of_range$rel_or_both)) {
      msg <- paste0(msg, '<p style="color:red"><b>',
                    'CAUTION: some combination(s) of input values you have ',
                    'specified violate their expected rank-order. </b></p>')
    }
    if (any(out_of_range$abs_or_both)) {
      msg <- paste0(msg, '<p style="color:orange"><b>',
                    'CAUTION: some input value(s) you have specified fall outside',
                    'the range available in the data we used to build the model. </b></p>')
    }
    HTML(msg)
  })

# Custom input widgets --------------------------------------------------------------------------------------------

  output$cancer.growth.widget <- renderUI({
    plotOutput("cancer_growth_rate")
  })
  output$cancer_growth_rate <- renderPlot({
    print(
      plot_cancer_growth_rate(
        log_norm_mean = input$log_norm_mean,
        log_norm_sd = input$log_norm_sd,
        max_size = input$max_size,
        start_size = input$start_size)
    )
  }, bg = "transparent")

  output$screening.widget <- renderUI({
    plotOutput("screening_widget")
  })
  output$screening_widget <- renderPlot({
    plot_sensitivity(beta_1 = input$beta_1,
                     beta_2 = input$beta_2,
                     sensitivity_max = input$sensitivity_max,
                     Sen_VDG = c(input$VDG1_sen, input$VDG2_sen, input$VDG3_sen, input$VDG4_sen))
  }, bg = "transparent")

# Soft Limits (feedback) ------------------------------------------------------------------------------------------

  out_of_range <- reactiveValues()
  observe({
    val <- used_inputs()

    out_of_range$abs_or_both <- abs_lim$check(val)
    out_of_range$rel_or_both <- rel_lim$check(val)

    out_of_range$both <- out_of_range$abs_or_both & out_of_range$rel_or_both
    out_of_range$rel <- out_of_range$rel_or_both & !out_of_range$abs_or_both
    out_of_range$abs <- out_of_range$abs_or_both & !out_of_range$rel_or_both
  })

  observeEvent(c(out_of_range$both, out_of_range$abs, out_of_range$rel), {
    # Clear all feedback
    sapply(basic_inputs, shinyFeedback::hideFeedback)

    if (any(out_of_range$both)) {
      mapply(shinyFeedback::showFeedbackDanger,
             basic_inputs[out_of_range$both],
             mapply(paste, rel_lim$msg[out_of_range$both], abs_lim$msg[out_of_range$both], sep = "<br>"))
    }

    if (any(out_of_range$rel)) {
      mapply(shinyFeedback::showFeedbackDanger,
             basic_inputs[out_of_range$rel],
             rel_lim$msg[out_of_range$rel])
    }

    if (any(out_of_range$abs)) {
      mapply(shinyFeedback::showFeedbackWarning,
             basic_inputs[out_of_range$abs],
             abs_lim$msg[out_of_range$abs])
    }

  }, ignoreInit = TRUE)

# Menu support ----------------------------------------------------------------------------------------------------

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
