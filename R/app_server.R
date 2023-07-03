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

# Basic output ----------------------------------------------------------------------------------------------------

  output$table <- gt::render_gt(
    pretty_incCU_table(mdl_output(), input$wtp*1000) %>% gt::tab_options(table.font.size = 11)
  )

  output$icer_plot <- renderPlot({
    print(plot_ce_table(mdl_output(), input$wtp*1000))
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

  observeEvent(used_inputs(), {

    val <- used_inputs()
    out_of_abs_range <- abs_lim$check(val)
    out_of_rel_range <- rel_lim$check(val)

    both_out <- out_of_abs_range & out_of_rel_range
    out_of_rel_range <- out_of_rel_range & !both_out
    out_of_abs_range <- out_of_abs_range & !both_out

    # Clear all feedback
    sapply(basic_inputs, shinyFeedback::hideFeedback)

    if (any(both_out)) {
      mapply(shinyFeedback::showFeedbackDanger,
             basic_inputs[both_out],
             mapply(paste, rel_lim$msg[both_out], abs_lim$msg[both_out], sep = "<br>"))
    }

    if (any(out_of_rel_range)) {
      mapply(shinyFeedback::showFeedbackDanger,
             basic_inputs[out_of_rel_range],
             rel_lim$msg[out_of_rel_range])
    }

    if (any(out_of_abs_range)) {
      mapply(shinyFeedback::showFeedbackWarning,
             basic_inputs[out_of_abs_range],
             abs_lim$msg[out_of_abs_range])
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
