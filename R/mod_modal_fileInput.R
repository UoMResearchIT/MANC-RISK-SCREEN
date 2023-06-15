#' modal_fileInput
#'
#' @name model_fileInput
#' @description A shiny module that wraps a `fileInput` into a `modalDialog`, with option for additional parsing.
#'
#' @details
#' Besides the conventional inputs for `fileInput`, the module server `mod_modal_fileInput_server` takes an optional
#' `parser` function, that attempts to read the selected file, and provides `shinyFeedback`, then and only then the
#' "ok" button is enabled.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param label,ic,width UI inputs, forwarded to `actionButton`
#' @param parser a function to read (and check) the selected file.
#' @param ... additional server inputs, passed to `fileInput`
#'
#' @examples
#' library(shiny)
#' library(yaml)
#'
#' ui <- fluidPage(
#'   mod_modal_fileInput_ui("test"),
#'   verbatimTextOutput("print")
#' )
#'
#' server <- function(input, output, session) {
#'   out <- mod_modal_fileInput_server("test",
#'                                     label = "Select a file",
#'                                     accept = "yml",
#'                                     parser = yaml.load_file)
#'
#'   output$print <- renderPrint({
#'     cat(as.yaml(out()))
#'   })
#' }
#' shinyApp(ui, server)
#'
#' @import shiny
#' @noRd

mod_modal_fileInput_ui <- function(id, label = "Browse", ic = icon("file-import"), ...) {
  tagList(
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    actionButton(NS(id, "dialog"), label = label, icon = ic, ...),
  )
}

mod_modal_fileInput_server <- function(id, parser = NULL, ...) {

  moduleServer( id, function(input, output, session) {

    ns <- session$ns

    fileInput_dlg <- function() modalDialog(

      fileInput(ns("file"), ...),

      footer = tagList(
        actionButton(ns("cancel"), "Cancel"),
        actionButton(ns("ok"), "Ok")
      )
    )

    dialog <- reactiveValues(ok = FALSE, cancel = FALSE, file = NULL)

    dialog_status <- reactiveVal(NULL)
    dialog$file <- reactiveVal(NULL)

    observeEvent(input$dialog, {

      dialog$ok <- FALSE
      dialog$cancel <- FALSE
      dialog$file <- NULL

      dialog_status(NULL)
      shinyFeedback::hideFeedback("file")

      if ( !is.null(input$file) ) {
        shinyjs::reset("file")
      }

      showModal( fileInput_dlg() )

      shinyjs::disable("ok")
    })

    update_output <- reactive({

      if ( dialog$ok ) {
        removeModal()
        file <- dialog$file

      } else if ( dialog$cancel ) {
        removeModal()
        file <- NULL

      } else {
        file <- NULL
      }

      file
    })

    observeEvent(input$cancel, {
      dialog$cancel <- TRUE
      update_output()
    })

    observeEvent(input$ok, {
      dialog$ok <- TRUE
      update_output()
    })

    observeEvent(input$file, {

      thefile <- NULL
      shinyFeedback::hideFeedback("file")

      if ( !is.null(input$file) ) {

        if ( is.null(parser) ) {
          thefile <- input$file
        } else {
          try( thefile <- parser(input$file$datapath), silent = TRUE )
        }

        if ( !is.null(thefile) ) {
          shinyFeedback::showFeedbackSuccess("file", "Loaded successfully")
          shinyjs::enable("ok")
        } else {
          shinyFeedback::showFeedbackDanger("file", "Failed to load file")
          shinyjs::disable("ok")
        }
      }
      dialog$file <- thefile
    })

    # server_output
    update_output
  })
}
