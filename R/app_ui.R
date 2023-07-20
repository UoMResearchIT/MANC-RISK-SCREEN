#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom utils packageVersion packageDescription
#' @noRd
app_ui <- function(request) {

  app_title <- "MANC-RISK-SCREEN"
  pkg <- "MancRiskScreenUI"
  n_runs <- .pkgenv$input_config_table["inum","default"]

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(app_title),

    navbarPage(title = app_title,
               id = "main_tab",
               selected = "About",
               fluid = FALSE,
               collapsible = TRUE,

      tabPanel("About",
        h1( paste(app_title, "-", packageVersion(pkg)) ),
        div(includeMarkdown("inst/about.md"), style = 'width:720px;')
      ),
      tabPanel("Main",

        shinyFeedback::useShinyFeedback(),
        shinyjs::useShinyjs(debug = TRUE),

        fluidPage( sidebarLayout(
          sidebarPanel( width = 5,

            fluidRow(style = 'margin-right: 3%;',
                     align = 'right',
                     mod_save_load_reset_ui("menu")
            ),
            br(),

            # tabPanel list, auto-generated from `input_config_table`
            auto_generated_ui(id = "tabs", selected = "Utility")

          ),
          mainPanel( width = 7,
            fluidRow(
              column(width = 8, tagList(
                h3("Model output"),
                p( paste("Generalized additive models (GAM) fit on", n_runs, "simulation runs.") )
              )),
              column(width = 4,
                shiny::numericInput("wtp","Value of 1 QALY (k\uA3)",20,0,50,1)
              )
            ),
            htmlOutput("explanation"),
            gt::gt_output("table"),
            br(),
            plotOutput("icer_plot"),
          )
        ),

        # h3("DEBUG: PSA input"),
        # verbatimTextOutput("status"),

        )
      ),
      tabPanel("Help",
        div(includeMarkdown("inst/help.md"), style = 'width:720px;')
      ),
      tabPanel("References",
        div(includeMarkdown("inst/refs.md"), style = 'width:720px;')
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(app_title) {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = app_title
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
