#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  app_title <- "MancRiskScreenUI"
  pkg <- golem::pkg_name()
  n_runs <- .pkgenv$input_config_table["inum","default"]

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(app_title),

    navbarPage(app_title, id = "main_tab",
      tabPanel("About",
        h1( paste(app_title, "-", packageVersion(pkg)) ),
        p( packageDescription(pkg)$Description )
      ),
      tabPanel("Main",

        shinyFeedback::useShinyFeedback(),
        shinyjs::useShinyjs(debug = TRUE),

        fluidPage( sidebarLayout(
          sidebarPanel( width = 6,

            fluidRow(style = 'margin-right: 3%;',
                     align = 'right',
                     mod_save_load_reset_ui("menu")
            ),
            br(),

            # tabPanel list, auto-generated from `input_config_table`
            auto_generated_ui(id = "tabs", selected = "Utility")
          ),

          mainPanel(
            h3("Model output"),
            p( paste("Generalized additive models (GAM) fit on", n_runs, "simulation runs.") ),
            tableOutput("table"),
            br(),
            plotOutput("icer_plot"),
            width = 6
          )
        ),

        # h3("DEBUG: PSA input"),
        # verbatimTextOutput("status"),

        )
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
