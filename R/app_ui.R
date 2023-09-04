
# Custom bootstrap theme
custom_theme <- bslib::bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#660099",
  secondary = "#999999",
  # accent = "#FFCC33"
  base_font = bslib::font_google("Open Sans")
)

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

    # Banner
    tag('header', list(
      id = "banner",
      class = "header",
      a(
        class = "header-item",
        href = "https://sites.manchester.ac.uk/health-economics/",
        itemtype = "https://schema.org/Organization",
        img(
          src = "img/logo_col_white_background.png",
          alt = "University of Manchester logo",
          id = "uom-logo",
          href = "https://sites.manchester.ac.uk/health-economics/"
        )
      ),
      div(
        id = "header-title",
        class = "header-item",
        p(id = "department", "Manchester Centre for Health Economics"),
        p(id = "header-appname", app_title)
      ),
      div(
        id = "github-logo-container",
        class = "header-item",
        a(
          href = "https://github.com/UoMResearchIT/MANC-RISK-SCREEN",
          img(
            src = "img/github-mark.png",
            alt = "GitHub logo",
            id = "github-logo",
            href = "https://sites.manchester.ac.uk/health-economics/"
          )
      ))
    )),

    bslib::page_navbar(
      id = "main_tab",
      selected = "About",
      fluid = FALSE,
      collapsible = TRUE,
      theme = custom_theme,
      lang = "en",
      bg = "#660099",

      tabPanel("About",
        h1( paste(app_title, "-", packageVersion(pkg)) ),
        div(includeMarkdown("inst/about.md"), style = 'width:720px;')
      ),
      tabPanel("Main",

        shinyFeedback::useShinyFeedback(),
        shinyjs::useShinyjs(debug = TRUE),

        fluidPage( sidebarLayout(
          sidebarPanel( width = 5,

            fluidRow(#style = 'margin-right: 3%;',
                     #align = 'right',
                     id = "mod_save_load_reset_ui",
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

# CSS cache bust
css_cache_bust <- function(css_file) {

  css_file_copy <- list.files(app_sys("app/www"), paste0(css_file,"#.*"), full.names = TRUE)
  file.remove(css_file_copy)
  css_file_copy <- paste0(css_file, "#", format(Sys.time(), "%Y%m%d%H%M%S"))
  file.copy(css_file, css_file_copy)

  file.path("www",basename(css_file_copy))
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
  add_resource_path("www", app_sys("app/www") )
  add_resource_path("img", app_sys("app/img") )

  stylesheet <- css_cache_bust("style.css")

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = app_title,
      all_files = TRUE
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    tags$link(rel = "stylesheet", type = "text/css", href = stylesheet)
  )
}
