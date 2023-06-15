#' save_load_reset
#'
#' @name save_load_reset
#' @description a shiny module to generate four working buttons: `reset`, `save`, `restore`, and `share`.
#'
#' @details
#'  `reset` is similar to `shinyjs::reset`, but affects only inputs listed in server argument `defaults`.
#'    We will call `names(defaults)` the _selected inputs_.
#'
#'  NOTE: make sure to include a call to `shinyjs::reset` in your main server. Both `reset` and `restore`
#'  use the list `input$shinyjs-resettable-` created by this function.
#'
#'  `save` is a `downloadButton`, with a default `downloadHandler` that writes the current value of
#'    the _selected inputs_ to a YAML file.
#'  `restore` opens a modal `file_Input` dialog, which can read and parse these files, and restore
#'    the _selected inputs_ to the file contents.
#'  `share` is a `bookmarkButton` (make sure to use `enableBookmarking = "url"`), it provides cleaner
#'    URLs by using `setBookmarkExclude` to remove anything except _selected inputs_ which are *not* set
#'    to their default value.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param main_session parent session, defaults to `getDefaultReactiveDomain()`
#' @param defaults list of (selected) inputs with their default values. `names(defaults)` will be used
#'    as the list of non-trivial inputs, i.e. those that should be saved, restored, and bookmarked.
#' @param ext file extension (default "yml", to work with default `downloader` and `parser`)
#' @param downloader a `downloadHandler` for the save button (see `downloadButton`)
#'    the default prints a commented header followed by `as.yaml(input_list)` where
#'    `input_list = reactiveValuesToList(main_session$input)[names(defaults)]`
#' @param parser a function to read and parse a saved file. The default is `yaml.load_file`
#'
#' @import shiny

mod_save_load_reset_ui <- function(id, ...) {
  ns <- NS(id)
  tagList(
      actionButton(ns("reset"), "Reset", icon = icon("undo"), ...),
      downloadButton(ns("save"), label = "Save", ...),
      mod_modal_fileInput_ui(ns("restore"), "Restore", ...),
      bookmarkButton(id = ns("share"), label = "Share", ...)
  )
}

mod_save_load_reset_server <- function(id,
                                       main_session = getDefaultReactiveDomain(),
                                       defaults,
                                       ext = "yml",
                                       downloader = NULL,
                                       parser = NULL,
                                       .filename = "shiny_session",
                                       .bookmark = c()) {

  stopifnot(is.reactivevalues(main_session$input))
  stopifnot(is.list(defaults))

  moduleServer(id, function(input, output, session) {

    # These will be the only inputs to be saved/restored/bookmarked
    default_names <-  names(defaults)
    input_list <- reactive( reactiveValuesToList(main_session$input)[default_names] )

    # Default downloader: print a header followed by as.yaml(input_list)
    if ( is.null(downloader) ) {

      stopifnot(ext == 'yml')

      pkg <- packageName()
      if ( !is.null(pkg) ) {
        filename <- fs::path_ext_set(pkg, ext)
        header <- paste(pkg, packageVersion(pkg))
      } else {
        filename <- fs::path_ext_set(fs::path_file(.filename), ext)
        header <- .filename
      }
      header <- c(paste0("# ", header, ", ", date()), '')

      downloader <- downloadHandler(
        filename = filename,
        content = function(file) {
          write(c(header, yaml::as.yaml(input_list())), file)
        }
      )
    } else {
      stopifnot(inherits(downloader, "shiny.render.function"))
    }

    # Assign downloadHandler to save button
    output$save <- downloader

    updateBookmarks <- function() {

      current <- input_list()
      not_default <- mapply(function(a, b) !isTRUE(all.equal(a, b)), defaults, current)

      toExclude <- setdiff(names(main_session$input), c(.bookmark, default_names[not_default]))
      setBookmarkExclude(toExclude, session = main_session)

      # Update the query string
      updateQueryString(getQueryString(main_session), mode = "replace", session = main_session)
    }

    # Bookmarking (share button)
    observeEvent(input$share, {

      updateBookmarks()

      main_session$doBookmark()

      # # Update the query string
      # updateQueryString(getQueryString(main_session), mode = "replace", session = main_session)
    })

    # onRestore(fun, session = main_session)

    # Call shinyjs::reset at startup, to populate input$`shinyjs-resettable-`
    # resetonce = reactiveVal(TRUE)
    # observe({
    #   req(resetonce())
    #   req(input_list())
    #   shinyjs::reset(asis = TRUE)
    #   resetonce(NULL)
    # })

    # Reset button
    observeEvent(input$reset, {
      custom_reset(main_session, defaults)

      # Reset the query string
      updateBookmarks()
    })

    # Default parser: read yaml and check all names are in defaults
    if ( is.null(parser) ) {

      stopifnot(ext == 'yml')

      parser <- function(file) {
        contents <- yaml::yaml.load_file(file)
        stopifnot(all(names(contents) %in% default_names))
        contents
      }
    }

    # Restore (load file)
    loaded_inputs <- mod_modal_fileInput_server("restore",
                                                label = "Select a file",
                                                accept = ext,
                                                parser = parser)

    # Restore after file loaded
    observeEvent(loaded_inputs(), {
      custom_reset(main_session, loaded_inputs())
      updateBookmarks()
    })

    loaded_inputs
  })
}


custom_reset <- function(main_session, value_list) {

  resettables <- main_session$input$`shinyjs-resettable-`
  stopifnot(!is.null(resettables) )

  for (id in names(value_list)) {

    typ <- resettables[[id]]$type

    switch(typ,
      "Slider" = updateSliderInput(main_session, id, value = value_list[[id]]),
      "Text" = updateTextInput(main_session, id, value = value_list[[id]]),
      "Numeric" = updateNumericInput(main_session, id, value = value_list[[id]]),
      "Checkbox" = updateCheckboxInput(main_session, id, value = value_list[[id]]),
      "Select" = updateSelectInput(main_session, id, selected = value_list[[id]]),
      "TabsetPanel" = updateTabsetPanel(main_session, id, selected = value_list[[id]]),
      "NavlistPanel" = updateNavlistPanel(main_session, id, selected = value_list[[id]]),
      warning("Unknown input type ", dQuote(typ), " for input:",id)
    )
  }
}

## To be copied in the UI
# mod_save_load_reset_ui("save_load_reset_1")

## To be copied in the server
# mod_save_load_reset_server("save_load_reset_1")
