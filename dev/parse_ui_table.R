#' This is a provisional development tool, used to generate a very simple UI
#' from "dev/inputs.xlsx":
#'
#' Writes h2 headers for every non-empty cell in the "Group" column (A:A),
#' then wraps every line until the next header into a FluidRow column.
#'
#'  h2("Header"),
#'  fluidRow(
#'    column(width=NCOL, numericInput("foo", "Foo?", value = 42) ),
#'    # Commented incomplete rows
#'    column(width=NCOL, numericInput("bar", "Bar?", value = 0.5, min = 0, max = 1) )
#'  ),
#'
#' It also keeps a list of "advanced-inputs", for which the column in_GAM is 0/empty
#'
#'  advanced_inputs <- c("foo","bar",...)
#'
#' @return Writes a file "dev/auto_generated_server.R" with a fluidPage object,
#'  to be sourced into "app_ui.R". The list "advanced_inputs" is written into
#'  "dev/auto_generated_server.R", to be sourced into and "app_server.R"
#'
#' @import glue
#' @import stringr
parse_ui_table <- function(INPUT_FILE="dev/inputs.xlsx",
                           INPUT_SHEET = "inputs",
                           OUTPUT_UI = "dev/auto_generated_ui.R",
                           OUTPUT_SERVER = "dev/auto_generated_server.R") {

  # Expected/adjusted column names
  COL_LABELS <- c('Group', 'ID', 'type', 'in GAM?', 'default', '"normal" range', 'absolute range', 'symbol', 'unit', 'description', 'Notes')
  COL_NAMES <- c('group', 'id', 'type', 'in_GAM', 'default', 'normal_range', 'abs_range', 'symbol', 'unit', 'description', 'notes')

  # Read input config table from inputs.xlsx, make sure column order hasn't changed
  config_table <- readxl::read_excel(INPUT_FILE, sheet = INPUT_SHEET)
  stopifnot(all.equal(colnames(config_table),COL_LABELS))
  colnames(config_table) = COL_NAMES

  write_out = function(...,file=OUTPUT_UI,append = T) cat(glue(...,.trim = F,.envir=parent.frame(n=1)),file=file,append = append)

  parse_bounds <- function(lim) {

    if ( is.na(lim) ) { return("") }

    bounds <- str_match(lim,"(?<min>[\\d]+)-(?<max>[\\d]+)")
    # bounds <- str_match(line$abs_range,"(?<min>[\\d.eE-]+)-(?<max>[\\d.eE-]+)")

    bounds <- list("min"=as.numeric(bounds[2]),"max"=as.numeric(bounds[3]))

    if ( is.na(bounds$min) || is.na(bounds$max) ){
      warning("Failed to parse bounds: ",str_flatten_comma(lim))
      args <- ""
    } else {
      args <- glue(', min = {bounds$min}, max = {bounds$max}')
    }
    return(args)
  }

  jth_line <- function(j) as.list(config_table[j,])

  is_group_header <- function(line) !is.na(line$group)

  advanced_inputs = c();
  add_advanced_input <- function(input){
    advanced_inputs <<- c(advanced_inputs,input)
  }

  parse_chunk <- function(line_counter) {

    line <- jth_line(line_counter)
    stopifnot( is_group_header(line) )

    header <- line
    elements <- list()

    while ( line_counter < nrow(config_table) ) {

      line_counter <- line_counter+1
      line <- jth_line(line_counter)

      if ( !is_group_header(line) ){
        elements = c(elements,parse_element(line))
        if ( line_counter == nrow(config_table) ) write_chunk(header,elements)
      } else {
        write_chunk(header,elements)
        line_counter <- parse_chunk(line_counter)
      }
    }
    return(line_counter)
  }

  write_chunk <- function(header, elements) {

    write_out('  h2("{header$group}"),\n')

    if ( rlang::is_empty(elements) ) return()

    is_comment <- sapply(elements, function(x) startsWith(as.character(x),'#'))

    if ( !all(is_comment) ) write_out('  fluidRow(\n')

    for ( j in seq_along(elements) ){
      msg <- elements[[j]]
      if ( startsWith(msg,'#') ){
        write_out('   {msg}\n')
      } else {
        write_out('    column(width=NCOL, {msg} )')
        if ( any(!is_comment[-(1:j)]) ){
          write_out(',\n')
        } else {
          write_out('\n')
        }
      }
    }

    if ( !all(is_comment) ) write_out('  ),\n')
  }

  parse_element <- function(line) {

    stopifnot( !is_group_header(line) )

    out <- list()
    super_glue <- function(...) c(out,glue(...,.trim = F,.envir=parent.frame(n=1)))

    if (!is.na(line$notes)){
      out <- super_glue('# {line$notes}')
    }

    # A "complete" entry has ID, description and default value
    # Incomplete entries are just printed as comments.
    if ( any(is.na(line[c('id','description','default')])) ) {

      msg <- str_flatten_comma(line[!is.na(line)])
      out <- super_glue('# {msg}')
      # warning("Incomplete input: ",msg)
      return(out)
    }

    args <- glue('"{line$id}", "{line$description}", value = {line$default}',.literal=T)

    # Keep a list of "advanced" inputs
    if ( is.na(line$in_GAM) || !grep("[xty1]+",line$in_GAM,ignore.case=T) ){

      add_advanced_input(line$id)

      # out <- super_glue('p("{line$id} = {line$default} # {line$description}")')
      out <- super_glue('numericInput({args})')

      return(out)
    }

    if ( grep("num|slider",line$type,ignore.case=T) ) {

      args <- paste0(args,parse_bounds(line$abs_range))

      if ( grep("num",line$type,ignore.case=T) ) {
        out <- super_glue('numericInput({args})')
      } else {
        out <- super_glue('sliderInput({args})')
      }
      return(out)
    }

    if ( grep("check|bool|logic",line$type,ignore.case=T) ) {
      out <- super_glue('checkboxInput({args})')
      return(out)
    }

    if ( grep("check|bool|logic",line$type,ignore.case=T) ) {
      out <- super_glue('selectInput({args})')
      return(out)
    }
  }

  # main --------------------------------------------------------------------

  write_out("# Code generated by UI_elements(), ", date(),"\n",append = F)
  write_out('# to be sourced into app_ui.R:\n\n')

  write_out('fluidPage(\n',
            '  # Customize this header later\n',
            '  shinyjs::useShinyjs(),\n',
            '  h1("MancRiskScreenUI"),\n',
            '  tableOutput("text"),\n\n\n')

  parse_chunk(1)

  write_out('\n\n',
            '  # Customize this closure later\n',
            '  p("This is still just a random plot:"),\n',
            '  plotOutput("plot")\n',
            ')\n')

  write_out("# Code generated by UI_elements(), ", date(),"\n",file=OUTPUT_SERVER,append = F)
  write_out('# to be sourced into app_server.R:\n\n',file=OUTPUT_SERVER)

  msg <- toString(dQuote(advanced_inputs,q='"'))
  write_out('advanced_inputs <- c({msg})\n',file=OUTPUT_SERVER)
}

 library("glue")
 library("stringr")
 parse_ui_table()
