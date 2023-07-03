#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "MancRiskScreenUI")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}

#' App-uniform styles (colors, line-widths, text sizes, etc.
#' @noRd
plot_settings <- function() {

  # Color-blind friendly palette, from:
  #   Masataka Okabe, Kei Ito, "Color Universal Design (CUD)",
  #   University of Tokyo, Institute for Molecular and Cellular Biosciences (Japan)
  #   11.20.2002. URL: https://jfly.uni-koeln.de/color/
  cbpalette = list(blue = "#0072B2",
                   cyan = "#56B4E9",
                   green = "#009E73",
                   yellow = "#F0E442",
                   orange = "#E69F00",
                   red = "#D55E00",
                   pink = "#CC79A7",
                   gray = "#999999")
  list(
    color = c(
      cbpalette,
      list(
        background = "white",
        line = cbpalette$red,
        dots = cbpalette$green,
        reflines = "gray80",
        labels = "white",
        axes = "black"
      )
    ),
    width = list(
      axes = 1.0,
      reflines = 0.5,
      main = 1.0),

    text = list(
      size = 12)
  )
}
