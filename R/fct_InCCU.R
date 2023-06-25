#Complex but nicer table and graph [REQUIRES gt, tidyverse, magrittr]

#' `get_incCU_table`
#'
#' @description incremental Cost & Utility table
#' @param Names strategy labels
#' @param Costs vector of costs (GBP)
#' @param QALYs vector of QALYs
#'
#' @return a `tibble` with columns: `stratName`, `Cost`, `QALY`, `dom`, `extdom`,
#'  `comp`, `IncCost`, `IncQALY`, `ICER`, and `ID`. Additionally, for each `WTP` value `j`,
#'  there will be a pair of columns `NHBj` and `rankNHBj`.
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr arrange mutate select row_number
get_incCU_table <- function(Names,
                           Costs,
                           QALYs) {

  IncCU <- tibble::tibble(
    StratName = Names,
    Cost = as.numeric(Costs),
    QALY = as.numeric(QALYs),
    dom = FALSE,
    extdom = FALSE,
    comp = 1,
    IncCost = 0,
    IncQALY = 0,
    ICER = 0,
  ) %>%
  arrange(.data$Cost) %>%
  mutate(ID = row_number())

  nn <- NROW(Names)
  for (i in 2:nn) {
    comp <- max(which(!IncCU$dom[1:(i - 1)] & !IncCU$extdom[1:(i - 1)]))
    IncCU$comp[i] <- comp
    IncCU$IncCost[i] <- IncCU$Cost[i] - IncCU$Cost[comp]
    IncCU$IncQALY[i] <- IncCU$QALY[i] - IncCU$QALY[comp]
    IncCU$ICER[i] <- IncCU$IncCost[i] / IncCU$IncQALY[i]
    IncCU$dom[i] <- max(IncCU$QALY[1:(i - 1)]) > IncCU$QALY[i]
    IncCU$extdom[i] <- if (i == nn) FALSE
    else
      any(
        IncCU$QALY[(i + 1):nn] > IncCU$QALY[i] &
          (IncCU$Cost[(i + 1):nn] - IncCU$Cost[comp]) / (IncCU$QALY[(i + 1):nn] - IncCU$QALY[comp]) < IncCU$ICER[i]
      )
  }
  IncCU
}

#' `pretty_incCU_table`
#'
#' @param IncCU the result of `get_incCU_table`
#' @param WTP reference Willingness to Pay, default 20e3
#' @param .costDP,.qalyDP,.icerDP display digits for Cost, QALY, and ICER variables
#'
#' @return a `gt` rendered and formated version of `IncCU`
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate case_when select row_number desc
#' @importFrom gt gt cols_label cols_label_with tab_spanner cols_align opt_horizontal_padding opt_table_font
pretty_incCU_table <- function(IncCU,
                               WTP = 20e3,
                               .costDP = dp(IncCU$Cost, 2),
                               .qalyDP = dp(IncCU$QALY, 2),
                               .icerDP = dp(IncCU$ICER, 2)) {

  dp <- function(x, .min = 0, .N = 2) max(.min, round(.N - log10(stats::sd(x))))
  fmt <- function(x, DP) format(round(x, DP), nsmall = DP, big.mark = ",")

  IncCU %<>%
    mutate(
      strCost = fmt(.data$Cost, .costDP),
      strQALY = fmt(.data$QALY, .qalyDP),
      strIncCost = fmt(.data$IncCost, .costDP),
      strIncQALY = fmt(.data$IncQALY, .qalyDP),
      strICER = case_when(
        .data$dom ~ "dominated",
        .data$extdom ~ "extendedly dominated",
        .default = fmt(.data$ICER, .icerDP)
      ),
      NHB = .data$QALY - .data$Cost / WTP,
      rank = rank(desc(.data$NHB))
    )

  IncCU %>%
    select("ID",
                "StratName",
                "strCost",
                "strQALY",
                "strIncCost",
                "strIncQALY",
                "strICER",
                "NHB",
                "rank") %>%
    gt() %>%
    cols_label(
      StratName = "Strategy",
      strCost = "Costs\n(\uA3)",
      strQALY = "Effects\n(QALYs)",
      strIncCost = "Costs\n(\uA3)",
      strIncQALY = "Effects\n(QALYs)",
      strICER = "ICER\n(\uA3/QALY)",
      NHB = paste0("NHB\n(\uA3", WTP / 1000, "K/QALY)"),
      rank = paste0("NHB rank\n(\uA3", WTP / 1000, "K/QALY)")
    ) %>%
    tab_spanner(label = "Incremental",
                    columns = c("strIncCost", "strIncQALY", "strICER")) %>%
    cols_align(align = "center",
                   columns = -c("StratName")) %>%
    opt_horizontal_padding(scale = 2) %>%
    opt_table_font(font = "Arial")
}

#' `IncCUplot`
#'
#' @param IncCU table, as returned by `get_incCU_table`
#' @param WTP Willingness to pay (slope of reference lines)
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_x_continuous scale_y_continuous sec_axis theme theme_minimal unit %+replace% .pt
#' @importFrom ggplot2 element_text element_blank geom_abline geom_text geom_hline geom_vline geom_line geom_point geom_text geom_segment
#' @importFrom dplyr filter
plot_ce_table <- function(IncCU, WTP = NULL) {

  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

  .colours = list(background = "white",
                  line = cbPalette[6],
                  dots = cbPalette[3],
                  reflines = "gray80",
                  labels = "white",
                  axes = "black")

  .widths = list(axes = 1.0,
                 reflines = 0.5,
                 main = 1.0)

  if (is.null(WTP)) {
    m <- grep("^NHB\\d*",colnames(IncCU), perl = TRUE, value = TRUE)
    WTP <- as.numeric(sub("NHB(\\d*)","\\1",m))
  }
  if (rlang::is_empty(WTP)) WTP <- NULL
  if (length(WTP) > 1) WTP <- WTP[1]
  stopifnot(is.null(WTP) || is.numeric(WTP))

  origin.x <- IncCU$QALY[which(IncCU$Cost == min(IncCU$Cost))]
  origin.y <- min(IncCU$Cost)

  # axes limits and ticks
  ax.x <- fnAxisScale(min(IncCU$QALY), max(IncCU$QALY))
  ax.y <- fnAxisScale(origin.y, max(IncCU$Cost))

  x.breaks <- seq(ax.x$dMin, ax.x$dMax, ax.x$dMajor)

  plt <- IncCU %>% ggplot(aes(x = .data$QALY, y = .data$Cost)) +
    scale_x_continuous(
      limits = c(ax.x$dMin, ax.x$dMax * 1.000000001),
      breaks = x.breaks,
      labels = seq(ax.x$dMin, ax.x$dMax, ax.x$dMajor),
      name = "\nQALYs"
    ) +
    scale_y_continuous(
      limits = c(ax.y$dMin, ax.y$dMax * 1.000000001),
      breaks = seq(ax.y$dMin, ax.y$dMax, ax.y$dMajor),
      labels = scales::number_format(prefix = "\uA3", big.mark = ","),
      name = "Costs\n"
    ) +
    theme_minimal() %+replace% theme(
      # panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      axis.text.x.top = element_text(colour = .colours$axes),
      axis.text.y.right = element_text(colour = .colours$axes),
      axis.title.x.top = element_text(colour = .colours$axes, face = "bold"),
      axis.title.y.right = element_text(colour = .colours$axes, face = "bold"),

      axis.text = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      # axis.ticks = element_blank()
    )

  plt.height <- unit(0, 'npc') %>% grid::convertY('native', valueOnly = T)
  plt.width <- unit(1, 'npc') %>% grid::convertX('native', valueOnly = T)

  if (!is.null(WTP)) {

    # Add reference lines
    plt <- plt +
      geom_abline(
        slope = WTP,
        intercept = seq_through(
          ax.y$dMin - WTP * (ax.x$dMax - origin.x),
          ax.y$dMax + WTP * (origin.x - ax.x$dMin),
          origin.y,
          ax.y$dMajor
        ) - origin.x * WTP,
        colour = .colours$reflines,
        lty = "longdash",
        linewidth = .widths$reflines
      )
  }

  ticklen.x <- (ax.x$dMax - ax.x$dMin) / 100 * min(plt.width, plt.height) / plt.width
  ticklen.y <- (ax.y$dMax - ax.y$dMin) / 100 * min(plt.width, plt.height) / plt.height
  tblXT <- data.frame(x = seq_through(ax.x$dMin, ax.x$dMax, origin.x, ax.x$dMajor))
  tblYT <- data.frame(y = seq_through(ax.y$dMin, ax.y$dMax, origin.y, ax.y$dMajor))

  plt <- plt +
    geom_hline(yintercept = origin.y, linewidth = .widths$axes, colour = .colours$axes) +
    geom_vline(xintercept = origin.x, linewidth = .widths$axes, colour = .colours$axes) +
    geom_segment(data = tblXT,
                 aes(x = .data$x,
                     xend = .data$x,
                     y = origin.y - ticklen.y,
                     yend = origin.y + ticklen.y),
                 linewidth = .widths$axes) +
    geom_segment(data = tblYT,
                 aes(x = origin.x - ticklen.x,
                     xend = origin.x + ticklen.x,
                     y = .data$y,
                     yend = .data$y),
                 linewidth = .widths$axes) +

    # Main layer
    geom_line(
      data = IncCU %>% filter(!.data$dom & !.data$extdom),
      aes(x = .data$QALY,
                   y = .data$Cost),
      colour = .colours$line,
      linewidth = .widths$main
    ) +
    geom_point(shape = 19,
               colour = .colours$dots,
               size = 10) +
    geom_text(aes(label = .data$ID),
              colour = .colours$labels,
              size = 12 * 1 / 72 * 25.4,
              fontface = "bold")
}

#' `seq_through` return a sequence that goes through a value
#'
#' @param from min. start point (only reached if `mod(through - from, by) == 0`)
#' @param to max. end point (only reached if `mod(to - through, by) == 0`)
#' @param through pivot point, included unless `through < from | through > to`
#' @param by step size
#' @noRd
seq_through <- function(from, to, through, by) {
  c(if (from <= through & to >= through) rev(seq(through, from, -by)) else c(),
    if (to >= through + by) seq(through + by, to, by) else c())
}

#' Nice axis limits & ticks for values between `dMin` and `dMax`
#'
#' @param dMin min. value
#' @param dMax max. value
#' @return c(dMin, dMax, dMajor, dMinor)
#'
#' @source modified from Jon Peltier's: https://peltiertech.com/calculate-nice-axis-scales-in-excel-vba/
#' @noRd
fnAxisScale <- function(dMin, dMax, .nticks = 6) {

  if (dMax == dMin) {
    dTemp = dMax
    dMax = dMax * 1.01
    dMin = dMin * 0.99
  }

  if (dMax < dMin) {
    dTemp = dMax
    dMax = dMin
    dMin = dTemp
  }

  epsilon <- (dMax - dMin) * 0.01
  if (dMax != 0) {
    dMax = ifelse(dMax > 0, dMax + epsilon, min(dMax + epsilon, 0))
  }
  if (dMin != 0) {
    dMin <- ifelse(dMin > 0, max(dMin - epsilon, 0), dMin - epsilon)
  }
  if (dMax == 0 & dMin == 0) dMax = 1

  dPower <- log10(dMax - dMin)
  P <- trunc(dPower)

  # We want (X/10^P)/step ~ nticks, i.e.
  approx_step = 10^(dPower - P - log10(.nticks))

  # Then we find the nearest "nice" value {0.1, 0.2, 0.5, 1, 2, 5, 10}
  nice <- c(0.1,0.2,0.5,1,2,5,10)
  bin_edges <- do.call(function(x) {
    c(-Inf, 10^((x[-1] + x[-length(x)])/2), Inf)
    },
    list(log10(nice))
  )
  idx <- cut(approx_step, breaks = bin_edges)

  dScale <- c(0.10, 0.20, 0.5, 1.0, 2.0, 5, 10)[idx]
  dSmall <- c(0.02, 0.05, 0.1, 0.2, 0.5, 1,  2)[idx]

  dScale = dScale * 10 ^ trunc(dPower)
  dSmall = dSmall * 10 ^ trunc(dPower)

  list(dMin = dScale * trunc(dMin / dScale),
      dMax = dScale * (trunc(dMax / dScale) + 1),
      dMajor = dScale,
      dMinor = dSmall)
}
