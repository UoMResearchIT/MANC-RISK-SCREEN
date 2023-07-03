#' Plot cancer growth rate distribution
#'
#' @details
#' Tumor growth is modeled as a logistic function:
#'
#'  `V(t)^0.25 = Vm^0.25 / ( 1 + exp( -k*(t - t0) ))`
#'
#' where `V(t)` is the tumor volume, `V = (4 / 3) * pi * (d / 2) ^ 3` for diameter `d`
#' with growth rate `k` distributed according to:
#'
#'  `k ~ 0.25 * LOGNORMAL(log_norm_mean, log_norm_sd )`
#'
#' @param log_norm_mean mean of log-normal distribution for growth rate `k`
#' @param log_norm_sd variance of log-normal distribution for growth rate `k`
#' @param max_size max. growth diameter, used to define `Vm`
#' @param start_size starting growth diameter (sets time offset `t0`)
#'
#' @return plot object
#'
#' @importFrom stats qlnorm
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line theme element_rect xlab ylab coord_cartesian
#' @importFrom ggplot2 scale_colour_identity scale_alpha_manual guide_legend
#' @noRd
plot_cancer_growth_rate <- function(log_norm_mean = 1.07,
                                    log_norm_sd = 1.31,
                                    max_size = 128,
                                    start_size = 0.25) {

  p_values <- c(lower = 0.025, P25 = 0.25, mean = 0.5, P75 = 0.75, upper = 0.975)
  rate_centiles <- qlnorm(p_values, meanlog = log_norm_mean, sdlog = sqrt(log_norm_sd))

  # logistic function and its inverse
  logistic <- function(x, k, L, x0 = 0) {  L / ( 1 + exp( -k*(x - x0) )) }
  inv_logistic <- function(y, k, L, x0 = 0) { x0 - log(L/y - 1) / k }

  # Tumor growth is modeled as a logistic function of y = V(t)^0.25, where V is the tumor volume
  y <- function(d) ((4 / 3) * pi * (d / 2) ^ 3) ^ 0.25
  d <- function(y)  2 * (y ^ 4 / ((4 / 3) * pi)) ^ (1 / 3)

  L <- y(max_size)
  k <- 0.25*rate_centiles
  t0 <- -inv_logistic(y(start_size), k, L, 0) # time to reach start_size

  grow_size <- data.frame(t = seq(0, 20, length.out = 101))
  for (j in names(p_values)) {
    grow_size[,j] <- d(logistic(grow_size$t, k[j], L, t0[j]))
  }

  # App-uniform styles (colors, line-widths, text sizes, etc.)
  pltset <- plot_settings()

  #Plot results
  plt <- ggplot(grow_size) +
    geom_line(aes(x = .data$t,
                  y = .data$mean,
                  colour = pltset$color$line),
      lty = "longdash",
      linewidth = pltset$width$reflines
    ) +
    scale_colour_identity(name = NULL,
                          guide = 'legend',
                          labels = c('median')) +
    geom_ribbon(aes(x = .data$t,
                    ymin = .data$lower,
                    ymax = .data$upper,
                    alpha = "95"),
      fill = pltset$color$cyan
    ) +
    geom_ribbon(aes(x = .data$t,
                    ymin = .data$P25,
                    ymax = .data$P75,
                    alpha = "50"),
      fill = pltset$color$cyan
    ) +
    scale_alpha_manual(
      name = NULL,
      values = c("50" = 0.2, "95" = 0.2),
      labels = c('50% CI', '95% CI'),
      guide = guide_legend(override.aes = list(alpha = c(0.4, 0.2)))
    ) +
    xlab("\nTime (Years)") +
    ylab("Tumour size (mm)\n") +
    coord_cartesian(ylim = c(0, 100), xlim = c(0, 20)) +
    theme(
      panel.background = element_rect(fill = 'transparent'),
      plot.background = element_rect(fill = 'transparent', color = NA),
      axis.text = element_text(size = pltset$text$size),
      axis.text.x.top = element_text(colour = pltset$color$axes),
      axis.text.y.right = element_text(colour = pltset$color$axes),
      axis.title.x.top = element_text(colour = pltset$color$axes, face = "bold"),
      axis.title.y.right = element_text(colour = pltset$color$axes, face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.title = element_blank(),
      legend.position = c(0.8, 0.3),
      legend.key = element_rect("transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.text=element_text(size = pltset$text$size)
    )
}

#' Plot mammography with sensitivity conditioned on tumour diameter and VDG
#'
#' @param beta_1 [mm] how rapidly sensitivity changes with tumour size
#' @param beta_2 [mm] size of tumour for which sensitivity is equal to 50%.
#' @param sensitivity_max sensitivity cap (0.95)
#' @param Sen_VDG (4-vector) relative sensitivity by Volpara Density Group
#'
#' @return plot object
#'
#' @importFrom ggplot2 ggplot aes geom_line theme element_rect xlab ylab coord_cartesian scale_colour_manual
#' @noRd
plot_sensitivity <- function(beta_1 = 1.47,
                             beta_2 = 6.51,
                             sensitivity_max = 0.95,
                             Sen_VDG = c(0.85, 0.78, 0.7, 0.61)) {

  Sen_VDG_av <- mean(Sen_VDG) # 0.757;

  # Create vector of cancer sizes (sensitivity pretty much always maxes out at 20mm)
  Ca_size <- seq(from = 0, to = 20, by = 0.2)

  # Create vector of sensitivities by size
  sens_size <- pmin(exp((Ca_size - beta_2) / beta_1) / (1 + exp((Ca_size - beta_2) / beta_1)), sensitivity_max)

  # Calculate size/density specific sensitivity
  dense_OR <- (Sen_VDG / (1 - Sen_VDG)) / (Sen_VDG_av / (1 - Sen_VDG_av))

  sens_VDG <- outer(sens_size / (1 - sens_size), dense_OR)
  sens_VDG <- sens_VDG / (1 + sens_VDG)
  colnames(sens_VDG) <- c("VDG1","VDG2","VDG3","VDG4")

  data <- reshape2::melt(cbind(data.frame(size = Ca_size), sens_VDG), id.vars = "size")

  # App-uniform styles (colors, line-widths, text sizes, etc.)
  pltset <- plot_settings()

  #Plot results
  plt <- ggplot(data, aes(.data$size, .data$value, col = .data$variable)) +
    geom_line(
      linewidth = pltset$width$main
    ) +
    xlab("\nCancer size (mm)") +
    ylab("Sensitivity\n") +
    coord_cartesian(ylim = c(0, 1), xlim = c(0, 20)) +
    theme(
      panel.background = element_rect(fill = 'transparent'),
      plot.background = element_rect(fill = 'transparent', color = NA),
      axis.text = element_text(size = pltset$text$size),
      # axis.text.x.top = element_text(colour = pltset$color$axes),
      # axis.text.y.right = element_text(colour = pltset$color$axes),
      # axis.title.x.top = element_text(colour = pltset$color$axes, face = "bold"),
      # axis.title.y.right = element_text(colour = pltset$color$axes, face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.title = element_blank(),
      legend.position = c(0.8, 0.3),
      legend.key = element_rect("transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.text = element_text(size = pltset$text$size)
    ) +
    scale_colour_manual(values = unlist(unname(pltset$color)))

  print(plt)
}
