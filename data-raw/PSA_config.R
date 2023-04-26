#' Defines Monte Carlo runs for Sensitivity Analysis
#'
#' @param version any of KNOWN_VERSIONS (we can expand this list later)
#' @param mcruns number of Monte Carlo runs
#' @param psa_prefix bool. T = use column names { mcid, PSA_var1, PSA_var2, ... }
#'                         F = use column names { mcid, var1, var2, ... }
#' @param write_out bool. write results to data/PSA_config.rda
#' @param seed set random seed, to make draws reproducible
#'
#' @return data.frame with `mcruns` rows
#'
#' @examples
#'
#'   # For PSA model fitting:
#'   PSA_all_p <- draw_psa_runs(version = 'wide', 1e4, write_out = FALSE)
#'
#'   # To generate table for soft UI boundaries
#'   PSA_all_p <- draw_psa_runs(version = '1.1', 1000, psa_prefix = FALSE)
#'
draw_psa_runs <- function(version = '1.1',
                          mcruns = 1000,
                          psa_prefix = TRUE,
                          write_out = TRUE,
                          seed = 42) {

  KNOWN_VERSIONS <- c('1.0','1.1','wide')
  stopifnot( version %in% KNOWN_VERSIONS )

  # Make random draws reproducible
  set.seed(seed)

  # Generate id for Monte Carlo set
  PSA <- data.frame( mcid = c(1:mcruns) )

  # Stage I to III survival parameters
  if ( version == 'wide' ) {
      PSA$gamma_survival_1 <- runif(mcruns,-6,-3)
      PSA$gamma_survival_2 <- runif(mcruns,-5,-1)
      PSA$gamma_survival_3 <- runif(mcruns,-3,-0.01)
  } else {
    survmvn <- data.frame(c(-5.46208, -5.2077, -5.8016),
                          c(-3.8163, -3.75901, -3.8811),
                          c(-2.72264, -2.66053, -2.78617))
    survcovmat <- cov(survmvn)
    survmeans <- c(survmvn[1, 1], survmvn[1, 2], survmvn[1, 3])
    PSA[,c("gamma_survival_1", "gamma_survival_2", "gamma_survival_3")] <-
      MASS::mvrnorm(mcruns, survmeans, survcovmat)
  }

  # Metastatic survival parameters
  if ( version == 'wide' ) {
      PSA$meta_survival_54 <- runif(mcruns, -2.5, -0.5)
      PSA$meta_survival_74 <- runif(mcruns, -2, -0.3)
      PSA$meta_survival_99 <- runif(mcruns, -1.5, -0.1)
  } else {
    metmvn <- data.frame(c(-1.78723, -1.67922, -1.89434),
                         c(-1.38762, -1.33512, -1.49956),
                         c(-1.01051, -0.93338, -1.08304))
    metmat <- cov(metmvn)
    metmeans <- c(metmvn[1, 1], metmvn[1, 2], metmvn[1, 3])
    PSA[,c("meta_survival_54", "meta_survival_74", "meta_survival_99")] <-
      MASS::mvrnorm(mcruns, metmeans, metmat)
  }

  # Mammography with sensitivity conditional on tumour diameter parameters W-F
  PSA$beta_1 <- rnorm(mcruns, 1.47, 0.1)
  PSA$beta_2 <- rnorm(mcruns, 6.51, 0.5)

  # Mammography sensitivity by volpara density grade from PREVENTICON
  PSA$VDG1_sen <- rbeta(mcruns, 96, 16)
  PSA$VDG2_sen <- rbeta(mcruns, 298, 86)
  PSA$VDG3_sen <- rbeta(mcruns, 212, 93)
  PSA$VDG4_sen <- rbeta(mcruns, 61, 39)
  # Sen_VDG_av <- 0.757 is this used?

  # Supplemental Screening CDRs
  PSA$MRI_cdr <- rbeta(mcruns, 99.495, 19799.5) # CDR for MRI in Mammo negative women (incremental)
  PSA$US_cdr <- rbeta(mcruns, 35.89, 11927) # CDR for US in Mammo negative women (incremental)

  # Tumour growth rate parameters
  if ( version == 'wide' ) {
    PSA$log_norm_mean <- runif(mcruns, 0.8, 1.2)
  } else {
    PSA$log_norm_mean <- rnorm(mcruns, 1.07, 0.09)
  }
  PSA$log_norm_sd <- rnorm(mcruns, 1.31, 0.11)

  # Costs
  PSA$cost_strat <- rlnorm(mcruns, 2.13387381, 0.06349671)
  PSA$costvar <- rnorm(mcruns, 0, 0.1020408)

  if ( version %in% c('1.1','wide') ) {
    PSA$costscreen <- rnorm(mcruns, 0, 0.1020408)
    PSA$cost_follow_up <- rnorm(mcruns, 0, 0.1020408)
    PSA$cost_biop <- rnorm(mcruns, 0, 0.1020408)
    PSA$cost_US <- rnorm(mcruns, 0, 0.1020408)
    PSA$cost_MRI <- rnorm(mcruns, 0, 0.1020408)
  }

  # Utility values
  if ( version == 'wide' ) {
    PSA$util_1to3 <- runif(mcruns,0.6,0.9)
    PSA$util_4 <- runif(mcruns,0.5,0.8)
  } else {
    utilmat <- data.frame(c(1 - 0.82, 1 - 0.81, 1 - 0.83),
                          c(1 - 0.75, 1 - 0.73, 1 - 0.77))
    lnutilmat <- log(utilmat)
    covutil <- cov(lnutilmat)
    utilmeans <- c(log(1 - 0.82), log(1 - 0.75))
    PSA[,c("util_1to3", "util_4")] <- 1 - exp(MASS::mvrnorm(mcruns, utilmeans, covutil))
  }

  # Save to data/PSA_config.rda
  if ( write_out ) {
    PSA_config <- PSA
    usethis::use_data(PSA_config, internal = FALSE, overwrite = TRUE)
  }

  # Use column names { mcid, PSA_var1, PSA_var2, ... }
  if ( psa_prefix ) {
    names <- colnames(PSA)
    prefixed <- (names != 'mcid')
    names[prefixed] <- paste("PSA", names[prefixed], sep = "_")
    colnames(PSA) <- names
  }

  invisible(PSA)
}
