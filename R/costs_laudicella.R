#' Cost of Breast Cancer
#'
#' @description
#' Refactored data from Laudicella et al. (2016) https://doi.org/10.1038/bjc.2016.77
#'
#' NEEDS PROPPER DOCUMENTATION!
#'
#' @importFrom magrittr %>%
#' @importFrom tibble tribble
#' @import dplyr
#' @return table with columns {"Yr", "Stage", "Age", "Cost", "DCost",
#'  "DCost.i", "disc", "DCost.i.d", "CDCost.i.d", "Yr1", "Yr2", "Yr3"},
#'  whose meaning is known only to the chosen ones.
#'
costs_laudicella <- function() {
  cost_table <- tibble::tribble(
    ~Yr, ~Early_18.64, ~Late_18.64, ~Diff1, ~Early_65plus, ~Late_65plus, ~Diff2,
    0, 464, 607, 143, 1086, 1324, 238,
    1, 10746, 13315, 2569, 7597, 8804, 1207,
    2, 3357, 5785, 2429, 2529, 3650, 1121,
    3, 1953, 3782, 1829, 2156, 3170, 1014,
    4, 1627, 2932, 1305, 2230, 2924, 693,
    5, 1617, 2841, 1225, 2077, 2957, 880,
    6, 1547, 2645, 1099, 2174, 2783, 609,
    7, 1394, 2618, 1225, 2063, 2903, 840,
    8, 1376, 2559, 1183, 2134, 2454, 320,
    9, 1279, 1848, 569, 2204, 2932, 728
  ) %>%
    select(-Diff1, -Diff2) %>%
    tidyr::pivot_longer(
      cols = contains("6"),
      names_to = c("Stage", "Age"),
      names_sep = "_",
      values_to = "Cost"
    ) %>%
    group_by(Stage, Age) %>%
    mutate(
      DCost = Cost - first(Cost),
      DCost.i = DCost * 1.219312579, # NHSCII inflator for 2010/11-->2020/21
      disc = 1 / 1.035^(Yr - 0.5),
      DCost.i.d = DCost.i * disc,
      CDCost.i.d = cumsum(DCost.i.d),
      Yr1 = as.factor(Yr == 1),
      Yr2 = as.factor(Yr == 2),
      Yr3 = as.factor(Yr == 3)
    ) %>%
    filter(Yr > 0) %>%
    arrange(Stage, Age, Yr)

  # log-linear model
  mod <- lm(
    data = cost_table,
    formula = log(DCost) ~ (Yr1 + Yr2 + Yr3 + Yr) * Stage * Age
  )

  # prediction matrix
  tblNewDat <- tidyr::crossing(Yr = 1:50, Stage = c("Early", "Late"), Age = c("18.64", "65plus")) %>%
    mutate(
      Yr1 = as.factor(Yr == 1),
      Yr2 = as.factor(Yr == 2),
      Yr3 = as.factor(Yr == 3)
    )

  # generate predictions
  tblNewDat %>%
    bind_cols(pred = mod %>% predict(newdata = tblNewDat)) %>%
    mutate(DCost.p = exp(pred)) -> tblPred

  # make lookup table
  tblLookup <- tblPred %>%
    filter(Yr == 1) %>%
    mutate(across(c(Yr, pred, DCost.p), ~0)) %>%
    bind_rows(tblPred) %>%
    group_by(Stage, Age) %>%
    mutate(
      DCost.p.i = DCost.p * 1.219312579, # NHSCII inflator for 2010/11-->2020/21
      disc = 1 / 1.035^(Yr - 0.5),
      DCost.p.i.d = DCost.p.i * disc,
      CDCost.p.i.d = cumsum(DCost.p.i.d),
      StageEarly = Stage == "Early",
      AgeYoung = Age == "18.64"
    ) %>%
    arrange(Stage, Age, Yr) %>%
    ungroup()

  return(tblLookup)
}
