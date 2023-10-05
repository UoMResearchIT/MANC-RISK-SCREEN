#' Metadata tags for browsers and social media
#' @importFrom metathis meta meta_general meta_social meta_google_scholar
#' @return shiny.tag.list
page_meta_data <- function(app_title = "MANC-RISK-SCREEN",
                           description = "Discrete event simulation model to predict the costs, outcomes, and cost-effectiveness of six breast cancer screening strategies in the UK."
                           ) {
  meta() |>
    meta_general(
      application_name = app_title,
      description = description,
      generator = "Shiny",
      subject = "Health Economics",
      rating = "General",
    ) |>
    meta_social(
      title = app_title,
      description = description,
      url = "https://shiny.its.manchester.ac.uk/MancRiskScreen/",
      image = "img/thumbnail.png",
      image_alt = "Example cost-Utility plot showing six strategies",
      twitter_creator = "@HealthEcon_MCR",
      twitter_card_type = "app",
      twitter_site = "@HealthEcon_MCR"
    ) |>
    meta_google_scholar(
      title = "Evaluation of a Stratified National Breast Screening Program in the United Kingdom: An Early Model-Based Cost-Effectiveness Analysis",
      author = c("Ewan Gray", "Anna Donten", "Nico Karssemeijer", "Carla van Gils", "D. Gareth Evans", "Sue Astley", "Katherine Payne"),
      publication_date = "2017/9/27",
      online_date = "2017/6/1",
      journal_title = "Value in Health",
      volume = 20,
      issue = 8,
      firstpage = 1100,
      lastpage = 1109,
      pdf_url = "https://doi.org/10.1016/j.jval.2017.04.012",
      issn = "1098-3015"
    )
}
