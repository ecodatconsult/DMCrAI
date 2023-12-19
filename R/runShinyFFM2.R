#' @export
#'
#'
#'
runShinyFFM2 <- function(){

  species_table <- readr::read_csv2(system.file("arten.csv", package = "DMCrAI")) %>%
    dplyr::mutate(VernacularName = stringi::stri_trans_general(VernacularName, "Latin-ASCII")) %>%
    dplyr::mutate(ScientificName = stringi::stri_trans_general(ScientificName, "Latin-ASCII"))

  choices <-
    list(
      classified_by = c(NA, "Schneider, Anja", "Milles, Alexander"),
      species = species_table$VernacularName,
      sex = c("männlich", "weiblich", "unbestimmt", NA),
      age = c("juvenil", "subadult", "adult", "non-juvenil", "unbestimmt"),
      behaviour = c("foraging", "resting", "moving", "watching", "other", "unbestimmt", NA),
      project = download_import_megadetector_projects()
    )

  options(DT.options = list(pageLength = 5))
  Sys.setenv(TZ='UTC')
  shiny::shinyApp(shinyFFM2_ui(choices, species_table), shinyFFM2_server, options = list("launch.browser" = TRUE))
}
