#' @export

runShinyDMCrAI <- function(...){
  options(DT.options = list(pageLength = 5))
  Sys.setenv(TZ='UTC')
  shiny::shinyApp(shinyDMCrAI_ui(), shinyDMCrAI_server, options = list("launch.browser" = TRUE), ...)
}
