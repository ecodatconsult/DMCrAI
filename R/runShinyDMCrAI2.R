#' @export

runShinyDMCrAI2 <- function(...){
  options(DT.options = list(pageLength = 5))
  Sys.setenv(TZ='UTC')
  shiny::shinyApp(shinyDMCrAI2_ui(), shinyDMCrAI2_server, ...)
}
