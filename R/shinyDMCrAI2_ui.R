#' @export
shinyDMCrAI2_ui <- function(){

  shiny::fluidPage(
    theme =  shinythemes::shinytheme("sandstone"),
    shiny::titlePanel("Run Megadetector"),
    shiny::wellPanel(
      shiny::fluidRow(
        shiny::column(
          2,
          shinyFiles::shinyDirButton(
            id = "md_dir",
            label = "Verzeichnis wählen",
            title = "Bitte einen Ordner auswählen (Bilder werden rekursiv ermittelt)",
            multiple = FALSE,
            buttonType = "default",
            class = "btn-primary",
            icon = shiny::icon("folder-open")
            )
      ),
      shiny::column(1, shiny::actionButton("refresh", "", icon = shiny::icon("redo"))),
      shiny::column(3, shiny::verbatimTextOutput("md_dir_text")),
      shiny::column(3, shiny::verbatimTextOutput("n_images_text"))),
      shiny::br(),
      shiny::conditionalPanel(condition = "output.md_status = 1",
                              shiny::fluidRow(
                                # shiny::column(3,
                                #              shiny::actionButton(
                                #                "setupBAT",
                                #                "Batchfile initialisieren",
                                #                icon =  shiny::icon("scroll"),
                                #                width = "100%")),
                                shiny::column(3,
                                              shiny::actionButton(
                                                "runMD",
                                                "Megadetector starten",
                                                icon =  shiny::icon("eye"),
                                                width = "100%"
                                                )
                                              ),
                                shiny::column(3,
                                              shiny::verbatimTextOutput("n_images_classified_text"))),
                              shiny::fluidRow(
                                shiny::column(6,
                                              shiny::verbatimTextOutput("bat_file")
                                              ),
                                shiny::column(6,
                                              DT::dataTableOutput("img_files_table")
                                              )
                                ),
                              shiny::conditionalPanel(
                                condition = "output.md_status = 2",
                              shiny::fluidRow(
                                shiny::column(3,
                                              shiny::checkboxInput(
                                                "blur",
                                                "Personen und Fahrzeuge (bounding boxes) unkenntlich machen",
                                                value = TRUE)
                                              ),
                                shiny::column(3,
                                              shiny::sliderInput(
                                                "blur_radius",
                                                "Blurparameter: Radius",
                                                min = 30,
                                                max = 100,
                                                value = 40,
                                                step = 1)
                                              ),
                                shiny::column(3,
                                              shiny::sliderInput(
                                                "blur_sigma",
                                                "Blurparameter: Sigma",
                                                min = 15,
                                                max = 50,
                                                value = 20,
                                                step = 1)
                                              )
                                ),
                              shiny::fluidRow(
                                shiny::actionButton(
                                  "accept",
                                  "Klassifikation und Optionen übernehmen",
                                  width = "100%")
                                ),
                              shiny::fluidRow(ggiraph::girafeOutput("md_threshold_figure"))
                              )
                              )
      )
    )
}
