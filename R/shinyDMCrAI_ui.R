#' @export
shinyDMCrAI_ui <- function(){

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
                                                    icon = shiny::icon("folder-open"))
      ),
      shiny::column(1, shiny::actionButton("refresh", "", icon = shiny::icon("redo"))),
      shiny::column(3, shiny::verbatimTextOutput("md_dir_text")),
      shiny::column(3, shiny::verbatimTextOutput("n_images_text"))),
      shiny::br(),
      shiny::conditionalPanel(condition = "output.md_status > 0",
                              shiny::tabsetPanel(type = "pills",
                                   ##### UI PANEL 1 ####

                                   shiny::tabPanel("Megadetector ausführen",
                                                   shiny::br(),
                                                   shiny::fluidRow(
                                                     shiny::column(3,
                                                                   shiny::actionButton(
                                                       "setupBAT",
                                                       "Batchfile initialisieren",
                                                       icon =  shiny::icon("scroll"),
                                                       width = "100%")),
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
                                            )
                                   ),
                                   ##### UI PANEL 2 ####
                                   shiny::tabPanel("Megadetector Output",
                                                   shiny::conditionalPanel(condition = "output.md_status > 1",
                                                                           shiny::fluidRow( shiny::column(6,
                                                                                                  shiny::fluidRow(DT::dataTableOutput("md_out_table", height = "440px"))),
                                                                                            shiny::column(6,  shiny::fluidRow(
                                                                                              shiny::column(4,
                                                                                                            shiny::numericInput(
                                                                                 "bbox_threshold",
                                                                                 "Konfidenzlevel für Bounding Box",
                                                                                 min = 0,
                                                                                 max = 1,
                                                                                 value = 0.6,
                                                                                 step = 0.01)),
                                                                                 shiny::column(4,
                                                                                               shiny::checkboxInput(
                                                                                 "bbox_preview",
                                                                                 "Bounding Box zeichnen (langsamer)?",
                                                                                 value = TRUE)),
                                                                                 shiny::column(4,
                                                                                               shiny::conditionalPanel("input.bbox_preview",
                                                                                                                       shiny::checkboxInput(
                                                                                                  "safe_mode",
                                                                                                  "Vorschau blurren?",
                                                                                                  value = TRUE)))
                                                                      ),
                                                                      shiny::fluidRow(
                                                                        shiny::imageOutput("img_hover")))),
                                                                      shiny::fluidRow(
                                                                        ggiraph::girafeOutput("md_out_figure", height = "400px"))
                                            )),
                                   ##### UI PANEL 3 ####
                                   shiny::tabPanel("Klassifizierung übernehmen",
                                                   shiny::conditionalPanel(
                                              condition = "output.md_status > 1",
                                              shiny::fluidRow(
                                                shiny::checkboxInput(
                                                  "show_advanced_options",
                                                  label = "Erweiterte Optionen",
                                                  value = FALSE)),
                                              shiny::conditionalPanel(
                                                condition = "input.show_advanced_options",
                                                shiny::fluidRow(
                                                  shiny::column(3,
                                                                shiny::numericInput(
                                                           "animal_threshold",
                                                           "Conf-Grenzwert Tier",
                                                           0.6,
                                                           0,1)
                                                  ),
                                                  shiny::column(3,
                                                                shiny::numericInput(
                                                           "person_threshold",
                                                           "Conf-Grenzwert Person",
                                                           0.6,
                                                           0,1)
                                                  ),
                                                  shiny::column(3,
                                                                shiny::numericInput(
                                                           "vehicle_threshold",
                                                           "Conf-Grenzwert Fahrzeug",
                                                           0.6,
                                                           0,1))
                                                ),
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
                                                  shiny::selectInput(
                                                    "process_empty_img_mode",
                                                    "Option für den Umgang mit Leerbildern wählen",
                                                    choices = c("Keine Bilder löschen, Upload in Datenbank" = "upload",
                                                                "Leerbilder teilweise löschen" = "delete_some",
                                                                "Leerbilder u Personenbilder umbenennen" = "rename"),
                                                    selected = "rename"
                                                  ),
                                                  shiny::numericInput(
                                                    inputId =  "event_definition_minutes",
                                                    label = "Evendefintion, zeitlicher Abstand zw. Bildern [min]",
                                                    value = 5,
                                                    min = 1,
                                                    max = 60,
                                                    step = .5
                                                  )
                                                )
                                              ),
                                              shiny::fluidRow(
                                                shiny::actionButton(
                                                  "accept",
                                                  "Klassifikation und Optionen übernehmen",
                                                  width = "100%")
                                              ),
                                              shiny::fluidRow(ggiraph::girafeOutput("md_threshold_figure")))
                                   ))
      )
    )
  )
}
