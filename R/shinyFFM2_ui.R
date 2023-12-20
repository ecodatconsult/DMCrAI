shinyFFM2_ui <- function(choices, species_table, ...){
  ui <- shiny::fluidPage(

    theme =  shinythemes::shinytheme("sandstone"),
    shiny::titlePanel("ShinyFFM", "ShinyFFM"),
    keys::useKeys(),
    keys::keysInput("keys", c("left", "right", "space",
                              "pagedown", "pageup", "del",
                              "1", "2", "3", "4", "5",
                              "g+m", "g+w", "g+u",
                              "a+j", "a+s", "a+d", "a+n", "a+o",
                              "shift+s")),
    shiny::fluidRow(
      shiny::column(2, shiny::selectInput("project",
                                          "Projekt wählen",
                                          choices = choices$project,
                                          selected = NULL)),
      #shiny::column(2, checkboxInput("edit_classified", "Auch frühere Klassifizierungen bearbeiten", TRUE)),
      shiny::column(2, shiny::selectInput("deployment",
                                          "Deployment wählen",
                                          choices = "Erst Projekt wählen",
                                          selected = NULL)),
      shiny::column(2, shiny::actionButton("download1",
                                           "Deployment abrufen",
                                           icon = shiny::icon("download")
      )
      ),
      #shiny::column(1, conditionalPanel(condition = "output.unassigned_folders", shiny::actionButton("assign_folders", "Fehlende Ordner hinzufügen"))),
      #shiny::column(1, shiny::actionButton("assign_folders", "Fehlende Ordner hinzufügen", icon = icon("plus"))),
      shiny::column(2, shiny::actionButton("upload",
                                           "In Datenbank speichern",
                                           icon = shiny::icon("database")
      )
      ),
      shiny::column(1, shiny::actionButton("locations1",
                                           "Standorte verwalten",
                                           icon = shiny::icon("location-crosshairs")
      )
      ),
      shinyBS::bsTooltip("upload", "
                         Speichert in der Session klassifizierte Bilder in der Datenbank (Shortcut: shift+s)",
                         options = list(container = "body"))
    ),
    shinyBS::bsModal(id = "modal_add_standorte",
                     title = "Standortverwaltung",
                     trigger = "locations1",
                     size = "large",
                     shiny::uiOutput("add_standorte")
    ),
    shinyBS::bsModal(id = "modal_assign_deployment_parameters",
                     title =  "Deployment-Einstellungen",
                     trigger =  "download1",
                     size = "large",
                     shiny::fluidRow(
                       shiny::column(2,
                                     shiny::numericInput("md_threshold",
                                                         "Megadetector-Grenzwert",
                                                         value = 0.2,
                                                         min = 0,
                                                         max = 0.6,
                                                         step = 0.01)
                       ),
                       shiny::column(2,
                                     shiny::numericInput("event_definition",
                                                         "Eventdefinition [min]",
                                                         min = 2,
                                                         max = 15,
                                                         value = 5,
                                                         step = 1)
                       ),
                       shiny::column(2,
                                     shiny::checkboxInput("classify_bbox",
                                                          "Auch BoundingBoxes klassifizieren",
                                                          value = FALSE)
                       ),
                       shiny::column(2,
                                     shiny::selectInput("md_categories",
                                                        label = "Welche Events sollen angezeigt & klassifiziert werden?",
                                                        selected = c("md_animal"),
                                                        choices = c("md_animal", "md_vehicle", "md_person", "md_mixed", "md_empty"),
                                                        multiple = TRUE

                                     )
                       )

                     ),
                     br(),
                     h4("Deploymentübersicht"),
                     shiny::fluidRow(
                       shiny::dataTableOutput("deployment_summary")
                     ),
                     br(),
                     shiny::fluidRow(
                       shiny::column(3,
                                     shiny::textInput("classified_by",
                                                      "Klassifizierung durch:",
                                                      placeholder = "Name, Vorname")
                       )
                     ),
                     shiny::fluidRow(
                       shinyBS::bsButton(
                         "download2",
                         "Deployment abrufen",
                         icon = shiny::icon("download"),
                         size = "large",
                         type = "action",
                         block = TRUE,
                         disabled = TRUE
                       )
                     )
    ),

    shiny::conditionalPanel(condition = "output.dirs_available == true",
                            #shiny::fluidRow(shiny::imageOutput("event_imgs", height = "200px")), #display event images at top
                            shiny::fluidRow(shiny::column(6,shiny::imageOutput("full_img", height = "500px", brush = "manual_bbox")),

                                            shiny::column(6,
                                                          shiny::tabsetPanel(
                                                            shiny::tabPanel("Zoom auf BoundingBox",
                                                                            shiny::imageOutput("bbox_img", height = "500px")
                                                            ),
                                                            shiny::tabPanel("Eventanimation",
                                                                            fluidRow(
                                                                              column(6, (shiny::selectInput("event_imgs_animation_fps", "Bilder pro Sekunde", choices = c(1,2,5,10,20), selected = 2, width = "100%"))),
                                                                              column(6, shiny::numericInput("event_imgs_animation_sf", "Skalierungsfaktor", min = .025, max = 1, value = .15, step = .01, width = "100%"))),
                                                                            shiny::fluidRow(shiny::imageOutput("event_imgs_animated", height = "400px"))
                                                            )
                                                          ))

                            ),
                            shiny::fluidRow(
                              shiny::column(3),
                              shiny::column(6, shiny::textOutput("text_event_id")),
                              #htmltools::tags$head(htmltools::tags$style("#text_event_id{font-size: 20px;}")),
                              shiny::column(3)
                            ),

                            shiny::fluidRow(
                              shiny::column(1),
                              shiny::column(1, shiny::actionButton(inputId = "left_event", label = "Vorheriges Event", icon = shiny::icon("backward-fast"), width = "100%")),
                              shiny::column(1, shiny::actionButton(inputId = "left", label = "Vorherige Box", icon = shiny::icon("arrow-left"), width = "100%")),
                              shiny::column(1, shiny::selectInput("species", "Artname", choices = choices$species)),
                              shiny::column(1, shiny::numericInput("count", "Anzahl [nur Event]", 1, min = 1, step = 1)),
                              shinyBS::bsTooltip("species", "Artnamen eingeben. Es gibt folgende Shortcuts:\n1: Rothirsch\n2: Reh", options = list(container = "body")),
                              shiny::column(1, shiny::selectInput("sex", "Geschlecht", choices = choices$sex)),
                              shiny::column(1, shiny::selectInput("age_class", "Alter", choices = choices$age)),
                              shiny::column(1, shiny::selectInput("behaviour", "Verhalten", choices = choices$behaviour)),
                              shiny::column(1, shiny::textInput("id_of_animal", "ID-Merkmal", value = NA)),
                              shiny::column(1, shiny::textInput("notes", "Bemerkungen", value = NA)),
                              shiny::column(1, fluidRow(shiny::actionButton(inputId = "right", label = "Nächste Box", icon = shiny::icon("arrow-right"), width = "100%"))),
                              shiny::column(1, shiny::actionButton(inputId = "right_event", label = "Nächstes Event", icon = shiny::icon("forward-fast"), width = "100%")),
                              shiny::column(1)),
                            shiny::fluidRow(
                              shiny::column(2),
                              shiny::column(4, shiny::actionButton(inputId = "add2event", "Event: Klassifizierung hinzufügen (Leerzeichen)", icon = shiny::icon("circle-check"), width = "100%")),
                              #shiny::column(4, shiny::actionButton(inputId = "accept", "Bounding Box: Klassifizierung übernehmen (Leerzeichen)", icon = shiny::icon("circle-check"), width = "100%")),
                              shiny::column(2, shiny::conditionalPanel(condition = "output.bbox_type == 'manual'", shiny::actionButton(inputId = "remove_row", "Drücke 'Entf' um manuelle Bounding Box zu entfernen", icon = shiny::icon("trash")))),
                              shiny::column(2)
                            ),
                            shiny::fluidRow(
                              h4("Eventtabelle"),
                              DT::dataTableOutput("event_table")
                              # ,
                              # shiny::tableOutput("md_table")
                            )
    )
  )

  return(ui)
}

