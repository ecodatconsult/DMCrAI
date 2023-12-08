ffm2_camtraploc_ui <- function(){
  fluidPage(
    tabsetPanel(type = "tabs",
                tabPanel("Upload",
                         shinyFiles::shinyFilesButton(
                           'sf11',
                           '.shp oder .gpkg auswählen'
                           , '.shp oder .gpkg auswählen'
                           , multiple = FALSE
                           , class = "btn-primary"
                         ),
                         textOutput("colnames11_1"),
                         textOutput("colnames11_2"),
                         verbatimTextOutput(
                           "path_sf11",
                           placeholder = TRUE
                           ),
                         ## Projekt
                         selectizeInput(
                           'project11'
                           , label = "Projekt aus der Datenbank wählen"
                           , choices = projectsDB()
                           , multiple=FALSE
                           , selected = ""
                         ),
                         textOutput("status_check_names1"),
                         DT::DTOutput('x5'),
                         textOutput("status_check_names12"),
                         actionButton("upload11",
                                      "Übernehmen & hochladen",
                                      class = "btn-warning"),
                         textOutput("status_upload11")
                ),
                tabPanel(
                  "Karte",
                    leaflet::leafletOutput(
                      "standorte_map",
                      width = "100%",
                      height = "600px")
                  ),
                tabPanel(
                  "Neue Fotofallen Standorte",
                  DT::dataTableOutput(
                    'standorte11'
                    )
                  ),
                tabPanel(
                  "DB Fotofallen Standorte",
                  DT::dataTableOutput(
                    'standorte12'
                    )
                  )
    )
  )
}
