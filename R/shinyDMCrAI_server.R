#' @export
#'
shinyDMCrAI_server <- function(input, output, session) {
  ##### SIDEBAR PANEL -- SET DIRECTORY AND PROVIDE INITIAL INFO #####


  # data.frame with exif info
  exif_info <- shiny::reactiveValues(df = NULL)

  # data.frame with md out json input
  md_out_updated <- shiny::reactiveValues(df = NULL)


  # refresh_nr: reactive value, increase by on if button is pressed. invalidating refresh_nr will trigger md_dir()
  refresh_nr <- shiny::reactiveVal(0)
  shiny::observe({
    refresh_nr(refresh_nr() + 1)
    exif_info$df <- NULL
    md_out_updated$df <- NULL
  }) %>%
    shiny::bindEvent(input$refresh, input$md_dir)

  shiny::observe({
    shiny::updateActionButton(session = session, inputId = "accept", "Klassifikation und Optionen übernehmen")
  }) %>%
    shiny::bindEvent(md_dir())


  # md_dir: reactive, selected directory
  md_dir <- shiny::reactive({
    refresh_nr()
    shiny::req(input$md_dir)
    rs <- shinyFiles::getVolumes()()
    shinyFiles::shinyDirChoose(input = input, id = "md_dir", roots = rs, session = session)
    return(shinyFiles::parseDirPath(rs, input$md_dir) %>% normalizePath(winslash = "\\"))
  })

  # to track status of megadetector processing (0 = nothing loaded, 1 = directory chosen, 2 = directory chosen and md_json.out exists) and enable/disable UI components
  output$md_status <- shiny::reactive({

    md_out_found <- 0
    md_dir_valid <- shiny::isTruthy(md_dir())
    md_finished <- !is.null(md_out_updated$df)
    print(md_dir_valid + md_out_found + md_finished)
    return(md_dir_valid + md_out_found + md_finished)
  })

  shiny::outputOptions(output, "md_status", suspendWhenHidden = FALSE)

  # render md_dir()
  output$md_dir_text <- shiny::renderPrint({
    paste0("Gewählt: ", md_dir())
  })


  ######################################
  ##### PANEL 1 - RUN MEGADETECTOR #####
  ######################################

  # render batch script
  output$bat_file <- shiny::renderText({
    paste(readLines("shiny_md.bat"), collapse = "\n")
  }) %>%
    shiny::bindEvent(input$runMD) %>%
    debounce(500)

  # reactive values to observer the megadetector run
  curr_time <- shiny::reactiveVal(NULL)
  start_time <- shiny::reactiveVal(NULL)

  shiny::observe({
    if(!is.null(start_time())){
      shiny::invalidateLater(3000)
      curr_time(Sys.time())
    }
  })

  # generate batch script and run megadetector
  shiny::observe({

    bat_loc <- switch(Sys.info()[['sysname']],
           Linux =  here::here("shiny_md.sh"),
           Windows = here::here("shiny_md.bat"),
           Darwin = here::here("shiny_md.sh"))

    with(read.csv(system.file("md.csv", package = "DMCrAI")),
         create_md_bat(pics_dir = md_dir(), md_out = md_dir(), py_scripts_loc = py_scripts_loc, md_model_loc = md_model_loc, bat_loc, run_info = FALSE, force.overwrite = FALSE, checkpoint_freq = -1, show_finish = TRUE))

    if(is.null(start_time())) start_time(Sys.time())
    shiny::showNotification(paste0(lubridate::now(), ": Starte Megadetector"), duration = NULL)
    shiny::updateActionButton(session = session, inputId = "runMD", label = "Megadetector läuft", icon = icon("mug-hot"))

    switch(Sys.info()[['sysname']],
           Linux = system(paste0("gnome-terminal -- bash ", bat_loc), wait = FALSE, intern = FALSE),
           Windows = shell.exec(bat_loc),
           Darwin = system(paste0("gnome-terminal -- bash ", bat_loc ), wait = FALSE, intern = FALSE))

  }) %>%
    shiny::bindEvent(input$runMD)

  # generate batch script and run exiftool
  shiny::observe({
    if(is.null(exif_info$df)){
      shiny::showNotification(paste0(lubridate::now(), ": Starte exiftool zum Auslesen der Zeitstempel.."), duration = NULL)
      exif_info$df <- exif_image_info(
        md_dir = normalizePath(paste0(dirname(md_dir()), "/", basename(md_dir()))) #remove trailing // of md_dir()
      ) %>%
        dplyr::mutate(filepath = normalizePath(filepath))

      shiny::showNotification(paste0(lubridate::now(), ": Alle Zeitstempel ausgelesen!"), duration = NULL)
    }
  }) %>%
    shiny::bindEvent(input$runMD) %>%
    shiny::debounce(200)


  md_out_exists <- shiny::reactive({
    file.exists(paste0(md_dir(), .Platform$file.sep, "md_out.json"))
  })


  # observer to check if megadetector has finished and to load json via refresh
  shiny::observe({
    print(start_time())
    if(file.exists(paste0(md_dir(), .Platform$file.sep, "md_out.json")) & !is.null(start_time())){

      finished <- file.exists(paste0(md_dir(), .Platform$file.sep, "megadetector_just_finished.txt"))

#
#       print(file.info(paste0(md_dir(), .Platform$file.sep, "md_out.json"))$mtime)
#       md_out_updated(file.info(paste0(md_dir(), .Platform$file.sep, "md_out.json"))$mtime > start_time())
#

      if(file.exists(paste0(md_dir(), .Platform$file.sep, "megadetector_just_finished.txt"))){
        non_empty_category <- c("md_animal", "md_person", "md_vehicle")

        md_out_updated$df <- lapply(load_md_out_imgs(paste0(md_dir(), .Platform$file.sep, "md_out.json")),
                         extract_md_info) %>%
          do.call(what = rbind) %>%
          dplyr::mutate(file = normalizePath(file, winslash = "\\")) %>%
          dplyr::mutate(category = as.numeric(category)) %>%
          dplyr::mutate(bbox_category = c("md_empty", "md_animal", "md_person", "md_vehicle")[category+1]) %>%
          dplyr::mutate(bbox_category = ifelse(conf < .2, "md_empty", bbox_category)) %>%
          dplyr::group_by(file) %>%
          dplyr::mutate(image_category = ifelse(sum(unique(bbox_category) %in% non_empty_category) > 1,
                                                "md_mixed",
                                                ifelse(any(non_empty_category %in% unique(bbox_category)),
                                                       non_empty_category[which(non_empty_category %in% unique(bbox_category))],
                                                       "md_empty"))) %>%
          dplyr::left_join(exif_info$df, by = c("file" = "filepath")) %>%
          dplyr::ungroup()



        shiny::updateActionButton(session = session, inputId = "runMD", label = "Megadetector starten", icon = icon("eye"))
        refresh_nr(refresh_nr() + 1)
        start_time(NULL)

        file.remove(paste0(md_dir(), .Platform$file.sep, "megadetector_just_finished.txt"))
      }
    }
  }) %>%
    shiny::bindEvent(curr_time())



  ##########################################
  #####  STEP 2 - PROJECT INFO         #####
  ##########################################

  # A modal opens once the input$accept button is clicked
  shiny::observeEvent(input$accept, {
    shiny::showModal(projectModal())
  })


  # Modal to enter or edit project information in core.projects
  projectModal <- function(){
    shiny::modalDialog(
      fluidRow(
        column(8,
               shiny::selectInput("project_id_db", "Projekt aus der Datenbank auswählen", choices = db_download(selection = "name", schema = "core", table = "projects") |> dplyr::pull())
               ),
        shiny::actionButton("download_project_db", "Download Projektdaten", icon = shiny::icon("download")),
        shiny::actionButton("download_project_new", "", icon = shiny::icon("plus"))
        ),
      shiny::conditionalPanel(condition = "input.download_project_db | input.download_project_new",
      h3("Neues Projekt hinzufügen bzw. Eingabe überprüfen"),
      fluidRow(column(4,
                      shiny::textInput(inputId = "project_id_name_short",
                                      label= "Kurzbezeichnung",
                                     placeholder = 'Kurzname wie "Rotwildmanagement"')
                      ),
               column(4,
                      shiny::textInput(inputId = "project_id_name",label =
                                       "Vollständige Bezeichnung",
                                       placeholder = 'Vollständiger Projektname')
               ),
               column(4,
                      shiny::textInput(inputId = "project_id_contact",label =
                                       "Kontaktperson",
                                       placeholder = 'vorname.name@mail.de')
               )),
               fluidRow(column(12,
                      shiny::textInput(inputId = "project_id_description",label =
                                       "Projektbeschreibung",
                                       placeholder = 'Kurze Beschreibung des Projekts', width = "100%")
                      )
               )),
      footer = tagList(
        shiny::modalButton("Abbrechen"),
        shinyBS::bsButton(
          "project_id_accept",
          "Projektdaten übernehmen & abspeichern",
          icon = shiny::icon("upload"),
          size = "large",
          type = "action",
          block = TRUE,
          disabled = TRUE
        ),
      )
    )
  }

  # initial project data is NA only
  project_data <- shiny::reactiveValues(
    df = data.frame(NA, NA, NA, NA, NA) |>
      setNames(db_column_names("core", "projects"))
    )

  # if information is downloaded from database, input fields are updated accordingly
  shiny::observe({
    project_data$df <- db_download_with_filter(selection = db_column_names("core", "projects"),
                            schema = "core",
                            table = "projects",
                            filter_col = "name",
                            filter_values = input$project_id_db)

    print(project_data$df)

    shiny::updateTextInput(session,
                           inputId = "project_id_name",
                           value = project_data$df$name)

    shiny::updateTextInput(session,
                           inputId = "project_id_name_short",
                           value = project_data$df$name_short)

    shiny::updateTextInput(session,
                           inputId = "project_id_contact",
                           value = project_data$df$contact)

    shiny::updateTextInput(session,
                           inputId = "project_id_description",
                           value = project_data$df$description)

  }) %>%
    bindEvent(input$download_project_db)

  # if a new project needs to be created, the project_data needs to be reset to NA only
  shiny::observeEvent(input$download_project_new, {
    project_data$df = data.frame(NA, NA, NA, NA, NA) |>
      setNames(db_column_names("core", "projects"))
  })

  # only if all text inputs have at least 3 characters, the database input can be updated
  #TODO:maybe increase
  shiny::observe({
    print("toggle action button")

    inputs <- c(input$project_id_name,
    input$project_id_name_short,
    input$project_id_contact,
    input$project_id_description)

    print(stringr::str_length(inputs))

    if(min(stringr::str_length(inputs)) > 2){
      shinyBS::updateButton(
        session,
        inputId = "project_id_accept",
        disabled = FALSE
      )
    }
  })

  # once all information is entered, it is either uploaded (appended) or updated
  shiny::observeEvent(input$project_id_accept, {

    project_data$df[, db_column_names("core", "projects")[-1]] <- c(
      input$project_id_name,
      input$project_id_name_short,
      input$project_id_contact,
      input$project_id_description)

   if(is.na(project_data$df$project_id)){
     db_upload(upload_data = project_data$df,
               schema = "core",
               table = "projects",
               idfield = "project_id",
               update_key = TRUE,
               append = TRUE,
               row.names = FALSE,
               overwrite = FALSE)

     project_data$df <- db_download_with_filter(selection = db_column_names("core", "projects"),
                                                schema = "core",
                                                table = "projects",
                                                filter_col = "name",
                                                filter_values = input$project_id_name)

   }else{
     con <- dbConnection()
     db_upsert(project_data$df, con, "core", "projects", idfields = "project_id", verbose = TRUE)
     DBI::dbDisconnect(con)
   }

    md_out_updated$df$project_id <- project_data$df$project_id
    shiny::removeModal()

    md_out_updated


    #Upload image classification and show result
    upload_results <- upload_md_data(md_out_updated$df)

    showNotification(
      upload_results$msg
    )

    ##########################################
    #####  STEP 4 - BLUR IMAGES          #####
    ##########################################

   # blur only images that have been added to the database
    if(!is.null(upload_results$uploaded_data)){
      if(input$blur & any(upload_results$uploaded_data$bbox_category %in%  c("md_person", "md_vehicle"))){
        imgs_2blur_list <- upload_results$uploaded_data %>%
          dplyr::filter(bbox_category %in% c("md_person", "md_vehicle") | (image_category == "md_mixed" & input$blur_md_mixed)) %>%
          dplyr::group_by(file) %>%
          dplyr::group_split()

        shiny::withProgress(message = "Personenbezogene Daten verpixeln", detail = "Fahrzeuge und Personen werden unkenntlich gemacht!", value = 0, {
          for(img in seq(length(imgs_2blur_list))){
            shiny::incProgress(1/length(imgs_2blur_list))
            blur_md_imgs(imgs_2blur_list[[img]], sigma = input$blur_sigma, radius = input$blur_radius)
          }
        })
      }
    }else{
      showNotification("No new images to blur, skipping")
    }
    #Blur bboxes with person or vehicles and animals if they are on the same image as persons or vehicles (md_mixed)



    shiny::showNotification(paste0(lubridate::now(), ": Prozessierung abgeschlossen, Bilder können nun manuell klassifiziert werden"))

    shiny::updateActionButton(session = session, inputId = "accept", "Klassifikation wurde übernommen (aktualisieren, um Änderungen in json einzulesen)")

    #refresh_nr(refresh_nr() + 1)

    print("Megadetector output processing complete - now continue working with FFM2 on camera trap images")
  })
}
