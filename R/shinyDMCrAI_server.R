#' @export
#'
shinyDMCrAI2_server <- function(input, output, session) {
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
  #####  STEP 2 - BLUR IMAGES & UPLOAD #####
  ##########################################

  projectModal <- function(){
    shiny::modalDialog(
      shiny::textInput("project_id_text",
                "Wähle eine Projektbezeichnung",
                placeholder = 'Bspw. "Rotwildmanagement" or "Grünbrückenmonitoring"'
      ),

      footer = tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("project_id_accept", "OK")
      )
    )
  }

  shiny::observeEvent(input$accept, {
    shiny::showModal(projectModal())
  })


  shiny::observeEvent(input$project_id_accept, {

    print(md_out_updated$df)
    nrow(md_out_updated$df)
    md_out_updated$df$project_id <- input$project_id_text

    shiny::removeModal()


    #Upload image classification and show result

    readr::write_csv(md_out_updated$df, here("md_out.csv"))

    upload_results <- upload_md_data(md_out_updated$df)

    showNotification(
      upload_results$msg
    )

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
