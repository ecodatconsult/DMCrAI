shinyFFM2_server <- function(input, output, session){

  #### KEY REACTIVE VALUES
  selected_bbox <- shiny::reactiveValues(md_out = NULL)
  manual_bbox <- shiny::reactiveValues(df = NULL)
  md_out <- shiny::reactiveValues(df = NULL)
  # counter for the selected bounding box (adjust prevents negative index)
  selected_bbox_num <- shiny::reactiveVal(1)


  #### UPDATE SELECTION OF DEPLOYMENTS BASED ON SELECTED PROJECT ####
  shiny::observe({
    shiny::updateSelectInput(session,
                             inputId = "deployment",
                             label = "Deployment wählen:",
                             choices = download_import_megadetector_deployments(input$project),
                             selected = download_import_megadetector_deployments(input$project)[1])
  }) %>%
    shiny::bindEvent(input$project)


  #### INPUT MODAL WHEN LOADING DEPLOYMENT

  # prevent loading deployment if attributes are not specified
  shiny::observe({
    print("toggle action button")

    if(stringr::str_length(input$classified_by) > 6 &
       stringr::str_detect(input$classified_by, ",") &
       input$classified_by != "Name, Vorname"){
      shinyBS::updateButton(
        session,
        "download2",
        "Deployment abrufen",
        icon = shiny::icon("download"),
        size = "large",
        block = TRUE,
        disabled = FALSE
      )
    }

  }) %>%
    bindEvent(input$classified_by)


  # close modal and reset reactive values
  shiny::observe({
    # reset reactive values
    selected_bbox$md_out <- NULL
    md_out$df <- NULL

    shinyBS::toggleModal(session,
                         "modal_assign_deployment_parameters",
                         toggle = "close")
  }) %>%
    shiny::bindEvent(input$download2)


  #### STATIC INFORMATION ABOUT POSSIBLE CHOICES WHEN UPDATING SELECTIONS
  choices <- reactive({
    species_table <- readr::read_csv2(system.file("arten.csv", package = "DMCrAI")) %>%
      dplyr::mutate(VernacularName = stringi::stri_trans_general(VernacularName, "Latin-ASCII")) %>%
      dplyr::mutate(ScientificName = stringi::stri_trans_general(ScientificName, "Latin-ASCII"))

    list(
      classified_by = c(NA, "Schneider, Anja", "Milles, Alexander"),
      species = species_table$VernacularName,
      sex = c("männlich", "weiblich", "unbestimmt", NA),
      age = c("juvenil", "subadult", "adult", "non-juvenil", "unbestimmt"),
      behaviour = c("foraging", "resting", "moving", "watching", "other", "unbestimmt", NA),
      project = download_import_megadetector_projects()
    )
  })


  #### CHECK WHETHER DEPLOYMENT HAS BEEN LOADED (UI UPDATES)
  deployment_loaded <- shiny::reactive({
    !is.null(md_out$df)
  })
  output$dirs_available <- reactive(deployment_loaded())
  shiny::outputOptions(output, "dirs_available", suspendWhenHidden = FALSE)



  #### HANDLE DOWNLOAD OF MEGADETECTOR OUTPUT
  # download if button in modal is activated
  #TODO: make post-processing a function which can be tested

  #   input <- list(project_id = "test", deployment = "wvb_ff_5034_220809", event_definition = 5, md_categories = "md_animal", md_threshold = 0.6)


  shiny::observe({
    md_out$df<-
    download_import_megadetector_bboxes(
      project_id = input$project,
      deployment = input$deployment) %>%
      dplyr::mutate(category = ifelse(conf <= input$md_threshold, 0, category)) %>%
      dplyr::mutate(bbox_category = ifelse(conf <= input$md_threshold, "md_empty", bbox_category)) %>%
      dplyr::group_by(file) %>%
      dplyr::filter(dplyr::n() == 1 | conf > input$md_threshold) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(x_off = ifelse(conf < input$md_threshold, NA, x_off)) %>%
      dplyr::mutate(y_off = ifelse(conf < input$md_threshold, NA, y_off)) %>%
      dplyr::mutate(width = ifelse(conf < input$md_threshold, NA, width)) %>%
      dplyr::mutate(height = ifelse(conf < input$md_threshold, NA, height)) %>%
      dplyr::mutate(event_num = 1+cumsum(c(0,diff(as.numeric(datetimeoriginal)))>60*input$event_definition)) %>%
      dplyr::mutate(event_id = paste0(project_id, "_", deployment, "_", event_num)) %>%
      dplyr::group_by(event_id) %>%
      dplyr::mutate(event_cat_num = sum(unique(bbox_category) %in% c("md_animal", "md_vehicle", "md_person"))) %>%
      dplyr::mutate(event_category = ifelse(event_cat_num == 0, "md_empty",
                                            ifelse(
                                              event_cat_num > 1, "md_mixed",
                                              bbox_category[bbox_category %in% c("md_animal", "md_vehicle", "md_person")][1])                                            )) %>%
      dplyr::ungroup() %>%
      dplyr::filter(event_category %in% input$md_categories) %>%
      dplyr::mutate(species = NA, sex = NA, age_class = NA, behaviour = NA, id_of_animal = NA, notes = NA, classified = FALSE)

    print(md_out$df)


      selected_bbox_num(1)
      selected_bbox$md_out <- md_out$df[selected_bbox_num_adjust(),]
   # }
  })  %>%
    shiny::bindEvent(input$download2)

  selected_bbox_num_adjust <- shiny::reactive({
    # print(selected_bbox_num())
    # print(selected_bbox$md_out)
    ifelse(selected_bbox_num() %% nrow(md_out$df) == 0,
           nrow(md_out$df),
           selected_bbox_num() %% nrow(md_out$df))
  })

  observe({
    selected_bbox$md_out <- md_out$df[selected_bbox_num_adjust(),]
  }) %>%
    bindEvent(selected_bbox_num_adjust())

  # observe({
  #   md_out$df <- selected_bbox_num_adjust()
  # })


  # update selected bounding box based on arrow input
  shiny::observe({
    if(input$keys == "left") selected_bbox_num(selected_bbox_num() - 1)
    if(input$keys == "right") selected_bbox_num(selected_bbox_num() + 1)
  }) %>%
    shiny::bindEvent(input$keys)

  # shiny::observe({
  #   if(input$keys == "del" & selected_bbox$md_out$bbox_category == "manual") {
  #
  #     if(sum(md_out$df$file == selected_bbox$md_out$file) == 1){
  #       md_out$df[selected_bbox_num_adjust(), ] <-  selected_bbox$md_out %>%
  #         dplyr::mutate(species = "Leerbild", sex = "unbestimmt", age_class = "unbestimmt", behaviour = "unbestimmt", id_of_animal = NA, notes = NA, x_off = NA, y_off = NA, width = NA, height = NA, obs_id = stringr::str_remove_all(obs_id, "_m"), conf = 0, category = 0, bbox_category = "md_empty")
  #     }else{
  #       md_out$df <- md_out$df[-selected_bbox_num_adjust(), ]
  #       shiny::showNotification(paste0(Sys.time(), ": manuell erstelle Bounding Box entfernt!"))
  #     }
  #   }
  # }) %>%
  #   shiny::bindEvent(input$keys)
  #
  shiny::observe({selected_bbox_num(selected_bbox_num() - 1)}) %>%
    shiny::bindEvent(input$left)

  shiny::observe({selected_bbox_num(selected_bbox_num() + 1)}) %>%
    shiny::bindEvent(input$right)
  #
  # shiny::observe({
  #   selected_bbox$md_out$species = input$species
  #   selected_bbox$md_out$sex = input$sex
  #   selected_bbox$md_out$age_class_class = input$age_class
  #   selected_bbox$md_out$behaviour = input$behaviour
  #   selected_bbox$md_out$id_of_animal = input$id_of_animal
  #   selected_bbox$md_out$notes = input$notes
  #   selected_bbox$md_out$classified = TRUE
  #
  #   md_out$df[selected_bbox_num_adjust(), ] <- selected_bbox$md_out
  #
  #   selected_bbox_num(selected_bbox_num() + 1)
  # }) %>%
  #   shiny::bindEvent(input$accept)
  #
  # # accept classification for current bbox
  # shiny::observe({
  #   if(input$keys == "space"){
  #     selected_bbox$md_out$species = input$species
  #     selected_bbox$md_out$sex = input$sex
  #     selected_bbox$md_out$age_class = input$age_class
  #     selected_bbox$md_out$behaviour = input$behaviour
  #     selected_bbox$md_out$id_of_animal = input$id_of_animal
  #     selected_bbox$md_out$classified = TRUE
  #
  #     md_out$df[selected_bbox_num_adjust(), ] <- selected_bbox$md_out
  #
  #     selected_bbox_num(selected_bbox_num() + 1)
  #
  #     #readr::write_csv(md_out$df, "md_classified_df.csv")
  #   }
  # }) %>%
  #   shiny::bindEvent(input$keys)

#
#   shiny::observe({
#
#     img_dim <- selected_bbox$md_out$file %>%
#       magick::image_read() %>%
#       magick::image_scale(geometry = "x480") %>%
#       magick::image_info() %>%
#       dplyr::select(width, height)
#
#     selected_bbox$md_out$x_off <- input$manual_bbox$xmin / img_dim$width
#     selected_bbox$md_out$y_off <- input$manual_bbox$ymin  / img_dim$height
#     selected_bbox$md_out$width <- (input$manual_bbox$xmax - input$manual_bbox$xmin) / img_dim$width
#     selected_bbox$md_out$height <- (input$manual_bbox$ymax - input$manual_bbox$ymin)  / img_dim$height
#     selected_bbox$md_out$category <- 4
#     selected_bbox$md_out$conf <- 0
#     selected_bbox$md_out$obs_id <- paste0(selected_bbox$md_out$obs_id, "_m")
#     selected_bbox$md_out$bbox_category <- "manual"
#
#     selected_bbox$md_out <-
#       selected_bbox$md_out %>%
#       dplyr::mutate(species = "unbestimmt", sex = "unbestimmt", age_class = "unbestimmt", behaviour = "unbestimmt", id_of_animal = NA, notes = NA)
#
#     # print(md_out$df$x_off[selected_bbox_num_adjust()])
#     # print(selected_bbox$md_out$x_off)
#     curr_bbox_xoff <- round(md_out$df$x_off[selected_bbox_num_adjust()], 3)
#     drawn_bbox_xoff <- round(selected_bbox$md_out$x_off, 3)
#
#     if(is.na(curr_bbox_xoff)){
#       md_out$df[selected_bbox_num_adjust(), ] <- selected_bbox$md_out
#       shiny::showNotification(paste0(Sys.time(), ": manuelle Bounding Box erstellt, drücke 'Entf' zum Löschen!"))
#
#     }else{
#       if(curr_bbox_xoff != drawn_bbox_xoff){
#         md_out$df <- md_out$df %>%
#           dplyr::add_row(selected_bbox$md_out, .before = selected_bbox_num_adjust())
#         shiny::showNotification(paste0(Sys.time(), ": manuelle Bounding Box erstellt, drücke 'Entf' zum Löschen!"))
#       }
#     }
#
#     print(selected_bbox$md_out)
#
#   }) %>%
#     shiny::bindEvent(input$manual_bbox) %>%
#     shiny::debounce(200)

  # output$bbox_type <- shiny::reactive({
  #   selected_bbox$md_out$image_category
  # })
  #
  # shiny::outputOptions(output, "bbox_type", suspendWhenHidden = FALSE)

  # accept classification for all bounding boxes in the picture

  # shiny::observe({
  #   if(input$keys == "pagedown"){
  #     selected_bbox_num(bbox + 1)
  #   }
  # })

  # shiny::observe({
  #   if(input$keys == "pagedown" & selected_bbox$md_out$bbox_category != "manual"){
  #     selected_bbox$md_out$species = input$species
  #     selected_bbox$md_out$sex = input$sex
  #     selected_bbox$md_out$age_class = input$age_class
  #     selected_bbox$md_out$behaviour = input$behaviour
  #     selected_bbox$md_out$id_of_animal = input$id_of_animal
  #     selected_bbox$md_out$classified = TRUE
  #
  #     current_file <- md_out$df$file[selected_bbox_num_adjust()]
  #
  #     for(bbox in which(md_out$df$file == current_file)) md_out$df[bbox, ] <- selected_bbox$md_out
  #
  #     selected_bbox_num(bbox + 1)
  #   }
  # }) %>%
  #   shiny::bindEvent(input$keys)
#
#   shiny::observe({
#     #print(input$keys)
#     if(input$keys == "1"){
#       shiny::updateSelectInput(session, "species", selected = "Rothirsch", label = "Artname", choices = choices()$species)
#     }
#     if(input$keys == "2"){
#       shiny::updateSelectInput(session, "species", selected = "Reh", label = "Artname", choices = choices()$species)
#     }
#     if(input$keys == "3"){
#       shiny::updateSelectInput(session, "species", selected = "Wildschwein", label = "Artname", choices = choices()$species)
#     }
#     if(input$keys == "4"){
#       shiny::updateSelectInput(session, "species", selected = "Fuchs", label = "Artname", choices = choices()$species)
#     }
#     if(input$keys == "5"){
#       shiny::updateSelectInput(session, "species", selected = "Mensch", label = "Artname", choices = choices()$species)
#     }
#
#     if(input$keys == "g+m"){
#       shiny::updateSelectInput(session, "sex", "Geschlecht", selected = "männlich", choices = choices()$sex)
#     }
#     if(input$keys == "g+w"){
#       shiny::updateSelectInput(session, "sex", "Geschlecht", selected = "weiblich", choices = choices()$sex)
#     }
#     if(input$keys == "g+u"){
#       shiny::updateSelectInput(session, "sex", "Geschlecht", selected = "unbestimmt", choices = choices()$sex)
#     }
#
#     if(input$keys == "a+j"){
#       shiny::updateSelectInput(session, "age_class", "Altersklasse", selected = "juvenil", choices = choices()$age)
#     }
#     if(input$keys == "a+s"){
#       shiny::updateSelectInput(session, "age_class", "Altersklasse", selected = "subadult", choices = choices()$age)
#     }
#     if(input$keys == "a+d"){
#       shiny::updateSelectInput(session, "age_class", "Altersklasse", selected = "adult", choices = choices()$age)
#     }
#     if(input$keys == "a+n"){
#       shiny::updateSelectInput(session, "age_class", "Altersklasse", selected = "non-juvenil", choices = choices()$age)
#     }
#     if(input$keys == "a+o"){
#       shiny::updateSelectInput(session, "age_class", "Altersklasse", selected = "unbestimmt", choices = choices()$age)
#     }
#   }) %>%
#     shiny::bindEvent(input$keys)
#
#   shiny::observe({
#     if(input$keys %in% c("space", "left", "right")){
#       selected_bbox$md_out <- md_out$df[selected_bbox_num_adjust(), ]
#       shiny::updateSelectInput(session, "behaviour", "Verhalten", selected = NA, choices = choices()$behaviour)
#     }
#   }) %>%
#     shiny::bindEvent(input$keys)
#
#   shiny::observe({
#     selected_bbox$md_out <- md_out$df[selected_bbox_num_adjust(), ]
#     shiny::updateSelectInput(session, "behaviour", "Verhalten", selected = NA, choices = choices()$behaviour)
#   }) %>%
#     shiny::bindEvent(input$left, input$right, md_out$df, input$accept)
#
#   shiny::observe({
#     if(!is.null(selected_bbox$md_out)){
#       if(selected_bbox$md_out$bbox_category != "md_animal"){
#         print("Updating inputs")
#         shiny::updateSelectInput(session, "sex", "Geschlecht", selected = "unbestimmt", choices = choices()$sex)
#         shiny::updateSelectInput(session, "age_class", "Altersklasse", selected = "unbestimmt", choices = choices()$age)
#         shiny::updateSelectInput(session, "behaviour", "Verhalten", selected = "unbestimmt", choices = choices()$behaviour)
#         shiny::updateTextInput(session, "id_of_animal", "ID-Merkmal", value = NA)
#
#         if(selected_bbox$md_out$bbox_category == "md_empty"){
#           shiny::updateSelectInput(session, "species", selected = "Leerbild", label = "Artname", choices = choices()$species)
#         }
#
#         if(selected_bbox$md_out$bbox_category == "md_person"){
#           shiny::updateSelectInput(session, "species", selected = "Mensch", label = "Artname", choices = choices()$species)
#         }
#         if(selected_bbox$md_out$bbox_category == "md_vehicle"){
#           shiny::updateSelectInput(session, "species", selected = "Fahrzeug", label = "Artname", choices = choices()$species)
#         }
#       }
#     }
#   })

  # selected_bbox <- function() 1


  event_images <- shiny::reactiveValues(imgs = NULL, event_num = -999)

  shiny::observe({
    if(deployment_loaded() & !is.null(selected_bbox$md_out)){
      print(event_images$event_num)
      print(selected_bbox$md_out$event_num)
      if(event_images$event_num != selected_bbox$md_out$event_num){
        print("loading event images")
        event_images$imgs <- ffm2_event_images(md_out = md_out,
                                                event_num = selected_bbox$md_out$event_num,
                                                scale_factor = input$event_imgs_animation_sf)
        event_images$event_num <- selected_bbox$md_out$event_num
      }
      print("skipping")
    }
  })

  #TODO: find a good way to display large images..
  # output$event_imgs <- shiny::renderImage({
  #   shiny::req(selected_bbox$md_out)
  #
  #   event_images() %>%
  #     ffm2_event_images_append()
  #
  # }, deleteFile = TRUE)

  output$event_imgs_animated <- shiny::renderImage({
    shiny::req(selected_bbox$md_out)

    event_images$imgs %>%
      ffm2_event_images_animate(fps = as.numeric(input$event_imgs_animation_fps))

  }, deleteFile = TRUE)

  output$bbox_img <- shiny::renderImage({
    shiny::req(selected_bbox$md_out)
    #print(selected_bbox$md_out)
    extract_bbox_md_imgs(selected_bbox$md_out, safe.mode = FALSE)
  }, deleteFile = TRUE)

  output$full_img <- shiny::renderImage({
    shiny::req(selected_bbox$md_out)
    #print(md_out$df)
    empty_img <- any(is.na((md_out$df %>% dplyr::filter(file == selected_bbox$md_out$file))$x_off))
    bbox_md_imgs(md_out$df %>% dplyr::filter(file == selected_bbox$md_out$file),
                         out.file = tempfile("full_img", fileext = ".png"),
                         safe.mode = FALSE,
                         skip = empty_img)
  }, deleteFile = TRUE)

  output$text_event_id <- shiny::renderText({
    shiny::req(selected_bbox$md_out)
    event_imgs <- md_out$df %>%
      dplyr::filter(!duplicated(file_id)) %>%
      dplyr::filter(event_id == selected_bbox$md_out$event_id)

    curr_img_pos <- which(event_imgs$file_id == selected_bbox$md_out$file_id)

    paste0("Event: ", selected_bbox$md_out$event_id, " - Bild ", curr_img_pos, " von ", nrow(event_imgs))
  })

  ### Eventtable https://stackoverflow.com/questions/53908266/r-shiny-remove-row-button-in-data-table
  buttonCounter <- 0L
  values <- reactiveValues()
  values$tab <- dplyr::tibble(
    EventNr = 0,
    Art = NULL,
                        Anzahl = NULL,
                        Geschlecht = NULL,
                        Alter = NULL,
                        Verhalten = NULL,
                        ID_Merkmal = NULL,
                        Bemerkungen = NULL,
                        id = 0) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Remove = util_removeButton(id, idS = "", lab = "Tab1"))

  proxyTable <- DT::dataTableProxy("tab")

  output$event_table <- DT::renderDataTable({
    req(deployment_loaded())
    DT::datatable(values$tab %>% dplyr::filter(EventNr == selected_bbox$md_out$event_num),
                  options = list(pageLength = 25,
                                 dom        = "rt"),
                  rownames = FALSE,
                  escape   = FALSE,
                  editable = TRUE)
  })

  observeEvent(input$remove_button_Tab1, {
    myTable <- values$tab
    s <- as.numeric(strsplit(input$remove_button_Tab1, "_")[[1]][2])
    myTable <- dplyr::filter(myTable, id != s)
    DT::replaceData(proxyTable, myTable, resetPaging = FALSE)
    values$tab <- myTable
  })
  observeEvent(input$add2event, {
    buttonCounter <<- buttonCounter + 1L
    myTable <- shiny::isolate(values$tab)
    myTable <- dplyr::bind_rows(
      myTable,
      dplyr::tibble(
        EventNr = selected_bbox$md_out$event_num,
        Art = input$species,
             Anzahl = input$count,
             Geschlecht = input$sex,
             Alter = input$age_class,
             Verhalten = input$behaviour,
             ID_Merkmal = input$id_of_animal,
             Bemerkungen = input$notes) %>%
        dplyr::mutate(id = buttonCounter,
               Remove = getRemoveButton(buttonCounter, idS = "", lab = "Tab1")))
    DT::replaceData(proxyTable, myTable, resetPaging = FALSE)
    values$tab <- myTable
  })


  # selected_bbox_num_adjust <- function() 19
  output$md_table <- shiny::renderTable({
    shiny::req(selected_bbox_num_adjust())
    rows <- (selected_bbox_num_adjust() - 2) : (selected_bbox_num_adjust() + 2) %% nrow(md_out$df)
    rows[rows == 0] <- nrow(md_out$df)

    table <- md_out$df[rows,] %>%
      dplyr::select(obs_id, file, conf, event_id, species, sex, age_class, behaviour, id_of_animal, notes)

    table[-ceiling(length(rows)/2),] <-  table[-ceiling(length(rows)/2),] %>% apply(c(1,2), function(x) paste0('<FONT COLOR="#666666">', x, "</FONT>"))
    table[ceiling(length(rows)/2),] <-  table[ceiling(length(rows)/2),] %>% apply(1, function(x) paste0("<strong>", x, "</strong>"))


    return(table)
  },sanitize.text.function=function(x){x})

  shiny::observe({
    msg <- upload_md_classified_data(md_out$df %>% dplyr::filter(classified),
                                             include.classified = input$edit_classified)
    shiny::showNotification(paste0(Sys.time(), " ", msg))
  }) %>%
    shiny::bindEvent(input$upload)

  shiny::observe({
    if(input$keys == "shift+s"){
      msg <- upload_md_classified_data(md_out$df %>% dplyr::filter(classified),
                                               include.classified = input$edit_classified)
      shiny::showNotification(paste0(Sys.time(), " ", msg))
    }
  }) %>%
    shiny::bindEvent(input$keys)

}

