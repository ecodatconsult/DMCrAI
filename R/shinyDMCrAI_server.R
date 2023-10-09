#' @export
#'
shinyDMCrAI_server <- function(input, output, session) {
    ##### SIDEBAR PANEL -- SET DIRECTORY AND PROVIDE INITIAL INFO #####

    # refresh_nr: reactive value, increase by on if button is pressed. invalidating refresh_nr will trigger md_dir()
    refresh_nr <- shiny::reactiveVal(0)
    shiny::observe({
      refresh_nr(refresh_nr() + 1)
    }) %>%
      shiny::bindEvent(input$refresh)

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
      if(md_dir_valid) md_out_found <- md_out_exists()
      print(md_dir_valid + md_out_found)
      return(md_dir_valid + md_out_found)
    })

    shiny::outputOptions(output, "md_status", suspendWhenHidden = FALSE)

    # render md_dir()
    output$md_dir_text <- shiny::renderPrint({
      paste0("Gewählt: ", md_dir())
    })

    # reactive, recursively screen for image files
    img_files <- shiny::reactive({
      #TODO: this may cause issues with maany files..! use some indexing?
      list.files(
        md_dir(),
        pattern = ".jpg$|.jpeg$|.png$",
        recursive = TRUE,
        ignore.case = TRUE,
        full.names = TRUE
      ) %>%
        normalizePath(winslash = "\\")
    })

    # image files that are already processed by MD
    classified_images <- shiny::reactive({
      img_files() %in% md_out()$file
    })

    # number of unclassified images
    sum_unclassified_images <- shiny::reactive(sum(!classified_images()))

    # update action button based on MD status
    shiny::observe({
      if(sum_unclassified_images() == 0){
        shiny::updateActionButton(session = session, inputId = "runMD", label = "Klassifikation abgeschlossen!", icon = shiny::icon("face-grin-stars"))
      }else{
        shiny::updateActionButton(session = session, inputId = "runMD", label = "Megadetector starten", icon = shiny::icon("eye"))
      }
    }) %>%
      shiny::bindEvent(sum_unclassified_images())

    output$n_images_text <- shiny::renderPrint({
      paste0("Gefundene Bilder: ", length(img_files()))
    })

    output$n_images_classified_text <- shiny::renderPrint({
      paste0("Unprozessierte Bilder: ", sum_unclassified_images())
    })

    ######################################
    ##### PANEL 1 - RUN MEGADETECTOR #####
    ######################################

    # generate batch script
    shiny::observe({
      with(read.csv(system.file("md.csv", package = "DMCrAI")),
           create_md_bat(pics_dir = md_dir(), md_out = md_dir(), py_scripts_loc = py_scripts_loc, md_model_loc = md_model_loc, bat_loc = "shiny_md.bat", run_info = FALSE, force.overwrite = FALSE, checkpoint_freq = -1))
    }) %>%
      shiny::bindEvent(input$setupBAT)

    # render batch script
    output$bat_file <- shiny::renderText({
      paste(readLines("shiny_md.bat"), collapse = "\n")
    }) %>%
      shiny::bindEvent(input$setupBAT)

    # reactive values to observer the megadetector run
    curr_time <- shiny::reactiveVal(NULL)
    start_time <- shiny::reactiveVal(NULL)

    shiny::observe({
      if(!is.null(start_time())){
        shiny::invalidateLater(3000)
        curr_time(Sys.time())
        print(curr_time())
      }
    })

    exif_image_info_df <- shiny::reactive({
      exif_image_info_df <-
        exif_image_info(
          md_dir = normalizePath(paste0(dirname(md_dir()), "/", basename(md_dir()))) #remove trailing // of md_dir()
        ) %>%
        dplyr::mutate(filepath = normalizePath(filepath))
      return(exif_image_info_df)
    })


    # generate batch script and run megadetector
    shiny::observe({
      with(read.csv(system.file("md.csv", package = "DMCrAI")),
           create_md_bat(pics_dir = md_dir(), md_out = md_dir(), py_scripts_loc = py_scripts_loc, md_model_loc = md_model_loc, bat_loc = "shiny_md.bat", run_info = FALSE, force.overwrite = FALSE, checkpoint_freq = -1))


      if(is.null(start_time())) start_time(Sys.time())
      shiny::updateActionButton(session = session, inputId = "runMD", label = "Indiziere Bilder", icon = icon("file-image"))
      print(exif_image_info_df())
      shiny::updateActionButton(session = session, inputId = "runMD", label = "Megadetector läuft", icon = icon("mug-hot"))
      shell.exec("shiny_md.bat")
    }) %>%
      shiny::bindEvent(input$runMD)


    # md_dir <- function() "C:\\md_pics\\beispieldaten_large\\"
    output$img_files_table <- DT::renderDataTable({
      DT::datatable(data.frame("Pfad" = img_files(),
                               "Bereits klassizifizert" = ifelse(classified_images(), "ja", "nein")), style = "bootstrap")
    })


    md_out_exists <- shiny::reactive({
      file.exists(paste0(md_dir(), .Platform$file.sep, "md_out.json"))
    })

    shiny::observe({
      print(start_time())
      if(file.exists(paste0(md_dir(), .Platform$file.sep, "md_out.json")) & !is.null(start_time())){

        print(file.info(paste0(md_dir(), .Platform$file.sep, "md_out.json"))$mtime)
        new_md_out <- file.info(paste0(md_dir(), .Platform$file.sep, "md_out.json"))$mtime > start_time() | sum_unclassified_images() == 0
        if(new_md_out){
          shiny::updateActionButton(session = session, inputId = "runMD", label = "Klassifikation abgeschlossen!", icon = icon("face-grin-stars"))
          refresh_nr(refresh_nr() + 1)
          start_time(NULL)
        }
      }

    }) %>%
      shiny::bindEvent(curr_time())


    ######################################
    #####   PANEL 2 - INSPECT MD_OUT #####
    ######################################

    md_out <- shiny::reactive({
      if(md_out_exists()){
        md_out <- lapply(load_md_out_imgs(paste0(md_dir(), .Platform$file.sep, "md_out.json")),
                         extract_md_info) %>%
          do.call(what = rbind) %>%
          dplyr::mutate(file = normalizePath(file, winslash = "\\")) %>%
          dplyr::mutate(category = as.numeric(category)) %>%
          dplyr::mutate(category_word = c("md_empty", "md_animal", "md_person", "md_vehicle")[category+1])
      }else{
        md_out <- data.frame(file = NA)
      }
      return(md_out)
    })

    md_out_bbox <- shiny::reactive({
      md_out() %>%
        dplyr::filter(conf >= input$bbox_threshold)
    })

    md_out_events_df <- shiny::reactive({
      md_out_events_df <-  md_out() %>%
        dplyr::left_join(exif_image_info_df(), by = c("file" = "filepath")) %>%
        dplyr::group_by(deployment) %>%
        dplyr::mutate(event = cumsum(zoo::rollapply(as.numeric(datetimeoriginal), 2, function(x) (x[2]-x[1])/60 > input$event_definition_minutes, fill = FALSE, align = "right"))) %>%
        dplyr::mutate(event_id = paste0(deployment, "_", event)) %>%
        dplyr::ungroup()

      return(md_out_events_df)
    })

    #input <- list(event_definition_minutes = 5)
    md_out_table_df <- shiny::reactive({
      md_out_table_df <-
        md_out_events_df() %>%
        dplyr::select(file, conf, category_word, event_id) %>%
        dplyr::group_by(file) %>%
        dplyr::summarise(event_id = event_id[1], max_conf = max(conf), n_tot = dplyr::n()/length(unique(file)), n_th = sum(conf >= input$bbox_threshold)/length(unique(file)), categories = paste(unique(category_word), collapse = ", ")) %>%
        dplyr::mutate(file_link = ToLink(basename(file), file), .before = file)
    })

    output$md_out_table <- DT::renderDataTable({
      #input <- list(bbox_threshold = 0.6)
      DT_out <- DT::datatable(md_out_table_df(),
                              rownames = F,
                              options = list(autowidth = TRUE, columnDefs = list(list(width = '100px', targets = "_all"))),
                              style = "bootstrap",
                              callback = DT::JS("table.on('mouseenter', 'td', function() {
                                                 var td = $(this);
                                                 var info_out = table.cell( this ).data();
                                                 Shiny.onInputChange('clickIndexJS', info_out);
                                              });"),
                              escape = FALSE
      )
      return(DT_out)
    })

    hover_path <- shiny::reactiveVal(0)

    shiny::observe({
      if(file.exists(as.character(input$clickIndexJS))){
        hover_path(input$clickIndexJS)
      }
    }) %>%
      shiny::bindEvent(input$clickIndexJS) %>%
      shiny::debounce(100)

    output$img_hover <- shiny::renderImage({

      if(hover_path() == 0){
        tmp_info <- tempfile(fileext = "jpg")
        file.copy("www/info.jpg", tmp_info)
        list(src = normalizePath(tmp_info))
      }else{
        md_img_info <-
          md_out_bbox() %>%
          dplyr::filter(file == hover_path())

        no_detections_above_threshold <- nrow(md_img_info) == 0

        if(no_detections_above_threshold) md_img_info <- data.frame(file = hover_path())

        out.file <- tempfile("bbox", fileext = ".png")

        bbox_md_imgs(md_img_info, out.file = out.file, scale = "x400", safe.mode = input$safe_mode, skip = no_detections_above_threshold | !input$bbox_preview)
      }


    }, deleteFile = TRUE)

    output$md_out_figure <- ggiraph::renderGirafe({
      (md_out() %>%
         dplyr::filter(category > 0) %>%
         dplyr::group_by(category_word) %>%
         dplyr::mutate(count = dplyr::n()) %>%
         dplyr::ungroup() %>%
         ggplot2::ggplot(ggplot2::aes(x = conf, color = category_word, fill = category_word))+
         ggplot2::scale_color_manual("Category", values = c("coral", "navyblue", "wheat"))+
         ggplot2::scale_fill_manual("Category", values = c("coral", "navyblue", "wheat"))+
         ggplot2::theme_minimal()+
         ggplot2::labs(x = "MD-Confidence", y = "Anzahl")+
         ggplot2::scale_y_log10()+
         ggiraph::geom_histogram_interactive(ggplot2::aes(tooltip = paste0(category_word, " (n = ", count, ")"), data_id = category_word))) %>%
        ggiraph::girafe(code = NULL) %>%
        ggiraph::girafe_options(ggiraph::opts_hover(css = "opacity:1;"),
                                ggiraph::opts_hover_inv(css = "opacity:0.2;"))
    })



    ######################################
    ##### PANEL 3 ACCEPT MD CLASSES  #####
    ######################################

    # input <- list(person_threshold = 0.6, vehicle_threshold = 0.6, animal_threshold = 0.6)

    # assign categories by applying thresholds
    md_out_threshold <- shiny::reactive({
      md_obs_cats <- c("md_animal", "md_person", "md_vehicle")


      md_out_threshold <-
        md_out() %>%
        dplyr::mutate(category_word = ifelse(
          (category_word == "md_vehicle" & conf > input$vehicle_threshold) |
            (category_word == "md_animal" & conf > input$animal_threshold) |
            (category_word == "md_person" & conf > input$person_threshold),
          category_word,
          "md_empty")) %>%
        dplyr::left_join(md_out_table_df() %>%
                           dplyr::select(file, event_id)) %>%
        dplyr::group_by(event_id) %>%
        dplyr::mutate(dir_category = ifelse(
          length(unique(category_word)) == 1,
          category_word,
          ifelse(
            sum(unique(category_word) %in% md_obs_cats) > 1,
            "md_mixed",
            md_obs_cats[md_obs_cats %in% unique(category_word)]))) %>%
        dplyr::ungroup()

    })

    output$md_threshold_figure <- ggiraph::renderGirafe({
      (md_out_threshold()  %>%
         dplyr::filter(!duplicated(file)) %>%
         dplyr::group_by(dir_category) %>%
         dplyr::summarise(count = 100 * dplyr::n()/sum(classified_images())) %>%
         ggplot2::ggplot(ggplot2::aes(x = dir_category, y = count, fill = dir_category))+
         ggiraph::geom_col_interactive(ggplot2::aes(tooltip = paste0(round(count), "% aller Bilder"), data_id = dir_category))+
         ggplot2::scale_fill_manual(values = c("coral", "gray", "navyblue", "wheat", "deepskyblue4"))+
         ggplot2::theme_classic()+
         ggplot2::theme(legend.position = "none")+
         ggplot2::labs(x = "Kategorie", y = "Anteil nach Grenzwertfestlegung")) %>%
        ggiraph::girafe(code = NULL)
    })

    ### move images to respective new files while maintaining the folder structure
    # assign readable category to images table
    # mixed categories will be treated as person, new folders will be generated for empty images

    shiny::observeEvent(input$accept, {

      if(input$process_empty_img_mode == "upload"){

        md_obs_cats <- c("md_animal", "md_person", "md_vehicle")

        md_out_upload <-
          md_out_events_df() %>%
          dplyr::mutate(category_word = ifelse(
            (category_word == "md_vehicle" & conf > input$vehicle_threshold) |
              (category_word == "md_animal" & conf > input$animal_threshold) |
              (category_word == "md_person" & conf > input$person_threshold),
            category_word,
            "md_empty")) %>%
          dplyr::group_by(file) %>%
          dplyr::mutate(empty_box = dplyr::n() == 1 & category_word == "md_empty") %>%
          dplyr::ungroup() %>%
          dplyr::mutate(conf = ifelse(empty_box, 0 , conf),
                        category = ifelse(empty_box, 0 , category),
                        x_off = ifelse(empty_box, NA , x_off),
                        y_off = ifelse(empty_box, NA , y_off),
                        width = ifelse(empty_box, NA , width),
                        height = ifelse(empty_box, NA , height)
          ) %>%
          dplyr::filter(!(!is.na(x_off) & category_word == "md_empty")) %>%
          dplyr::group_by(event_id) %>%
          dplyr::mutate(event_category = ifelse(
            length(unique(category_word)) == 1,
            category_word,
            ifelse(
              sum(unique(category_word) %in% md_obs_cats) > 1,
              "md_mixed",
              md_obs_cats[md_obs_cats %in% unique(category_word)]))) %>%
          dplyr::ungroup() %>%
          dplyr::filter(event_category != "md_empty")

        readr::write_csv(md_out_upload, "md_out_upload.csv")

        #DMCrAI
        upload_md_data(md_out_upload)

        screened_images <- md_out_threshold() %>%
          dplyr::mutate(new_file_alternativ = file)


      }else{
        if(input$process_empty_img_mode == "delete"){
          print("Updating JSON")

          files2remove <- md_out_threshold() %>%
            dplyr::filter(dir_category == "md_empty") %>%
            dplyr::select(file) %>%
            purrr::as_vector()

          # do not remove files that have been moved to md_empty folder and/or have been renamed to md_empty..
          files2remove <- files2remove[basename(dirname(files2remove)) != "md_empty"]
          files2remove <- files2remove[!stringr::str_detect(basename(files2remove), pattern = "^md_empty")]

          file.remove(files2remove)

          rjs <- rjson::fromJSON(file = paste0(md_dir(), "\\md_out.json"))
          rjs_files <- lapply(rjs$images, function(x) x$file) %>%
            unlist()


          rjs$images <- rjs$images[!rjs_files %in% files2remove]

          write(rjson::toJSON(rjs),  paste0(md_dir(), "\\md_out.json"))

        }else{
          #TODO add option to resize empty images?!
          if(input$process_empty_img_mode == "move"){
            print("Moving files")

            screened_images <-
              md_out_threshold() %>%
              dplyr::mutate(new_file_alternativ = ifelse(dir_category == "md_empty" & basename(dirname(file)) != "md_empty",
                                                         paste(dirname(file), "md_empty", basename(file), sep = .Platform$file.sep),
                                                         file))

            screened_images_no_dupl <- screened_images %>%
              dplyr::filter(!duplicated(file))

            if(any(screened_images_no_dupl$new_file_alternativ != screened_images_no_dupl$file)){
              #DMCrAI function
              moved_all_files <- move_md_img2folder(screened_images = screened_images_no_dupl %>%
                                                              dplyr::filter(file != new_file_alternativ),
                                                            reverse = FALSE,
                                                            original_file_col = "file",
                                                            md_file_col = "new_file_alternativ")
            }
          }else{
            if(input$process_empty_img_mode == "rename"){

              screened_images <-
                md_out_threshold() %>%
                dplyr::mutate(new_file_alternativ = ifelse(dir_category %in% c("md_person", "md_vehicle", "md_mixed", "md_empty") & !stringr::str_detect(basename(file), pattern = "^z_md_person|^z_md_vehicle|^z_md_mixed|^z_md_empty"),
                                                           stringi::stri_replace_last_regex(file, basename(file), paste0("z_", dir_category, "_", basename(file))),
                                                           file))


              screened_images_no_dupl <- screened_images %>%
                dplyr::filter(!duplicated(file))

              file.rename(screened_images_no_dupl$file, screened_images_no_dupl$new_file_alternativ)
            }else{




              # remove empty images not part of an event
              files2remove_a <- md_out_threshold() %>%
                dplyr::filter(dir_category == "md_empty") %>%
                dplyr::select(file) %>%
                purrr::as_vector()

              # only keep images with the most bounding boxes in md_person, md_mixed and/or md_vehicle
              files2remove_b <- md_out_threshold() %>%
                dplyr::filter(!dir_category %in% c("md_animal", "md_empty")) %>%
                dplyr::group_by(file) %>%
                dplyr::summarise(n_bbox = sum(category_word != "md_empty"), event_id = event_id[1]) %>%
                dplyr::group_by(event_id) %>%
                dplyr::filter(n_bbox < max(n_bbox))%>%
                dplyr::ungroup() %>%
                dplyr::select(file) %>%
                purrr::as_vector()


              files2remove <- unique(c(files2remove_a, files2remove_b))
              # do not remove files that have been moved to md_empty folder and/or have been renamed to md_empty..
              files2remove <- files2remove[basename(dirname(files2remove)) != "md_empty"]
              files2remove <- files2remove[!stringr::str_detect(basename(files2remove), pattern = "^md_empty")]

              file.remove(files2remove)

              rjs <- rjson::fromJSON(file = paste0(md_dir(), "\\md_out.json"))
              rjs_files <- lapply(rjs$images, function(x) x$file) %>%
                unlist()


              rjs$images <- rjs$images[!rjs_files %in% files2remove]

              write(rjson::toJSON(rjs),  paste0(md_dir(), "\\md_out.json"))

              print("Renaming files")

              screened_images <-
                md_out_threshold() %>%
                dplyr::filter(!file %in% files2remove) %>%
                dplyr::mutate(new_file_alternativ = ifelse(dir_category %in% c("md_person", "md_vehicle", "md_mixed") & !stringr::str_detect(basename(file), pattern = "^md_person|^md_vehicle|^md_mixed"),
                                                           stringi::stri_replace_last_regex(file, basename(file), paste0(dir_category, "_", basename(file))),
                                                           file))

              screened_images_no_dupl <- screened_images %>%
                dplyr::filter(!duplicated(file))

              file.rename(screened_images_no_dupl$file, screened_images_no_dupl$new_file_alternativ)
            }}}


        print("Updating JSON")

        rjs <- rjson::fromJSON(file = paste0(md_dir(), "\\md_out.json"))

        shiny::withProgress(message = "Update md_out.json", detail = "Updating path of files with images classified as empty", value = 0, {
          for(img in seq(nrow(screened_images_no_dupl))){

            si_paths <- screened_images_no_dupl[img, c("file", "new_file_alternativ")] %>% unlist()

            md_out_path <- rjs$images[[img]]$file

            shiny::incProgress(1/length(rjs$images))
            if(!md_out_path %>% normalizePath(winslash = "\\") %in% si_paths){
              stop("Error, files do not match!")
            }
            rjs$images[[img]]$file <-  screened_images_no_dupl$new_file_alternativ[img]
          }
        })

        write(rjson::toJSON(rjs),  paste0(md_dir(), "\\md_out.json"))
      }




      print("Blurring")
      if(input$blur & any(screened_images$category_word %in%  c("md_person", "md_vehicle"))){
        imgs_2blur_list <- screened_images %>%
          dplyr::filter(category_word %in% c("md_person", "md_vehicle")) %>%
          dplyr::mutate(file = new_file_alternativ) %>%
          dplyr::group_by(file) %>%
          dplyr::group_split()

        #TODO - how to skip if already blurred?! similar to handling of files2remove!
        shiny::withProgress(message = "Blurring images with personal data...", detail = "Vehicles and persons are blurred!", value = 0, {
          for(img in seq(length(imgs_2blur_list))){
            shiny::incProgress(1/length(imgs_2blur_list))
            print(Sys.time())
            print(imgs_2blur_list[[img]])
            # add option to resize person and vehicle data? DMCrAI
            blur_md_imgs(imgs_2blur_list[[img]], sigma = input$blur_sigma, radius = input$blur_radius)
          }
        })
      }


      shiny::updateActionButton(session = session, inputId = "accept", "Klassifikation wurde übernommen (aktualisieren, um Änderungen in json einzulesen)")

      #refresh_nr(refresh_nr() + 1)

      print("Megadetector output processing complete - now continue working with FFM2 on camera trap images")
    })
  }
