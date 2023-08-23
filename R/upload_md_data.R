#' Upload Megadetectorergebnisse
#' Lade die Ergebnisse der Megadetector-Klassifikation in die Datenbank
#'
#'
#' @param md_out_events data.frame, Datentabelle mit den Attributen c("file", "conf", "category", "x_off", "y_off", "width", "height", "category_word", "datetimeoriginal", "deployment", "event", "event_id"), die durch die ShinyApp DMCrAI generiert werden
#'
#' @export


upload_md_data <- function(md_out_events){
  req_names <- c("file", "conf", "category", "x_off", "y_off", "width", "height", "category_word", "datetimeoriginal", "deployment", "event", "event_id")

  if(!all(req_names %in% names(md_out_events))) stop(paste0("missing names in md_out_events (", paste(req_names[!req_names %in% names(md_out_events)], collapse = ", "),  ")!"))


  con <- DMCr2::dbConnection()

  ### upload directories where images are located to prevent overly long paths to images

  # check for table in database if not, create
  if(RPostgreSQL::dbExistsTable(con,  c("megadetector", "directories"))){
    megadetector_directories <- RPostgreSQL::dbGetQuery(con, "SELECT * FROM megadetector.directories", .con = con)
  }else{
    megadetector_directories <- dplyr::tibble(directory_id = NULL, directory = NULL, project = NULL, location = NULL, deployment = NULL)
  }

  # filter new directories
  new_directories <-
  md_out_events %>%
    dplyr::mutate(directory = dirname(file)) %>%
    dplyr::select(directory) %>%
    dplyr::filter(!duplicated(directory) & !directory %in% megadetector_directories$directory) %>%
    dplyr::mutate(project = as.character(NA), location = as.character(NA), deployment = as.character(NA))

  if(nrow(new_directories) > 0){
    new_directories <-
      new_directories %>%
      dplyr::mutate(directory_id = seq(dplyr::n())+nrow(megadetector_directories), .before = directory)

    # append new directories to database
    RPostgreSQL::dbWriteTable(con, c("megadetector", "directories"), new_directories, append = TRUE, overwrite = FALSE, row.names = FALSE)
    print("Uplodad new directories")
  }else{
    print("No new directories!")
  }



  new_classifications <-
  md_out_events %>%
    dplyr::select(dplyr::all_of(req_names)) %>%
    dplyr::mutate(directory = dirname(file)) %>%
    dplyr::left_join(
      dplyr::bind_rows(
      new_directories,
      megadetector_directories
    ) %>%
      dplyr::select(directory_id, directory)
    ) %>%
    dplyr::mutate(file = basename(file)) %>%
    dplyr::relocate(directory_id, .before = file) %>%
    dplyr::select(-directory)

  if(RPostgreSQL::dbExistsTable(con,  c("megadetector", "classifications"))){
    megadetector_classifications <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT file, directory_id FROM megadetector.classifications where directory_id in ({directory_ids*})", directory_ids = unique(new_classifications$directory_id), .con = con))
    megadetector_classifications_n <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT count(obs_id) as count FROM megadetector.classifications", directory_ids = unique(new_classifications$directory_id), .con = con))$count
  }else{
    megadetector_classifications <- dplyr::tibble(directory_id = NULL, file = NULL)
    megadetector_classifications_n <- 0
  }
#
#   megadetector_classifications <- new_classifications[1, c("directory_id", "file")]

  if(megadetector_classifications_n > 0){
    new_classifications <-
      new_classifications %>%
      dplyr::left_join(megadetector_classifications %>%
                         dplyr::mutate(duplicate_found = TRUE),
                       by = c("directory_id", "file"), relationship = "many-to-many") %>%
      dplyr::filter(is.na(duplicate_found)) %>%
      dplyr::select(-duplicate_found)
  }


  if(nrow(new_classifications) > 0){
    new_classifications <-
    new_classifications %>%
      dplyr::mutate(obs_id = paste0(deployment, "_", seq(dplyr::n()) + megadetector_classifications_n), .before = "directory_id") %>%
      dplyr::relocate(event_id, .before = "directory_id") %>%
      dplyr::relocate(file, .before = "event_id")

    RPostgreSQL::dbWriteTable(con, c("megadetector", "classifications"), new_classifications, append = TRUE, overwrite = FALSE, row.names = FALSE)
    print("Uploaded new classifications!")
  }else{
    print("No new classifications to upload!")
  }

  RPostgreSQL::dbDisconnect(con)
}

