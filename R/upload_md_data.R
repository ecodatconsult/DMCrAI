#' Upload Megadetectorergebnisse
#' Lade die Ergebnisse der Megadetector-Klassifikation in die Datenbank
#'
#'
#' @param md_out_events data.frame, Datentabelle mit den Attributen c("file", "conf", "category", "x_off", "y_off", "width", "height", "category_word", "datetimeoriginal", "deployment", "event", "event_id"), die durch die ShinyApp DMCrAI generiert werden
#'
#' @export


upload_md_data <- function(md_out_events){
  req_names <- c("project_id", "deployment", "file", "conf", "category", "x_off", "y_off", "width", "height", "bbox_category","image_category",  "datetimeoriginal")

  if(!all(req_names %in% names(md_out_events))) stop(paste0("missing names in md_out_events (", paste(req_names[!req_names %in% names(md_out_events)], collapse = ", "),  ")!"))


  con <- dbConnection()

  new_classifications <-  md_out_events %>%
    dplyr::select(dplyr::all_of(req_names)) %>%
    dplyr::group_by(file) %>%
    dplyr::mutate(file_id = digest::digest(file, "md5"), .before = "file") %>%
    dokyr::ungroup()
  megadetector_classifications_max <- 0

  if(RPostgreSQL::dbExistsTable(con,  c("import", "megadetector"))){
    megadetector_classifications <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT deployment, file_id FROM import.megadetector where deployment in ({deployments*})", deployments = unique(new_classifications$deployment), .con = con))
    megadetector_classifications_max <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT max(obs_id) as max FROM import.megadetector", .con = con))$max

    new_classifications <-
      new_classifications %>%
      dplyr::filter(!file_id %in% megadetector_classifications$file_id)
  }

  if(nrow(new_classifications) > 0){
    new_classifications <-
    new_classifications %>%
      dplyr::mutate(obs_id = seq(dplyr::n()) + megadetector_classifications_max, .before = "deployment") # obs_id tries to track the total amount of processed images (eventually it may reset if import.megadetector is empty)

    RPostgreSQL::dbWriteTable(con, c("import", "megadetector"),
                              new_classifications,
                              append = TRUE,
                              overwrite = FALSE,
                              row.names = FALSE)

    msg <- paste0("Uploaded ", nrow(new_classifications), " new classifications!")
    uploaded_data <- new_classifications
  }else{
    msg <- "No new classifications to upload!"
    uploaded_data <- NULL
  }

  RPostgreSQL::dbDisconnect(con)

  return(list(msg = msg, uploaded_data = uploaded_data))
}

