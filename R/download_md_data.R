#' Download Megadetectorergebnisse
#' Lade die Ergebnisse der Megadetector-Klassifikation aus der Datenbank
#'
#'
#' @param directories character vector, Pfade zu den gewählten directories, die klassifiziert werden
#' @param include.classified boolean, gibt an ob auch bereits klassifizierte Bounding Boxes geladen werden sollen
#'
#' @export

download_md_data <- function(directories, include.classified = FALSE){
  con <- DMCr2::dbConnection()

  if(RPostgreSQL::dbExistsTable(con,  c("megadetector", "directories"))){
    megadetector_directories <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT * FROM megadetector.directories where directory in ({directories*})", directories = directories, .con = con))
  }else{
    stop("megadetector.directories table does not exist but is required!")
  }


  if(RPostgreSQL::dbExistsTable(con,  c("megadetector", "classifications"))){
    megadetector_classifications <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT * FROM megadetector.classifications where directory_id in ({directories*})", directories = megadetector_directories$directory_id, .con = con))
  }else{
    stop("megadetector.classifications table does not exist but is required!")
  }

  if(include.classified){
    if(RPostgreSQL::dbExistsTable(con,  c("megadetector", "manually_tagged"))){
      megadetector_manually_tagged <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT * FROM megadetector.manually_tagged where directory_id in ({directories*})", directories = megadetector_directories$directory_id, .con = con))

      if(nrow(megadetector_manually_tagged) > 0){
        megadetector_classifications <-
          megadetector_classifications %>%
          dplyr::filter(!obs_id %in% megadetector_manually_tagged$obs_id) %>%
          dplyr::bind_rows(megadetector_manually_tagged) %>%
          dplyr::arrange(event_id, obs_id) %>%
          mutate(file = basename(file))
      }
    }
  }else{
    if(RPostgreSQL::dbExistsTable(con,  c("megadetector", "manually_tagged"))){
      megadetector_manually_tagged <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT obs_id FROM megadetector.manually_tagged where directory_id in ({directories*})", directories = megadetector_directories$directory_id, .con = con))

      megadetector_classifications <-
        megadetector_classifications %>%
        dplyr::filter(!obs_id %in% megadetector_manually_tagged$obs_id)
    }
  }

  RPostgreSQL::dbDisconnect(con)

  return(megadetector_classifications)
}
