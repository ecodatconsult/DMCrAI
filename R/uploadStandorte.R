#' Standorte der Session werden in die FVA-Fotofallendatenbank (fotofallen.fotofallen_standorte_import) hochgeladen (PostgreSQL)
#'
#' @param upload_sf simple feature Objekt, Fotofallenstandorte mit korrigierten bzw. überprüften Spaltenbezeichnungen
#' @param db_table character, Zieltabelle im Schema fotofallen (entweder "fotofallen_standorte" oder "fotofallen_standorte_import")
#' @return character, Nachricht über den Verlauf des Uploads
#' @export
#'

uploadStandorte <- function(upload_sf, db_table = "fotofallen_standorte"){

  con <- dbConnection()

  # TODO clarify: can some standort_ids be assigned to multiple project_ids?
  standort_ids_in_DB <- c(
    RPostgreSQL::dbGetQuery(con,"SELECT standort_id FROM fotofallen.fotofallen_standorte;")$standort_id,
    RPostgreSQL::dbGetQuery(con,"SELECT standort_id FROM fotofallen.fotofallen_standorte_import;")$standort_id) %>% unique()

  if(all(upload_sf$standort_id %in% standort_ids_in_DB)){
    out <- "Alle Standorte sind bereits in der Datenbank (fotofallen.fotofallen_standorte oder fotofallen.fotofallen_standorte_import). Es erfolgt kein Upload!"
  }else{
    upload_sf %>%
      dplyr::relocate(getInfoDB(type = "column_name", schema = "fotofallen", table = db_table) %>% as.vector(), names(upload_sf)) %>%
      dplyr::filter(!standort_id %in% standort_ids_in_DB) %>%
      dplyr::mutate(bilderordner = NA,
                    standort_id_org = NA,
                    sichtfeld_entfernung = NA) %>% #TODO: clarify purpose
      sf::st_write(dsn = con,
                   layer = c("fotofallen", db_table),
                   delete_layer = FALSE,
                   append = TRUE)
    out <- "Erfolgreich hochgeladen!"
  }

  message(out)
  RPostgreSQL::dbDisconnect(con)
  return(out)
}
