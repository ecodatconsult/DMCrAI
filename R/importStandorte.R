#' Ruft Fotofallenstandorte aus der Datenbank ab und lädt zugleich neue Standorte in die akutelle Session ein
#'
#' @param file_path character, Pfad zu den lokalen Geodaten
#' @param project character, Projektbezeichnung der importierten Standorte
#' @param target_crs numeric, EPSG-Nummer der Koordinatenreferenzsystems (CRS)
#'
#' @return list, Liste mit neu importierte Standorten, Standorten mit der gegebenen Projektbezeichnung in der Datenbank (fotofallen.fotofallen_standorte) und alle Standorten im Importbereich der Datenbank (import.fotofallen_standorte)
#' @export
#'
#'

importStandorte <- function(file_path, project, target_crs = 25832){

  con <- dbConnection()
  ## Database
    # get project id for selected project name
    project_id <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT projekt_id FROM fotofallen.projekte WHERE projekt_name = '", stringi::stri_enc_toutf8(project), "'")) %>%
      purrr::as_vector()

    # sf object or data frame fotofallen_standorte, surpress warnings in case there are no entries with the given project
    standorte_DB <- suppressWarnings(sf::st_read(con, query = glue::glue_sql("SELECT * FROM fotofallen.fotofallen_standorte WHERE projekt_id = '", stringi::stri_enc_toutf8(project_id), "';")))

    if(nrow(standorte_DB) > 0) standorte_DB$projekt_name <- project

    # TODO: check if select * is needed
    standorte_import_DB_sf <- sf::st_read(con, query = "SELECT * FROM import.fotofallen_standorte;")


    standorte_import_new_sf <- sf::st_read(file_path) %>%
      sf::st_transform(as.numeric(target_crs))

    RPostgreSQL::dbDisconnect(con)

    return(list(standorte_import_new_sf = standorte_import_new_sf,
              standorte_import_DB_sf = standorte_import_DB_sf,
              standorte_DB = standorte_DB
              ))
}
