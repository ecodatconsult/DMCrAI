#' Ruft Infos aus Information_Schema ab, um bspw. Spaltennamen einer Tabelle aus der Datenbank abzurufen
#'
#' @param type character, bestimmt welche Information abgefragt werden soll
#' @param data character, bestimmt in welchem Schema die Tabelle vorliegt
#' @param table character, bestimmt den Namen der Tabelle
#'
#' @return named character vector
#' @export
#'
#' @examples getInfoDB(type = "column_name", schema = "fotofallen", table = "fotofallen_standorte_import")
#'
getInfoDB <- function(type = "column_name", schema = "fotofallen", table = "fotofallen_standorte_import"){
  con <- dbConnection()

  #read column names from database via INFORMATION_SCHEMA
  namesDB <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT ", type, " FROM INFORMATION_SCHEMA.COLUMNS
                                                  WHERE TABLE_NAME = N'", table, "' AND TABLE_SCHEMA ='" , schema, "';")) %>%
    purrr::as_vector()

  RPostgreSQL::dbDisconnect(con)

  return(namesDB)
}
