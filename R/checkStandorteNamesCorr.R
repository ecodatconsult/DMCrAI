#' Führt manuelle Korrektur von Spaltenbezeichnungen der Standorte durch
#'
#' @param standorte_import_new_names_corr_auto_sf simple feature (sf), Standorte mit automatisch korrigierten Spaltenbezeichnungen
#' @param standort_names_corr_manual_df data.frame, Tabelle mit alten und korrigierten Spaltenbezeichnungen
#'
#' @return simple feature (sf), Standorte mit automatisch und manuell korrigierten Spaltenbezeichnungen
#' @export

checkStandorteNamesCorr <- function(standorte_import_new_names_corr_auto_sf, standorte_names_corr_manual_df){

  namesSF <- names(standorte_import_new_names_corr_auto_sf)

  namesSF[na.omit(match(standorte_names_corr_manual_df$alt, namesSF))] <-  standorte_names_corr_manual_df$korrigiert[which(standorte_names_corr_manual_df$alt %in% namesSF)]


  names(standorte_import_new_names_corr_auto_sf) <- namesSF
  sf::st_geometry(standorte_import_new_names_corr_auto_sf) <- "geom"

  namesDB <- getInfoDB(type = "column_name",
                       schema = "fotofallen",
                       table = "fotofallen_standorte_import")

  if(all(namesDB %in% names(standorte_import_new_names_corr_auto_sf))){
    print("Alle erforderlichen Spalten vorhanden")

    return(
      standorte_import_new_names_corr_auto_sf %>%
        dplyr::select((namesDB %>% as.vector()))
    )
  }else{
    print("Nicht alle erforderlichen Spalten vorhanden. Überprüfe Datensatz und Korrektur")
    return(NULL)
  }
}
