#' Führt automatische Korrektur von Spaltenbezeichnungen der Standorte durch
#'
#' @param standorte_import_new_sf simple feature (sf), Standorte ohne Korrektur der Spaltenbezeichnungen
#'
#' @return simple feature (sf), Standorte mit automatisch korrigierten Spaltenbezeichnungen
#' @export

checkStandorteNamesWrong <- function(standorte_import_new_sf){

  #read column names from database via INFORMATION_SCHEMA
  namesDB <- getInfoDB(type = "column_name",
                        schema = "fotofallen",
                        table = "fotofallen_standorte_import")

  if(ncol(standorte_import_new_sf) >= length(namesDB)){
    #comment Alex: these common wrong names most certainly result from exporting geodata as shp
    wr_co <- data.frame(wrong = c("standort_i", "ts_kamera_", "ts_kamera_.1", "ts_kamer_1" ,"kamera_aus","kamera_hoe","sichtfeld_","standortfo", "aufgehaeng", "bemerkunge", "bilderordn", "standort_1", "sichtfel_1", "geometry"),
                        correct = c("standort_id","ts_kamera_start", "ts_kamera_ende", "ts_kamera_ende", "kamera_ausrichtung", "kamera_hoehe", "sichtfeld_hoehe", "standortfotos", "aufgehaengt_von", "bemerkungen", "bilderordner", "standort_id_org", "sichtfeld_entfernung", "geom"))

    namesSF <- names(standorte_import_new_sf)

    #correct common wrong names
    namesSF[na.omit(match(wr_co$wrong, namesSF))] <-  wr_co$correct[which(wr_co$wrong %in% namesSF)]

    names(standorte_import_new_sf) <- namesSF
    sf::st_geometry(standorte_import_new_sf) <- "geom"

    #wrong names
    falsche_names <- namesSF[!is.element(namesSF,namesDB)]
    #TODO: indicate missing names in ShinyApp
    missing_names <- namesDB[!is.element(namesDB, namesSF)]

    wrong_names_df <- dplyr::tibble("alt" = falsche_names,
                  korrigiert = "")

    if(length(missing_names) > 0){
      wrong_names_df <- wrong_names_df %>%
        dplyr::mutate(vorschlag = sapply(alt, function(wrong_term){
                                  missing_names[agrep(pattern = wrong_term, x = missing_names)][1]
                                }))
    }


    return(list(wrong_names_df = wrong_names_df,
                missing_names = missing_names,
                standorte_import_new_sf_names_corr = standorte_import_new_sf))
  }else{
    print(paste0("Imported locations have only ", ncol(standorte_import_new_sf), " columns while", length(namesDB), " are required!"))
    return(NULL)
  }
}
