#' Upload Megadetectorergebnisse
#' Lade die Ergebnisse der manuellen Klassifikation in die Datenbank hoch
#'
#'
#' @param md_classified_df data.frame, Datentabelle die mit ShinyFFM2 generiert wird
#' @param include.classified boolean, gibt an ob auch bereits klassifizierte Daten hochgeladen werden
#'
#' @export

upload_md_classified_data <- function(md_classified_df, include.classified){

  con <- DMCr2::dbConnection()


    if(RPostgreSQL::dbExistsTable(con,  c("megadetector", "manually_tagged"))){
      megadetector_manually_tagged <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT obs_id FROM megadetector.manually_tagged where directory_id in ({directories*})", directories = unique(md_classified_df$directory_id), .con = con))
      if(!include.classified){


      md_classified_df <- md_classified_df %>%
        filter(!obs_id %in% megadetector_manually_tagged$obs_id)
      }else{
        dupl_obs_id <- md_classified_df$obs_id[md_classified_df$obs_id %in% megadetector_manually_tagged$obs_id]

        RPostgreSQL::dbGetQuery(con, glue::glue_sql("DELETE FROM megadetector.manually_tagged where obs_id in ({dupl_obs_id*})", dupl_obs_id = dupl_obs_id, .con = con))
      }
  }

  if(nrow(md_classified_df) > 0){
    RPostgreSQL::dbWriteTable(con, c("megadetector", "manually_tagged"), md_classified_df, append = TRUE, overwrite = FALSE, row.names = FALSE)
    msg <- ("Uploaded new manual classifications!")
  }else{
    msg <- ("No new manual classifications to upload!")
  }

  RPostgreSQL::dbDisconnect(con)
  return(msg)
}
