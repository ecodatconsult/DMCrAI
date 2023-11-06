#' Verschiebt Dateien zwischen Verzeichnissen
#'
#' @param screened_images data.frame, muss Pfade mit Start und Zielverzeichnis enthalten
#' @param reverse boolean, in umgekehrter Richtung verschieben (nur zu Testzwecken)
#' @param original_file_col character, Pfad der Datei
#' @param md_file_col character, Zielpfad
#'
#' @returns boolean
#' @export
#'
#'
move_md_img2folder <- function(screened_images, reverse, original_file_col, md_file_col){
  files2move <-
    screened_images %>%
    dplyr::select(dplyr::all_of(c(original_file_col, md_file_col))) %>%
    setNames(c("original_file", "md_file"))

  for(files in seq(nrow(files2move))){
    if(reverse){
      filesstrings::file.move(files2move$md_file[files], dirname(files2move$original_file[files]))
    }else{
      filesstrings::file.move(files2move$original_file[files], dirname(files2move$md_file[files]))
    }
  }

  if(reverse){
    return(all(file.exists(files2move$original_file)) & !any(file.exists(files2move$md_file)))
  }else{
    return(all(file.exists(files2move$md_file)) & !any(file.exists(files2move$original_file)))
  }
}
