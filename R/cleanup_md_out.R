#' Entfernt nicht mehr existente Bilder aus mit Megadetector erzeugter JSON
#'
#' @param file Pfad zur mit Megadetector erzeugten JSON-Datei
#'
#'
#' @export
#'

cleanup_md_out <- function(file){
  md_out <- rjson::fromJSON(file = file)

  files_exist <-
  md_out$images  %>%
    lapply(function(x) file.exists(x$file)) %>%
    unlist()

  md_out$images <- md_out$images[files_exist]

  rjson::toJSON(md_out) %>%
    write(file = file)
}
