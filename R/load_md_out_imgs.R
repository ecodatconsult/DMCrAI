#' Lade JSON Datei
#'
#' @param img file.path, Pfad zu der mittels Megadetector erzeugten JSON-Datei (z.B. "md_out.json")
#'
#' @returns list, enthält die Images-Attribute der Megadetector-JSON Datei
#'
#' @export
#'
load_md_out_imgs <- function(file.path){

  md_out <- rjson::fromJSON(file = file.path)

  return(md_out$images)
}
