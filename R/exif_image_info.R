#' Extrahiere Pfad und Erstellungsdatum der Bilder in einem Verzeichnis (rekursiv) mittels exiftool und gib einen data.frame aus
#'
#'
#' @param md_dir character, Verzeichnis in dem rekursiv Bilder gesucht werden
#' @param info_out character, Verzeichnis in dem die Tabelle zwischengespeichert wird
#' @param optional_tags character, Weitere exiftool-Tags die in der Tabelle eingetragen werden sollen (z.B: "-Model" um das Kameramodell zu extrahieren)
#' @examples
#' #Checks all images in the current directory recursively
#' exif_image_info(here::here(), optional_tags = "-Model")
#'
#' @export



exif_image_info <- function(md_dir = here::here(), info_out = tempfile("out", tmpdir = here::here(), fileext = ".txt"), optional_tags = NULL){
  tags <- paste0("-filepath -datetimeoriginal ")

  if(!is.null(optional_tags)) tags <- paste0(tags, optional_tags, " ")

  exif_init <- switch(Sys.info()[['sysname']],
                         Linux =  "exiftool",
                         Windows = paste0('"',system.file("exiftool.exe", package = "DMCrAI"),'"'),
                         Darwin = stop("Darwin OS not implemented yet"))


  cmd <- paste0(exif_init, " -T -r ", tags, '"', md_dir,'"' ," > ", '"', normalizePath(info_out, mustWork = FALSE), '"')

  temp_bat_ext <- switch(Sys.info()[['sysname']],
                    Linux =  ".sh",
                    Windows = ".bat",
                    Darwin = ".sh")

  temp_bat <- tempfile("run_exif", tmpdir = here::here(), fileext = temp_bat_ext)
  write(cmd, temp_bat)


  switch(Sys.info()[['sysname']],
         Linux =  system(paste0("bash ", temp_bat)),
         Windows = shell(temp_bat),
         Darwin = stop("Darwin OS not implemented yet"))

  file.remove(temp_bat)

  img_info <- read.delim(normalizePath(info_out))

  img_info<- img_info[img_info[,2] != "-",]

  names(img_info) <- substr(unlist(strsplit(tags, " ")), 2,35)

  img_info$datetimeoriginal <- lubridate::as_datetime(img_info$datetimeoriginal)
  img_info$deployment <- basename(dirname(img_info$filepath))

  file.remove(info_out)

  return(img_info)
}

