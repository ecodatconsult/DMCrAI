#' Diese Funktion ermittelt das Hauptverzeichnis mehrerer Pfade und legt dort eine neue Unterordner an. In diesen Ordner wird die Ordnerstruktur des Hauptverzeichnis mit allen Unterordnern (ohne den neuen Unterordner) kopiert.
#'
#' @param md_bbox_info data.frame, Datentabelle mit information über Speicherort des Bildes sowie der Geometrie der Bounding Box
#' @param img.in.file character, aktueller Speicherort des Bildes (Standardwert ist die Angabe in md_img_info)
#' @param img.out.file character, Ausgabepfad des prozessierten Bildes (Standardwert ist die Angabe in md_img_info)
#' @param radius numeric, Radius des Rauschfilters
#' @param sigma numeric, Stärke des Rauschfilters
#'
#' @returns list, enthälte Informationen für die Wiedergabe in R-Shiny (Pfad, Größe)
#' @export
#'

extract_bbox_md_imgs <- function(md_bbox_info, out.file = tempfile("extracted_bbox_img", fileext = ".png"), scale = "x480", safe.mode = TRUE, skip = FALSE){

#
#   md_bbox_info <- DMCrAI::load_md_out_imgs("Y:\\amilles\\megadetector\\test_imgs_small\\md_out.json") %>%
#     lapply(DMCrAI::extract_md_info) %>%
#     do.call(what = rbind) %>%
#     dplyr::arrange(desc(conf))

  # md_bbox_info <- md_bbox_info[1,]

  imgs <- magick::image_read(md_bbox_info$file)

  xy <- magick::image_info(imgs)[, c("width", "height")]

  geometry_crop <- md_bbox_info[, c("x_off", "y_off",
                                         "width", "height")] * as.numeric(c(xy[1], xy[2],
                                                                            xy[1], xy[2]))
  geometry <- with(geometry_crop, {
    magick::geometry_area(x_off = x_off, y_off = y_off,
                          width = width, height = height)})

  imgs <-
  imgs %>%
    magick::image_crop(geometry = geometry) %>%
    magick::image_resize(geometry = scale)

  if(safe.mode & md_bbox_info$category != 1){
    imgs <- imgs %>%
      magick::image_blur(radius = 30, sigma = 15)
  }

  magick::image_write(imgs, out.file)

  xy <- magick::image_info(imgs)[, c("width", "height")]

  return(list(src = normalizePath(out.file),
       width = xy$width,
       height = xy$height))
}
