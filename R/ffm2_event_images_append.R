#' @export
#'

ffm2_event_images_append <- function(event_images){
event_images_appended <-
    event_images %>%
    magick::image_append()

tmpfile <- tempfile(fileext = ".png")

magick::image_write(event_images_appended, tmpfile)

xy <- magick::image_info(event_images_appended)[, c("width", "height")]

return(list(src = normalizePath(tmpfile),
            width = xy$width,
            height = xy$height))
}
