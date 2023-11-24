#' @export
#'

ffm2_event_images_animate <- function(event_images, ...){

  event_images_animated <-
    event_images %>%
    magick::image_animate(...)

  tmpfile <- tempfile(fileext = ".gif")

  magick::image_write(event_images_animated, tmpfile)

  xy <- magick::image_info(event_images_animated)[, c("width", "height")]

  return(list(src = normalizePath(tmpfile),
              width = xy$width,
              height = xy$height))
}
