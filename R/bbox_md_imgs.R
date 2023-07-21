#' @export
#'
bbox_md_imgs <- function(md_img_info, out.file = tempfile("bbox_img", fileext = ".png"), scale = "x480", safe.mode = TRUE, skip = FALSE){

  imgs <- magick::image_read(md_img_info$file[1]) %>%
    magick::image_resize(geometry = scale)

  xy <- magick::image_info(imgs)[, c("width", "height")]

  if(!skip){
    for (bbox in seq(nrow(md_img_info))) {
      geometry_crop <- ((md_img_info[bbox, c("x_off", "y_off",
                                             "width", "height")] * as.numeric(c(xy[1], xy[2],
                                                                                xy[1], xy[2]))))
      geometry <- with(geometry_crop, {
        magick::geometry_area(x_off = x_off, y_off = y_off,
                              width = width, height = height)})

      bbox_image <- magick::image_crop(imgs, geometry)

      # blur sensible data on the fly with safe.mode enabled
      if(safe.mode & md_img_info$category_word[bbox] %in% c("md_person", "md_vehicle")){
        bbox_image <- bbox_image %>% magick::image_blur(radius = 30, sigma = 15)
      }

      bbox_col <- c("coral", "navyblue", "wheat")[md_img_info$category[bbox]]
      text_col <- c("black", "white", "black")[md_img_info$category[bbox]]

      bbox_image_border <- magick::image_border(bbox_image, color = bbox_col, geometry = "4x4") %>%
        magick::image_annotate(text = md_img_info$conf[bbox], size = 20, color = text_col, boxcolor = bbox_col)

      imgs <- magick::image_composite(imgs, bbox_image_border,
                                      offset = geometry)
    }
  }


  magick::image_write(imgs, out.file)

  return(list(src = normalizePath(out.file),
       width = xy$width,
       height = xy$height))
}