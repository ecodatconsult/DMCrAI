#' @export
#'
blur_md_imgs <- function(md_img_info, img.in.file = md_img_info$file, img.out.file = md_img_info$file, radius = 30, sigma = 15){
    imgs <- magick::image_read(img.in.file[1])

    xy <- magick::image_info(imgs)[, c("width", "height")]

    for(bbox in seq(nrow(md_img_info))){
      geometry_crop <-  ((md_img_info[bbox,c("x_off", "y_off", "width", "height")] * as.numeric(c(xy[1], xy[2], xy[1], xy[2]))))

      geometry <-
        with(geometry_crop,{
          magick::geometry_area(x_off = x_off, y_off = y_off, width = width, height = height)
        })

      bbox_image <- magick::image_crop(imgs, geometry)

      bbox_image_blur <- magick::image_blur(bbox_image, radius = radius, sigma = sigma)


      imgs <- magick::image_composite(imgs, bbox_image_blur, offset = geometry)
    }

    magick::image_write(imgs, img.out.file[1])
    print(paste0("blurred bounding boxes of ", img.in.file[1], " and wrote it to ", img.out.file[1]))
    return(TRUE)
}
