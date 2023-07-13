#' @export
#'
extract_md_info <- function(img){
  if(img$max_detection_conf > 0){
    category <- unlist(lapply(img$detections, function(y) y$category))
    conf <- unlist(lapply(img$detections, function(y) y$conf))


    bbox <- do.call(rbind,
                    lapply(img$detections,
                           function(y){
                             t(data.frame(y$bbox))
                           }
                    )
    )
    bbox <- as.data.frame(bbox)


    names(bbox) <- c("x_off", "y_off", "width", "height")

    cbind(
      data.frame(file = img$file,
                 conf =  conf,
                 category = category),
      bbox
    )
  }else{
    data.frame(file = img$file,
               conf = 0, category = 0, x_off = NA, y_off = NA, width = NA, height = NA)
  }
}
