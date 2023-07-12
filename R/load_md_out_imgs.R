load_md_out_imgs <- function(file.path){
  md_out <- rjson::fromJSON(file = file.path)

  md_out$images <-
    lapply(md_out$images, function(x){
      x$file <- stringr::str_replace(x$file, "/images", dirname(file.path))
      return(x)
    })

  return(md_out$images)
}
