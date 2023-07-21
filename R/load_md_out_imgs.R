#' @export
#'
load_md_out_imgs <- function(file.path){

  md_out <- rjson::fromJSON(file = file.path)

  return(md_out$images)
}
