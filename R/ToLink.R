#' @export

ToLink <- function(txt,link) {
  paste0('<a href=file:///',link,">",txt,'</a>')
}
