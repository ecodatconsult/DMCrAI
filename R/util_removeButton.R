util_removeButton <- function(n, idS = "", lab = "Pit") {
  if (stringr::str_length(idS) > 0) idS <- paste0(idS, "-")
  ret <- shinyInput(shiny::actionButton,
                           n,
                           'button_',
                           label = "Remove",
                           onclick = sprintf('Shiny.onInputChange(\"%sremove_button_%s\",  this.id)' ,idS, lab))
  return (ret)
}

shinyInput <- function(FUN, n, id, ses, ...) {
  as.character(FUN(paste0(id, n), ...))
}
