#' Setze die Pfade für die Ausführung von Megadetector
#'
#' @param test_md boolean, gibt an, ob nach dem Setzen der Pfade ein Testlauf durchgeführt werden soll
#' @param md_model_loc, character, Verzeichnis in dem Megadetector-Modell liegt
#' @param py_scripts_loc character, Verzeichnis in dem die für Megadetector Python-Skripte hinterlegt sind
#' #'
#' @export

setup_md <- function(test_md = TRUE,
                     md_model_loc = NULL,
                     py_scripts_loc = NULL){

  if(is.null(md_model_loc)){
    md_model_loc = switch(Sys.info()[['sysname']],
                          Linux = tcltk::tk_choose.files(caption =  "select MD-model file, e.g. md_v5a.0.0.pt", multi = FALSE),
                          Windows = utils::choose.files(caption = "select MD-model file, e.g. md_v5a.0.0.pt", multi = FALSE),
                          Darwin =  tcltk::tk_choose.files(caption =  "select MD-model file, e.g. md_v5a.0.0.pt", multi = FALSE))
  }

  if(is.null(py_scripts_loc)){
    py_scripts_loc = switch(Sys.info()[['sysname']],
                          Linux = tcltk::tk_choose.dir(caption =  "select directory where ai4eutils, cameratraps and yolov5 are located - path may not contain blanks!"),
                          Windows = utils::choose.dir(caption = "select directory where ai4eutils, cameratraps and yolov5 are located - path may not contain blanks!"),
                          Darwin =  tcltk::tk_choose.dir(caption =  "select directory where ai4eutils, cameratraps and yolov5 are located - path may not contain blanks!"))
  }

  if(stringr::str_detect(py_scripts_loc, " ")){
    message("Path to Python scripts contains blanks, this may result in errors!")
  }

  write.csv(data.frame(md_model_loc = md_model_loc,
                         py_scripts_loc = py_scripts_loc),
              paste0(system.file("", package = "DMCrAI"), "/md.csv"), row.names = FALSE)

  if(test_md) test_run_md()

}
