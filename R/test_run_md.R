# Testlauf zur Überprüfung der Funktionalität von Megadetector

#' @export

test_run_md <- function(){

  if(system.file("md.csv", package = "DMCrAI") != ""){
    test <- read.csv(system.file("md.csv", package = "DMCrAI"))
    test$pics_dir <- system.file("test_data/wvb_ff_5034_220809", package = "DMCrAI")

    bat_loc <- switch(Sys.info()[['sysname']],
      Linux = here::here("test.sh"),
      Windows = here::here("test.bat"),
      Darwin = here::here("test.sh"))

    with(test, {
      create_md_bat(pics_dir = pics_dir, md_out = pics_dir, py_scripts_loc = py_scripts_loc, md_model_loc = md_model_loc, force.overwrite = TRUE, bat_loc = bat_loc, run_info = TRUE, checkpoint_freq = -1)
    })

    switch(Sys.info()[['sysname']],
           Linux = system(paste0("bash ", bat_loc)),
           Windows = system(bat_loc),
           Darwin = system(paste0("bash ", bat_loc)))

    detection_finished <- "md_out.json" %in% list.files(test$pics_dir)

    if(detection_finished){
      load_md_out_imgs(list.files(test$pics_dir, pattern = "md_out.json", full.names = TRUE)) %>%
        lapply(extract_md_info) %>%
        do.call(what = rbind) %>%
        dplyr::mutate(conf = as.numeric(conf)) %>%
        dplyr::select(category, conf) %>%
        plot(pch = "_", ylab = "Konfidenz", xlab = "Kategorie")

      message("Megadetector setup successful! :-)")
    }else{
      message(paste0("Megadetector setup failed! :-( Check provided information in ", system.file("md.csv", package = "DMCrAI"), " or run DMCrAI::setup_md() again:"))
    }

    file.remove(list.files(test$pics_dir, pattern = "md_out.json", full.names = TRUE))
    file.remove(bat_loc)
  }else{
    detection_finished <- FALSE
    message("Run DMCrAI::setup_md() first to specify location of megadetector model and python scripts!")
  }

  return(detection_finished)
}
