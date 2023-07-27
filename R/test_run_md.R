# Testlauf zur Überprüfung der Funktionalität von Megadetector

#' @export

test_run_md <- function(){

  if(system.file("md.csv", package = "DMCrAI") != ""){
    test <- read.csv(system.file("md.csv", package = "DMCrAI"))
    test$pics_dir <- system.file("test_data/wvb_ff_5034_220809", package = "DMCrAI")

    with(test, {
      create_md_bat(pics_dir = pics_dir, md_out = pics_dir, py_scripts_loc = py_scripts_loc, md_model_loc = md_model_loc, force.overwrite = TRUE, bat_loc = "test.bat", run_info = TRUE, checkpoint_freq = -1)
    })

    system("test.bat")

    info <- as.numeric(strsplit(tail(readLines("run_info.txt"), 3)[1], " ")[[1]][c(4,7)])

    write.csv(data.frame(performance = info[1]/(info[2] - 4.6)), paste0(system.file(package = "DMCrAI"), "/performance.csv"))

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
    file.remove("test.bat")
    file.remove("run_info.txt")
  }else{
    detection_finished <- FALSE
    message("Run DMCrAI::setup_md() first to specify location of megadetector model and python scripts!")
  }

  return(detection_finished)
}
