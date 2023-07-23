#' @export

create_md_bat <- function(pics_dir, md_out = pics_dir, py_scripts_loc = NULL, md_model_loc = NULL, force.overwrite = FALSE, bat_loc = NULL, run_info = FALSE, checkpoint_freq = 500){

  cmd_message <-
  paste0('cd "', py_scripts_loc,'\\cameratraps"\n',
         "call activate cameratraps-detector\n",
         'set PYTHONPATH=%PYTHONPATH%;', py_scripts_loc, '\\CameraTraps;', py_scripts_loc, '\\ai4eutils;', py_scripts_loc, '\\yolov5\n',
         'python detection\\run_detector_batch.py "', md_model_loc, '" "', pics_dir, '" "', pics_dir,'\\md_out.json"', " --recursive")

  if(!force.overwrite & file.exists(paste0(pics_dir,'\\md_out.json'))) {
    cmd_message <- paste0(cmd_message, ' --resume_from_checkpoint "', paste0(pics_dir,'\\md_out.json"'))
  }

  if(run_info){
    cmd_message <- paste0(cmd_message, " --checkpoint_frequency ", checkpoint_freq, " --checkpoint_path ", here::here("checkpoint.json"), " >> ", here::here("run_info.txt"))
  }


  writeLines(cmd_message, bat_loc)

}
