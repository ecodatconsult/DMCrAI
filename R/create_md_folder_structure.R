#' @export
#'

create_md_folder_structure <- function(screened_images_files, simple.solution = TRUE){
  if(simple.solution){
    for(md_dir in c("md_empty", "md_person", "md_vehicle")){
      new_dirs <- paste0(unique(dirname(screened_images_files)),
                         .Platform$file.sep,
                         md_dir)
      for(new_dir in new_dirs) dir.create(new_dir)
    }
  }else{
    folder_structure <- strsplit(screened_images$file, "/")
    folder_depth <- max(unlist(lapply(folder_structure, length)))
    folder_structure <- lapply(folder_structure, function(x){
      x[length(x)] <- NA
      c(x[1:(length(x) - 1)], rep(NA, folder_depth - (length(x) - 1)))
    } )

    folder_structure <- do.call(rbind, folder_structure)

    folder_structure_equal <- paste(folder_structure[1,apply(folder_structure, 2, function(x) length(unique(x[!is.na(x)])) == 1)], collapse = "/")
    folder_structure_branch <- unique(folder_structure[,which.min(apply(folder_structure, 2, function(x) length(unique(x[!is.na(x)])) == 1))])

    for(md_dir in c("md_empty", "md_person", "md_vehicle")){
      dir.create(paste0(folder_structure_equal, "/", md_dir))

      for(branch in folder_structure_branch){
          dir.create(paste0(folder_structure_equal, "/", md_dir, "/", branch))
          copy_dir(paste0(folder_structure_equal, "/", branch, "/"),
                   paste0(folder_structure_equal, "/", md_dir ,"/", branch, "/"))
      }
    }
    return(folder_structure_equal)
  }
}

