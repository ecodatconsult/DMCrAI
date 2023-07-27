#' Diese Funktion ermittelt das Hauptverzeichnis mehrerer Pfade und legt dort eine neue Unterordner an. In diesen Ordner wird die Ordnerstruktur des Hauptverzeichnis mit allen Unterordnern (ohne den neuen Unterordner) kopiert.
#'
#' @param files character vector, Pfade zu den einzelnen Bildern
#' @param md_dirs character vector, Gibt an für welche Megadetector-Kategorien (md_empty, md_person, md_animal, md_vehicle) separate Ordner erstellt werden sollen. Achtung - Die Shiny-App DMCrAI unterstützt nur md_dirs = "md_empty"
#' @param slash character, gibt den zu nutzenden Pfad-Seperator an.
#'
#' @export
#'

create_md_folder_structure <- function(files, md_dirs = "md_empty", slash = "\\"){
  if(length(unique(basename(dirname(files)))) == 1){
    for(md_dir in md_dirs){
      empty_dir <- paste0(dirname(dirname(files[1])), slash, md_dir, slash, basename(dirname(files[1])))
      dir.create(dirname(empty_dir))
      dir.create(empty_dir)
    }
    folder_structure_equal <- dirname(dirname(files[1]))
  }else{
    folder_structure <- strsplit(files, paste0(slash, slash))
    folder_depth <- max(unlist(lapply(folder_structure, length)))
    folder_structure <- lapply(folder_structure, function(x){
      x[length(x)] <- NA
      c(x[1:(length(x) - 1)], rep(NA, folder_depth - (length(x) - 1)))
    } )


    folder_structure_equal <- paste(folder_structure[1,apply(folder_structure, 2, function(x) length(unique(x[!is.na(x)])) == 1)], collapse = slash)
    folder_structure_branch <- unique(folder_structure[,which.min(apply(folder_structure, 2, function(x) length(unique(x[!is.na(x)])) == 1))])

    for(md_dir in md_dirs){
      dir.create(paste0(folder_structure_equal, slash, md_dir))

      for(branch in folder_structure_branch){
        dir.create(paste0(folder_structure_equal, slash, md_dir, slash, branch))
        copy_dir(paste0(folder_structure_equal, slash, branch, slash),
                 paste0(folder_structure_equal, slash, md_dir ,slash, branch, slash))
      }
    }
  }


    return(folder_structure_equal)
}

