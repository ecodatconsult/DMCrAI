#' @export
#'
# copy folder structure
copy_dir = function(from=choose.dir(),to=choose.dir()){
  oridir=list.dirs(from,full.names = F)

  if(length(oridir) > 1){
    for(i in 2:length(oridir)){
      dir.create(paste0(to,'/',oridir[i]))
    }
  }
}
