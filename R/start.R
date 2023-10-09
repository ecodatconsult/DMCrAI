#' @export
quick_setup <- function(x){
  setDBLoginCredentials("postgres", "postgres", "localhost", 5433, "fotofallen")
  setup_md(FALSE, "C:/megadetector/md_v5a.0.0.pt", "C:/git/")
}
