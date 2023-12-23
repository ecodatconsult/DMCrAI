#' @export
quick_setup <- function(x){
  setDBLoginCredentials("postgres", "postgres", "localhost", 5432, "ctm_db")
  setup_md(FALSE, "/home/alex/git/md_model/md_v5a.0.0.pt", "/home/alex/git")
}
