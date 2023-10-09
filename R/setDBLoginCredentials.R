#' Ändert oder erstellt die Datei mit Login-Information für die Datenbank
#' @param user Character, Name der Datenbankrolle
#' @param pw Character, Passwort für die Datenbankrolle
#' @param host Character, Adresse der Datenbank
#' @param port Numeric, Port der Datenbank
#' @param db Character, Name der Datenbank (i.d.R. "fotofallen")
#'
#'
#' @return Nothing
#' @export
#'
#' @examples setDBLoginCredentials("postgres", "postgres", "localhost", 5433, "fotofallen")
#'

setDBLoginCredentials <- function(user, pw, host, port, db){
  path2login_csv <- system.file("exiftool.exe", package = "DMCrAI")
  filename <- paste0(dirname(path2login_csv), .Platform$file.sep, "db_login.csv")


  data.frame(
    user = user,
    pw = pw,
    host = host,
    port = as.numeric(port),
    db = db
  ) %>%
  readr::write_csv(filename)

  if(dbConnectionWorking(pw)){
    message("RPostgreSQL-Datenbankverbindung eingerichtet und funktionsfähig :-)")
  }else{
    file.remove(filename)
    stop("RPostgreSQL-Datenbankverbindung fehlerhaft, überprüfe zunächst die Login-Informationen und dann den Status der Datenbank! :-(")
  }
}


