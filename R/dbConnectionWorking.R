#' Eine Probe-Verbindung zur FVA-Fotofallendatenbank mit Schreibrechten, um das Passwort zu überprüfen.
#'
#' @return Boolean
#' @export
#'
#' @examples #'
#' # returns FALSE
#' dbConnectionWorking("falsches_passwort")
#'
#'
dbConnectionWorking <- function(psw){

  if(file.exists(system.file("db_login.csv", package = "DMCrAI"))){
      tryCatch(
        con_check <- with(read.csv(system.file("db_login.csv", package = "DMCrAI")),{
          DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         user = user,
                         password = psw,
                         host = host,
                         port = port,
                         dbname = db)
        })

      , error=function(e) {})

      if(exists("con_check")) DBI::dbDisconnect(con_check)

      return(exists("con_check"))
    }else{
      stop("Fehlende Login-Informationen, bitte führe zunächst DMCr2::setDBLoginCredentials() erfolgreich aus.")
      }
}

