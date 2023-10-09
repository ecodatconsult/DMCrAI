#' Baut eine Verbindung zur Fotofallendatenbank auf (PostgreSQL).
#'
#' @return Formal class PostgreSQLConnection
#' @export
#'
dbConnection <- function(){
  if(file.exists(system.file("db_login.csv", package = "DMCrAI"))){
    con <- with(read.csv(system.file("db_login.csv", package = "DMCrAI")), {
      DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                     user = user,
                     password = pw,
                     host = host,
                     port = port,
                     dbname = db)
    })

    return(con)
  }else{
    stop("Kann Datenbank nicht erreichen, bitte führe zunächst DMCrAI::setDBLoginCredentials() erfolgreich aus.")
  }
}
