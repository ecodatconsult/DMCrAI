
#' @export
db_download <- function(upload_data, schema, table, ...){
  con <- dbConnection()

  RPostgreSQL::dbWriteTable(con, c(schema, table),
                            upload_data,
                            ...)

  DBI::dbDisconnect(con)
}
