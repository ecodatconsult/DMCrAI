
#' @param upload_data data.frame, data to be uploaded
#' @param schema, character, name of the schema
#' @param table, character, name of the table
#' @param idfield, character, name of the column that contains a key that should be incremented, only relevent if update_key set to TRUE
#' @param update_key, boolean, determines whether key defined by idfield should be updated

#' @export
db_upload <- function(upload_data, schema, table, idfield = NULL, update_key = FALSE, ...){
  con <- dbConnection()

  if(update_key){
    max_val <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT max({`idfield`}) from {`schema`}.{`table`}", .con =con, schema = schema, table = table, idfield= idfield))
    if(is.na(max_val)) max_val <- 0
    upload_data[,idfield] <- seq(nrow(upload_data)) + max_val
  }

  RPostgreSQL::dbWriteTable(con,
                            c(schema, table),
                            upload_data,
                            ...)

  DBI::dbDisconnect(con)
}
