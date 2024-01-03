#' @export
db_download_with_filter <- function(schema, table, filter_col, filter_values, selection, ...){
  con <- dbConnection()

  if(RPostgreSQL::dbExistsTable(con,  c(schema, table))){
    db_download <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT {`selection`*} FROM {`schema`}.{`table`} where {`filter_col`} in ({filter_values*})",
                                                               .con = con,
                                                               schema = schema,
                                                               table = table,
                                                               filter_col = filter_col,
                                                               filter_values = filter_values,
                                                               selection = selection),
                                           ...)
  }else{
    stop(paste0(schema, ".", table, " table does not exist but is required!"))
  }

  DBI::dbDisconnect(con)
  return(db_download)
}
