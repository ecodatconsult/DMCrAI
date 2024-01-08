db_column_names <- function(schema, table){
  con <- dbConnection()

  if(RPostgreSQL::dbExistsTable(con,  c(schema, table))){
    db_download <- RPostgreSQL::dbGetQuery(con, glue::glue_sql(
    "SELECT column_name FROM information_schema.columns
    WHERE table_schema = {schema*}
    AND table_name   = {table*};",
    .con = con,
    schema = schema,
    table = table)
    ) |>
      dplyr::pull(column_name)
  }else{
    stop(paste0(schema, ".", table, " table does not exist but is required!"))
  }

  DBI::dbDisconnect(con)
  return(db_download)
}
