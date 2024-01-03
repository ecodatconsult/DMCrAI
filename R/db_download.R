
#' @export
db_download <- function(selection = c("project_id", "name"), schema = "core", table = "projects", ...){
  con <- dbConnection()

  if(RPostgreSQL::dbExistsTable(con,  c(schema, table))){
    db_download <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT {`selection`*} FROM {`schema`}.{`table`}",
                                                            .con = con,
                                                            selection = selection,
                                                            schema = schema,
                                                            table = table),
                                        ...)
  }else{
    stop(paste0(schema, ".", table, " table does not exist but is required!"))
  }

  DBI::dbDisconnect(con)
  return(db_download)
}
