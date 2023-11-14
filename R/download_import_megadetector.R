download_import_megadetector_projects <- function(){
  con <- dbConnection()

  if(RPostgreSQL::dbExistsTable(con,  c("import", "megadetector"))){
    projects <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT DISTINCT project_id FROM import.megadetector", .con = con)) |>
      dplyr::pull(project_id)
  }else{
    stop("import.megadetector table does not exist but is required!")
  }

  DBI::dbDisconnect(con)
  return(projects)
}


download_import_megadetector_deployments <- function(project_id){
  con <- dbConnection()

  if(RPostgreSQL::dbExistsTable(con,  c("import", "megadetector"))){
    deployments <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT DISTINCT deployment FROM import.megadetector where project_id in ({project_ids*})", project_ids = project_id, .con = con)) |>
      dplyr::pull(deployment)
  }else{
    stop("import.megadetector table does not exist but is required!")
  }

  DBI::dbDisconnect(con)
  return(deployments)
}

download_import_megadetector_bboxes <- function(project_id, deployment){
  con <- dbConnection()

  if(RPostgreSQL::dbExistsTable(con,  c("import", "megadetector"))){
    bboxes <- RPostgreSQL::dbGetQuery(con, glue::glue_sql("SELECT * FROM import.megadetector where project_id in ({project_ids*}) AND deployment in ({deployments*})", project_ids = project_id, deployments = deployment, .con = con))
  }else{
    stop("import.megadetector table does not exist but is required!")
  }
  DBI::dbDisconnect(con)
  return(bboxes)
}
