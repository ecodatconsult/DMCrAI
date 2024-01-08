
DMCrAI_upload_project <- function(project_data){
  project_data |>
    db_upload(
      schema = "core",
      table = "projects",
      row.names = FALSE,
      append = TRUE,
      overwrite = FALSE
    )
}


DMCrAI_upload_ctds <- function(md_out){
  ctds_names <- db_column_names("import", "ctds")

  dummy <-
  ctds_names |>
    as.data.frame() |>
    t() |>
    as.data.frame() |>
    setNames(ctds_names) |>
    head(0)

  md_out <- readr::read_csv("md_out.csv")

  upload_data <-
  md_out |>
    dplyr::select(deployment, project_id) |>
    dplyr::rename(deployment_id = deployment) |>
    dplyr::filter(!duplicated(deployment_id)) |>
    dplyr::bind_rows(dummy) |>
    dplyr::select(ctds_names) |>
    db_upload(
      schema = "import",
      table = "ctds",
      row.names = FALSE,
      append = TRUE,
      overwrite = FALSE
      )
}


DMCrAI_upload_pictures <- function(md_out){

}


DMCrAI_upload_bbs <- function(md_out){

}


