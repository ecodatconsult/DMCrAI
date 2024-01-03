test_that("downloading projects works", {
  expect_s3_class(db_download(selection = c("project_id", "name"),
                              schema = "core",
                              table = "projects"),
                  "data.frame")
})

test_that("downloading projects works", {
  expect_s3_class(db_download_with_filter(selection = c("deployment_id"),
                              schema = "import",
                              table = "ctds",
                              filter_col = "project_id",
                              filter_values = "arbitrary"),
                  "data.frame")
})
