test_that("returns valid output", {
  out <- checkStandorteNamesWrong(importStandorte(system.file("test_geom/fotofallen_import.gpkg", package = "DMCrAI"), "Grünbrückenmonitoring")$standorte_import_new_sf)
  expect_length(out, 3)
  expect_s3_class(out$standorte_import_new_sf_names_corr, "sf")
  expect_length(out$missing_names, 0)
})
