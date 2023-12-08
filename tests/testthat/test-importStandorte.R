test_that("correct length of import data", {
  expect_length(importStandorte(system.file("test_geom/fotofallen_import.shp", package = "DMCrAI"), "Grünbrückenmonitoring"), 3)
})

test_that("shp data imported as sf", {
  expect_s3_class(importStandorte(system.file("test_geom/fotofallen_import.shp", package = "DMCrAI"), "Grünbrückenmonitoring")$standorte_import_new_sf, "sf")
})

test_that("gpkg data imported as sf", {
  expect_s3_class(importStandorte(system.file("test_geom/fotofallen_import.gpkg", package = "DMCrAI"), "Grünbrückenmonitoring")$standorte_import_new_sf, "sf")
})
