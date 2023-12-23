test_that("multiplication works", {
  expect_s3_class(exif_image_info(system.file("test_data/wvb_ff_5034_220809", package = "DMCrAI")), "data.frame")
})
