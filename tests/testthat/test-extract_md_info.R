test_that("extracting information from md_out works with new jsons", {
  new <- lapply(load_md_out_imgs(
    system.file("test_json/new_md_out.json", package = "DMCrAI")
    ),
    extract_md_info)

  expect_length(new, 44)
})

test_that("extracting information from md_out works with old jsons", {
  old <- lapply(load_md_out_imgs(
    system.file("test_json/old_md_out.json", package = "DMCrAI")
    ),
                extract_md_info)
  expect_length(old, 61)
})
