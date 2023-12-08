test_that("db connection works", {
  expect_s4_class(dbConnection(), "PostgreSQLConnection")
})
