test_that("dbConnectonWorking returns true with right psw",
          {
            expect_true(dbConnectionWorking(psw = "postgres") | dbConnectionWorking(psw = "fotofalle"))
          }
)
