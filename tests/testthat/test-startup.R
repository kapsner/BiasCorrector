context("startup isalive")

library(processx)

test_that("test is_alive", {
  x <- process$new("Rscript", "app.R", wd = "../../../")
  
  Sys.sleep(5)
  expect_true(x$is_alive())
  
  x$kill()
})

