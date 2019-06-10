context("test app utils")

# # we need to import the writeLog-function
# source("../../R/utils.R", encoding = "UTF-8")

# debugging prefix
#prefix <- "./BiasCorrector/app/tests/testthat/"
prefix <- "./"

library(data.table)


test_that("correct functioning app utils",{
  
  expect_true(onStart(plotdir = paste0(prefix, "plotdir"),
                        csvdir = paste0(prefix, "csvdir"),
                        logfilename = paste0(prefix, "log.txt")))
  
  
  # cleanup
  expect_silent(cleanUp(plotdir = paste0(prefix, "plotdir"),
                        csvdir = paste0(prefix, "csvdir")))
  expect_true(file.remove(paste0(prefix, "log.txt")))
})
