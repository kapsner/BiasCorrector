context("test headless function")

# # we need to import the writeLog-function
# source("../../R/utils.R", encoding = "UTF-8")

# debugging prefix
#prefix <- "./BiasCorrector/app/tests/testthat/"
prefix <- "./"

library(data.table)


test_that("correct functioning of headless, data type 1",{
  
  expect_true(headless(experimental = paste0(prefix, "testdata/exp_type_1.csv"), 
                       calibration = paste0(prefix, "testdata/cal_type_1.csv"),
                       samplelocusname = "Testlocus",
                       type = "1",
                       plotdir = paste0(prefix, "plotdir"),
                       csvdir = paste0(prefix, "csvdir"),
                       logfilename = paste0(prefix, "log.txt")))
  expect_length(list.files(paste0(prefix, "plotdir")), 30)
  expect_length(list.files(paste0(prefix, "csvdir")), 7)
  
  
  # cleanup
  expect_silent(cleanUp(plotdir = paste0(prefix, "plotdir"),
                        csvdir = paste0(prefix, "csvdir")))
  expect_true(file.remove(paste0(prefix, "log.txt")))
})
