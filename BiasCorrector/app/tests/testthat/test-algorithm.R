context("test functioning of algorithm, type 1")

# # we need to import the writeLog-function
# source("../../R/utils.R", encoding = "UTF-8")

# debugging prefix
#prefix <- "./BiasCorrector/app/tests/testthat/"
prefix <- "./"

# the writeLog-function needs the logfilename
logfilename <- paste0(prefix, "log.txt")

# initialize our list for reactive values
rv <- list()

library(data.table)


test_that("algorithm test, type 1",{
  # experimental data
  exp_type_1 <- fread(paste0(prefix, "testdata/exp_type_1.csv"))
  rv$fileimportExp <- cleanDT(exp_type_1, "experimental", 1)[["dat"]]
  
  # calibration data
  cal_type_1 <- fread(paste0(prefix, "testdata/cal_type_1.csv"))
  cal_type_1 <- cleanDT(cal_type_1, "calibration", 1)
  rv$fileimportCal <- cal_type_1[["dat"]]
  rv$vec_cal <- cal_type_1[["vec_cal"]]
  
  # some tests
  expect_length(rv$vec_cal, 10)
  expect_type(rv$vec_cal, "character")
  
  
  # reconstruct parts from app_plottingUtility.R
  regression_results <- regressionUtility(rv$fileimportCal, "Testlocus", locus_id = NULL, rv = rv, mode = NULL, headless = TRUE)
  plotlistR <- regression_results[["plot_list"]]
  rv$result_list <- regression_results[["result_list"]]
  
  regression_results2 <- regression_type1(rv$fileimportCal, rv$vec_cal, mode=NULL)
  
  # save regression statistics to reactive value
  rv$regStats <- statisticsList(rv$result_list)
  
  # some tests
  expect_type(regression_results, "list")
  expect_known_hash(regression_results, "fc7ae30d08") # oder 0bdeacf677
  expect_type(plotlistR, "list")
  expect_known_hash(plotlistR, "0c3c5db52b") # oder c2e96f84fc
  expect_type(rv$result_list, "list")
  expect_known_hash(rv$result_list, "8c7d29964f")
  expect_type(rv$regStats, "list")
  expect_s3_class(rv$regStats, "data.table")
  expect_known_hash(rv$regStats, "a27d84167e")
  expect_equal(regression_results, regression_results2)
  expect_equal(regression_results[["plot_list"]], regression_results2[["plot_list"]])
  expect_equal(regression_results[["result_list"]], regression_results2[["result_list"]])
  
  # calculate final results
  # default rv$choices_list == rv$regStats[,.(Name, better_model)]
  solved_eq <- solving_equations(rv$fileimportExp, rv$regStats[,c("Name", "better_model"),with=F], type = 1, rv = rv)
  rv$finalResults <- solved_eq[["results"]]
  rv$substitutions <- solved_eq[["substitutions"]]
  
  # Calibration Data (to show corrected calibration curves)
  solved_eq2 <- solving_equations(rv$fileimportCal, rv$regStats[,c("Name", "better_model"),with=F], type = 1, rv = rv, mode = "corrected")
  rv$fileimportCal_corrected <- solved_eq2[["results"]]
  colnames(rv$fileimportCal_corrected) <- colnames(rv$fileimportCal)
  
  # some tests
  expect_type(solved_eq, "list")
  expect_known_hash(solved_eq, "564019ef97")
  expect_type(rv$finalResults, "list")
  expect_s3_class(rv$finalResults, "data.table")
  expect_known_hash(rv$finalResults, "920658389f")
  expect_type(rv$substitutions, "list")
  expect_s3_class(rv$substitutions, "data.table")
  expect_known_hash(rv$substitutions, "411246447a")
  expect_type(solved_eq2, "list")
  expect_known_hash(solved_eq2, "91795b9115")
  expect_type(rv$fileimportCal_corrected, "list")
  expect_s3_class(rv$fileimportCal_corrected, "data.table")
  expect_known_hash(rv$fileimportCal_corrected, "913f716d0c")
})
