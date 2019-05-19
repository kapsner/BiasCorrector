context("dataimport filechecks")

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


# tests
# normal experimental data: exp_type_1
# exp_type_1 <- data.table()
# exp_type_1[,"Sample Identifier" := paste("ID #", 1:100)]
# vec <- paste("CpG #", 1:30)
# exp_type_1[,(vec) := lapply(1:30, function(x){sample(1000, 100, replace = T)/10})]
# # export exp_type_1
# writeCSV(exp_type_1, "./BiasCorrector/app/tests/testthat/exp_type_1.csv")


test_that("test normal function of file import of type 1",{
  # experimental data
  exp_type_1 <- fread(paste0(prefix, "testdata/exp_type_1.csv"))
  exp_type_1 <- cleanDT(exp_type_1, "experimental", 1, rv)
  expect_known_hash(exp_type_1, "ab5287b084")
  
  exp_type_1 <- fread(paste0(prefix, "testdata/exp_type_1_empty_col.csv"), header = T)
  exp_type_1 <- cleanDT(exp_type_1, "experimental", 1, rv)
  expect_known_hash(exp_type_1, "ab5287b084")
  
  exp_type_1 <- fread(paste0(prefix, "testdata/exp_type_1_empty_row.csv"), header = T)
  exp_type_1 <- cleanDT(exp_type_1, "experimental", 1, rv)
  expect_known_hash(exp_type_1, "1fb62d8498")
  
  # calibration data
  cal_type_1 <- fread(paste0(prefix, "testdata/cal_type_1.csv"))
  cal_type_1 <- cleanDT(cal_type_1, "calibration", 1, rv)
  expect_known_hash(cal_type_1, "23f21fc354")
})

test_that("test normal function of file import of type 2",{
  # experimental data
  exp_type_2 <- fread(paste0(prefix, "testdata/exp_type_2.csv"))
  exp_type_2 <- cleanDT(exp_type_2, "experimental", 2, rv)
  expect_known_hash(exp_type_2, "f8d57b6e9c")
  
  exp_type_2 <- fread(paste0(prefix, "testdata/exp_type_2_empty_col.csv"), header = T)
  exp_type_2 <- cleanDT(exp_type_2, "experimental", 2, rv)
  expect_known_hash(exp_type_2, "f8d57b6e9c")
  
  exp_type_2 <- fread(paste0(prefix, "testdata/exp_type_2_empty_row.csv"), header = T)
  exp_type_2 <- cleanDT(exp_type_2, "experimental", 2, rv)
  expect_known_hash(exp_type_2, "516b7aee57")
  
  # calibration data
  cal_type_2 <- fread(paste0(prefix, "testdata/cal_type_2.csv"))
  cal_type_2 <- cleanDT(cal_type_2, "calibration", 2, rv)
  expect_known_hash(cal_type_2, "d32a53505b")
})

test_that("wrong description",{
  # type 1 data
  cal_type_1 <- fread(paste0(prefix, "testdata/cal_type_1.csv"))
  expect_null(cleanDT(cal_type_1, "calibraRAtion", 1, rv))
  
  exp_type_1 <- fread(paste0(prefix, "testdata/exp_type_1.csv"))
  expect_null(cleanDT(exp_type_1, "experiRINKLmental", 1, rv))
  
  # type 2 data
  cal_type_2 <- fread(paste0(prefix, "testdata/cal_type_2.csv"))
  expect_null(cleanDT(cal_type_2, "calibraRAtion", 2, rv))
  
  exp_type_2 <- fread(paste0(prefix, "testdata/exp_type_2.csv"))
  expect_null(cleanDT(exp_type_2, "experiRINKLmental", 2, rv))
})

# wrong type
test_that("wrong type",{
  # type 1 data
  cal_type_1 <- fread(paste0(prefix, "testdata/cal_type_1.csv"))
  expect_null(cleanDT(cal_type_1, "calibration", 3, rv))
  expect_null(cleanDT(cal_type_1, "calibration", "a", rv))
  
  exp_type_1 <- fread(paste0(prefix, "testdata/exp_type_1.csv"))
  expect_null(cleanDT(exp_type_1, "experimental", 65, rv))
  expect_null(cleanDT(exp_type_1, "experimental", "tre", rv))
  
  # type 2 data
  cal_type_2 <- fread(paste0(prefix, "testdata/cal_type_2.csv"))
  expect_null(cleanDT(cal_type_2, "calibration", 3, rv))
  expect_null(cleanDT(cal_type_2, "calibration", "a", rv))
  
  exp_type_2 <- fread(paste0(prefix, "testdata/exp_type_2.csv"))
  expect_null(cleanDT(exp_type_2, "experimental", 65, rv))
  expect_null(cleanDT(exp_type_2, "experimental", "tre", rv))
})

# wrong first col
test_that("wrong first column in calibration data type 1",{
  # type 1 data
  cal_type_1 <- fread(paste0(prefix, "testdata/cal_type_1_wrong_col_1.csv"))
  expect_null(cleanDT(cal_type_1, "calibration", 1, rv))
  
  cal_type_1 <- fread(paste0(prefix, "testdata/cal_type_1_wrong_col_1_2.csv"))
  expect_null(cleanDT(cal_type_1, "calibration", 1, rv))
  
  cal_type_1 <- fread(paste0(prefix, "testdata/cal_type_1_less4.csv"))
  expect_null(cleanDT(cal_type_1, "calibration", 1, rv))
})

# heterogenous cpg-sites per locus
test_that("heterogenous cpg-sites per locus in type 2 data", {
  cal_type_2 <- fread(paste0(prefix, "testdata/cal_type_2_heterogenous.csv"))
  expect_null(cleanDT(cal_type_2, "calibration", 2, rv))
  
  exp_type_2 <- fread(paste0(prefix, "testdata/exp_type_2_heterogenous.csv"))
  expect_null(cleanDT(exp_type_2, "experimental", 2, rv))
})

# remove temporary files
file.remove(logfilename)
