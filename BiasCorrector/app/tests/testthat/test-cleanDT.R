context("dataimport filechecks")

# we need to import the writeLog-function
source("../../R/utils.R", encoding = "UTF-8")
# the writeLog-function needs the logfilename
logfilename <- "./log.txt"
# initialize our list for reactive values
rv <- list()


# tests
# normal experimental data: exp_type_1
# exp_type_1 <- data.table()
# exp_type_1[,"Sample Identifier" := paste("ID #", 1:100)]
# vec <- paste("CpG #", 1:30)
# exp_type_1[,(vec) := lapply(1:30, function(x){sample(1000, 100, replace = T)/10})]
# # export exp_type_1
# writeCSV(exp_type_1, "./BiasCorrector/app/tests/testthat/exp_type_1.csv")


test_that("test normal function of experimental file import of type 1",{
  exp_type_1 <- fread("exp_type_1.csv")
  expect_known_hash(cleanDT(exp_type_1, "experimental", 1, rv), "090e67e14a")
})



# remove or temporary logfile
file.remove(logfilename)
