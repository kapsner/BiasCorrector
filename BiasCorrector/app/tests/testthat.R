library(testthat)
options(testthat.use_colours = TRUE)

# TODO workaround outside r package
source("../R/headless.R", encoding = "UTF-8")

test_check("testthat")

# start tests manually
# test_dir("./BiasCorrector/app/tests/")
# test_file("./BiasCorrector/app/tests/testthat/test-startup.R")
# test_file("./BiasCorrector/app/tests/testthat/test-algorithm.R")
