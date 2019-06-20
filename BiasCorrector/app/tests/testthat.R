library(testthat)
options(testthat.use_colours = TRUE)

debug_prefix <- "./"
#debug_prefix <- "./BiasCorrector/app/"

# TODO workaround outside r package
source(paste0(debug_prefix, "./R/headless.R"), encoding = "UTF-8")

test_check("testthat")

# start tests manually
test_dir(paste0(debug_prefix, "BiasCorrector/app/tests/"))
test_file(paste0(debug_prefix, "BiasCorrector/app/tests/testthat/test-startup.R"))
test_file(paste0(debug_prefix, "BiasCorrector/app/tests/testthat/test-algorithm.R"))
test_file(paste0(debug_prefix, "BiasCorrector/app/tests/testthat/test-headless.R"))
test_file(paste0(debug_prefix, "BiasCorrector/app/tests/testthat/test-create_aggregated.R"))
test_file(paste0(debug_prefix, "BiasCorrector/app/tests/testthat/test-cleanDT.R"))
test_file(paste0(debug_prefix, "BiasCorrector/app/tests/testthat/test-app_utils.R"))
