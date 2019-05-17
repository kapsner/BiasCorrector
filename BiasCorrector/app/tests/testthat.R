library(testthat)
options(testthat.use_colours = TRUE)

# we need the data.table package
library(data.table)

test_check("testthat")

# start tests manually
# test_dir("./BiasCorrector/app/tests/")