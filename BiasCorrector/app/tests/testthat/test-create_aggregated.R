context("create df_agg")

# debugging prefix
#prefix <- "./BiasCorrector/app/tests/testthat/"
prefix <- "./"

# the writeLog-function needs the logfilename
logfilename <- paste0(prefix, "log.txt")


library(data.table)

test_that("test functioning of aggregated function", {
  # calibration data
  cal_type_1 <- fread(paste0(prefix, "testdata/cal_type_1.csv"))
  cal_type_1 <- cleanDT(cal_type_1, "calibration", 1)[["dat"]]
  df_agg <- create_agg_df(cal_type_1, colnames(cal_type_1)[2])
  expect_known_hash(df_agg, "6aa2d6fc51")
  
  # experimental data
  exp_type_1 <- fread(paste0(prefix, "testdata/exp_type_1.csv"))
  exp_type_1 <- cleanDT(exp_type_1, "experimental", 1)[["dat"]]
  df_agg <- create_agg_df_exp(exp_type_1, colnames(exp_type_1)[2], type = 1)
  expect_known_hash(df_agg, "eed63df625")
  
  exp_type_2 <- fread(paste0(prefix, "testdata/exp_type_2.csv"))
  exp_type_2 <- cleanDT(exp_type_2, "experimental", 2)[["dat"]]
  df_agg <- create_agg_df_exp(exp_type_2, colnames(exp_type_2)[2], type = 2)
  expect_known_hash(df_agg, "a98d2e8771")
})