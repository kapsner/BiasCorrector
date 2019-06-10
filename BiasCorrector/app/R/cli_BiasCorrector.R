# This is a cli-Rscript to run BiasCorrector's algortihm

library(optparse)

# define options
option_list <- list(
  make_option(
    c("--experimental"),
    type = "character",
    default = "NULL",
    help = "path to the experimental file [default= %default]",
    metavar = "character"
  ),
  make_option(
    c("--calibration"),
    type = "character",
    default = "NULL",
    help = "path to the calibration file [default= %default]",
    metavar = "character"
  ),
  make_option(
    c("--type"),
    type = "character",
    default = "1",
    help = "type of data to correct [default= %default]",
    metavar = "character"
  ),
  make_option(
    c("--csvdir"),
    type = "character",
    default = "./csv",
    help = "directory to store created csv files [default= %default]",
    metavar = "character"
  ),
  make_option(
    c("--plotdir"),
    type = "character",
    default = "./plot",
    help = "directory to store created plots [default= %default]",
    metavar = "character"
  ),
  make_option(
    c("--port"),
    type = "numeric",
    default = 5432,
    help = "i2b2 database port [default= %default]",
    metavar = "numeric"
  )
)

# get options from cli arguments
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)