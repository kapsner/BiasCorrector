# BiasCorrector: Correct PCR-bias in DNA methylation analyses
# Copyright (C) 2019 Lorenz Kapsner
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

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