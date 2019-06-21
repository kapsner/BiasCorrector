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

# define global variables here

# directories
tempdir <- tempdir()
plotdir <- paste0(tempdir, "/plots/")
csvdir <- paste0(tempdir, "/csv/")

# logfilename
logfilename <- paste0(tempdir, "/biascorrector.log")

# maximum filesize in MB
maxfilesize <- 100

# set shiny option here
options(shiny.maxRequestSize = maxfilesize*1024^2)

# include modules
source("./_modules/moduleFileupload.R", encoding = "UTF-8")
source("./_modules/moduleExperimentalFile.R", encoding = "UTF-8")
source("./_modules/moduleCalibrationFile.R", encoding = "UTF-8")
source("./_modules/modulePlotting.R", encoding = "UTF-8")
source("./_modules/moduleStatistics.R", encoding = "UTF-8")
source("./_modules/moduleModelSelection.R", encoding = "UTF-8")
source("./_modules/moduleResults.R", encoding = "UTF-8")
source("./_modules/moduleCorrectedPlots.R", encoding = "UTF-8")

# include global functions
source("./R/type2Files.R", echo = F, encoding = "UTF-8")
source("./R/app_requirementsError.R", echo = F, encoding = "UTF-8")
source("./R/app_omitnasModal.R", echo = F, encoding = "UTF-8")
source("./R/app_renderRegressionStatistic.R", echo = F, encoding = "UTF-8")
source("./R/app_utils.R", echo = F, encoding = "UTF-8")

