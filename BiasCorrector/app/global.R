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
source("./R/cleanDT.R", echo = F, encoding = "UTF-8")
source("./R/type2Files.R", echo = F, encoding = "UTF-8")
source("./R/create_aggregated.R", echo = F, encoding = "UTF-8")
source("./R/hyperbolic.R", echo = F, encoding = "UTF-8")
source("./R/cubic.R", echo = F, encoding = "UTF-8")
source("./R/utils.R", echo = F, encoding = "UTF-8")
source("./R/regression.R", echo = F, encoding = "UTF-8")
source("./R/createPlots.R", echo = F, encoding = "UTF-8")
source("./R/solving_equations.R", echo = F, encoding = "UTF-8")
source("./R/statisticsList.R", echo = F, encoding = "UTF-8")
source("./R/app_requirementsError.R", echo = F, encoding = "UTF-8")
source("./R/app_omitnasModal.R", echo = F, encoding = "UTF-8")
source("./R/app_plottingUtility.R", echo = F, encoding = "UTF-8")
source("./R/app_regressionUtility.R", echo = F, encoding = "UTF-8")
source("./R/app_renderRegressionStatistic.R", echo = F, encoding = "UTF-8")
source("./R/app_utils.R", echo = F, encoding = "UTF-8")

