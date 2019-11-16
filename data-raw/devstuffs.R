packagename <- "BiasCorrector"

# remove existing description object
unlink("DESCRIPTION")
# Create a new description object
my_desc <- desc::description$new("!new")
# Set your package name
my_desc$set("Package", packagename)
# Set your name
my_desc$set_authors(c(
  person("Lorenz A.", "Kapsner", email = "lorenz.kapsner@gmail.com", role = c("cre", "aut")),
  person("Evgeny A.", "Moskalev", role = "aut")
))
# Remove some author fields
my_desc$del("Maintainer")
# Set the version
my_desc$set_version("0.0.3.9000")
# The title of your package
my_desc$set(Title = "A GUI to Correct Measurement Bias in DNA Methylation Analyses")
# The description of your package
my_desc$set(Description = "BiasCorrector provides a GUI to correct measurement bias in DNA methylation analyses.")
# The description of your package
my_desc$set("Date" = as.character(Sys.Date()))
# The urls
my_desc$set("URL", "https://github.com/kapsner/BiasCorrector")
my_desc$set(
  "BugReports",
  "https://github.com/kapsner/BiasCorrector/issues"
)
# License
my_desc$set("License", "GPL-3")
# Save everyting
my_desc$write(file = "DESCRIPTION")

# License
usethis::use_gpl3_license(name = "Lorenz Kapsner")

# Depends
usethis::use_package("R", min_version = "2.10", type = "Depends")

# Imports
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
usethis::use_package("data.table", type = "Imports")
usethis::use_package("shiny", type = "Imports")
usethis::use_package("shinyjs", type = "Imports")
usethis::use_package("shinydashboard", type = "Imports")
usethis::use_package("magrittr", type = "Imports")
usethis::use_package("DT", type = "Imports")

# Suggests
usethis::use_package("testthat", type = "Suggests")
usethis::use_package("processx", type = "Suggests")
usethis::use_package("lintr", type = "Suggests")

# dev packages
devtools::install_github(repo = "kapsner/rBiasCorrection", ref = "latest", upgrade = "always")
#usethis::use_dev_package("rBiasCorrection", type = "Imports")
# https://cran.r-project.org/web/packages/devtools/vignettes/dependencies.html
desc::desc_set_remotes("github::kapsner/rBiasCorrection@latest", file = usethis::proj_get())


# gitignore
usethis::use_git_ignore("*.Rproj")
usethis::use_git_ignore(".Rproj.user")
usethis::use_git_ignore(".Rhistory")
# usethis::use_git_ignore("LICENSE.md")
# usethis::use_git_ignore("DESCRIPTION")

# BiasCorrection(experimental = "../19_PCR-bias/data/example_data/type1/example_data_type1_experimentaldata.csv", calibration = "../19_PCR-bias/data/example_data/type1/example_data_type1_calibrationdata.csv", samplelocusname = "Test")
# covr::package_coverage()
# lintr::lint_package()
