# nolint start

packagename <- "BiasCorrector"

# remove existing description object
unlink("DESCRIPTION")
# Create a new description object
my_desc <- desc::description$new("!new")
# Set your package name
my_desc$set("Package", packagename)
# Set your name
my_desc$set_authors(c(
  person("Lorenz A.", "Kapsner", email = "lorenz.kapsner@gmail.com", role = c("cre", "aut", "cph"),
         comment = c(ORCID = "0000-0003-1866-860X")),
  person("Evgeny A.", "Moskalev", role = "aut")
))
# Remove some author fields
my_desc$del("Maintainer")
my_desc$del("LazyData")
# Set the version
my_desc$set_version("0.2.2.9001")
# The title of your package
my_desc$set(Title = "A GUI to Correct Measurement Bias in DNA Methylation Analyses")
# The description of your package
my_desc$set(Description = paste0(
  "A GUI to correct measurement bias in DNA methylation analyses. The 'BiasCorrector' package ",
  "just wraps the functions implemented in the 'R' package 'rBiasCorrection' into a ",
  "shiny web application in order to make them more easily accessible. ",
  "Publication: Kapsner et al. (2021) <doi:10.1002/ijc.33681>."
))
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
#usethis::use_gpl3_license(name = "Lorenz Kapsner")

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
usethis::use_package("rBiasCorrection", type = "Imports", min_version = "0.3.4")

# Suggests
usethis::use_package("testthat", type = "Suggests")
usethis::use_package("lintr", type = "Suggests")

# dev packages
# tag <- "development"
# devtools::install_github(repo = "kapsner/rBiasCorrection", ref = tag, upgrade = "always")
# # https://cran.r-project.org/web/packages/devtools/vignettes/dependencies.html
# desc::desc_set_remotes(paste0("github::kapsner/rBiasCorrection@", tag), file = usethis::proj_get())

# buildignore
usethis::use_build_ignore(".lintr")
usethis::use_build_ignore(".vscode")
usethis::use_build_ignore("tic.R")
usethis::use_build_ignore(".github")
usethis::use_build_ignore("README.md")
usethis::use_build_ignore("NEWS.md")

# gitignore
usethis::use_git_ignore("/*")
usethis::use_git_ignore("/*/")
usethis::use_git_ignore("*.log")
usethis::use_git_ignore("!/.gitignore")
usethis::use_git_ignore("!/.Rbuildignore")
usethis::use_git_ignore("!/.gitlab-ci.yml")
usethis::use_git_ignore("!/data-raw/")
usethis::use_git_ignore("!/DESCRIPTION")
usethis::use_git_ignore("!/inst/")
usethis::use_git_ignore("!/LICENSE.md")
usethis::use_git_ignore("!/man/")
usethis::use_git_ignore("!NAMESPACE")
usethis::use_git_ignore("!/R/")
usethis::use_git_ignore("!/docker/")
usethis::use_git_ignore("!/README.md")
usethis::use_git_ignore("!/NEWS.md")
usethis::use_git_ignore("!/tests/")
usethis::use_git_ignore("/.Rhistory")
usethis::use_git_ignore("!/*.Rproj")
usethis::use_git_ignore("/.Rproj*")
usethis::use_git_ignore("/.RData")
usethis::use_git_ignore("/.vscode")
usethis::use_git_ignore("!/.lintr")
usethis::use_git_ignore("!/.github/")
usethis::use_git_ignore("!/tic.R")

# BiasCorrection(experimental = "../19_PCR-bias/data/example_data/type1/example_data_type1_experimentaldata.csv", calibration = "../19_PCR-bias/data/example_data/type1/example_data_type1_calibrationdata.csv", samplelocusname = "Test")
# covr::package_coverage()
# lintr::lint_package()

usethis::use_tidy_description()


# create NEWS.md using the python-package "auto-changelog" (must be installed)
# https://www.conventionalcommits.org/en/v1.0.0/
# build|ci|docs|feat|fix|perf|refactor|test

# https://github.com/gitpython-developers/GitPython/issues/1016#issuecomment-1104114129
an <- autonewsmd::autonewsmd$new(repo_name = packagename)
an$generate()
an$write(force = TRUE)

# eventually move to nloptr for non linear optimization
badger::badge_doi("10.1002/ijc.33681", "yellow")
badger::badge_cran_download("BiasCorrector", "grand-total", "blue")
badger::badge_cran_download("BiasCorrector", "last-month", "blue")
badger::badge_dependencies("BiasCorrector")

# nolint end
