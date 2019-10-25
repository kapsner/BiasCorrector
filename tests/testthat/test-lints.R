context("lints")

#% prefix <- "./"

prefix <- "../../00_pkg_src/BiasCorrector/"

test_that(
  desc = "test lints",
  code = {
    lintlist <- list(
      "R" = list(
        "app_omitnas_modal.R" = NULL,
        "app_render_regressionstatistics.R" = NULL,
        "app_requirements_error.R" = NULL,
        "app_utils.R" = NULL,
        "launch_app.R" = NULL,
        "moduleCalibrationFile.R" = NULL,
        "moduleCorrectedPlots.R" = "cyclomatic",
        "moduleCorrectedStatistics.R" = NULL,
        "moduleExperimentalFile.R" = NULL,
        "moduleFileupload.R" = "cyclomatic",
        "moduleLog.R" = NULL,
        "moduleModelSelection.R" = NULL,
        "modulePlotting.R" = NULL,
        "moduleInfo.R" = NULL,
        "moduleResults.R" = "cyclomatic",
        "moduleSettings.R" = NULL,
        "moduleStatistics.R" = NULL,
        "type2Files.R" = NULL
      ),
      "tests/testthat" = list(
        "test-startup.R" = NULL,
        "test-lints.R" = NULL
      )
    )
    for (directory in names(lintlist)) {
      print(directory)
      for (fname in names(lintlist[[directory]])) {
        print(fname)
        #% print(list.files(prefix))

        # skip on cran
        skip_on_cran()

        lintr::expect_lint(
          file = paste0(
            prefix,
            directory,
            "/",
            fname
          ),
          checks = lintlist[[directory]][[fname]]
        )
      }
    }
  }
)
