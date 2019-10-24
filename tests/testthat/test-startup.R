context("test startup")

# debugging prefix
#% prefix <- "./tests/testthat/"
#% utildir <- paste0(prefix, "../../")
#% prefix <- "./"
#% utildir <- paste0(prefix, "../../baseApp/")

library(processx)

test_that("correct startup", {
  # from here: https://www.r-bloggers.com/building-a-shiny-app-as-a-package/
  # We're creating a new process that runs the app
  #% print(list.files())
  #% print(list.files(utildir))
  skip_on_cran()
  skip("Skipping during R check")
  x <- process$new(
    "R",
    c(
      "-e",
      # As we are in the tests/testthat dir, we're moving
      # two steps back before launching the whole package
      # and we try to launch the app
      "library(BiasCorrector);launchApp()"
    )
  )
  # We leave some time for the app to launch
  # Configure this according to your need
  Sys.sleep(5)
  # We check that the app is alive
  expect_true(x$is_alive())
  # We kill it
  x$kill()
})
