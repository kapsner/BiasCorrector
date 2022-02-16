# BiasCorrector: A GUI to Correct Measurement Bias in DNA Methylation Analyses
# Copyright (C) 2019-2022 Lorenz Kapsner
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

# give back deleted rows
omitnas_modal <- function(omitnas,
                          data_type) {
  if (!is.null(omitnas)) {
    if (data_type == "experimental") {
      data_type <- "experimental data"
    } else {
      data_type <- "calibration data"
    }
    if (omitnas == 1) {
      message <- paste0("Deleted 1 row containing missing ",
                        "values from ", data_type, ".")
    } else {
      message <- paste0("Deleted ", omitnas, " rows containing ",
                        "missing values from ", data_type, ".")
    }
    # show modal here
    shiny::showModal(shiny::modalDialog(
      message,
      title = "Missing values deleted"
    ))
  }
  invisible(gc())
}
