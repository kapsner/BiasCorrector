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

# render regression-statistics table
render_regressionstatistics <- function(dt,
                                        mode = NULL,
                                        minmax) {
  #% col2rgb("lawngreen"): red=124, green=252, blue=0
  #% rgb (124, 252, 0, max=255, alpha=90) --> "#7CFC005A"
  # https://stackoverflow.com/questions/49636423/how-to-change-the-
  # cell-color-of-a-cell-of-an-r-shiny-data-table-dependent-on-it
  if (isFALSE(minmax)) {
    cols <- c(
      "Name", "Relative error", # 2
      "SSE [h]", "R\u00B2 [h]", "a", "b", "d", "b1", "s", "  ", # 8
      # "SSE [c]", "R\u00B2 [c]", "ax\u00B3", "bx\u00B2", "cx", "d",
      "SSE [c]", "R\u00B2 [c]", "a", "b", "c", "d", # 6
      "better_model"
    )
    ncols <- 16
    hyperlength <- 9
    lastcolor <- ncols
  } else if (isTRUE(minmax)) {
    cols <- c(
      "Name", "Relative error", # 2
      "SSE [h]", "R\u00B2 [h]", "b", "  ", # 4
      # "SSE [c]", "R\u00B2 [c]", "ax\u00B3", "bx\u00B2", "   ",
      "SSE [c]", "R\u00B2 [c]", "a", "b", "   ", # 5
      "y\u2080", "y\u2081", "m\u2080", "m\u2081", # 4
      "better_model"
    )
    ncols <- 15
    hyperlength <- 5
    lastcolor <- 10
  }
  if (is.null(mode)) {
    dt[, ("better_sse") := ifelse(
      get("SSE_cubic") <= get("SSE_hyperbolic"),
      1,
      0
    )]
    cols <- c(cols, "better_sse")
    t <- DT::datatable(dt,
                       colnames = cols,
                       rownames = FALSE,
                       options = list(
                         scrollX = TRUE,
                         pageLength = 20,
                         columnDefs = list(
                           list(
                             targets = c(ncols, ncols + 1),
                             visible = FALSE
                           )
                         ),
                         dom = "ltip"
                       )
    ) %>%
      DT::formatRound(columns = c(2:ncols), digits = 3) %>%
      # hyperbolic parameters
      DT::formatStyle(
        columns = 3,
        valueColumns = "better_sse",
        fontWeight = DT::styleEqual(0, "bold")
      ) %>%
      DT::formatStyle(
        columns = 3:hyperlength,
        valueColumns = "better_model",
        backgroundColor = DT::styleEqual(0, "#7CFC005A")
      ) %>%
      # cubic parameters
      DT::formatStyle(
        columns = hyperlength + 2,
        valueColumns = "better_sse",
        fontWeight = DT::styleEqual(1, "bold")
      ) %>%
      DT::formatStyle(
        columns = (hyperlength + 2):lastcolor,
        valueColumns = "better_model",
        backgroundColor = DT::styleEqual(1, "#7CFC005A")
      ) # %>%
    #" formatStyle(columns = c(1:11), fontSize = "80%")
  } else if (mode == "corrected") {
    t <- DT::datatable(dt,
                       colnames = cols,
                       rownames = FALSE,
                       options = list(
                         scrollX = TRUE,
                         pageLength = 20,
                         columnDefs = list(
                           list(
                             targets = ncols,
                             visible = FALSE
                           )
                         ),
                         dom = "ltip"
                       )
    ) %>%
      DT::formatRound(columns = c(2:ncols), digits = 3)
  } else {
    t <- "error"
  }
  return(t)
}
