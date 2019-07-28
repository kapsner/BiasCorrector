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

# render regression-statistics table
renderRegressionStatisticTable <- function(dt, mode = NULL, minmax){
  # col2rgb("lawngreen"): red=124, green=252, blue=0
  # rgb(124, 252, 0, max=255, alpha=90): "#7CFC005A"
  # https://stackoverflow.com/questions/49636423/how-to-change-the-cell-color-of-a-cell-of-an-r-shiny-data-table-dependent-on-it
  
  if (isFALSE(minmax)){
    cols <- c("Name", "Relative error",
              "SSE [h]", "R\u00B2 [h]", "a", "b", "d", "  ",
              "SSE [c]", "R\u00B2 [c]", "ax\u00B3", "bx\u00B2", "cx", "d",
              "better_model")
    ncols <- 14
    hyperlength <- 7
    
  } else if (isTRUE(minmax)){
    cols <- c("Name", "Relative error",
              "SSE [h]", "R\u00B2 [h]", "b", "y₀", "y₁", "m₀", "m₁", "  ",
              "SSE [c]", "R\u00B2 [c]", "ax\u00B3", "bx\u00B2", "cx", "d",
              "better_model")
    ncols <- 16
    hyperlength <- 9
    
  }
  
  if (is.null(mode)){
    t <- DT::datatable(dt, colnames = cols,
                       rownames = F,
                       options = list(scrollX = TRUE, 
                                      pageLength = 20,
                                      columnDefs = list(list(targets = ncols, visible = FALSE)), 
                                      dom="ltip"
                       )) %>%
      formatRound(columns=c(2:ncols), digits=3) %>%
      formatStyle(columns = 3,
                  valueColumns = "better_model",
                  fontWeight = styleEqual(0, "bold")) %>%
      formatStyle(columns = 3:hyperlength,
                  valueColumns = "better_model",
                  backgroundColor = styleEqual(0, "#7CFC005A")) %>%
      formatStyle(columns = hyperlength + 2,
                  valueColumns = "better_model",
                  fontWeight = styleEqual(1, "bold")) %>%
      formatStyle(columns = (hyperlength + 2):ncols,
                  valueColumns = "better_model",
                  backgroundColor = styleEqual(1, "#7CFC005A")) #%>%
    #formatStyle(columns = c(1:11), fontSize = "80%")
  } else if (mode == "corrected"){
    t <- DT::datatable(dt, colnames = cols,
                       rownames = F,
                       options = list(scrollX = TRUE, 
                                      pageLength = 20,
                                      columnDefs = list(list(targets = ncols, visible = FALSE)), 
                                      dom="ltip"
                       )) %>%
      formatRound(columns=c(2:ncols), digits=3)
  } else {
    t <- "error"
  }
  return(t)
}
