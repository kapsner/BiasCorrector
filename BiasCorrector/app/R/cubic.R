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

# method 2: cubic regression

# implementation of cubic equation
cubic_equation <- function(x, c){
  return((c[4]*I(x^3) + c[3]*I(x^2) + c[2]*x + c[1]))
}

# find best parameters for cubic regression
cubic_regression <- function(df_agg, vec) {
  writeLog("Entered 'cubic_regression'-Function")
  
  #pol_reg <- lm(true_methylation ~ poly(CpG, degree = 3, raw = T), data = df_agg)
  pol_reg <- lm(CpG ~ true_methylation + I(true_methylation^2) + I(true_methylation^3), data = df_agg)
  cof <- coefficients(pol_reg)
  
  # true y-values
  true_levels <- df_agg[,true_methylation]
  
  # correct values
  fitted_values <- cubic_equation(true_levels, c = cof)
  
  # fitted values
  df_agg[, fitted := fitted_values]
  
  # sum of squares between fitted and measuerd values
  df_agg[,CpG_fitted_diff := CpG-fitted]
  df_agg[,squared_error := I((CpG_fitted_diff)^2)]
  
  # sum of squared errors = residual sum of squares
  SSE <- as.numeric(df_agg[,sum(squared_error, na.rm = T)])
  
  # squared dist to mean
  df_agg[,squared_dist_mean := sdm(fitted)]
  
  # total sum of squares
  TSS <- as.numeric(df_agg[,sum(squared_dist_mean, na.rm = T)])
  
  
  # sum of squared errors
  outlist <- list("SSE_cubic" = SSE)
  outlist[["Coef_cubic"]] <- list("ax3" = unname(cof[4]),
                                                "bx2" = unname(cof[3]),
                                                "cx" = unname(cof[2]),
                                                "d" = unname(cof[1]),
                                                "R2" = 1 - (SSE / TSS))
  
  df_agg[,c("fitted", "squared_error", "CpG_fitted_diff", "squared_dist_mean") := NULL]
  
  return(outlist)
}
