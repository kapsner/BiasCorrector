## method 2: cubic regression

# implementation of cubic equation
cubic_equation <- function(x, c = NULL, rv){
  
  # in fitting function, c != NULL;
  # but afterwards, we hardcode b with the recently optimized value to calculate fits
  if (is.null(c) == T) {
    c <- sapply(rv$result_list[[rv$vec_cal[rv$j]]][["Coef_cubic"]], unlist)[c(4:1)]
    #c <- sapply(rv$result_list[[rv$vec_cal[i]]][["Coef_cubic"]], `[`)[c(4:1)]
    message <- paste0("# CpG-site: ", rv$vec_cal[rv$j])
    msg2 <- paste("Using c =", paste(c, collapse = ", "))
    writeLog(paste0(message, "  \n", msg2))
    
    # increase j here, when hyperbolic regression has already been called
    rv$j <- rv$j + 1
  }
  return((c[4]*I(x^3) + c[3]*I(x^2) + c[2]*x + c[1]))
}

# find best parameters for cubic regression
cubic_regression <- function(df_agg, vec, rv) {
  writeLog("Entered 'cubic_regression'-Function")
  
  #pol_reg <- lm(true_methylation ~ poly(CpG, degree = 3, raw = T), data = df_agg)
  pol_reg <- lm(CpG ~ true_methylation + I(true_methylation^2) + I(true_methylation^3), data = df_agg)
  cof <- coefficients(pol_reg)
  
  # true y-values
  true_levels <- df_agg[,true_methylation]
  
  # correct values
  fitted_values <- cubic_equation(true_levels, c = cof, rv = rv)
  
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
  # rv$result_list[Var==vec[i], SSE_cubic := df_agg[,sum(squared_error)]]
  rv$result_list[[vec]]["SSE_cubic"] <- SSE
  rv$result_list[[vec]][["Coef_cubic"]] <- list("ax3" = unname(cof[4]),
                                                "bx2" = unname(cof[3]),
                                                "cx" = unname(cof[2]),
                                                "d" = unname(cof[1]),
                                                "R2" = 1 - (SSE / TSS))
  
  df_agg[,c("fitted", "squared_error", "CpG_fitted_diff", "squared_dist_mean") := NULL]
}
