## method 1: hyperbolic regression

# implementation of hyperbolic equation
hyperbolic_equation <- function(x, b = NULL, rv){
  
  # in optimization function, b != 0;
  # but afterwards, we hardcode b with the recently optimized value to calculate fits
  if (is.null(b)) {
    b <- rv$result_list[[rv$vec_cal[rv$j]]][["Coef_hyper"]]$b
    y0 <- rv$result_list[[rv$vec_cal[rv$j]]][["Coef_hyper"]]$y0
    y1 <- rv$result_list[[rv$vec_cal[rv$j]]][["Coef_hyper"]]$y1
    m0 <- rv$result_list[[rv$vec_cal[rv$j]]][["Coef_hyper"]]$m0
    m1 <- rv$result_list[[rv$vec_cal[rv$j]]][["Coef_hyper"]]$m1
    
    message <- paste0("# CpG-site: ", rv$vec_cal[rv$j])
    msg2 <- paste("Using bias_weight =", b, ", y0 =", y0, ", y1 =", y1)
    writeLog(paste0(message, "  \n", msg2))
  } else {
    y0 <- rv$y0
    y1 <- rv$y1
    m0 <- rv$m0
    m1 <- rv$m1
  }
  
  # old equation (16.01.2019)
  #return((((y1 * b) - y0) * x + 100 * y0) / ((b * x) - x + 100))
  # new equation (17.01.2019)
  return((((b * y1) - y0) * (x - m0) + (m1 - m0) * y0) / ((b - 1) * (x - m0) + (m1 - m0)))
}

# find best parameters for hyperbolic regression
hyperbolic_regression <- function(df_agg, vec, rv){
  writeLog("Entered 'hyperbolic_regression'-Function")
  
  # y0 <- df_agg[true_methylation==0, CpG]
  # y1 <- df_agg[true_methylation==100, CpG]
  
  rv$y0 <- df_agg[true_methylation==df_agg[,min(true_methylation)], CpG]
  rv$y1 <- df_agg[true_methylation==df_agg[,max(true_methylation)], CpG]
  rv$m0 <- df_agg[,min(true_methylation)]
  rv$m1 <- df_agg[,max(true_methylation)]
  
  # true y-values
  true_levels <- df_agg[,true_methylation]
  
  # implementation of optimization function
  fn <- function(bias){
    fitted_vals <- hyperbolic_equation(true_levels, b = bias, rv = rv)
    # optimize biasfactor with minimizing sum of squares error
    return(sum(I(df_agg[,CpG] - fitted_vals)^2))
  }
  
  # optimization function of built in R -> based on Nelder-Mead
  # by default, optim performs minimization
  # bias_factor <- optim(1, fn, method = "Nelder-Mead")$par
  rv$b <- optim(1, fn, method = "Brent", lower = 0, upper = 50)$par # due to error with Nelder-Mead
  
  # correct values, based on optimized b
  fitted_values <- hyperbolic_equation(true_levels, rv$b, rv = rv)
  
  # fitted values, extrapolated by true methylation and y0 and y1
  df_agg[, fitted := fitted_values]
  
  # sum of squares between fitted and measuerd values
  df_agg[,CpG_fitted_diff := CpG-fitted]
  df_agg[,squared_error := I((CpG_fitted_diff)^2)]
  
  # sum of squared errors = residual sum of squares
  SSE <- as.numeric(df_agg[,sum(squared_error, na.rm = T)])
  
  # calculate raw_error
  df_agg[,CpG_true_diff := abs(CpG-true_methylation)]
  df_agg[,relative_error := ifelse(true_methylation != 0, (CpG_true_diff/true_methylation)*100, NA)]
  
  # squared dist to mean
  df_agg[,squared_dist_mean := sdm(fitted)]
  
  # total sum of squares
  TSS <- as.numeric(df_agg[,sum(squared_dist_mean, na.rm = T)])
  
  # sum of squared errors
  rv$result_list[[vec]] <- list("Var" = vec,
                                "relative_error" = df_agg[,mean(relative_error, na.rm = T)],
                                "SSE_hyper" = SSE)
  
  rv$result_list[[vec]][["Coef_hyper"]] = list("y0" = rv$y0,
                                               "y1" = rv$y1,
                                               "b" = rv$b,
                                               "m0" = rv$m0,
                                               "m1" = rv$m1,
                                               "R2" = 1 - (SSE / TSS))
  
  # delete fitted/squared_error
  df_agg[,c("fitted", "squared_error", "CpG_fitted_diff", "CpG_true_diff", "relative_error", "squared_dist_mean") := NULL]
}
