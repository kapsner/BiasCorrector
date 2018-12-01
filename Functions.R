# modify datatable
cleanDT <- function(datatable, description, type) {
  
  # load type 1 data
  if (type == "1") {
    print("Importing data of type 1")
    
    # rename cols, save colnames in vector
    if (description == "calibration"){
      cat("got calibration data\n\n")
      names(datatable)[1] <- "true_methylation"
      
    } else if (description == "experimental") {
      cat("got experimental data\n\n")
      names(datatable)[1] <- "sample_id"
      
    } else {
      cat("### ERROR ###")
      break
    }
    
    # load type 2 data
  } else if (type == "2") {
    print("Importing data of type 2")
    
    # rename cols, save colnames in vector
    if (description == "calibration"){
      cat("got calibration data\n\n")
      names(datatable)[1] <- "locus_id"
      
    } else if (description == "experimental") {
      cat("got experimental data\n\n")
      names(datatable)[1] <- "locus_id"
      
    } else {
      cat("### ERROR ###")
      break
    }
  } else {
    cat("### ERROR ###")
    break
  }
    
  # all other columns are numeric
  vec <- names(datatable)
  datatable[, (vec) := lapply(.SD, function(x){gsub(",", ".", x)}), .SDcols = vec]
  
  # fist column is factor
  if (description == "calibration" & type == "1"){
    datatable[, (vec[1]) := lapply(.SD, function(x){factor(as.numeric(as.character(x)))}), .SDcols = vec[1]]
  } else {
    datatable[, (vec[1]) := lapply(.SD, as.factor), .SDcols = vec[1]]
  }
  
  # rest is numeric
  datatable[, (vec[-1]) := lapply(.SD, function(x){as.numeric(as.character(x))}), .SDcols = vec[-1]]
  
  # sort datatable by first column and return it
  datatable <- datatable[order(datatable[[vec[1]]])]
  
  # some more dataprepration
  vec_cal <<- names(datatable)[-1]
  datatable[, rowmeans := rowMeans(datatable[,vec_cal, with=F])]
  vec_cal <<- names(datatable)[-1]
  
  # count number of CpGs in type 2 data
  if (type == "2"){
    datatable[,CpG_count := rowSums(!is.na(datatable[, vec[-1], with=F]))]
  }
  
  return(datatable)
}


# create aggregated datatable for calibration data
create_agg_df <- function(datatable, index){
  df <- datatable[,c("true_methylation", index), with = F]
  colnames(df)[2] <- "CpG"
  df[,true_methylation := as.numeric(as.character(true_methylation))]
  return(df[, mean(CpG), by = true_methylation][,CpG := V1][,V1 := NULL])
}


### implementation of different regression methods
## method 1: hyperbolic regression

# implementation of hyperbolic equation
hyperbolic_equation <- function(x, b = NULL){
  
  # in optimization function, b != 0;
  # but afterwards, we hardcode b with the recently optimized value to calculate fits
  if (is.null(b) == T) {
    b <- result_list[[vec_cal[j]]][["Coef_hyper"]]$b
    y0 <- result_list[[vec_cal[j]]][["Coef_hyper"]]$y0
    y1 <- result_list[[vec_cal[j]]][["Coef_hyper"]]$y1
    print(paste("Using bias_weight =", b, ", y0 =", y0, ", y1 =", y1, "\n"))
  }
  
  return((((y1 * b) - y0) * x + 100 * y0) / ((b * x) - x + 100))
}

# find best parameters for hyperbolic regression
hyperbolic_regression <- function(df_agg, vec){
  print("Entering hyperbolic regression\n\n")
  
  y0 <<- df_agg[true_methylation==0, CpG]
  y1 <<- df_agg[true_methylation==100, CpG]
  
  # true y-values
  true_levels <- df_agg[,true_methylation]
  
  # implementation of optimization function
  fn <- function(bias){
    fitted_vals <- hyperbolic_equation(true_levels, b = bias)
    # optimize biasfactor with minimizing sum of squares error
    return(sum(I(df_agg[,CpG] - fitted_vals)^2))
  }
  
  # optimization function of built in R -> based on Nelder-Mead
  # by default, optim performs minimization
  # bias_factor <<- optim(1, fn, method = "Nelder-Mead")$par
  bias_factor <<- optim(1, fn, method = "Brent", lower = -10, upper = 10)$par # due to error with Nelder-Mead
  
  # correct values, based on optimized b
  fitted_values <- hyperbolic_equation(true_levels, bias_factor)
  
  # fitted values, extrapolated by true methylation and y0 and y1
  df_agg[, fitted := fitted_values]
  
  # sum of squares between fitted and measuerd values
  df_agg[,squared_error := I((CpG-fitted)^2)]
  
  # sum of squared errors
  # result_list <<- rbind(result_list, data.table(Var = vec[i], 
  #                                               SSE_hyper = df_agg[,sum(squared_error)],
  #                                               SSE_cubic = NA))
  result_list[[vec]] <<- list("Var" = vec,
                              "SSE_hyper" = df_agg[,sum(squared_error)],
                              "Coef_hyper" = list("y0" = y0,
                                                  "y1" = y1,
                                                  "b" = bias_factor))
}


## method 2: cubic regression

# implementation of cubic equation
cubic_equation <- function(x, c = NULL){
  
  # in fitting function, c != NULL;
  # but afterwards, we hardcode b with the recently optimized value to calculate fits
  if (is.null(c) == T) {
    c <- sapply(result_list[[vec_cal[j]]][["Coef_cubic"]], unlist)[c(4:1)]
    #c <- sapply(result_list[[vec_cal[i]]][["Coef_cubic"]], `[`)[c(4:1)]
    print(paste("Using c =", paste(c, collapse = ", "), "\n"))
    j <<- j + 1
  }
  return((c[4]*I(x^3) + c[3]*I(x^2) + c[2]*x + c[1]))
}

# find best parameters for cubic regression
cubic_regression <- function(df_agg, vec) {
  print("Entering cubic regression\n\n")
  
  #pol_reg <- lm(true_methylation ~ poly(CpG, degree = 3, raw = T), data = df_agg)
  pol_reg <<- lm(CpG ~ true_methylation + I(true_methylation^2) + I(true_methylation^3), data = df_agg)
  #print(summary(pol_reg))
  cof <- coefficients(pol_reg)
  
  # true y-values
  true_levels <- df_agg[,true_methylation]
  
  # correct values
  fitted_values <- cubic_equation(true_levels, c = cof)
  
  # fitted values
  df_agg[, fitted := fitted_values]
  
  # sum of squares between fitted and measuerd values
  df_agg[,squared_error := I((CpG-fitted)^2)]
  
  # sum of squared errors
  # result_list[Var==vec[i], SSE_cubic := df_agg[,sum(squared_error)]]
  result_list[[vec]]["SSE_cubic"] <<- df_agg[,sum(squared_error)]
  result_list[[vec]][["Coef_cubic"]] <<- list("ax3" = unname(cof[4]),
                                              "bx2" = unname(cof[3]),
                                              "cx" = unname(cof[2]),
                                              "d" = unname(cof[1]))
}

initializeListJ <- function(){
  # initialize result_list and j
  
  # save all goodness of fit statistics and for equation necessary parameters in list
  result_list <<- list()
  
  # "j" is necessary to get values for curve in equations
  j <<- 1
}

# perform regression with input data of type 1
regression_type1 <- function(datatable, vec_cal){

  plot.listR <<- list()
  
  for (i in 1:length(vec_cal)){
    cat(paste0("\n\n##########\n", vec_cal[i], "\n\n"))
    df_agg <<- create_agg_df(datatable, vec_cal[i])
    hyperbolic_regression(df_agg, vec_cal[i])
    cubic_regression(df_agg, vec_cal[i])
    
    p <- ggplot(data=df_agg, aes(x = true_methylation, y = CpG)) + 
      geom_point() + 
      ylab("% apparent methylation after PCR") + 
      xlab("% actual methylation") + 
      ggtitle(paste("Variable:", vec_cal[i])) + 
      geom_text(data = data.frame(),
                aes(x=-Inf, y=Inf),
                label = paste("SSE cubic:", round(result_list[[vec_cal[i]]]$SSE_cubic, 3),
                              "\nSSE hyperbolic:", round(result_list[[vec_cal[i]]]$SSE_hyper, 3)),
                hjust = 0, vjust = 1, size = 4)
    plot.listR[[i]] <<- p
  }
  return(plot.listR)
}

# output of regression results
statisticsList <- function(result_list){
  dt_list <- data.table("Name" = names(result_list), 
                        "SSE_hyperbolic" = NA, 
                        "b" = NA, 
                        "y0" = NA, 
                        "y1" = NA,
                        "###" = NA,
                        "SSE_cubic" = NA,
                        "ax³" = NA,
                        "bx²" = NA,
                        "cx" = NA,
                        "d" = NA)
  
  dt_list[, Name := names(result_list)]
  
  vec <- names(dt_list)[-1]
  dt_list[,(vec) := lapply(.SD, function(x){as.numeric(as.character(x))}), .SDcols = vec]
  
  for (i in names(result_list)){
    dt_list[Name == i, SSE_hyperbolic := result_list[[i]][["SSE_hyper"]]
            ][
              Name == i, b := result_list[[i]][["Coef_hyper"]][["b"]]
              ][
                Name == i, y0 := result_list[[i]][["Coef_hyper"]][["y0"]]
                ][
                  Name == i, y1 := result_list[[i]][["Coef_hyper"]][["y1"]]
                  ][
                    Name == i, SSE_cubic := result_list[[i]][["SSE_cubic"]]
                    ][
                      Name == i, "ax³" := result_list[[i]][["Coef_cubic"]][["ax3"]]
                      ][
                        Name == i, "bx²" := result_list[[i]][["Coef_cubic"]][["bx2"]]
                        ][
                          Name == i, cx := result_list[[i]][["Coef_cubic"]][["cx"]]
                          ][
                            Name == i, d := result_list[[i]][["Coef_cubic"]][["d"]]
                            ]
  }
  #dt_list[,(vec) := lapply(.SD, function(x){round(x, 2)}), .SDcols = vec]
  dt_list[,better_model := ifelse(SSE_cubic <= SSE_hyperbolic, 1, 0)]
  
  return(dt_list)
}

### fit regression parameters to experimental data
# create aggregated datatable for experimental data
create_agg_df_exp <- function(datatable, index){
  df <- datatable[,c("sample_id", index), with = F]
  colnames(df)[2] <- "CpG"
  return(df[, mean(CpG), by = sample_id][,CpG := V1][,V1 := NULL])
}

# solved hyperbolic equation
hyperbolic_equation_solved <- function(y){
  return(((100 * y0) - (100 * y)) / ((y * b) - (y1 * b) + y0 - y))
} 

# perform fitting of regressions to experimental data
solving_equations <- function(datatable, regmethod){
  # create results dataframe
  results <- data.table(sample_id = datatable[,unique(sample_id)])
  
  # loop through colnames aka. CpGs
  for (i in colnames(datatable)[-1]){
    
    # initialize ouput-vector
    vector <- character(0)
    
    df_agg_ex <<- create_agg_df_exp(datatable, i)
    
    # if cubic regression has better sse-score (default), or
    # if user selects cubic regression for calculation manually in GUI
    if (regmethod[Name==i,better_model] == 1){
      print(paste("Solving cubic regression for", i, "\n\n"))
      
      # get parameters
      ax3 <<- result_list[[i]][["Coef_cubic"]][["ax3"]]
      bx2 <<- result_list[[i]][["Coef_cubic"]][["bx2"]]
      cx <<- result_list[[i]][["Coef_cubic"]][["cx"]]
      d <<- result_list[[i]][["Coef_cubic"]][["d"]]
      
      # loop through rows by samplenames
      for (j in as.vector(df_agg_ex[,sample_id])){
        
        # this is the required form of the coefficients for polynomial-function
        coe <- c(d-df_agg_ex[sample_id==j,CpG], cx, bx2, ax3)
        print(coe)
        
        x_vec <- solve(polynomial(coe))               # polynom
        #x_vec <- cubic(rev(coe))                    # RConics
        
        print(paste(x_vec, class(x_vec)))
        
        find_x <- list()
        
        for (m in x_vec){
          print(m)
          if (grepl("+0i", as.character(m))){   # muss am ende stehen!
            compl <- as.character(m)
            compl_out <- as.numeric((substr(compl, 1, nchar(compl)-3)))
            find_x <- c(find_x, compl_out)
          } else {
            find_x <- c(find_x, m)
          }
        }
        
        checkpoint <- FALSE
        
        for (k in 1:length(find_x)){
          if (class(find_x[[k]]) == "numeric"){
            if (find_x[[k]] >= 0 & find_x[[k]] <= 100){
              # valid values must be 0 <= x <= 100
              print("Root in between the borders! Calculating results.")
              vector <- c(vector, find_x[[k]])
              checkpoint <- TRUE
              break
            } 
          }
        }
        if (checkpoint == FALSE) {
          print("Checkpoint is FALSE, no fitting root found; add NA")
          vector <- c(vector, NA)
        }
      }
      results <- results[, (paste0(i, "_c")) := as.numeric(as.character(vector))]
      
    } else if (regmethod[Name==i,better_model] == 0){
      print(paste("Solving hyperbolic regression for", i, "\n\n"))
      
      y0 <<- result_list[[i]][["Coef_hyper"]][["y0"]]
      y1 <<- result_list[[i]][["Coef_hyper"]][["y1"]]
      b <<- result_list[[i]][["Coef_hyper"]][["b"]]
      
      for (j in as.vector(df_agg_ex[,sample_id])){
        vector <- c(vector, hyperbolic_equation_solved(df_agg_ex[sample_id==j,CpG]))
      }
      results <- results[, (paste0(i, "_h")) := as.numeric(as.character(vector))]
    }
  }
  return(results)
}

# handle user text inputs
handleTextInput <- function(textinput){
  textinput <- gsub("[^[:alnum:]]", "", textinput)
  
  # max 15 chars:
  if (nchar(textinput) > 15){
    textinput <- substr(textinput, 1, 15)
  }
  
  return(textinput)
}
