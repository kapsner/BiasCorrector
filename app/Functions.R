# setup
setup <- function(){
  library(shiny)
  library(shinyjs)
  library(DT)
  library(data.table)
  library(ggplot2)
  library(magrittr)
  library(polynom)
  
  # required packages:
  req_packages <- c("shiny",
                    "shinyjs",
                    "DT",
                    "data.table",
                    "ggplot2",
                    "magrittr",
                    "polynom")
  # check, if required packages are already installed, otherwise install them
  vec <- setdiff(req_packages, installed.packages()[,"Package"])
  if (length(vec) != 0){
    for (i in vec){
      cat("Installing required package: ", i, "\n\n")
      suppressMessages(install.packages(i, repos = "https://ftp.fau.de/cran/"))
    }
  }
  rm(vec)
  # load all libraries
  for (i in req_packages){
    suppressMessages(library(i, character.only = T))
  }
  rm(req_packages)
  
  # initialize logfile here
  logfilename <<- paste0("./biascorrector.log")
  suppressMessages(suppressWarnings(file.create(logfilename)))
  invisible(gc())
}


# modify datatable
cleanDT <- function(datatable, description, type) {
  writeLog("Entered 'cleanDT'-Function")
  
  # remove all columns that only contain empty cells
  datatable <- datatable[,Filter(function(x) !(all(x=="")), .SD)]
  
  # load type 1 data
  if (type == "1") {
    message <- "Importing data of type 1: One locus in many samples (e.g., pyrosequencing data)"
    writeLog(message)
    
    # rename cols, save colnames in vector
    if (description == "calibration"){
      message <- "got calibration data"
      writeLog(message)
      names(datatable)[1] <- "true_methylation"
      
    } else if (description == "experimental") {
      message <- "got experimental data"
      writeLog(message)
      names(datatable)[1] <- "sample_id"
      
    } else {
      cat("### ERROR 18 ###")
      return(NULL)
    }
    
    # load type 2 data
  } else if (type == "2") {
    message <- "Importing data of type 2: Many loci in one sample (e.g., next-gen seq or microarray data)"
    writeLog(message)
    
    # rename cols, save colnames in vector
    if (description == "calibration"){
      message <- "got calibration data"
      writeLog(message)
      names(datatable)[1] <- "locus_id"
      
    } else if (description == "experimental") {
      message <- "got experimental data"
      writeLog(message)
      names(datatable)[1] <- "locus_id"
      
    } else {
      cat("### ERROR 36 ###")
      return(NULL)
    }
  } else {
    cat("### ERROR 40 ###")
    return(NULL)
  }
  
  # all other columns are numeric
  vec <- names(datatable)
  datatable[, (vec) := lapply(.SD, function(x){gsub(",", ".", x)}), .SDcols = vec]
  
  # fist column is factor
  if (description == "calibration" && type == "1"){
    # this is needed, because values here must be numeric
    tryCatch({
      datatable[, (vec[1]) := lapply(.SD, function(x){factor(as.numeric(as.character(x)))}), .SDcols = vec[1]]
    }, error = function(e){
      cat("### ERROR 54 ###")
      return(NULL)
    })
  } else {
    datatable[, (vec[1]) := lapply(.SD, as.factor), .SDcols = vec[1]]
  }
  
  # rest is numeric
  datatable[, (vec[-1]) := lapply(.SD, function(x){as.numeric(as.character(x))}), .SDcols = vec[-1]]
  
  # sort datatable by first column and return it
  datatable <- datatable[order(datatable[[vec[1]]])]
  
  
  # some more dataprepration 
  vec_cal <- names(datatable)[-1]
  
  #debug_Data <- data.table(CpG1 = c(1, -5, 432, 5, NA), CpG2 = c(3, -1, -153, 143, 43))
  # replace negative values and values > 100 with NA:
  #datatable[,(vec_cal) := lapply(.SD, function(x){ifelse(x < 0, NA, ifelse(x > 100, NA, as.numeric(x)))}), .SDcols=vec_cal]
  
  # rowmeans are already in type 2 calibration data table (from fileMerger-application!)
  datatable[, rowmeans := rowMeans(datatable[,vec_cal, with=F], na.rm = T)]
  
  # make vec_cal global for type 1 data (many operations of the app rely on vec_cal)
  if (type == "1"){
    vec_cal <<- names(datatable)[-1]
  }
  
  # count number of CpGs in type 2 data
  if (type == "2"){
    datatable[,CpG_count := rowSums(!is.na(datatable[, vec[-1], with=F]))]
  }
  
  # check file requirements: missing values and remove rows containing missing values
  if (type == "1"){
    if (isTRUE(any(is.na(datatable)))){
      before <- nrow(datatable)
      datatable <- na.omit(datatable)
      after <- nrow(datatable)
      omitnas <<- before - after
      message <- paste0("Deleted ", omitnas, " row(s) containing missing values from '", description, " data'.")
      writeLog(message)
    } 
    
    if (description == "calibration"){
      # type 1 data must have at least 4 calibration steps
      if (datatable[,nlevels(factor(true_methylation))] < 4){
        writeLog("### ERROR ###\nThe less than four calibration datasets were provided.\nAt least four distinct calibration steps are required to perform bias correction.")
        return(NULL)
      }
    }
  }
  return(datatable)
}


# check type 2 file requirements
type2FileReq <- function(filelist){
  writeLog("Entered 'type2FileReq'-Function")
  
  if (length(filelist) < 4){
    # error handling fileimport
    writeLog("### ERROR ###\nThe less than four calibration datasets were provided.\nAt least four distinct calibration steps are required to perform bias correction.")
    return("four")
    
    # go on, if there have been uploaded at least 4 files.
  } else {
    
    dims <- lapply(filelist, dim)
    
    # test if all files have the same dimension:
    if (length(unique(dims)) != 1){
      # error handling fileimport
      writeLog("### ERROR ###\nThe dimensions of the provided calibration datasets are not equal.\nAll files have to have the same number of columns and rows.")
      return("dim")
      
      # if all files have the same dimension
    } else {
      
      # check here if all files have same column- and row-naming
      coln <- lapply(filelist, colnames)
      rown <- lapply(filelist, function(x){as.character(x[,locus_id])})
      
      if (length(unique(coln)) != 1 | length(unique(rown)) != 1){
        # error handling fileimport
        writeLog("### ERROR ###\nAll files need to have the same rownames (locus ids) and columnnames (CpG sites).")
        return("naming")
        
      } else {
        
        # necessary for selectInput-choices
        #numberby <- 100/(length(rv$fileimportCal)-1)
        #choicesseq <- seq(from = 0, to = 100.00, by = numberby) 
        pattern <- "(\\_CS\\d+(\\_\\d+)?(\\.csv|\\.CSV))$"
        calibr_steps <<- data.table(name = character(), step = numeric())
        for (i in names(filelist)){
          message <- paste("Filename:", i)
          writeLog(message)
          match <- regmatches(i, regexpr(pattern, i))
          if (length(match) < 1){
            writeLog("### ERROR ###\nFilenaming of the calibration files must be done properly.\nEnd of filename must begin with '_CS' followd by a number, indicating the degree of true methylation.\nAs decimal seperator, '_' is required.")
            return("filename")
          } else {
            calibr_steps <<- rbind(calibr_steps, data.table(name = i, step = as.numeric(gsub("\\_", ".", regmatches(match, regexpr("\\d+(\\_\\d+)?", match))))))
          }
        }
        calibr_steps <<- calibr_steps[order(step, decreasing = F)]
        
        if (calibr_steps[,min(step)] < 0 | calibr_steps[,max(step)] > 100){
          writeLog("### ERROR ###\nCalibration steps must be in range '0 <= calibration step <= 100'.")
          return("calibrange2")
        } else {
          
          # get unique gene names of first table (all tables must be equal, has been checked anywhere else??!)
          gene_names <- unique(filelist[[calibr_steps[1,name]]][,.(locus_id, CpG_count)])
          # get list of colnames
          col_names <- colnames(filelist[[calibr_steps[1,name]]])
          
          # check for duplicate locus_ids here
          if (sum(gene_names[,duplicated(locus_id)]) > 0){
            writeLog("### ERROR ###\nPlease specify an equal number of CpG-sites for each gene locus.")
            return("inconsistency")
          } else {
            return(TRUE) 
          }
        }
      }
    }
  }
}

type2FileConfirm <- function(filelist, choiceslist){
  writeLog("Entered 'type2FileConfirm'-Function")
  
  calibr_steps <<- choiceslist[,step := as.numeric(step)][order(step, decreasing = F)]
  
  if (calibr_steps[,min(step)] < 0 | calibr_steps[,max(step)] > 100){
    writeLog("### ERROR ###\nCalibration steps must be in range '0 <= calibration step <= 100'.")
    return("calibrange2")
  } else if (calibr_steps[,sum(duplicated(step))] > 0){
    writeLog("### ERROR ###\nThe calibration steps provided do not meet the file requirements!\nCalibration steps must be in range '0 <= calibration step <= 100'.\nEach calibration step may only be assigned once.")
    return("calibrange3")
  } else {
    
    # get unique gene names of first table (all tables must be equal, has been checked anywhere else??!)
    gene_names <- unique(filelist[[calibr_steps[1,name]]][,.(locus_id, CpG_count)])
    # get list of colnames
    col_names <- colnames(filelist[[calibr_steps[1,name]]])
    # initialize final calibration_list
    final_calibs <- list()
    for (g in gene_names[,locus_id]){
      # create empty matrix/data.table of dimension CpG_count + 2 (true_methylation +  rownames)
      m <- data.table(matrix(nrow = 0, ncol = (as.numeric(gene_names[locus_id==g, CpG_count]) + 2)))
      # rename columns
      colnames(m) <- c("true_methylation", col_names[2:(ncol(m)-1)], "rowmeans")
      # store empty data.table with right dimensions in list
      final_calibs[[g]] <- m
    }
    
    # loop through provided calibration files, extract calibration data for each locus and 
    # rbind it to final_calibs for specific locus id
    for (n in 1:nrow(calibr_steps)){
      # get imported calibration data (step by step)
      basefile <- filelist[[calibr_steps[n,name]]]
      calstep <- calibr_steps[n,step]
      vec <- colnames(basefile)
      
      # loop through loci in basefile and append results to final_calibs
      for (locus in gene_names[,locus_id]){
        vec2 <- c(vec[2:(gene_names[locus_id==locus,CpG_count]+1)], "rowmeans")
        add_df <- basefile[locus_id==locus, (vec2), with=F]
        final_calibs[[locus]] <- rbind(final_calibs[[locus]], cbind(true_methylation = rep(calstep, nrow(add_df)), add_df))
      }
    }
    return(final_calibs)
  }
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
    message <- paste0("# CpG-site: ", vec_cal[j])
    msg2 <- paste("Using bias_weight =", b, ", y0 =", y0, ", y1 =", y1)
    writeLog(paste0(message, "  \n", msg2))
  }
  
  return((((y1 * b) - y0) * x + 100 * y0) / ((b * x) - x + 100))
}

# find best parameters for hyperbolic regression
hyperbolic_regression <- function(df_agg, vec){
  writeLog("Entered 'hyperbolic_regression'-Function")
  
  # y0 <<- df_agg[true_methylation==0, CpG]
  # y1 <<- df_agg[true_methylation==100, CpG]
  
  y0 <<- df_agg[true_methylation==df_agg[,min(true_methylation)], CpG]
  y1 <<- df_agg[true_methylation==df_agg[,max(true_methylation)], CpG]
  
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
  result_list[[vec]] <<- list("Var" = vec,
                              "SSE_hyper" = df_agg[,sum(squared_error)],
                              "Coef_hyper" = list("y0" = y0,
                                                  "y1" = y1,
                                                  "b" = bias_factor))
  
  # delete fitted/squared_error
  df_agg[,c("fitted", "squared_error") := NULL]
}


## method 2: cubic regression

# implementation of cubic equation
cubic_equation <- function(x, c = NULL){
  
  # in fitting function, c != NULL;
  # but afterwards, we hardcode b with the recently optimized value to calculate fits
  if (is.null(c) == T) {
    c <- sapply(result_list[[vec_cal[j]]][["Coef_cubic"]], unlist)[c(4:1)]
    #c <- sapply(result_list[[vec_cal[i]]][["Coef_cubic"]], `[`)[c(4:1)]
    message <- paste0("# CpG-site: ", vec_cal[j])
    msg2 <- paste("Using c =", paste(c, collapse = ", "))
    writeLog(paste0(message, "  \n", msg2))
    
    # increase j here, when hyperbolic regression has already been called
    j <<- j + 1
  }
  return((c[4]*I(x^3) + c[3]*I(x^2) + c[2]*x + c[1]))
}

# find best parameters for cubic regression
cubic_regression <- function(df_agg, vec) {
  writeLog("Entered 'cubic_regression'-Function")
  
  #pol_reg <- lm(true_methylation ~ poly(CpG, degree = 3, raw = T), data = df_agg)
  pol_reg <<- lm(CpG ~ true_methylation + I(true_methylation^2) + I(true_methylation^3), data = df_agg)
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
  writeLog("Entered 'regression_type1'-Function")
  
  plot.listR <<- list()
  
  for (i in 1:length(vec_cal)){
    message <- paste0("# CpG-site: ", vec_cal[i])
    writeLog(message)
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
  # mark the better model: 1 = cubic, 0 = hyperbolic
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
  writeLog("Entered 'solving_equations'-Function")
  
  # create results dataframe
  results <- data.table(sample_id = datatable[,unique(sample_id)])
  
  substitutions <<- data.table(sample_id = character(), 
                               CpG_site = character(),
                               original = character(),
                               replacement = character())
  
  # loop through colnames aka. CpG-sites
  for (i in colnames(datatable)[-1]){
    
    # initialize ouput-vector
    vector <- character()
    
    df_agg_ex <<- create_agg_df_exp(datatable, i)
    
    # if cubic regression has better sse-score (default), or
    # if user selects cubic regression for calculation manually in GUI
    if (regmethod[Name==i,better_model] == 1){
      message <- paste("Solving cubic regression for", i)
      writeLog(message)
      
      # get parameters
      ax3 <<- result_list[[i]][["Coef_cubic"]][["ax3"]]
      bx2 <<- result_list[[i]][["Coef_cubic"]][["bx2"]]
      cx <<- result_list[[i]][["Coef_cubic"]][["cx"]]
      d <<- result_list[[i]][["Coef_cubic"]][["d"]]
      
      # loop through rows by samplenames
      for (j in as.vector(df_agg_ex[,sample_id])){
        msg1 <- paste("Samplename:", j)
        
        # this is the required form of the coefficients for polynomial-function
        coe <- c(d-df_agg_ex[sample_id==j,CpG], cx, bx2, ax3)
        #print(coe)
        
        x_vec <- solve(polynomial(coe))               # polynom
        #x_vec <- cubic(rev(coe))                    # RConics
        
        #print(paste(x_vec, class(x_vec)))
        
        find_x <- list()
        
        for (m in x_vec){
          #print(m)
          if (grepl("(+|-)0i", as.character(m))){   # muss am ende stehen!
            compl <- as.character(m)
            compl_out <- as.numeric((substr(compl, 1, nchar(compl)-3)))
            find_x <- c(find_x, compl_out)
          } else {
            find_x <- c(find_x, m)
          }
        }
        
        # generate checkpoint, to look, if fitting value has been found
        checkpoint <- FALSE
        # non-fitting numeric values
        nonfitting <- numeric()
        
        for (k in 1:length(find_x)){
          if (class(find_x[[k]]) == "numeric"){
            if (find_x[[k]] >= 0 & find_x[[k]] <= 100){
              # valid values must be 0 <= x <= 100
              msg2 <- "Root in between the borders! Added to results."
              vector <- c(vector, find_x[[k]])
              
              # break here, when first fitting value is found
              checkpoint <- TRUE
              writeLog(paste0(msg1, "  \nRoot: ", round(find_x[[k]], 3), "  \n--> ", msg2))
              break
              
            } else {
              nonfitting <- c(nonfitting, find_x[[k]])
            }
          }
        }
        
        if (checkpoint == FALSE) {
          msg2 <- "## WARNING ##\nNo fitting root within the borders found."
          
          # if there no fitting numeric roots have been found, look, if there are negative numeric roots
          # if there are nonfitting roots in range -10 - 0 (what we accept as "0")
          if (sum(nonfitting < 0 & nonfitting > -10) > 0){
            nf <- nonfitting[nonfitting < 0 & nonfitting > -10]
            msg3 <- paste0("Negative numeric root found:  \nRoot: ", round(nf, 3), "  \n--> '-10 < root < 0' --> substitute 0")
            # if there are negative numeric roots, substitute value with "0", which makes more sense!
            vector <- c(vector, 0)
            
            # store substitutions
            original = as.character(nf)
            replacement = "0"
            
          } else if (sum(nonfitting > 100 & nonfitting < 110) > 0){
            nf <- nonfitting[nonfitting > 100 & nonfitting < 110]
            msg3 <- paste0("Positive numeric root found:  \nRoot: ", round(nf, 3), "  \n--> '100 < root < 110' --> substitute 100")
            # if there are positiv non fitting numeric roots in range 100 - 110
            # substitute value with "100", which makes more sense!
            vector <- c(vector, 100)
            
            # store substitutions
            original = as.character(nf)
            replacement = "100"
            
          } else {
            msg3 <- "No fitting numeric roots within the borders found: substitute NA"
            vector <- c(vector, NA)
            
            # store substitutions
            original = as.character(paste(round(nonfitting, 3), collapse = ", "))
            replacement = "NA"
          }
          
          substitutions <<- rbind(substitutions, data.table(sample_id = j,
                                                            CpG_site = i,
                                                            original = original,
                                                            replacement = replacement))
          
          writeLog(paste0(msg1, "  \n  \n", msg2, "  \n", msg3))
        }
      }
      
      results <- results[, (paste0(i, "_c")) := as.numeric(as.character(vector))]
      
    } else if (regmethod[Name==i,better_model] == 0){
      message <- paste("Solving hyperbolic regression for", i)
      writeLog(message)
      
      y0 <<- result_list[[i]][["Coef_hyper"]][["y0"]]
      y1 <<- result_list[[i]][["Coef_hyper"]][["y1"]]
      b <<- result_list[[i]][["Coef_hyper"]][["b"]]
      
      
      for (j in as.vector(df_agg_ex[,sample_id])){
        msg1 <- paste("Samplename:", j)
        
        h_solv <- as.numeric(as.character(hyperbolic_equation_solved(df_agg_ex[sample_id==j,CpG])))
        print(h_solv)
        
        if (h_solv >= 0 & h_solv <= 100){
          msg2 <- "Root in between the borders! Added to results."
          vector <- c(vector, h_solv)
          writeLog(paste0(msg1, "  \nRoot: ", round(h_solv, 3), "  \n--> ", msg2))
          
        } else {
          msg2 <- "## WARNING ##\nNo fitting root within the borders found."
          
          if (h_solv < 0 & h_solv > -10){
            msg3 <- paste0("Negative numeric root found:  \nRoot: ", round(h_solv, 3), "  \n--> '-10 < root < 0' --> substitute 0")
            vector <- c(vector, 0)
            
            # store substitutions
            original = as.character(h_solv)
            replacement = "0"
            
          } else if (h_solv > 100 & h_solv < 110){
            msg3 <- paste0("Positive numeric root found:  \nRoot: ", round(nf, 3), "  \n--> '100 < root < 110' --> substitute 100")
            vector <- c(vector, 100)
            
            # store substitutions
            original = as.character(h_solv)
            replacement = "100"
            
          } else {
            msg3 <- "No fitting numeric roots within the borders found: substitute NA"
            vector <- c(vector, NA)
            
            # store substitutions
            original = as.character(h_solv)
            replacement = "NA"
            
          }
          
          substitutions <<- rbind(substitutions, data.table(sample_id = j,
                                                            CpG_site = i,
                                                            original = original,
                                                            replacement = replacement))
          
          writeLog(paste0(msg1, "  \n  \n", msg2, "  \n", msg3))
        }
      }
      
      # replace negative values, which are mathematically correct, with "0", which makes more sense
      #vector <- as.numeric(as.character(vector))
      #vector <- ifelse(vector < 0, ifelse(vector > -10, 0, NA), ifelse(vector <= 100, vector, ifelse(vector < 110, 100, NA)))
      
      # append output-vector to results
      results <- results[, (paste0(i, "_h")) := vector]
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

getTimestamp <- function(){
  return(paste(gsub("\\-", "", substr(Sys.time(), 1, 10)), gsub("\\:", "", substr(Sys.time(), 12, 20)), sep="_"))
}

writeLog <- function(message){
  print(paste0("[", getTimestamp(), "]: ", message))
  message_out <- paste0("===========================================  \n",
                        "[Timestamp: ", getTimestamp(), "]  \n  \n",
                        message, "  \n  \n")
  write(message_out, file = logfilename, append = T)
}