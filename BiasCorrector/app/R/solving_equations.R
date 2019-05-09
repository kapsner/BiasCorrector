# solved hyperbolic equation
hyperbolic_equation_solved <- function(y, rv){
  b <- rv$b
  y0 <- rv$y0
  y1 <- rv$y1
  m0 <- rv$m0
  m1 <- rv$m1
  
  # old solved equation
  #return(((100 * y0) - (100 * y)) / ((y * b) - (y1 * b) + y0 - y))
  # new solved equation
  return(((m0 * b * (y - y1)) + (m1 * (y0 - y))) / ((b * (y - y1)) - y + y0))
}

# perform fitting of regressions to experimental data
solving_equations <- function(datatable, regmethod, type, rv){
  writeLog("Entered 'solving_equations'-Function")
  
  first_colname <- colnames(datatable)[1]
  
  # create results dataframe
  results <- data.table(id = datatable[,unique(get(first_colname))])
  
  # loop through colnames aka. CpG-sites
  for (i in colnames(datatable)[-1]){
    
    # initialize ouput-vector
    vector <- character()
    
    df_agg_ex <- create_agg_df_exp(datatable, i, type)
    
    # if cubic regression has better sse-score (default), or
    # if user selects cubic regression for calculation manually in GUI
    if (regmethod[Name==i,better_model] == 1){
      message <- paste("Solving cubic regression for", i)
      writeLog(message)
      
      # get parameters
      ax3 <- rv$result_list[[i]][["Coef_cubic"]][["ax3"]]
      bx2 <- rv$result_list[[i]][["Coef_cubic"]][["bx2"]]
      cx <- rv$result_list[[i]][["Coef_cubic"]][["cx"]]
      d <- rv$result_list[[i]][["Coef_cubic"]][["d"]]
      
      # loop through rows by samplenames
      for (j in as.vector(df_agg_ex[,get(first_colname)])){
        msg1 <- paste("Samplename:", j)
        
        # this is the required form of the coefficients for polynomial-function
        coe <- c(d-df_agg_ex[get(first_colname)==j,CpG], cx, bx2, ax3)
        print(coe)
        
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
          
          rv$substitutions <- rbind(rv$substitutions, data.table(id = j,
                                                            CpG_site = i,
                                                            corrected = original,
                                                            replacement = replacement))
          
          writeLog(paste0(msg1, "  \n  \n", msg2, "  \n", msg3))
        }
      }
      
      results <- results[, (paste0(i, "_c")) := as.numeric(as.character(vector))]
      
    } else if (regmethod[Name==i,better_model] == 0){
      message <- paste("Solving hyperbolic regression for", i)
      writeLog(message)
      
      rv$y0 <- rv$result_list[[i]][["Coef_hyper"]][["y0"]]
      rv$y1 <- rv$result_list[[i]][["Coef_hyper"]][["y1"]]
      rv$b <- rv$result_list[[i]][["Coef_hyper"]][["b"]]
      
      
      for (j in as.vector(df_agg_ex[,get(first_colname)])){
        msg1 <- paste("Samplename:", j)
        
        h_solv <- as.numeric(as.character(hyperbolic_equation_solved(df_agg_ex[get(first_colname)==j,CpG], rv=rv)))
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
          
          rv$substitutions <- rbind(rv$substitutions, data.table(id = j,
                                                            CpG_site = i,
                                                            corrected = original,
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

