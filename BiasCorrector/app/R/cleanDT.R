# modify datatable
cleanDT <- function(datatable, description, type) {
  writeLog("Entered 'cleanDT'-Function")
  
  # workaround for vec_cal
  vec_cal <- NULL
  
  # remove all columns that only contain empty cells
  datatable <- Filter(function(x) !(all(x=="")), datatable)
  # remove all columns that only contain missings
  datatable <- Filter(function(x) !(all(is.na(x))), datatable)
  
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
      cat("### ERROR 18: wrong description ###")
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
      cat("### ERROR 36: wrong description ###")
      return(NULL)
    }
  } else {
    cat("### ERROR 40: wrong type ###")
    return(NULL)
  }
  
  # all other columns are numeric
  vec <- names(datatable)
  datatable[, (vec) := lapply(.SD, function(x){gsub(",", ".", x)}), .SDcols = vec]
  
  # fist column is factor
  if (description == "calibration" & type == "1"){
    # this is needed, because values here must be numeric
    result <- tryCatch({
      datatable[, (vec[1]) := factor(as.numeric(as.character(get(vec[1]))))]
    }, warning = function(w){
      print(w)
      cat("### ERROR 54: first column cannot be parsed to numeric ###")
      return(NULL)
    })
    
    if (is.null(result)){
      return(NULL)
    }
    
  } else {
    datatable[, (vec[1]) := as.factor(get(vec[1]))]
  }
  
  # rest is numeric
  datatable[, (vec[-1]) := lapply(.SD, function(x){as.numeric(as.character(x))}), .SDcols = vec[-1]]
  
  # sort datatable by first column and return it
  datatable <- datatable[order(datatable[[vec[1]]])]
  
  # remove empty rows
  datatable <- datatable[rowSums(datatable[,-1], na.rm = T) != 0,]
  
  # some more dataprepration 
  vec_cal <- names(datatable)[-1]
  
  #debug_Data <- data.table(CpG1 = c(1, -5, 432, 5, NA), CpG2 = c(3, -1, -153, 143, 43))
  # replace negative values and values > 100 with NA:
  #datatable[,(vec_cal) := lapply(.SD, function(x){ifelse(x < 0, NA, ifelse(x > 100, NA, as.numeric(x)))}), .SDcols=vec_cal]
  
  # rowmeans are already in type 2 calibration data table (from fileMerger-application!)
  datatable[, row_means := rowMeans(datatable[,vec_cal, with=F], na.rm = T)]
  
  # make vec_cal global for type 1 data (many operations of the app rely on vec_cal)
  if (type == "1"){
    vec_cal <- names(datatable)[-1]
  }
  
  # count number of CpGs in type 2 data
  if (type == "2"){
    datatable[,CpG_count := rowSums(!is.na(datatable[, vec[-1], with=F]))]
    
    # requirements-check: does every repeated measurement of each locus id have 
    # the same number of CpG-sites specified?
    if (sum(duplicated(unique(datatable[,CpG_count,by=locus_id])$locus_id)) > 0){
      writeLog("### ERROR ###\nThe data provided contains locus ids with heterogeneous counts of CpG-sites.")
      return(NULL)
    }
    
    if (description == "experimental"){
      # order experimental data by CpG-Count in decreasing order
      datatable <- datatable[order(CpG_count, decreasing = T)]
    }
  }
  
  # check file requirements: missing values and remove rows containing missing values
  if (type == "1"){
    # if (isTRUE(any(is.na(datatable)))){
    #   before <- nrow(datatable)
    #   datatable <- na.omit(datatable)
    #   after <- nrow(datatable)
    #   rv$omitnas <- before - after
    #   message <- paste0("Deleted ", rv$omitnas, " row(s) containing missing values from '", description, " data'.")
    #   writeLog(message)
    # } 
    
    if (description == "calibration"){
      # type 1 data must have at least 4 calibration steps
      if (datatable[,nlevels(factor(true_methylation))] < 4){
        writeLog("### ERROR ###\nThe data provided contains less than four calibration steps.\nAt least four distinct calibration steps are required to perform bias correction.")
        return(NULL)
      }
    }
  }
  return(list("dat" = datatable, "vec_cal" = vec_cal))
}
