createPlots <- function(plotlist, f, rv, filename){
  plotPNG({
    
    # hyperbolic parameters
    b <- rv$result_list[[rv$vec_cal[f]]][["Coef_hyper"]]$b
    y0 <- rv$result_list[[rv$vec_cal[f]]][["Coef_hyper"]]$y0
    y1 <- rv$result_list[[rv$vec_cal[f]]][["Coef_hyper"]]$y1
    m0 <- rv$result_list[[rv$vec_cal[f]]][["Coef_hyper"]]$m0
    m1 <- rv$result_list[[rv$vec_cal[f]]][["Coef_hyper"]]$m1
    
    message <- paste0("# CpG-site: ", rv$vec_cal[f])
    msg2 <- paste("Using bias_weight =", b, ", y0 =", y0, ", y1 =", y1)
    writeLog(paste0(message, "  \n", msg2))
    
    # cubic parameters
    c <- sapply(rv$result_list[[rv$vec_cal[f]]][["Coef_cubic"]], unlist)[c(4:1)]
    #c <- sapply(rv$result_list[[rv$vec_cal[i]]][["Coef_cubic"]], `[`)[c(4:1)]
    message <- paste0("# CpG-site: ", rv$vec_cal[f])
    msg2 <- paste("Using c =", paste(c, collapse = ", "))
    writeLog(paste0(message, "  \n", msg2))
    
    return(print(plotlist +
                   stat_function(fun = hyperbolic_equation, args = list(b=b, y0=y0, y1=y1, m0=m0, m1=m1), geom = "line", aes(color = "Hyperbolic Regression"), size=1.06) +
                   stat_function(fun = cubic_equation, args = list(c=c), geom = "line", aes(color = "Cubic Regression"), size=1.06) +
                   geom_line(aes(x=plotlist$data$true_methylation, y=plotlist$data$true_methylation, color = "unbiased"), linetype="dashed", size=1.04) +
                   labs(color = element_blank()) +
                   scale_color_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF"),
                                      labels = c("Cubic Regression", "Hyperbolic Regression", "unbiased")) + 
                   #scale_colour_manual("Regression:", values = c(Cubic = "indianred1", Hyperbolic = "mediumspringgreen", unbiased = "lightblue")) +
                   ggpubr::theme_pubr() +
                   theme(plot.title = element_text(hjust = 0.5))
    ))
  },
  filename = filename,
  height = 400, 
  width = 450)
}

createBarErrorPlots <- function(statstable_pre, statstable_post, rv, type, b=NULL){
  
  stats_pre <- statstable_pre[,.(Name, relative_error, better_model)]
  stats_post <- statstable_post[,.(Name, relative_error, better_model)]
  
  error_data <- merge(stats_post, stats_pre, by="Name", sort=F, suffixes=c("", "_pre"))
  
  # Test if names are eqal
  if (identical(stats_pre[,Name], stats_post[,Name])){
    
    vec_cal <- stats_pre[,Name]
    length_vector <- length(vec_cal)
    
    Map(function(i) {
      
      plotname <- paste0(gsub("[[:punct:]]", "", vec_cal[i]))
      
      if (type == 1){
        filename <- paste0(plotdir, rv$sampleLocusName, "_", plotname, "_errorplot.png")
      } else if (type == 2){
        filename <- paste0(plotdir,  paste0(gsub("[[:punct:]]", "", b)), "-", rv$sampleLocusName, "_", plotname, "_errorplot.png")
      }
      
      
      plotmessage <- paste("Creating barplot No.", i)
      writeLog(paste(plotmessage, "- filename:", filename))
        
      dt <- data.table(timepoint = character(0), value = numeric(0), regressiontype = character(0))
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = plotmessage, value = 0)
      
      # Increment the progress bar, and update the detail text.
      progress$inc(1/1, detail = paste("... working hard on barplot", i, "of", length_vector))
      
      
      # add relative error of corrected hyperbolic curve
      
      dt <- rbind(dt, cbind(timepoint="biased", value = round(error_data[Name==vec_cal[i],relative_error_pre], 3), 
                            regressiontype = "Uncorrected"))
      dt <- rbind(dt, cbind(timepoint="corrected", value = round(error_data[Name==vec_cal[i],relative_error], 3),
                            regressiontype = ifelse(error_data[Name==vec_cal[i],better_model] == 1, "Corrected [Cubic Regression]", "Corrected [Hyperbolic Regression]")))
      
      # set "Raw" as first level, to show corresponding bar on the left of the plot
      dt[,regressiontype := factor(regressiontype, levels = c("Uncorrected", "Corrected [Cubic Regression]", "Corrected [Hyperbolic Regression]"))]
      
      if ("Corrected [Cubic Regression]" %in% dt[,regressiontype]){
        values <- c("#8491B4FF", "#E64B35FF")
      } else {
        values <- c("#8491B4FF", "#4DBBD5FF")
      }
      
      plotPNG({
        p <- ggplot(dt, aes(x = regressiontype, y=as.numeric(as.character(value)), fill=regressiontype)) +
          #scale_fill_manual(values = c("Cubic Regression" = "indianred1", "Hyperbolic Regression" = "mediumspringgreen")) + 
          geom_col()+
          geom_text(aes(label = as.character(value), y=as.numeric(as.character(value))),  vjust = 3) +
          ylab("% average relative error") +
          labs(title = paste0("Quantification Error ", vec_cal[i]), fill = element_blank()) +
          scale_fill_manual(values = values) + 
          ggpubr::theme_pubr() +
          theme(axis.title.x = element_blank(), 
                legend.position = "none",
                plot.title = element_text(hjust = 0.5)) #, 
                #axis.ticks.x = element_blank(), 
                #axis.text.x = element_blank())
        # print whole plot in return, otherwise it will fail
        return(print(p))
      },
      filename = filename,
      height = 400, 
      width = 450)
    }, 1:length_vector)
  } else {
    writeLog("Error during creating bar plot; Names are not identical.")
  }
}
