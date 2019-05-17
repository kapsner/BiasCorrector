createPlots <- function(plotlist, f, rv, filename){
  plotPNG({
    # add plot and plot statistics here, "j" is necessary to get values for curve in equations
    # always reset j to f
    rv$j <- f
    return(print(plotlist +
                   stat_function(fun = hyperbolic_equation, args = list(rv=rv), geom = "line", aes(color = "Hyperbolic Regression"), size=1.06) +
                   stat_function(fun = cubic_equation, args = list(rv=rv), geom = "line", aes(color = "Cubic Regression"), size=1.06) +
                   geom_line(aes(x=plotlist$data$true_methylation, y=plotlist$data$true_methylation, color = "unbiased"), linetype="dashed", size=1.04) +
                   labs(color = element_blank()) +
                   scale_color_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF"),
                                      labels = c("Cubic Regression", "Hyperbolic Regression", "unbiased")) + 
                   #scale_colour_manual("Regression:", values = c(Cubic = "indianred1", Hyperbolic = "mediumspringgreen", unbiased = "lightblue")) +
                   ggpubr::theme_pubr()
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
      dt <- rbind(dt, cbind(timepoint="corrected", value = round(error_data[Name==vec_cal[i],relative_error], 3),
                            regressiontype = ifelse(error_data[Name==vec_cal[i],better_model] == 1, "Cubic Regression", "Hyperbolic Regression")))
      dt <- rbind(dt, cbind(timepoint="biased", value = round(error_data[Name==vec_cal[i],relative_error_pre], 3), 
                            regressiontype = "unbiased"))
      
      dt[,regressiontype := factor(regressiontype, levels = c("Cubic Regression", "Hyperbolic Regression", "unbiased"))]
      
      if ("Cubic Regression" %in% dt[,regressiontype]){
        values <- c("#E64B35FF", "#00A087FF")
      } else if ("Hyperbolic Regression" %in% dt[,regressiontype]){
        values <- c("#4DBBD5FF", "#00A087FF")
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
                #legend.position = "none", 
                axis.ticks.x = element_blank(), 
                axis.text.x = element_blank())
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
