createPlots <- function(plotlist, f, rv, filename){
  plotPNG({
    # add plot and plot statistics here, "j" is necessary to get values for curve in equations
    # always reset j to f
    rv$j <- f
    return(print(plotlist +
                   geom_abline(aes(slope = 1, intercept = 0, color = "unbiased"), linetype="dashed", size=1.05) +
                   stat_function(fun = hyperbolic_equation, args = list(rv=rv), geom = "line", aes(color = "Hyperbolic")) +
                   stat_function(fun = cubic_equation, args = list(rv=rv), geom = "line", aes(color = "Cubic")) +
                   scale_colour_manual("Regression:", values = c(Cubic = "red", Hyperbolic = "green", unbiased = "lightblue"))
    ))
  },
  filename = filename,
  height = 400, 
  width = 600)
}

createBarErrorPlots <- function(statstable_pre, statstable_post, rv){
  
  stats_pre <- statstable_pre[,.(Name, SSE_hyperbolic, SSE_cubic)]
  stats_post <- statstable_post[,.(Name, SSE_hyperbolic, SSE_cubic)]
  
  # Test if names are eqal
  if (identical(stats_pre[,Name], stats_post[,Name])){
    
    vec_cal <- stats_pre[,Name]
    length_vector <- length(vec_cal)
    
    Map(function(i) {
      
      plotname <- paste0(gsub("[[:punct:]]", "", vec_cal[i]))
      filename <- paste0(plotdir, rv$sampleLocusName, "_", plotname, "_sse.png")
      
      print(paste("Creating", filename))
        
      dt <- data.table(timepoint = character(0), value = numeric(0), regressiontype = character(0))
      
      for (j in c("SSE_hyperbolic", "SSE_cubic")){
        regtype <- ifelse(j == "SSE_hyperbolic", "Hyperbolic Regression", "Cubic Regression")
        dt <- rbind(dt, cbind(timepoint = "biased", value = round(stats_pre[Name==vec_cal[i],get(j)], 3), regressiontype = regtype))
        dt <- rbind(dt, cbind(timepoint = "unbiased", value = round(stats_post[Name==vec_cal[i],get(j)], 3), regressiontype = regtype))
      }
      
      plotPNG({
        # print whole plot in return, otherwise it will fail
        return(print(ggplot(dt, aes(x = timepoint, y=value, fill=regressiontype)) + 
                       geom_col() +
                       facet_wrap(~ regressiontype) + 
                       scale_fill_manual(values = c("red", "green")) +
                       ylab("SSE") +
                       labs(title = paste0("Comparison of SSE for ", vec_cal[i])) +
                       theme(legend.position = "none", axis.title.x = element_blank())))
      },
      filename = filename,
      height = 400, 
      width = 600)
    }, 1:length_vector)
  } else {
    cat("\nError\n")
  }
}
