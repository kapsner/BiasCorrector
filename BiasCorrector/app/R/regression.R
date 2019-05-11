# perform regression with input data of type 1
## during preprocessing, type 2 data will be transformed into representation of type 1 data
regression_type1 <- function(datatable, vec_cal, rv, mode=NULL){
  writeLog("Entered 'regression_type1'-Function")
  
  plot.listR <- list()
  
  for (i in 1:length(vec_cal)){
    message <- paste0("# CpG-site: ", vec_cal[i])
    writeLog(message)
    df_agg <- create_agg_df(datatable, vec_cal[i])
    hyperbolic_regression(df_agg, vec_cal[i], rv = rv)
    cubic_regression(df_agg, vec_cal[i], rv = rv)
    
    if (is.null(mode)){
      custom_ylab <- "% apparent methylation after PCR"
    } else if (mode == "corrected"){
      custom_ylab <- "% apparent methylation after BiasCorrection"
    }
    
    
    p <- ggplot(data=df_agg, aes(x = true_methylation, y = CpG)) + 
      geom_point() + 
      ylab(custom_ylab) + 
      xlab("% actual methylation") + 
      ggtitle(paste("CpG-site:", vec_cal[i])) + 
      geom_text(data = data.frame(),
                aes(x=-0.95, y=Inf, hjust=0, vjust = 1),
                label = paste0("SSE cubic: ", round(rv$result_list[[vec_cal[i]]]$SSE_cubic, 3),
                               "\nSSE hyperbolic: ", round(rv$result_list[[vec_cal[i]]]$SSE_hyper, 3)),
                size = 4)
    plot.listR[[i]] <- p
  }
  return(plot.listR)
}
