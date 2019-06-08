# perform regression with input data of type 1
## during preprocessing, type 2 data will be transformed into representation of type 1 data
regression_type1 <- function(datatable, vec_cal, rv, mode=NULL){
  writeLog("Entered 'regression_type1'-Function")
  
  plot.listR <- list()
  
  for (i in 1:length(vec_cal)){
    message <- paste0("# CpG-site: ", vec_cal[i])
    writeLog(message)
    df_agg <- na.omit(create_agg_df(datatable, vec_cal[i]))
    
    print(df_agg)
    writeLog(paste("Logging df_agg:", vec_cal[i]))
    writeLog(df_agg)
    
    rv$result_list[[vec_cal[i]]] <- hyperbolic_regression(df_agg, vec_cal[i])
    # append result_list
    rv$result_list[[vec_cal[i]]] <- c(rv$result_list[[vec_cal[i]]], cubic_regression(df_agg, vec_cal[i]))
    
    if (is.null(mode)){
      custom_ylab <- "% apparent methylation after PCR"
    } else if (mode == "corrected"){
      custom_ylab <- "% methylation after BiasCorrection"
    }
    
    
    p <- ggplot(data=df_agg, aes(x = as.numeric(as.character(true_methylation)), y = as.numeric(as.character(CpG)))) + 
      geom_point() + 
      ylab(custom_ylab) + 
      xlab("% actual methylation") + 
      ggtitle(paste("CpG-site:", vec_cal[i])) + 
      geom_text(data = data.frame(),
                aes(x=-Inf, y=Inf, hjust=0, vjust = 1),
                label = paste0(" Cubic:\n",
                               "  SSE: ", round(rv$result_list[[vec_cal[i]]]$SSE_cubic, 2),
                               "\n  R²: ", round(rv$result_list[[vec_cal[i]]]$Coef_cubic$R2, 2),
                               "\n\n Hyperbolic:\n", 
                               "  SSE: ", round(rv$result_list[[vec_cal[i]]]$SSE_hyper, 2),
                               "\n  R²: ", round(rv$result_list[[vec_cal[i]]]$Coef_hyper$R2, 2)),
                size = 3.5)
    plot.listR[[i]] <- p
  }
  return(plot.listR)
}
