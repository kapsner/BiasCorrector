# BiasCorrector: Correct PCR-bias in DNA methylation analyses
# Copyright (C) 2019 Lorenz Kapsner
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# perform regression with input data of type 1
## during preprocessing, type 2 data will be transformed into representation of type 1 data
regression_type1 <- function(datatable, vec_cal, mode=NULL){
  writeLog("Entered 'regression_type1'-Function")
  
  # result_list
  result_list <- list()
  
  plot.listR <- list()
  
  for (i in 1:length(vec_cal)){
    message <- paste0("# CpG-site: ", vec_cal[i])
    writeLog(message)
    df_agg <- na.omit(create_agg_df(datatable, vec_cal[i]))
    
    print(df_agg)
    writeLog(paste("Logging df_agg:", vec_cal[i]))
    writeLog(df_agg)
    
    result_list[[vec_cal[i]]] <- hyperbolic_regression(df_agg, vec_cal[i])
    # append result_list
    result_list[[vec_cal[i]]] <- c(result_list[[vec_cal[i]]], cubic_regression(df_agg, vec_cal[i]))
    
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
                               "  SSE: ", round(result_list[[vec_cal[i]]]$SSE_cubic, 2),
                               "\n  R²: ", round(result_list[[vec_cal[i]]]$Coef_cubic$R2, 2),
                               "\n\n Hyperbolic:\n", 
                               "  SSE: ", round(result_list[[vec_cal[i]]]$SSE_hyper, 2),
                               "\n  R²: ", round(result_list[[vec_cal[i]]]$Coef_hyper$R2, 2)),
                size = 3.5)
    plot.listR[[i]] <- p
  }
  return(list("plot_list" = plot.listR, "result_list" = result_list))
}
