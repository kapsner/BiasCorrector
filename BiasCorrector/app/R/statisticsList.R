statisticsList <- function(resultlist){
  dt_list <- data.table("Name" = names(resultlist), 
                        "relative_error" = NA,
                        "SSE_hyperbolic" = NA, 
                        "R2_hyperbolic" = NA,
                        "b" = NA, 
                        "y0" = NA, 
                        "y1" = NA,
                        "###" = NA,
                        "SSE_cubic" = NA,
                        "R2_cubic" = NA,
                        "ax3" = NA,
                        "bx2" = NA,
                        "cx" = NA,
                        "d" = NA)
  
  dt_list[, Name := names(resultlist)]
  
  vec <- names(dt_list)[-1]
  dt_list[,(vec) := lapply(.SD, function(x){as.numeric(as.character(x))}), .SDcols = vec]
  
  for (i in names(resultlist)){
    dt_list[Name == i, relative_error := resultlist[[i]][["relative_error"]]
            ][
              Name == i, SSE_hyperbolic := resultlist[[i]][["SSE_hyper"]]
              ][
                Name == i, R2_hyperbolic := resultlist[[i]][["Coef_hyper"]][["R2"]]
                ][
                Name == i, b := resultlist[[i]][["Coef_hyper"]][["b"]]
                ][
                  Name == i, y0 := resultlist[[i]][["Coef_hyper"]][["y0"]]
                  ][
                    Name == i, y1 := resultlist[[i]][["Coef_hyper"]][["y1"]]
                    ][
                      Name == i, SSE_cubic := resultlist[[i]][["SSE_cubic"]]
                      ][
                        Name == i, R2_cubic := resultlist[[i]][["Coef_cubic"]][["R2"]]
                        ][
                        Name == i, ax3 := resultlist[[i]][["Coef_cubic"]][["ax3"]]
                        ][
                          Name == i, bx2 := resultlist[[i]][["Coef_cubic"]][["bx2"]]
                          ][
                            Name == i, cx := resultlist[[i]][["Coef_cubic"]][["cx"]]
                            ][
                              Name == i, d := resultlist[[i]][["Coef_cubic"]][["d"]]
                              ]
  }
  # mark the better model: 1 = cubic, 0 = hyperbolic
  dt_list[,better_model := ifelse(SSE_cubic <= SSE_hyperbolic, 1, 0)]
  
  return(dt_list)
}
