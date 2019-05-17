# render regression-statistics table
renderRegressionStatisticTable <- function(dt){
  # col2rgb("lawngreen"): red=124, green=252, blue=0
  # rgb(124, 252, 0, max=255, alpha=90): "#7CFC005A"
  # https://stackoverflow.com/questions/49636423/how-to-change-the-cell-color-of-a-cell-of-an-r-shiny-data-table-dependent-on-it
  t <- DT::datatable(dt, colnames = c("Name", "Avg. rel. error",
                                      "SSE [h]", "b", "y₀", "y₁", "  ",
                                      "SSE [c]", "ax³", "bx²", "cx", "d",
                                      "better_model"),
                     rownames = F,
                     options = list(scrollX = TRUE, 
                                    pageLength = 20,
                                    columnDefs = list(list(targets = 12, visible = FALSE)), 
                                    dom="ltip"
                     )) %>%
    formatRound(columns=c(2:12), digits=3) %>%
    formatStyle(columns = 3,
                valueColumns = "better_model",
                fontWeight = styleEqual(0, "bold")) %>%
    formatStyle(columns = 3:6,
                valueColumns = "better_model",
                backgroundColor = styleEqual(0, "#7CFC005A")) %>%
    formatStyle(columns = 8,
                valueColumns = "better_model",
                fontWeight = styleEqual(1, "bold")) %>%
    formatStyle(columns = 8:12,
                valueColumns = "better_model",
                backgroundColor = styleEqual(1, "#7CFC005A")) #%>%
    #formatStyle(columns = c(1:11), fontSize = "80%")
  return(t)
}
