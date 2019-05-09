# render regression-statistics table
renderRegressionStatisticTable <- function(dt){
  # https://stackoverflow.com/questions/49636423/how-to-change-the-cell-color-of-a-cell-of-an-r-shiny-data-table-dependent-on-it
  t <- DT::datatable(dt, colnames = c("Name", "SSE (h)", "b", "y0", "y1", "  ", "SSE (c)", "ax³", "bx²", "cx", "d", "better_model"),
                options = list(scrollX = TRUE, 
                               pageLength = 20,
                               columnDefs = list(list(targets = 12, visible = FALSE))
                )) %>%
    formatRound(columns=c(2:12), digits=3) %>%
    formatStyle(columns = 2,
                valueColumns = "better_model",
                fontWeight = styleEqual(0, "bold")) %>%
    formatStyle(columns = 2:5,
                valueColumns = "better_model",
                backgroundColor = styleEqual(0, "lawngreen")) %>%
    formatStyle(columns = 7,
                valueColumns = "better_model",
                fontWeight = styleEqual(1, "bold")) %>%
    formatStyle(columns = 7:11,
                valueColumns = "better_model",
                backgroundColor = styleEqual(1, "lawngreen")) %>%
    formatStyle(columns = c(1:11), fontSize = "80%")
  return(t)
}
