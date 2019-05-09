# create aggregated datatable for calibration data
create_agg_df <- function(datatable, index){
  df <- datatable[,c("true_methylation", index), with = F]
  colnames(df)[2] <- "CpG"
  df[,true_methylation := as.numeric(as.character(true_methylation))]
  return(df[, mean(CpG), by = true_methylation][,CpG := V1][,V1 := NULL])
}

# create aggregated datatable for experimental data
create_agg_df_exp <- function(datatable, index, type){
  if (type==1){
    df <- datatable[,c("sample_id", index), with = F]
    colnames(df)[2] <- "CpG"
    df <- df[, mean(CpG), by = sample_id][,CpG := V1][,V1 := NULL]
  } else if (type==2){
    df <- datatable[,c("locus_id", index), with = F]
    colnames(df)[2] <- "CpG"
    df <- df[, mean(CpG), by = locus_id][,CpG := V1][,V1 := NULL]
  }
  return(df)
}
