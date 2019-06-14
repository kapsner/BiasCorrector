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

# create aggregated datatable for calibration data
create_agg_df <- function(datatable, index){
  df <- datatable[,c("true_methylation", index), with = F]
  colnames(df)[2] <- "CpG"
  df[,true_methylation := as.numeric(as.character(true_methylation))]
  return(df[, mean(CpG, na.rm = T), by = true_methylation][,CpG := V1][,V1 := NULL])
}

# create aggregated datatable for experimental data
create_agg_df_exp <- function(datatable, index, type){
  if (type==1){
    df <- datatable[,c("sample_id", index), with = F]
    colnames(df)[2] <- "CpG"
    df <- df[, mean(CpG, na.rm = T), by = sample_id][,CpG := V1][,V1 := NULL]
  } else if (type==2){
    df <- datatable[,c("locus_id", index), with = F]
    colnames(df)[2] <- "CpG"
    df <- df[, mean(CpG, na.rm = T), by = locus_id][,CpG := V1][,V1 := NULL]
  }
  return(df)
}
