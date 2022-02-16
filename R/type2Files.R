# BiasCorrector: A GUI to Correct Measurement Bias in DNA Methylation Analyses
# Copyright (C) 2019-2022 Lorenz Kapsner
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

type2_fileconfirm <- function(filelist,
                              choiceslist,
                              rv,
                              ...) {

  arguments <- list(...)

  rBiasCorrection::write_log(
    message = "Entered 'type2FileConfirm'-Function",
    logfilename = arguments$logfilename
  )

  rv$calibr_steps <- choiceslist[, ("step") := as.numeric(
    get("step")
  )][order(get("step"), decreasing = FALSE)]

  if (rv$calibr_steps[, min(get("step"))] < 0 |
      rv$calibr_steps[, max(get("step"))] > 100) {
    rBiasCorrection::write_log(
      message = paste0("### ERROR ###\nCalibration steps must be ",
                       "in range '0 <= calibration step <= 100'."),
      logfilename = arguments$logfilename
    )
    return("calibrange2")
  } else if (rv$calibr_steps[, sum(duplicated(get("step")))] > 0) {
    rBiasCorrection::write_log(
      message = paste0("### ERROR ###\nThe calibration steps ",
                       "provided do not meet the file requirements!",
                       "\nCalibration steps must be in range '0 <= ",
                       "calibration step <= 100'.\nEach calibration ",
                       "step may only be assigned once."),
      logfilename = arguments$logfilename
    )
    return("calibrange3")
  } else {

    # get unique gene names of first table (all tables must be equal,
    # has been checked anywhere else??!)
    gene_names <- unique(
      filelist[[rv$calibr_steps[1, get("name")]]][
        , c("locus_id", "CpG_count"), with = FALSE
        ]
    )
    # get list of colnames
    col_names <- colnames(filelist[[rv$calibr_steps[1, get("name")]]])
    # initialize final calibration_list
    final_calibs <- list()
    for (g in gene_names[, get("locus_id")]) {
      # create empty matrix/data.table of dimension CpG_count + 2
      #% (true_methylation +  rownames)
      m <- data.table::data.table(
        matrix(
          nrow = 0,
          ncol = (as.numeric(
            gene_names[get("locus_id") == g, get("CpG_count")]
          ) + 2)
        )
      )
      # rename columns
      colnames(m) <- c("true_methylation",
                       col_names[2:(ncol(m) - 1)],
                       "row_means")
      # store empty data.table with right dimensions in list
      final_calibs[[g]] <- m
    }

    # loop through provided calibration files, extract
    # calibration data for each locus and
    # rbind it to final_calibs for specific locus id
    for (n in seq_len(nrow(rv$calibr_steps))) {
      # get imported calibration data (step by step)
      basefile <- filelist[[rv$calibr_steps[n, get("name")]]]
      calstep <- rv$calibr_steps[n, get("step")]
      vec <- colnames(basefile)

      # loop through loci in basefile and append results to
      # final_calibs
      for (locus in gene_names[, get("locus_id")]) {
        vec2 <- c(vec[2:(gene_names[get("locus_id") == locus, get("CpG_count")]
                         + 1)],
                  "row_means")
        add_df <- basefile[get("locus_id") == locus, (vec2), with = FALSE]
        final_calibs[[locus]] <- rbind(
          final_calibs[[locus]],
          cbind(true_methylation = rep(calstep, nrow(add_df)),
                add_df
          )
        )
      }
    }
    return(final_calibs)
  }
}
