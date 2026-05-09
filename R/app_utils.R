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

# open modal help function -> handling reactive values
open_modal <- function(description, rv) {
  rv$modal_closed <- FALSE
  rv$modal_type <- description
  requirements_error(description)
}

#' @title Sanitize Filenames
#' @description Replaces non-alphanumeric characters with underscores to prevent
#'   path traversal and other file-related issues.
#' @param filename A character string.
#' @return A sanitized character string.
#' @export
sanitize_filename <- function(filename) {
  if (is.null(filename) || is.na(filename)) {
    return(NULL)
  }
  # Allow only alphanumeric, underscores, and hyphens
  sanitized <- gsub("[^[:alnum:]_-]", "_", filename)
  return(sanitized)
}
