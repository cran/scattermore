# This file is part of scattermore.
#
# Copyright (C) 2022 Mirek Kratochvil <exa.exa@gmail.com>
#               2022-2023 Tereza Kulichova <kulichova.t@gmail.com>
#
# scattermore is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# scattermore is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with scattermore. If not, see <https://www.gnu.org/licenses/>.

#' blend_rgba_float
#'
#' Blend RGBA matrices.
#'
#' @param fRGBA_list List of floating-point RGBA arrays with premultiplied alpha (each of the same size N-by-M-by-4). The "first" matrix in the list is the one that will be rendered on "top".
#'
#' @return Blended RGBA matrix.
#'
#' @export
#' @useDynLib scattermore, .registration=TRUE
blend_rgba_float <- function(fRGBA_list) {
  if (length(fRGBA_list) < 1) stop("No input RGBA given.")
  if (length(fRGBA_list) == 1) {
    return(fRGBA_list[[1]])
  }

  fRGBA_1 <- fRGBA_list[[1]]
  if (!is.array(fRGBA_1) || dim(fRGBA_1)[3] != 4) stop("unsupported RGBA format")
  for (i in 2:length(fRGBA_list)) {
    fRGBA_2 <- fRGBA_list[[i]]

    if (!is.array(fRGBA_2) || dim(fRGBA_2)[3] != 4) stop("unsupported RGBA format")
    if ((dim(fRGBA_1)[1] != dim(fRGBA_2)[1]) || (dim(fRGBA_1)[2] != dim(fRGBA_2)[2])) stop("input bitmap dimensions differ")

    rows <- dim(fRGBA_1)[1]
    cols <- dim(fRGBA_1)[2]

    A_1 <- fRGBA_1[, , 4]
    A_2 <- fRGBA_2[, , 4]

    fRGBA <- array(0, c(rows, cols, 4))
    # blend with premultiplied alpha
    fRGBA[, , 1] <- fRGBA_1[, , 1] + (fRGBA_2[, , 1] * (1 - A_1))
    fRGBA[, , 2] <- fRGBA_1[, , 2] + (fRGBA_2[, , 2] * (1 - A_1))
    fRGBA[, , 3] <- fRGBA_1[, , 3] + (fRGBA_2[, , 3] * (1 - A_1))
    fRGBA[, , 4] <- A_1 + (A_2 * (1 - A_1))

    fRGBA_1 <- fRGBA
  }

  return(fRGBA_1)
}
