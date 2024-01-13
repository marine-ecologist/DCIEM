#' Equivalent Air Depth (EAD) Calculation for Nitrox
#'
#' @description Function to calculate Equivalent Air Depth (EAD) for EANx.
#' @param EANx Percentage of oxygen in the EANx mix (default is 21%).
#' @param depth Depth in metres.
#' @returns A character vector indicating the EAD.
#' @export
#' @examples
#'
#' EAD(EANx=21,depth=11)
#' EAD(27, 9)
#'
EAD <- function(EANx = 21, depth = NULL) { # default is 21% O2
  EAD <- ((depth + 10) * ((1 - (EANx / 100)) / 0.79)) - 10
  round(EAD, 1)
}
