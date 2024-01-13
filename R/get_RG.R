#' Calculate Repetitive Groups (RG) for DCIEM Tables
#'
#' @description Function to calculate the Repetitive Group (RG) based on DCIEM diving tables.
#' @param depth Depth in metres.
#' @param bottomtime Bottom time in minutes.
#' @param EANx Enriched air nitrox (EANx) mix.
#' @param RF Repetitive factor.
#' @return A character vector indicating the Repetitive Group.
#' @export
#' @examples
#'
#' get_RG(depth = 5, bottomtime = 50)
#' get_RG(19, 30)
#'
get_RG <- function(depth = NULL, bottomtime = NULL, RF = 1, EANx = 21,...) {

  depth <- EAD(EANx, depth)

  if (depth <= 6 & bottomtime <= 30) {
    RG <- "A"
  } else if (depth <= 6 & bottomtime <= 60) {
    RG <- "B"
  } else if (depth <= 6 & bottomtime <= 90) {
    RG <- "C"
  } else if (depth <= 6 & bottomtime <= 120) {
    RG <- "D"
  } else if (depth <= 6 & bottomtime <= 150) {
    RG <- "E"
  } else if (depth <= 6 & bottomtime <= 180) {
    RG <- "F"
  } else if (depth <= 6 & bottomtime <= 240) {
    RG <- "H"
  } else if (depth <= 6 & bottomtime <= 300) {
    RG <- "H"
  } else if (depth <= 6 & bottomtime <= 360) {
    RG <- "I"
  } else if (depth <= 6 & bottomtime <= 420) {
    RG <- "J"
  } else if (depth <= 6 & bottomtime <= 480) {
    RG <- "K"
  } else if (depth <= 6 & bottomtime <= 600) {
    RG <- "L"
  } else if (depth <= 6 & bottomtime <= 720) {
    RG <- "M"
  } else if (depth <= 9 & bottomtime <= 30) {
    RG <- "A"
  } else if (depth <= 9 & bottomtime <= 45) {
    RG <- "B"
  } else if (depth <= 9 & bottomtime <= 60) {
    RG <- "C"
  } else if (depth <= 9 & bottomtime <= 90) {
    RG <- "D"
  } else if (depth <= 9 & bottomtime <= 100) {
    RG <- "E"
  } else if (depth <= 9 & bottomtime <= 120) {
    RG <- "F"
  } else if (depth <= 9 & bottomtime <= 150) {
    RG <- "H"
  } else if (depth <= 9 & bottomtime <= 180) {
    RG <- "H"
  } else if (depth <= 9 & bottomtime <= 190) {
    RG <- "I"
  } else if (depth <= 9 & bottomtime <= 210) {
    RG <- "J"
  } else if (depth <= 9 & bottomtime <= 240) {
    RG <- "K"
  } else if (depth <= 9 & bottomtime <= 270) {
    RG <- "L"
  } else if (depth <= 9 & bottomtime <= 300) {
    RG <- "M"
  } else if (depth <= 9 & bottomtime > 300) {
    return(print("WARNING: DECOMPRESSION REQUIRED CHECK TABLE"))
  } else if (depth <= 12 & bottomtime <= 22) {
    RG <- "A"
  } else if (depth <= 12 & bottomtime <= 30) {
    RG <- "B"
  } else if (depth <= 12 & bottomtime <= 40) {
    RG <- "C"
  } else if (depth <= 12 & bottomtime <= 60) {
    RG <- "D"
  } else if (depth <= 12 & bottomtime <= 70) {
    RG <- "E"
  } else if (depth <= 12 & bottomtime <= 80) {
    RG <- "F"
  } else if (depth <= 12 & bottomtime <= 90) {
    RG <- "H"
  } else if (depth <= 12 & bottomtime <= 120) {
    RG <- "H"
  } else if (depth <= 12 & bottomtime <= 120) {
    RG <- "I"
  } else if (depth <= 12 & bottomtime <= 120) {
    RG <- "J"
  } else if (depth <= 12 & bottomtime > 150) {
    return(print("WARNING: DECOMPRESSION REQUIRED CHECK TABLE"))
  } else if (depth <= 15 & bottomtime <= 18) {
    RG <- "A"
  } else if (depth <= 15 & bottomtime <= 25) {
    RG <- "B"
  } else if (depth <= 15 & bottomtime <= 30) {
    RG <- "C"
  } else if (depth <= 15 & bottomtime <= 40) {
    RG <- "D"
  } else if (depth <= 15 & bottomtime <= 50) {
    RG <- "E"
  } else if (depth <= 15 & bottomtime <= 60) {
    RG <- "F"
  } else if (depth <= 15 & bottomtime <= 75) {
    RG <- "H"
  } else if (depth <= 15 & bottomtime > 120) {
    return(print("WARNING: DECOMPRESSION REQUIRED CHECK TABLE"))
  } else if (depth <= 18 & bottomtime <= 14) {
    RG <- "A"
  } else if (depth <= 18 & bottomtime <= 20) {
    RG <- "B"
  } else if (depth <= 18 & bottomtime <= 25) {
    RG <- "C"
  } else if (depth <= 18 & bottomtime <= 30) {
    RG <- "D"
  } else if (depth <= 18 & bottomtime <= 40) {
    RG <- "E"
  } else if (depth <= 18 & bottomtime <= 50) {
    RG <- "F"
  } else if (depth <= 18 & bottomtime > 50) {
    return(print("WARNING: DECOMPRESSION REQUIRED CHECK TABLE"))
  } else if (depth <= 21 & bottomtime <= 12) {
    RG <- "A"
  } else if (depth <= 21 & bottomtime <= 15) {
    RG <- "B"
  } else if (depth <= 21 & bottomtime <= 20) {
    RG <- "C"
  } else if (depth <= 21 & bottomtime <= 25) {
    RG <- "D"
  } else if (depth <= 21 & bottomtime <= 35) {
    RG <- "E"
  } else if (depth <= 21 & bottomtime > 34) {
    return(print("WARNING: DECOMPRESSION REQUIRED CHECK TABLE"))
  } else if (depth <= 24 & bottomtime <= 10) {
    RG <- "A"
  } else if (depth <= 24 & bottomtime <= 13) {
    RG <- "B"
  } else if (depth <= 24 & bottomtime <= 15) {
    RG <- "C"
  } else if (depth <= 24 & bottomtime <= 20) {
    RG <- "D"
  } else if (depth <= 24 & bottomtime <= 25) {
    RG <- "E"
  } else if (depth <= 24 & bottomtime > 25) {
    return(print("WARNING: DECOMPRESSION REQUIRED CHECK TABLE"))
  } else if (depth <= 27 & bottomtime <= 9) {
    RG <- "A"
  } else if (depth <= 27 & bottomtime <= 12) {
    RG <- "B"
  } else if (depth <= 27 & bottomtime <= 15) {
    RG <- "C"
  } else if (depth <= 27 & bottomtime <= 20) {
    RG <- "D"
  } else if (depth <= 27 & bottomtime > 20) {
    return(print("WARNING: BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth <= 30 & bottomtime <= 7) {
    RG <- "A"
  } else if (depth <= 30 & bottomtime <= 10) {
    RG <- "B"
  } else if (depth <= 30 & bottomtime <= 12) {
    RG <- "C"
  } else if (depth <= 30 & bottomtime <= 15) {
    RG <- "D"
  } else if (depth <= 30 & bottomtime > 15) {
    return(print("WARNING: BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth <= 33 & bottomtime <= 6) {
    RG <- "A"
  } else if (depth <= 33 & bottomtime <= 10) {
    RG <- "B"
  } else if (depth <= 33 & bottomtime <= 12) {
    RG <- "C"
  } else if (depth <= 33 & bottomtime > 12) {
    return(print("WARNING: BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth <= 36 & bottomtime <= 6) {
    RG <- "A"
  } else if (depth <= 36 & bottomtime <= 8) {
    RG <- "B"
  } else if (depth <= 36 & bottomtime <= 10) {
    RG <- "C"
  } else if (depth <= 36 & bottomtime > 10) {
    return(print("WARNING: BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth <= 39 & bottomtime <= 5) {
    RG <- "A"
  } else if (depth <= 39 & bottomtime <= 8) {
    RG <- "B"
  } else if (depth <= 39 & bottomtime > 8) {
    return(print("WARNING: BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth <= 42 & bottomtime <= 5) {
    RG <- "A"
  } else if (depth <= 42 & bottomtime <= 7) {
    RG <- "B"
  } else if (depth <= 42 & bottomtime > 7) {
    return(print("WARNING: BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth <= 45 & bottomtime <= 4) {
    RG <- "A"
  } else if (depth <= 45 & bottomtime <= 6) {
    RG <- "B"
  } else if (depth <= 45 & bottomtime > 6) {
    return(print("WARNING: BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth > 45 & bottomtime >= 0) {
    return(print("WARNING: BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  }

  return(RG)
}
