#' Function to calculate Repetitive Factors (RF) for DCIEM tables
#'
#' @param RG Repetitive Group
#' @param surfaceinterval Surface Interval (minutes)
#' @returns A numeric vector.
#' @export
#' @examples
#'
#' get_RF(RG = "A", surfaceinterval = 50)
#' get_RF("C", 16)
#'


#-----------------------------------------------------------------------------
# function to calculate Repetitive Factor (RF) for DCIEM tables
get_RF <- function(RG = NULL, surfaceinterval = NULL) {

  if (RG == "A" & surfaceinterval <= 15) {
    return(print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "A" & surfaceinterval >= 15 & surfaceinterval < 30) {
    SI <- 1.4
  } else if (RG == "A" & surfaceinterval >= 30 & surfaceinterval < 60) {
    SI <- 1.2
  } else if (RG == "A" & surfaceinterval >= 60 & surfaceinterval < 90) {
    SI <- 1.1
  } else if (RG == "A" & surfaceinterval >= 90 & surfaceinterval < 120) {
    SI <- 1.1
  } else if (RG == "A" & surfaceinterval >= 120 & surfaceinterval < 180) {
    SI <- 1.1
  } else if (RG == "A" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.1
  } else if (RG == "A" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.1
  } else if (RG == "A" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.1
  } else if (RG == "A" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.1
  } else if (RG == "A" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1.0
  } else if (RG == "A" & surfaceinterval >= 900) {
    SI <- 1
  } else if (RG == "B" & surfaceinterval <= 15) {
    return(print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "B" & surfaceinterval >= 15 & surfaceinterval < 30) {
    SI <- 1.5
  } else if (RG == "B" & surfaceinterval >= 30 & surfaceinterval < 60) {
    SI <- 1.3
  } else if (RG == "B" & surfaceinterval >= 60 & surfaceinterval < 90) {
    SI <- 1.2
  } else if (RG == "B" & surfaceinterval >= 90 & surfaceinterval < 120) {
    SI <- 1.2
  } else if (RG == "B" & surfaceinterval >= 120 & surfaceinterval < 180) {
    SI <- 1.2
  } else if (RG == "B" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.1
  } else if (RG == "B" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.1
  } else if (RG == "B" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.1
  } else if (RG == "B" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.1
  } else if (RG == "B" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1.1
  } else if (RG == "B" & surfaceinterval >= 900) {
    SI <- 1
  } else if (RG == "C" & surfaceinterval <= 15) {
    return(print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "C" & surfaceinterval >= 15 & surfaceinterval < 30) {
    SI <- 1.6
  } else if (RG == "C" & surfaceinterval >= 30 & surfaceinterval < 60) {
    SI <- 1.4
  } else if (RG == "C" & surfaceinterval >= 60 & surfaceinterval < 90) {
    SI <- 1.3
  } else if (RG == "C" & surfaceinterval >= 90 & surfaceinterval < 120) {
    SI <- 1.2
  } else if (RG == "C" & surfaceinterval >= 120 & surfaceinterval < 180) {
    SI <- 1.2
  } else if (RG == "C" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.2
  } else if (RG == "C" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.1
  } else if (RG == "C" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.1
  } else if (RG == "C" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.1
  } else if (RG == "C" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1.1
  } else if (RG == "C" & surfaceinterval >= 900) {
    SI <- 1
  } else if (RG == "D" & surfaceinterval <= 15) {
    return(print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "D" & surfaceinterval >= 15 & surfaceinterval < 30) {
    SI <- 1.8
  } else if (RG == "D" & surfaceinterval >= 30 & surfaceinterval < 60) {
    SI <- 1.5
  } else if (RG == "D" & surfaceinterval >= 60 & surfaceinterval < 90) {
    SI <- 1.4
  } else if (RG == "D" & surfaceinterval >= 90 & surfaceinterval < 120) {
    SI <- 1.3
  } else if (RG == "D" & surfaceinterval >= 120 & surfaceinterval < 180) {
    SI <- 1.3
  } else if (RG == "D" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.2
  } else if (RG == "D" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.2
  } else if (RG == "D" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.1
  } else if (RG == "D" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.1
  } else if (RG == "D" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1
  } else if (RG == "D" & surfaceinterval >= 900) {
    SI <- 1
  } else if (RG == "E" & surfaceinterval <= 15) {
    return(print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "E" & surfaceinterval >= 15 & surfaceinterval < 30) {
    SI <- 1.9
  } else if (RG == "E" & surfaceinterval >= 30 & surfaceinterval < 60) {
    SI <- 1.6
  } else if (RG == "E" & surfaceinterval >= 60 & surfaceinterval < 90) {
    SI <- 1.5
  } else if (RG == "E" & surfaceinterval >= 90 & surfaceinterval < 120) {
    SI <- 1.4
  } else if (RG == "E" & surfaceinterval >= 120 & surfaceinterval < 180) {
    SI <- 1.3
  } else if (RG == "E" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.4
  } else if (RG == "E" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.2
  } else if (RG == "E" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.2
  } else if (RG == "E" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.1
  } else if (RG == "E" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1.1
  } else if (RG == "E" & surfaceinterval >= 900) {
    SI <- 1
  } else if (RG == "F" & surfaceinterval <= 15) {
    return( print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "F" & surfaceinterval >= 15 & surfaceinterval < 30) {
    SI <- 2
  } else if (RG == "F" & surfaceinterval >= 30 & surfaceinterval < 60) {
    SI <- 1.7
  } else if (RG == "F" & surfaceinterval >= 60 & surfaceinterval < 90) {
    SI <- 1.6
  } else if (RG == "F" & surfaceinterval >= 90 & surfaceinterval < 120) {
    SI <- 1.5
  } else if (RG == "F" & surfaceinterval >= 120 & surfaceinterval < 180) {
    SI <- 1.4
  } else if (RG == "F" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.3
  } else if (RG == "F" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.3
  } else if (RG == "F" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.2
  } else if (RG == "F" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.1
  } else if (RG == "F" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1.1
  } else if (RG == "F" & surfaceinterval >= 900) {
    SI <- 1
  } else if (RG == "G" & surfaceinterval <= 15) {
    return(print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "G" & surfaceinterval >= 15 & surfaceinterval < 30) {
    return(print("less than 30 minute required surface interval, combine dives"))
  } else if (RG == "G" & surfaceinterval >= 30 & surfaceinterval < 60) {
    SI <- 1.9
  } else if (RG == "G" & surfaceinterval >= 60 & surfaceinterval < 90) {
    SI <- 1.7
  } else if (RG == "G" & surfaceinterval >= 90 & surfaceinterval < 120) {
    SI <- 1.6
  } else if (RG == "G" & surfaceinterval >= 120 & surfaceinterval < 180) {
    SI <- 1.5
  } else if (RG == "G" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.4
  } else if (RG == "G" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.3
  } else if (RG == "G" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.2
  } else if (RG == "G" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.1
  } else if (RG == "G" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1.1
  } else if (RG == "G" & surfaceinterval >= 900) {
    SI <- 1
  } else if (RG == "H" & surfaceinterval <= 15) {
    return(print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "H" & surfaceinterval >= 15 & surfaceinterval < 30) {
    return(print("less than 30 minute required surface interval, combine dives"))
  } else if (RG == "H" & surfaceinterval >= 30 & surfaceinterval < 60) {
    return(print("less than 60 minute required surface interval, combine dives"))
  } else if (RG == "H" & surfaceinterval >= 60 & surfaceinterval < 90) {
    SI <- 1.9
  } else if (RG == "H" & surfaceinterval >= 90 & surfaceinterval < 120) {
    SI <- 1.7
  } else if (RG == "H" & surfaceinterval >= 120 & surfaceinterval < 180) {
    SI <- 1.6
  } else if (RG == "H" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.5
  } else if (RG == "H" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.4
  } else if (RG == "H" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.3
  } else if (RG == "H" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.1
  } else if (RG == "H" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1.1
  } else if (RG == "H" & surfaceinterval >= 900) {
    SI <- 1.1
  } else if (RG == "I" & surfaceinterval <= 15) {
    return(print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "I" & surfaceinterval >= 15 & surfaceinterval < 30) {
    return( print("less than 30 minute required surface interval, combine dives"))
  } else if (RG == "I" & surfaceinterval >= 30 & surfaceinterval < 60) {
    return(print("less than 60 minute required surface interval, combine dives"))
  } else if (RG == "I" & surfaceinterval >= 60 & surfaceinterval < 90) {
    SI <- 2
  } else if (RG == "I" & surfaceinterval >= 90 & surfaceinterval < 120) {
    SI <- 1.8
  } else if (RG == "I" & surfaceinterval >= 120 & surfaceinterval < 180) {
    SI <- 1.5
  } else if (RG == "I" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.4
  } else if (RG == "I" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.3
  } else if (RG == "I" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.1
  } else if (RG == "I" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.1
  } else if (RG == "I" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1.1
  } else if (RG == "I" & surfaceinterval >= 900) {
    SI <- 1.1
  } else if (RG == "J" & surfaceinterval <= 15) {
    return(print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "J" & surfaceinterval >= 15 & surfaceinterval < 30) {
    return(print("less than 30 minute required surface interval, combine dives"))
  } else if (RG == "J" & surfaceinterval >= 30 & surfaceinterval < 60) {
    return(print("less than 60 minute required surface interval, combine dives"))
  } else if (RG == "J" & surfaceinterval >= 60 & surfaceinterval < 90) {
    return(print("less than 90 minute required surface interval, combine dives"))
  } else if (RG == "J" & surfaceinterval >= 90 & surfaceinterval < 120) {
    SI <- 1.9
  } else if (RG == "J" & surfaceinterval >= 120 & surfaceinterval < 180) {
    SI <- 1.8
  } else if (RG == "J" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.6
  } else if (RG == "J" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.5
  } else if (RG == "J" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.3
  } else if (RG == "J" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.2
  } else if (RG == "J" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1.1
  } else if (RG == "J" & surfaceinterval >= 900) {
    SI <- 1.1
  } else if (RG == "K" & surfaceinterval <= 15) {
    return(print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "K" & surfaceinterval >= 15 & surfaceinterval < 30) {
    return(print("less than 30 minute required surface interval, combine dives"))
  } else if (RG == "K" & surfaceinterval >= 30 & surfaceinterval < 60) {
    return(print("less than 60 minute required surface interval, combine dives"))
  } else if (RG == "K" & surfaceinterval >= 60 & surfaceinterval < 90) {
    return(print("less than 90 minute required surface interval, combine dives"))
  } else if (RG == "K" & surfaceinterval >= 90 & surfaceinterval < 120) {
    SI <- 2
  } else if (RG == "K" & surfaceinterval >= 120 & surfaceinterval < 180) {
    SI <- 1.9
  } else if (RG == "K" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.7
  } else if (RG == "K" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.5
  } else if (RG == "K" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.3
  } else if (RG == "K" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.2
  } else if (RG == "K" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1.1
  } else if (RG == "K" & surfaceinterval >= 900) {
    SI <- 1.1
  } else if (RG == "L" & surfaceinterval <= 15) {
    return(print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "L" & surfaceinterval >= 15 & surfaceinterval < 30) {
    return(print("less than 30 minute required surface interval, combine dives"))
  } else if (RG == "L" & surfaceinterval >= 30 & surfaceinterval < 60) {
    return(print("less than 60 minute required surface interval, combine dives"))
  } else if (RG == "L" & surfaceinterval >= 60 & surfaceinterval < 90) {
    return(print("less than 90 minute required surface interval, combine dives"))
  } else if (RG == "L" & surfaceinterval >= 90 & surfaceinterval < 120) {
    SI <- 2
  } else if (RG == "L" & surfaceinterval >= 120 & surfaceinterval < 180) {
    SI <- 1.9
  } else if (RG == "L" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.7
  } else if (RG == "L" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.5
  } else if (RG == "L" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.3
  } else if (RG == "L" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.2
  } else if (RG == "L" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1.1
  } else if (RG == "L" & surfaceinterval >= 900) {
    SI <- 1.1
  } else if (RG == "M" & surfaceinterval <= 15) {
    return(print("less than 15 minute surface interval, combine dives"))
  } else if (RG == "M" & surfaceinterval >= 15 & surfaceinterval < 30) {
    return(print("less than 30 minute required surface interval, combine dives"))
  } else if (RG == "M" & surfaceinterval >= 30 & surfaceinterval < 60) {
    return(print("less than 50 minute required surface interval, combine dives"))
  } else if (RG == "M" & surfaceinterval >= 60 & surfaceinterval < 90) {
    return(print("less than 90 minute required surface interval, combine dives"))
  } else if (RG == "M" & surfaceinterval >= 90 & surfaceinterval < 120) {
    return(print("less than 2 hr required surface interval, combine dives"))
  } else if (RG == "M" & surfaceinterval >= 120 & surfaceinterval < 180) {
    return(print("less than 3 hr required surface interval, combine dives"))
  } else if (RG == "M" & surfaceinterval >= 180 & surfaceinterval < 240) {
    SI <- 1.8
  } else if (RG == "M" & surfaceinterval >= 240 & surfaceinterval < 360) {
    SI <- 1.6
  } else if (RG == "M" & surfaceinterval >= 360 & surfaceinterval < 540) {
    SI <- 1.3
  } else if (RG == "M" & surfaceinterval >= 540 & surfaceinterval < 720) {
    SI <- 1.2
  } else if (RG == "M" & surfaceinterval >= 720 & surfaceinterval < 900) {
    SI <- 1.1
  } else if (RG == "M" & surfaceinterval >= 900) {
    SI <- 1.1
  }
  return(SI)
}
