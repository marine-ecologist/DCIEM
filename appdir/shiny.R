library(lubridate)
library(tidyverse)

#-----------------------------------------------------------------------------
# function to calculate Repetitive Group (RG) for DCIEM tables
get_RG <- function(depth = NULL, bottomtime = NULL, RF = 1, nitrox = 21) {
  
  depth <- EAD(nitrox = nitrox, depth = depth)
  
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
    RG <- "G"
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
    return(print("BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth <= 30 & bottomtime <= 7) {
    RG <- "A"
  } else if (depth <= 30 & bottomtime <= 10) {
    RG <- "B"
  } else if (depth <= 30 & bottomtime <= 12) {
    RG <- "C"
  } else if (depth <= 30 & bottomtime <= 15) {
    RG <- "D"
  } else if (depth <= 30 & bottomtime > 15) {
    return(print("BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth <= 33 & bottomtime <= 6) {
    RG <- "A"
  } else if (depth <= 33 & bottomtime <= 10) {
    RG <- "B"
  } else if (depth <= 33 & bottomtime <= 12) {
    RG <- "C"
  } else if (depth <= 33 & bottomtime > 12) {
    return(print("BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth <= 36 & bottomtime <= 6) {
    RG <- "A"
  } else if (depth <= 36 & bottomtime <= 8) {
    RG <- "B"
  } else if (depth <= 36 & bottomtime <= 10) {
    RG <- "C"
  } else if (depth <= 36 & bottomtime > 10) {
    return(print("BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth <= 39 & bottomtime <= 5) {
    RG <- "A"
  } else if (depth <= 39 & bottomtime <= 8) {
    RG <- "B"
  } else if (depth <= 39 & bottomtime > 8) {
    return(print("BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth <= 42 & bottomtime <= 5) {
    RG <- "A"
  } else if (depth <= 42 & bottomtime <= 7) {
    RG <- "B"
  } else if (depth <= 42 & bottomtime > 7) {
    return(print("BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth <= 45 & bottomtime <= 4) {
    RG <- "A"
  } else if (depth <= 45 & bottomtime <= 6) {
    RG <- "B"
  } else if (depth <= 45 & bottomtime > 6) {
    return(print("BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  } else if (depth > 45 & bottomtime >= 0) {
    return(print("BEYOND NO DECO LIMITS, CONSULT DCIEM TABLE"))
  }
  
  return(RG)
}

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

#-----------------------------------------------------------------------------
# Create a sequence for each minute of the day
minutes_of_day <- 0:(24 * 60 - 1)
times <- format(strptime(minutes_of_day * 60, format = "%s"), "%H:%M")

#-----------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      $(document).on('shiny:connected', function(event) {
        $('#date').attr('readonly', true);
      });
    ")),
    tags$style(HTML("
      .border-right {
        border-right: 1px solid #ddd;
      }
      .border-left {
        border-left: 1px solid #ddd;
      }
      body, .container-fluid {
        min-width: 900px;
        max-width: 900px;
        margin-right: auto;
        margin-left: auto;
      }
      .input-box {
        border: 1px solid #ddd;
        padding: 6px;
        border-radius: 5px;
        margin-bottom: 10px;
        background-color: #f9f9f9;
      }
      .input-box .col-sm-2, .input-box .col-sm-1 {
        min-width: 100px;
        max-width: 120px;
      }
    "))
  ),
  titlePanel("dciem dive log"),
  div(class = "input-box",
      fluidRow(
        column(2, dateInput("date", "Date")),
        column(2, selectInput("timeIn", "Time In", choices = times)),
        column(2, selectInput("timeOut", "Time Out", choices = times)),
#        column(2, textInput("timeIn", "Time In", value = "12:00", placeholder = "HH:MM")),
#        column(2, textInput("timeOut", "Time Out", value = "12:05", placeholder = "HH:MM")),
        column(1, numericInput("TotalTime", "TotalTime", value = 0 )),
        column(1, numericInput("depth", "Depth", value = 0 )),
        column(2, numericInput("airIn", "Air in", value = 200) ),
        column(2, numericInput("airOut", "Air Out", value = 100 )),
      ),
      fluidRow(
        column(12, actionButton("addBtn", "Add Entry"))
      )
  ),
  div(class = "output-box",
      DTOutput("table")
  )
)

#-----------------------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(data = data.frame(Date = as.Date(character()), 
                                         TimeIn = character(),
                                         AirIn = numeric(), 
                                         Time = numeric(), 
                                         Depth = numeric(), 
                                         TimeOut = character(),
                                         AirOut = numeric(),
                                         TotalTime = numeric(), 
                                         RG = numeric(), 
                                         RG2 = character(),
                                         SI = character(),
                                         RF = numeric(), 
                                         stringsAsFactors = FALSE))
  
  observeEvent(input$addBtn, {
    # Combine Date and Time and convert to POSIXct
    dateTimeIn <- ymd_hm(paste(input$date, input$timeIn))
    dateTimeOut <- ymd_hm(paste(input$date, input$timeIn))
    
    if (nrow(rv$data) > 0) {
     
      # Calculate SI
      lastDateTimeOut <- ymd_hm(paste(rv$data$Date[nrow(rv$data)], rv$data$TimeOut[nrow(rv$data)]))
      diff_in_seconds <- as.numeric(difftime(dateTimeIn, lastDateTimeOut, units = "secs")) # Calculate the difference in seconds and then in hours and minutes
      hours <- diff_in_seconds %/% 3600
      minutes <- (diff_in_seconds %% 3600) / 60
      minutes2 <- hours*60 + minutes
      SIval <- sprintf("%02d:%02d", hours, minutes) # Format SIval
    } else {
      SIval <- NA # Set SI to 24:00 if there's no previous row
      minutes2 <- 24*60
    }
    
    # Calculate Time
    if (nrow(rv$data) > 0) {
      lastRF <- rv$data$RF[nrow(rv$data)]
      Timeval <- ceiling(lastRF * input$TotalTime) # NOTE always rounds up
    } else {
      Timeval <- input$TotalTime
    }
    
    # Function to increment a letter alphabetically
    increment_letter <- function(letter) {
      if (letter == "" || is.na(letter) || letter == "Z" || letter == "z") {
        return("")
      } else {
        return(LETTERS[which(LETTERS == toupper(letter)) + 1])
      }
    }
    
    # Calculate RG2 based on the last RG value
    RG2val <- if (nrow(rv$data) > 0) {
      increment_letter(rv$data$RG[nrow(rv$data)])
    } else {
      ""
    }
    
    
    # Create new entry
    newEntry <- data.frame(Date = input$date, 
                           TimeIn = format(dateTimeIn, "%H:%M"),
                           AirIn = input$airIn, 
                           Time = Timeval,
                           Depth = input$depth, 
                           TimeOut = format(ymd_hm(paste(input$date, input$timeOut)), "%H:%M"),
                           AirOut = input$airOut,
                           TotalTime = input$TotalTime, 
                           RG = get_RG(input$depth, input$TotalTime),
                           RG2 = RG2val,
                           SI = SIval, 
                           RF = get_RF(get_RG(input$depth, input$TotalTime), minutes2)
                           )
 
    
    # Add new entry to data
    rv$data <- rbind(rv$data, newEntry)
    
  })
  
  output$table <- renderDT({
    datatable(rv$data, 
              options = list(pageLength = 100, searching = FALSE), 
              editable = list(target = 'cell', disable = list(columns = c(8)))) %>%
      formatStyle('AirOut', 
                  backgroundColor = styleInterval(c(30, 50), c('red', 'orange', 'white'))) 
     
  })
  
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    str(info)
    rv$data[info$row, info$col] <<- DT::coerceValue(info$value, rv$data[info$row, info$col])
    
    # Update RG if Depth or TotalTime is edited
    if (info$col %in% c("Depth", "TotalTime")) { # Assuming 'Depth' is 4th and 'TotalTime' is 5th column
      rv$data[info$row, 'RG'] <<- get_RG(rv$data[info$row, 'Depth'], rv$data[info$row, 'TotalTime'])
    }
  })
}

shinyApp(ui, server)
options(shiny.launch.browser = .rs.invokeShinyWindowExternal)

