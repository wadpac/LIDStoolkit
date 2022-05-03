#' detect_sleepbout
#'
#' @param sleepBinary Numeric vector with for each epoch in time a 1 for sleep or a 0 for wakefulness
#' @param wakeBoutThreshold Number between 0 and 1 being the allowed ratio of wakefulness per sleep bout
#' @param wakeBoutMin Maximum duration of a wakefulness bout
#' @param sleepBoutMin Minimum duration of a sleep bout
#' @param epochSize Epoch size in seconds
#' @return Data.frame bouts, with for each bout start index, end index, duration, state being
#'  sleep or wake, and sleepRatio being the average ratio of sleep across all of the rolling 
#'  30 minute windows in a bout
#' @importFrom zoo rollmean
#' @export
# roxygen2::roxygenise()

detect_sleepbout = function(sleepBinary = c(), wakeBoutThreshold = 0.3, 
                            wakeBoutMin = 10, sleepBoutMin = 180, epochSize = 5) {
  # Following two lines are not commented out, because changing sleepRatio definition
  # # Use 30 minute rolling average to later use for identifying proportion of wakefulness per 30 minutes
  # sleepRatio = zoo::rollmean(x = sleepBinary, k = (30 * (60 / epochSize)), fill = "extend", align = "center")
  
  sleepBinary = c(1, 0, sleepBinary, rep(0, wakeBoutMin * 10 * (60 / epochSize)), 0, 1)
  # If there are sleep bouts then list these in a data.frame and merge or remove based on criteria
  if (length(which(sleepBinary == 1)) > (sleepBoutMin) * (60 / epochSize)) { # We are interested in the Bouts with at least sleepBoutMin minutes
    changepoints = which(abs(diff(sleepBinary)) == 1) - 2 # where does value change?
    Nbouts = length(changepoints) - 1
    bouts = data.frame(start = numeric(Nbouts), end = numeric(Nbouts), 
                       dur = numeric(Nbouts), state = numeric(Nbouts), sleepRatio = numeric(Nbouts)) 
    bouts$state = rep(c(0, 1), ceiling(Nbouts/2))[1:Nbouts]
    bouts$start = changepoints[1:(length(changepoints) - 1)]
    bouts$end = changepoints[2:length(changepoints)]
    bouts$start[1] = bouts$start[1] + 1
    bouts$start = bouts$start  + 1
    bouts$dur = (bouts$end - bouts$start) + 1
    bouts$remove = FALSE
    # identify too short wake bouts
    tooshort = which(bouts$state == 0 & bouts$dur < (wakeBoutMin * (60 / epochSize)))
    if (length(tooshort) > 0) {
      # remove too short wake bouts
      for (i in tooshort) {
        if (i == 1 | i == nrow(bouts)) {
          bouts$remove[i] = TRUE
        } else {
          bouts$end[i - 1] = bouts$end[i + 1]
          bouts$remove[i:(i + 1)] = TRUE
        }
      }
      bouts = bouts[which(bouts$remove == FALSE),]
      if (nrow(bouts) > 1) {
        # merge resulting adjacent sleep bouts, if any
        for (i in 1:(nrow(bouts) - 1)) {
          if (bouts$state[i] == bouts$state[i + 1]) {
            bouts$end[i] = bouts$end[i + 1]
            bouts$remove[i + 1] = TRUE
          }
        }
        bouts = bouts[which(bouts$remove == FALSE),]
      }
    }
    bouts$dur = (bouts$end - bouts$start) + 1
    
    for (j in 1:nrow(bouts)) {
    #   if (length(is.na(sleepRatio[bouts$start[j]:bouts$end[j]]) == TRUE)) {
    #     bouts$sleepRatio[j] = mean(sleepRatio[bouts$start[j]:bouts$end[j]], na.rm = TRUE)
    #   } else {
    #     bouts$sleepRatio[j] = 0
    #   }
      # If we would like to use the 30% criteria at bout level then we would use instead
      bouts$sleepRatio[j] = mean(sleepBinary[(bouts$start[j] + 2):(bouts$end[j]+ 2)], na.rm = TRUE)
    }
    # remove sleep bouts that are either too short or have too much wakefulness and remove the wake bout that follows them
    shortsleep = which(bouts$state == 1 & 
                         (bouts$dur < (sleepBoutMin * (60 / epochSize)) | 
                            bouts$sleepRatio < (1 - wakeBoutThreshold)))
    if (length(shortsleep) > 0) {
      bouts = bouts[-c(shortsleep, shortsleep + 1),]
    }
    if (nrow(bouts) > 0) {
      bouts$end[nrow(bouts)] = bouts$end[nrow(bouts)] - (wakeBoutMin * 10 * (60 / epochSize)) - 1
      bouts$dur = (bouts$end - bouts$start) + 1
      if (bouts$dur[nrow(bouts)] <= 0) {
        # sleep was not actually followed by wakefullness
        bouts$remove[nrow(bouts)] = TRUE
      }
      if (bouts$state[1] == 0) {
        # remove trailing wake period
        bouts$remove[1] = TRUE
      }
    }
    bouts = bouts[which(bouts$remove == FALSE),]
    bouts = bouts[, !(names(bouts) %in% "remove")]
    bouts$state = factor(bouts$state , levels = c(0, 1), 
                         labels = c("wake", "sleep"))
  } else {
    bouts = data.frame(start = numeric(0), end = numeric(0), 
                       dur = numeric(0), state = numeric(0), sleepRatio = numeric(0))
  }
  return(bouts)
} 