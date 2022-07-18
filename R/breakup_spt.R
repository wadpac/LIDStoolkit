#' detect_sleepbout
#'
#' @param sleepBinary Numeric vector with for each epoch in time a 1 for sleep or a 0 for wakefulness
#' @param tsSPT Numeric vector with for each epoch in time a 1 for SPT or a 0 for wake window
#' @param wakeBoutThreshold Number between 0 and 1 being the allowed ratio of wakefulness per sleep bout
#' @param maxLengthWake Maximum duration of a wakefulness bout in minutes (wake shorter than this will be ignored)
#' @param sleepBoutMin Minimum duration of a sleep bout
#' @param epochSize Epoch size in seconds
#' @return Data.frame bouts, with for each bout start index, end index, duration, state being
#'  sleep or wake, and sleepRatio being the average ratio of sleep across all of the rolling 
#'  30 minute windows in a bout
#' @importFrom zoo rollmean
#' @export
# roxygen2::roxygenise()

breakup_spt = function(sleepBinary = c(), tsSPT = c(), wakeBoutThreshold = 0.3, 
                       maxLengthWake = 10, sleepBoutMin = 180, epochSize = 5) {
  
  # Smooth wake/sleep signal to turn segments with a lot of wakefulness to consistent wakefulness
  sleepRatio = zoo::rollmean(x = sleepBinary, k = (30 * (60 / epochSize)), fill = "extend", align = "center")
  wake_periods = which(sleepRatio < wakeBoutThreshold)
  if (length(wake_periods) > 0)  {
    sleepBinary[wake_periods] = 0
  }
 
  # Break up SPT segments when there is too much wake
  wbouts = extract_bouts(sleepBinary)
  wbouts = wbouts[which(wbouts$state == 0),]
  for (j in 1:nrow(wbouts)) {
    if (wbouts$dur[j] > maxLengthWake * (60/epochSize)) {
      tsSPT[wbouts$start[j]:wbouts$end[j]] = 0
    }
  }
  # Keep SPT segments longer than sleepBoutMin
  sbouts = extract_bouts(tsSPT)
  sbouts = sbouts[which(sbouts$state == 1),]
  for (j in 1:nrow(sbouts)) {
    if (sbouts$dur[j] < sleepBoutMin * (60/epochSize)) {
      tsSPT[sbouts$start[j]:sbouts$end[j]] = 0
    }
  }
  return(tsSPT)
} 