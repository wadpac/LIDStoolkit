#' add_bouts
#'
#' @param ts Time series dataframe produced by GGIR that has column window to indicate wakingup-wakingup window and column class_id, where 0 reflects sleep
#' @param sleepBinary See \link{detect_sleepbout}
#' @param wakeBoutThreshold See \link{detect_sleepbout}
#' @param wakeBoutMin See \link{detect_sleepbout}
#' @param sleepBoutMin See \link{detect_sleepbout}
#' @param maxLengthWake See \link{breakup_spt}
#' @param epochSize See \link{detect_sleepbout}
#' @param detection.method 1 for bout detection, 2 for SPT break up
#' @return Input data frame expanded with one column to indicate where the sleep bouts occur by a number which reflects the sleep bouw number per window
#' @export
# roxygen2::roxygenise()

# define custom add_sleepbout_column (as current one breaks if no bouts detected)
add_sleepbout_column = function(ts = c(), sleepBinary = c(), wakeBoutThreshold = 0.3,
                                wakeBoutMin = 10, sleepBoutMin = 180, epochSize = c(), maxLengthWake = 10, detection.method = 2) {
  if (length(epochSize) == 0) {
    stop("\nArgument epochSize not specified")
  }
  if (length(ts) == 0) {
    stop("\nArgument ts not specified")
  }
  sleepBinary = rep(0, nrow(ts))
  sleepBinary[which(ts$class_id == 0)] = 1
  if (detection.method == 1) {
    # Try to detect sleep bout based only on detected sustained inactivity bouts (van Hees 2015)
    bouts = detect_sleepbout(sleepBinary = sleepBinary, wakeBoutThreshold = wakeBoutThreshold,
                             wakeBoutMin = wakeBoutMin, sleepBoutMin = sleepBoutMin, epochSize = epochSize)
  } else if (detection.method == 2) {
    # Try to detect sleep bout based only on detected Sleep Period Time window (van Hees 2018), which is broken up in smaller parts
    brokenspt = breakup_spt(sleepBinary = sleepBinary, tsSPT = ts$SleepPeriodTime, wakeBoutThreshold = wakeBoutThreshold,
                            maxLengthWake  = maxLengthWake, sleepBoutMin = sleepBoutMin, epochSize = epochSize)
    if (nrow(ts) <= length(brokenspt)) {
      ts$SleepPeriodTime = brokenspt[1:nrow(ts)]
    } else {
      ts$SleepPeriodTime[1:length(brokenspt)] = brokenspt
    }
    bouts = extract_bouts(ts$SleepPeriodTime)
    
  }
  ts$sleepbouts = 0
  lastwindow = 0
  boutsexist = FALSE
  if (nrow(bouts) > 0) {
    sele = which(bouts$state == 1 | bouts$state == "sleep")
    if (length(sele) > 0) {
      boutsexist = TRUE
    }
  }
  if (boutsexist == TRUE) {
    bouts = bouts[sele, ]  
    for (j in 1:nrow(bouts)) {
      TB = table(ts$window[bouts$start[j]:bouts$end[j]])
      window = as.numeric(names(sort(-TB))[1])
      if (window != lastwindow) {
        boutnumber = 1
        lastwindow = window
      } else {
        boutnumber = boutnumber + 1
      }
      ts$sleepbouts[bouts$start[j]:bouts$end[j]] = boutnumber
    }
  }
  return(ts)
}
