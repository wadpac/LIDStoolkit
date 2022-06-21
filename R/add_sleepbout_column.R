#' add_bouts
#'
#' @param ts Time series dataframe produced by GGIR that has column window to indicate wakingup-wakingup window and column class_id, where 0 reflects sleep
#' @param sleepBinary See \link{detect_sleepbout}
#' @param wakeBoutThreshold See \link{detect_sleepbout}
#' @param wakeBoutMin See \link{detect_sleepbout}
#' @param sleepBoutMin See \link{detect_sleepbout}
#' @param epochSize See \link{detect_sleepbout}
#' @return Input data frame expanded with one column to indicate where the sleep bouts occur by a number which reflects the sleep bouw number per window
#' @export
# roxygen2::roxygenise()

# define custom add_sleepbout_column (as current one breaks if no bouts detected)
add_sleepbout_column = function(ts = c(), sleepBinary = c(), wakeBoutThreshold = 0.3,
                                wakeBoutMin = 10, sleepBoutMin = 180, epochSize = c()) {
  if (length(epochSize) == 0) {
    stop("\nArgument epochSize not specified")
  }
  if (length(ts) == 0) {
    stop("\nArgument ts not specified")
  }
  sleepBinary = rep(0, nrow(ts))
  sleepBinary[which(ts$class_id == 0)] = 1
  bouts = detect_sleepbout(sleepBinary = sleepBinary, wakeBoutThreshold = wakeBoutThreshold,
                           wakeBoutMin = wakeBoutMin, sleepBoutMin = sleepBoutMin, epochSize = epochSize)
  ts$sleepbouts = 0
  lastwindow = 0
  # custom fix 1 -> only add information to the column if there is at least one sleep bout identified
  if (sum(bouts$state == "sleep") > 0) {
    for (j in 1:nrow(bouts)) {
      
      if (bouts$state[j] == "sleep") {
        window = as.numeric(names(sort(-table(ts$window[bouts$start[j]:bouts$end[j]])))[1])
        # custom fix 2 -> if window=0 and boutnumber is not defined, it breaks the script
        # so add extra condition to catch if boutnumber not defined
        if (window != lastwindow) {
          boutnumber = 1
          lastwindow = window
        } else {
          boutnumber = boutnumber + 1
        }
        ts$sleepbouts[bouts$start[j]:bouts$end[j]] = boutnumber
      }
    }
  }
  return(ts)
}
