#' extract_bouts 
#'
#' @param x Numeric vector with for each epoch in time a 1 a 0
#' @return Data.frame bouts, with for each bout start index, end index, duration, state being
#'  1 or 2
#' @importFrom zoo rollmean
#' @export
# roxygen2::roxygenise()

# Function to extract bout start and end-points, and duration
extract_bouts = function(x) {
  x = c(1, 0, x, 0, 1)
  changepoints = which(abs(diff(x)) == 1) - 2 # where does value change?
  Nbouts = length(changepoints) - 1
  bouts = data.frame(start = numeric(Nbouts), end = numeric(Nbouts), 
                     dur = numeric(Nbouts), state = numeric(Nbouts)) 
  bouts$state = rep(c(0, 1), ceiling(Nbouts/2))[1:Nbouts]
  bouts$start = changepoints[1:(length(changepoints) - 1)]
  bouts$end = changepoints[2:length(changepoints)]
  bouts$start[1] = bouts$start[1] + 1
  bouts$start = bouts$start  + 1
  bouts$dur = (bouts$end - bouts$start) + 1
  bouts$remove = FALSE
  bouts$dur = (bouts$end - bouts$start) + 1
  return(bouts)
}