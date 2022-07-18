library(LIDStoolkit)
context("breakup_spt")
test_that("Can break up SPT correctly", {
  # SPT with short break that does not meet criteria
  sleepBinary = c(rep(0, 60 * 2), rep(1, 60 * 4), rep(0, 5), rep(1, 60 * 4), rep(0, 60 * 3))
  tsSPT = rep(1, length(sleepBinary))
  newspt = breakup_spt(sleepBinary = sleepBinary, tsSPT = tsSPT, wakeBoutThreshold = 0.3, 
                         maxLengthWake = 10, sleepBoutMin = 180, epochSize = 60)
  Nsleep = length(which(newspt == 1))
  Nbouts = length(which(diff(newspt) == 1))
  expect_equal(Nsleep, 485)
  expect_equal(Nbouts, 1)
  # SPT with reak that does meets criteria
  sleepBinary = c(rep(0, 60 * 2), rep(1, 60 * 4), rep(0, 15), rep(1, 60 * 4), rep(0, 60 * 3))
  tsSPT = rep(1, length(sleepBinary))
  newspt = breakup_spt(sleepBinary = sleepBinary, tsSPT = tsSPT, wakeBoutThreshold = 0.3, 
                       maxLengthWake = 10, sleepBoutMin = 180, epochSize = 60)
  Nsleep = length(which(newspt == 1))
  Nbouts = length(which(diff(newspt) == 1))
  expect_equal(Nsleep, 480)
  expect_equal(Nbouts, 2)
})
