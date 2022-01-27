library(LIDStoolkit)
context("detect_sleepbout")
test_that("Can detect 1 sleep bout short wake in the middle", {
  sleepBinary = c(rep(0, 60 * 2), rep(1, 60 * 4), rep(0, 5), rep(1, 60 * 4), rep(0, 60 * 3))
  bouts = detect_sleepbout(sleepBinary = sleepBinary, wakeBoutThreshold = 0.3, epochSize = 60)
  expect_equal(nrow(bouts), 2)
  expect_equal(bouts$dur, c(485, 180))
})

test_that("Can detect 1 sleep bout and ignore other sleep bouts with too much wake", {
  sleepBinary = c(rep(0, 60 * 2), rep(1, 60 * 4), rep(0, 5), rep(1, 60 * 4), 
                  rep(c(rep(0, 9), rep(1, 11)), times = 4), rep(0, 60 * 3))
  bouts = detect_sleepbout(sleepBinary = sleepBinary, wakeBoutThreshold = 0.3, wakeBoutMin = 10, epochSize = 60)
  expect_equal(nrow(bouts), 2)
  expect_equal(bouts$dur, c(485, 180))
})
test_that("Can detect 3 sleep bouts", {
  sleepBinary = c(rep(0, 60 * 2), rep(1, 60 * 4), rep(0, 45),
                                  rep(1, 60 * 4), rep(0, 45),
                                  rep(1, 60 * 4), rep(0, 60 * 2))
  bouts = detect_sleepbout(sleepBinary = sleepBinary, epochSize = 60)
  expect_equal(nrow(bouts), 6)
  expect_equal(sum(bouts$dur), 930)
})
test_that("Can handle absence of sleep", {
  sleepBinary = c(rep(0, 60 * 8))
  bouts = detect_sleepbout(sleepBinary = sleepBinary, epochSize = 60)
  expect_equal(nrow(bouts), 0)
})
test_that("Can handle absence of wake", {
  sleepBinary = c(rep(1, 60 * 8))
  bouts = detect_sleepbout(sleepBinary = sleepBinary, epochSize = 60)
  expect_equal(nrow(bouts), 1)
  expect_equal(ncol(bouts), 5)
})

test_that("Can handle too short time series", {
  sleepBinary = 1
  bouts = detect_sleepbout(sleepBinary = sleepBinary, epochSize = 60)
  expect_equal(nrow(bouts), 0)
})
