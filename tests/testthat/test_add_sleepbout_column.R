library(LIDStoolkit)
context("add_sleepbout_column")
test_that("Generates a sleepbouts column with expected frequency of values", {
  N = 1440 * 7
  ts = data.frame(class_id = numeric(N), window = numeric(N))
  ts$class_id = rep(c(rep(10, 60 * 7), rep(0, 60 * 4), rep(10, 60 * 1), rep(0, 60 * 4),rep(10, 60 * 8)), times = 7)
  ts$window = rep(1:7, each = 1440)
  ts_new = add_sleepbout_column(ts = ts, epochSize = 60) 
  expect_equal(as.numeric(table(ts_new$sleepbouts)), c(6720, 1680, 1680))
})