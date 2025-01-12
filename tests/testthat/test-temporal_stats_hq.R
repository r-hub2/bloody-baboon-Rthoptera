# test-temporal_stats_hq.R

library(testthat)
library(tuneR)
library(RthopteraSounds)
library(Rthoptera)

# Load example data from RthopteraSounds
data("gryllus")
test_wave <- gryllus  # Assign loaded data to test_wave

test_that("temporal_stats_hq runs without errors", {
  result <- temporal_stats_hq(test_wave, specimen.id = "Test_Specimen")

  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_true("summary_data" %in% names(result))
  expect_true("train_data" %in% names(result))
  expect_true("motif_data" %in% names(result))
  expect_true("params" %in% names(result))
})


