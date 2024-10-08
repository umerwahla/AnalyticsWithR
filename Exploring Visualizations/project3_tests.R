#project3_tests
# While variables were not specified in the problem instructions
# Tests will reference the problem number and expected variable name
# Much of this project focuses on visualizations which are not tested
# by this script.

library(testthat)
test_that("Checking books data frame. Checking Number of Rows.", {
  expect_equal(nrow(books), 20116)
})

test_that("Checking books data frame. Checking Number of Columns", {
  expect_equal(ncol(books), 17)
})

test_that("Checking books data frame. Checking year column.", {
  expect_equal(books$year[5], 1997)
})

test_that("Checking books data frame. Checking year column.", {
  expect_equal(books$year[1], 2005)
})

test_that("Checking books data frame. Checking for no large books.", {
  expect_equal(sum(books$pages[books$pages>=1200]), 0)
})

test_that("Checking books data frame. Checking for 1990 first year.", {
  expect_equal(min(books$year), 1990)
})
test_that("Checking books data frame. Checking for 2020 last year.", {
  expect_equal(max(books$year), 2020)
})

test_that("Checking book_publishers data frame. Checking Random House total_books", {
  expect_equal(book_publishers$total_books[1], 2607)
})

test_that("Checking book_publishers data frame. Checking Harper Collins cum_count", {
  expect_equal(book_publishers$cum_count[2], 4119)
})

test_that("Checking book_publishers data frame. Checking Simon rel_freq", {
  expect_equal(book_publishers$rel_freq[5], .0938, tolerance = 1e-3)
})

test_that("Checking book_publishers data frame. Checking MacMillan cum_freq", {
  expect_equal(book_publishers$cum_freq[3], .742, tolerance = 1e-3)
})


test_that("Checking by_year data frame. Checking 1990 total_books", {
  expect_equal(by_year$total_books[1], 340, tolerance = 15)
})


test_that("Checking by_year data frame.  Checking for year 1996.", {
  expect_equal(by_year$year[7], 1996)
})


