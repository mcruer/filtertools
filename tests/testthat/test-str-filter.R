test_that("str_filter returns matching strings", {
  result <- str_filter(c("apple", "banana", "cherry"), "a")

  expect_equal(result, c("apple", "banana"))
})

test_that("str_filter is case insensitive by default", {
  result <- str_filter(c("Apple", "BANANA", "cherry"), "a")

  expect_equal(result, c("Apple", "BANANA"))
})

test_that("str_filter respects ignore_case = FALSE", {
  result <- str_filter(c("Apple", "apple", "APPLE"), "apple", ignore_case = FALSE)

  expect_equal(result, "apple")
})

test_that("str_filter with negate returns non-matching strings", {
  result <- str_filter(c("apple", "banana", "cherry"), "a", negate = TRUE)

  expect_equal(result, "cherry")
})

test_that("str_filter works with regex anchors", {
  result <- str_filter(c("apple", "application", "banana"), "^app")

  expect_equal(result, c("apple", "application"))
})

test_that("str_filter works with end anchor", {
  result <- str_filter(c("apple", "pineapple", "banana"), "apple$")

  expect_equal(result, c("apple", "pineapple"))
})

test_that("str_filter returns empty vector when no matches", {
  result <- str_filter(c("apple", "banana", "cherry"), "xyz")

  expect_equal(length(result), 0)
  expect_type(result, "character")
})
test_that("str_filter handles empty input vector", {
  result <- str_filter(character(0), "a")

  expect_equal(length(result), 0)
  expect_type(result, "character")
})

test_that("str_filter handles NA values in input", {
  result <- str_filter(c("apple", NA, "cherry"), "a")

  expect_true(any(is.na(result)) || length(result) == 1)
})

test_that("str_filter throws error for non-character input", {
  expect_error(
    str_filter(c(1, 2, 3), "1"),
    "must be a character vector"
  )
})

test_that("str_filter throws error for missing pattern", {
  expect_error(
    str_filter(c("a", "b")),
    "must be a single character string"
  )
})

test_that("str_filter throws error for non-character pattern", {
  expect_error(
    str_filter(c("a", "b"), 123),
    "must be a single character string"
  )
})

test_that("str_filter throws error for vector pattern", {
  expect_error(
    str_filter(c("a", "b"), c("a", "b")),
    "must be a single character string"
  )
})
