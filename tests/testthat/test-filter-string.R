test_that("filter_in keeps rows matching pattern", {
  df <- tibble::tibble(x = c("apple", "banana", "cherry"))

  result <- filter_in(df, x, "app")

  expect_equal(nrow(result), 1)
  expect_equal(result$x, "apple")
})

test_that("filter_in is case insensitive by default", {
  df <- tibble::tibble(x = c("Apple", "BANANA", "cherry"))

  result <- filter_in(df, x, "apple")

  expect_equal(nrow(result), 1)
  expect_equal(result$x, "Apple")
})
test_that("filter_in respects ignore_case = FALSE", {
  df <- tibble::tibble(x = c("Apple", "apple", "APPLE"))

  result <- filter_in(df, x, "apple", ignore_case = FALSE)

  expect_equal(nrow(result), 1)
  expect_equal(result$x, "apple")
})

test_that("filter_in preserves NA values by default", {
  df <- tibble::tibble(x = c("apple", "banana", NA))

  result <- filter_in(df, x, "apple")

  expect_equal(nrow(result), 2)
  expect_true(any(is.na(result$x)))
})

test_that("filter_in removes NA values when na_rm = TRUE", {
  df <- tibble::tibble(x = c("apple", "banana", NA))

  result <- filter_in(df, x, "apple", na_rm = TRUE)

  expect_equal(nrow(result), 1)
  expect_false(any(is.na(result$x)))
})

test_that("filter_in can drop the filtered column", {
  df <- tibble::tibble(x = c("apple", "banana"), y = c(1, 2))

  result <- filter_in(df, x, "apple", drop_col = TRUE)

  expect_false("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("filter_in supports regex patterns", {
  df <- tibble::tibble(x = c("apple", "application", "banana"))

  result <- filter_in(df, x, "^app")

  expect_equal(nrow(result), 2)
})

test_that("filter_out removes rows matching pattern", {
  df <- tibble::tibble(x = c("apple", "banana", "cherry"))

  result <- filter_out(df, x, "app")

  expect_equal(nrow(result), 2)
  expect_false("apple" %in% result$x)
})

test_that("filter_out is case insensitive by default", {
  df <- tibble::tibble(x = c("Apple", "BANANA", "cherry"))

  result <- filter_out(df, x, "apple")

  expect_equal(nrow(result), 2)
  expect_false("Apple" %in% result$x)
})

test_that("filter_out preserves NA values by default", {
  df <- tibble::tibble(x = c("apple", "banana", NA))

  result <- filter_out(df, x, "apple")

  expect_equal(nrow(result), 2)
  expect_true(any(is.na(result$x)))
})

test_that("filter_out removes NA values when na_rm = TRUE", {
  df <- tibble::tibble(x = c("apple", "banana", NA))

  result <- filter_out(df, x, "apple", na_rm = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$x, "banana")
})

test_that("filter_in and filter_out are complementary (excluding NA)", {
  df <- tibble::tibble(x = c("apple", "banana", "cherry", "apricot"))

  kept <- filter_in(df, x, "a", na_rm = TRUE)
  removed <- filter_out(df, x, "a", na_rm = TRUE)

  expect_equal(nrow(kept) + nrow(removed), nrow(df))
})

test_that("filter_in throws error for non-dataframe input", {
  expect_error(
    filter_in(c("a", "b"), x, "a"),
    "must be a data frame"
  )
})

test_that("filter_in throws error for missing pattern", {
  df <- tibble::tibble(x = c("a", "b"))

  expect_error(
    filter_in(df, x),
    "must be a single character string"
  )
})

test_that("filter_in throws error for non-character pattern", {
  df <- tibble::tibble(x = c("a", "b"))

  expect_error(
    filter_in(df, x, 123),
    "must be a single character string"
  )
})

test_that("filter_in throws error for vector pattern", {
  df <- tibble::tibble(x = c("a", "b"))

  expect_error(
    filter_in(df, x, c("a", "b")),
    "must be a single character string"
  )
})
