test_that("filter_out_numeric keeps rows with non-numeric values", {
  df <- data.frame(
    a = c("1", "2", "x", "4"),
    stringsAsFactors = FALSE
  )

  result <- filter_out_numeric(df)

  expect_equal(nrow(result), 1)
  expect_equal(result$a, "x")
})

test_that("filter_out_numeric works with multiple columns", {
  df <- data.frame(
    a = c("1", "2", "x", NA),
    b = c("y", "2", "3", "4"),
    stringsAsFactors = FALSE
  )

  result <- filter_out_numeric(df)

  expect_equal(nrow(result), 3)
})

test_that("filter_out_numeric can filter specific columns", {
  df <- data.frame(
    a = c("1", "2", "x", NA),
    b = c("y", "2", "3", "4"),
    stringsAsFactors = FALSE
  )

  result <- filter_out_numeric(df, .cols = a)

  expect_equal(nrow(result), 2)
})

test_that("filter_out_numeric includes NA by default", {
  df <- data.frame(
    a = c("1", "x", NA),
    stringsAsFactors = FALSE
  )

  result <- filter_out_numeric(df)

  expect_equal(nrow(result), 2)
  expect_true(any(is.na(result$a)))
})

test_that("filter_out_numeric excludes NA when na_rm = TRUE", {
  df <- data.frame(
    a = c("1", "x", NA),
    stringsAsFactors = FALSE
  )

  result <- filter_out_numeric(df, na_rm = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$a, "x")
})

test_that("filter_out_numeric returns empty df when all values are numeric", {
  df <- data.frame(
    a = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )

  result <- filter_out_numeric(df)

  expect_equal(nrow(result), 0)
})

test_that("filter_out_numeric handles decimal numbers", {
  df <- data.frame(
    a = c("1.5", "2.7", "abc"),
    stringsAsFactors = FALSE
  )

  result <- filter_out_numeric(df)

  expect_equal(nrow(result), 1)
  expect_equal(result$a, "abc")
})

test_that("filter_out_numeric handles negative numbers", {
  df <- data.frame(
    a = c("-1", "-2.5", "abc"),
    stringsAsFactors = FALSE
  )

  result <- filter_out_numeric(df)

  expect_equal(nrow(result), 1)
  expect_equal(result$a, "abc")
})

test_that("filter_out_numeric throws error for non-dataframe input", {
  expect_error(
    filter_out_numeric(c("1", "x")),
    "must be a data frame"
  )
})
