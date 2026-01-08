test_that("filter_out_na removes rows where all specified columns are NA (default)", {
  df <- tibble::tibble(
    a = c("a", "b", NA),
    b = c("a", NA, NA)
  )

  result <- filter_out_na(df, a, b)

  expect_equal(nrow(result), 2)
  expect_equal(result$a, c("a", "b"))
})

test_that("filter_out_na with if_any removes rows where any column is NA", {
  df <- tibble::tibble(
    a = c("a", "b", NA),
    b = c("a", NA, NA)
  )

  result <- filter_out_na(df, a, b, if_any_or_all = "if_any")

  expect_equal(nrow(result), 1)
  expect_equal(result$a, "a")
})

test_that("filter_out_na works with everything()", {
  df <- tibble::tibble(
    a = c("a", "b", NA),
    b = c("a", NA, NA)
  )

  result <- filter_out_na(df, dplyr::everything(), if_any_or_all = "if_any")

  expect_equal(nrow(result), 1)
})

test_that("filter_out_na preserves grouping", {
  df <- tibble::tibble(
    g = c("x", "x", "y"),
    a = c("a", NA, NA)
  ) %>% dplyr::group_by(g)

  result <- filter_out_na(df, a)

  expect_equal(dplyr::group_vars(result), "g")
})

test_that("filter_out_na throws error for invalid if_any_or_all", {
  df <- tibble::tibble(a = c(1, NA))

  expect_error(
    filter_out_na(df, a, if_any_or_all = "invalid"),
    "must be either"
  )
})

test_that("filter_out_na throws error for non-dataframe input", {
  expect_error(
    filter_out_na(c(1, 2, 3), a),
    "must be a data frame"
  )
})

test_that("filter_in_na keeps rows where all specified columns are NA (default)", {
  df <- tibble::tibble(
    a = c("a", "b", NA),
    b = c("a", NA, NA)
  )

  result <- filter_in_na(df, a, b)

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$a))
  expect_true(is.na(result$b))
})

test_that("filter_in_na with if_any keeps rows where any column is NA", {
  df <- tibble::tibble(
    a = c("a", "b", NA),
    b = c("a", NA, NA)
  )

  result <- filter_in_na(df, a, b, if_any_or_all = "if_any")

  expect_equal(nrow(result), 2)
})

test_that("filter_in_na preserves grouping", {
  df <- tibble::tibble(
    g = c("x", "x", "y"),
    a = c("a", NA, NA)
  ) %>% dplyr::group_by(g)

  result <- filter_in_na(df, a)

  expect_equal(dplyr::group_vars(result), "g")
})

test_that("filter_in_na throws error for invalid if_any_or_all", {
  df <- tibble::tibble(a = c(1, NA))

  expect_error(
    filter_in_na(df, a, if_any_or_all = "bad_value"),
    "must be either"
  )
})

test_that("filter_in_na throws error for non-dataframe input", {
  expect_error(
    filter_in_na(list(a = 1), a),
    "must be a data frame"
  )
})

test_that("filter_in_na and filter_out_na are complementary", {
  df <- tibble::tibble(
    a = c("a", "b", NA, NA),
    b = c("a", NA, "c", NA)
  )

  kept <- filter_in_na(df, a, b, if_any_or_all = "if_any")
  removed <- filter_out_na(df, a, b, if_any_or_all = "if_any")

  expect_equal(nrow(kept) + nrow(removed), nrow(df))
})
