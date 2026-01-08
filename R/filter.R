#' Filter Out Rows Containing NA in Specified Columns
#'
#' This function removes rows from the data where specified columns contain NA values.
#'
#' @param .data A dataframe or tibble.
#' @param ... Columns to check for NA values. Supports tidyselect syntax
#'   (e.g., `everything()`, `starts_with("x")`).
#' @param if_any_or_all Should the row be removed if any or all selected columns contain NA?
#'   Must be either "if_any" or "if_all". Defaults to "if_all".
#'
#' @return A tibble with rows containing NA in specified columns removed.
#'   Grouping structure is preserved.
#'
#' @details
#' The `if_any_or_all` parameter controls the removal logic:
#' \itemize{
#'   \item `"if_all"` (default): Remove rows only when ALL specified columns are NA.
#'     Rows with at least one non-NA value are kept.
#'   \item `"if_any"`: Remove rows when ANY specified column is NA.
#'     Only rows with no NA values in the specified columns are kept.
#' }
#'
#' @examples
#' df <- tibble::tibble(
#'   a = c("x", "y", NA, NA),
#'   b = c("x", NA, "z", NA)
#' )
#'
#' # Remove rows where ALL columns are NA (default)
#' filter_out_na(df, a, b)
#' # Keeps rows 1, 2, 3 (only row 4 has all NA)
#'
#' # Remove rows where ANY column is NA
#' filter_out_na(df, a, b, if_any_or_all = "if_any")
#' # Keeps only row 1 (the only row with no NAs)
#'
#' @importFrom dplyr ungroup select group_vars filter if_all if_any all_of group_by across
#' @importFrom magrittr %>%
#' @export
filter_out_na <- function(.data, ..., if_any_or_all = "if_all") {
  if (!is.data.frame(.data)) {
    stop("`.data` must be a data frame or tibble.", call. = FALSE)
  }
  if (!if_any_or_all %in% c("if_any", "if_all")) {
    stop("`if_any_or_all` must be either \"if_any\" or \"if_all\".", call. = FALSE)
  }

  col.names <- .data %>% dplyr::ungroup() %>% dplyr::select(...) %>% names()
  groups <- dplyr::group_vars(.data)

  if (if_any_or_all == "if_any") {
    .data %>%
      dplyr::ungroup() %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(col.names), ~ !is.na(.x))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(groups)))
  } else {
    .data %>%
      dplyr::ungroup() %>%
      dplyr::filter(dplyr::if_any(dplyr::all_of(col.names), ~ !is.na(.x))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(groups)))
  }
}

#' Filter In Rows Containing NA in Specified Columns
#'
#' This function keeps rows from the data where specified columns contain NA values.
#'
#' @param .data A dataframe or tibble.
#' @param ... Columns to check for NA values. Supports tidyselect syntax
#'   (e.g., `everything()`, `starts_with("x")`).
#' @param if_any_or_all Should the row be kept if any or all selected columns contain NA?
#'   Must be either "if_any" or "if_all". Defaults to "if_all".
#'
#' @return A tibble with only rows containing NA in specified columns.
#'   Grouping structure is preserved.
#'
#' @details
#' The `if_any_or_all` parameter controls the selection logic:
#' \itemize{
#'   \item `"if_all"` (default): Keep rows only when ALL specified columns are NA.
#'   \item `"if_any"`: Keep rows when ANY specified column is NA.
#' }
#'
#' @examples
#' df <- tibble::tibble(
#'   a = c("x", "y", NA, NA),
#'   b = c("x", NA, "z", NA)
#' )
#'
#' # Keep rows where ALL columns are NA (default)
#' filter_in_na(df, a, b)
#' # Keeps only row 4 (the only row with all NA)
#'
#' # Keep rows where ANY column is NA
#' filter_in_na(df, a, b, if_any_or_all = "if_any")
#' # Keeps rows 2, 3, 4 (all have at least one NA)
#'
#' @importFrom dplyr ungroup select group_vars filter if_all if_any all_of group_by across
#' @importFrom magrittr %>%
#' @export
filter_in_na <- function(.data, ..., if_any_or_all = "if_all") {
  if (!is.data.frame(.data)) {
    stop("`.data` must be a data frame or tibble.", call. = FALSE)
  }
  if (!if_any_or_all %in% c("if_any", "if_all")) {
    stop("`if_any_or_all` must be either \"if_any\" or \"if_all\".", call. = FALSE)
  }

  col.names <- .data %>% dplyr::ungroup() %>% dplyr::select(...) %>% names()
  groups <- dplyr::group_vars(.data)

  if (if_any_or_all == "if_any") {
    .data %>%
      dplyr::ungroup() %>%
      dplyr::filter(dplyr::if_any(dplyr::all_of(col.names), ~ is.na(.x))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(groups)))
  } else {
    .data %>%
      dplyr::ungroup() %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(col.names), ~ is.na(.x))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(groups)))
  }
}

#' Filter rows in a data frame based on string matching in a column
#'
#' Internal function used by [filter_in()] and [filter_out()].
#'
#' @param .data A data frame or tibble.
#' @param col The column to filter on (unquoted).
#' @param pattern The regex pattern to match against.
#' @param ignore_case Whether to ignore case (default is TRUE).
#' @param drop_col Whether to remove the filtered column from results (default is FALSE).
#' @param negate Whether to invert the match (default is FALSE).
#' @param na_rm Whether to remove rows where `col` is NA (default is FALSE).
#'   When FALSE, rows with NA in the filtered column are always kept.
#'
#' @return A filtered data frame.
#'
#' @importFrom dplyr filter select
#' @importFrom stringr str_detect regex
#' @keywords internal
filter_str <- function(.data, col, pattern, ignore_case = TRUE, drop_col = FALSE, negate = FALSE, na_rm = FALSE) {
  if (!is.data.frame(.data)) {
    stop("`.data` must be a data frame or tibble.", call. = FALSE)
  }
  if (missing(col)) {
    stop("`col` must be specified.", call. = FALSE)
  }
  if (missing(pattern) || !is.character(pattern) || length(pattern) != 1) {
    stop("`pattern` must be a single character string.", call. = FALSE)
  }

  if (na_rm) {
    .data <- .data %>%
      dplyr::filter(stringr::str_detect({{ col }}, stringr::regex(pattern, ignore_case = ignore_case), negate = negate))
  } else {
    .data <- .data %>%
      dplyr::filter(stringr::str_detect({{ col }}, stringr::regex(pattern, ignore_case = ignore_case), negate = negate) | is.na({{ col }}))
  }

  if (drop_col) {
    return(.data %>% dplyr::select(-{{ col }}))
  }

  .data
}

#' Filter rows containing a specific string pattern
#'
#' Keep rows where the specified column matches a regex pattern.
#'
#' @param .data A data frame or tibble.
#' @param col The column to filter on (unquoted).
#' @param pattern A regex pattern to match. Use `stringr` regex syntax.
#' @param ignore_case Ignore case when matching (default TRUE).
#' @param drop_col Remove the filtered column from results (default FALSE).
#' @param na_rm Remove rows where `col` is NA (default FALSE).
#'
#' @return A filtered data frame.
#'
#' @details
#' ## NA Handling
#' By default (`na_rm = FALSE`), rows where `col` is NA are **preserved** in the
#' output. This prevents accidental data loss during exploratory analysis. Set
#' `na_rm = TRUE` to exclude NA rows.
#'
#' ## Pattern Matching
#' The `pattern` argument accepts regular expressions. Common patterns:
#' \itemize{
#'   \item `"foo"` - contains "foo" anywhere
#'   \item `"^foo"` - starts with "foo"
#'   \item `"foo$"` - ends with "foo"
#'   \item `"foo|bar"` - contains "foo" or "bar"
#' }
#'
#' @examples
#' df <- tibble::tibble(
#'   fruit = c("apple", "banana", "cherry", NA),
#'   count = 1:4
#' )
#'
#' # Keep rows containing "an"
#' filter_in(df, fruit, "an")
#'
#' # Case insensitive by default
#' filter_in(df, fruit, "APPLE")
#'
#' # Exclude NA rows
#' filter_in(df, fruit, "an", na_rm = TRUE)
#'
#' # Use regex for starts-with
#' filter_in(df, fruit, "^a")
#'
#' @seealso [filter_out()] for the inverse operation
#' @export
filter_in <- function(.data, col, pattern, ignore_case = TRUE, drop_col = FALSE, na_rm = FALSE) {
  filter_str(.data = .data, col = {{ col }}, pattern = pattern, ignore_case = ignore_case, drop_col = drop_col, negate = FALSE, na_rm = na_rm)
}

#' Filter out rows containing a specific string pattern
#'
#' Remove rows where the specified column matches a regex pattern.
#'
#' @inheritParams filter_in
#'
#' @return A filtered data frame.
#'
#' @details
#' ## NA Handling
#' By default (`na_rm = FALSE`), rows where `col` is NA are **preserved** in the
#' output. This is because NA values don't match the pattern being filtered out.
#' Set `na_rm = TRUE` to also exclude NA rows.
#'
#' ## Pattern Matching
#' The `pattern` argument accepts regular expressions. See [filter_in()] for
#' pattern examples.
#'
#' @examples
#' df <- tibble::tibble(
#'   status = c("active", "inactive", "pending", NA),
#'   id = 1:4
#' )
#'
#' # Remove inactive rows (keeps NA)
#' filter_out(df, status, "inactive")
#'
#' # Remove inactive rows AND NA rows
#' filter_out(df, status, "inactive", na_rm = TRUE)
#'
#' @seealso [filter_in()] for the inverse operation
#' @export
filter_out <- function(.data, col, pattern, ignore_case = TRUE, drop_col = FALSE, na_rm = FALSE) {
  filter_str(.data = .data, col = {{ col }}, pattern = pattern, ignore_case = ignore_case, drop_col = drop_col, negate = TRUE, na_rm = na_rm)
}

#' Filter to rows with non-numeric values
#'
#' Keep rows where selected columns contain values that cannot be converted to numbers.
#' Useful for finding data entry errors or non-numeric entries in columns that
#' should be numeric.
#'
#' @param .data A dataframe to be filtered.
#' @param .cols Columns to check for non-numeric values. Defaults to all columns.
#'   Supports tidyselect syntax.
#' @param na_rm If TRUE, exclude rows where the selected columns are NA (default FALSE).
#'
#' @return A dataframe containing only rows with at least one non-numeric value
#'   in the specified columns.
#'
#' @details
#' A value is considered "non-numeric" if `as.numeric()` returns NA (with warnings
#' suppressed). This means:
#' \itemize{
#'   \item `"123"` is numeric
#'   \item `"12.5"` is numeric
#'   \item `"-3"` is numeric
#'   \item `"abc"` is non-numeric
#'   \item `"12a"` is non-numeric
#'   \item `NA` is treated as non-numeric by default (use `na_rm = TRUE` to exclude)
#' }
#'
#' @examples
#' df <- data.frame(
#'   a = c("1", "2", "x", NA),
#'   b = c("y", "2", "3", "4"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Find rows with any non-numeric values
#' filter_out_numeric(df)
#'
#' # Check only column 'a', excluding NAs
#' filter_out_numeric(df, .cols = a, na_rm = TRUE)
#'
#' @importFrom dplyr filter if_any everything
#' @export
filter_out_numeric <- function(.data, .cols = dplyr::everything(), na_rm = FALSE) {
  if (!is.data.frame(.data)) {
    stop("`.data` must be a data frame or tibble.", call. = FALSE)
  }

  is_non_numeric <- function(x) {
    suppressWarnings(is.na(as.numeric(x)))
  }

  if (na_rm) {
    .data %>%
      dplyr::filter(dplyr::if_any({{ .cols }}, ~ is_non_numeric(.x) & !is.na(.x)))
  } else {
    .data %>%
      dplyr::filter(dplyr::if_any({{ .cols }}, is_non_numeric))
  }
}
