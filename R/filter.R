#' Filter Out Rows Containing NA in Specified Columns
#'
#' This function removes rows from the data where specified columns contain NA values.
#'
#' @param .data A dataframe or tibble.
#' @param ... Columns to check for NA values.
#' @param if_any_or_all Should the row be removed if any or all selected columns contain NA?
#'   Must be either "if_any" or "if_all". Defaults to "if_all".
#'
#' @return A tibble with rows containing NA in specified columns removed.
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
#' @param ... Columns to check for NA values.
#' @param if_any_or_all Should the row be kept if any or all selected columns contain NA?
#'   Must be either "if_any" or "if_all". Defaults to "if_all".
#'
#' @return A tibble with only rows containing NA in specified columns.
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
#' @param .data A data frame or tibble.
#' @param col The column to filter on.
#' @param pattern The string pattern to look for.
#' @param ignore_case Whether to ignore case (default is TRUE).
#' @param drop_col Whether to remove the column that was filtered on (default is FALSE).
#' @param negate Whether to keep or remove rows that match the string (default is FALSE).
#' @param na_rm Whether to remove NA values (default is FALSE).
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

#' Filter rows containing a specific string in a given column
#'
#' This function filters rows where the specified column contains the given string.
#'
#' @param .data A data frame or tibble.
#' @param col The column to filter on.
#' @param pattern The string pattern to look for.
#' @param ignore_case Whether to ignore case (default is TRUE).
#' @param drop_col Whether to remove the column that was filtered on (default is FALSE).
#' @param na_rm Whether to remove NA values (default is FALSE).
#'
#' @return A filtered data frame.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' tibble(x = c("apple", "banana", "cherry")) %>%
#'   filter_in(col = x, pattern = "app")
#' }
#' @export
filter_in <- function(.data, col, pattern, ignore_case = TRUE, drop_col = FALSE, na_rm = FALSE) {
  filter_str(.data = .data, col = {{ col }}, pattern = pattern, ignore_case = ignore_case, drop_col = drop_col, negate = FALSE, na_rm = na_rm)
}

#' Filter out rows containing a specific string in a given column
#'
#' This function filters out rows where the specified column contains the given string.
#'
#' @param .data A data frame or tibble.
#' @param col The column to filter on.
#' @param pattern The string pattern to look for.
#' @param ignore_case Whether to ignore case (default is TRUE).
#' @param drop_col Whether to remove the column that was filtered on (default is FALSE).
#' @param na_rm Whether to remove NA values (default is FALSE).
#'
#' @return A filtered data frame.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' tibble(x = c("apple", "banana", "cherry")) %>%
#'   filter_out(col = x, pattern = "app")
#' }
#' @export
filter_out <- function(.data, col, pattern, ignore_case = TRUE, drop_col = FALSE, na_rm = FALSE) {
  filter_str(.data = .data, col = {{ col }}, pattern = pattern, ignore_case = ignore_case, drop_col = drop_col, negate = TRUE, na_rm = na_rm)
}

#' Filter Out Numeric Values from Selected Columns
#'
#' This function filters a dataframe to retain rows where the selected columns contain non-numeric values.
#' It can optionally remove rows where the selected columns are NA.
#'
#' @param .data A dataframe to be filtered.
#' @param .cols Columns to check for non-numeric values; defaults to all columns.
#' @param na_rm Logical; if TRUE, rows where the selected columns are NA are excluded.
#'
#' @return A dataframe with rows containing non-numeric values in the specified columns.
#'
#' @importFrom dplyr filter if_any everything
#' @export
#'
#' @examples
#' # Example dataframe
#' df <- data.frame(
#'   a = c("1", "2", "x", NA),
#'   b = c("y", "2", "3", "4")
#' )
#'
#' # Filter out rows with numeric values in all columns
#' filter_out_numeric(df)
#'
#' # Filter out rows with numeric values in column 'a', ignoring NAs
#' filter_out_numeric(df, .cols = a, na_rm = TRUE)
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
