#' Filter strings based on a regex pattern
#'
#' This function filters a character vector, returning strings that match (or do not match if negate is TRUE) a specified regular expression pattern.
#'
#' @param string A character vector to be filtered.
#' @param pattern The regular expression pattern to match against.
#' @param ignore_case Logical; should case be ignored in the match? Defaults to TRUE.
#' @param negate Logical; should the sense of the match be reversed? Defaults to FALSE.
#'
#' @return A character vector containing the filtered strings.
#'
#' @importFrom stringr str_detect regex
#' @export
#'
#' @examples
#' str_filter(c("apple", "banana", "cherry"), "a")
#' str_filter(c("apple", "banana", "cherry"), "^a", negate = TRUE)
str_filter <- function(string, pattern, ignore_case = TRUE, negate = FALSE) {
  if (!is.character(string)) {
    stop("`string` must be a character vector.", call. = FALSE)
  }
  if (missing(pattern) || !is.character(pattern) || length(pattern) != 1) {
    stop("`pattern` must be a single character string.", call. = FALSE)
  }

  index <- stringr::str_detect(string, stringr::regex(pattern, ignore_case = ignore_case), negate = negate)
  string[index]
}
