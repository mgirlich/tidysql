#' new_sql(c("x", "y + 1"))
#' new_sql(c(a = "x", b = "y + 1"))
new_sql <- function(x) {
  new_vctr(x, class = "vctrs_sql")
}

sql <- function(...) {
  x <- vec_c(...)
  new_sql(x)
}

is_sql <- function(x) {
  inherits(x, "vctrs_sql")
}

#' @export
print.vctrs_sql <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' @export
format.vctrs_sql <- function(x, ...) {
  if (length(x) == 0) {
    paste0("<SQL> [empty]")
  } else {
    if (!is.null(names(x))) {
      paste0("<SQL> ", paste0(x, " AS ", names(x)))
    } else {
      paste0("<SQL> ", x)
    }
  }
}
