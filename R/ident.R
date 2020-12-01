#' ident("x")
#' ident(a = "x", b = "y")
new_ident <- function(x) {
  new_vctr(x, class = "vctrs_ident")
}

ident <- function(...) {
  x <- vec_c(...)
  new_ident(x)
}

is_ident <- function(x) {
  inherits(x, "vctrs_ident")
}

#' @export
print.vctrs_ident <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' @export
format.vctrs_ident <- function(x, ...) {
  if (length(x) == 0) {
    paste0("<IDENT> [empty]")
  } else {
    nms <- names2(x)
    paste0(
      "<IDENT> ", x,
      ifelse(
        nms == "",
        "",
        paste0(" AS ", nms)
      )
    )
  }
}
