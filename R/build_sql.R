#' * should `build_sql` support bang bang?

build_sql <- function(..., .env = parent.frame(), con) {
  if (is.null(con)) {
    stop("`con` must not be NULL", call. = FALSE)
  }

  escape_expr <- function(x, con) {
    if (is_ident(x)) return()

    # If it's a string, leave it as is
    if (is.character(x)) return(x)

    val <- eval_bare(x, .env)
    # Skip nulls, so you can use if statements like in paste
    if (is.null(val)) return("")

    # escape(val, con = con)
    paste0("<", val, ">")
  }

  pieces <- purrr::map_chr(enexprs(...), escape_expr, con = con)
  sql(paste0(pieces, collapse = ""))
}


sql <- dbplyr::sql
tmp <- sql("table")

debugonce(build_sql)
build_sql("select * from ", !!tmp, !!sql(" more sql"))
debugonce(build_sql)
build_sql("select * from ", tmp, sql(" more sql"))
