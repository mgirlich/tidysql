#' helper functions to escape R values

#' date

# install.packages("RMySQL")
install.packages("RMariaDB")
install.packages("RSQLite")
install.packages("RPostgres")
install.packages("odbc")
# install.packages("RKazam")
# install.packages("RODBC")
# install.packages("ROracle")
# install.packages("bigrquery")

library(RMariaDB)
library(RSQLite)
library(RPostgres)
library(odbc)
library(DBI)


simulate_dbi2 <- function(class = "DBIConnection", ...) {
  structure(
    list(),
    ...,
    class = class
  )
}

# dbplyr::simulate_dbi
simulate_mssql2 <- function(version = "15.0") {
  simulate_dbi2("Microsoft SQL Server", version = numeric_version(version))
}
simulate_access2 <- function() {
  simulate_dbi2("ACCESS")
}
simulate_hana2 <- function() {
  simulate_dbi2("HDB")
}
simulate_hive2 <- function() {
  simulate_dbi2("Hive")
}
simulate_impala2 <- function() {
  simulate_dbi2("Impala")
}
simulate_mysql2 <- function() {
  simulate_dbi2("MariaDBConnection")
}
simulate_odbc2 <- function() {
  simulate_dbi2("OdbcConnection")
}
simulate_oracle2 <- function() {
  simulate_dbi2("Oracle")
}
simulate_postgres2 <- function() {
  simulate_dbi2("PqConnection")
}
simulate_redshift2 <- function() {
  simulate_dbi2("RedshiftConnection")
}
simulate_sqlite2 <- function() {
  simulate_dbi2("SQLiteConnection")
}
simulate_teradata2 <- function() {
  simulate_dbi2("Teradata")
}

cons <- list(
  simulate_dbi2(),
  # simulate_mssql2(),
  # simulate_access2(),
  # simulate_hana2(),
  # simulate_hive2(),
  # simulate_impala2(),
  # simulate_mysql2(),
  simulate_odbc2(),
  # simulate_oracle2(),
  simulate_postgres2(),
  simulate_redshift2(),
  simulate_sqlite2()
  # simulate_teradata2()
)

cons <- set_names(cons, purrr::map_chr(cons, class))


purrr::map(cons, dbQuoteLiteral, x = Sys.Date())
sql_escape_date(cons$DBIConnection, x = Sys.Date())

purrr::map(cons, dbQuoteLiteral, x = Sys.time())
sql_escape_date(cons$DBIConnection, x = Sys.time())




#' SQL escaping/quoting generics
#'
#' These generics translate individual values into SQL. The core
#' generics are [DBI::dbQuoteIdentifier()] and[DBI::dbQuoteString]
#' for quoting identifiers and strings, but dbplyr needs additional
#' tools for inserting logical, date, date-time, and raw values into
#' queries.
#'
#' @keywords internal
#' @family generic
#' @name db-quote
#' @aliases NULL
#' @examples
#' con <- simulate_dbi()
#' sql_escape_logical(con, c(TRUE, FALSE, NA))
#' sql_escape_date(con, Sys.Date())
#' sql_escape_date(con, Sys.time())
#' sql_escape_datetime(con, Sys.time())
#' sql_escape_raw(con, charToRaw("hi"))
#' sql_escape_ident(con, "my col")
#' sql_escape_string(con, "string")
NULL

#' @rdname db-quote
#' @export
sql_escape_logical <- function(con, x) {
  UseMethod("sql_escape_logical")
}
#' @export
sql_escape_logical.DBIConnection <- function(con, x) {
  y <- as.character(x)
  y[is.na(x)] <- "NULL"
  y
}

#' @export
#' @rdname db-quote
sql_escape_date <- function(con, x) {
  UseMethod("sql_escape_date")
}
#' @export
sql_escape_date.DBIConnection <- function(con, x) {
  sql_escape_string(con, as.character(x))
}

#' @export
#' @rdname db-quote
sql_escape_datetime <- function(con, x) {
  UseMethod("sql_escape_datetime")
}
#' @export
sql_escape_datetime.DBIConnection <- function(con, x) {
  x <- strftime(x, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  sql_escape_string(con, x)
}

#' @export
#' @rdname db-quote
sql_escape_raw <- function(con, x) {
  UseMethod("sql_escape_raw")
}
#' @export
sql_escape_raw.DBIConnection <- function(con, x) {
  # SQL-99 standard for BLOB literals
  # https://crate.io/docs/sql-99/en/latest/chapters/05.html#blob-literal-s
  paste0(c("X'", format(x), "'"), collapse = "")
}


#' @export
sql_escape_ident <- function(con, x) {
  UseMethod("sql_escape_ident")
}
#' @export
sql_escape_ident.DBIConnection <- function(con, x) {
  dbQuoteIdentifier(con, x)
}
#' @export
sql_escape_ident.TestConnection <- function(con, x) {
  sql_quote(x, "`")
}

#' @export
sql_escape_string <- function(con, x) {
  UseMethod("sql_escape_string")
}
#' @export
sql_escape_string.DBIConnection <- function(con, x) {
  dbQuoteString(con, x)
}
#' @export
sql_escape_string.TestConnection <- function(con, x) {
  sql_quote(x, "'")
}
