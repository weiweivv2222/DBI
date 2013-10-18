#' DBI tests
#' 
#' If run in a testthat context, they show expectations passing as they
#' occur; otherwise they'll just throw an error if any expectation fails.
#' 
#' @param con A connection to a clean database with write access.
#' @examples
#' if (require("RSQLite")) {
#' con <- dbConnect(SQLite(), dbname = tempfile())
#' dbTestTableInspection(con)
#' }
#' @name tests
NULL

#' @rdname tests
#' @export
dbTestTableInspection <- function(con) {
  if (!require("testthat")) stop("testthat required for testing")
  
  expect_equal(dbListTables(con), character())
  dbWriteTable(con, "mtcars", mtcars)
  expect_equal(dbListTables(con), "mtcars")
  
  dbRemoveTable(con, "mtcars")
  expect_equal(dbListTables(con), character())
}

#' @rdname tests
#' @export
dbTestConnectionClone <- function(con) {
  con1 <- dbConnect(con)
  con2 <- dbConnect(con)
  
  expect_true(dbIsValid(con1))
  expect_true(dbIsValid(con2))
  
  dbDisconnect(con1)
  expect_false(dbIsValid(con1))
  expect_true(dbIsValid(con2))

  dbDisconnect(con2)
  expect_false(dbIsValid(con1))
  expect_false(dbIsValid(con2))
}
  
