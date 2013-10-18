\name{tests}
\alias{dbTestConnectionClone}
\alias{dbTestTableInspection}
\alias{tests}
\title{DBI tests}
\usage{
  dbTestTableInspection(con)

  dbTestConnectionClone(con)
}
\arguments{
  \item{con}{A connection to a clean database with write
  access.}
}
\description{
  If run in a testthat context, they show expectations
  passing as they occur; otherwise they'll just throw an
  error if any expectation fails.
}
\examples{
if (require("RSQLite")) {
con <- dbConnect(SQLite(), dbname = tempfile())
dbTestTableInspection(con)
}
}

