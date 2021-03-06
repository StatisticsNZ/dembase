% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGeneric.R, R/DemographicArray-methods.R,
%   R/MetaData-methods.R
\name{pairAligned}
\alias{pairAligned}
\alias{pairAligned,DemographicArray-method}
\alias{pairAligned,MetaData-method}
\title{Test whether pairs of dimensions aligned.}
\usage{
pairAligned(object, base = NULL)

\S4method{pairAligned}{DemographicArray}(object, base = NULL)

\S4method{pairAligned}{MetaData}(object, base = NULL)
}
\arguments{
\item{object}{An object of class \code{\linkS4class{DemographicArray}}.}

\item{base}{Character vector giving the "base names" for the dimension pairs.
If \code{base} is omitted, the base names from all dimensions with
\code{\link{dimtype}} \code{"origin"}, \code{"destination"}, \code{"parent"},
or \code{"child"} are used.}
}
\value{
\code{TRUE} or an error describing any differences.
}
\description{
Dimensions with \code{\link{dimtypes}} \code{"origin"} and
\code{"destination"} or \code{"parent"} and \code{"child"} come in pairs.
\code{pairAligned} tests whether the members of each pair have
the same categories, in the same order.  If they do, \code{pairAligned}
returns \code{TRUE}.  If not, it raises an error with a message that
describes the differences.
}
\details{
By default \code{pairAligned} tests all origin-destination and
parent-child pairs.  Argument \code{base} can be used to restrict
the test to specific pairs.  See below for an example.
}
\examples{
x <- Counts(array(1:4,
                  dim = c(2, 2),
                  dimnames = list(reg_orig = c("A", "B"),
                                  reg_dest = c("A", "B"))))
pairAligned(x)
x <- Counts(array(1:6,
                  dim = 3:2,
                  dimnames = list(reg_orig = c("A", "B", "C"),
                                  reg_dest = c("A", "B"))))
\dontrun{pairAligned(x)}

## first pair not aligned; second pair OK
x <- Counts(array(1:16,
                  dim = c(2, 2, 2, 2),
                  dimnames = list(reg_orig = c("B", "A"),
                                  reg_dest = c("A", "B"),
                                  income_parent = c("Low", "High"),
                                  income_child = c("Low", "High"))))
\dontrun{pairAligned(x)}
pairAligned(x, base = "income")
\dontrun{pairAligned(x, base = "reg")}
}
\seealso{
Pairs of dimensions can be aligned using function
\code{\link{alignPair}}. In programming, functions \code{\link{try}} and
\code{\link{tryCatch}} may be useful for intercepting and dealing with
errors.
}
