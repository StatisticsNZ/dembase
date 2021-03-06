% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGeneric.R, R/Counts-methods.R,
%   R/Values-methods.R
\name{collapseOrigDest}
\alias{collapseOrigDest}
\alias{collapseOrigDest,Counts,ANY,ANY,missing-method}
\alias{collapseOrigDest,Values,ANY,ANY,ANY-method}
\title{Convert data on movements into a less detailed format.}
\usage{
collapseOrigDest(
  object,
  base = NULL,
  to = c("net", "pool", "in", "out"),
  weights,
  omitted = ifelse(methods::is(object, "Counts"), 0L, NA_integer_),
  ...
)

\S4method{collapseOrigDest}{Counts,ANY,ANY,missing}(
  object,
  base = NULL,
  to = c("net", "pool", "in", "out"),
  omitted = ifelse(methods::is(object, "Counts"), 0L, NA_integer_)
)

\S4method{collapseOrigDest}{Values,ANY,ANY,ANY}(
  object,
  base = NULL,
  to = c("net", "pool", "in", "out"),
  weights,
  omitted = ifelse(methods::is(object, "Counts"), 0L, NA_integer_)
)
}
\arguments{
\item{object}{Object of class \code{\linkS4class{DemographicArray}}.}

\item{base}{Character vector giving the "base names" for the
origin-destination pairs.  If \code{base} is omitted, the base names from
all dimensions with \code{\link{dimtype}} \code{"origin"} and
\code{"destination"} are used.}

\item{to}{The new format(s): \code{"net"} (the default), \code{"out"},
and \code{"in"}.}

\item{weights}{Object of class \code{\linkS4class{Counts}} providing the
weights to be used.  Required if \code{object} has class
\code{\linkS4class{Values}}, and prohibited if it has class
\code{\linkS4class{Counts}}.}

\item{omitted}{Value to be used for omitted origin-destination or
parent-child combinations.  Defaults to \code{0} when \code{object} has
class \code{\linkS4class{Counts}} and \code{NA} otherwise.}

\item{\dots}{Not currently used.}
}
\value{
If \code{object} has class \code{linkS4class{Counts}} and
\code{to} is \code{"net"}, then the return value has class
\code{\linkS4class{Net}}.  If \code{object} has class
\code{linkS4class{Counts}} and' \code{to} is \code{"pool"},
then the return value has class \code{\linkS4class{Pool}}.
Otherwise the return value has the same class as \code{object}.
}
\description{
Convert data on movements classifed by state of origin and state of
destination into a less detailed format.  The choices of format are
\describe{
  \item{\code{"out"}}{Total outward moves from each state}
  \item{\code{"in"}}{Total inward moves to each state}
  \item{\code{"pool"}}{\code{"out"} and \code{"in"}}
  \item{\code{"net"}}{\code{"in"} minus \code{"out"}}
}
}
\details{
\code{collapseOrigDest} calls function \code{\link{alignPair}} before
collapsing cells.  Argument \code{omitted} is passed to \code{alignPair}.
}
\section{Warning}{
 If \code{ans} is the result of applying
\code{collapseOrigDest} to \code{x}, then \code{sum(ans)} does not
necessarily equal \code{sum(x)}.  For instance,
\code{sum(collapseOrigDest(x, to = "net"))} is always \code{0}.
}

\examples{
library(demdata)
mig <- Counts(nz.mig)
collapseOrigDest(mig, base = "island", to = "net")

## base defaults to all origin-destination base names,
## and 'to' defaults to "net"
collapseOrigDest(mig)

## multiple formats, distinguished by dimension "direction"
collapseOrigDest(mig, to = "pool")
collapseOrigDest(mig, to = "net")
collapseOrigDest(mig, to = "out")

## different representations imply different totals
sum(mig)
sum(collapseOrigDest(mig, to = "net"))
sum(collapseOrigDest(mig, to = "pool"))

x <- Counts(array(1:4,
                  dim = c(2, 2),
                  dimnames = list(region_orig = c("a", "b"),
                  region_dest = c("c", "b"))))
x
## extra categories are added to region_orig and
## region_dest via 'alignPair' before collapsing
collapseOrigDest(x, to = "pool")
}
\references{
Wilson, T. and Bell, M. (2004) Comparative empirical evaluations
of internal migration models in subnational population projections.
\emph{Journal of Population Research}. 21(2): 127-160.
}
\seealso{
\code{\link{alignPair}} \code{\link{collapseCategories}},
\code{\link{collapseIntervals}}, \code{\link{collapseIterations}},
\code{\link{collapseOrigDest}}, \code{\link{collapseDimension}},
\code{\link{dimtypes}}, \code{\link{dimscales}}
}
