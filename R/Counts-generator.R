
## HAS_TESTS
#' @rdname counts-values-generator
#' @export
setMethod("Counts",
          signature(object = "array"),
          function(object, dimtypes = NULL, dimscales = NULL) {
              metadata <- MetaData(object = object,
                                   dimtypes = dimtypes,
                                   dimscales = dimscales)
              object <- permuteToMatchIntervalOrPointMetadata(object, metadata = metadata)
              object <- array(object@.Data,
                              dim = dim(metadata),
                              dimnames = dimnames(metadata))
              methods::new("Counts", object, metadata = metadata)
          })

## HAS_TESTS
#' @rdname counts-values-generator
#' @export
setMethod("Counts",
          signature(object = "numeric"),
          function(object, dimtypes = NULL, dimscales = NULL) {
              name <- deparse(substitute(object))
              dimnames <- list(names(object))
              names(dimnames) <- name
              object <- array(object,
                              dim = length(object),
                              dimnames = dimnames)
              methods::callGeneric()
          })

## HAS_TESTS
#' @rdname counts-values-generator
#' @export
setMethod("Counts",
          signature(object = "ANY"),
          function(object, dimtypes = NULL, dimscales = NULL) {
              object <- array(object,
                              dim = dim(object),
                              dimnames = dimnames(object))
              methods::callGeneric()
          })

## HAS_TESTS
#' @rdname counts-values-generator
#' @export
setMethod("Counts",
          signature(object = "DemographicArray"),
          function(object, dimtypes = NULL, dimscales = NULL) {
              .Data <- object@.Data
              metadata <- object@metadata
              new("Counts", .Data = .Data, metadata = metadata)
          })

#' Create one-dimensional object of class "Counts" or "Values"
#' 
#' Convenience functions for creating one-dimensions objects of class
#' \code{"\linkS4class{Counts}"} or \code{"\linkS4class{Values}"}.
#' 
#' When creating one-dimensional \code{"\linkS4class{Counts}"} or
#' \code{"\linkS4class{Counts}"} objects, using \code{CountsOne} and
#' \code{ValuesOne} is slightly easier than using the standard generator
#' functions \code{\link{Counts}} and \code{\link{Values}}.
#' 
#' If \code{values} is shorter than \code{labels}, it is recycled.
#' 
#' @param values Numeric vector.
#' @param labels Character vector of labels.
#' @param name Name of dimension.
#' @param dimtype \code{\link{dimtype}} of dimension.  Optional.
#' @param dimscale \code{\link{dimscale}} of dimension.  Optional.
#' @return Object of class \code{"\linkS4class{Counts}"} or
#' \code{"\linkS4class{Counts}"}.
#' @seealso \code{\link{Counts}} and \code{\link{Values}} are used to created
#' \code{"\linkS4class{Counts}"} or \code{"\linkS4class{Counts}"} of any
#' dimension.
#' @examples
#' 
#' CountsOne(values = c(-1, 1), labels = c("Female", "Male"), name = "sex")
#' 
#' ValuesOne(values = rnorm(10), labels = 0:9, name = "age")
#' 
#' ValuesOne(values = rnorm(3), labels = 1:3, name = "time", dimtype = "state")
#' 
#' ValuesOne(values = rnorm(10), labels = 0:9, name = "age", dimscale = "Points")
#' @name countsone-valuesone
NULL

## HAS_TESTS
#' @rdname countsone-valuesone
#' @export
CountsOne <- function(values, labels, name, dimtype = NULL, dimscale = NULL) {
    n.values <- length(values)
    n.labels <- length(labels)
    if (n.values < n.labels) {
        if (n.labels %% n.values == 0L)
            values <- rep(values, length.out = n.labels)
        else
            stop(gettextf("length of '%s' not a multiple of length of '%s'",
                          "labels", "values"))
    }
    else if (n.values > n.labels) {
        stop(gettextf("length of '%s' greater than length of '%s'",
                      "values", "labels"))
    }
    if (!identical(length(name), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "name", 1L))
    if (!is.null(dimtype) && !identical(length(dimtype), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "dimtype", 1L))
    if (!is.null(dimscale) && !identical(length(dimscale), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "dimscale", 1L))
    dimnames <- list(labels)
    names(dimnames) <- name
    object <- array(values, dim = n.labels, dimnames = dimnames)
    if (is.null(dimtype))
        dimtypes <- NULL
    else
        dimtypes <- structure(.Data = dimtype, names = name)
    if (is.null(dimscale))
        dimscales <- NULL
    else
        dimscales <- structure(.Data = dimscale, names = name)
    Counts(object, dimtypes = dimtypes, dimscales = dimscales)
}

