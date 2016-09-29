## HAS_TESTS
#' @rdname counts-values-generator
#' @export
setMethod("Values",
          signature(object = "array"),
          function(object, dimtypes = NULL, dimscales = NULL) {
              metadata <- MetaData(object = object,
                                   dimtypes = dimtypes,
                                   dimscales = dimscales)
              object <- permuteToMatchIntervalOrPointMetadata(object, metadata = metadata)
              object <- array(object@.Data,
                              dim = dim(metadata),
                              dimnames = dimnames(metadata))
              methods::new("Values", object, metadata = metadata)
          })

## HAS_TESTS
#' @rdname counts-values-generator
#' @export
setMethod("Values",
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
setMethod("Values",
          signature(object = "ANY"),
          function(object, dimtypes = NULL, dimscales = NULL) {
              object <- array(object,
                              dim = dim(object),
                              dimnames = dimnames(object))
              methods::callGeneric()
          })

## NO_TESTS
#' @rdname counts-values-generator
#' @export
setMethod("Values",
          signature(object = "DemographicArray"),
          function(object, dimtypes = NULL, dimscales = NULL) {
              object <- methods::as(object, "array")
              methods::callGeneric()
          })

## HAS_TESTS
#' @rdname countsone-valuesone
#' @export
ValuesOne <- function(values, labels, name, dimtype = NULL, dimscale = NULL) {
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
    Values(object, dimtypes = dimtypes, dimscales = dimscales)
}

