

## HAS_TESTS
## Using stronger definition of compatible than usual.
## Allow rearranging of dimensions and categories, but
## do not allow collapsing, extending, or subsetting.
## Assume 'interval' and 'truth' have already be checked.
checkIntervalAndTruthArrayCompatible <- function(interval, truth) {
    dim.int <- dim(interval)
    dim.tr <- dim(truth)
    names.int <- names(interval)
    names.tr <- names(truth)
    dimtypes.int <- dimtypes(interval, use.names = FALSE)
    DimScales.int <- DimScales(interval, use.names = FALSE)
    DimScales.tr <- DimScales(truth, use.names = FALSE)
    i.quantile.int <- match("quantile", dimtypes.int)
    ## have same names, apart from quantile
    if (!setequal(names.int[-i.quantile.int], names.tr))
        stop(gettextf("'%s' and '%s' have incompatible dimensions : %s vs %s",
                      "interval",
                      "truth",
                      paste(sprintf("\"%s\"", names.int), collapse = ", "),
                      paste(sprintf("\"%s\"", names.tr), collapse = ", ")))
    for (i.tr in seq_along(names.tr)) {
        name <- names.tr[i.tr]
        i.int <- match(name, names.int)
        len.dim.int <- dim.int[i.int]
        len.dim.tr <- dim.tr[i.tr]
        ## dimensions have same length
        if (!identical(len.dim.int, len.dim.tr)) 
            stop(gettextf("\"%s\" dimensions of '%s' and '%s' have different lengths",
                          name, "interval", "truth"))
        DS.int <- DimScales.int[[i.int]]
        DS.tr <- DimScales.tr[[i.tr]]
        dv.int <- dimvalues(DS.int)
        dv.tr <- dimvalues(DS.tr)
        ## dimensions have same dimvalues
        if (!setequal(dv.int, dv.tr))
            stop(gettextf("%s for \"%s\" dimension of '%s' and '%s' incompatible",
                          "dimscales", name, "interval", "truth"))
    }
    NULL
}

## HAS_TESTS
checkIntervalAndTruthNumericCompatible <- function(interval = interval,
                                                   truth = truth) {
    dim <- dim(interval)
    n.dim <- length(dim)
    if (n.dim > 1L) {
        dimtypes <- dimtypes(interval, use.names = FALSE)
        i.quantile <- match("quantile", dimtypes)
        if (any(dim[-i.quantile] != 1L))
            stop(gettextf("'%s' is a single number but '%s' has dimensions (other than the \"%s\" dimension) with length not equal to %d",
                          "truth", "interval", "quantile", 1L))
    }
    NULL
}

## HAS_TESTS
checkIntervalArray <- function(interval) {
    dim <- dim(interval)
    dimtypes <- dimtypes(interval, use.names = FALSE)
    i.quantile <- match("quantile", dimtypes, nomatch = 0L)
    ## has quantile dimension
    has.quantile <- i.quantile > 0L
    if (!has.quantile)
        stop(gettextf("'%s' does not have dimension with %s \"%s\"",
                      "interval", "dimtype", "quantile"))
    ## quantile dimension has length 2
    dim.quantile <- dim[i.quantile]
    if (!identical(dim.quantile, 2L))
        stop(gettextf("dimension of '%s' with %s \"%s\" does not have length %d",
                      "interval", "dimtype", "quantile", 2L))
    NULL
}

## HAS_TESTS
## Using stronger definition of compatible than usual.
## Allow rearranging of dimensions and categories, but
## do not allow collapsing, extending, or subsetting.
## Assume 'point' and 'truth' have already be checked.
checkPointAndTruthCompatible <- function(point, truth) {
    dim.pt <- dim(point)
    dim.tr <- dim(truth)
    names.pt <- names(point)
    names.tr <- names(truth)
    DimScales.pt <- DimScales(point)
    DimScales.tr <- DimScales(truth)
    ## have same dimension names
    if (!setequal(names.pt, names.tr))
        stop(gettextf("'%s' and '%s' have incompatible dimensions : %s vs %s",
                      "point",
                      "truth",
                      paste(sprintf("\"%s\"", names.pt), collapse = ", "),
                      paste(sprintf("\"%s\"", names.tr), collapse = ", ")))
    for (name in names.tr) {
        ## dimensions have same lengths
        len.dim.pt <- dim.pt[match(name, names.pt)]
        len.dim.tr <- dim.tr[match(name, names.tr)]
        if (!identical(len.dim.pt, len.dim.tr)) 
            stop(gettextf("\"%s\" dimensions of '%s' and '%s' have different lengths",
                          name, "point", "truth"))
        ## dimensions have same dimvalues
        DS.pt <- DimScales.pt[[name]]
        DS.tr <- DimScales.tr[[name]]
        dv.pt <- dimvalues(DS.pt)
        dv.tr <- dimvalues(DS.tr)
        if (!setequal(dv.pt, dv.tr))
            stop(gettextf("%s for \"%s\" dimension of '%s' and '%s' incompatible",
                          "dimscales", name, "point", "truth"))
    }
    NULL
}

## HAS_TESTS
checkPointArray <- function(point) {
    dimtypes <- dimtypes(point, use.names = FALSE)
    for (dimtype in c("iteration", "quantile"))
        if (dimtype %in% dimtypes)
            stop(gettextf("'%s' has dimension with %s \"%s\"",
                          "point", "dimtype", dimtype))
    NULL
}

## HAS_TESTS
checkTruthArray <- function(truth) {
    dimtypes <- dimtypes(truth, use.names = FALSE)
    ## no iteration or quantile dimensions
    for (dimtype in c("iteration", "quantile"))
        if (dimtype %in% dimtypes)
            stop(gettextf("'%s' has dimension with %s \"%s\"",
                          "truth", "dimtype", dimtype))
    NULL
}

## HAS_TESTS
checkTruthNumeric <- function(truth) {
    if (!identical(length(truth), 1L))
        stop(gettextf("'%s' is a number but does not have length %d",
                      "truth", 1L))
}

## HAS_TESTS
getAlphaInterval <- function(interval) {
    dimtypes <- dimtypes(interval, use.names = FALSE)
    DimScales <- DimScales(interval, use.names = FALSE)
    i.quantile <- match("quantile", dimtypes)
    DS.quantile <- DimScales[[i.quantile]]
    dv.quantile <- dimvalues(DS.quantile)
    dv.lower <- dv.quantile[1L]
    dv.upper <- dv.quantile[2L]
    if (!isTRUE(all.equal(dv.lower, 1 - dv.upper)))
        stop(gettextf("quantiles for '%s' not symmetric",
                      "interval"))
    2 * dv.lower
}

## HAS_TESTS
splitLowerUpper <- function(interval) {
    dimtypes <- dimtypes(interval, use.names = FALSE)
    i.quantile <- match("quantile", dimtypes)
    lower <- slab(interval,
                  dimension = i.quantile,
                  elements = 1L,
                  drop = "dimension")
    upper <- slab(interval,
                  dimension = i.quantile,
                  elements = 2L,
                  drop = "dimension")
    list(lower = lower, upper = upper)
}
