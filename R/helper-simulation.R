
checkIntervalArray <- function(interval) {
    dim <- dim(interval)
    dimtypes <- dimtypes(interval, use.names = FALSE)
    i.quantile <- match("quantile", dimtypes, nomatch = 0L)
    has.quantile <- i.quantile > 0L
    if (!has.quantile)
        stop(gettextf("'%s' does not have dimension with %s \"%s\"",
                      "interval", "dimtype", "quantile"))
    dim.quantile <- dim[i.quantile]
    if (!identical(dim.quantile, 2L))
        stop(gettextf("dimension of '%s' with %s \"%s\" does not have length %d",
                      "interval", "dimtype", "quantile", 2L))
    NULL
}


checkTruthArray <- function(truth) {
    dimtypes <- dimtypes(truth, use.names = FALSE)
    for (dimtype in c("iteration", "quantile"))
        if (dimtype %in% dimtypes)
            stop(gettextf("'%s' has dimension with %d \"%s\"",
                          "truth", "dimtype", dimtype))
    NULL
}


checkPointArray <- function(point) {
    dimtypes <- dimtypes(point, use.names = FALSE)
    for (dimtype in c("iteration", "quantile"))
        if (dimtype %in% dimtypes)
            stop(gettextf("'%s' has dimension with %d \"%s\"",
                          "point", "dimtype", dimtype))
    NULL
}



## Using stronger definition of compatible than usual.
## Allow rearranging of dimensions and categories, but
## do not allow collapsing, extending, or subsetting.
## Assume 'interval' and 'truth' have already be checked.
checkIntervalAndTruthCompatible <- function(interval, truth) {
    dim.int <- dim(interval)
    dim.tr <- dim(truth)
    names.int <- names(interval)
    names.tr <- names(truth)
    DimScales.int <- DimScales(interval)
    DimScales.tr <- DimScales(truth)
    i.quantile.int <- match("quantile", dimtypes.int)
    if (!setequal(names.int[-i.quantile.int], names.tr))
        stop(gettextf("'%s' and '%s' have incompatible dimensions : %s vs %s",
                      "interval",
                      "truth",
                      paste(sprintf("\"%s\"", names.int), collapse = ", "),
                      paste(sprintf("\"%s\"", names.tr), collapse = ", ")))
    for (name in names.tr) {
        len.dim.int <- dim.int[match(name, names.int)]
        len.dim.tr <- dim.tr[match(name, names.tr)]
        if (!identical(len.dim.int, len.dim.tr)) 
            stop(gettextf("\"%s\" dimensions of '%s' and '%s' have different lengths",
                          name, "interval", "truth"))
        DS.int <- DimScales.int[[name]]
        DS.tr <- DimScales.tr[[name]]
        dv.int <- dimvalues(DS.int)
        dv.tr <- dimvalues(DS.tr)
        if (!setequal(dv.int, dv.tr))
            stop(gettextf("%s for \"%s\" dimension of '%s' and '%s' incompatible",
                          "dimscales", name, "interval", "truth"))
    }
    NULL
}




getAlphaInterval <- function(interval) {
    dimtypes <- dimtypes(interval, use.names = FALSE)
    DimScales <- DimScales(interval, use.names = FALSE)
    i.quantile <- match("quantile", dimtypes)
    DS.quant <- DimScales[[i.quant]]
    dv.quant <- dimvalues(DS.quant)
    dv.lower <- dv.quant[1L]
    dv.upper <- dv.quant[2L]
    if (!isTRUE(all.equal(dv.lower, 1 - dv.upper)))
        stop(gettextf("quantiles for '%s' not symmetric",
                      "interval"))
    2 * dv.lower
}



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
    if (!setequal(names.pt, names.tr))
        stop(gettextf("'%s' and '%s' have incompatible dimensions : %s vs %s",
                      "point",
                      "truth",
                      paste(sprintf("\"%s\"", names.pt), collapse = ", "),
                      paste(sprintf("\"%s\"", names.tr), collapse = ", ")))
    for (name in names.tr) {
        len.dim.pt <- dim.pt[match(name, names.pt)]
        len.dim.tr <- dim.tr[match(name, names.tr)]
        if (!identical(len.dim.pt, len.dim.tr)) 
            stop(gettextf("\"%s\" dimensions of '%s' and '%s' have different lengths",
                          name, "point", "truth"))
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
