
## HAS_TESTS
Population <- function(object) {
    ## is Counts
    if (!methods::is(object, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "population", class(object)))
    ## extract slots
    dimtypes <- dimtypes(object, use.names = FALSE)
    DimScales <- DimScales(object, use.names = FALSE)
    ## is integer
    object <- tryCatch(toInteger(object),
                       error = function(e) e)
    if (methods::is(object, "error"))
        stop(gettextf("'%s' invalid : %s",
                      "population", object$message))
    ## time, age, cohort dimensions
    i.time <- match("time", dimtypes, nomatch = 0L)
    i.age <- match("age", dimtypes, nomatch = 0L)
    i.cohort <- match("cohort", dimtypes, nomatch = 0L)
    has.time <- i.time > 0L
    has.age <- i.age > 0L
    has.cohort <- i.cohort > 0L
    if (!has.time)
        stop(gettextf("'%s' does not have dimension with dimtype \"%s\"",
                      "population", "time"))
    DimScale.time <- DimScales[[i.time]]
    if (!methods::is(DimScale.time, "Points"))
        stop(gettextf("dimension of '%s' with dimtype \"%s\" has dimscale \"%s\"",
                      "population", "time", class(DimScale.time)))
    if (length(DimScale.time) < 2L)
        stop(gettextf("dimension of '%s' with dimtype \"%s\" has length %d",
                      "population", "time", length(DimScale.time)))
    if (has.age) {
        DimScale.age <- DimScales[[i.age]]
        if (!methods::is(DimScale.age, "Intervals"))
            stop(gettextf("dimension of '%s' with dimtype \"%s\" has dimscale \"%s\"",
                          "population", "age", class(DimScale.age)))
        if (length(DimScale.age) < 2L)
            stop(gettextf("dimension of '%s' with dimtype \"%s\" has length %d",
                          "population", "age", length(DimScale.age)))
        dv.age <- dimvalues(DimScale.age)
        if (is.infinite(dv.age[1L]))
            stop(gettextf("'%s' invalid : first interval of dimension with dimtype \"%s\" is open",
                          "population", "age"))
        if (is.finite(dv.age[length(dv.age)]))
            stop(gettextf("'%s' invalid : last interval of dimension with dimtype \"%s\" is closed",
                          "population", "age"))
    }
    if (has.cohort)
        stop(gettextf("'%s' has dimension with dimtype \"%s\"",
                      "population", "cohort"))
    ## origin-destination
    is.orig.dest <- any(dimtypes == "origin")
    if (is.orig.dest)
        stop(gettextf("'%s' has dimensions with dimtypes \"%s\" and \"%s\"",
                      "population", "origin", "destination"))
    ## parent-child
    is.parent.child <- any(dimtypes == "parent")
    if (is.parent.child)
        stop(gettextf("'%s' has dimensions with dimtypes \"%s\" and \"%s\"",
                      "population", "parent", "child"))
    ## regular
    is.regular <- tryCatch(hasRegularAgeTime(object),
                           error = function(e) e)
    if (methods::is(is.regular, "error"))
        stop(gettextf("'%s' does not have regular age-time plan : %s",
                      "population", is.regular$message))
    ## positive length
    if (length(object) == 0L)
        stop(gettextf("'%s' has length %d",
                      "population", 0L))
    ## negatives
    if (any(object[!is.na(object)] < 0L))
        stop(gettextf("'%s' has negative values",
                      "population"))
    methods::new("Population",
        .Data = object@.Data,
        metadata = object@metadata)
}

