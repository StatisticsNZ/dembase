



              
## makePopnStartPeriods <- function(object, ageForward = FALSE, exactAge = FALSE) {
##     .Data <- object@.Data
##     dim <- dim(object)
##     names <- names(object)
##     dimtypes <- dimtypes(object, use.names = FALSE)
##     DimScales <- DimScales(object, use.names = FALSE)
##     i.time <- match("time", dimtypes)
##     DS.time <- DimScales[[i.time]]
##     dv.time <- dimvalues(DS.time)
##     i.age <- match("age", dimtypes, nomatch = 0L)
##     has.age <- i.age > 0L
##     if (has.age) {
##         n.age <- dim[i.age]
##         DS.age <- DimScales[[i.age]]
##         dv.age <- dimvalues(DS.age)
##     }
##     DS.time.ans <- methods::new("Intervals",
##                                 dimvalues = dv.time)
##     DimScales.ans <- replace(DimScales,
##                              list = i.time,
##                              values = list(DS.time.ans))
##     if (exactAge) {
##         exact.ages.upper <- dv.age[-1L]
##         DS.age.ans <- methods::new("Points",
##                                    dimvalues = exact.ages.upper)
##         DimScales.ans <- replace(DimScales.ans,
##                                  list = i.time,
##                                  values = list(DS.age.ans))
##     }
##     metadata.ans <- methods::new("MetaData",
##                                  nms = names,
##                                  dimtypes = dimtypes,
##                                  DimScales = DimScales.ans)
##     dim.ans <- dim(metadata.ans)
##     dimnames.ans <- dimnames(metadata.ans)
##     n.time <- dim[i.time]
##     .Data.ans <- array(0L,
##                        dim = dim.ans,
##                        dimnames = dimnames.ans)
##     .Data.start <- .Data[slice.index(.Data, MARGIN = i.time) != n.time]
##     if (has.age && ageForward) {
##         ind.ans <- slice.index(.Data.ans, MARGIN = i.age)
##         ind.start <- slice.index(.Data.start, MARGIN = i.age)
##         .Data.ans[ind.ans != 1L] <- .Data.start[ind.start != n.age]
##         .Data.ans[ind.ans == n.age] <- .Data.ans[ind.ans == n.age] +
##             .Data.start[ind.start == n.age]
##     }
##     else
##         .Data.ans[] <- .Data.start
##     methods::new("Counts",
##                  .Data = .Data.ans,
##                  metadata = metadata.ans)
## }



              

## NO_TESTS
#' @rdname exported-not-api
setMethod("dimtypes",
          signature(object = "DemographicAccount"),
          function(object) {
              population <- object@population
              dimtypes(population)
          })
              
setMethod("metadata",
          signature(object = "Movements"),
          function(object) {
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              methods::new("MetaData",
                  nms = names,
                  dimtypes = dimtypes,
                  DimScales = DimScales)
          })



makePopnStartPeriods <- function(object, ageForward = FALSE, exactAge = FALSE) {
    .Data <- object@.Data
    dim <- dim(object)
    names <- names(object)
    dimtypes <- dimtypes(object, use.names = FALSE)
    DimScales <- DimScales(object, use.names = FALSE)
    i.time <- match("time", dimtypes)
    DS.time <- DimScales[[i.time]]
    dv.time <- dimvalues(DS.time)
    i.age <- match("age", dimtypes, nomatch = 0L)
    has.age <- i.age > 0L
    if (has.age) {
        n.age <- dim[i.age]
        DS.age <- DimScales[[i.age]]
        dv.age <- dimvalues(DS.age)
    }
    DS.time.ans <- methods::new("Intervals",
                                dimvalues = dv.time)
    DimScales.ans <- replace(DimScales,
                             list = i.time,
                             values = list(DS.time.ans))
    if (exactAge) {
        exact.ages.upper <- dv.age[-1L]
        DS.age.ans <- methods::new("Points",
                                   dimvalues = exact.ages.upper)
        DimScales.ans <- replace(DimScales.ans,
                                 list = i.time,
                                 values = list(DS.age.ans))
    }
    metadata.ans <- methods::new("MetaData",
                                 nms = names,
                                 dimtypes = dimtypes,
                                 DimScales = DimScales.ans)
    dim.ans <- dim(metadata.ans)
    dimnames.ans <- dimnames(metadata.ans)
    n.time <- dim[i.time]
    .Data.ans <- array(0L,
                       dim = dim.ans,
                       dimnames = dimnames.ans)
    .Data.start <- .Data[slice.index(.Data, MARGIN = i.time) != n.time]
    if (has.age && ageForward) {
        ind.ans <- slice.index(.Data.ans, MARGIN = i.age)
        ind.start <- slice.index(.Data.start, MARGIN = i.age)
        .Data.ans[ind.ans != 1L] <- .Data.start[ind.start != n.age]
        .Data.ans[ind.ans == n.age] <- .Data.ans[ind.ans == n.age] +
            .Data.start[ind.start == n.age]
    }
    else
        .Data.ans[] <- .Data.start
    methods::new("Counts",
                 .Data = .Data.ans,
                 metadata = metadata.ans)
}

makePopnEnd <- function(object) {
    .Data <- object@.Data
    dim <- dim(object)
    names <- names(object)
    dimtypes <- dimtypes(object, use.names = FALSE)
    DimScales <- DimScales(object, use.names = FALSE)
    i.time <- match("time", dimtypes)
    n.time <- dim[i.time]
    DS.time <- DimScales[[i.time]]
    dv.time <- dimvalues(DS.time)
    DS.time.ans <- methods::new("Intervals", dimvalues = dv.time)
    DimScales.ans <- replace(DimScales, list = i.time, values = DS.time.ans)
    metadata.ans <- methods::new("MetaData",
                        nms = names,
                        dimtypes = dimtypes,
                        DimScales = DimScales.ans)
    dim.ans <- dim(metadata.ans)
    dimnames.ans <- dimnames(metadata.ans)
    .Data.ans <- .Data[slice.index(.Data, MARGIN = i.time) != 1L]
    .Data.ans <- array(.Data.ans, dim = dim.ans, dimnames = dimnames.ans)
    methods::new("Counts", .Data = .Data.ans, metadata = metadata.ans)
}


        






