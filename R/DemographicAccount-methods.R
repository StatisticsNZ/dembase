
#' @rdname exported-not-api
setMethod("DimScales",
          signature(object = "Movements"),
          function(object) {
              population <- object@population
              components <- object@components
              first.component <- components[[1L]]
              dimtypes.popn <- dimtypes(population, use.names = FALSE)
              dimtypes.comp <- dimtypes(first.component, use.names = FALSE)
              DimScales.popn <- DimScales(population, use.names = FALSE)
              DimScales.comp <- DimScales(first.component, use.names = FALSE)
              i.time.popn <- match("time", dimtypes.popn)
              i.time.comp <- match("time", dimtypes.comp)
              DS.time.comp <- DimScales.comp[[i.time.comp]]
              replace(DimScales.popn,
                      list = i.time.popn,
                      values = DS.time.comp)
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

setMethod("accession",
          signature(object = "Movements"),
          function(object) {
              population <- object@population
              components <- object@components
              dimtypes <- dimtypes(population, use.names = FALSE)
              has.age <- "age" %in% dimtypes
              if (has.age) {
                  ans <- makePopnStart(accession)
                  for (component in components) {
                      accession.component <- accessionComponent(component = component,
                                                                population = population)
                      ans <- ans + accession.component
                  }
                  ans
              }
              else
                  NULL
          })


makePopnStart <- function(object, ageForward = TRUE) {
    .Data <- object@.Data
    dim <- dim(object)
    names <- names(object)
    dimtypes <- dimtypes(object, use.names = FALSE)
    DimScales <- DimScales(object, use.names = FALSE)
    i.time <- match("time", dimtypes)
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
    n.time <- dim[i.time]
    .Data.ans <- array(0L, dim = dim.ans, dimnames = dimnames.ans)
    .Data.start <- .Data[slice.index(.Data, MARGIN = i.time) != n.time]
    i.age <- match("age", dimtypes, nomatch = 0L)
    has.age <- i.age > 0L
    if (has.age && ageForward) {
        n.age <- dim[i.age]
        ind.ans <- slice.index(.Data.ans, MARGIN = i.age)
        ind.start <- slice.index(.Data.start, MARGIN = i.age)
        .Data.ans[ind.ans != 1L] <- .Data.start[ind.start != n.age]
        .Data.ans[ind.ans == n.age] <- .Data.ans[ind.ans == n.age] +
            .Data.start[ind.start == n.age]
    }
    else
        .Data.ans[] <- .Data.start
    methods::new("Counts", .Data = .Data.ans, metadata = metadata.ans)
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


## setMethod("isConsistent",
##           signature(object = "DemographicAccount"),
##           function(object) {
##               population <- object@population
##               components <- object@components
##               popn.obtained <- makePopnStart(population, ageForward = TRUE)
##               for (component in components) {
##                   contrib.popn.end <- contribPopnEnd(component = component,
##                                                      population = population)
##                   popn.obtained <- popn.obtained + contrib.popn.end
##               }
##               popn.obtained <- makePopnEnd(population)
##               is.consistent <- popn.expected == popn.obtained
##               if (all(is.consistent))
##                   TRUE
##               else
##                   is.consistent
##           })

        






