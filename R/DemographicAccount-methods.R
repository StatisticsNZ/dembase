

## NO_TESTS
#' @rdname exported-not-api
setMethod("DimScales",
          signature(object = "DemographicAccount"),
          function(object, use.names = TRUE) {
              population <- object@population
              dimtypes <- dimtypes(population,
                                   use.names = FALSE)
              DimScales <- DimScales(population,
                                     use.names = use.names)
              i.time <- match("time", dimtypes)
              DimScale.time <- DimScales[[i.time]]
              dimvalues.time <- dimvalues(DimScale.time)
              DimScale.time.new <- new("Intervals", dimvalues = dimvalues.time)
              DimScales <- replace(DimScales,
                                   list = i.time,
                                   values = list(DimScale.time.new))
              DimScales
          })

## HAS_TESTS
#' @rdname components
#' @export
setMethod("components",
          signature(object = "DemographicAccount"),
          function(object, names = NULL) {
              components <- object@components
              names.components <- object@namesComponents
              if (is.null(names)) {
                  ans <- components
                  names(ans) <- names.components
              }
              else {
                  if (identical(length(names), 0L))
                      stop(gettextf("'%s' has length %d",
                                    "names", length(names)))
                  if (any(is.na(names)))
                      stop(gettextf("'%s' has missing values",
                                    "names"))
                  i.component <- match(names, names.components, nomatch = 0L)
                  is.unmatched <- i.component == 0L
                  has.unmatched <- any(is.unmatched)
                  if (has.unmatched) {
                      i.unmatched <- which(is.unmatched)
                      first.unmatched <- names[i.unmatched[1L]]
                      stop(gettextf("account does not contain component called \"%s\"",
                                    first.unmatched))
                  }
                  ans <- components[i.component]
                  names(ans) <- names.components[i.component]
              }
              for (i in seq_along(ans))
                  ans[[i]] <- new("Counts",
                                  .Data = ans[[i]]@.Data,
                                  metadata = ans[[i]]@metadata)
              ans
          })

## HAS_TESTS
#' @rdname componentNames
#' @export
setMethod("componentNames",
          signature(object = "DemographicAccount"),
          function(object) {
              object@namesComponents
          })

## HAS_TESTS
#' @rdname componentNames
#' @export
setReplaceMethod("componentNames",
                 signature(object = "DemographicAccount"),
                 function(object, value) {
                     names.old <- object@namesComponents
                     n.value <- length(value)
                     n.names.old <- length(names.old)
                     if (!identical(n.value, n.names.old))
                         stop(gettextf("length of replacement value [%d] does not equal number of components [%d]",
                                       n.value, n.names.old))
                     object@namesComponents <- value
                     validObject(object)
                     object                                           
                 })

## NO_TESTS
#' @rdname exported-not-api
#' @export
setMethod("dimtypes",
          signature(object = "DemographicAccount"),
          function(object, use.names = TRUE) {
              population <- object@population
              dimtypes(population, use.names = use.names)
          })
              
setMethod("metadata",
          signature(object = "DemographicAccount"),
          function(object) {
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              methods::new("MetaData",
                           nms = names,
                           dimtypes = dimtypes,
                           DimScales = DimScales)
          })


## NO_TESTS
#' @rdname names-methods
#' @export
setMethod("names",
          signature(x = "DemographicAccount"),
          function(x) {
              x <- x@population
              callGeneric()
          })

## HAS_TESTS
#' @rdname population
#' @export
setMethod("population",
          signature(object = "DemographicAccount"),
          function(object) {
              as(object@population, "Counts")
          })

## HAS_TESTS
#' @rdname componentNames
#' @export
setMethod("setComponentNames",
          signature(object = "DemographicAccount"),
          function(object, value) {
              componentNames(object) <- value
              object
          })

#' @rdname internal-methods
#' @export
setMethod("show",
          signature(object = "DemographicAccount"),
          function(object) {
              metadata <- metadata(object)
              population <- object@population
              components <- object@components
              names.components <- object@namesComponents
              cat(gettextf("An object of class \"%s\"\n", class(object)))
              showMetaData(metadata)
              cat("\nall cells consistent :", all(isConsistent(object)), "\n")
              cat("\n")
              cat("--- population -------------\n\n")
              print(population@.Data)
              cat("\n\n")
              for (i in seq_along(components)) {
                  cat("--- ", names.components[i], " ----------\n\n")
                  print(components[[i]]@.Data)
                  if (i < length(components))
                      cat("\n\n")
              }
          })


              


              
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

## makePopnEnd <- function(object) {
##     .Data <- object@.Data
##     dim <- dim(object)
##     names <- names(object)
##     dimtypes <- dimtypes(object, use.names = FALSE)
##     DimScales <- DimScales(object, use.names = FALSE)
##     i.time <- match("time", dimtypes)
##     n.time <- dim[i.time]
##     DS.time <- DimScales[[i.time]]
##     dv.time <- dimvalues(DS.time)
##     DS.time.ans <- methods::new("Intervals", dimvalues = dv.time)
##     DimScales.ans <- replace(DimScales, list = i.time, values = DS.time.ans)
##     metadata.ans <- methods::new("MetaData",
##                         nms = names,
##                         dimtypes = dimtypes,
##                         DimScales = DimScales.ans)
##     dim.ans <- dim(metadata.ans)
##     dimnames.ans <- dimnames(metadata.ans)
##     .Data.ans <- .Data[slice.index(.Data, MARGIN = i.time) != 1L]
##     .Data.ans <- array(.Data.ans, dim = dim.ans, dimnames = dimnames.ans)
##     methods::new("Counts", .Data = .Data.ans, metadata = metadata.ans)
## }


        






