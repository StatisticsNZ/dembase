
## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("[",
          signature(x = "MetaData", i = "numeric"),
          function(x, i) {
              if (any(is.na(i)))
                  stop(gettextf("'%s' has missing values", "i"))
              s <- seq.int(from = 0L, to = length(x))
              if (!all(abs(i) %in% s))
                  stop(gettextf("'%s' outside valid range", "i"))
              names.after <- names(x)[i]
              dimtypes.after <- dimtypes(x, use.names = FALSE)[i]
              DimScales.after <- DimScales(x, use.names = FALSE)[i]
              ## correct dimtypes and names for lost pairs
              names.pairs <- getNamesPairs(names = names.after)
              lost.pair <- !(names.pairs %in% names.after)
              names.after[lost.pair] <- removeSuffixes(names = names.after[lost.pair])
              dimtypes.after[lost.pair] <- "state"
              methods::new("MetaData",
                           nms = names.after,
                           dimtypes = dimtypes.after,
                           DimScales = DimScales.after)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("[",
          signature(x = "MetaData", i = "character"),
          function(x, i) {
              if (any(is.na(i)))
                  stop(gettextf("'%s' has missing values", "i"))
              i <- match(i, names(x))
              if (any(is.na(i)))
                  stop(gettextf("'%s' outside valid range", "i"))
              methods::callGeneric(x = x, i = i)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("[",
          signature(x = "MetaData", i = "logical"),
          function(x, i) {
              if (any(is.na(i)))
                  stop(gettextf("'%s' has missing values", "i"))
              s <- seq_along(names(x))
              i <- s[i]
              if (any(is.na(i)))
                  stop(gettextf("'%s' outside valid range", "i"))
              methods::callGeneric(x = x, i = i)
          })

## HAS_TESTS
#' @rdname ageMinMax
#' @export
setMethod("ageMax",
          signature(object = "MetaData"),
          function(object) {
              ageMinMax(object = object,
                        min = FALSE)
          })

## HAS_TESTS
#' @rdname ageMinMax
#' @export
setReplaceMethod("ageMax",
                 signature(object = "MetaData"),
                 function(object, value) {
                     ageMinMaxReplace(object = object,
                                      value = value,
                                      min = FALSE)
                 })

## HAS_TESTS
#' @rdname ageMinMax
#' @export
setMethod("ageMin",
          signature(object = "MetaData"),
          function(object) {
              ageMinMax(object = object,
                        min = TRUE)
          })

## HAS_TESTS
#' @rdname ageMinMax
#' @export
setReplaceMethod("ageMin",
                 signature(object = "MetaData"),
                 function(object, value) {
                     ageMinMaxReplace(object = object,
                                      value = value,
                                      min = TRUE)
                 })

## HAS_TESTS - via DemographicArray-methods
#' @rdname ageTimeStep
#' @export
setMethod("ageTimeStep",
          signature(object = "MetaData"),
          function(object) {
              hasRegularAgeTime(object)
              i.age <- match("age", dimtypes(object), nomatch = 0L)
              i.time <- match("time", dimtypes(object), nomatch = 0L)
              has.age <- i.age > 0L
              has.time <- i.time > 0L
              if (has.age) {
                  DimScale.age <- DimScales(object)[[i.age]]
                  age.steps <- stepLengths(DimScale.age)
                  if (length(age.steps) > 0L)
                      return(age.steps[1L])
              }
              if (has.time) {
                  DimScale.time <- DimScales(object)[[i.time]]
                  time.steps <- stepLengths(DimScale.time)
                  if (length(time.steps) > 0L)
                      return(time.steps[1L])
              }
              if (has.age) {
                  if (has.time)
                      stop(gettextf("neither %s dimension nor %s dimension has any steps",
                                    "age", "time"))
                  else
                      stop(gettextf("%s dimension does not have any steps",
                                    "age"))
              }
              else {
                  if (has.time)
                      stop(gettextf("%s dimension does not have any steps",
                                    "time"))
                  else
                      stop(gettextf("does not have %s or %s dimensions",
                                    "age", "time"))
              }
          })

#' @rdname pairAligned
#' @export
setMethod("pairAligned",
          signature(object = "MetaData"),
          function(object, base = NULL) {
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              dimtypes.with.pairs <- getDimtypesWithPairs(firstElementOnly = TRUE)
              names.first <- names[dimtypes %in% dimtypes.with.pairs]
              if (!is.null(base)) {
                  ## base may legitimately match more than one pair
                  base.names.first <- removeSuffixes(names.first)
                  base.found <- base %in% base.names.first
                  if (any(!base.found))
                      stop(gettextf("\"%s\" is not a valid base name", base[!base.found][1L]))
                  names.first <- names.first[base.names.first %in% base]
              }
              indices.first <- match(names.first, names, nomatch = 0L)
              for (i in seq_along(indices.first)) {
                  ## object potentially changed at each iteration
                  name.first <- names.first[i]
                  index.first <- indices.first[i]
                  name.second <- getNamesPairs(name.first)
                  index.second <- match(name.second, names)
                  dv.first <- dimvalues(DimScales(object)[[index.first]])
                  dv.second <- dimvalues(DimScales(object)[[index.second]])
                  if (!identical(length(dv.first), length(dv.second)))
                      stop(gettextf("dimensions \"%s\" and \"%s\" have different lengths",
                                    name.first, name.second))
                  if (!setequal(dv.first, dv.second))
                      stop(gettextf("dimensions \"%s\" and \"%s\" have different categories",
                                    name.first, name.second))
                  if (!all(all(dv.first == dv.second)))
                      stop(gettextf("dimensions \"%s\" and \"%s\" have same categories, but in different order",
                                    name.first, name.second))
              }
              TRUE
          })

## HAS_TESTS (via Counts)
#' @rdname exported-not-api
#' @export
setMethod("collapse",
          signature(object = "MetaData", transform = "CollapseTransform"),
          function(object, transform, concordances = list()) {
              ## preliminaries
              dim.before <- dim(object)
              if (!identical(dim.before, transform@dimBefore))
                  stop(gettextf("'%s' does not have the dimensions expected by '%s'",
                                "object", "transform"))
              names.before <- names(object)
              dimtypes.before <- dimtypes(object, use.names = FALSE)
              DimScales.before <- DimScales(object, use.names = FALSE)
              dims <- transform@dims
              indices <- transform@indices
              concordances <- tidyConcordanceList(concordances = concordances,
                                                  object = object)
              cellsAggregated <- function(x) any(duplicated(x[x != 0L]))
              ## if object has quantile dimension, prohibit aggregation of cells
              if ("quantile" %in% dimtypes(object)) {
                  if (any(sapply(indices, cellsAggregated)))
                      stop(gettextf("attempt to aggregate cells when there is a dimension with dimtype \"%s\"",
                                    "quantile"))
              }
              ## if an object has interations dimension, do not allow
              ## cells to be aggregated along that dimension
              i.iter <- match("iteration", dimtypes(object), nomatch = 0L)
              has.iter <- i.iter > 0L
              if (has.iter) {
                  if (cellsAggregated(indices[[i.iter]]))
                      stop(gettext("attempt to collapse cells across iterations"))
              }
              ## new names and DimScales
              inverse.dims <- match(seq_along(dims), dims, nomatch = 0L)
              names.after <- names.before[inverse.dims]
              DimScales.after <- mapply(collapseDimScale,
                                        object = DimScales.before[inverse.dims],
                                        index = indices[inverse.dims],
                                        concordance = concordances[inverse.dims],
                                        SIMPLIFY = FALSE,
                                        USE.NAMES = FALSE)
              ## first attempt at new dimtypes
              dimtypes.after <- dimtypes.before[inverse.dims]
              ## correct dimtypes and names for lost pairs
              names.pairs <- getNamesPairs(names = names.after)
              lost.pair <- !(names.pairs %in% names.after)
              names.after[lost.pair] <- removeSuffixes(names = names.after[lost.pair])
              dimtypes.after[lost.pair] <- "state"
              ## find cases where "Intervals" dimscales were coerced to "Categories",
              ## and modify dimtypes accordingly
              is.age.time.cohort <- dimtypes.after %in% c("age", "time", "cohort")
              is.categories <- sapply(DimScales.after, methods::is,"Categories")
              coerced <- is.age.time.cohort & is.categories
              dimtypes.after[coerced] <- "state"
              methods::new("MetaData",
                  nms = names.after,
                  dimtypes = dimtypes.after,
                  DimScales = DimScales.after)
          })

#' @rdname exported-not-api
#' @export
setMethod("DimScales",
          signature(object = "MetaData"),
          function(object, use.names = TRUE) {
            ans <- object@DimScales
            if (use.names)
              names(ans) <- names(object)
            ans
          })

#' @export
dim.MetaData <- function(x) sapply(DimScales(x, use.names = FALSE), length)

#' @rdname internal-methods
#' @export
setMethod("dim",
          signature(x = "MetaData"),
          dim.MetaData)

#' @export
dimnames.MetaData <- function(x) {
    labelsOrNULL <- function(y) if (length(y) > 0L) labels(y) else NULL
    lapply(DimScales(x), labelsOrNULL)
}

#' @rdname internal-methods
#' @export
setMethod("dimnames",
          signature(x = "MetaData"),
          dimnames.MetaData)
          
#' @rdname dimscales
#' @export
setMethod("dimscales",
          signature(object = "MetaData"),
          function(object, use.names = TRUE)
          sapply(DimScales(object, use.names = use.names), class))

#' @rdname dimscales
#' @export
setReplaceMethod("dimscales",
                 signature(object = "MetaData"),
                 function(object, value) {
                   value <- as.character(value)
                   n.object <- length(object)
                   n.value <- length(value)
                   if (!identical(n.object, n.value)) {
                     if (!identical(n.object %% n.value, 0L))
                       stop("number of items to replace is not a multiple of replacement length")
                     value <- rep(value, length.out = n.object)
                   }
                   DimScales <- DimScales(object, use.names = FALSE)
                   for (i in seq_along(value))
                     DimScales[[i]] <- methods::as(DimScales[[i]], value[i])
                   methods::new(class(object),
                       nms = names(object),
                       dimtypes = dimtypes(object, use.names = FALSE),
                       DimScales = DimScales)
                 })

#' @rdname dimtypes
#' @export
setMethod("dimtypes",
          signature(object = "MetaData"),
          function(object, use.names = TRUE) {
            ans <- object@dimtypes
            if (use.names)
              names(ans) <- names(object)
            ans
          })

#' @rdname dimtypes
#' @export
setReplaceMethod("dimtypes",
                 signature(object = "MetaData"),
                 function(object, value) {
                     value <- as.character(value)
                     n.object <- length(object)
                     n.value <- length(value)
                     if (!identical(n.object, n.value)) {
                         if (!identical(n.object %% n.value, 0L))
                             stop("number of items to replace is not a multiple of replacement length")
                         value <- rep(value, length.out = n.object)
                     }
                     DimScales <- DimScales(object, use.names = FALSE)
                     dimscales <- dimscales(object, use.names = FALSE)
                     for (i in seq_along(value)) {
                         possible.dimscales <- getPossibleDimscales(value[i])
                         current.dimscale <- dimscales[i]
                         if (!(current.dimscale %in% possible.dimscales)) {
                             current.DimScale <- DimScales[[i]]
                             n.possible.dimscales <- length(possible.dimscales)
                             found.dimscale <- FALSE
                             j <- 1L
                             while (!found.dimscale && j <= n.possible.dimscales) {
                                 possible.dimscale <- possible.dimscales[j]
                                 return.value <- tryCatch(methods::as(current.DimScale, possible.dimscale),
                                                          error = function(e) e)
                                 found.dimscale <- methods::is(return.value, "DimScale")
                                 j <- j + 1L
                             }
                             if (found.dimscale)
                                 DimScales[[i]] <- return.value
                             else
                                 stop(return.value)
                         }
                     }
                     methods::new(class(object),
                         nms = names(object),
                         dimtypes = value,
                         DimScales = DimScales)
                 })

## HAS_TESTS (via DemographicArray)
#' @rdname hasRegularAgeTime
#' @export
setMethod("hasRegularAgeTime",
          signature(object = "MetaData"),
          function(object) {
              i.age <- match("age", dimtypes(object), nomatch = 0L)
              i.time <- match("time", dimtypes (object), nomatch = 0L)
              has.age <- i.age > 0L
              has.time <- i.time > 0L
              if (has.age) {
                  DimScale.age <- DimScales(object)[[i.age]]
                  age.steps <- stepLengths(DimScale.age)
                  has.age.steps <- length(age.steps) > 0L
                  if (has.age.steps) {
                      age.steps.finite <- is.finite(age.steps)
                      if (any(age.steps.finite))
                          age.steps <- age.steps[age.steps.finite]
                      first.age.step <- age.steps[1L]
                      if (!all(age.steps == first.age.step))
                          stop(gettextf("%s steps unequal", "age"))
                  }
              }
              if (has.time) {
                  DimScale.time <- DimScales(object)[[i.time]]
                  time.steps <- stepLengths(DimScale.time)
                  has.time.steps <- length(time.steps) > 0L
                  if (has.time.steps) {
                      time.steps.finite <- is.finite(time.steps)
                      if (any(time.steps.finite))
                          time.steps <- time.steps[time.steps.finite]
                      first.time.step <- time.steps[1L]
                      if (!all(time.steps == first.time.step))
                          stop(gettextf("%s steps unequal", "time"))
                  }
              }
              if (has.age && has.age.steps && has.time && has.time.steps) {
                  if (is.finite(first.age.step)
                      & is.finite(first.time.step)
                      & (first.age.step != first.time.step))
                      stop(gettextf("%s step [%s] does not equal %s step [%s]",
                                    "age", first.age.step, "time", first.time.step))
              }
              TRUE
          })

length.MetaData <- function(x) length(names(x))

#' @rdname internal-methods
#' @export
setMethod("length",
          signature(x = "MetaData"),
          length.MetaData)

## NO_TESTS
#' @rdname limits
#' @export
setMethod("limits",
          signature(object = "MetaData"),
          function(object) {
              getLimits <- function(x) {
                  n <- length(x)
                  if (n == 0L)
                      rep(as.character(NA), times = 2L)
                  else if (n == 1L)
                      rep(x, times = 2L)
                  else
                      x[c(1L, n)]
              }
              ans <- dimnames(object)
              ans <- lapply(ans, getLimits)
              ans <- data.frame(ans)
              rownames(ans) <- c("first", "last")
              ans
          })          

## HAS_TESTS (via DemographicArray)
#' @rdname midpoints
#' @export
setMethod("midpoints",
          signature(object = "MetaData", dimension = "integer"),
          function(object, dimension) {
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              DimScales.selected <- DimScales[dimension]
              not.intervals <- !sapply(DimScales.selected, methods::is,  "Intervals")
              n.not.intervals <- sum(not.intervals)
              if (n.not.intervals > 0L) {
                  selected.names <- names(object)[dimension]
                  stop(sprintf(ngettext(n.not.intervals,
                                        "dimension %s does not have dimscale \"%s\"",
                                        "dimensions %s do not have dimscale \"%s\""),
                               paste(dQuote(selected.names[not.intervals]), collapse = ", "),
                               "Intervals"))
              }
              DimScales[dimension] <- lapply(DimScales[dimension],
                                             intervalsToPoints)
              i.triangle <- match("triangle", dimtypes, nomatch = 0L)
              has.triangle <- i.triangle > 0L
              if (has.triangle) {
                  dimtypes[i.triangle] <- "state"
                  DimScales[[i.triangle]] <- as(DimScales[[i.triangle]], "Categories")
              }
              methods::new(class(object),
                           nms = names,
                           dimtypes = dimtypes,
                           DimScales = DimScales)
          })

#' @rdname names-methods
#' @export
setMethod("names",
          signature(x = "MetaData"),
          function(x) x@nms)

#' @rdname names-methods
#' @export
setReplaceMethod("names",
                 signature(x = "MetaData"),
                 function(x, value) {
                   value <- as.character(value)
                   x@nms <- value
                   methods::validObject(x)
                   x
                 })

## HAS_TESTS
#' @rdname resetIterations
#' @export
setMethod("resetIterations",
          signature(object = "MetaData"),
          function(object) {
              dimtypes <- dimtypes(object, use.names = FALSE)
              i.iter <- match("iteration", dimtypes, nomatch = 0L)
              if (i.iter > 0L) {
                  dim <- dim(object)
                  n.iter <- dim[i.iter]
                  dimvalues <- seq_len(n.iter)
                  DimScale <- methods::new("Iterations", dimvalues = dimvalues)
                  object@DimScales[[i.iter]] <- DimScale
              }
              object
          })

## HAS_TESTS
#' @rdname ageMinMax
#' @export
setMethod("setAgeMax",
          signature(object = "MetaData"),
          function(object, value) {
              ageMinMaxReplace(object = object,
                               value = value,
                               min = FALSE)
          })

## HAS_TESTS
#' @rdname ageMinMax
#' @export
setMethod("setAgeMin",
          signature(object = "MetaData"),
          function(object, value) {
              ageMinMaxReplace(object = object,
                               value = value,
                               min = TRUE)
          })

#' @rdname internal-methods
#' @export
setMethod("show",
          signature(object = "MetaData"),
          function(object) {
            cat(gettextf("An object of class \"%s\"\n\n",
                         class(object)))
            showMetaData(object)
          })








