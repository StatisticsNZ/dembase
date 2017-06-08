
## HAS_TESTS
#' @export
setAs(from = "DemographicArray", to = "Counts",
      function(from) {
          .Data <- from@.Data
          metadata <- metadata(from)
          methods::new("Counts", .Data = .Data, metadata = metadata)
      })

## HAS_TESTS
#' @export
setAs(from = "DemographicArray", to = "Values",
      function(from) {
          .Data <- from@.Data
          metadata <- metadata(from)
          methods::new("Values", .Data = .Data, metadata = metadata)
      })

## HAS_TESTS
#' @export
setAs(from = "DemographicArray",
      to = "matrix",
      def = function(from) {
        if (!identical(length(dim(from)), 2L))
          stop("object does not have two dimensions")
        ans <- from@.Data
        dimnames(ans) <- dimnames(from)
        ans
      },
      replace = function(from, value) {
        from@.Data <- value
        from
      })

## HAS_TESTS
#' @export
`[.DemographicArray` <- function(x, i, j, ..., drop = TRUE) {
    nargs <- nargs() - methods::hasArg(drop)
    if (nargs == 1L)
        return(x)
    else if (nargs == 2L)
        .Data <- methods::callGeneric(x = x@.Data, i = i, drop = FALSE)
    else
        .Data <- methods::callGeneric(x = x@.Data, i = i, j = j, ..., drop = FALSE)
    dimnames.without.drop <- dimnames(.Data)
    if (drop)
        .Data <- drop(.Data)
    dim.after <- dim(.Data)
    if (is.null(dim.after))
        return(.Data)
    dim.before <- dim(x)
    dims <- match(names(x), names(dimnames(.Data)), nomatch = 0L)
    indices <- mapply(match,
                      x = dimnames(x),
                      table = dimnames.without.drop,
                      nomatch = 0L,
                      USE.NAMES = FALSE,
                      SIMPLIFY = FALSE)
    transform <- methods::new("CollapseTransform",
                     dims = dims,
                     indices = indices,
                     dimBefore = dim.before,
                     dimAfter = dim.after)
    metadata <- collapse(metadata(x), transform = transform)
    .Data <- array(.Data, dim = dim(metadata), dimnames = dimnames(metadata))
    methods::new(class(x), .Data = .Data, metadata = metadata)
}

#' @rdname internal-methods
#' @export
setMethod("[",
          signature(x = "DemographicArray"),
          `[.DemographicArray`)


## HAS_TESTS
#' @rdname Statistical-Functions
#' @export
setMethod("Summary",
          signature = "DemographicArrayOrNumeric",
          function(x, ..., na.rm = FALSE) {
              dots <- list(...)
              n.dots <- length(dots)
              ## if ((length(x) == 0L) && (n.dots > 0L) && (.Generic != "range"))
              ##     return(callGeneric(..., na.rm = na.rm))
              if (methods::is(x, "DemographicArray")) {
                  .Data.x <- x@.Data
                  names.x <- names(x)
                  metadata.x <- metadata(x)
                  dimtypes.x <- dimtypes(metadata.x, use.names = FALSE)
                  DimScales.x <- DimScales(metadata.x, use.names = FALSE)
                  has.quantile <- "quantile" %in% dimtypes.x
                  if (has.quantile)
                      stop(gettextf("dimension with dimtype \"%s\"",
                                    "quantile"))
                  i.iter.x <- match("iteration", dimtypes.x, nomatch = 0L)
                  has.iter.x <- i.iter.x > 0L
                  if (has.iter.x) {
                      n.iter.x <- dim(x)[i.iter.x]
                      if (.Generic == "range") {
                          name.iter.x <- names.x[i.iter.x]
                          nms <- c("range", name.iter.x)
                          nms <- make.unique(nms)
                          ds.range <- new("Categories", dimvalues = c("min", "max"))
                          ds.iter <- DimScales.x[[i.iter.x]]
                          metadata.ans.x <- new("MetaData",
                                                nms = nms,
                                                dimtypes = c("state", "iteration"),
                                                DimScales = list(ds.range, ds.iter))
                          .Data.ans.x <- matrix(nrow = 2L, ncol = n.iter.x)
                      }
                      else {
                          metadata.ans.x <- metadata.x[i.iter.x]
                          if (is.integer(x))
                              .Data.ans.x <- integer(length = n.iter.x)
                          else
                              .Data.ans.x <- numeric(length = n.iter.x)
                      }
                      if ((length(.Data.x) == 0L) && (.Generic %in% c("min", "max", "range"))) {
                          if (.Generic == "min")
                              .Data.ans.x <- rep(Inf, times = n.iter.x)
                          else if (.Generic == "max")
                              .Data.ans.x <- rep(-Inf, times = n.iter.x)
                          else
                              .Data.ans.x <- rep(c(Inf, -Inf), times = n.iter.x)                          
                      }
                      else {
                          index <- slice.index(.Data.x, MARGIN = i.iter.x)
                          if (.Generic == "range") {
                              for (i in seq_len(n.iter.x))
                                  .Data.ans.x[,i] <- callGeneric(.Data.x[index == i], na.rm = na.rm)
                          }
                          else {
                              for (i in seq_len(n.iter.x))
                                  .Data.ans.x[i] <- callGeneric(.Data.x[index == i], na.rm = na.rm)
                          }
                      }
                      .Data.ans.x <- array(.Data.ans.x,
                                           dim = dim(metadata.ans.x),
                                           dimnames = dimnames(metadata.ans.x))
                      class.x <- if(methods::is(x, "Counts")) "Counts" else "Values"
                      ans.x <- methods::new(class.x,
                                            .Data = .Data.ans.x,
                                            metadata = metadata.ans.x)
                  }
                  else {
                      if ((length(.Data.x) == 0L) && (.Generic %in% c("min", "max", "range"))) {
                          if (.Generic == "min")
                              ans.x <- Inf
                          else if (.Generic == "max")
                              ans.x <- -Inf
                          else
                              ans.x <- c(Inf, -Inf)
                      }
                      else 
                          ans.x <- methods::callGeneric(.Data.x, na.rm = na.rm)
                      .Data.ans.x <- ans.x
                  }
              }
              else {
                  has.iter.x <- FALSE
                  if ((length(x) == 0L) && (.Generic %in% c("min", "max", "range"))) {
                      if (.Generic == "min")
                          ans.x <- Inf
                      else if (.Generic == "max")
                          ans.x <- -Inf
                      else
                          ans.x <- c(Inf, -Inf)
                  }
                  else 
                      ans.x <- methods::callNextMethod(x, na.rm = TRUE)
                  .Data.ans.x <- ans.x
              }
              if (n.dots == 0L)
                  return(ans.x)
              checkIterationDimvalues(x, dots)
              if ((n.dots == 1L) && (length(dots[[1L]]) == 0L))
                  return(ans.x)
              ans.dots <- methods::callGeneric(..., na.rm = na.rm)
              if (methods::is(ans.dots, "DemographicArray")) {
                  dimtypes.dots <- dimtypes(ans.dots, use.names = FALSE)
                  i.iter.dots <- match("iteration", dimtypes.dots, nomatch = 0L)
                  has.iter.dots <- i.iter.dots > 0L
                  .Data.ans.dots <- ans.dots@.Data
              }
              else {
                  has.iter.dots <- FALSE
                  .Data.ans.dots <- ans.dots
              }
              if (has.iter.x || has.iter.dots) {
                  if (has.iter.x && has.iter.dots) {
                      class <- class(ans.x)
                      metadata.ans <- metadata.ans.x
                      .Data.ans <- .Data.ans.x
                      if (.Generic == "range") {
                          for (i in seq_len(n.iter.x)) {
                              .Data.ans[1L,i] <- min(.Data.ans.x[1L,i],
                                                     .Data.ans.dots[1L,i],
                                                     na.rm = na.rm)
                              .Data.ans[2L,i] <- max(.Data.ans.x[2L,i],
                                                     .Data.ans.dots[2L,i],
                                                     na.rm = na.rm)
                          }
                      }
                      else {
                          for (i in seq_len(n.iter.x))
                              .Data.ans[i] <- methods::callGeneric(.Data.ans.x[i],
                                                                   .Data.ans.dots[i],
                                                                   na.rm = na.rm)
                      }
                  }
                  else if (has.iter.x && !has.iter.dots) {
                      class <- class(ans.x)
                      metadata.ans <- metadata.ans.x
                      .Data.ans <- .Data.ans.x
                      if (.Generic == "range") {
                          for (i in seq_len(n.iter.x)) {
                              .Data.ans[1L,i] <- min(.Data.ans.x[1L,i],
                                                     .Data.ans.dots[1L],
                                                     na.rm = na.rm)
                              .Data.ans[2L,i] <- max(.Data.ans.x[2L,i],
                                                     .Data.ans.dots[2L],
                                                     na.rm = na.rm)
                          }
                      }
                      else {
                          for (i in seq_len(n.iter.x))
                              .Data.ans[i] <- methods::callGeneric(.Data.ans.x[i],
                                                                   .Data.ans.dots,
                                                                   na.rm = na.rm)
                      }
                  }
                  else {
                      class <- class(ans.dots)
                      .Data.ans <- .Data.ans.dots
                      metadata.ans.dots <- metadata(ans.dots)
                      metadata.ans <- metadata.ans.dots
                      n.iter.dots <- dim(ans.dots)[i.iter.dots]
                      if (.Generic == "range") {
                          for (i in seq_len(n.iter.dots)) {
                              .Data.ans[1L,i] <- min(.Data.ans.x[1L],
                                                     .Data.ans.dots[1L,i],
                                                     na.rm = na.rm)
                              .Data.ans[2L,i] <- max(.Data.ans.x[2L],
                                                     .Data.ans.dots[2L,i],
                                                     na.rm = na.rm)
                          }
                      }
                      else {
                          for (i in seq_len(n.iter.dots))
                              .Data.ans[i] <- methods::callGeneric(.Data.ans.x,
                                                                   .Data.ans.dots[i],
                                                                   na.rm = na.rm)
                      }
                  }
                  methods::new(class, .Data = .Data.ans, metadata = metadata.ans)
              }
              else {
                  if (.Generic == "range")
                      c(min(ans.x[1L], ans.dots[1L], na.rm = na.rm),
                        max(ans.x[2L], ans.dots[2L], na.rm = na.rm))
                  else
                      methods::callGeneric(ans.x, ans.dots, na.rm = na.rm)
              }
          })

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("DimScales",
          signature(object = "DemographicArray"),
          function(object, use.names = TRUE) {
              object <- metadata(object)
              methods::callGeneric()
          })

## HAS_TESTS
#' @rdname ageMinMax
#' @export
setMethod("ageMax",
          signature(object = "DemographicArray"),
          function(object) {
              object <- metadata(object)
              ageMinMax(object = object,
                        min = FALSE)
          })

## HAS_TESTS
#' @rdname ageMinMax
#' @export
setReplaceMethod("ageMax",
                 signature(object = "DemographicArray"),
                 function(object, value) {
                     metadata.old <- metadata(object)
                     metadata.new <- ageMinMaxReplace(object = metadata.old,
                                                      value = value,
                                                      min = FALSE)
                     object@metadata <- metadata.new
                     dimnames(object@.Data) <- dimnames(metadata.new)
                     object
                 })

## HAS_TESTS
#' @rdname ageMinMax
#' @export
setMethod("ageMin",
          signature(object = "DemographicArray"),
          function(object) {
              object <- metadata(object)
              ageMinMax(object = object,
                        min = TRUE)

          })

## HAS_TESTS
#' @rdname ageMinMax
#' @export
setReplaceMethod("ageMin",
                 signature(object = "DemographicArray"),
                 function(object, value) {
                     metadata.old <- metadata(object)
                     metadata.new <- ageMinMaxReplace(object = metadata.old,
                                                      value = value,
                                                      min = TRUE)
                     object@metadata <- metadata.new
                     dimnames(object@.Data) <- dimnames(metadata.new)
                     object
                 })

## HAS_TESTS
#' @rdname ageTimeStep
#' @export
setMethod("ageTimeStep",
          signature(object = "DemographicArray"),
          function(object) {
              object <- metadata(object)
              methods::callGeneric()
          })

## HAS_TESTS
#' @rdname alignPair
#' @export
setMethod("alignPair",
          signature(object = "DemographicArray"),
          function(object, base = NULL, omitted = ifelse(methods::is(object, "Counts"), 0L, NA)) {
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
                  if (!identical(dv.first, dv.second)) {
                      dv.combined <- union(dv.first, dv.second)  ## dv.first comes first
                      i.second <- match(dv.combined, dv.second, nomatch = 0L)
                      DimScale.combined <- methods::new("Categories", dimvalues = dv.combined)
                      DimScales.new <- replace(DimScales(object, use.names = FALSE),
                                               list = c(index.first, index.second),
                                               values = list(DimScale.combined))
                      metadata <- methods::new("MetaData",
                                      nms = names,
                                      dimtypes = dimtypes,
                                      DimScales = DimScales.new)
                      .Data <- array(omitted,
                                     dim = dim(metadata),
                                     dimnames = dimnames(metadata))
                      is.old <- ((slice.index(.Data, MARGIN = index.first) <= length(dv.first)) &
                                 (slice.index(.Data, MARGIN = index.second) %in% which(i.second > 0L)))
                      .Data.old <- slab(object,
                                         dimension = index.second,
                                         elements = i.second[i.second > 0L])
                      .Data[is.old] <- .Data.old
                      object <- methods::new(class(object), .Data = .Data, metadata = metadata)
                  }
              }
              object
          })


## HAS_TESTS
#' @method aperm DemographicArray
#' @export
aperm.DemographicArray <- function(a, perm, resize = TRUE, keep.class = TRUE, ...) {
    if (missing(perm) || is.null(perm))
        perm <- seq_along(dim(a))
    if (!isTRUE(resize) || !isTRUE(keep.class))
        methods::callGeneric(a = a@.Data, perm = perm, resize = resize,
                    keep.class = keep.class, ...)
    else {
        .Data.new <- methods::callGeneric(a = a@.Data, perm = perm)
        metadata.new <- metadata(a)[perm]
        methods::new(class(a), .Data = .Data.new, metadata = metadata.new)
    }
}

#' @rdname internal-methods
#' @export
setMethod("aperm",
          signature(a = "DemographicArray"),
          aperm.DemographicArray)

## ## NO_TESTS
## setMethod("apply",
##           signature(X = "DemographicArray"),
##           function(X, MARGIN, FUN, ...) {
##               kSimpleFun <- c(mean, median, mode, max, min, sum, sd, var)
##               FUN <- match.fun(FUN)
##               MARGIN <- tidySubscript(MARGIN, nDim = length(dim(X)), names = names(X))
##               matchesFUN <- function(f) identical(f, FUN)
##               if (any(sapply(kSimpleFun, matchesFUN)))
##                   applySimple(X = X, MARGIN = MARGIN, FUN = FUN, ...)
##               else
##                   applyComplicated(X = X, MARGIN = MARGIN, FUN = FUN, ...)
##           })

## HAS_TESTS
#' @method as.array DemographicArray
#' @export
as.array.DemographicArray <- function(x, ...) {
    x@.Data
}

#' @rdname internal-methods
#' @export
setMethod("as.array",
          signature(x = "DemographicArray"),
          as.array.DemographicArray)

## HAS_TESTS
#' @method as.matrix DemographicArray
#' @export
as.matrix.DemographicArray <- function(x, ...) {
    as.matrix(x@.Data)
}

#' @rdname internal-methods
#' @export
setMethod("as.matrix",
          signature(x = "DemographicArray"),
          as.matrix.DemographicArray)

## HAS_TESTS
#' @rdname collapseIterations
#' @export
setMethod("collapseIterations",
          signature(object = "DemographicArray"),
          function(object, FUN = quantile, ...) {
              i.iter <- match("iteration", dimtypes(object), nomatch = 0L)
              if (identical(i.iter, 0L))
                  return(object)
              if (length(object) == 0L)
                  stop(gettextf("'%s' has length %d",
                                "object", 0L))
              name <- make.names(deparse(substitute(FUN)))
              FUN <- match.fun(FUN)
              n.dim <- length(dim(object))
              if (n.dim == 1L) {
                  .Data <- FUN(object@.Data, ...)
                  if (is.list(.Data))
                      return(.Data)
                  return.value.length.1 <- length(.Data) == 1L
                  if (return.value.length.1)
                      return(.Data)
                  else
                      .Data <- array(.Data,
                                     dim = length(.Data),
                                     dimnames = list(names(.Data)))
              }
              else {
                  MARGIN <- seq_len(n.dim)[-i.iter]
                  .Data <- apply(object@.Data, MARGIN = MARGIN, FUN = FUN, ...)
                  if (is.list(.Data))
                      return(.Data)
                  if (is.null(dim(.Data)))
                      .Data <- array(.Data,
                                     dim = length(.Data),
                                     dimnames = dimnames(object)[-i.iter])
                  return.value.length.1 <- identical(dim(.Data), dim(object)[-i.iter])
              }
              if (return.value.length.1)
                  metadata <- metadata(object)[-i.iter]
              else {
                  labels <- dimnames(.Data)[[1L]]
                  if (is.null(labels))
                      stop(gettextf("return values of '%s' do not have names", "FUN"))
                  if (any(is.na(labels)) || !all(nzchar(labels)))
                      stop(gettextf("return values of '%s' do not have valid names", "FUN"))
                  if (any(duplicated(labels)))
                      stop(gettextf("return values of '%s' have duplicated names [%s]",
                                    "FUN", paste(dQuote(labels[duplicated(labels)]), collapse = ", ")))
                  dimvalues <- inferDimvalues(methods::new("Quantiles"), labels = labels)
                  looks.like.quantile <- !is.null(dimvalues)
                  if (looks.like.quantile) {
                      DimScale <- methods::new("Quantiles", dimvalues = dimvalues)
                      dimtype <- "quantile"
                  }
                  else {
                      DimScale <- methods::new("Categories", dimvalues = labels)
                      dimtype <- "state"
                  }
                  names <- replace(names(object), list = i.iter, values = name)
                  dimtypes <- replace(dimtypes(object, use.names = FALSE),
                                      list = i.iter,
                                      values = dimtype)
                  DimScales <- replace(DimScales(object, use.names = FALSE),
                                       list = i.iter,
                                       values = list(DimScale))
                  metadata <- methods::new("MetaData",
                                           nms = names,
                                           dimtypes = dimtypes,
                                           DimScales = DimScales)
                  if (length(names) > 1L) {
                      perm <- append(seq.int(from = 2L, to = length(names)),
                                     values = 1L,
                                     after = i.iter - 1L)
                      .Data <- aperm(.Data, perm = perm)
                  }
                  dimnames(.Data) <- dimnames(metadata)
              }
              methods::new(class(object), .Data = .Data, metadata = metadata)
          })

## diff.DemographicArray <- function(x, lag = 1L, difference = 1L, dimension) {
##     if ("quantile" %in% dimtypes(x))
##         stop(gettext("'%s' has dimtype \"%s\"", "x", "quantile"))
##     if (methods::hasArg(dimension)) {
##         dimension <- tidySubscript(dimension, nDim = length(dim(x)), names = names(x))
##         if (!identical(length(dimension), 1L))
##             stop(gettext("more than one dimension selected"))
##     }
##     else {
##         i.time <- match("time", dimtypes(x), nomatch = 0L)
##         if (i > 0L)
##             dimension <- i.time
##         else
##             stop(gettextf("'%s' not supplied, and '%s' does not have dimtype \"%s\", so no default",
##                           "dimension", "x", "time"))
##     }
##     margin <- invertSubscript(dimension, nDim = length(dim(x)))
##     .Data <- apply(x@.Data, MARGIN = margin, FUN = diff, lag = lag, difference = difference)
##     DimScale.new <- diff(DimScales(x)[[dimension]], lag = lag, difference = difference)
##     ## TODO - FINISH
## }


## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("dim",
          signature(x = "DemographicArray"),
          function(x) {
            x <- metadata(x)
            methods::callGeneric(x)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setReplaceMethod("dim",
                 signature(x = "DemographicArray"),
                 function(x, value)
                 stop(sprintf("%s of object of class \"%s\" cannot be modified directly",
                              "dimensions", class(x))))

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("dimnames",
          signature(x = "DemographicArray"),
          function(x) {
            x <- metadata(x)
            methods::callGeneric(x)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setReplaceMethod("dimnames",
                 signature(x = "DemographicArray"),
                 function(x, value)
                 stop(sprintf("%s of object of class \"%s\" cannot be modified directly",
                              "dimnames", class(x))))

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("dimscales",
          signature(object = "DemographicArray"),
          function(object, use.names = TRUE) {
              object <- metadata(object)
              methods::callGeneric()
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setReplaceMethod("dimscales",
                 signature(object = "DemographicArray"),
                 function(object, value) {
                     object@metadata <- methods::callGeneric(object = metadata(object),
                                                    value = value)
                     object
                 })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("dimtypes",
          signature(object = "DemographicArray"),
          function(object, use.names = TRUE) {
              object <- metadata(object)
              methods::callGeneric()
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setReplaceMethod("dimtypes",
                 signature(object = "DemographicArray"),
                 function(object, value) {
                     object@metadata <- methods::callGeneric(object = metadata(object),
                                                    value = value)
                     object
                 })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("drop",
          signature(x = "DemographicArray"),
          function(x) {
              .Data <- x@.Data
              .Data <- drop(.Data)
              keep <- dim(x) != 1L
              not.dim.0 <- !identical(dim(x), 0L)
              like.vector.already <- length(dim(x)) == 1L
              like.vector.after.drop.length.1 <- sum(keep) == 1L
              if (not.dim.0 && (like.vector.already || like.vector.after.drop.length.1))
                  .Data
              else {
                  names <- names(x)[keep]
                  dimtypes <- dimtypes(x, use.names = FALSE)[keep]
                  DimScales <- DimScales(x, use.names = FALSE)[keep]
                  ## correct dimtypes and names for lost pairs
                  names.pairs <- getNamesPairs(names)
                  lost.pair <- !(names.pairs %in% names)
                  names[lost.pair] <- removeSuffixes(names[lost.pair])
                  dimtypes[lost.pair] <- "state"
                  metadata <- methods::new("MetaData",
                                  nms = names,
                                  dimtypes = dimtypes,
                                  DimScales = DimScales)
                  dimnames(.Data) <- dimnames(metadata)
                  methods::new(class(x), .Data = .Data, metadata = metadata)
              }
          })

## HAS_TESTS
#' @rdname extrapolate
#' @export
setMethod("extrapolate",
          signature(object = "DemographicArray"),
          function(object, along = NULL, labels, growth = 0,
                   type = c("exponential", "linear", "missing")) {
              metadata <- metadata(object)
              names <- names(object)
              dim <- dim(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              dimscales <- dimscales(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              along <- checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
              has.single.dim <- length(dim) == 1L
              name.along <- names[along]
              labels <- as.character(labels)
              type <- match.arg(type)
              if ("quantile" %in% dimtypes)
                  stop(gettextf("dimension with dimtype \"%s\"", "quantile"))
              if (identical(dim[along], 0L))
                  stop(gettextf("cannot extrapolate along dimension \"%s\" because dimension has length 0",
                                name.along))
              if (methods::is(growth, "DemographicArray")) {
                  if (name.along %in% names(growth))
                      stop(gettextf("extrapolating along dimension \"%s\" but '%s' has dimension named \"%s\"",
                                    name.along, "growth", name.along))
                  growth <- methods::as(growth, "Values")
              }
              else if (is.numeric(growth)) {
                  if (!identical(length(growth), 1L))
                      stop(gettextf("'%s' does not have length %d", "growth", 1L))
                  if (!has.single.dim) {
                      metadata.growth <- metadata[-along]
                      .Data.growth <- rep(growth,
                                          times = prod(dim[-along]))
                      .Data.growth <- array(.Data.growth,
                                            dim = dim(metadata.growth),
                                            dimnames = dimnames(metadata.growth))
                      growth <- methods::new("Values",
                                             .Data = .Data.growth,
                                             metadata = metadata.growth)
                  }
              }
              else
                  stop(gettextf("'%s' has class \"%s\"", "growth", class(growth)))
              dimtype.extra <- dimtypes[along]
              dimscale.extra <- dimscales[along]
              DimScale.extra <- inferDimScale(dimtype = dimtype.extra,
                                              dimscale = dimscale.extra,
                                              labels = labels,
                                              name = name.along)
              DimScale.existing <- DimScales[[along]]
              dv.existing <- dimvalues(DimScale.existing)
              dv.extra <- dimvalues(DimScale.extra)
              if (methods::is(DimScale.existing, "Points")) {
                  existing.first <- max(dv.existing) < min(dv.extra)
                  if (existing.first)
                      points <- c(dv.existing[length(dv.existing)], dv.extra)
                  else {
                      if (min(dv.existing) <= max(dv.extra))
                          stop(gettextf("extrapolated and existing points overlap"))
                      points <- c(dv.extra, dv.existing[1L])
                  }
              }
              else {
                  existing.first <- max(dv.existing) == min(dv.extra)
                  if (existing.first)
                      dimvalues.tmp <- c(dv.existing[length(dv.existing) - 1L], dv.extra)
                  else {
                      if (max(dv.extra) != min(dv.existing))
                          stop(gettextf("gap or overlap between extrapolated and existing intervals"))
                      dimvalues.tmp <- c(dv.extra, dv.existing[2L])
                  }
                  DimScale.tmp <- methods::new("Intervals", dimvalues = dimvalues.tmp)
                  points <- intervalsToPoints(DimScale.tmp)
                  points <- dimvalues(points)
              }
              if (existing.first)
                  distance <- points[-1L] - points[1L]
              else
                  distance <- points[-length(points)] - points[length(points)]
              metadata.distance <- methods::new("MetaData",
                                                nms = name.along,
                                                dimtypes = dimtypes[along],
                                                DimScales = list(DimScale.extra))
              .Data.distance <- array(distance,
                                      dim = length(distance),
                                      dimnames = dimnames(metadata.distance))
              distance <- methods::new("Values", .Data = .Data.distance, metadata = metadata.distance)
              i.jumpoff <- if (existing.first) length(DimScale.existing) else 1L
              jumpoff <- slab(object, dimension = along, elements = i.jumpoff, drop = TRUE)
              if (!has.single.dim) {
                  metadata.jumpoff <- metadata[-along]
                  .Data.jumpoff <- array(jumpoff@.Data,
                                         dim = dim(metadata.jumpoff),
                                         dimnames = dimnames(metadata.jumpoff))
                  jumpoff <- new("Values",
                                 .Data = .Data.jumpoff,
                                 metadata = metadata.jumpoff)
              }
              if (identical(type, "exponential"))
                  extra <- jumpoff * ((1 + growth) ^ distance)
              else if (identical(type, "linear"))
                  extra <- jumpoff + distance * growth
              else if (identical(type, "missing")) {
                  extra <- jumpoff + distance
                  extra[] <- as.integer(NA)
              }
              else
                  stop(gettextf("invalid value for '%s': \"%s\"", "type", type))
              extra <- methods::as(extra, class(object))
              dbind(object, extra, along = name.along)
          })

## HAS_TESTS
#' @rdname hasRegularAgeTime
#' @export
setMethod("hasRegularAgeTime",
          signature(object = "DemographicArray"),
          function(object) {
              object <- metadata(object)
              methods::callGeneric()
          })

## HAS_TESTS
#' @rdname impute
#' @export
setMethod("impute",
          signature(object = "DemographicArray"),
          function(object, mult = NULL, max = NULL) {
              .Data <- object@.Data
              i.missing <- sort(which(is.na(.Data)))
              n.missing <- length(i.missing)
              if (identical(n.missing, length(.Data)))
                  stop(gettext("no non-missing values"))
              if (identical(n.missing, 0L))
                  return(object)
              if (is.null(mult))
                  mult <- all(.Data >= 0, na.rm = TRUE)
              else {
                  if (!identical(length(mult), 1L))
                      stop(gettextf("'%s' does not have length %d",
                                    "mult", 1L))
                  if (!is.logical(mult))
                      stop(gettextf("'%s' does not have type \"%s\"",
                                    "mult", "logical"))
                  if (is.na(mult))
                      stop(gettextf("'%s' is missing",
                                    "mult"))
                  if (mult) {
                      if (any(.Data < 0, na.rm = TRUE))
                          stop(gettextf("'%s' is %s but '%s' has negative values",
                                        "mult", "TRUE", "object"))
                      if (all(.Data == 0, na.rm = TRUE))
                          stop(gettextf("'%s' is %s but '%s' has no positive values",
                                        "mult", "TRUE", "object"))
                  }
              }
              if (!is.null(max)) {
                  if (!is.numeric(max))
                      stop(gettextf("'%s' does not have type \"%s\"",
                                    "max", "numeric"))
                  if (any(is.na(max)))
                      stop(gettextf("'%s' has missing values",
                                    "max"))
                  if (isTRUE(all.equal(max, round(max))))
                      max <- as.integer(max)
                  max <- rep(max, length.out = length(.Data))
              }
              values <- .Data[!is.na(.Data)]
              is.integer.vals <- isTRUE(all.equal(round(values), values))
              is.integer.type <- is.integer(.Data)
              d <- as.data.frame(object, direction = "long", midpoints = TRUE)
              if (mult) {
                  n <- length(d)
                  is.zero <- !is.na(d[[n]]) & (d[[n]] == 0L)
                  d[[n]] <- log(d[[n]])
                  d[[n]][is.zero] <- NA
              }
              is.factor.one.level <- sapply(d[stats::complete.cases(d), ],
                                            function(x) is.factor(x) && length(unique(x)) == 1L)
              d <- d[!is.factor.one.level]
              p <- min(length(d) - 1L, sum(stats::complete.cases(d)))
              if (p > 1L)
                  predictors <- paste(names(d)[seq_len(p)], sep = " + ")
              else
                  predictors <- "1"
              formula <- stats::as.formula(sprintf("%s ~ %s", names(d)[length(d)], predictors))
              mod <- stats::lm(formula, data = d)
              xlevels <- mod$xlevels
              for (name in names(xlevels)) {
                  levels.without.obs <- setdiff(levels(d[[name]]), xlevels[[name]])
                  if (length(levels.without.obs) > 0L)
                      d[[name]][d[[name]] %in% levels.without.obs] <- xlevels[[name]][1L]
              }
              predicted <- stats::predict(mod, newdata = d[-length(d)])
              predicted <- predicted[i.missing]
              if (mult)
                  predicted <- exp(predicted)
              if (is.integer.vals)
                  imputed.values <- as.integer(stats::rpois(n = n.missing, lambda = predicted))
              else
                  imputed.values <- stats::rnorm(n = n.missing, mean = predicted, sd = sqrt(abs(predicted)))
              if (!is.null(max)) {
                  max.imputed <- max[i.missing]
                  exceeds.max <- imputed.values > max.imputed
                  imputed.values[exceeds.max] <- max.imputed[exceeds.max]
              }
              object[i.missing] <- imputed.values
              object
          })


## NO_TESTS
#' @rdname limits
#' @export
setMethod("limits",
          signature(object = "DemographicArray"),
          function(object, components = FALSE) {
              object <- metadata(object)
              methods::callGeneric()
          })

## HAS_TESTS
## function is complicated because of 'center = median(x)' argument
#' @method mad DemographicArray
#' @export
mad.DemographicArray <- function(x, center = median(x), constant = 1.4826,
                                 na.rm = FALSE, low = FALSE, high = FALSE) {
    .Data <- x@.Data
    metadata <- metadata(x)
    dimtypes <- dimtypes(metadata, use.names = FALSE)
    has.quantile <- "quantile" %in% dimtypes
    if (has.quantile)
        stop(gettextf("'%s' has dimension with dimtype \"%s\"",
                      "x", "quantile"))
    i.iter <- match("iteration", dimtypes, nomatch = 0L)
    has.iter <- i.iter > 0L
    if (has.iter) {
        metadata.ans <- metadata[i.iter]
        if (methods::hasArg(center)) {
            FUN <- function(X)
                stats::mad(X,
                           center = center,
                           constant = constant,
                           na.rm = na.rm,
                           low = low,
                           high = high)
        }
        else {
            FUN <- function(X)
                stats::mad(X,
                           center = stats::median(X, na.rm = na.rm),
                           constant = constant,
                           na.rm = na.rm,
                           low = low,
                           high = high)
        }
        .Data.ans <- apply(.Data,
                           MARGIN = i.iter,
                           FUN = FUN)
        .Data.ans <- array(.Data.ans,
                           dim = dim(metadata.ans),
                           dimnames = dimnames(metadata.ans))
        class <- if(methods::is(x, "Counts")) "Counts" else "Values"
        new(class,
            .Data = .Data.ans,
            metadata = metadata.ans)
    }
    else {
        if (methods::hasArg(center)) {
            stats::mad(.Data,
                       center = center,
                       constant = constant,
                       na.rm = na.rm,
                       low = low,
                       high = high)
        }
        else {
            stats::mad(.Data,
                       center = stats::median(.Data, na.rm = na.rm),
                       constant = constant,
                       na.rm = na.rm,
                       low = low,
                       high = high)
        }
    }
}

## HAS_TESTS
#' @rdname Statistical-Functions
#' @export
setMethod("mad",
          signature(x = "DemographicArray"),
              mad.DemographicArray)

## HAS_TESTS
#' @method mean DemographicArray
#' @export
mean.DemographicArray <- function(x, ...) {
    .Data <- x@.Data
    metadata <- metadata(x)
    dimtypes <- dimtypes(metadata, use.names = FALSE)
    has.quantile <- "quantile" %in% dimtypes
    if (has.quantile)
        stop(gettextf("'%s' has dimension with dimtype \"%s\"",
                      "x", "quantile"))
    i.iter <- match("iteration", dimtypes, nomatch = 0L)
    has.iter <- i.iter > 0L
    if (has.iter) {
        metadata.ans <- metadata[i.iter]
        .Data.ans <- apply(.Data,
                           MARGIN = i.iter,
                           FUN = mean,
                           ...)
        .Data.ans <- array(.Data.ans,
                           dim = dim(metadata.ans),
                           dimnames = dimnames(metadata.ans))
        class <- if(methods::is(x, "Counts")) "Counts" else "Values"
        new(class,
            .Data = .Data.ans,
            metadata = metadata.ans)
    }
    else
        mean(.Data, ...)
}

## HAS_TESTS
#' @rdname Statistical-Functions
#' @export
setMethod("mean",
          signature(x = "DemographicArray"),
          mean.DemographicArray)

## HAS_TESTS
#' @method median DemographicArray
#' @export
median.DemographicArray <- function(x, na.rm = FALSE) {
    .Data <- x@.Data
    metadata <- metadata(x)
    dimtypes <- dimtypes(metadata, use.names = FALSE)
    has.quantile <- "quantile" %in% dimtypes
    if (has.quantile)
        stop(gettextf("'%s' has dimension with dimtype \"%s\"",
                      "x", "quantile"))
    i.iter <- match("iteration", dimtypes, nomatch = 0L)
    has.iter <- i.iter > 0L
    if (has.iter) {
        metadata.ans <- metadata[i.iter]
        .Data.ans <- apply(.Data,
                           MARGIN = i.iter,
                           FUN = stats::median,
                           na.rm = na.rm)
        .Data.ans <- array(.Data.ans,
                           dim = dim(metadata.ans),
                           dimnames = dimnames(metadata.ans))
        class <- if(methods::is(x, "Counts")) "Counts" else "Values"
        new(class,
            .Data = .Data.ans,
            metadata = metadata.ans)
    }
    else
        stats::median(.Data, na.rm = na.rm)
}

## HAS_TESTS
#' @rdname Statistical-Functions
#' @export
setMethod("median",
          signature(x = "DemographicArray"),
          median.DemographicArray)

## HAS_TESTS
setMethod("metadata",
          signature(object = "DemographicArray"),
          function(object) object@metadata)

## HAS_TESTS
#' @rdname midpoints
#' @export
setMethod("midpoints",
          signature(object = "DemographicArray", dimension = "ANY"),
          function(object, dimension) {
              .Data <- object@.Data
              metadata <- metadata(object)
              dimension <- tidySubscript(subscript = dimension,
                                         nDim = length(dim(object)),
                                         names = names(object))
              metadata <- midpoints(object = metadata, dimension = dimension)
              dimnames(.Data) <- dimnames(metadata)
              methods::new(class(object), .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
#' @rdname midpoints
#' @export
setMethod("midpoints",
          signature(object = "DemographicArray", dimension = "missing"),
          function(object) {
              dimension <- which(dimscales(object) == "Intervals")
              methods::callGeneric(object = object, dimension = dimension)
          })

#' Get or set dimension names
#' 
#' Query or change the dimension names of a \code{\linkS4class{DemographicArray}}
#' object.
#' 
#' @name names-methods
#' @docType methods
#' @param x Object of class \code{\linkS4class{DemographicArray}}.
#' @param value Character vector.
#' @return A character vector.  The replacement method returns an object of
#' class \code{\linkS4class{DemographicArray}}.
#' @author John Bryant \email{demographic.packages@@gmail.com}
#' @seealso As described in \code{\link{dimtypes}}, some dimensions must come
#' in pairs, and there are special rules governing the names of these
#' dimensions.
#' @examples
#' library(demdata)
#' popn <- Counts(VAPopn)
#' names(popn)
#' names(popn) <- toupper(names(popn))
NULL

## HAS_TESTS
#' @rdname names-methods
#' @export
setMethod("names",
          signature(x = "DemographicArray"),
          function(x) {
              x <- metadata(x)
              methods::callGeneric(x)
          })

## HAS_TESTS
#' @rdname names-methods
#' @export
setReplaceMethod("names",
                 signature(x = "DemographicArray"),
                 function(x, value) {
                     value <- as.character(value)
                     names(x@metadata) <- value
                     names(dimnames(x@.Data)) <- value
                     methods::validObject(x)
                     x
                 })

## HAS_TESTS
#' @rdname nIteration
#' @export
setMethod("nIteration",
          signature(object = "DemographicArray"),
          function(object) {
              i.iter <- match("iteration", dimtypes(object), nomatch = 0L)
              if (identical(i.iter, 0L))
                  stop(gettextf("no dimension with dimtype \"%s\"", "iteration"))
              dim(object)[i.iter]
          })

#' @rdname pairAligned
#' @export
setMethod("pairAligned",
          signature(object = "DemographicArray"),
          function(object, base = NULL) {
              object <- metadata(object)
              callGeneric()
          })

## HAS_TESTS
#' @rdname perturb
#' @export
setMethod("perturb",
          signature(object = "DemographicArray"),
          function(object, n = 1L, order = 2L, phi = 1, subtotals = NULL) {
              if (!is.null(subtotals))
                  stop("'subtotals' not yet implemented")
              for (name in c("n", "order", "phi")) {
                  value <- get(name)
                  if (!identical(length(value), 1L))
                      stop(gettextf("'%s' does not have length %d", name, 1L))
                  if (is.na(value))
                      stop(gettextf("'%s' is missing", name))
                  if (value < 1L)
                      stop(gettextf("'%s' is less than %d", name, 1L))
              }
              i.iter <- match("iteration", dimtypes(object), nomatch = 0L)
              has.iter <- i.iter > 0L
              if (has.iter)
                  perturbUsingIterations(object = object, n = n, i.iter = i.iter)
              else
                  perturbUsingModel(object = object, n = n, order = order, phi = phi)
          })

## HAS_TESTS
#' @rdname prop.table
#' @export
setMethod("prop.table",
          signature(x = "DemographicArray", margin = "ANY"),
          function(x, margin = NULL) {
              if ("quantile" %in% dimtypes(x))
                  stop(gettextf("dimension with dimtype \"%s\"", "quantile"))
              i.iter <- match("iteration", dimtypes(x), nomatch = 0L)
              has.iter <- i.iter > 0L
              if (has.iter) {
                  if (!(i.iter %in% margin))
                      margin <- c(margin, i.iter)
              }
              .Data <- prop.table(x@.Data, margin = margin)
              metadata <- metadata(x)
              methods::new("Values", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
#' @rdname prop.table
#' @export
setMethod("prop.table",
          signature(x = "DemographicArray", margin = "character"),
          function(x, margin = NULL) {
              margin <- match(margin, names(x), nomatch = 0L)
              if (any(margin == 0L))
                  stop(gettextf("'%s' outside valid range", "margin"))
              methods::callGeneric()
          })

## ## NO_TESTS
## setMethod("relabelCategories",
##           signature(object = "DemographicArray",
##                     dimension = "ANY",
##                     concordance = "OneToOne"),
##           function(object, dimension, concordance, to = NULL) {
##               dim <- dim(object)
##               names <- names(object)
##               dimtypes <- dimtypes(object, use.names = FALSE)
##               DimScales <- DimScales(object, use.names = FALSE)
##               dimension <- tidySubscript(dimension, nDim = length(dim), names = names)
##               for (i in seq_along(dim)) {
##                   apply.concordance <- i %in% dimension
##                   if (apply.concordance) {
##                       DimScale <- DimScales[[i]]
##                       if (!methods::is(DimScale, "Categories"))
##                           stop(gettextf("dimension \"%s\" has dimscale \"%s\"",
##                                         names[i], class(DimScale)))
##                       codes.old <- dimvalues(DimScale)
##                       codes.new <- classconc::translate(codes.old, concordance = concordance)
##                       DimScale <- methods::new("Categories", dimvalues = codes.new)
##                       DimScales[[i]] <- DimScale
##                   }
##               }
##               metadata <- methods::new("MetaData",
##                               nms = names,
##                               dimtypes = dimtypes,
##                               DimScales = DimScales)
##               .Data <- object@.Data
##               dimnames(.Data)[dimension] <- dimnames(metadata)[dimension]
##               methods::new("Counts", .Data = .Data, metadata = metadata)
##           })

## ## NO_TESTS
## setMethod("relabelCategories",
##           signature(object = "DemographicArray",
##                     dimension = "missing",
##                     concordance = "OneToOne"),
##           function(object, dimension, concordance) {
##               DimScales <- DimScales(object, use.names = FALSE)
##               in.dimension <- logical(length = length(DimScales))
##               codes.from <- codesFrom(concordance)
##               for (i in seq_along(DimScales)) {
##                   DimScale <- DimScales[[i]]
##                   if (methods::is(DimScale, "Categories")) {
##                       codes <- dimvalues(DimScale)
##                       in.dimension[i] <- all(codes %in% codes.from)
##                   }
##                   else
##                       in.dimension[i] <- FALSE
##               }
##               dimension <- which(in.dimension)
##               methods::callGeneric(object = object,
##                           dimension = dimension,
##                           concordance = concordance)
##           })

## ## need to deal with pairs of dims
## setMethod("reorderCategories",
##           signature(object = "DemographicArray"),
##           function(object, dimension, FUN, ...) {
##               .Data <- object@.Data
##               metadata <- metadata(object)
##               dim <- dim(object)
##               dimension <- tidySubscript(dimension, nDim = length(dim), names = names(metadata))
##               if (identical(length(dimension), 1L)) {
##                   DimScale <- DimScales(object)[[dimension]]
##                   if (!methods::is(DimScale, "Categories"))
##                       stop(gettextf("'%s' has dimscale \"%s\"", "dimension", class(DimScale)))
##                   res <- numeric(
##               }
##               else
##                   stop(gettextf("'%s' has length %d", "dimension", length(dimension)))
##               metadata.tmp <- metadata[-dimension]
##               slab <- slice.index(.Data, MARGIN = dimension)
##               data.tmp <- array(.Data[slab == 1L], dim = dim(metadata.tmp), dimnames = dimnames(metadata.tmp))
##               obj.tmp <- methods::new(class(object), .Data.tmp)
##               res <- numeric(length = dim[dimension])
##               for (i in seq_len(dim[dimension])) {
##                   obj.tmp@.Data@.Data <- .Data[slab == i]
##                   res.tmp <- FUN(obj.tmp, ...)
##                   if (!(identical(length(res.tmp, 1L))))
##                       stop(gettextf("return value does not have length %d", 1L))
##                   if (!is.numeric(res.tmp))
##                       stop(gettextf("return value does not have type \"%s\"", "numeric"))
##                   if (is.na(res.tmp))
##                       stop(gettext("return value is missing"))
##                   res[i] <- res.tmp
##               }
##               DimScale.reorder <- DimScales(object)[dimension]

## HAS_TESTS
#' @rdname resetIterations
#' @export
setMethod("resetIterations",
          signature(object = "DemographicArray"),
          function(object) {
              .Data <- object@.Data
              metadata <- metadata(object)
              metadata <- resetIterations(metadata)
              dimnames(.Data) <- dimnames(metadata)
              methods::new(class(object), .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("show",
          signature(object = "DemographicArray"),
          function(object) {
              metadata <- metadata(object)
              .Data <- object@.Data
              cat(gettextf("An object of class \"%s\"\n", class(object)))
              showMetaData(metadata)
              cat("\n")
              methods::show(.Data)
          })

## HAS_TESTS
#' @rdname slab
#' @export
setMethod("slab",
          signature(object = "DemographicArray"),
          function(object, dimension, elements, drop = TRUE) {
              dim.before <- dim(object)
              dims <- seq_along(dim.before)
              if (!identical(length(dimension), 1L))
                  stop(gettextf("'%s' does not have length %d", "dimension", 1L))
              if (!is.numeric(dimension))
                  dimension <- match(dimension, names(object), nomatch = 0L)
              if (!(dimension %in% dims))
                  stop(gettextf("'%s' outside valid range", "dimension"))
              if (any(duplicated(elements)))
                  stop(gettextf("'%s' has duplicates", "elements"))
              if (!is.numeric(elements))
                  elements <- match(elements, dimnames(object)[[dimension]], nomatch = 0L)
              s <- seq_len(dim.before[dimension])
              if (!all(elements %in% s))
                  stop(gettextf("'%s' outside valid range", "elements"))
              indices <- lapply(dim.before, seq_len)
              indices[[dimension]] <- match(s, elements, nomatch = 0L)
              dim.after <- replace(dim.before, list = dimension, values = length(elements))
              transform <- methods::new("CollapseTransform",
                               dims = dims,
                               indices = indices,
                               dimBefore = dim.before,
                               dimAfter = dim.after)
              ans <- collapse(object, transform = transform)
              if (drop) {
                  if (length(ans) == 1L) {
                      ans <- ans[[1L]]
                  }
                  else {
                      is.length.1 <- dim.after == 1L
                      metadata <- metadata(ans)[!is.length.1]
                      .Data <- array(ans@.Data,
                                     dim = dim(metadata),
                                     dimnames = dimnames(metadata))
                      ans <- methods::new(class(ans), .Data = .Data, metadata = metadata)
                  }
              }
              ans
          })

## HAS_TESTS
#' @rdname slab
#' @export
setReplaceMethod("slab",
                 signature(object = "DemographicArray"),
                 function(object, dimension, elements, drop = TRUE, value) {
                     dim <- dim(object)
                     names <- names(object)
                     dimnames <- dimnames(object)
                     if (!identical(length(dimension), 1L))
                         stop(gettextf("'%s' does not have length %d",
                                       "dimension", 1L))
                     if (!is.numeric(dimension))
                         dimension <- match(dimension, names, nomatch = 0L)
                     if (!(dimension %in% seq_along(dim)))
                         stop(gettextf("'%s' outside valid range", "dimension"))
                     if (any(duplicated(elements)))
                         stop(gettextf("'%s' has duplicates", "elements"))
                     if (!is.numeric(elements))
                         elements <- match(elements, dimnames[[dimension]], nomatch = 0L)
                     s <- seq_len(dim[dimension])
                     if (!all(elements %in% s))
                         stop(gettextf("'%s' outside valid range", "elements"))
                     if (methods::hasArg(drop))
                         warning(gettextf("'%s' argument ignored by replacement method for '%s'",
                                          "drop", "slab"))
                     if (!is.numeric(value)) {
                         if (is.logical(value))
                             value <- as.integer(value)
                         else
                             stop(gettext("replacement value is non-numeric"))
                     }
                     dim.slab <- replace(dim, list = dimension, values = length(elements))
                     length.slab <- prod(dim.slab)
                     length.val <- length(value)
                     if (length.val == 0L) {
                         if (length.slab == 0L)
                             return(object)
                         else
                             stop(gettextf("replacement value has length %d",
                                           0L))
                     }
                     if (length.val > length.slab)
                         stop(gettext("length of replacement value greater than length of slab"))
                     is.multiple <- (length.slab %% length.val) == 0L
                     if (!is.multiple)
                         stop(gettext("length of replacement value not multiple of length of slab"))
                     value <- rep(value, length.out = length.slab)
                     is.slab <- slice.index(object, dimension) %in% elements
                     object@.Data[is.slab] <- value
                     object
                 })

## HAS_TESTS
## Tried to improve performance when called inside functions via
## subarray <- eval(subarray, envir = DimScales, enclos = parent.frame(n = 2L))
## but it created new problems.
#' @rdname subarray
#' @export
setMethod("subarray",
          signature(object = "DemographicArray"),
          function(object, subarray, drop = TRUE) {
              .Data <- object@.Data
              names <- names(object)
              metadata <- metadata(object)
              DimScales <- DimScales(object)
              subarray <- substitute(subarray)
              subarray <- eval(subarray, envir = DimScales)
              subarray.names <- subarray@nms
              subarray.indices <- subarray@indices
              dim.before <- dim(object)
              dims <- seq_along(dim.before)
              indices <- vector(mode = "list", length = length(dims))
              for (i in seq_along(dim.before)) {
                  name <- names[i]
                  i.subarray <- match(name, subarray.names, nomatch = 0L)
                  if (i.subarray > 0L) {
                      index <- subarray.indices[[i.subarray]]
                      index[index] <- cumsum(index[index])
                      indices[[i]] <- index
                  }
                  else
                      indices[[i]] <- seq_len(dim.before[i])
              }
              dim.after <- sapply(indices, max)
              transform <- methods::new("CollapseTransform",
                               dims = dims,
                               indices = indices,
                               dimBefore = dim.before,
                               dimAfter = dim.after)
              .Data <- collapse(.Data, transform = transform)
              metadata <- collapse(metadata, transform = transform)
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              ans <- methods::new(class(object), .Data = .Data, metadata = metadata)
              if (drop)
                  ans <- drop(ans)
              ans
          })

## HAS_TESTS
#' @method t DemographicArray
#' @export
t.DemographicArray <- function(x) {
    if (identical(length(dim(x)), 2L))
        aperm(x, perm = 2:1)
    else
        stop("does not have 2 dimensions")
}

#' @rdname internal-methods
#' @export
setMethod("t",
          signature(x = "DemographicArray"),
          t.DemographicArray)

## HAS_TESTS
#' @rdname thinIterations
#' @export
setMethod("thinIterations",
          signature(object = "DemographicArray"),
          function(object, n) {
              i.iter <- match("iteration", dimtypes(object), nomatch = 0L)
              has.iter <- i.iter > 0L
              if (!has.iter)
                  stop(gettextf("'%s' does not have a dimension with dimtype \"%s\"",
                                "object", "iteration"))
              if (!identical(length(n), 1L))
                  stop(gettextf("'%s' does not have length %d", "n", 1L))
              if (!is.numeric(n))
                  stop(gettextf("'%s' does not have type \"%s\"", "n", "numeric"))
              if (is.na(n))
                  stop(gettextf("'%s' is missing", "n"))
              if (round(n) != n)
                  stop(gettextf("'%s' is not an integer", "n"))
              if (n < 1L)
                  stop(gettextf("'%s' is less than %d", "n", 1L))
              n.iter <- dim(object)[i.iter]
              if (n > n.iter)
                  stop(gettextf("'%s' greater than number of iterations", "n"))
              elements <- sample.int(n = n.iter, size = n)
              ans <- slab(object, dimension = i.iter, elements = elements, drop = FALSE)
              DimScales <- replace(DimScales(ans, use.names = FALSE),
                                   list = i.iter,
                                   values = list(methods::new("Iterations", dimvalues = seq_len(n))))
              metadata <- methods::new("MetaData",
                              nms = names(ans),
                              dimtypes = dimtypes(ans, use.names = FALSE),
                              DimScales = DimScales)
              .Data <- ans@.Data
              dimnames(.Data) <- dimnames(metadata)
              methods::new(class(object), .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
#' @rdname coerce-data
#' @export
setMethod("toDouble",
          signature(object = "DemographicArray"),
          function(object) {
              .Data <- object@.Data
              if (!is.double(.Data)) {
                  object@.Data <- array(as.double(.Data),
                                        dim = dim(.Data),
                                        dimnames = dimnames(.Data))
              }
              object
          })

## HAS_TESTS
#' @rdname coerce-data
#' @export
setMethod("toInteger",
          signature(object = "DemographicArray"),
          function(object, force = FALSE) {
              .Data <- object@.Data
              if (!is.integer(.Data)) {
                  if (!force) {
                      values <- .Data[!is.na(.Data)]
                      if (!isTRUE(all.equal(values, round(values))))
                          stop(gettext("non-integer values"))
                  }
                  object@.Data <- array(as.integer(round(.Data)),
                                        dim = dim(.Data),
                                        dimnames = dimnames(.Data))
              }
              object
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("unname",
          signature(obj = "DemographicArray"),
          function(obj, force = FALSE) {
              unname(obj@.Data)
          })











