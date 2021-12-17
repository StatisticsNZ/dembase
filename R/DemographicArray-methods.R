
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
#' @rdname addDimension
#' @export
setMethod("addDimension",
          signature(object = "DemographicArray"),
          function(object, name, labels, after = length(dim(object)),
                   dimtype = NULL, dimscale = NULL, scale = 1L) {
              class <- if (methods::is(object, "Counts")) "Counts" else "Values"
              n.name <- length(name)
              n.dim <- length(dim(object))
              if (is.list(labels)) {
                  if (length(labels) > n.name)
                      stop(gettextf("'%s' has more elements than '%s'",
                                    "labels", "name"))
              }
              else
                  labels <- list(labels)
              labels <- rep(labels, length.out = n.name)
              labels <- lapply(labels, as.character)
              if (!identical(length(after), 1L))
                  stop(gettextf("'%s' does not have length %d", "after", 1L))
              if (is.character(after))
                  after <- match(after, names(object), nomatch = -1L)
              if (!(after %in% seq.int(from = 0, to = n.dim)))
                  stop(gettextf("'%s' outside valid range", "after"))
              if (is.list(scale)) {
                  if (length(scale) > n.name)
                      stop(gettextf("'%s' has more elements than '%s'",
                                    "scale", "name"))
              }
              else
                  scale <- list(scale)
              scale <- rep(scale, length.out = n.name)
              for (i in seq_len(n.name)) {
                  n.scale <- length(scale[[i]])
                  n.labels <- length(labels[[i]])
                  if (n.scale < n.labels)
                      scale[[i]] <- rep(scale[[i]], length.out = n.labels)
                  else if (n.scale > n.labels)
                      stop(gettextf("'%s' has more elements than '%s'",
                                    "scale", "labels"))
              }
              if (is.null(dimtype))
                  dimtype <- inferDimtypes(name)
              else {
                  n.dimtype <- length(dimtype)
                  if (n.dimtype < n.name)
                      dimtype <- rep(dimtype, length.out = n.name)
                  else if (n.dimtype > n.name)
                      stop(gettextf("'%s' has more elements than '%s'",
                                    "dimtype", "name"))
              }
              if (is.null(dimscale))
                  dimscale <- list(NULL)
              else {
                  n.dimscale <- length(dimscale)
                  if (n.dimscale < n.name)
                      dimscale <- rep(dimscale, length.out = n.name)
                  else if (n.dimscale > n.name)
                      stop(gettextf("'%s' has more elements than '%s'",
                                    "dimscale", "name"))
              }
              DimScale <- mapply(inferDimScale,
                                 dimtype = dimtype,
                                 dimscale = dimscale,
                                 labels = labels,
                                 name = name,
                                 SIMPLIFY = FALSE,
                                 USE.NAMES = FALSE)
              nms <- c(names(object), name)
              dimtypes <- c(dimtypes(object, use.names = FALSE), unlist(dimtype))
              DimScales <- c(DimScales(object, use.names = FALSE), DimScale)
              metadata <- methods::new("MetaData",
                                       nms = nms,
                                       dimtypes = dimtypes,
                                       DimScales = DimScales)
              .Data <- c(object@.Data)
              all.integer <- all(sapply(scale, is.integer))
              scale <- Reduce("%o%", scale)
              if (all.integer)
                  scale <- as.integer(scale)
              scale <- rep(scale, each = length(.Data))
              .Data <- scale * .Data
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              permute <- !identical(after, n.dim)
              if (permute) {
                  old <- seq_len(n.dim)
                  new <- seq.int(from = n.dim + 1L, length.out = length(name))
                  perm <- append(old, values = new, after = after)
                  .Data <- aperm(.Data, perm = perm)
                  metadata <- metadata[perm]
              }
              methods::new(class,
                           .Data = .Data,
                           metadata = metadata)
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

 
## NO_TESTS
#' @rdname credibleInterval
#' @export
setMethod("credibleInterval",
          signature(object = "DemographicArray"),
          function(object, width = 95, na.rm = FALSE,
                   adjust = c("search", "expand", "none")) {
              dim <- dim(object)
              .Data <- object@.Data
              dimtypes.obj <- dimtypes(object, use.names = FALSE)
              i.iter <- match("iteration", dimtypes.obj, nomatch = 0L)
              has.iter <- i.iter > 0L
              if (!has.iter)
                  stop(gettextf("'%s' does not have a dimension with %s \"%s\"",
                                "object", "dimtype", "iteration"))
              n.iter <- dim[i.iter]
              width <- checkAndTidyPercentage(width)
              if (isTRUE(all.equal(width, 0)))
                  stop(gettextf("'%s' equals %d",
                                "width", 0L))
              checkLogicalFlag(value = na.rm,
                               name = "na.rm")
              adjust <- match.arg(adjust)
              q <- 1 - width / 100
              prob <- c(q/2, 1 - q/2)
              if (na.rm) {
                  quantile <- stats::quantile
                  ans <- collapseIterations(object,
                                            FUN = quantile,
                                            prob = prob,
                                            na.rm = na.rm)
              }
              else {
                  names <- paste0(prob * 100, "%")
                  quantile <- function(x) {
                      if (any(is.na(x)))
                          structure(.Data = c(NA, NA), names = names)
                      else
                          stats::quantile(x, prob = prob)
                  }
                  ans <- collapseIterations(object,
                                            FUN = quantile)
              }
              if (all(is.na(ans)))
                  return(ans)
              if (identical(adjust, "none"))
                  return(ans)
              .Data.obs <- !is.na(.Data)
              .Data.whole.num <- (is.integer(.Data)
                  || (all(.Data[.Data.obs] == as.integer(.Data[.Data.obs]))))
              if (!.Data.whole.num)
                  return(ans)
              dimtypes.ans <- dimtypes(ans, use.names = FALSE)
              i.quantile <- match("quantile", dimtypes.ans)
              lower <- slab(ans,
                            dimension = i.quantile,
                            elements = 1L,
                            drop = "dimension")
              upper <- slab(ans,
                            dimension = i.quantile,
                            elements = 2L,
                            drop = "dimension")
              floor.lower <- floor(lower)
              ceiling.upper <- ceiling(upper)
              if (identical(adjust, "search")) {
                  ceiling.lower <- ceiling(lower)
                  floor.upper <- floor(upper)
                  width.floor.floor <- floor.upper@.Data - floor.lower@.Data
                  width.ceiling.floor <- floor.upper@.Data - ceiling.lower@.Data
                  width.floor.ceiling <- ceiling.upper@.Data - floor.lower@.Data
                  width.ceiling.ceiling <- ceiling.upper@.Data - ceiling.lower@.Data
                  width.all <- matrix(c(width.floor.floor,
                                        width.ceiling.floor,
                                        width.floor.ceiling,
                                        width.ceiling.ceiling),                                        
                                      nrow = length(width.floor.floor),
                                      ncol = 4L)
                  if (length(lower) > 1L)
                      x <- as(lower, "Values")
                  else
                      x <- lower
                  transform.extend <- makeTransform(x = x,
                                                    y = object)
                  x <- as(object, "Counts")
                  dimtypes(x)[i.iter] <- "state"
                  transform.collapse <- makeTransform(x = x,
                                                      y = lower)
                  floor.lower.ext <- extend(floor.lower@.Data,
                                            transform = transform.extend)
                  ceiling.lower.ext <- extend(ceiling.lower@.Data,
                                              transform = transform.extend)
                  floor.upper.ext <- extend(floor.upper@.Data,
                                            transform = transform.extend)
                  ceiling.upper.ext <- extend(ceiling.upper@.Data,
                                              transform = transform.extend)
                  inside.floor.floor <- 1L * ((.Data.obs & (floor.lower.ext <= .Data))
                      & (.Data.obs & (.Data <= floor.upper.ext)))
                  inside.ceiling.floor <- 1L * ((.Data.obs & (ceiling.lower.ext <= .Data))
                      & (.Data.obs & (.Data <= floor.upper.ext)))
                  inside.floor.ceiling <- 1L * ((.Data.obs & (floor.lower.ext <= .Data))
                      & (.Data.obs & (.Data <= ceiling.upper.ext)))
                  inside.ceiling.ceiling <- 1L * ((.Data.obs & (ceiling.lower.ext <= .Data))
                      & (.Data.obs & (.Data <= ceiling.upper.ext)))
                  sum.floor.floor <- collapse(inside.floor.floor,
                                              transform = transform.collapse)
                  sum.ceiling.floor <- collapse(inside.ceiling.floor,
                                                transform = transform.collapse)
                  sum.floor.ceiling <- collapse(inside.floor.ceiling,
                                                transform = transform.collapse)
                  sum.ceiling.ceiling <- collapse(inside.ceiling.ceiling,
                                                  transform = transform.collapse)
                  sum.obs <- collapse(1L * .Data.obs,
                                      transform = transform.collapse)
                  cover.floor.floor <- sum.floor.floor / sum.obs
                  cover.ceiling.floor <- sum.ceiling.floor / sum.obs
                  cover.floor.ceiling <- sum.floor.ceiling / sum.obs
                  cover.ceiling.ceiling <- sum.ceiling.ceiling / sum.obs
                  cover.all <- matrix(c(cover.floor.floor,
                                        cover.ceiling.floor,
                                        cover.floor.ceiling,
                                        cover.ceiling.ceiling),                                        
                                      nrow = length(cover.floor.floor),
                                      ncol = 4L)
                  width.all[is.na(cover.all) | (cover.all < (width / 100))] <- Inf
                  best.combination <- apply(width.all, 1, which.min)
                  lower.all <- matrix(c(floor.lower,
                                        ceiling.lower,
                                        floor.lower,
                                        ceiling.lower),                                        
                                      nrow = length(floor.lower),
                                      ncol = 4L)
                  upper.all <- matrix(c(floor.upper,
                                        floor.upper,
                                        ceiling.upper,
                                        ceiling.upper),                                        
                                      nrow = length(floor.upper),
                                      ncol = 4L)
                  index.best <- cbind(seq_along(best.combination),
                                      best.combination)
                  lower <- lower.all[index.best]
                  upper <- upper.all[index.best]
              }
              else if (identical(adjust, "expand")) {
                  lower <- floor.lower
                  upper <- ceiling.upper
              }
              else
                  stop(gettextf("invalid value for '%s' : \"%s\"",
                                "adjust", adjust))
              slab(ans,
                   dimension = i.quantile,
                   elements = 1L) <- lower
              slab(ans,
                   dimension = i.quantile,
                   elements = 2L) <- upper
              ans <- toInteger(ans)
              ans
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
              d <- as.data.frame(object,
                                 direction = "long",
                                 midpoints = TRUE,
                                 stringsAsFactors = TRUE)
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
                  predictors <- paste(names(d)[seq_len(p)], collapse = " + ")
              else
                  predictors <- "1"
              formula <- stats::as.formula(sprintf("%s ~ %s",
                                                   names(d)[length(d)],
                                                   predictors))
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
                  imputed.values <- stats::rnorm(n = n.missing,
                                                 mean = predicted,
                                                 sd = sqrt(abs(predicted)))
              if (!is.null(max)) {
                  max.imputed <- max[i.missing]
                  exceeds.max <- imputed.values > max.imputed
                  imputed.values[exceeds.max] <- max.imputed[exceeds.max]
              }
              object[i.missing] <- imputed.values
              object
          })



## HAS_TESTS
#' @rdname intervalContainsTruth
#' @export
setMethod("intervalContainsTruth",
          signature(interval = "DemographicArray",
                    truth = "DemographicArray"),
          function(interval, truth) {
              checkIntervalArray(interval)
              checkTruthArray(truth)
              checkIntervalAndTruthArrayCompatible(interval = interval,
                                                   truth = truth)
              metadata <- truth@metadata
              l <- splitLowerUpper(interval)
              lower <- l$lower
              upper <- l$upper
              lower <- makeCompatible(x = lower,
                                      y = truth,
                                      subset = FALSE)
              upper <- makeCompatible(x = upper,
                                      y = truth,
                                      subset = FALSE)
              truth <- truth@.Data
              .Data <- 1L * ((lower <= truth) & (truth <= upper))
              new("Values",
                  .Data = .Data,
                  metadata = metadata)
          })


## HAS_TESTS
#' @rdname intervalContainsTruth
#' @export
setMethod("intervalContainsTruth",
          signature(interval = "DemographicArray",
                    truth = "numeric"),
          function(interval, truth) {
              checkIntervalArray(interval)
              checkTruthNumeric(truth)
              checkIntervalAndTruthNumericCompatible(interval = interval,
                                                     truth = truth)
              l <- splitLowerUpper(interval)
              lower <- l$lower
              upper <- l$upper
              ans <- (lower <= truth) & (truth <= upper)
              as.integer(ans)
          })

#' @rdname intervalScore
setMethod("intervalScore",
          signature(interval = "DemographicArray",
                    truth = "DemographicArray"),
          function(interval, truth) {
              checkIntervalArray(interval)
              checkTruthArray(truth)
              checkIntervalAndTruthArrayCompatible(interval = interval,
                                                   truth = truth)
              metadata <- truth@metadata
              alpha <- getAlphaInterval(interval)
              l <- splitLowerUpper(interval)
              lower <- l$lower
              upper <- l$upper
              lower <- makeCompatible(x = lower,
                                      y = truth,
                                      subset = FALSE)
              upper <- makeCompatible(x = upper,
                                      y = truth,
                                      subset = FALSE)
              lower <- lower@.Data
              upper <- upper@.Data
              truth <- truth@.Data
              width <- upper - lower
              penalty.below.lower <- (2 / alpha) * (lower - truth) * (truth < lower)
              penalty.above.upper <- (2 / alpha) * (truth - upper) * (truth > upper)
              .Data <- width + penalty.below.lower + penalty.above.upper
              new("Counts",
                  .Data = .Data,
                  metadata = metadata)
          })

#' @rdname intervalScore
setMethod("intervalScore",
          signature(interval = "DemographicArray",
                    truth = "numeric"),
          function(interval, truth) {
              checkIntervalArray(interval)
              checkTruthNumeric(truth)
              checkIntervalAndTruthNumericCompatible(interval = interval,
                                                     truth = truth)
              alpha <- getAlphaInterval(interval)
              l <- splitLowerUpper(interval)
              lower <- l$lower
              upper <- l$upper
              lower <- lower@.Data
              upper <- upper@.Data
              width <- upper - lower
              penalty.below.lower <- (2 / alpha) * (lower - truth) * (truth < lower)
              penalty.above.upper <- (2 / alpha) * (truth - upper) * (truth > upper)
              ans <- width + penalty.below.lower + penalty.above.upper
              as.numeric(ans)
          })



#' @rdname intervalWidth
#' @export
setMethod("intervalWidth",
          signature(interval = "DemographicArray"),
          function(interval) {
              checkIntervalArray(interval)
              l <- splitLowerUpper(interval)
              lower <- l$lower
              upper <- l$upper
              upper - lower
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
median.DemographicArray <- function(x, na.rm = FALSE, ...) {
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
                           na.rm = na.rm,
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
        stats::median(.Data, na.rm = na.rm, ...)
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

## NO_TESTS
#' @rdname MSE
#' @export
setMethod("MSE",
          signature(point = "DemographicArray",
                    truth = "DemographicArray"),
          function(point, truth) {
              checkPointArray(point)
              checkTruthArray(truth)
              checkPointAndTruthCompatible(point = point,
                                           truth = truth)
              (truth - point)^2 # 'truth' first, so that answer has same metadata as 'truth'
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
#' @rdname pairToState
#' @export
setMethod("pairToState",
          signature(object = "DemographicArray"),
          function(object) {
              .Data.old <- object@.Data
              names.old <- names(object)
              dimtypes.old <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              dimtypes.with.pairs <- getDimtypesWithPairs()
              names.new <- names.old
              dimtypes.new <- dimtypes.old
              for (i in seq_along(names.old)) {
                  name.old <- names.old[i]
                  dimtype.old <- dimtypes.old[i]
                  if (dimtype.old %in% dimtypes.with.pairs) {
                      dimtypes.new[i] <- "state"
                      suffix <- getSuffixes(dimtype.old)
                      pattern <- paste0(suffix, "$")
                      replacement <- sub("^_", ".", suffix)
                      name.new <- sub(pattern = pattern,
                                      replacement = replacement,
                                      x = name.old)
                      names.new[i] <- name.new
                  }
              }
              metadata.new <- methods::new("MetaData",
                                           nms = names.new,
                                           dimtypes = dimtypes.new,
                                           DimScales = DimScales)
              .Data.new <- array(.Data.old,
                                 dim = dim(metadata.new),
                                 dimnames = dimnames(metadata.new))
              new(class(object),
                  .Data = .Data.new,
                  metadata = metadata.new)
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
#' @rdname population
#' @export
setMethod("population",
          signature(object = "DemographicArray"),
          function(object) {
              as(object@population, "Counts")
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

## HAS_TESTS
#' @rdname recodeCategories
#' @export
setMethod("recodeCategories",
          signature(object = "DemographicArray",
                    dimension = "ANY",
                    old = "ANY",
                    new = "ANY",
                    concordance = "missing"),
          function(object, dimension = NULL, old = NULL, new = NULL,
                   concordance = NULL) {
              dim <- dim(object)
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              dimension <- tidySubscript(dimension,
                                         nDim = length(dim),
                                         names = names)
              for (name in c("old", "new")) {
                  value <- get(name)
                  if (any(is.na(value)))
                      stop(gettextf("'%s' has missing values",
                                    name))
                  if (any(duplicated(value)))
                      stop(gettextf("'%s' has duplicates",
                                    name))
              }
              if (!identical(length(old), length(new)))
                  stop(gettextf("'%s' and '%s' have different lengths",
                                "old", "new"))
              for (i.dim in seq_along(dim)) {
                  recode.this.dimension <- i.dim %in% dimension
                  if (recode.this.dimension) {
                      DimScale <- DimScales[[i.dim]]
                      if (!methods::is(DimScale, "Categories"))
                          stop(gettextf("dimension \"%s\" has dimscale \"%s\"",
                                        names[i.dim], class(DimScale)))
                      labels.old <- dimvalues(DimScale)
                      labels.new <- labels.old
                      name.dim <- names[i.dim]
                      for (i.old in seq_along(old)) {
                          label.old <- old[i.old]
                          i.label <- match(label.old, labels.old, nomatch = 0L)
                          has.label <- i.label > 0L
                          if (has.label)
                              labels.new[i.label] <- new[i.old]
                          else {
                              stop(gettextf("'%s' includes value \"%s\" but \"%s\" dimension of '%s' does not include \"%s\"",
                                            "old", label.old, name.dim, "object", label.old, "strict", "TRUE"))
                          }
                      }
                      class.dimscale <- class(DimScale)
                      DimScale <- tryCatch(methods::new(class.dimscale, dimvalues = labels.new),
                                           error = function(e) e)
                      if (methods::is(DimScale, "error"))
                          stop(gettextf("problem creating \"%s\" %s for \"%s\" dimension : %s",
                                        class.dimscale, "dimscale", name.dim, DimScale$message))
                      DimScales[[i.dim]] <- DimScale
                  }
              }
              metadata <- methods::new("MetaData",
                                       nms = names,
                                       dimtypes = dimtypes,
                                       DimScales = DimScales)
              .Data <- object@.Data
              dimnames(.Data)[dimension] <- dimnames(metadata)[dimension]
              methods::new(class(object),
                           .Data = .Data,
                           metadata = metadata)
          })


## HAS_TESTS
#' @rdname recodeCategories
#' @export
setMethod("recodeCategories",
          signature(object = "DemographicArray",
                    dimension = "ANY",
                    old = "missing",
                    new = "missing",
                    concordance = "Concordance"),
          function(object, dimension = NULL,
                   old = NULL, new = NULL,
                   concordance = NULL) {
              dim <- dim(object)
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              dimension <- tidySubscript(dimension,
                                         nDim = length(dim),
                                         names = names)
              if (!methods::is(concordance, "OneToOne"))
                  stop(gettextf("'%s' has class \"%s\"",
                                "concordance", class(concordance)))
              for (i.dim in seq_along(dim)) {
                  recode.this.dimension <- i.dim %in% dimension
                  if (recode.this.dimension) {
                      name.dim <- names[i.dim]
                      DimScale <- DimScales[[i.dim]]
                      if (!methods::is(DimScale, "Categories"))
                          stop(gettextf("\"%s\" dimension has dimscale \"%s\"",
                                        names[i.dim], class(DimScale)))
                      labels.old <- dimvalues(DimScale)
                      labels.new <- tryCatch(translate(labels.old,
                                                       concordance = concordance),
                                             error = function(e) e)
                      if (methods::is(labels.new, "error"))
                          stop(gettextf("unable to recode categories for \"%s\" dimension : %s",
                                        name.dim, labels.new$message))
                      class.dimscale <- class(DimScale)
                      DimScale <- tryCatch(methods::new(class.dimscale, dimvalues = labels.new),
                                           error = function(e) e)
                      if (methods::is(DimScale, "error"))
                          stop(gettextf("problem creating \"%s\" %s for \"%s\" dimension : %s",
                                        class.dimscale, "dimscale", name.dim, DimScale$message))
                      DimScales[[i.dim]] <- DimScale
                  }
              }
              metadata <- methods::new("MetaData",
                                       nms = names,
                                       dimtypes = dimtypes,
                                       DimScales = DimScales)
              .Data <- object@.Data
              dimnames(.Data)[dimension] <- dimnames(metadata)[dimension]
              methods::new(class(object),
                           .Data = .Data,
                           metadata = metadata)
          })

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


## NO_TESTS
#' @rdname rotateAgeTime
#' @export
setMethod("rotateAgeTime",
          signature(object = "DemographicArray"),
          function(object, to = NULL, name = NULL) {
              choices.at <- c("at", "ta", "age-time", "time-age")
              choices.ac <- c("ac", "ca", "age-cohort", "cohort-age")
              choices.tc <- c("tc", "ct", "time-cohort", "cohort-time")
              choices.to <- c(choices.at, choices.ac, choices.tc)
              hasRegularAgeTime(object)
              .Data.old <- object@.Data
              names.old <- names(object)
              dim.old <- dim(object)
              dimtypes.old <- dimtypes(object,
                                       use.names = FALSE)
              DimScales.old <- DimScales(object,
                                         use.names = FALSE)
              n.dim <- length(dim.old)
              n.old <- length(.Data.old)
              pos.old <- seq_len(n.old) - 1L # C-style
              i.age <- match("age", dimtypes.old, nomatch = 0L)
              i.time <- match("time", dimtypes.old, nomatch = 0L)
              i.cohort <- match("cohort", dimtypes.old, nomatch = 0L)
              i.triangle <- match("triangle", dimtypes.old, nomatch = 0L)
              has.age <- i.age > 0L
              has.time <- i.time > 0L
              has.cohort <- i.cohort > 0L
              has.triangle <- i.triangle > 0L
              if (has.age + has.time + has.cohort != 2L)
                  stop(gettextf("'%s' must have dimensions with two of the following three dimtypes: \"%s\", \"%s\", \"%s\"",
                                "object", "age", "time", "cohort"))
              has.at <- has.age && has.time
              has.ac <- has.age && has.cohort
              has.tc <- has.time && has.cohort
              if (is.null(to))
                  stop(gettextf("argument \"%s\" is missing, with no default",
                                "to"))
              to <- tolower(to)
              to <- match.arg(to, choices = choices.to)
              to.at <- to %in% choices.at
              to.ac <- to %in% choices.ac
              to.tc <- to %in% choices.tc
              no.change <- (has.at && to.at) || (has.ac && to.ac) || (has.tc && to.tc)
              if (no.change)
                  return(object)
              if (!is.null(name)) {
                  if (!identical(length(name), 1L))
                      stop(gettextf("'%s' does not have length %d",
                                    "name", 1L))
                  if (is.na(name))
                      stop(gettextf("'%s' is missing",
                                    "name"))
              }
              if (has.age) {
                  DimScale.age <- DimScales.old[[i.age]]
                  dv.age <- dimvalues(DimScale.age)
                  if (any(is.infinite(dv.age)))
                      stop(gettextf("dimension with %s \"%s\" has open interval",
                                    "dimtype", "age"))
                  if (any(dv.age < 0))
                      stop(gettext("cannnegative ages"))
                  n.age <- dim.old[i.age]
                  step.age <- 1L
                  for (d in seq_len(i.age - 1L))
                      step.age <- step.age * dim.old[d]
                  pos.age <- (pos.old %/% step.age) %% n.age # C-style
              }
              if (has.time) {
                  DimScale.time <- DimScales.old[[i.time]]
                  dv.time <- dimvalues(DimScale.time)
                  if (any(is.infinite(dv.time)))
                      stop(gettextf("dimension with %s \"%s\" has open interval",
                                    "dimtype", "time"))
                  n.time <- dim.old[i.time]
                  step.time <- 1L
                  for (d in seq_len(i.time - 1L))
                      step.time <- step.time * dim.old[d]
                  pos.time <- (pos.old %/% step.time) %% n.time # C-style
              }
              if (has.cohort) {
                  DimScale.cohort <- DimScales.old[[i.cohort]]
                  dv.cohort <- dimvalues(DimScale.cohort)
                  if (any(is.infinite(dv.cohort)))
                      stop(gettextf("dimension with %s \"%s\" has open interval",
                                    "dimtype", "cohort"))
                  n.cohort <- dim.old[i.cohort]
                  step.cohort <- 1L
                  for (d in seq_len(i.cohort - 1L))
                      step.cohort <- step.cohort * dim.old[d]
                  pos.cohort <- (pos.old %/% step.cohort) %% n.cohort # C-style
              }
              if (has.triangle) {
                  DimScale.triangle <- DimScales.old[[i.triangle]]
                  n.triangle <- dim.old[i.triangle]
                  step.triangle <- 1L
                  for (d in seq_len(i.triangle - 1L))
                      step.triangle <- step.triangle * dim.old[d]
                  pos.triangle <- (pos.old %/% step.triangle) %% n.triangle # C-style
              }
              else
                  pos.triangle <- rep.int(0L, times = n.old)
              if (has.at) { # new dimension is cohort
                  if (is.null(name))
                      name <- "cohort"
                  DimScale.cohort <- makeMissingAgeTimeDimScale(age = DimScale.age,
                                                                time = DimScale.time)
                  pos.along.new <- pos.time - pos.age - pos.triangle
                  if (to.ac) { # replacing time with cohort
                      name <- make.unique(c(names.old[-i.time], name))[n.dim]
                      names.new <- replace(names.old,
                                           list = i.time,
                                           values = name)
                      dimtypes.new <- replace(dimtypes.old,
                                              list = i.time,
                                              values = "cohort")
                      DimScales.new <- replace(DimScales.old,
                                               list = i.time,
                                               values = list(DimScale.cohort))
                      iAlong <- i.time
                  }
                  else { # replacing age with cohort
                      name <- make.unique(c(names.old[-i.age], name))[n.dim]
                      names.new <- replace(names.old,
                                           list = i.age,
                                           values = name)
                      dimtypes.new <- replace(dimtypes.old,
                                              list = i.age,
                                              values = "cohort")
                      DimScales.new <- replace(DimScales.old,
                                               list = i.age,
                                               values = list(DimScale.cohort))
                      iAlong <- i.age
                  }
              }
              else if (has.ac) { # new dimension is time
                  if (is.null(name))
                      name <- "time"
                  DimScale.time <- makeMissingAgeTimeDimScale(age = DimScale.age,
                                                              cohort = DimScale.cohort)
                  pos.along.new <- pos.cohort + pos.age + pos.triangle
                  if (to.at) { # replacing cohort with time
                      name <- make.unique(c(names.old[-i.cohort], name))[n.dim]
                      names.new <- replace(names.old,
                                           list = i.cohort,
                                           values = name)
                      dimtypes.new <- replace(dimtypes.old,
                                              list = i.cohort,
                                              values = "time")
                      DimScales.new <- replace(DimScales.old,
                                               list = i.cohort,
                                               values = list(DimScale.time))
                      iAlong <- i.cohort
                  }
                  else { # replacing age with time
                      name <- make.unique(c(names.old[-i.age], name))[n.dim]
                      names.new <- replace(names,
                                           list = i.age,
                                           values = name)
                      dimtypes.new <- replace(dimtypes.old,
                                              list = i.age,
                                              values = "time")
                      DimScales.new <- replace(DimScales,
                                               list = i.age,
                                               values = list(DimScale.time))
                      iAlong <- i.age
                  }
              }
              else { # has.tc - new dimension is age
                  if (!all(dv.cohort <= max(dv.time)))
                      stop(gettext("cohort starting after final time point or interval"))
                  if (is.null(name))
                      name <- "age"
                  DimScale.age <- makeMissingAgeTimeDimScale(time = DimScale.time,
                                                             cohort = DimScale.cohort)
                  pos.along.new <- pos.time - pos.cohort - pos.triangle
                  if (to.ac) { # replacing time with age
                      name <- make.unique(c(names.old[-i.time], name))[n.dim]
                      names.new <- replace(names.old,
                                           list = i.time,
                                           values = name)
                      dimtypes.new <- replace(dimtypes.old,
                                              list = i.time,
                                              values = "age")
                      DimScales.new <- replace(DimScales.old,
                                               list = i.time,
                                               values = list(DimScale.age))
                      iAlong <- i.time
                  }
                  else { # replacing cohort with age
                      name <- make.unique(c(names.old[-i.cohort], name))[n.dim]
                      names.new <- replace(names.old,
                                           list = i.cohort,
                                           values = name)
                      dimtypes.new <- replace(dimtypes.old,
                                              list = i.cohort,
                                              values = "age")
                      DimScales.new <- replace(DimScales.old,
                                               list = i.cohort,
                                               values = list(DimScale.age))
                      iAlong <- i.cohort
                  }
              }
              metadata.new <- new("MetaData",
                                  nms = names.new,
                                  dimtypes = dimtypes.new,
                                  DimScales = DimScales.new)
              dim.new <- dim(metadata.new)
              dimnames.new <- dimnames(metadata.new)
              .Data.new <- array(NA_integer_,
                                 dim = dim.new,
                                 dimnames = dimnames.new)
              n.along.old <- dim.old[iAlong]
              n.along.new <- dim.new[iAlong]
              if (iAlong > 1L) {
                  s.before <- seq.int(from = 1L, to = iAlong - 1L)
                  n.within <- prod(dim.old[s.before])
              }
              else
                  n.within <- 1L
              if (iAlong < n.dim) {
                  s.after <- seq.int(from = iAlong + 1L, to = n.dim)
                  n.between <- prod(dim.old[s.after])
              }
              else
                  n.between <- 1L
              seq.within <- seq_len(n.within) - 1L
              seq.along.old <- seq_len(n.along.old) - 1L
              seq.between <- seq_len(n.between) - 1L
              pos.within.old <- rep(seq.within, times = n.along.old * n.between)
              pos.along.old <- rep(rep(seq.along.old, each = n.within), times = n.between)
              pos.along.new <- pos.along.new - min(pos.along.new)
              pos.between.old <- rep(seq.between, each = n.within * n.along.old)
              pos.new <- pos.within.old + n.within * pos.along.new + n.within * n.along.new * pos.between.old
              pos.new <- pos.new + 1L # R-style
              .Data.new[pos.new] <- .Data.old
              class.new <- if (methods::is(object, "Counts")) "Counts" else "Values"
              new(class.new,
                  .Data = .Data.new,
                  metadata = metadata.new)
          })



## HAS_TESTS
#' @rdname ageMinMax
#' @export
setMethod("setAgeMax",
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
setMethod("setAgeMin",
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
              drop <- checkAndTidyDrop(drop)
              indices <- lapply(dim.before, seq_len)
              indices[[dimension]] <- match(s, elements, nomatch = 0L)
              dim.after <- replace(dim.before,
                                   list = dimension,
                                   values = length(elements))
              transform <- methods::new("CollapseTransform",
                                        dims = dims,
                                        indices = indices,
                                        dimBefore = dim.before,
                                        dimAfter = dim.after)
              ans <- collapse(object, transform = transform)
              if (identical(drop, TRUE)) {
                  is.length.1 <- dim.after == 1L
                  if (all(is.length.1))
                      ans <- ans[[1L]]
                  else {
                      metadata <- metadata(ans)[!is.length.1]
                      .Data <- array(ans@.Data,
                                     dim = dim(metadata),
                                     dimnames = dimnames(metadata))
                      ans <- methods::new(class(ans),
                                          .Data = .Data,
                                          metadata = metadata)
                  }
              }
              if (identical(drop, "dimension")) {
                  dimension.now.has.length.1 <- dim.after[dimension] == 1L
                  if (dimension.now.has.length.1) {
                      is.only.dimension <- length(dim.after) == 1L
                      if (is.only.dimension)
                          ans <- ans[[1L]]
                      else {
                          metadata <- metadata(ans)[-dimension]
                          .Data <- array(ans@.Data,
                                         dim = dim(metadata),
                                         dimnames = dimnames(metadata))
                          ans <- methods::new(class(ans),
                                              .Data = .Data,
                                              metadata = metadata)
                      }
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











