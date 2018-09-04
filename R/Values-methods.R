
## HAS_TESTS
setAs(from = "Values", to = "data.frame",
      function(from) asDataFrame(from, responseName = "value", stringsAsFactors = TRUE))

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "Values", e2 = "Counts"),
          function(e1, e2) {
              canMakePairCompatible(e1 = e1, e2 = e2, allowCopyIterDim = TRUE)
              pair <- makePairCompatible(e1 = e1, e2 = e2, check = FALSE)
              e1 <- pair[[1L]]
              e2 <- pair[[2L]]
              metadata <- metadata(e1)
              .Data <- methods::callGeneric(e1 = e1@.Data, e2 = e2@.Data)
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              if (.Generic == "*")
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              else  {
                  if (is.numeric(.Data))
                      methods::new("Values", .Data = .Data, metadata = metadata)
                  else
                      .Data
              }
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "Values", e2 = "Values"),
          function(e1, e2) {
              canMakePairCompatible(e1 = e1, e2 = e2)
              pair <- makePairCompatible(e1 = e1, e2 = e2, check = FALSE)
              e1 <- pair[[1L]]
              e2 <- pair[[2L]]
              metadata <- metadata(e1)
              .Data <- methods::callGeneric(e1 = e1@.Data, e2 = e2@.Data)
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              if (is.numeric(.Data))
                  methods::new("Values", .Data = .Data, metadata = metadata)
              else
                  .Data
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "Values", e2 = "numeric"),
          function(e1, e2) {
              .Data <- methods::callGeneric(e1 = e1@.Data, e2 = e2)
              if (is.numeric(.Data)) {
                  checkQuantilesDemographicNumeric(e1 = e1, e2 = e2, .Generic = .Generic)
                  metadata <- metadata(e1)
                  methods::new("Values", .Data = .Data, metadata = metadata)
              }
              else
                  .Data
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "numeric", e2 = "Values"),
          function(e1, e2) {
              .Data <- methods::callGeneric(e1 = e1, e2 = e2@.Data)
              if (is.numeric(.Data)) {
                  checkQuantilesNumericDemographic(e1 = e1, e2 = e2, .Generic = .Generic)
                  metadata <- metadata(e2)
                  methods::new("Values", .Data = .Data, metadata = metadata)
              }
              else
                  .Data
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "Values", e2 = "array"),
          function(e1, e2) {
              checkQuantilesDemographicArray(x = e1, .Generic = .Generic)
              canMakePairCompatible(e1 = e1, e2 = e2)
              need.to.add.iter <- !identical(dim(e1), dim(e2))
              if (need.to.add.iter)
                  e2 <- addMissingIter(x = e2, y = e1)
              .Data <- methods::callGeneric(e1 = e1@.Data, e2 = e2)
              if (is.numeric(.Data)) {
                  metadata <- metadata(e1)
                  .Data <- array(.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  methods::new("Values", .Data = .Data, metadata = metadata)
              }
              else
                  .Data
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "array", e2 = "Values"),
          function(e1, e2) {
              checkQuantilesDemographicArray(x = e2, .Generic = .Generic)
              canMakePairCompatible(e1 = e1, e2 = e2)
              need.to.add.iter <- !identical(dim(e1), dim(e2))
              if (need.to.add.iter)
                  e1 <- addMissingIter(x = e1, y = e2)
              .Data <- methods::callGeneric(e1 = e1, e2 = e2@.Data)
              if (is.numeric(.Data)) {
                  metadata <- metadata(e2)
                  .Data <- array(.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  methods::new("Values", .Data = .Data, metadata = metadata)
              }
              else
                  .Data
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "Values", e2 = "table"),
          function(e1, e2) {
              e2 <- methods::as(e2, "array")
              methods::callGeneric(e1 = e1, e2 = e2)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "table", e2 = "Values"),
          function(e1, e2) {
              e1 <- methods::as(e1, "array")
              methods::callGeneric(e1 = e1, e2 = e2)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "Values", e2 = "xtabs"),
          function(e1, e2) {
              e2 <- methods::as(e2, "array")
              methods::callGeneric(e1 = e1, e2 = e2)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "xtabs", e2 = "Values"),
          function(e1, e2) {
              e1 <- methods::as(e1, "array")
              methods::callGeneric(e1 = e1, e2 = e2)
          })

## HAS_TESTS
## Have method for Values to avoid method for arrays being selected
#' @method as.data.frame Values
#' @export
as.data.frame.Values <- function(x, row.names = NULL, optional = FALSE,
                                 stringsAsFactors = TRUE,
                                 responseName = "value",
                                 direction = c("wide", "long"),
                                 midpoints = FALSE, ...) {
    direction <- match.arg(direction)
    if (!identical(midpoints, FALSE)) {
        if (isTRUE(midpoints))
            x <- midpoints(x)
        else
            x <- midpoints(x, dimension = midpoints)
    }
    if (direction == "wide")
        as.data.frame(x@.Data, row.names = row.names, optional = optional, ...)
    else
        asDataFrame(x,
                    responseName = responseName,
                    stringsAsFactors = stringsAsFactors)
}

#' @rdname as.data.frame
#' @export
setMethod("as.data.frame",
          signature(x = "Values"),
          as.data.frame.Values)

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("canMakeCompatible",
          signature(x = "Values", y = "DemographicArray"),
          function(x, y, subset = FALSE, concordances) {
              doesNotHaveQuantiles(x)
              doesNotHaveQuantiles(y)
              e1 <- x; e2 <- y ## allow reversal of arguments
              containsNames(x = e2, y = e1)
              consistentDimtypes(e1 = x, e2 = y)
              canMakeSharedDimScalesCompatible(x = x,
                                               y = y,
                                               subset = subset,
                                               concordances = concordances)
              TRUE
          })

## HAS_TESTS
setMethod("canMakeOrigDestParentChildCompatible",
          signature(x = "Values", y = "DemographicArray"),
          function(x, y, subset = FALSE) {
              names.x <- names(x)
              dimtypes.x <- dimtypes(x, use.names = FALSE)
              dimtypes.y <- dimtypes(y, use.names = FALSE)
              for (dimtype in c("origin", "parent")) {
                  if (dimtype %in% dimtypes.y)
                      stop(gettextf("'%s' has dimension with dimtype \"%s\"",
                                    "y", dimtype))
              }
              has.orig <- "origin" %in% dimtypes.x
              if (has.orig)
                  x <- collapseOrigDest(x, to = "in", weights = 1)
              is.parent <- dimtypes.x == "parent"
              if (any(is.parent)) {
                  base <- removeSuffixes(names.x[is.parent])
                  x <- alignPair(x, base = base)
                  i.parent <- which(is.parent)
                  x <- collapseDimension(x, dimension = i.parent, weights = 1)
              }
              canMakeCompatible(x = x,
                                y = y,
                                subset = subset,
                                concordances = list())
          })

## HAS_TESTS
setMethod("canMakePairCompatible",
          signature(e1 = "Values", e2 = "Counts"),
          function(e1, e2, allowCopyIterDim = TRUE) {
              doesNotHaveQuantiles(e1)
              doesNotHaveQuantiles(e2)
              if (!allowCopyIterDim)
                  bothHaveIter(x = e1, y = e2)
              haveNamesInCommon(e1 = e1, e2 = e2, ignoreIterations = TRUE)
              consistentDimtypes(e1 = e1, e2 = e2)
              canMakeSharedDimScalePairsCompatible(e1 = e1, e2 = e2)
              TRUE
          })

## HAS_TESTS
setMethod("canMakePairCompatible",
          signature(e1 = "Values", e2 = "Values"),
          function(e1, e2) {
              doesNotHaveQuantiles(e1)
              doesNotHaveQuantiles(e2)
              consistentDimtypes(e1 = e1, e2 = e2)
              canMakeSharedDimScalePairsCompatible(e1 = e1, e2 = e2)
              TRUE
          })

## HAS_TESTS
setMethod("canMakePairCompatible",
          signature(e1 = "Values", e2 = "array"),
          function(e1, e2) {
              canMakeDemographicAndArrayCompatible(x = e1, y = e2)
          })

## HAS_TESTS
setMethod("canMakePairCompatible",
          signature(e1 = "array", e2 = "Values"),
          function(e1, e2) {
              canMakeDemographicAndArrayCompatible(x = e2, y = e1)
          })

## HAS_TESTS
setMethod("canMakeSharedDimScalesCompatible",
          signature(x = "Values", y = "DemographicArray"),
          function(x, y, subset = FALSE, concordances) {
              shared.names <- intersect(names(x), names(y))
              DimScales.x <- DimScales(x)[shared.names]
              DimScales.y <- DimScales(y)[shared.names]
              concordances <- concordances[shared.names]
              for (i in seq_along(shared.names)) {
                  return.value <-
                      tryCatch(canMakeDimScalesCompatible(x = DimScales.x[[i]],
                                                          y = DimScales.y[[i]],
                                                          subset = subset,
                                                          collapse = FALSE,
                                                          concordance = concordances[[i]]),
                               error = function(e) e)
                  if (!isTRUE(return.value))
                      stop(gettextf("\"%s\" dimensions have incompatible dimscales : %s",
                                    shared.names[i], return.value$message))
              }
              TRUE
          })

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("collapse",
          signature(object = "Values", transform = "CollapseTransform"),
          function(object, transform) {
              dims <- transform@dims
              indices <- transform@indices
              s <- seq_along(dim(object))
              containsDuplicates <- function(x) any(duplicated(x[x != 0L]))
              if (any(sapply(indices, containsDuplicates)))
                  stop(gettextf("attempt to collapse elements of object with class \"%s\"",
                                class(object)))
              metadata <- collapse(metadata(object), transform = transform)
              .Data <- collapse(object@.Data, transform = transform)
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              methods::new("Values", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
#' @rdname collapseCategories
#' @export
setMethod("collapseCategories",
          signature(object = "Values",
                    dimension = "ANY",
                    old = "ANY",
                    new = "ANY",
                    concordance = "missing",
                    weights = "ANY"),
          function(object, dimension, old, new, weights) {
              weights <- checkAndTidyWeights(weights = weights,
                                             target = object,
                                             nameWeights = "weights",
                                             nameTarget = "object",
                                             allowNA = TRUE)
              counts <- weights * object
              counts <- collapseCategories(object = counts,
                                           dimension = dimension,
                                           old = old,
                                           new = new)
              weights <- collapseCategories(object = weights,
                                            dimension = dimension,
                                            old = old,
                                            new = new)
              counts / weights
          })

## HAS_TESTS
#' @rdname collapseCategories
#' @export
setMethod("collapseCategories",
          signature(object = "Values",
                    dimension = "ANY",
                    old = "missing",
                    new = "missing",
                    concordance = "ManyToOne",
                    weights = "ANY"),
          function(object, dimension, concordance, weights) {
              weights <- checkAndTidyWeights(weights = weights,
                                             target = object,
                                             nameWeights = "weights",
                                             nameTarget = "object",
                                             allowNA = TRUE)
              counts <- weights * object
              counts <- collapseCategories(object = counts,
                                           dimension = dimension,
                                           concordance = concordance)
              weights <- collapseCategories(object = weights,
                                            dimension = dimension,
                                            concordance = concordance)
              counts / weights
          })

## HAS_TESTS
#' @rdname collapseCategories
#' @export
setMethod("collapseCategories",
          signature(object = "Values",
                    dimension = "ANY",
                    old = "missing",
                    new = "missing",
                    concordance = "OneToOne",
                    weights = "ANY"),
          function(object, dimension = NULL, concordance, weights = NULL) {
              .Data <- object@.Data
              dim <- dim(object)
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              dimension <- checkAndTidyDimColExtCat(dimension = dimension,
                                                    names = names,
                                                    DimScales = DimScales)
              for (i in dimension) {
                  dv.old <- dimvalues(DimScales[[i]])
                  dv.new <- tryCatch(translate(dv.old, concordance = concordance),
                                     error = function(e) e)
                  if (methods::is(dv.new, "error"))
                      stop(gettextf("problem translating dimension \"%s\" : %s",
                                    names[dimension[i]], dv.new$message))
                  DimScales[[i]] <- methods::new("Categories", dimvalues = dv.new)
              }
              metadata.new <- methods::new("MetaData",
                                  nms = names,
                                  dimtypes = dimtypes,
                                  DimScales = DimScales)
              dimnames(.Data) <- dimnames(metadata.new)
              methods::new("Values", .Data = .Data, metadata = metadata.new)
          })

## HAS_TESTS
#' @rdname collapseDimension
#' @export
setMethod("collapseDimension",
          signature(object = "Values",
                    dimension = "ANY",
                    margin = "ANY",
                    weights = "ANY"),
          function(object, dimension = NULL, margin = NULL, weights, na.rm = FALSE) {
              weights <- checkAndTidyWeights(weights = weights,
                                             target = object,
                                             nameWeights = "weights",
                                             nameTarget = "object",
                                             allowNA = TRUE)
              counts <- weights * object
              counts <- collapseDimension(object = counts,
                                          dimension = dimension,
                                          margin = margin,
                                          na.rm = na.rm)
              weights <- collapseDimension(object = weights,
                                           dimension = dimension,
                                           margin = margin,
                                           na.rm = na.rm)
              counts / weights
          })

## HAS_TESTS
#' @rdname collapseDimension
#' @export
setMethod("collapseDimension",
          signature(object = "Values",
                    dimension = "ANY",
                    margin = "ANY",
                    weights = "missing"),
          function(object, dimension = NULL, margin = NULL, na.rm = FALSE) {
              names <- names(object)
              n.dim <- length(names)
              has.dimension <- !is.null(dimension)
              has.margin <- !is.null(margin)
              if (has.dimension) {
                  if (has.margin)
                      stop(gettextf("has '%s' and '%s' arguments",
                                    "dimension", "margin"))
                  else {
                      dimension <- tidySubscript(subscript = dimension,
                                                 nDim = n.dim,
                                                 names = names)
                      margin <- invertSubscript(dimension, nDim = n.dim)
                  }
              }
              else {
                  if (has.margin) {
                      margin <- tidySubscript(subscript = margin,
                                              nDim = n.dim,
                                              names = names)
                      dimension <- invertSubscript(margin, nDim = n.dim)
                  }
                  else
                      stop(gettextf("no '%s' or '%s' arguments",
                                    "dimension", "margin"))
              }
              i.length.one <- which(dim(object) == 1L)
              if (all(dimension %in% i.length.one)) {
                  all.dim.length.1 <- identical(margin, integer())
                  if (all.dim.length.1)
                      object[[1L]]
                  else {
                      metadata <- metadata(object)[margin]
                      .Data <- array(object@.Data,
                                     dim = dim(metadata),
                                     dimnames = dimnames(metadata))
                      methods::new("Values", .Data = .Data, metadata = metadata)
                  }
              }
              else
                  stop(gettextf("'%s' is missing", "weights"))
          })

## HAS_TESTS
#' @rdname collapseIntervals
#' @export
setMethod("collapseIntervals",
          signature(object = "Values",
                    dimension = "ANY",
                    breaks = "ANY",
                    width = "ANY",
                    old = "ANY",
                    weights = "ANY"),
          function(object, dimension, breaks = NULL, width = NULL,
                   old = NULL, weights) {
              weights <- checkAndTidyWeights(weights = weights,
                                             target = object,
                                             nameWeights = "weights",
                                             nameTarget = "object",
                                             allowNA = TRUE)
              counts <- collapseIntervals(object = weights * object,
                                          dimension = dimension,
                                          breaks = breaks,
                                          width = width,
                                          old = old)
              weights <- collapseIntervals(object = weights,
                                           dimension = dimension,
                                           breaks = breaks,
                                           width = width,
                                           old = old)
              counts / weights
          })

## HAS_TESTS
#' @rdname collapseOrigDest
#' @export
setMethod("collapseOrigDest",
          signature(object = "Values", weights = "ANY"),
          function(object, base = NULL, to = c("net", "pool", "in", "out"),
                   weights, omitted = ifelse(methods::is(object, "Counts"), 0L, NA_integer_)) {
              weights <- checkAndTidyWeights(weights = weights,
                                             target = object,
                                             nameWeights = "weights",
                                             nameTarget = "object",
                                             allowNA = TRUE)
              counts <- weights * object
              counts <- collapseOrigDest(object = counts,
                                         base = base,
                                         to = to,
                                         omitted = omitted)
              weights <- collapseOrigDest(object = weights,
                                          base = base,
                                          to = to,
                                          omitted = omitted)
              counts / weights
          })

## HAS_TESTS
setMethod("dbind2",
          signature(e1 = "Values", e2 = "Values"),
          function(e1, e2, name1, name2, along, dimtypeAlong) {
              e1 <- fixAlongForDbind(object = e1,
                                     name = name1,
                                     along = along,
                                     dimtypeAlong = dimtypeAlong)
              e2 <- fixAlongForDbind(object = e2,
                                     name = name2,
                                     along = along,
                                     dimtypeAlong = dimtypeAlong)
              e1.is.first <- e1IsFirst(e1 = e1, e2 = e2, along = along)
              if (!e1.is.first) {
                  tmp <- e1
                  e1 <- e2
                  e2 <- tmp
              }
              checkCanCombineAlong(e1 = e1, e2 = e2, along = along)
              metadata1 <- metadata(e1)
              metadata2 <- metadata(e2)
              pair <- makePairTransformsDbind(e1 = e1, e2 = e2, along = along)
              transform1 <- pair[[1L]]
              transform2 <- pair[[2L]]
              e1 <- extend(e1, transform = transform1)
              e2 <- extend(e2, transform = transform2)
              metadata <- combineDbindMetadataValues(metadata1 = metadata1,
                                                     metadata2 = metadata2,
                                                     transform1 = transform1,
                                                     transform2 = transform2,
                                                     along = along)
              .Data <- combineDbindData(e1 = e1, e2 = e2, metadata = metadata)
              methods::new("Values", .Data = .Data, metadata = metadata)
          })


## HAS_TESTS
setMethod("dbind2",
          signature(e1 = "Values", e2 = "Counts"),
          function(e1, e2) {
              stop(gettextf("cannot combine object of class \"%s\" with object of class \"%s\"",
                            class(e1), class(e2)))
          })


## HAS_TESTS
#' @rdname dplot
#' @export
setMethod("dplot",
          signature(formula = "formula", data = "Values"),
          function(formula, data, type = NULL, panel = panel.dplot,
                   weights, midpoints = FALSE, subarray,
                   probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                   horizontal = FALSE,
                   overlay = NULL, ...) {
              ## extract info about call
              original.call <- match.call(call = sys.call(sys.parent()))
              group.vars <- all.vars(original.call$groups)
              has.response <- identical(length(formula), 3L)
              if (!has.response)
                  formula <- stats::as.formula(paste("value", deparse(formula))) ## update doesn't work with |
              ## apply subarray argument if present
              if (methods::hasArg(subarray)) {
                  subarray <- deparse(original.call$subarray)
                  subarray <- paste(subarray, collapse = "")
                  text <- sprintf("subarray(data, %s, drop = FALSE)", subarray)
                  expr <- parse(text = text)
                  data <- eval(expr)
              }
              ## collapse unused dimensions - apart from any "iteration" or "quantile" dimension
              conditioning.vars <- all.vars(formula)[-1L]
              margin <- c(conditioning.vars, group.vars)
              is.not.in.names <- !(margin %in% names(data))
              if (any(is.not.in.names))
                  stop(gettextf("'%s' does not contain a dimension called \"%s\"",
                                "data", margin[is.not.in.names][1L]))
              collapse.iter <- FALSE
              i.iter <- match("iteration", dimtypes(data), nomatch = 0L)
              has.iter <- i.iter > 0L
              if (has.iter) {
                  name.iter <- names(data)[i.iter]
                  collapse.iter <- !(name.iter %in% margin)
                  if (collapse.iter)
                      margin <- c(margin, name.iter)
              }
              i.quantile <- match("quantile", dimtypes(data), nomatch = 0L)
              has.quantile <- i.quantile > 0L
              if (has.quantile) {
                  name.quantile <- names(data)[i.quantile]
                  if (!(name.quantile %in% margin))
                      margin <- c(margin, name.quantile)
              }
              dims.to.collapse <- setdiff(names(data)[dim(data) != 1L], margin)
              n.dims.to.collapse <- length(dims.to.collapse)
              if (n.dims.to.collapse > 0L) {
                  if (has.quantile)
                      stop(gettextf("trying to collapse dimensions, but '%s' has dimension with %s \"%s\"",
                                    "data", "dimtype", "quantile"))
                  if (!methods::hasArg(weights))
                      stop(sprintf(ngettext(n.dims.to.collapse,
                                            "need to collapse %s dimension but '%s' argument not supplied",
                                            "need to collapse %s dimensions but '%s' argument not supplied"),
                                   paste(dQuote(dims.to.collapse), collapse = ", "),
                                   "weights"))
                  data <- collapseDimension(data, margin = margin, weights = weights)
              }
              ## if necessary, collapse iterations
              if (collapse.iter)
                  data <- collapseIterations(data, probs = probs, ...)
              ## convert data to data frame, with quantile stored as attribute
              i.quantile <- match("quantile", dimtypes(data), nomatch = 0L)
              data <- as.data.frame(data, direction = "long", midpoints = midpoints)
              if (i.quantile > 0L) {
                  quantile <-  data[[i.quantile]]
                  data <- data[-i.quantile]
                  attr(data, "quantile") <- quantile
              }
              ## fix up any cases where name has loss suffix
              nms <- names(data)[-length(data)]
              not.in.margin <- !(nms %in% margin)
              if (any(not.in.margin)) {
                  without.suffixes <- removeSuffixes(margin)
                  nms[not.in.margin] <- margin[match(nms[not.in.margin], without.suffixes)]
                  names(data)[-length(data)] <- nms
              }
              ## overlay
              if (!is.null(overlay)) {
                  if (!is.list(overlay))
                      stop(gettextf("'%s' has class \"%s\"",
                                    "overlay", class(overlay)))
                  data <- addOverlayToData(data = data,
                                           overlay = overlay,
                                           weights = weights,
                                           probs = probs,
                                           midpoints = midpoints)
              }
              ## horizontal
              if (horizontal) {
                  y.orig <- formula[[2L]]
                  rhs <- formula[[3]]
                  rhs.one.term <- length(rhs) == 1L
                  if (rhs.one.term)
                      x.orig <- rhs
                  else
                      x.orig <- rhs[[2L]]
                  formula[[2L]] <- x.orig
                  if (rhs.one.term)
                      formula[[3L]] <- y.orig
                  else
                      formula[[3L]][[2L]] <- y.orig
              }
              ## call 'xyplot' with panel = panel.dplot,
              ## then fix "call" attribute of result 
              is.data <- attr(data, "is.data")
              quantile <- attr(data, "quantile")
              ans <- lattice::xyplot(x = formula,
                                     data = data,
                                     type = type,
                                     panel = panel,
                                     quantile = quantile,
                                     horizontal = horizontal,
                                     is.data = is.data,
                                     overlay = overlay,
                                     ...)
              ans$call <- original.call
              ans
          })


## HAS_TESTS
#' @rdname expandCategories
#' @export
setMethod("expandCategories",
          signature(object = "Values",
                    dimension = "ANY",
                    old = "ANY",
                    new = "ANY",
                    concordance = "missing",
                    weights = "ANY"),
          function(object, dimension, old, new, weights) {
              .Data.obj <- object@.Data
              dim.obj <- dim(object)
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              dimension <- checkAndTidyDimColExtCat(dimension = dimension,
                                                    names = names,
                                                    DimScales = DimScales)
              if (!is.null(weights))
                  warning(gettextf("'%s' argument ignored",
                                   "weights"))
              old <- checkAndTidyOldNew(old, name = "old", lengthOne = TRUE)
              new <- checkAndTidyOldNew(new, name = "new", lengthOne = FALSE)
              dims <- seq_along(dim.obj)
              indices <- lapply(dim.obj, seq_len)
              for (i in dimension) {
                  dv.obj <- dimvalues(DimScales[[i]])
                  i.old <- match(old, dv.obj, nomatch = 0L)
                  not.found <- i.old == 0L
                  if (not.found) {
                      stop(gettextf("cannot expand category for dimension \"%s\" : value \"%s\" not found",
                                    names[i], old))
                  }
                  dv.ans <- dv.obj[-i.old]
                  dv.ans <- append(dv.ans, values = new, after = i.old - 1L)
                  ind.ans <- indices[[i]][-i.old]
                  i.old.rep <- rep(i.old, times = length(new))
                  ind.ans <- append(ind.ans, values = i.old.rep, after = i.old - 1L)
                  DimScales[[i]] <- methods::new("Categories", dimvalues = dv.ans)
                  indices[[i]] <- ind.ans
              }
              metadata.ans <- methods::new("MetaData",
                                    nms = names,
                                    dimtypes = dimtypes,
                                    DimScales = DimScales)
              dim.ans <- dim(metadata.ans)
              transform <- methods::new("ExtendTransform",
                               dims = dims,
                               indices = indices,
                               dimBefore = dim.obj,
                               dimAfter = dim.ans)
              .Data.ans <- extend(.Data.obj, transform = transform)
              dimnames(.Data.ans) <- dimnames(metadata.ans)
              methods::new("Values", .Data = .Data.ans, metadata = metadata.ans)
          })

## HAS_TESTS
#' @rdname expandCategories
#' @export
setMethod("expandCategories",
          signature(object = "Values",
                    dimension = "ANY",
                    old = "missing",
                    new = "missing",
                    concordance = "ManyToOne",
                    weights = "ANY"),
          function(object, dimension, concordance, weights) {
              .Data.obj <- object@.Data
              dim.obj <- dim(object)
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              dimension <- checkAndTidyDimColExtCat(dimension = dimension,
                                                    names = names,
                                                    DimScales = DimScales)
              if (!is.null(weights))
                  warning(gettextf("'%s' argument ignored",
                                   "weights"))
              classif.to <- classificationTo(concordance)
              classif.from <- classificationFrom(concordance)
              codes.to <- codes(concordance, classification = classif.to)
              codes.from <- codes(concordance, classification = classif.from)
              dims <- seq_along(dim.obj)
              indices <- lapply(dim.obj, seq_len)
              for (i in dimension) {
                  dv.obj <- dimvalues(DimScales[[i]])
                  i.to <- match(dv.obj, codes.to, nomatch = 0L)
                  found.in.to <- i.to > 0L
                  if (any(!found.in.to)) {
                      first.obj.not.found <- dv.obj[!found.in.to][1L]
                      stop(gettextf("cannot expand categories for dimension \"%s\" : value \"%s\" not found in classification '%s'",
                                    names[i], first.obj.not.found, classif.to))
                  }
                  i.obj <- match(codes.to, dv.obj, nomatch = 0L)
                  found.in.obj <- i.obj > 0L
                  dv.new <- codes.from[found.in.obj]
                  DimScales[[i]] <- methods::new("Categories", dimvalues = dv.new)
                  indices[[i]] <- i.obj[found.in.obj]
              }
              metadata.ans <- methods::new("MetaData",
                                  nms = names,
                                  dimtypes = dimtypes,
                                  DimScales = DimScales)
              dim.ans <- dim(metadata.ans)
              dimnames.ans <- dimnames(metadata.ans)
              transform <- methods::new("ExtendTransform",
                               dims = dims,
                               indices = indices,
                               dimBefore = dim.obj,
                               dimAfter = dim.ans)
              .Data.ans <- extend(.Data.obj, transform = transform)
              .Data.ans <- array(.Data.ans, dim = dim.ans, dimnames = dimnames.ans)
              methods::new("Values", .Data = .Data.ans, metadata = metadata.ans)
          })              

## HAS_TESTS
#' @rdname growth
#' @export
setMethod("growth",
          signature(object = "Values"),
          function(object, along = NULL, within = NULL, weights,
                   type = c("exponential", "linear"),
                   method = c("endpoints", "lm")) {
              metadata <- metadata(object)
              dim <- dim(object)
              names <- names(object)
              dimtypes <- dimtypes(object)
              DimScales <- DimScales(object)
              along <- checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
              name.along <- names[along]
              s <- seq_along(dim)
              type <- match.arg(type)
              method <- match.arg(method)
              if ("quantile" %in% dimtypes)
                  stop(gettextf("dimension with dimtype \"%s\"", "quantile"))
              n <- dim[along]
              if (n < 2L)
                  stop(gettextf("cannot calculate growth along dimension \"%s\" because dimension has length %d",
                                name.along, n))
              if (is.null(within))
                  within <- integer()
              else if (identical(within, "."))
                  within <- s[-along]
              else {
                  if (any(is.na(within)))
                      stop(gettextf("'%s' has missing values", "within"))
                  if (any(duplicated(within)))
                      stop(gettextf("'%s' has duplicates", "within"))
                  if (any(dimtypes %in% getDimtypesWithPairs())) {
                      base.names <- removeSuffixes(names)
                      matches <- lapply(within, grep, x = base.names)
                      within <- as.list(within)
                      for (i in seq_along(within)) {
                          if (length(matches[[i]]) > 0L)
                              within[[i]] <- names[matches[[i]]]
                      }
                      within <- unlist(within)
                  }
                  if (!is.numeric(within))
                      within <- match(within, names, nomatch = 0L)
                  if (!all(within %in% s))
                      stop(gettextf("'%s' outside valid range", "within"))
                  if (along %in% within)
                      stop(gettextf("dimension \"%s\" included in '%s' and '%s'",
                                    name.along, "along", "within"))
              }
              i.iter <- match("iteration", dimtypes, nomatch = 0L)
              has.iter <- i.iter > 0L
              if (has.iter) {
                  if (!(i.iter %in% within))
                      within <- c(within, i.iter)
              }
              if (missing(weights))
                  weights <- 1
              weights <- checkAndTidyWeights(weights = weights,
                                             target = object,
                                             nameWeights = "weights",
                                             nameTarget = "object",
                                             allowNA = TRUE)
              values <- collapseDimension(object,
                                          margin = c(within, along),
                                          weights = weights)
              values <- matrix(as.numeric(values), ncol = n)
              DimScale <- DimScales[[along]]
              if (methods::is(DimScale, "Points"))
                  distance <- dimvalues(DimScale)
              else
                  distance <- dimvalues(intervalsToPoints(DimScale))
              if (identical(type, "linear")) {
                  if (identical(method, "endpoints"))
                      FUN <- function(v) (v[n] - v[1L]) / (distance[n] - distance[1L])
                  else if (identical(method, "lm"))
                      FUN <- function(v) stats::coef(stats::lm(v ~ distance))["distance"]
                  else
                      stop(gettextf("invalid value for '%s' : \"%s\"",
                                    "method", method))
              }
              else if (identical(type, "exponential")) {
                  if (identical(method, "endpoints"))
                      FUN <- function(v)
                          (v[n] / v[1L])^(1 / (distance[n] - distance[1L])) - 1
                  else if (identical(method, "lm")) {
                      FUN <- function(v) {
                          slope <- stats::coef(stats::lm(log(v) ~ distance))["distance"]
                          exp(slope) - 1
                      }
                  }
                  else
                      stop(gettextf("invalid value for '%s' : \"%s\"",
                                    "method", method))
              }
              else
                  stop(gettextf("invalid value for '%s': \"%s\"",
                                "type", type))
              .Data <- apply(values, MARGIN = 1L, FUN = FUN)
              if (identical(length(within), 0L))
                  .Data
              else {
                  metadata <- metadata(object)[within]
                  .Data <- array(.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  methods::new("Values", .Data = .Data, metadata = metadata)
              }
          })

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("makeCompatible",
          signature(x = "Values", y = "DemographicArray"),
          function(x, y, subset = FALSE, check = TRUE) {
              if (check)
                  canMakeCompatible(x = x, y = y, subset = subset)
              metadata <- metadata(y)
              transform <- makeTransform(x = x, y = y, subset = subset, check = FALSE)
              .Data <- extend(x@.Data, transform = transform)
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              methods::new("Values", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
## makes 'x' weakly compatible with 'y' - keeps any
## orig-dest or parent-child dimensions in 'x'
setMethod("makeOrigDestParentChildCompatible",
          signature(x = "Values", y = "DemographicArray"),
          function(x, y, subset = FALSE, check = TRUE) {
              x <- alignPair(x)
              if (check)
                  canMakeOrigDestParentChildCompatible(x = x,
                                                       y = y,
                                                       subset = subset)
              metadata <- makeMetadataExtendOrigDestParentChild(x = x, y = y)
              transform <- makeOrigDestParentChildTransform(x = x,
                                                            y = y,
                                                            subset = subset,
                                                            check = FALSE)
              .Data <- extend(x@.Data, transform = transform)
              if (!identical(dim(.Data), dim(metadata)))
                  stop(gettextf("'%s' and '%s' have different dimensions",
                                ".Data", "metadata"))
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              methods::new("Values", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
## transform that makes 'x' weakly compatible with 'y' while
## keeping orig-dest or parent-child format
setMethod("makeOrigDestParentChildTransform",
          signature(x = "Values", y = "DemographicArray"),
          function(x, y, subset = FALSE, check = TRUE) {
              if (check)
                  canMakeOrigDestParentChildCompatible(x = x,
                                                       y = y,
                                                       subset = subset,
                                                       allowCopyIterDim = FALSE)
              names.x <- names(x)
              names.y <- names(y)
              dim.y <- dim(y)
              dimtypes.x <- dimtypes(x, use.names = FALSE)
              dimtypes.y <- dimtypes(y, use.names = FALSE)
              DimScales.x <- DimScales(x, use.names = FALSE)
              DimScales.y <- DimScales(y, use.names = FALSE)
              dim.before <- dim(x)
              base.orig <- removeSuffixes(names.x[dimtypes.x == "origin"])
              base.parent <- removeSuffixes(names.x[dimtypes.x == "parent"])
              suffixes.orig.dest <- getSuffixes(c("origin", "destination"))
              suffixes.parent.child <- getSuffixes(c("parent", "child"))
              dims <- vector(mode = "list", length = length(names.y))
              indices <- vector(mode = "list", length = length(names.y))
              dim.after <- vector(mode = "list", length = length(names.y))
              for (i.y in seq_along(dims)) {
                  name.y <- names.y[i.y]
                  if (name.y %in% base.orig)
                      names.after <- paste0(name.y, suffixes.orig.dest)
                  else if (name.y %in% base.parent)
                      names.after <- paste0(name.y, suffixes.parent.child)
                  else
                      names.after <- name.y
                  dims[[i.y]] <- match(names.after, names.x, nomatch = 0L)
                  d.y <- dim.y[i.y]
                  add <- identical(dims[[i.y]], 0L)
                  if (add) {
                      indices[[i.y]] <- list(rep(1L, times = d.y))
                      dim.after[[i.y]] <- d.y
                  }
                  else {
                      pair <- identical(length(dims[[i.y]]), 2L)
                      if (pair) {
                          indices[[i.y]] <- vector(mode = "list", length = 2L)
                          for (j in 1:2) {
                              i.x <- dims[[i.y]][j]
                              DimScale.x <- DimScales.x[[i.x]]
                              DimScale.y <- DimScales.y[[i.y]]
                              indices[[i.y]][[j]] <- makeIndices(x = DimScale.x,
                                                                 y = DimScale.y,
                                                                 collapse = FALSE,
                                                                 concordance = NULL)
                          }
                          dim.after[[i.y]] <- rep(d.y, times = 2L)
                      }
                      else {
                          i.x <- dims[[i.y]]
                          DimScale.x <- DimScales.x[[i.x]]
                          DimScale.y <- DimScales.y[[i.y]]
                          indices[[i.y]] <- list(makeIndices(x = DimScale.x,
                                                             y = DimScale.y,
                                                             collapse = FALSE,
                                                             concordance = NULL))
                          dim.after[[i.y]] <- d.y
                      }
                  }
              }
              dims <- unlist(dims)
              indices <- unlist(indices, recursive = FALSE)
              dim.after <- unlist(dim.after)
              methods::new("ExtendTransform",
                  dims = dims,
                  indices = indices,
                  dimBefore = dim.before,
                  dimAfter = dim.after)
          })

## HAS_TESTS
setMethod("makePairCompatible",
          signature(e1 = "Values", e2 = "Counts"),
          function(e1, e2, check = TRUE) {
              if (check)
                  canMakePairCompatible(e1 = e1, e2 = e2,
                                        allowCopyIterDim = TRUE)
              e2 <- copyZeroDim(x = e2, y = e1)
              e2 <- copyIterDim(x = e2, y = e1)
              pair <- makePairTransforms(e1 = e1, e2 = e2, check = FALSE)
              messageAboutPairSubsetting(pair)
              .Data1 <- extend(e1, transform = pair[[1L]])
              ans2 <- collapse(e2, transform = pair[[2L]])
              metadata <- metadata(ans2)
              .Data1 <- array(.Data1,
                              dim = dim(metadata),
                              dimnames = dimnames(metadata))
              ans1 <- methods::new("Values", .Data = .Data1, metadata = metadata)
              list(ans1, ans2)
          })

## HAS_TESTS
setMethod("makePairCompatible",
          signature(e1 = "Values", e2 = "Values"),
          function(e1, e2, check = TRUE) {
              if (check)
                  canMakePairCompatible(e1 = e1, e2 = e2)
              pair <- makePairTransforms(e1 = e1, e2 = e2, check = FALSE)
              messageAboutPairSubsetting(pair)
              metadata <- mergeMetadata(metadata1 = metadata(e1),
                                        metadata2 = metadata(e2),
                                        transform1 = pair[[1L]],
                                        transform2 = pair[[2L]])
              .Data1 <- extend(object = e1@.Data, transform = pair[[1L]])
              .Data1 <- array(.Data1,
                              dim = dim(metadata),
                              dimnames = dimnames(metadata))
              .Data2 <- extend(object = e2@.Data, transform = pair[[2L]])
              .Data2 <- array(.Data2,
                              dim = dim(metadata),
                              dimnames = dimnames(metadata))
              ans1 <- methods::new("Values", .Data = .Data1, metadata = metadata)
              ans2 <- methods::new("Values", .Data = .Data2, metadata = metadata)
              list(ans1, ans2)              
          })

## HAS_TESTS
setMethod("makePairTransforms",
          signature(e1 = "Values", e2 = "Counts"),
          function(e1, e2, check = TRUE) {
              if (check)
                  canMakePairCompatible(e1 = e1, e2 = e2)
              DimScales1 <- DimScales(e1, use.names = FALSE)
              DimScales2 <- DimScales(e2, use.names = FALSE)
              dimBefore1 <- dim(e1)
              dimBefore2 <- dim(e2)
              names.after <- c(intersect(names(e1), names(e2)),
                               setdiff(names(e2), names(e1)))
              dims1 <- match(names.after, names(e1), nomatch = 0L)
              dims2 <- match(names(e2), names.after)
              indices1 <- vector(mode = "list", length = length(names.after))
              indices2 <- vector(mode = "list", length = length(names.after))
              for (i in seq_along(names.after)) {
                  d1 <- dims1[i]
                  d2 <- match(i, dims2)
                  if (d1 > 0L) {
                      pair <- makePairIndices(e1 = DimScales1[[d1]],
                                              e2 = DimScales2[[d2]],
                                              isCounts1 = FALSE,
                                              isCounts2 = TRUE)
                      indices1[[i]] <- pair[[1L]]
                      indices2[[d2]] <- pair[[2L]]
                  }
                  else {
                      length.dim <- dimBefore2[d2]
                      indices1[[i]] <- rep(1L, times = length.dim)
                      indices2[[d2]] <- seq_len(length.dim)
                  }
              }
              dimAfter <- sapply(indices1, length)
              list(methods::new("ExtendTransform",
                       dims = dims1,
                       indices = indices1,
                       dimBefore = dimBefore1,
                       dimAfter = dimAfter),
                   methods::new("CollapseTransform",
                       dims = dims2,
                       indices = indices2,
                       dimBefore = dimBefore2,
                       dimAfter = dimAfter))
          })

## HAS_TESTS
setMethod("makePairTransforms",
          signature(e1 = "Values", e2 = "Values"),
          function(e1, e2, check = TRUE) {
              if (check)
                  canMakePairCompatible(e1 = e1, e2 = e2)
              DimScales1 <- DimScales(e1, use.names = FALSE)
              DimScales2 <- DimScales(e2, use.names = FALSE)
              dimBefore1 <- dim(e1)
              dimBefore2 <- dim(e2)
              names.after <- union(names(e1), names(e2))
              dims1 <- match(names.after, names(e1), nomatch = 0L)
              dims2 <- match(names.after, names(e2), nomatch = 0L)
              indices1 <- vector(mode = "list", length = length(names.after))
              indices2 <- vector(mode = "list", length = length(names.after))
              for (i in seq_along(names.after)) {
                  d1 <- dims1[i]
                  d2 <- dims2[i]
                  if (d1 > 0L) {
                      if (d2 > 0L) {
                          pair <- makePairIndices(e1 = DimScales1[[d1]],
                                                  e2 = DimScales2[[d2]],
                                                  isCounts1 = FALSE,
                                                  isCounts2 = FALSE)
                          indices1[[i]] <- pair[[1L]]
                          indices2[[i]] <- pair[[2L]]
                      }
                      else {
                          length.dim <- dimBefore1[d1]
                          indices1[[i]] <- seq_len(length.dim)
                          indices2[[i]] <- rep(1L, times = length.dim)
                      }
                  }
                  else {
                      length.dim <- dimBefore2[d2]
                      indices1[[i]] <- rep(1L, times = length.dim)
                      indices2[[i]] <- seq_len(length.dim)
                  }
              }
              dimAfter <- sapply(indices1, length)
              list(methods::new("ExtendTransform",
                       dims = dims1,
                       indices = indices1,
                       dimBefore = dimBefore1,
                       dimAfter = dimAfter),
                   methods::new("ExtendTransform",
                       dims = dims2,
                       indices = indices2,
                       dimBefore = dimBefore2,
                       dimAfter = dimAfter))
          })

## HAS_TESTS
setMethod("makePairTransformsDbind",
          signature(e1 = "Values", e2 = "Values"),
          function(e1, e2, along) {
              e1.slab <- slab(e1,
                                dimension = along,
                                elements = integer(),
                                drop = FALSE)
              e2.slab <- slab(e2,
                                dimension = along,
                                elements = integer(),
                                drop = FALSE)
              canMakePairCompatible(e1 = e1.slab,
                                    e2 = e2.slab,
                                    allowCopyIterDim = TRUE)
              dimtype.along.tmp <- dimtypes(e1)[[along]]
              if (!identical(dimtype.along.tmp, "iteration")) {
                  e1 <- copyIterDim(x = e1, y = e2)
                  e2 <- copyIterDim(x = e2, y = e1)
              }
              names1 <- names(e1)
              names2 <- names(e2)
              dimBefore1 <- dim(e1)
              dimBefore2 <- dim(e2)
              DimScales1 <- DimScales(e1, use.names = FALSE)
              DimScales2 <- DimScales(e2, use.names = FALSE)
              names.after <- union(names1, names2)
              names.after <- c(setdiff(names.after, along), along)
              n.after <- length(names.after)
              dims1 <- match(names.after, names1, nomatch = 0L)
              dims2 <- match(names.after, names2, nomatch = 0L)
              indices1 <- vector(mode = "list", length = n.after)
              indices2 <- vector(mode = "list", length = n.after)
              for (i in seq_len(n.after)) {
                  d1 <- dims1[i]
                  d2 <- dims2[i]
                  if (d1 > 0L) {
                      if (d2 > 0L) {
                          if (i == n.after) {
                              indices1[[i]] <- seq_len(dimBefore1[d1])
                              indices2[[i]] <- seq_len(dimBefore2[d2])
                          }
                          else {
                              pair <- makePairIndices(e1 = DimScales1[[d1]],
                                                      e2 = DimScales2[[d2]],
                                                      isCounts1 = FALSE,
                                                      isCounts2 = FALSE)
                              indices1[[i]] <- pair[[1L]]
                              indices2[[i]] <- pair[[2L]]
                          }
                      }
                      else {
                          length.dim <- dimBefore1[d1]
                          indices1[[i]] <- seq_len(length.dim)
                          indices2[[i]] <- rep(1L, times = length.dim)
                      }
                  }
                  else {
                      length.dim <- dimBefore2[d2]
                      indices1[[i]] <- rep(1L, times = length.dim)
                      indices2[[i]] <- seq_len(length.dim)
                  }
              }
              dimAfter1 <- sapply(indices1, length)
              dimAfter2 <- sapply(indices2, length)
              list(methods::new("ExtendTransform",
                       dims = dims1,
                       indices = indices1,
                       dimBefore = dimBefore1,
                       dimAfter = dimAfter1),
                   methods::new("ExtendTransform",
                       dims = dims2,
                       indices = indices2,
                       dimBefore = dimBefore2,
                       dimAfter = dimAfter2))
          })

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("makeTransform",
          signature(x = "Values", y = "DemographicArray"),
          function(x, y, subset = FALSE, concordances = list(), check = TRUE) {
              concordances <- tidyConcordanceList(concordances = concordances,
                                                  object = y)
              if (check)
                  canMakeCompatible(x = x,
                                    y = y,
                                    subset = subset,
                                    concordances = concordances)
              names.x <- names(x)
              names.y <- names(y)
              DimScales.x <- DimScales(x)
              DimScales.y <- DimScales(y)
              dimBefore <- dim(x)
              dimAfter <- dim(y)
              dims <- match(names.y, names.x, nomatch = 0L)
              indices <- vector(mode = "list", length = length(dims))
              for (i in seq_along(indices)) {
                  add <- identical(dims[i], 0L)
                  if (add)
                      indices[[i]] <- rep(1L, times = dimAfter[i])
                  else {
                      DimScale.x <- DimScales.x[[dims[i]]]
                      DimScale.y <- DimScales.y[[i]]
                      concordance <- concordances[[i]]
                      indices[[i]] <- makeIndices(x = DimScale.x,
                                                  y = DimScale.y,
                                                  collapse = FALSE,
                                                  concordance = concordance)
                  }
              }
              methods::new("ExtendTransform",
                  dims = dims,
                  indices = indices,
                  dimBefore = dimBefore,
                  dimAfter = dimAfter)
          })

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("makeTransform",
          signature(x = "Values", y = "numeric"),
          function(x, y, subset = FALSE, check = TRUE) {
              if (check) {
                  if (!identical(length(y), 1L))
                      stop(gettextf("'%s' has class \"%s\" but does not have length %d",
                                    "y", class(y), 1L))
                  if (identical(length(x), 0L))
                      stop(gettextf("'%s' has length %d",
                                    "x", 0L))
              }
              dimBefore <- dim(x)
              indices <- lapply(dimBefore, function(n) rep(1L, times = n))
              dims <- c(1L, rep(0L, times = length(dimBefore) - 1L))
              methods::new("CollapseTransform",
                  indices = indices,
                  dims = dims,
                  dimBefore = dimBefore,
                  dimAfter = 1L)
          })                  

## HAS_TESTS
#' @export
#' @method plot Values
plot.Values <- function(x, threshold = 20, main = NULL, cex.main = 1.2,
                        col.main = "black", font.main = 2, las = 1, ...) {
    n <- length(names(x))
    nrow <- ceiling(sqrt(n))
    ncol <- ceiling(n / nrow)
    mfrow <- c(nrow, ncol)
    oma <- if (is.null(main)) rep(0, 4) else c(0, 0, 3, 0)
    mar <- c(3, 6, 2, 1)
    old.par <- graphics::par(mfrow = mfrow, oma = oma, mar = mar)
    on.exit(graphics::par(old.par))
    data <- methods::as(x, "data.frame")
    for (margin in seq_len(n))
        plotSingleDimensionValues(data = data,
                                  margin = margin,
                                  threshold = threshold,
                                  las = las,
                                  ...)
    if (!is.null(main))
        graphics::mtext(text = main, outer = TRUE, line = 1, cex = cex.main,
              col = col.main, font = font.main)
}

#' @rdname plot-methods
#' @export
setMethod("plot",
          signature(x = "Values"),
          plot.Values)


## HAS_TESTS
plotSingleDimensionValues <- function(data, margin, threshold, las, ...) {
    main <- names(data)[margin]
    formula <- stats::as.formula(paste("value", main, sep = "~"))
    if (sum(stats::complete.cases(data)) > 0L) {
        values.per.category <- nrow(data) / nlevels(data[[margin]])
        do.boxplot <- values.per.category >= threshold
        if (do.boxplot)
            graphics::boxplot(formula,
                    data = data,
                    horizontal = TRUE,
                    main = main,
                    las = las,
                    ...)
        else
            graphics::stripchart(formula,
                       data = data,
                       main = main,
                       las = las,
                       ...)
    }
    else {
        graphics::plot(x = c(0, 1), y = c(0, 1), type = "n",
             ylab = "", axes = FALSE, main = main)
        graphics::text(gettext("no values to plot"), x = 0.5, y = 0.5)
    }
}

## HAS_TESTS
#' @rdname reallocateToEndAges
#' @export
setMethod("reallocateToEndAges",
          signature(object = "Values",
                    weights = "missing"),
          function(object, min = 15, max = 50, weights, ...) {
              stop(gettextf("'%s' is missing",
                            "weights"))
          })

## HAS_TESTS
#' @rdname reallocateToEndAges
#' @export
setMethod("reallocateToEndAges",
          signature(object = "Values",
                    weights = "Counts"),
          function(object, min = 15, max = 50, weights, ...) {
              weights <- checkAndTidyWeights(weights = weights,
                                             target = object,
                                             nameWeights = "weights",
                                             nameTarget = "object",
                                             allowNA = TRUE)
              counts <- reallocateToEndAges(object = weights * object,
                                              min = min,
                                              max = max,
                                              ...)
              weights <- makeCompatible(x = weights,
                                        y = counts,
                                        subset = TRUE,
                                        check = TRUE)
              counts / weights
          })

## NO_TESTS
#' @rdname redistribute
#' @export
setMethod("redistribute",
          signature(counts = "Values",
                    weights = "DemographicArray"),
          function(counts, weights, n = NULL) {
              stop(gettextf("function '%s' cannot be used when '%s' has class \"%s\"",
                            "redistribute", "counts", class(counts)))
          })

## HAS_TESTS
#' @rdname tfr
#' @export
setMethod("tfr",
          signature(object = "Values"),
          function(object) {
              .Data <- object@.Data
              metadata <- object@metadata
              dim <- dim(.Data)
              dimnames <- dimnames(.Data)
              names <- names(metadata)
              dimtypes <- dimtypes(metadata, use.names = FALSE)
              DimScales <- DimScales(metadata, use.names = FALSE)
              if (identical(length(.Data), 0L))
                  stop(gettextf("'%s' has length %d",
                                "object", 0L))
              if (any(.Data < 0, na.rm = TRUE))
                  stop(gettext("negative values"))
              i.sex <- match("sex", dimtypes, nomatch = 0L)
              i.triangle <- match("triangle", dimtypes, nomatch = 0L)
              i.age <- match("age", dimtypes, nomatch = 0L)
              checkAge(object, minAges = 1L)
              has.triangle <- i.triangle > 0
              if (has.triangle) {
                  width <- ageTimeStep(object)
                  .Data.person.years <- array(0.5 * width,
                                              dim = dim,
                                              dimnames = dimnames)
              }
              else {
                  DimScale.age <- DimScales[[i.age]]
                  dimvalues.age <- dimvalues(DimScale.age)
                  widths <- diff(dimvalues.age)
                  if (i.age > 1L) {
                      each <- prod(dim[seq_len(i.age - 1L)])
                      widths <- rep(widths, each = each)
                  }
                  .Data.person.years <- array(widths,
                                              dim = dim,
                                              dimnames = dimnames)
              }
              person.years <- methods::new("Counts",
                                           .Data = .Data.person.years,
                                           metadata = metadata)
              object <- person.years * object
              dimension <- c(i.age, i.sex, i.triangle)
              dimension <- dimension[dimension != 0L]
              collapseDimension(object, dimension = dimension)
          })

## HAS_TESTS
#' @rdname resetDiag
#' @export
setMethod("resetDiag",
          signature(object = "Values",
                    reset = "ANY"),
          function(object, base = NULL, reset = NULL) {
              resetDiagInner(object = object,
                             base = base,
                             reset = reset)
          })

## HAS_TESTS
#' @rdname resetDiag
#' @export
setMethod("resetDiag",
          signature(object = "Values",
                    reset = "NULL"),
          function(object, base = NULL, reset = NULL) {
              reset <- NA_integer_
              resetDiagInner(object = object,
                             base = base,
                             reset = reset)
          })

## HAS_TESTS
#' @rdname resetDiag
#' @export
setMethod("resetDiag",
          signature(object = "Values",
                    reset = "missing"),
          function(object, base = NULL, reset = NULL) {
              reset <- NA_integer_
              resetDiagInner(object = object,
                             base = base,
                             reset = reset)
          })


## HAS_TESTS
#' @rdname round3
#' @export
setMethod("round3",
          signature(object = "Values"),
          function(object) {
              metadata <- object@metadata
              .Data <- object@.Data
              .Data <- round3(.Data)
              ## recreate object to trigger validity tests
              new(class(object),
                  .Data = .Data,
                  metadata = metadata)
          })
