
## HAS_TESTS
#' @export
setAs(from = "Counts", to = "data.frame",
      function(from) asDataFrame(from, responseName = "count", stringsAsFactors = TRUE))

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "Counts", e2 = "Counts"),
          function(e1, e2) {
              canMakePairCompatible(e1 = e1, e2 = e2, allowCopyIterDim = TRUE)
              pair <- makePairCompatible(e1 = e1, e2 = e2)
              e1 <- pair[[1L]]
              e2 <- pair[[2L]]
              .Data <- methods::callGeneric(e1 = e1@.Data, e2 = e2@.Data)
              metadata <- metadata(e1)
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              if (is.numeric(.Data)) {
                  if (.Generic %in% c("+", "-"))
                      methods::new("Counts", .Data = .Data, metadata = metadata)
                  else
                      methods::new("Values", .Data = .Data, metadata = metadata)
              }
              else
                  .Data
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "Counts", e2 = "Values"),
          function(e1, e2) {
              canMakePairCompatible(e1 = e1, e2 = e2, allowCopyIterDim = TRUE)
              pair <- makePairCompatible(e1 = e1, e2 = e2, check = FALSE)
              e1 <- pair[[1L]]
              e2 <- pair[[2L]]
              .Data <- methods::callGeneric(e1 = e1@.Data, e2 = e2@.Data)
              metadata <- metadata(e1)                  
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              if (is.numeric(.Data)) {
                  if (.Generic  %in% c("*", "/", "%/%", "^"))
                      methods::new("Counts", .Data = .Data, metadata = metadata)
                  else
                      methods::new("Values", .Data = .Data, metadata = metadata)
              }
              else
                  .Data
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "Counts", e2 = "numeric"),
          function(e1, e2) {
              .Data <- methods::callGeneric(e1 = e1@.Data, e2 = e2)
              if (is.numeric(.Data)) {
                  checkQuantilesDemographicNumeric(e1 = e1, e2 = e2, .Generic = .Generic)
                  metadata <- metadata(e1)
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  .Data
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "numeric", e2 = "Counts"),
          function(e1, e2) {
              .Data <- methods::callGeneric(e1 = e1, e2 = e2@.Data)
              if (is.numeric(.Data)) {
                  checkQuantilesNumericDemographic(e1 = e1, e2 = e2, .Generic = .Generic)
                  metadata <- metadata(e2)
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  .Data
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "Counts", e2 = "array"),
          function(e1, e2) {
              checkQuantilesDemographicArray(x = e1, .Generic = .Generic)
              canMakePairCompatible(e1 = e1, e2 = e2)
              need.to.add.iter <- !identical(dim(e1), dim(e2))
              if (need.to.add.iter)
                  e2 <- addMissingIter(x = e2, y = e1)
              .Data <- methods::callGeneric(e1 = e1@.Data, e2 = e2)
              metadata <- metadata(e1)
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              if (is.numeric(.Data))
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              else
                  .Data
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "array", e2 = "Counts"),
          function(e1, e2) {
              checkQuantilesDemographicArray(x = e2, .Generic = .Generic)
              canMakePairCompatible(e1 = e1, e2 = e2)
              need.to.add.iter <- !identical(dim(e1), dim(e2))
              if (need.to.add.iter)
                  e1 <- addMissingIter(x = e1, y = e2)
              .Data <- methods::callGeneric(e1 = e1, e2 = e2@.Data)
              metadata <- metadata(e2)
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              if (is.numeric(.Data))
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              else
                  .Data
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "Counts", e2 = "table"),
          function(e1, e2) {
              e2 <- methods::as(e2, "array")
              methods::callGeneric(e1 = e1, e2 = e2)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "table", e2 = "Counts"),
          function(e1, e2) {
              e1 <- methods::as(e1, "array")
              methods::callGeneric(e1 = e1, e2 = e2)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "Counts", e2 = "xtabs"),
          function(e1, e2) {
              e2 <- methods::as(e2, "array")
              methods::callGeneric(e1 = e1, e2 = e2)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Ops",
          signature(e1 = "xtabs", e2 = "Counts"),
          function(e1, e2) {
              e1 <- methods::as(e1, "array")
              methods::callGeneric(e1 = e1, e2 = e2)
          })

## HAS_TESTS
#' @rdname addDimension
#' @export
setMethod("addDimension",
          signature(object = "Counts", scale = "missing"),
          function(object, name, labels, after = length(dim(object)),
                   dimtype = NULL, dimscale = NULL) {
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
              not.iteration <- dimtype != "iteration"
              length.dim <- sapply(labels, length)
              too.long <- not.iteration & (length.dim > 1L)
              n.too.long <- sum(too.long)
              if (n.too.long > 0L)
                  stop(sprintf(ngettext(n.too.long,
                                        "new dimension not of length 1 [%s]",
                                        "new dimensions not of length 1 [%s]"),
                               paste(dQuote(name[too.long]), collapse = ", ")))
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
              .Data <- array(object@.Data,
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
              methods::new("Counts", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
## Have method for Counts to avoid method for arrays being selected
#' @method as.data.frame Counts
#' @export
as.data.frame.Counts <- function(x, row.names = NULL, optional = FALSE,
                                 stringsAsFactors = TRUE, responseName = "count",
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
          signature(x = "Counts"),
          as.data.frame.Counts)

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("canMakeCompatible",
          signature(x = "Counts", y = "DemographicArray"),
          function(x, y, subset = FALSE, concordances, allowCopyIterDim = TRUE) {
              doesNotHaveQuantiles(x)
              doesNotHaveQuantiles(y)
              alsoHasIterations(x = x, y = y)
              containsNames(x = x, y = y, ignoreIterations = allowCopyIterDim)
              alsoHasZeroLengthDim(x = x, y = y)
              consistentDimtypes(e1 = x, e2 = y)
              canMakeSharedDimScalesCompatible(x = x,
                                               y = y,
                                               subset = subset,
                                               concordances = concordances)
              TRUE
          })

## HAS_TESTS
setMethod("canMakeOrigDestParentChildCompatible",
          signature(x = "Counts", y = "DemographicArray"),
          function(x, y, subset = FALSE, allowCopyIterDim = TRUE) {
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
                  x <- collapseOrigDest(x, to = "in")
              is.parent <- dimtypes.x == "parent"
              if (any(is.parent)) {
                  base <- removeSuffixes(names.x[is.parent])
                  x <- alignPair(x, base = base)
                  i.parent <- which(is.parent)
                  x <- collapseDimension(x, dimension = i.parent)
              }
              canMakeCompatible(x = x,
                                y = y,
                                subset = subset,
                                concordances = list(),
                                allowCopyIterDim = allowCopyIterDim)
          })

## HAS_TESTS
setMethod("canMakePairCompatible",
          signature(e1 = "Counts", e2 = "Counts"),
          function(e1, e2, allowCopyIterDim = TRUE) {
              doesNotHaveQuantiles(e1)
              doesNotHaveQuantiles(e2)
              if (!allowCopyIterDim) {
                  bothHaveIter(x = e1, y = e2)
                  bothHaveIter(x = e2, y = e1)
              }
              haveNamesInCommon(e1 = e1, e2 = e2, ignoreIterations = TRUE)
              consistentDimtypes(e1 = e1, e2 = e2)
              canMakeSharedDimScalePairsCompatible(e1 = e1, e2 = e2)
              TRUE
          })

## HAS_TESTS
setMethod("canMakePairCompatible",
          signature(e1 = "Counts", e2 = "Values"),
          function(e1, e2, allowCopyIterDim = TRUE) {
              doesNotHaveQuantiles(e1)
              doesNotHaveQuantiles(e2)
              if (!allowCopyIterDim)
                  bothHaveIter(x = e2, y = e1)
              haveNamesInCommon(e1 = e1, e2 = e2, ignoreIterations = TRUE)
              consistentDimtypes(e1 = e1, e2 = e2)
              canMakeSharedDimScalePairsCompatible(e1 = e1, e2 = e2)
              TRUE
          })

## HAS_TESTS
setMethod("canMakePairCompatible",
          signature(e1 = "Counts", e2 = "array"),
          function(e1, e2) {
              canMakeDemographicAndArrayCompatible(x = e1, y = e2)
          })

## HAS_TESTS
setMethod("canMakePairCompatible",
          signature(e1 = "array", e2 = "Counts"),
          function(e1, e2) {
              canMakeDemographicAndArrayCompatible(x = e2, y = e1)
          })

## HAS_TESTS
setMethod("canMakeSharedDimScalesCompatible",
          signature(x = "Counts", y = "DemographicArray"),
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
                                                          collapse = TRUE,
                                                          concordance = concordances[[i]]),
                               error = function(e) e)
                  if (!isTRUE(return.value))
                      stop(gettextf("\"%s\" dimensions have incompatible dimscales : %s",
                                    shared.names[i], return.value$message))
              }
              TRUE
          })

## HAS_TESTS
setMethod("checkAndTidyWeights",
          signature(weights = "Counts",
                    target = "DemographicArray"),
          function(weights, target, nameWeights = "weights", nameTarget = "object",
                   allowNA = FALSE) {
              .Data <- weights@.Data
              names.weights <- names(weights)
              dimtypes.weights <- dimtypes(weights, use.names = FALSE)
              DimScales.weights <- DimScales(weights, use.names = FALSE)
              names.target <- names(target)
              dimtypes.target <- dimtypes(target, use.names = FALSE)
              DimScales.target <- DimScales(target, use.names = FALSE)
              ## check values valid
              if (!allowNA) {
                  if (any(is.na(.Data)))
                      stop(gettextf("'%s' has missing values",
                                    nameWeights))
              }
              if (any(.Data[!is.na(.Data)] < 0L))
                  stop(gettextf("'%s' has negative values",
                                nameWeights))
              ## add any extra dimensions present in 'target'
              names.add <- setdiff(names.target, names(weights))
              if (length(names.add) > 0L) {
                  i.add <- match(names.add, names.target)
                  names.weights <- c(names.weights, names.add)
                  dimtypes.weights <- c(dimtypes.weights, dimtypes.target[i.add])
                  DimScales.weights <- c(DimScales.weights, DimScales.target[i.add])
                  metadata <- methods::new("MetaData",
                                  nms = names.weights,
                                  dimtypes = dimtypes.weights,
                                  DimScales = DimScales.weights)
                  .Data <- array(.Data, ## replicated
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  weights <- methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              ## make compatible
              ans <- tryCatch(makeCompatible(x = weights,
                                             y = target,
                                             subset = TRUE),
                              error = function(e) e)
              if (methods::is(ans, "error"))
                  stop(gettextf("'%s' and '%s' not compatible: %s",
                                nameTarget, nameWeights, ans$message))
              else
                  ans              
          })

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("collapse",
          signature(object = "Counts", transform = "CollapseTransform"),
          function(object, transform) {
              metadata <- collapse(metadata(object), transform = transform)
              .Data <- collapse(object@.Data, transform = transform)
              .Data <- array(.Data,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              methods::new("Counts", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
#' @rdname collapseCategories
#' @export
setMethod("collapseCategories",
          signature(object = "Counts",
                    dimension = "ANY",
                    old = "ANY",
                    new = "ANY",
                    concordance = "missing",
                    weights = "ANY"),
          function(object, dimension = NULL, old, new, weights = NULL) {
              .Data.before <- object@.Data
              dim.before <- dim(object)
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              dimension <- checkAndTidyDimColExtCat(dimension = dimension,
                                                    names = names,
                                                    DimScales = DimScales)
              if (!is.null(weights))
                  warning(gettextf("'%s' argument ignored",
                                   "weights"))
              old <- checkAndTidyOldNew(old, name = "old", lengthOne = FALSE)
              new <- checkAndTidyOldNew(new, name = "new", lengthOne = TRUE)
              dims <- seq_along(dim.before)
              indices <- lapply(dim.before, seq_len)
              for (i in dimension) {
                  dv.before <- dimvalues(DimScales[[i]])
                  i.old <- match(old, dv.before, nomatch = 0L)
                  not.found <- i.old == 0L
                  if (any(not.found)) {
                      first.not.found <- old[not.found][1L]
                      stop(gettextf("cannot collapse categories for dimension \"%s\" : value \"%s\" not found",
                                    names[i], first.not.found))
                  }
                  dv.after.rep <- replace(dv.before,
                                          list = i.old,
                                          values = new)
                  dv.after <- unique(dv.after.rep)
                  DimScale.after <- methods::new("Categories", dimvalues = dv.after)
                  DimScales[[i]] <- DimScale.after
                  if (dimtypes[i] %in% c("sex", "triangle"))
                      dimtypes[i] <- "state"
                  indices[[i]] <- match(dv.after.rep, dv.after)
              }
              metadata.after <- methods::new("MetaData",
                                             nms = names,
                                             dimtypes = dimtypes,
                                             DimScales = DimScales)
              dim.after <- dim(metadata.after)
              dimnames.after <- dimnames(metadata.after)
              transform <- methods::new("CollapseTransform",
                                        dims = dims,
                                        indices = indices,
                                        dimBefore = dim.before,
                                        dimAfter = dim.after)
              .Data.after <- collapse(.Data.before, transform = transform)
              .Data.after <- array(.Data.after,
                                   dim = dim.after,
                                   dimnames = dimnames.after)
              methods::new("Counts", .Data = .Data.after, metadata = metadata.after)
          })

## HAS_TESTS
#' @rdname collapseCategories
#' @export
setMethod("collapseCategories",
          signature(object = "Counts",
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
              if (!is.null(weights))
                  warning(gettextf("'%s' argument ignored",
                                   "weights"))
              for (i in dimension) {
                  dv.old <- dimvalues(DimScales[[i]])
                  dv.new <- tryCatch(classconc::translate(dv.old, concordance = concordance),
                                     error = function(e) e)
                  if (methods::is(dv.new, "error"))
                      stop(gettextf("problem translating dimension \"%s\" : %s",
                                    names[dimension[i]], dv.new$message))
                  DimScales[[i]] <- methods::new("Categories", dimvalues = dv.new)
                  if (dimtypes[i] %in% c("sex", "triangle"))
                      dimtypes[i] <- "state"
              }
              metadata.new <- methods::new("MetaData",
                                           nms = names,
                                           dimtypes = dimtypes,
                                           DimScales = DimScales)
              dimnames(.Data) <- dimnames(metadata.new)
              methods::new("Counts", .Data = .Data, metadata = metadata.new)
          })

## HAS_TESTS
#' @rdname collapseCategories
#' @export
setMethod("collapseCategories",
          signature(object = "Counts",
                    dimension = "ANY",
                    old = "missing",
                    new = "missing",
                    concordance = "ManyToOne",
                    weights = "ANY"),
          function(object, dimension = NULL, concordance, weights = NULL) {
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
              classif.to <- classconc::classificationTo(concordance)
              classif.from <- classconc::classificationFrom(concordance)
              codes.to <- classconc::codes(concordance, classification = classif.to)
              codes.from <- classconc::codes(concordance, classification = classif.from)
              dims <- seq_along(dim.obj)
              indices <- lapply(dim.obj, seq_len)
              for (i in dimension) {
                  dv.obj <- dimvalues(DimScales[[i]])
                  i.from <- match(dv.obj, codes.from, nomatch = 0L)
                  found.in.from <- i.from > 0L
                  if (any(!found.in.from)) {
                      first.obj.not.found <- dv.obj[!found.in.from][1L]
                      stop(gettextf("cannot collapse categories for dimension \"%s\" : value \"%s\" not found in classification '%s'",
                                    names[i], first.obj.not.found, classif.from))
                  }
                  dv.obj.translated <- codes.to[i.from]
                  dv.ans <- unique(dv.obj.translated)
                  ind.ans <- match(dv.obj.translated, dv.ans)
                  DimScales[[i]] <- methods::new("Categories", dimvalues = dv.ans)
                  indices[[i]] <- ind.ans
              }
              metadata.ans <- methods::new("MetaData",
                                  nms = names,
                                  dimtypes = dimtypes,
                                  DimScales = DimScales)
              dim.ans <- dim(metadata.ans)
              dimnames.ans <- dimnames(metadata.ans)
              transform <- methods::new("CollapseTransform",
                               dims = dims,
                               indices = indices,
                               dimBefore = dim.obj,
                               dimAfter = dim.ans)
              .Data.ans <- collapse(.Data.obj, transform = transform)
              .Data.ans <- array(.Data.ans,
                                 dim = dim.ans,
                                 dimnames = dimnames.ans)
              methods::new("Counts", .Data = .Data.ans, metadata = metadata.ans)
          })

## HAS_TESTS
#' @rdname collapseDimension
#' @export
setMethod("collapseDimension",
          signature(object = "Counts",
                    dimension = "ANY",
                    margin = "ANY",
                    weights = "missing"),
          function(object, dimension = NULL, margin = NULL, weights) {
              .Data <- object@.Data
              names <- names(object)
              dim <- dim(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              n.dim <- length(names)
              if (any(dimtypes == "quantile"))
                  stop(gettextf("dimension with dimtype \"%s\"",
                                "quantile"))
              i.iter <- match("iteration", dimtypes, nomatch = 0L)
              has.iter <- i.iter > 0L
              has.dimension <- !is.null(dimension)
              has.margin <- !is.null(margin)
              if (has.dimension) {
                  if (has.margin)
                      stop(gettextf("has '%s' and '%s' arguments", "dimension", "margin"))
                  else {
                      dimension <- tidySubscript(subscript = dimension, nDim = n.dim, names = names)
                      if (any(dimension == i.iter))
                          stop(gettextf("attempt to collapse dimension with dimtype \"%s\" (consider using function '%s' instead)",
                                        "iteration", "collapseIterations"))
                      margin <- invertSubscript(dimension, nDim = n.dim)
                  }
              }
              else {
                  if (has.margin) {
                      margin <- tidySubscript(subscript = margin, nDim = n.dim, names = names)
                      if (has.iter && !any(margin == i.iter))
                          margin <- c(margin, i.iter)
                          dimension <- invertSubscript(subscript = margin, nDim = n.dim)
                  }
                  else
                      stop(gettextf("no '%s' or '%s' arguments", "dimension", "margin"))
              }
              if (identical(margin, integer()))
                  sum(object)
              else {
                  ## make metadata
                  names.margin <- names[margin]
                  dimtypes.margin <- dimtypes[margin]
                  DimScales.margin <- DimScales[margin]
                  names.pairs <- getNamesPairs(names = names.margin)
                  lost.pair <- !(names.pairs %in% names.margin)
                  names.margin[lost.pair] <- removeSuffixes(names = names.margin[lost.pair])
                  dimtypes.margin[lost.pair] <- "state"
                  metadata <- methods::new("MetaData",
                                  nms = names.margin,
                                  dimtypes = dimtypes.margin,
                                  DimScales = DimScales.margin)
                  ## make .Data
                  dims <- match(seq_len(n.dim), margin, nomatch = 0L)
                  indices <- vector(length = n.dim, mode = "list")
                  indices[margin] <- lapply(dim[margin], seq_len)
                  indices[dimension] <- lapply(dim[dimension], function(x) rep(1L, x))
                  dim.after <- dim[margin]
                  transform <- methods::new("CollapseTransform",
                                   dims = dims,
                                   indices = indices,
                                   dimBefore = dim,
                                   dimAfter = dim.after)
                  .Data <- collapse(.Data, transform = transform)
                  .Data <- array(.Data,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  ## return object
                  methods::new("Counts",
                      .Data = .Data,
                      metadata = metadata)
              }
          })

## HAS_TESTS
#' @rdname collapseDimension
#' @export
setMethod("collapseDimension",
          signature(object = "Counts",
                    dimension = "ANY",
                    margin = "ANY",
                    weights = "Counts"),
          function(object, dimension = NULL, margin = NULL, weights) {
              stop(gettextf("weights cannot be used when '%s' has class \"%s\"",
                            "object", class(object)))
          })

## HAS_TESTS
#' @rdname collapseIntervals
#' @export
setMethod("collapseIntervals",
          signature(object = "Counts",
                    dimension = "ANY",
                    breaks = "numeric",
                    width = "missing",
                    old = "missing",
                    weights = "missing"),
          function(object, dimension, breaks = NULL, width = NULL, old = NULL, weights) {
              if (!identical(length(dimension), 1L))
                  stop(gettextf("'%s' does not have length %d",
                                "dimension", 1L))
              names <- names(object)
              dimension <- tidySubscript(subscript = dimension,
                                         nDim = length(names),
                                         names = names)
              DimScale <- DimScales(object, use.names = FALSE)[[dimension]]
              if (!methods::is(DimScale, "Intervals"))
                  stop(gettextf("dimension \"%s\" has dimscale \"%s\"",
                                names[dimension], class(DimScale)))
              breaks.old <- dimvalues(DimScale)
              if (any(is.na(breaks)))
                  stop(gettextf("'%s' has missing values", "breaks"))
              if (!all(diff(breaks) > 0))
                  stop(gettextf("'%s' not increasing", "breaks"))
              invalid.breaks <- setdiff(breaks, breaks.old)
              n.invalid.breaks <- length(invalid.breaks)
              if (n.invalid.breaks > 0L)
                  stop(sprintf(ngettext(n.invalid.breaks,
                                        "no existing break at value %s",
                                        "no existing breaks at values %s"),
                               paste(invalid.breaks, collapse = ", ")))
              if (min(breaks) > min(breaks.old))
                  breaks <- c(min(breaks.old), breaks)
              if (max(breaks) < max(breaks.old))
                  breaks <- c(breaks, max(breaks.old))
              index <- findInterval(x = breaks.old[-length(breaks.old)], vec = breaks)
              dimBefore <- dim(object)
              dims <- seq_along(dimBefore)
              indices <- lapply(dimBefore, seq_len)
              indices[[dimension]] <- index
              dimAfter <- replace(dimBefore,
                                  list = dimension,
                                  values = length(breaks) - 1L)
              transform <- methods::new("CollapseTransform",
                               dims = dims,
                               indices = indices,
                               dimBefore = dimBefore,
                               dimAfter = dimAfter)
              collapse(object, transform = transform)
          })

## HAS_TESTS
#' @rdname collapseIntervals
#' @export
setMethod("collapseIntervals",
          signature(object = "Counts",
                    dimension = "ANY",
                    breaks = "numeric",
                    width = "NULL",
                    old = "NULL",
                    weights = "missing"),
          function(object, dimension, breaks = NULL, width = NULL, old = NULL, weights) {
              methods::callGeneric(object = object, dimension = dimension, breaks = breaks)
          })

## HAS_TESTS
#' @rdname collapseIntervals
#' @export
setMethod("collapseIntervals",
          signature(object = "Counts",
                    dimension = "ANY",
                    breaks = "missing",
                    width = "numeric",
                    old = "missing",
                    weights = "missing"),
          function(object, dimension, breaks = NULL, width = NULL, old = NULL, weights) {
              if (!identical(length(dimension), 1L))
                  stop(gettextf("'%s' does not have length %d",
                                "dimension", 1L))
              names <- names(object)
              dimension <- tidySubscript(subscript = dimension,
                                         nDim = length(names),
                                         names = names)
              DimScale <- DimScales(object, use.names = FALSE)[[dimension]]
              if (!methods::is(DimScale, "Intervals"))
                  stop(gettextf("dimension \"%s\" has dimscale \"%s\"",
                                names[dimension], class(DimScale)))
              breaks.old <- dimvalues(DimScale)
              if (!identical(length(width), 1L))
                  stop(gettextf("'%s' does not have length %d", "width", 1L))
              if (width <= 0)
                  stop(gettextf("'%s' is non-positive", "width"))
              finite <- is.finite(breaks.old)
              if (sum(finite) <= 1L)
                  breaks <- breaks.old
              else {
                  range <- range(breaks.old[finite])
                  if (diff(range) %% width != 0)
                      stop(gettextf("'%s' [%s] is not a divisor of difference between lowest and highest finite breaks [%s]",
                                    "width", width, diff(range)))
                  breaks <- seq(from = range[1L], to = range[2L], by = width)
              }
              methods::callGeneric(object = object, dimension = dimension, breaks = breaks)
          })

## HAS_TESTS
#' @rdname collapseIntervals
#' @export
setMethod("collapseIntervals",
          signature(object = "Counts",
                    dimension = "ANY",
                    breaks = "NULL",
                    width = "numeric",
                    old = "NULL",
                    weights = "missing"),
          function(object, dimension, breaks = NULL, width = NULL, old = NULL, weights) {
              methods::callGeneric(object = object, dimension = dimension, width = width)
          })

## HAS_TESTS
#' @rdname collapseIntervals
#' @export
setMethod("collapseIntervals",
          signature(object = "Counts",
                    dimension = "ANY",
                    breaks = "missing",
                    width = "missing",
                    old = "character",
                    weights = "missing"),
          function(object, dimension, breaks = NULL, width = NULL, old = NULL, weights) {
              if (!identical(length(dimension), 1L))
                  stop(gettextf("'%s' does not have length %d",
                                "dimension", 1L))
              names <- names(object)
              dimension <- tidySubscript(subscript = dimension,
                                         nDim = length(names),
                                         names = names)
              DimScale <- DimScales(object, use.names = FALSE)[[dimension]]
              if (!methods::is(DimScale, "Intervals"))
                  stop(gettextf("dimension \"%s\" has dimscale \"%s\"",
                                names[dimension], class(DimScale)))
              if (identical(length(old), 0L))
                  stop(gettextf("'%s' has length %d", "old", 0L))
              labels.old <- labels(DimScale)
              i.old <- match(old, labels.old, nomatch = 0L)
              not.found <- i.old == 0L
              n.not.found <- sum(not.found)
              if (n.not.found > 0L)
                  stop(sprintf(ngettext(n.not.found,
                                        "value in '%s' [%s] not found in dimension \"%s\"",
                                        "values in '%s' [%s] not found in dimension \"%s\""),
                               "old",
                               paste(dQuote(old[not.found]), collapse = ", "),
                               names[dimension]))
              if (!all(diff(i.old) == 1L))
                  stop(gettextf("elements of '%s' are not consecutive", "old"))
              index <- seq_along(labels.old)
              index[i.old] <- min(i.old)
              index[index > max(i.old)] <- index[index > max(i.old)] - length(i.old) + 1L
              dimBefore <- dim(object)
              dims <- seq_along(dimBefore)
              indices <- lapply(dimBefore, seq_len)
              indices[[dimension]] <- index
              dimAfter <- replace(dimBefore, list = dimension, values = max(index))
              transform <- methods::new("CollapseTransform",
                               dims = dims,
                               indices = indices,
                               dimBefore = dimBefore,
                               dimAfter = dimAfter)
              collapse(object, transform = transform)
          })

## HAS_TESTS
#' @rdname collapseIntervals
#' @export
setMethod("collapseIntervals",
          signature(object = "Counts",
                    dimension = "ANY",
                    breaks = "NULL",
                    width = "NULL",
                    old = "character",
                    weights = "missing"),
          function(object, dimension, breaks = NULL, width = NULL, old = NULL, weights) {
              methods::callGeneric(object = object, dimension = dimension, old = old)
          })

## HAS_TESTS
#' @rdname collapseIntervals
#' @export
setMethod("collapseIntervals",
          signature(object = "Counts",
                    dimension = "ANY",
                    breaks = "missing",
                    width = "missing",
                    old = "numeric",
                    weights = "missing"),
          function(object, dimension, breaks = NULL, width = NULL, old = NULL, weights) {
              old <- as.character(old)
              methods::callGeneric()
          })

## HAS_TESTS
#' @rdname collapseIntervals
#' @export
setMethod("collapseIntervals",
          signature(object = "Counts",
                    dimension = "ANY",
                    breaks = "NULL",
                    width = "NULL",
                    old = "numeric",
                    weights = "missing"),
          function(object, dimension, breaks = NULL, width = NULL, old = NULL, weights) {
              methods::callGeneric(object = object, dimension = dimension, old = old)
          })

## HAS_TESTS
#' @rdname collapseIntervals
#' @export
setMethod("collapseIntervals",
          signature(object = "Counts",
                    dimension = "ANY",
                    breaks = "ANY",
                    width = "ANY",
                    old = "ANY",
                    weights = "Counts"),
          function(object, dimension, breaks = NULL, width = NULL, old = NULL, weights) {
              stop(gettextf("weights cannot be used when '%s' has class \"%s\"",
                            "object", class(object)))
          })

## HAS_TESTS
#' @rdname collapseOrigDest
#' @export
setMethod("collapseOrigDest",
          signature(object = "Counts", weights = "missing"),
          function(object, base = NULL, to = c("net", "pool", "in", "out"),
                   omitted = ifelse(methods::is(object, "Counts"), 0L, NA_integer_)) {
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              if (is.null(base)) {
                  is.orig <- dimtypes == "origin"
                  if (!any(is.orig))
                      stop(gettextf("no dimensions with dimtypes \"%s\" or \"%s\"",
                                    "origin", "destination"))
                  base <- removeSuffixes(names[is.orig])
              }
              i.orig <- match(sprintf("%s_orig", base), names, nomatch = 0L)
              if (any(i.orig == 0L))
                  stop(gettextf("'%s' outside valid range", "base"))
              i.dest <- match(sprintf("%s_dest", base), names)
              object <- alignPair(object, base = base, omitted = omitted)
              for (j in seq_along(i.orig)) {
                  is.stayer <- (slice.index(object, MARGIN = i.orig[j]) ==
                                    slice.index(object, MARGIN = i.dest[j]))
                  object[is.stayer] <- 0L
              }
              to <- tolower(to)
              to <- match.arg(to)
              has.out <- to %in% c("net", "pool", "out")
              has.in <- to %in% c("net", "pool", "in")
              if (has.out)
                  Out <- collapseDimension(object, dimension = i.dest)
              if (has.in)
                  In <- collapseDimension(object, dimension = i.orig)
              if (to == "net") {
                  ans <- In - Out
                  dimtypes.ans <- dimtypes(ans, use.names = FALSE)
                  if (("origin" %in% dimtypes.ans) || ("parent" %in% dimtypes.ans))
                      ans
                  else {
                      i.between <- match(base, names(ans))
                      methods::new("Net",
                          .Data = ans@.Data,
                          metadata = ans@metadata,
                          iBetween = i.between)
                  }
              }
              else if (to == "pool") {
                  ans <- dbind(Out, In, along = "direction")
                  dimtypes.ans <- dimtypes(ans, use.names = FALSE)
                  if (("origin" %in% dimtypes.ans) || ("parent" %in% dimtypes.ans))
                      ans
                  else {
                      i.direction <- length(dim(ans))
                      i.between <- match(base, names(ans))
                      methods::new("Pool",
                          .Data = ans@.Data,
                          metadata = ans@metadata,
                          iDirection = i.direction,
                          iBetween = i.between)
                  }
              }
              else if (to == "in") {
                  In
              }
              else if (to == "out") {
                  Out
              }
              else {
                  stop(gettextf("invalid value for '%s' : \"%s\"",
                                "to", to))
              }
          })

## HAS_TESTS
setMethod("dbind2",
          signature(e1 = "Counts", e2 = "Counts"),
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
              pair <- makePairTransformsDbind(e1 = e1, e2 = e2, along = along)
              transform1 <- pair[[1L]]
              transform2 <- pair[[2L]]
              e1 <- collapse(e1, transform = transform1)
              e2 <- collapse(e2, transform = transform2)
              metadata <- combineDbindMetadataCounts(e1 = e1,
                                                     e2 = e2,
                                                     along = along)
              .Data <- combineDbindData(e1 = e1, e2 = e2, metadata = metadata)
              methods::new("Counts", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
setMethod("dbind2",
          signature(e1 = "Counts", e2 = "Values"),
          function(e1, e2) {
              stop(gettextf("cannot combine object of class \"%s\" with object of class \"%s\"",
                            class(e1), class(e2)))
          })

## HAS_TESTS
#' @rdname dplot
#' @export
setMethod("dplot",
          signature(formula = "formula", data = "Counts"),
          function(formula, data, type = NULL, panel = panel.dplot,
                   groups, midpoints = FALSE, subarray,
                   probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                   horizontal = FALSE,
                   overlay = NULL, ...) {
              ## extract info about call
              original.call <- match.call(call = sys.call(sys.parent()))
              group.vars <- all.vars(original.call$groups)
              has.response <- identical(length(formula), 3L)
              if (has.response)
                  response.is.propn <- deparse(formula[[2L]]) %in% c("proportion", "percent")
              else {
                  if (!has.response)
                      formula <- stats::as.formula(paste("count", deparse(formula))) ## update doesn't work with |
                  response.is.propn <- FALSE
              }
              response.name <- if (response.is.propn) deparse(formula[[2L]]) else "count"
              ## apply subarray argument if present
              if (methods::hasArg(subarray)) {
                  subarray <- deparse(original.call$subarray)
                  text <- sprintf("subarray(data, %s, drop = FALSE)", subarray)
                  expr <- parse(text = text)
                  data <- eval(expr)
              }
              ## collapse unused dimensions - apart from any "iteration" dimension
              margin <- c(all.vars(formula)[-1L], group.vars)
              collapse.iter <- FALSE
              i.iter <- match("iteration", dimtypes(data), nomatch = 0L)
              has.iter <- i.iter > 0L
              if (has.iter) {
                  name.iter <- names(data)[i.iter]
                  collapse.iter <- !(name.iter %in% margin)
                  if (collapse.iter)
                      margin <- c(margin, name.iter)
              }
              data <- collapseDimension(data, margin = margin)
              ## deal with cases where response is "proportion" or "percent"
              if (response.is.propn) {
                  if (!methods::hasArg(groups))
                      stop(gettextf("response is \'%s\' but \'%s\' is missing",
                                    response.name, "groups"))
                  ## use numbers to refer to dimensions, to allow for possibility
                  ## that names of paired dimensions have changed
                  margin.prop <- which(!(margin %in% group.vars))
                  data <- prop.table(data, margin = margin.prop)
                  if (identical(response.name, "percent"))
                      data <- 100 * data
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
              ## fix up any cases where name has lost suffix
              nms <- names(data)[-length(data)]
              not.in.margin <- !(nms %in% margin)
              if (any(not.in.margin)) {
                  without.suffixes <- removeSuffixes(margin)
                  nms[not.in.margin] <- margin[match(nms[not.in.margin], without.suffixes)]
                  names(data)[-length(data)] <- nms
              }
              ## fix up response
              if (response.is.propn)
                  names(data)[length(data)] <- response.name
              ## overlay
              if (!is.null(overlay)) {
                  if (!is.list(overlay))
                      stop(gettextf("'%s' has class \"%s\"",
                                    "overlay", class(overlay)))
                  data <- addOverlayToData(data = data,
                                           overlay = overlay,
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

## NO_TESTS - has some, needs more
#' @rdname expandCategories
#' @export
setMethod("expandCategories",
          signature(object = "Counts",
                    dimension = "ANY",
                    old = "missing",
                    new = "missing",
                    concordance = "ManyToOne",
                    weights = "ANY"),
          function(object, dimension, concordance, weights = NULL,
                   means = FALSE, n = NULL) {
              object <- checkAndTidyObjExpCatCounts(object = object,
                                                    weights = weights,
                                                    n = n)
              .Data.obj <- object@.Data
              .Data.obj <- as.integer(.Data.obj)
              dim.obj <- dim(object)
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              dimension <- checkAndTidyDimColExtCat(dimension = dimension,
                                                    names = names,
                                                    DimScales = DimScales)
              checkMeans(means)
              classif.to <- classconc::classificationTo(concordance)
              classif.from <- classconc::classificationFrom(concordance)
              codes.to <- classconc::codes(concordance, classification = classif.to)
              codes.from <- classconc::codes(concordance, classification = classif.from)
              dims <- seq_along(dim.obj)
              indices <- lapply(dim.obj, seq_len)
              for (i in dimension) {
                  dv.obj <- dimvalues(DimScales[[i]])
                  i.to <- match(dv.obj, codes.to, nomatch = 0L)
                  found.in.to <- i.to > 0L
                  if (any(!found.in.to)) {
                      first.obj.not.found <- dv.obj[!found.in.to][1L]
                      stop(gettextf("cannot expand category for dimension \"%s\" : value \"%s\" not found in classification '%s'",
                                    names[i], first.obj.not.found, classif.to))
                  }
                  i.obj <- match(codes.to, dv.obj, nomatch = 0L)
                  found.in.obj <- i.obj > 0L
                  dv.ans <- codes.from[found.in.obj]
                  DimScales[[i]] <- methods::new("Categories", dimvalues = dv.ans)
                  indices[[i]] <- i.obj[found.in.obj]
              }
              metadata.ans <- methods::new("MetaData",
                                           nms = names,
                                           dimtypes = dimtypes,
                                           DimScales = DimScales)
              dim.ans <- dim(metadata.ans)
              dimnames.ans <- dimnames(metadata.ans)
              .Data.target <- array(1L, dim = dim.ans, dimnames = dimnames.ans)
              target <- methods::new("Counts", .Data = .Data.target, metadata = metadata.ans)
              weights <- checkAndTidyWeights(weights = weights,
                                             target = target,
                                             allowNA = FALSE)
              weights <- as.double(weights)
              transform <- methods::new("CollapseTransform",
                                        indices = indices,
                                        dims = dims,
                                        dimBefore = dim.ans,
                                        dimAfter = dim.obj)
              transform <- makeCollapseTransformExtra(transform)
              if (means)
                  .Data.ans <- redistributeInnerMeans(counts = .Data.obj,
                                                      weights = weights,
                                                      transform = transform,
                                                      useC = TRUE)
              else
                  .Data.ans <- redistributeInnerDistn(counts = .Data.obj,
                                                      weights = weights,
                                                      transform = transform,
                                                      useC = TRUE)
              .Data.ans <- array(.Data.ans, dim = dim.ans, dimnames = dimnames.ans)
              methods::new("Counts", .Data = .Data.ans, metadata = metadata.ans)
          })

#' @rdname exposure
#' @export
setMethod("exposure",
          signature(object = "Counts"),
          function(object, triangles = FALSE) {
              .Data <- object@.Data
              names <- names(object)
              dim <- dim(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              i.time <- match("time", dimtypes, nomatch = 0L)
              i.age <- match("age", dimtypes, nomatch = 0L)
              i.cohort <- match("cohort", dimtypes, nomatch = 0L)
              has.time <- i.time > 0L
              has.age <- i.age > 0L
              has.cohort <- i.cohort > 0L
              if (has.time) {
                  DimScale.time <- DimScales[[i.time]]
                  time.is.points <- methods::is(DimScale.time, "Points")
              }
              if (has.age) {
                  DimScale.age <- DimScales[[i.age]]
                  age.is.points <- methods::is(DimScale.age, "Points")
              }
              ## check dimtypes and dimscales
              if (has.time && has.age) {
                  if (!(time.is.points && !age.is.points))
                      stop(gettextf("dimension with dimtype \"%s\" has dimscale \"%s\" and dimension with dimtype \"%s\" has dimscale \"%s\"",
                                    "time", class(DimScale.time), "age", class(DimScale.age)))
              }
              else if (has.time && !has.age) {
                  if (!time.is.points)
                      stop(gettextf("dimension with dimtype \"%s\" has dimscale \"%s\"",
                                    "time", class(DimScale.time)))
              }
              else if (!has.time && has.age) {
                  if (!age.is.points)
                      stop(gettextf("dimension with dimtype \"%s\" has dimscale \"%s\"",
                                    "age", class(DimScale.age)))
              }
              else {
                  stop(gettextf("no dimensions with dimtype \"%s\" or \"%s\"",
                                "time", "age"))
              }
              ## check dimension lengths
              if (has.time) {
                  n.time <- dim[i.time]
                  if (n.time < 2L)
                      stop(gettextf("dimension with dimtype \"%s\" has length %d",
                                    "time", n.time))
              }
              else {
                  n.age <- dim[i.age]
                  if (n.age < 2L)
                      stop(gettextf("dimension with dimtype \"%s\" has length %d",
                                    "age", n.age))
              }
              ## triangles
              if (!identical(length(triangles), 1L))
                  stop(gettextf("'%s' has length %d",
                                "triangles", length(triangles)))
              if (!is.logical(triangles))
                  stop(gettextf("'%s' does not have type \"%s\"",
                                "triangles", "logical"))
              if (is.na(triangles))
                  stop(gettextf("'%s' is missing",
                                "triangles"))
              if (triangles && !(has.time && has.age)) {
                  stop(gettextf("'%s' is %s but '%s' does not have dimensions with dimtypes \"%s\" and \"%s\"",
                                "triangles", "TRUE", "object", "time", "age"))
              }
              value.is.regular  <- tryCatch(hasRegularAgeTime(object), error = function(e) e)
              is.regular <- isTRUE(value.is.regular)
              if (triangles) {
                  if (!is.regular)
                      stop(gettextf("'%s' is %s but age-time plan is not regular : %s",
                                    "triangles", "TRUE", value.is.regular$message))
              }
              ## do calculations
              if (triangles)
                  exposureWithTriangles(object)
              else 
                  exposureNoTriangles(object)
          })
        





## exposureBirths <- function(object, triangles = FALSE, minAge, maxAge,
##                            sex = c("sex", "gender"),
##                            dominant = c("female", "females")) {
                                                                                                    



## exposureBirths <- function(object, triangles = FALSE, periodCohort = FALSE, minAgeRepr = NULL, maxAgeRepr = NULL,
##                      dominant = NULL) {
##     i.time <- match("time", dimtypes(object), nomatch = 0L)
##     has.time <- i.time > 0L
##     if (!has.time)
##         stop(gettextf("no dimension with dimtype \"%s\"",
##                       "time"))
##     DimScale.time <- DimScales(object)[[i.time]]
##     if (!methods::is(DimScale.time, "Points"))
##         stop(gettextf("dimension with dimtype \"%s\" has dimscale \"%s\"",
##                       "time", class(DimScale.time)))
##     n.time.points <- length(DimScale.time)
##     if (n.time.points < 2L)
##         stop(gettextf("only %d time points", n.time.points))
##     has.age <- "age" %in% dimtypes
##     has.triangles <- "triangles" %in% dimtypes(object)
##     .Data <- object@.Data
##     index.time <- slice.index(.Data, MARGIN = i.time)
##     n.time <- length(DimScale.time)
##     if (!periodCohort) {
##         time.step <- stepLengths(DimScale.time)
##         each <- prod(c(1L, dim.before[seq_len(n.time)]))
##         multiplier <- 0.5 * rep(time.step, each = each)
##         .Data.new <-  multiplier * (.Data[index.time != n.time] + .Data[index.time != 1L])
##     }
##     else {
##         is.regular <- tryCatch(hasRegularAgeTime(object),
##                                error = function(e) e)
##         if (!isTRUE(is.regular))
##             stop(gettext("cannot calculate exposure for period-cohorts if age-time plan is not regular"))
##         time.step <- ageTimeStep(object)
##         multiplier <- 0.5 * time.step
##         .Data.new <- array(dim = replace(dim(object), list = i.time, value = n.time - 1L))
##     }
##     DimScale.time.new <- intervalsBetweenPoints(DimScale.time)
## }

## HAS_TESTS
#' @rdname growth
#' @export
setMethod("growth",
          signature(object = "Counts"),
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
              if (methods::hasArg(weights))
                  warning(gettextf("'%s' ignored when '%s' has class \"%s\"",
                                   "weights", "object", class(object)))
              values <- collapseDimension(object, margin = c(within, along))
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
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
          })

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("makeCompatible",
          signature(x = "Counts", y = "DemographicArray"),
          function(x, y, subset = FALSE, check = TRUE) {
              if (check)
                  canMakeCompatible(x = x, y = y, subset = subset,
                                    allowCopyIterDim = TRUE)
              x <- copyIterDim(x = x, y = y)
              transform <- makeTransform(x = x, y = y, subset = subset, check = FALSE)
              collapse(object = x, transform = transform)
          })

## HAS_TESTS
## makes 'x' weakly compatible with 'y' - keeps any
## orig-dest or parent-child dimensions in 'x'
setMethod("makeOrigDestParentChildCompatible",
          signature(x = "Counts", y = "DemographicArray"),
          function(x, y, subset = FALSE, check = TRUE) {
              x <- alignPair(x)
              if (check)
                  canMakeOrigDestParentChildCompatible(x = x,
                                                       y = y,
                                                       subset = subset,
                                                       allowCopyIterDim = TRUE)
              x <- copyIterDim(x = x, y = y)
              transform <- makeOrigDestParentChildTransform(x = x,
                                                            y = y,
                                                            subset = subset,
                                                            check = FALSE)
              collapse(object = x, transform = transform)
          })

## HAS_TESTS
## Transform that makes 'x' weakly compatible with 'y' while
## keeping orig-dest or parent-child format.  Puts
## dimensions of 'x' in same order as 'y', except that
## orig-dest or parent-child pairs take place of
## corresponding state dimensions
setMethod("makeOrigDestParentChildTransform",
          signature(x = "Counts", y = "DemographicArray"),
          function(x, y, subset = FALSE, check = TRUE) {
              if (check)
                  canMakeOrigDestParentChildCompatible(x = x,
                                                       y = y,
                                                       subset = subset,
                                                       allowCopyIterDim = FALSE)
              names.x <- names(x)
              names.y <- names(y)
              dim.x <- dim(x)
              dim.y <- dim(y)
              dimtypes.x <- dimtypes(x, use.names = FALSE)
              dimtypes.y <- dimtypes(y, use.names = FALSE)
              DimScales.x <- DimScales(x, use.names = FALSE)
              DimScales.y <- DimScales(y, use.names = FALSE)
              dim.before <- dim(x)
              base.orig <- removeSuffixes(names.x[dimtypes.x == "origin"])
              base.parent <- removeSuffixes(names.x[dimtypes.x == "parent"])
              names.after <- vector(mode = "list", length = length(names.y))
              dim.after <- vector(mode = "list", length = length(names.y))
              suffixes.orig.dest <- getSuffixes(c("origin", "destination"))
              suffixes.parent.child <- getSuffixes(c("parent", "child"))
              for (i in seq_along(names.after)) {
                  name.y <- names.y[i]
                  d.y <- dim.y[i]
                  if (name.y %in% base.orig) {
                      names.after[[i]] <- paste0(name.y, suffixes.orig.dest)
                      dim.after[[i]] <- rep(d.y, 2L)
                  }
                  else if (name.y %in% base.parent) {
                      names.after[[i]] <- paste0(name.y, suffixes.parent.child)
                      dim.after[[i]] <- rep(d.y, 2L)
                  }
                  else {
                      names.after[[i]] <- name.y
                      dim.after[[i]] <- d.y
                  }
              }
              names.after <- unlist(names.after)
              dim.after <- unlist(dim.after)
              dims <- match(names.x, names.after, nomatch = 0L)
              indices <- vector(mode = "list", length = length(names.x))
              for (i in seq_along(names.x)) {
                  drop <- identical(dims[[i]], 0L)
                  if (drop)
                      indices[[i]] <- rep(1L, times = dim.before[i])
                  else {
                      name.x <- names.x[i]
                      name.x <- removeSuffixes(name.x)
                      i.y <- match(name.x, names.y)
                      DimScale.x <- DimScales.x[[i]]
                      DimScale.y <- DimScales.y[[i.y]]
                      indices[[i]] <- makeIndices(x = DimScale.x,
                                                  y = DimScale.y,
                                                  collapse = TRUE,
                                                  concordance = NULL)
                  }
              }
              methods::new("CollapseTransform",
                  dims = dims,
                  indices = indices,
                  dimBefore = dim.before,
                  dimAfter = dim.after)
          })

## HAS_TESTS
setMethod("makePairCompatible",
          signature(e1 = "Counts", e2 = "Counts"),
          function(e1, e2, check = TRUE) {
              if (check)
                  canMakePairCompatible(e1 = e1, e2 = e2,
                                        allowCopyIterDim = TRUE)
              e1 <- copyZeroDim(x = e1, y = e2)
              e2 <- copyZeroDim(x = e2, y = e1)
              e1 <- copyIterDim(x = e1, y = e2)
              e2 <- copyIterDim(x = e2, y = e1)
              pair <- makePairTransforms(e1 = e1, e2 = e2, check = FALSE)
              messageAboutPairSubsetting(pair)
              ans1 <- collapse(e1, transform = pair[[1L]])
              ans2 <- collapse(e2, transform = pair[[2L]])
              list(ans1, ans2)
          })

## HAS_TESTS
setMethod("makePairCompatible",
          signature(e1 = "Counts", e2 = "Values"),
          function(e1, e2, check = TRUE) {
              if (check)
                  canMakePairCompatible(e1 = e1, e2 = e2,
                                        allowCopyIterDim = TRUE)
              e1 <- copyZeroDim(x = e1, y = e2)
              e1 <- copyIterDim(x = e1, y = e2)
              pair <- makePairTransforms(e1 = e1, e2 = e2, check = FALSE)
              messageAboutPairSubsetting(pair)
              ans1 <- collapse(e1, transform = pair[[1L]])
              metadata <- metadata(ans1)
              .Data2 <- extend(e2, transform = pair[[2L]])
              .Data2 <- array(.Data2,
                              dim = dim(metadata),
                              dimnames = dimnames(metadata))
              ans2 <- methods::new("Values", .Data = .Data2, metadata = metadata)
              list(ans1, ans2)
          })

## HAS_TESTS
setMethod("makePairTransforms",
          signature(e1 = "Counts", e2 = "Counts"),
          function(e1, e2, check = TRUE) {
              if (check)
                  canMakePairCompatible(e1 = e1, e2 = e2,
                                        allowCopyIterDim = FALSE)
              DimScales1 <- DimScales(e1, use.names = TRUE)
              DimScales2 <- DimScales(e2, use.names = TRUE)
              dimBefore1 <- dim(e1)
              dimBefore2 <- dim(e2)
              names.after <- intersect(names(e1), names(e2))
              dims1 <- match(names(e1), names.after, nomatch = 0L)
              dims2 <- match(names(e2), names.after, nomatch = 0L)
              indices1 <- vector(mode = "list", length = length(dims1))
              indices2 <- vector(mode = "list", length = length(dims2))
              for (name in names.after) {
                  pair <- makePairIndices(e1 = DimScales1[[name]],
                                          e2 = DimScales2[[name]],
                                          isCounts1 = TRUE,
                                          isCounts2 = TRUE)
                  indices1[[match(name, names(e1))]] <- pair[[1L]]
                  indices2[[match(name, names(e2))]] <- pair[[2L]]
              }
              for (i in seq_along(dims1))
                  if (dims1[i] == 0L)
                      indices1[[i]] <- rep(1L, times = dimBefore1[i])
              for (i in seq_along(dims2))
                  if (dims2[i] == 0L)
                      indices2[[i]] <- rep(1L, times = dimBefore2[i])
              maxOrZero <- function(x) if (length(x) > 0L) max(x) else 0L
              dimAfter <- sapply(indices1[dims1 != 0L], maxOrZero)
              list(methods::new("CollapseTransform",
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
          signature(e1 = "Counts", e2 = "Values"),
          function(e1, e2, check = TRUE) {
              if (check)
                  canMakePairCompatible(e1 = e1, e2 = e2)
              DimScales1 <- DimScales(e1, use.names = FALSE)
              DimScales2 <- DimScales(e2, use.names = FALSE)
              dimBefore1 <- dim(e1)
              dimBefore2 <- dim(e2)
              names.after <- names(e1)
              dims1 <- seq_along(names.after)
              dims2 <- match(names.after, names(e2), nomatch = 0L)
              indices1 <- vector(mode = "list", length = length(names.after))
              indices2 <- vector(mode = "list", length = length(names.after))
              for (i in seq_along(names.after)) {
                  d1 <- match(i, dims1)
                  d2 <- dims2[i]
                  if (d2 > 0L) {
                      pair <- makePairIndices(e1 = DimScales1[[d1]],
                                              e2 = DimScales2[[d2]],
                                              isCounts1 = TRUE,
                                              isCounts2 = FALSE)
                      indices1[[d1]] <- pair[[1L]]
                      indices2[[i]] <- pair[[2L]]
                  }
                  else {
                      length.dim <- dimBefore1[d1]
                      indices1[[d1]] <- seq_len(length.dim)
                      indices2[[i]] <- rep(1L, times = length.dim)
                  }
              }
              dimAfter <- sapply(indices2, length)
              list(methods::new("CollapseTransform",
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
          signature(e1 = "Counts", e2 = "Counts"),
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
              n.before1 <- length(names1)
              n.before2 <- length(names2)
              dimBefore1 <- dim(e1)
              dimBefore2 <- dim(e2)
              DimScales1 <- DimScales(e1, use.names = FALSE)
              DimScales2 <- DimScales(e2, use.names = FALSE)
              names.after <- intersect(names1, names2)
              names.after <- c(setdiff(names.after, along), along)
              n.after <- length(names.after)
              dims1 <- integer(length = n.before1)
              dims2 <- integer(length = n.before2)
              indices1 <- vector(mode = "list", length = n.before1)
              indices2 <- vector(mode = "list", length = n.before2)
              for (i in seq_len(n.after)) {
                  name <- names.after[i]
                  i1 <- match(name, names1)
                  i2 <- match(name, names2)
                  dims1[i1] <- i
                  dims2[i2] <- i
                  if (identical(name, along)) {
                      indices1[[i1]] <- seq_len(dimBefore1[i1])
                      indices2[[i2]] <- seq_len(dimBefore2[i2])
                  }
                  else {
                      pair <- makePairIndices(e1 = DimScales1[[i1]],
                                              e2 = DimScales2[[i2]],
                                              isCounts1 = TRUE,
                                              isCounts2 = TRUE)
                      indices1[[i1]] <- pair[[1L]]
                      indices2[[i2]] <- pair[[2L]]
                  }
              }
              for (i in seq_along(dims1))
                  if (dims1[i] == 0L)
                      indices1[[i]] <- rep(1L, times = dimBefore1[i])
              for (i in seq_along(dims2))
                  if (dims2[i] == 0L)
                      indices2[[i]] <- rep(1L, times = dimBefore2[i])
              maxOrZero <- function(x) if (length(x) > 0L) max(x) else 0L
              dimAfter1 <- sapply(indices1, maxOrZero)
              dimAfter1 <- dimAfter1[match(names.after, names1)]
              length.along2 <- dimBefore2[match(along, names2)]
              dimAfter2 <- replace(dimAfter1, list = n.after, values = length.along2)
              list(methods::new("CollapseTransform",
                       dims = dims1,
                       indices = indices1,
                       dimBefore = dimBefore1,
                       dimAfter = dimAfter1),
                   methods::new("CollapseTransform",
                       dims = dims2,
                       indices = indices2,
                       dimBefore = dimBefore2,
                       dimAfter = dimAfter2))
          })

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("makeTransform",
          signature(x = "Counts", y = "DemographicArray"),
          function(x, y, subset = FALSE, concordances = list(), check = TRUE) {
              concordances <- tidyConcordanceList(concordances = concordances,
                                                  object = x)
              if (check)
                  canMakeCompatible(x = x,
                                    y = y,
                                    subset = subset,
                                    concordances = concordances,
                                    allowCopyIterDim = FALSE)
              names.x <- names(x)
              names.y <- names(y)
              DimScales.x <- DimScales(x)
              DimScales.y <- DimScales(y)
              dimBefore <- dim(x)
              dimAfter <- dim(y)
              dims <- match(names.x, names.y, nomatch = 0L)
              indices <- vector(mode = "list", length = length(dims))
              for (i in seq_along(indices)) {
                  drop <- identical(dims[i], 0L)
                  if (drop)
                      indices[[i]] <- rep(1L, times = dimBefore[i])
                  else {
                      DimScale.x <- DimScales.x[[i]]
                      DimScale.y <- DimScales.y[[dims[i]]]
                      concordance <- concordances[[i]]
                      indices[[i]] <- makeIndices(x = DimScale.x,
                                                  y = DimScale.y,
                                                  collapse = TRUE,
                                                  concordance = concordance)
                  }
              }
              methods::new("CollapseTransform",
                           dims = dims,
                           indices = indices,
                           dimBefore = dimBefore,
                           dimAfter = dimAfter)
          })

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("makeTransform",
          signature(x = "Counts", y = "numeric"),
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
#' @method plot Counts
#' @export
plot.Counts <- function(x, main = NULL, cex.main = 1.2, col.main = "black",
                        font.main = 2, las = 1, ...) {
    n <- length(names(x))
    nrow <- ceiling(sqrt(n))
    ncol <- ceiling(n / nrow)
    mfrow <- c(nrow, ncol)
    oma <- if (is.null(main)) rep(0, 4) else c(0, 0, 3, 0)
    mar <- c(3, 6, 2, 1)
    old.par <- graphics::par(mfrow = mfrow, oma = oma, mar = mar)
    on.exit(graphics::par(old.par))
    .Data <- x@.Data
    .Data <- array(as.numeric(.Data), dim = dim(.Data))
    marginTotals <- function(i) apply(.Data, i, sum, na.rm = TRUE)
    margin.totals.all <- lapply(seq_len(n), marginTotals)
    for (margin in seq_len(n))
        plotSingleDimensionCounts(margin.totals = margin.totals.all[[margin]],
                                  labels = dimnames(x)[[margin]],
                                  main = names(x)[margin],
                                  las = las,
                                  ...)
    if (!is.null(main))
        graphics::mtext(text = main, outer = TRUE, line = 1, cex = cex.main,
              col = col.main, font = font.main)
}

#' @rdname plot-methods
#' @export
setMethod("plot",
          signature(x = "Counts"),
          plot.Counts)

## HAS_TESTS
plotSingleDimensionCounts <- function(margin.totals, labels, main, las, ...) {
    if (sum(!is.na(margin.totals) > 0L)) {
        graphics::barplot(height = margin.totals,
                horiz = TRUE,
                names.arg = labels,
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

#' @rdname reallocateToEndAges
#' @export
setMethod("reallocateToEndAges",
          signature(object = "Counts",
                    weights = "missing"),
          function(object, min = 15, max = 50, weights, ...) {
              for (name in c("min", "max")) {
                  value <- get(name)
                  if(!is.numeric(value))
                      stop(gettextf("'%s' is non-numeric",
                                    name))
                  if (!identical(length(value), 1L))
                      stop(gettextf("'%s' does not have length %d",
                                    name, 1L))
                  if (is.na(value))
                      stop(gettextf("'%s' is missing",
                                    name))
              }
              if (min >= max)
                  stop(gettextf("'%s' greater than or equal to '%s'",
                                "min", "max"))
              dim <- dim(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              if (any(dim == 0L))
                  stop(gettextf("'%s' has dimension with length %d",
                                "object", 0L))
              i.age <- match("age", dimtypes, nomatch = 0L)
              has.age <- i.age > 0L
              if (!has.age)
                  stop(gettextf("'%s' does not have dimension with dimtype \"%s\"",
                                "object", "age"))
              n.age <- dim[i.age]
              DimScale.age <- DimScales[[i.age]]
              if (!is(DimScale.age, "Intervals"))
                  stop(gettextf("dimension with dimtype \"%s\" does not have %s \"%s\"",
                                "age", "dimscale", "Intervals"))
              dv.age <- dimvalues(DimScale.age)
              min.dv.age <- dv.age[1L]
              max.dv.age <- dv.age[length(dv.age)]
              if (min > min.dv.age) {
                  i.min <- match(min, dv.age, nomatch = 0L)
                  if ((i.min == 0L) || (i.min == length(dv.age)))
                      stop(gettextf("value for '%s' not equal to lower limit for age group in '%s'",
                                    "min", "object"))
                  s.below <- seq_len(i.min - 1L)
                  s.other <- seq.int(from = i.min,
                                     to = n.age)
                  counts.below <- slab(object,
                                       dimension = i.age,
                                       elements = s.below,
                                       drop = FALSE)
                  object <- slab(object,
                                 dimension = i.age,
                                 elements = s.other,
                                 drop = FALSE)
                  counts.below <- collapseDimension(counts.below,
                                                    dimension = i.age)
                  slab(object,
                       dimension = i.age,
                       elements = 1L) <- slab(object,
                                              dimension = i.age,
                                              elements = 1L) + counts.below
                  dim <- dim(object)
                  DimScales <- DimScales(object, use.names = FALSE)
                  DimScale.age <- DimScales[[i.age]]
                  n.age <- dim[i.age]
                  dv.age <- dimvalues(DimScale.age)
                  max.dv.age <- dv.age[length(dv.age)]
              }
              if (max < max.dv.age) {
                  i.max <- match(max, dv.age, nomatch = 0L)
                  if ((i.max == 0L) || (i.max == 1L))
                      stop(gettextf("value for '%s' not equal to upper limit for age group in '%s'",
                                    "max", "object"))
                  s.above <- seq.int(from = i.max,
                                     to = n.age)
                  s.other <- seq_len(i.max - 1L)
                  counts.above <- slab(object,
                                       dimension = i.age,
                                       elements = s.above,
                                       drop = FALSE)
                  object <- slab(object,
                                 dimension = i.age,
                                 elements = s.other,
                                 drop = FALSE)
                  counts.above <- collapseDimension(counts.above,
                                                    dimension = i.age)
                  slab(object,
                       dimension = i.age,
                       elements = i.max - 1L) <- slab(object,
                                                      dimension = i.age,
                                                      elements = i.max - 1L) + counts.above
              }
              object
          })

#' @rdname reallocateToEndAges
#' @export
setMethod("reallocateToEndAges",
          signature(object = "Counts",
                    weights = "ANY"),
          function(object, min = 15, max = 50, weights, ...) {
              stop(gettextf("weights cannot be used when '%s' has class \"%s\"",
                            "object", class(object)))
          })

## NO_TESTS - has some, needs more
#' @rdname redistribute
#' @export
setMethod("redistribute",
          signature(counts = "Counts",
                    weights = "DemographicArray"),
          function(counts, weights, means = FALSE, n = NULL) {
              for (name in c("counts", "weights")) {
                  value <- get(name)
                  if ("quantile" %in% dimtypes(value))
                      stop(gettextf("'%s' has dimension with %s \"%s\"",
                                    name, "dimtype", "quantile"))
                  if (identical(length(value), 0L))
                      stop(gettextf("'%s' has length %d",
                                    name, 0L))
                  if (any(is.na(value)))
                      stop(gettextf("'%s' has missing values",
                                    name))
                  if (any(value < 0))
                      stop(gettextf("'%s' has negative values",
                                    name))
              }
              if (any(sum(weights) == 0L))
                  stop(gettextf("'%s' sum to %d",
                                "weights", 0))
              if (!isTRUE(all.equal(as.integer(counts), as.double(counts))))
                  stop(gettextf("'%s' has non-integer values",
                                "counts"))
              checkMeans(means)
              weights <- methods::as(weights, "Counts")
              i.iter.counts <- match("iteration", dimtypes(counts), nomatch = 0L)
              i.iter.weights <- match("iteration", dimtypes(weights), nomatch = 0L)
              has.iter.counts <- i.iter.counts > 0L
              has.iter.weights <- i.iter.weights > 0L
              if (has.iter.counts && !has.iter.weights) {
                  DimScale.iter <- DimScales(counts)[[i.iter.counts]]
                  iterations <- DimScale.iter@dimvalues
                  metadata.weights <- addIterationsToMetadata(metadata(weights),
                                                              iterations = iterations)
                  .Data.weights <- rep(as.double(weights), times = length(iterations))
                  .Data.weights <- array(.Data.weights,
                                         dim = dim(metadata.weights),
                                         dimnames = dimnames(metadata.weights))
                  weights <- methods::new("Counts",
                                          .Data = .Data.weights,
                                          metadata = metadata.weights)
              }
              else if (!has.iter.counts && has.iter.weights) {
                  DimScale.iter <- DimScales(weights)[[i.iter.weights]]
                  iterations <- DimScale.iter@dimvalues
                  metadata.counts <- addIterationsToMetadata(metadata(counts),
                                                             iterations = iterations)
                  .Data.counts <- rep(as.integer(counts), times = length(iterations))
                  .Data.counts <- array(.Data.counts,
                                        dim = dim(metadata.counts),
                                        dimnames = dimnames(metadata.counts))
                  counts <- methods::new("Counts",
                                         .Data = .Data.counts,
                                         metadata = metadata.counts)
              }
              else if (!has.iter.counts && !has.iter.weights) {
                  n <- checkAndTidyN(n)                  
                  if (!is.null(n)) {
                      iterations <- seq_len(n)
                      metadata.counts <- addIterationsToMetadata(metadata(counts),
                                                                 iterations = iterations)
                      metadata.weights <- addIterationsToMetadata(metadata(weights),
                                                                  iterations = iterations)
                      .Data.counts <- rep(as.integer(counts), times = n)
                      .Data.weights <- rep(as.double(weights), times = n)
                      .Data.counts <- array(.Data.counts,
                                            dim = dim(metadata.counts),
                                            dimnames = dimnames(metadata.counts))
                      .Data.weights <- array(.Data.weights,
                                             dim = dim(metadata.weights),
                                             dimnames = dimnames(metadata.weights))
                      counts <- methods::new("Counts",
                                             .Data = .Data.counts,
                                             metadata = metadata.counts)
                      weights <- methods::new("Counts",
                                              .Data = .Data.weights,
                                              metadata = metadata.weights)
                  }
              }
              transform <- tryCatch(makeTransform(x = weights,
                                                  y = counts,
                                                  subset = FALSE),
                                    error = function(e) e)
              if (methods::is(transform, "error"))
                  stop(gettextf("'%s' not compatible with '%s' : %s",
                                "weights", "counts", transform$message))
              transform <- makeCollapseTransformExtra(transform)
              if (means)
                  .Data <- redistributeInnerMeans(counts = as.integer(counts),
                                                  weights = as.double(weights),
                                                  transform = transform,
                                                  useC = TRUE)
              else
                  .Data <- redistributeInnerDistn(counts = as.integer(counts),
                                                  weights = as.double(weights),
                                                  transform = transform,
                                                  useC = TRUE)
              metadata <- metadata(weights)
              .Data <- array(.Data, dim = dim(metadata), dimnames = dimnames(metadata))
              methods::new("Counts", .Data = .Data, metadata = metadata)
          })

## NO_TESTS - has some, needs more for means = TRUE
#' @rdname redistributeCategory
#' @export
setMethod("redistributeCategory",
          signature(counts = "Counts"),
          function(counts, dimension, category, means = FALSE, epsilon = 0, n = NULL) {
              dim <- dim(counts)
              names <- names(counts)
              dimnames <- dimnames(counts)
              dimtypes <- dimtypes(counts, use.names = FALSE)
              DimScales <- DimScales(counts, use.names = FALSE)
              if (!identical(length(dimension), 1L))
                  stop(gettextf("'%s' does not have length %d",
                                "dimension", 1L))
              dimension <- tidySubscript(subscript = dimension,
                                         nDim = length(names),
                                         names = names)
              if (identical(length(category), 0L))
                  stop(gettextf("'%s' has length %d",
                                "category", 0L))
              if (any(is.na(category)))
                  stop(gettextf("'%s' has missing values",
                                "category"))
              epsilon <- checkAndTidyEpsilon(epsilon)
              checkMeans(means)
              has.pair <- identical(length(dimension), 2L)
              if (has.pair) {
                  ## need to temporarily override behaviour of pair dimensions
                  names.pair.tmp <- make.unique(c(names,
                                                  paste(names[dimension], "tmp",
                                                        sep = "_")))
                  names.pair.tmp <- utils::tail(names.pair.tmp, n = 2L)
                  names.tmp <- replace(names,
                                       list = dimension,
                                       values = names.pair.tmp)
                  dimtypes.tmp <- replace(dimtypes,
                                          list = dimension,
                                          values = "state")
                  metadata.tmp <- methods::new("MetaData",
                                      nms = names.tmp,
                                      dimtypes = dimtypes.tmp,
                                      DimScales = DimScales)
                  .Data.tmp <- array(counts@.Data,
                                     dim = dim(metadata.tmp),
                                     dimnames = dimnames(metadata.tmp))
                  counts.tmp <- methods::new("Counts",
                                    .Data = .Data.tmp,
                                    metadata = metadata.tmp)
                  counts.tmp <- Recall(counts.tmp,
                                       dimension = dimension[1L],
                                       category = category,
                                       epsilon = epsilon,
                                       means = means,
                                       n = n)
                  counts.tmp <- Recall(counts.tmp,
                                       dimension = dimension[2L],
                                       category = category,
                                       epsilon = epsilon,
                                       means = means,
                                       n = n)
                  DimScales.ans <- DimScales(counts.tmp, use.names = FALSE)
                  metadata.ans <- methods::new("MetaData",
                                      nms = names,
                                      dimtypes = dimtypes,
                                      DimScales = DimScales.ans)
                  .Data.ans <- array(counts.tmp@.Data,
                                     dim = dim(metadata.ans),
                                     dimnames = dimnames(metadata.ans))
                  methods::new("Counts", .Data = .Data.ans, metadata = metadata.ans)
              }
              else {
                  if (!methods::is(DimScales[[dimension]], "Categories"))
                      stop(gettextf("dimension \"%s\" does not have dimscale \"Categories\"",
                                    names[dimension]))
                  i.category <- match(category, dimnames[[dimension]], nomatch = 0L)
                  i.not.found <- i.category == 0L
                  if (any(i.not.found)) {
                      first.not.found <- which(i.not.found)[1L]
                      stop(gettextf("dimension \"%s\" does not have category \"%s\"",
                                    names[dimension], category[first.not.found]))
                  }
                  if (identical(length(category), dim[dimension]))
                      stop(gettextf("'%s' contains all of dimension \"%s\"",
                                    "category", names[dimension]))
                  counts.new <- slab(counts,
                                      dimension = dimension,
                                      elements = i.category,
                                      drop = FALSE)
                  counts.new <- collapseDimension(counts.new, dimension = dimension)
                  i.non.cat <- setdiff(seq_len(dim[dimension]), i.category)
                  weights <- slab(counts,
                                   dimension = dimension,
                                   elements = i.non.cat,
                                   drop = FALSE)
                  weights + redistribute(counts.new,
                                         weights = weights + epsilon,
                                         means = means,
                                         n = n)
              }
          })


#' @rdname resetDiag
#' @export
setMethod("resetDiag",
          signature(object = "Counts",
                    reset = "ANY"),
          function(object, base = NULL, reset = NULL) {
              resetDiagInner(object = object,
                             base = base,
                             reset = reset)
          })

#' @rdname resetDiag
#' @export
setMethod("resetDiag",
          signature(object = "Counts",
                    reset = "missing"),
          function(object, base = NULL, reset = NULL) {
              reset <- 0L
              resetDiagInner(object = object,
                             base = base,
                             reset = reset)
          })

#' @rdname resetDiag
#' @export
setMethod("resetDiag",
          signature(object = "Counts",
                    reset = "NULL"),
          function(object, base = NULL, reset = NULL) {
              reset <- 0L
              resetDiagInner(object = object,
                             base = base,
                             reset = reset)
          })

