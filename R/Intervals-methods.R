
## HAS_TESTS
setAs(from = "Intervals", to = "Sexes",
      function(from) {
          if (length(from) > 0L)
              stop("labels not valid for dimscale")
          else
              methods::new("Sexes")
      })

## HAS_TESTS
setAs(from = "Intervals", to = "Triangles",
      function(from) {
          if (length(from) > 0L)
              stop("labels not valid for dimscale")
          else
              methods::new("Triangles")
      })

## HAS_TESTS
setAs(from = "Intervals", to = "Points",
      function(from) {
          if (length(from) > 0L) {
              labels <- labels(from)
              labels <- suppressWarnings(as.numeric(labels))
              if (!any(is.na(labels)))
                  methods::new("Points", dimvalues = labels)
              else
                  stop("labels not valid for dimscale")
          }
          else
              methods::new("Points")
      })

## HAS_TESTS
setAs(from = "Intervals", to = "Quantiles",
      function(from) {
          if (length(from) > 0L)
              stop("labels not valid for dimscale")
          else
              methods::new("Quantiles")
      })

## HAS_TESTS
setAs(from = "Intervals", to = "Iterations",
      function(from) {
          labels <- labels(from)
          dimvalues <- suppressWarnings(as.numeric(labels))
          if (!any(is.na(dimvalues)) &&
              all(dimvalues > 0) &&
              all(dimvalues == as.integer(dimvalues)))
              methods::new("Iterations", dimvalues = as.integer(dimvalues))
          else
              stop("labels not valid for dimscale")
      })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("[",
          signature(x = "Intervals", i = "integer"),
          function(x, i) {
              if (any(is.na(i)))
                  stop(gettextf("'%s' has missing values", "i"))
              n <- length(x)
              if (!all(abs(i) <= n))
                  stop(gettextf("'%s' has values outside the valid range", "i"))
              if (n > 0L) {
                  i <- seq_len(n)[i]
                  if (all(diff(i) == 1L)) {
                      dv.before <- dimvalues(x)
                      lower.before <- dv.before[seq_len(n)]
                      lower.after <- lower.before[i]
                      final.after <- dv.before[max(i) + 1L]
                      dv.after <- c(lower.after, final.after)
                      methods::new(class(x), dimvalues = dv.after)
                  }
                  else {
                      dv.before <- labels(x)
                      dv.after <- dv.before[i]
                      methods::new("Categories", dimvalues = dv.after)
                  }
              }
              else
                  x
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("%in%",
          signature(x = "Intervals", table = "ANY"),
          function(x, table) {
              nms <- deparse(substitute(x))
              indices <- methods::callGeneric(x = labels(x), table = as.character(table))
              indices <- list(indices)
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Compare",
          signature(e1 = "Intervals", e2 = "ANY"),
          function(e1, e2) {
              nms <- deparse(substitute(e1))
              dimvalues <- dimvalues(e1)
              n <- length(dimvalues)
              if (n > 0L) {
                  if (.Generic %in% c(">", ">=")) {
                      lower <- dimvalues[-n]
                      indices <- lower >= e2
                  }
                  else if (.Generic %in% c("<", "<=")) {
                      upper <- dimvalues[-1L]
                      indices <- upper <= e2
                  }
                  else
                      stop(gettextf("invalid use of '%s' operator", .Generic))
              }
              else
                  indices <- logical()
              indices <- list(indices)
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Compare",
          signature(e1 = "ANY", e2 = "Intervals"),
          function(e1, e2) {
              nms <- deparse(substitute(e2))
              dimvalues <- dimvalues(e2)
              n <- length(dimvalues)
              if (n > 0L) {
                  if (.Generic %in% c(">", ">=")) {
                      upper <- dimvalues[-1L]
                      indices <- e1 >= upper
                  }
                  else if (.Generic %in% c("<", "<=")) {
                      lower <- dimvalues[-n]
                      indices <- e1 <= lower
                  }
                  else
                      stop(gettextf("invalid use of '%s' operator", .Generic))
              }
              else
                  indices <- logical()
              indices <- list(indices)
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Compare",
          signature(e1 = "Intervals", e2 = "character"),
          function(e1, e2) {
              nms <- deparse(substitute(e1))
              labels <- labels(e1)
              if (.Generic %in% c("==", "!="))
                  indices <- methods::callGeneric(e1 = labels, e2 = e2)
              else
                  stop(gettextf("invalid use of '%s' operator", .Generic))
              indices <- list(indices)
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Compare",
          signature(e1 = "character", e2 = "Intervals"),
          function(e1, e2) {
              nms <- deparse(substitute(e2))
              labels <- labels(e2)
              if (.Generic %in% c("==", "!="))
                  indices <- methods::callGeneric(e1 = e1, e2 = labels)
              else
                  stop(gettextf("invalid use of '%s' operator", .Generic))
              indices <- list(indices)
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Compare",
          signature(e1 = "Intervals", e2 = "Intervals"),
          function(e1, e2) {
              stop(gettext("attempt to compare two dimscales"))
          })

## HAS_TESTS
setMethod("canMakeDimScalePairCompatible",
          signature(e1 = "Intervals", e2 = "Intervals"),
          function(e1, e2, isCounts1, isCounts2) {
              dv1 <- dimvalues(e1)
              dv2 <- dimvalues(e2)
              n1 <- length(dv1)
              n2 <- length(dv2)
              if ((n1 > 0L) && (n2 > 0L)) {
                  dv12 <- intersect(dv1, dv2)
                  n12 <- length(dv12)
                  if (n12 < 2L)
                      stop(gettext("intervals do not align"))
                  if (isCounts1 != isCounts2) {
                      min <- dv12[1L]
                      max <- dv12[n12]
                      dv1.trim <- dv1[(min <= dv1) & (dv1 <= max)]
                      dv2.trim <- dv2[(min <= dv2) & (dv2 <= max)]
                      if (isCounts1) {
                          dv.counts <- dv1.trim
                          dv.values <- dv2.trim
                      }
                      else {
                          dv.counts <- dv2.trim
                          dv.values <- dv1.trim
                      }
                      if (!all(dv.values %in% dv.counts))
                          stop("intervals do not align")
                  }
              }
              TRUE
          })

## HAS_TESTS
setMethod("canMakeDimScalesCompatible",
          signature(x = "Intervals", y = "Intervals"),
          function(x, y, subset = FALSE, collapse) {
              if (subset)
                  limitsGreaterOrEqual(x = x, y = y)
              else
                  limitsEqual(e1 = x, e2 = y)
              if (collapse)
                  internalDetailGreaterOrEqual(x = x, y = y)
              else {
                  e1 <- x; e2 <- y ## allows reversal of arguments
                  internalDetailGreaterOrEqual(x = e2, y = e1)
              }
              TRUE
          })

## HAS_TESTS
## 'index' comes from a "CollapseTransform" object, so it should be in
## the valid range, but it may have gaps or permutations, which
## are acceptable for other types of DimScales.
## Note that length(object) equals length(index), which equals
## length(dimvalues(object)) - 1.
setMethod("collapseDimScale",
          signature(object = "Intervals", index = "integer"),
          function(object, index, concordance = NULL) {
              if (all(index == 0L))
                  return(methods::new("Intervals"))
              i.last.positive <- max(which(index > 0L))
              right.trimmed.index <- index[seq_len(i.last.positive)]
              no.gaps.or.permutations <- all(diff(right.trimmed.index) %in% c(0L, 1L))
              if (no.gaps.or.permutations) {
                  dv.before <- dimvalues(object)
                  i.last.upper <- i.last.positive + 1L
                  index[duplicated(index)] <- 0L
                  index <- match(seq_along(index), index, nomatch = 0L)
                  index <- c(index, i.last.upper)
                  dv.after <- dv.before[index]
                  methods::new(class(object), dimvalues = dv.after)
              }
              else {
                  object <- methods::new("Categories", dimvalues = labels(object))
                  methods::callGeneric()
              }
          })

## HAS_TESTS
setMethod("dbindDimScales",
          signature(e1 = "Intervals", e2 = "Intervals"),
          function(e1, e2, along) {
              dimvalues <- combineDimvaluesForIntervals(e1 = e1, e2 = e2, along = along)
              methods::new("Intervals", dimvalues = dimvalues)
          })

## HAS_TESTS
setMethod("e1IsFirstDimScale",
          signature(e1 = "Intervals", e2 = "Intervals"),
          function(e1, e2) {
              dv1 <- e1@dimvalues
              dv2 <- e2@dimvalues
              n1 <- length(dv1)
              n2 <- length(dv2)
              has.zero.length <- (n1 == 0L) || (n2 == 0L)
              if (has.zero.length)
                  TRUE
              else
                  !isTRUE(all.equal(max(dv2), min(dv1)))
          })

## HAS_TESTS
setMethod("extendDimScale",
          signature(object = "Intervals", index = "integer"),
          function(object, index) {
              if (identical(length(object), 0L))
                  return(object)
              if (!all(diff(index) %in% c(0L, 1L)))
                  stop(gettextf("'%s' has gaps", "index"))
              dimvalues <- dimvalues(object)
              last.lower <- max(index)
              last.upper <- max(index) + 1L
              index <- c(index, last.upper)
              dimvalues <- dimvalues[index]
              methods::new(class(object), dimvalues = dimvalues)
          })

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("incrementDimScale",
          signature(object = "Intervals"),
          function(object, n) {
              n <- checkAndTidyNIncrement(n)
              forward <- n > 0L
              dv <- object@dimvalues
              dvf <- dv[is.finite(dv)]
              n.dv <- length(dv)
              n.dvf <- length(dvf)
              if (n.dv == 0L)
                  stop(gettextf("\"%s\" dimension has length %d",
                                "along", 0L))
              if (n.dvf < 2L)
                  stop(gettextf("\"%s\" dimension has no finite intervals",
                                "along"))
              if (forward) {
                  if (is.infinite(dv[n.dv]))
                      stop(gettextf("last interval of \"%s\" dimension is open",
                                    "along"))
              }
              else {
                  if (is.infinite(dv[1L]))
                      stop(gettextf("first interval of \"%s\" dimension is open",
                                    "along"))
              }
              if (n.dvf == 2L)
                  step <- diff(dvf)
              else {
                  step <- diff(dvf[1:2])
                  if (!all(diff(dvf[-1L]) == step))
                      stop(gettextf("intervals on \"%s\" dimension have varying lengths",
                                    "along"))
              }
              if (forward)
                  dimvalues <- seq(from = dv[n.dv],
                                   by = step,
                                   length.out = n + 1L)
              else
                  dimvalues <- seq(to = dv[1L],
                                   by = step,
                                   length.out = -n + 1L)
              methods::new("Intervals", dimvalues = dimvalues)
          })

## HAS_TESTS
setMethod("inferDimvalues",
          signature(DimScale = "Intervals", labels = "character"),
          function(DimScale, labels) {
              n <- length(labels)
              if (identical(n, 0L))
                  return(numeric())
              month.dimvalues <- tryCatch(monthLabelsToDimvalues(labels),
                                          error = function(e) NULL)
              if (!is.null(month.dimvalues))
                  return(month.dimvalues)
              labels <- orderLabelsNumerically(labels)
              decoded.labels <- rep(FALSE, n)
              dimvalues <- numeric(n + 1L)
              intervalSeparators <- getSynonymsForIntervalSeparator()
              limitPrintLower <- getLimitPrintLower()
              ## remove year|years from labels
              labels <- sub("year|years", "", ignore.case = TRUE, labels)
              ## remove white space from labels
              labels <- gsub(" ", "", labels)
              ## see if the first label can be interpreted as denoting an open
              ## interval, and infer the appropriate dimvalues if it can
              for (which in c("firstLeft", "firstRight")) {
                  dimvalue <- extractNumberFromOpenInterval(labels[1L], which = which)
                  if (!is.null(dimvalue)) {
                      decoded.labels[1L] <- TRUE
                      dimvalues[1:2] <- c(-Inf, dimvalue)
                      break
                  }
              }
              ## see if the final label can be interpreted as denoting an
              ## open interval, and infer the appropriate dimvalues if it can
              first <- extractNumberFromOpenInterval(labels[n], which = "final")
              if (!is.null(first)) {
                  print.lower <- first < limitPrintLower
                  if ((first == as.integer(first)) && !print.lower)
                      first <- first - 1L
                  dimvalues[n] <- first
                  dimvalues[n + 1L] <- Inf
                  decoded.labels[n] <- TRUE
              }
              if (!decoded.labels[n]) {
                  ## If the last label has not been decoded, see if it can be
                  ## interpreted as (x, x + a). The decoding cannot be done in
                  ## the same way as other labels, since the last label supplies
                  ## two dimvalues rather than one.
                  for (separator in intervalSeparators) {
                      first <- extractNumbersFromStartOfStrings(labels[n])
                      last <- extractNumbersFromEndOfStrings(labels[n],
                                                             intervalSeparator = separator)
                      implied.label <- paste(first, last, sep = separator)
                      if (identical(implied.label, labels[n])) {
                          print.lower <- last < limitPrintLower
                          if (all(c(first, last) == as.integer(c(first, last)))) {
                              if (print.lower)
                                  last <- last + 1L
                              else
                                  first <- first - 1L
                          }
                          dimvalues[c(n, n + 1L)] <- c(first, last)
                          decoded.labels[n] <- TRUE
                          break
                      }
                  }
              }
              if (!decoded.labels[n]) {
                  ## If the last interval has not been decoded, see if it is a
                  ## single integer x.  If so, assume that the final two
                  ## dimvalues are x and x+1. The decoding cannot be done
                  ## in the same way as other labels, since the last label
                  ## supplies two dimvalues rather than one.
                  if (stringsAreIntegers(labels[n])) {
                      lower <- as.integer(labels[n])
                      print.lower <- lower < limitPrintLower
                      if (print.lower)
                          dimvalues[c(n, n + 1L)] <- c(lower, lower + 1L)
                      else
                          dimvalues[c(n, n + 1L)] <- c(lower - 1L, lower)
                      decoded.labels[n] <- TRUE
                  }
              }
              if (!decoded.labels[n])
                  return(NULL)
              if (all(decoded.labels))
                  return(dimvalues)
              ## Deal with all remaining labels.  Extract a number from the front
              ## of each label, and construct implied labels based on these
              ## numbers. If these implied labels match the actual labels,
              ## then the attempt attempt was successful.
              lower <- extractNumbersFromStartOfStrings(labels[!decoded.labels])
              if (any(is.na(lower)))
                  return(NULL)
              if (!print.lower) {
                  lower.is.integer <- lower == as.integer(lower)
                  finite.dv <- dimvalues[is.finite(dimvalues)]
                  dv.is.integer <- finite.dv == as.integer(finite.dv)
                  if (all(lower.is.integer) && all(dv.is.integer))
                      lower <- lower - 1L
              }
              for (sep in intervalSeparators) {
                  possible.labels <-
                      makeLabelsForClosedIntervals(dimvalues =
                                                       c(lower, dimvalues[n]),
                                                   intervalSeparator = sep)
                  if (identical(possible.labels, labels[!decoded.labels])) {
                      dimvalues[seq_len(n)][!decoded.labels] <- lower
                      return(dimvalues)
                  }
              }
              NULL
          })

## HAS_TESTS
setMethod("inferDimvalues",
          signature(DimScale = "Intervals", labels = "NULL"),
          function(DimScale, labels) numeric())

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("labels",
          signature(object = "Intervals"),
          function(object) {
              dimvalues <- dimvalues(object)
              makeLabelsForIntervals(dimvalues = dimvalues)
          })

#' @export
length.Intervals <- function(x) {
    dimvalues <- dimvalues(x)
    length.dimvalues <- length(dimvalues)
    if (length.dimvalues > 0L)
        length.dimvalues - 1L
    else
        0L
}

#' @rdname internal-methods
#' @export
## HAS_TESTS
setMethod("length",
          signature(x = "Intervals"),
          length.Intervals)

## assume that canMakeCompatible has returned TRUE
## HAS_TESTS
setMethod("makeIndices",
          signature(x = "Intervals", y = "Intervals"),
          function(x, y, collapse) {
              if (identical(length(x), 0L))
                  return(integer())
              dvx <- dimvalues(x)
              dvy <- dimvalues(y)
              nx <- length(dvx)
              ny <- length(dvy)
              if (collapse) {
                  ans <- findInterval(x = dvx[-nx], vec = dvy)
                  ans[ans > (ny - 1L)] <- 0L
              }
              else  {
                  ans <- findInterval(x = dvy[-ny], vec = dvx)
                  ans[ans > (nx - 1L)] <- 0L
              }
              ans
          })

## HAS_TESTS
## Assume that canMakePairCompatible has returned TRUE.
setMethod("makePairIndices",
          signature(e1 = "Intervals", e2 = "Intervals"),
          function(e1, e2, isCounts1, isCounts2) {
              dv1 <- dimvalues(e1)
              dv2 <- dimvalues(e2)
              dv12 <- intersect(dv1, dv2)
              n1 <- length(dv1)
              n2 <- length(dv2)
              n12 <- length(dv12)
              if ((n1 == 0L) || (n2 == 0L)) {
                  ans1 <- if (isCounts1 && (n1 > 0L)) rep(0L, times = n1 - 1L) else integer()
                  ans2 <- if (isCounts2 && (n2 > 0L)) rep(0L, times = n2 - 1L) else integer()
              }
              else {
                  if (isCounts1 && isCounts2) {
                      ans1 <- findInterval(x = dv1[-n1], vec = dv12)
                      ans2 <- findInterval(x = dv2[-n2], vec = dv12)
                      ans1[ans1 == n12] <- 0L
                      ans2[ans2 == n12] <- 0L
                  }
                  else {
                      min <- dv12[1L]
                      max <- dv12[n12]
                      dv1.trim <- dv1[(min <= dv1) & (dv1 <= max)]
                      dv2.trim <- dv2[(min <= dv2) & (dv2 <= max)]
                      n1.trim <- length(dv1.trim)
                      n2.trim <- length(dv2.trim)
                      if (isCounts1) {
                          ans1 <- findInterval(x = dv1[-n1], vec = dv1.trim)
                          ans1[ans1 == n1.trim] <- 0L
                          ans2 <- findInterval(x = dv1.trim[-n1.trim], vec = dv2)
                      }
                      else if (isCounts2) {
                          ans1 <- findInterval(x = dv2.trim[-n2.trim], vec = dv1)
                          ans2 <- findInterval(x = dv2[-n2], vec = dv2.trim)
                          ans2[ans2 == n2.trim] <- 0L
                      }
                      else {
                          dv12.trim <- sort(union(dv1.trim, dv2.trim))
                          n12.trim <- length(dv12.trim)
                          ans1 <- findInterval(x = dv12.trim[-n12.trim], vec = dv1)
                          ans2 <- findInterval(x = dv12.trim[-n12.trim], vec = dv2)
                      }
                  }
              }
              list(ans1, ans2)
          })

## HAS_TESTS
## Assume that canMakePairCompatible has returned TRUE, implying that
## e1 and e2 start and finish at the same places.  Assume also that
## the objects being manipulated both have class "Values".
setMethod("mergeDimScales",
          signature(e1 = "Intervals", e2 = "Intervals"),
          function(e1, e2) {
              dv1 <- dimvalues(e1)
              dv2 <- dimvalues(e2)
              dv12 <- intersect(dv1, dv2)
              min12 <- dv12[1L]
              max12 <- dv12[length(dv12)]
              dv <- sort(union(dv1, dv2))
              dv <- dv[dv >= min12]
              dv <- dv[dv <= max12]
              methods::new("Intervals", dimvalues = dv)
          })

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("stepLengths",
          signature(object = "Intervals"),
          function(object) {
              dimvalues <- dimvalues(object)
              diff(dimvalues)
          })

