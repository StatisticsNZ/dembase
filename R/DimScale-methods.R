
## HAS_TESTS
setAs(from = "DimScale", to = "Categories",
      function(from)
      methods::new("Categories", dimvalues = labels(from)))

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("[",
          signature(x = "DimScale", i = "integer"),
          function(x, i) {
              if (any(is.na(i)))
                  stop(gettextf("'%s' has missing values", "i"))
              dv.before <- dimvalues(x)
              n <- length(dv.before)
              if (!all(abs(i) <= n))
                  stop(gettextf("'%s' has values outside the valid range", "i"))
              dv.after <- dv.before[i]
              methods::new(class(x), dimvalues = dv.after)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("%in%",
          signature(x = "DimScale", table = "ANY"),
          function(x, table) {
              nms <- deparse(substitute(x))
              indices <- methods::callGeneric(x = dimvalues(x), table = table)
              indices <- list(indices)
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Compare",
          signature(e1 = "DimScale", e2 = "ANY"),
          function(e1, e2) {
              nms <- deparse(substitute(e1))
              indices <- methods::callGeneric(e1 = dimvalues(e1), e2 = e2)
              indices <- list(indices)
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Compare",
          signature(e1 = "ANY", e2 = "DimScale"),
          function(e1, e2) {
              nms <- deparse(substitute(e2))
              indices <- methods::callGeneric(e1 = e1, e2 = dimvalues(e2))
              indices <- list(indices)
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Compare",
          signature(e1 = "DimScale", e2 = "DimScale"),
          function(e1, e2) {
              stop(gettext("attempt to compare two dimscales"))
          })

## HAS_TESTS
setMethod("canMakeDimScalePairCompatible",
          signature(e1 = "DimScale", e2 = "DimScale"),
          function(e1, e2) {
              if (!identical(class(e1), class(e2)))
                  stop(gettextf("one dimension has dimscale \"%s\" and other has dimscale \"%s\"",
                                class(e1), class(e2)))
              dv1 <- dimvalues(e1)
              dv2 <- dimvalues(e2)
              n1 <- length(dv1)
              n2 <- length(dv2)
              if ((n1 == 0L) || (n2 == 0L))
                  TRUE
              else {
                  dv12 <- intersect(dv1, dv2)
                  n12 <- length(dv12)
                  if (n12 > 0L)
                      TRUE
                  else
                      stop(gettext("no values in common"))
              }
          })

## HAS_TESTS
setMethod("canMakeDimScalesCompatible",
          signature(x = "DimScale", y = "DimScale"),
          function(x, y, subset = FALSE, collapse, concordance = NULL) {
              if (!identical(class(x), class(y)))
                  stop(gettextf("one dimension has dimscale \"%s\" but other has dimscale \"%s\"",
                                class(x), class(y)))
              in.y.not.in.x <- setdiff(dimvalues(y), dimvalues(x))
              n.in.y.not.in.x <- length(in.y.not.in.x)
              if (n.in.y.not.in.x > 0L) {
                  if (is.character(in.y.not.in.x))
                      in.y.not.in.x <- dQuote(in.y.not.in.x)
                  stop(sprintf(ngettext(n.in.y.not.in.x,
                                        "one dimension has value [%s] that other does not",
                                        "one dimension has values [%s] that other does not"),
                               paste(in.y.not.in.x, collapse = ", ")))
              }
              if (!subset) {
                  in.x.not.in.y <- setdiff(dimvalues(x), dimvalues(y))
                  n.in.x.not.in.y <- length(in.x.not.in.y)
                  if (n.in.x.not.in.y > 0L) {
                      if (is.character(in.x.not.in.y))
                          in.x.not.in.y <- dQuote(in.x.not.in.y)
                      stop(sprintf(ngettext(n.in.x.not.in.y,
                                            "one dimension has value [%s] that other does not",
                                            "one dimension has values [%s] that other does not"),
                                   paste(in.x.not.in.y, collapse = ", ")))
                  }
              }
              TRUE
          })

## HAS_TESTS
## 'index' comes from a "CollapseTransform" object, so it
## should be in the valid range, but it may have duplicates
## (which are accepted by "Intervals" method)
setMethod("collapseDimScale",
          signature(object = "DimScale", index = "integer"),
          function(object, index) {
              if (any(duplicated(index[index != 0L])))
                  stop(gettextf("'%s' has duplicates", "index"))
              index <- match(seq_along(index), index, nomatch = 0L)
              methods::new(class(object), dimvalues = dimvalues(object)[index])
          })

## HAS_TESTS
setMethod("dbindDimScales",
          signature(e1 = "DimScale", e2 = "DimScale"),
          function(e1, e2, along) {
              dimvalues1 <- dimvalues(e1, use.names = FALSE)
              dimvalues2 <- dimvalues(e2, use.names = FALSE)
              same <- intersect(dimvalues1, dimvalues2)
              n.same <- length(same)
              if (n.same > 0L)
                  stop(sprintf(ngettext(n.same,
                                        "\"%s\" dimensions both have value %s",
                                        "\"%s\" dimensions both have values %s"),
                               along,
                               paste(dQuote(same), collapse = ", ")))
              dimvalues <- c(dimvalues1, dimvalues2)
              methods::new("Categories", dimvalues = dimvalues)
          })

#' @rdname dimscales
setMethod("dimscales",
          signature(object = "DimScale"),
          function(object) as.character(class(object)))

setMethod("dimvalues",
          signature(object = "DimScale"),
          function(object) object@dimvalues)

## HAS_TESTS
setMethod("e1IsFirstDimScale",
          signature(e1 = "DimScale", e2 = "DimScale"),
          function(e1, e2) TRUE)

## HAS_TESTS
setMethod("extendDimScale",
          signature(object = "DimScale", index = "integer"),
          function(object, index)
          methods::new(class(object), dimvalues = dimvalues(object)[index]))

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("incrementDimScale",
          signature(object = "DimScale"),
          function(object, n) {
              stop(gettextf("'%s' argument can only be used when \"%s\" dimension has numeric dimscale (eg \"%s\" or \"%s\")",
                            "n", "along", "Intervals", "Points"))
          })

#' @rdname internal-methods
#' @export
setMethod("labels",
          signature(object = "DimScale"),
          function(object) as.character(dimvalues(object)))

#' @export
length.DimScale <- function(x) length(dimvalues(x))

#' @rdname internal-methods
#' @export
setMethod("length",
          signature(x = "DimScale"),
          length.DimScale)

## HAS_TESTS
## Assume that 'canMakeCompatible' has returned TRUE before function called.
setMethod("makeIndices",
          signature(x = "DimScale", y = "DimScale"),
          function(x, y, collapse, concordance) {
              if (collapse)
                  match(dimvalues(x), dimvalues(y), nomatch = 0L)
              else
                  match(dimvalues(y), dimvalues(x), nomatch = 0L)
          })


## makeMissingAgeTimeDimScale ############################################

## Assume no dimensions have length 0, and object
## has regular age-time plan

## HAS_TESTS
setMethod("makeMissingAgeTimeDimScale",
          signature(age = "DimScale",
                    time = "DimScale",
                    cohort = "missing",
                    triangle = "ANY"),
          function(age, time) {
              dv.age <- dimvalues(age)
              dv.time <- dimvalues(time)
              dv.cohort.head <- dv.time[1L] - rev(dv.age)
              dv.cohort.tail <- dv.time[-1L] - dv.age[1L]
              dv.cohort <- c(dv.cohort.head, dv.cohort.tail)
              new("Intervals", dimvalues = dv.cohort)
          })

## HAS_TESTS
setMethod("makeMissingAgeTimeDimScale",
          signature(age = "Intervals",
                    time = "missing",
                    cohort = "Intervals",
                    triangle = "missing"),
          function(age, cohort) {
              dv.age <- dimvalues(age)
              dv.cohort <- dimvalues(cohort)
              n.age <- length(dv.age)
              n.cohort <- length(dv.cohort)
              dv.time <- dv.cohort[-1L] + dv.age[1L]
              if (n.age > 2L) {
                  dv.time.last.cohort <- dv.cohort[n.cohort] + dv.age[-c(1L, n.age)]
                  dv.time <- c(dv.time, dv.time.last.cohort)
              }
              new("Points", dimvalues = dv.time)
          })

## HAS_TESTS
setMethod("makeMissingAgeTimeDimScale",
          signature(age = "Intervals",
                    time = "missing",
                    cohort = "Intervals",
                    triangle = "Triangles"),
          function(age, cohort, triangle) {
              dv.age <- dimvalues(age)
              dv.cohort <- dimvalues(cohort)
              n.age <- length(dv.age)
              n.cohort <- length(dv.cohort)
              dv.time <- dv.cohort + dv.age[1L]
              dv.time.last.cohort <- dv.cohort[n.cohort] + dv.age[-1L]
              dv.time <- c(dv.time, dv.time.last.cohort)
              new("Intervals", dimvalues = dv.time)
          })

## HAS_TESTS
## There is no 'with-triangles' version of this
## method, since age has DimScale "Points"
setMethod("makeMissingAgeTimeDimScale",
          signature(age = "Points",
                    time = "missing",
                    cohort = "Intervals",
                    triangle = "missing"),
          function(age, cohort) {
              dv.age <- dimvalues(age)
              dv.cohort <- dimvalues(cohort)
              n.age <- length(dv.age)
              n.cohort <- length(dv.cohort)
              dv.time <- dv.cohort + dv.age[1L]
              if (n.age > 1L) {
                  dv.time.last.cohort <- dv.cohort[n.cohort] + dv.age[-1L]
                  dv.time <- c(dv.time, dv.time.last.cohort)
              }
              new("Intervals", dimvalues = dv.time)
          })

## HAS_TESTS
setMethod("makeMissingAgeTimeDimScale",
          signature(age = "missing",
                    time = "Intervals",
                    cohort = "Intervals",
                    triangle = "missing"),
          function(time, cohort) {
              dv.time <- dimvalues(time)
              dv.cohort <- dimvalues(cohort)
              n.time <- length(dv.time)
              n.cohort <- length(dv.cohort)
              dv.age <- dv.time[2L] - rev(dv.cohort[-1L])
              if (n.time > 2L) {
                  dv.later.periods <- dv.time[2L] - dv.cohort[2L] + dv.time[-(1:2)] - dv.time[2L]
                  dv.age <- c(dv.age, dv.later.periods)
              }
              new("Points", dimvalues = dv.age)
          })

## HAS_TESTS
setMethod("makeMissingAgeTimeDimScale",
          signature(age = "missing",
                    time = "Intervals",
                    cohort = "Intervals",
                    triangle = "Triangles"),
          function(time, cohort, triangle) {
              dv.time <- dimvalues(time)
              dv.cohort <- dimvalues(cohort)
              n.time <- length(dv.time)
              n.cohort <- length(dv.cohort)
              dv.age <- dv.time[1L] - rev(dv.cohort)
              dv.age <- dv.age[dv.age >= 0L]
              dv.later.periods <- dv.time[1L] - dv.cohort[1L] + dv.time[-1L] - dv.time[1L]
              dv.age <- c(dv.age, dv.later.periods)
              new("Intervals", dimvalues = dv.age)
          })

## HAS_TESTS
## There is no 'with-triangles' version of this
## method, since time has DimScale "Points"
setMethod("makeMissingAgeTimeDimScale",
          signature(age = "missing",
                    time = "Points",
                    cohort = "Intervals",
                    triangle = "missing"),
          function(time, cohort) {
              dv.time <- dimvalues(time)
              dv.cohort <- dimvalues(cohort)
              n.time <- length(dv.time)
              dv.cohort.already.born <- dv.cohort[dv.cohort <= dv.time[1L]]
              n.cohort.already.born <- length(dv.cohort.already.born)
              dv.age <- dv.time[1L] - rev(dv.cohort.already.born)
              if (n.time > 1L) {
                  dv.age.first.cohort <- dv.age[n.cohort.already.born] + (dv.time[-1L] - dv.time[1L])
                  dv.age <- c(dv.age, dv.age.first.cohort)
              }
              new("Intervals", dimvalues = dv.age)
          })

## makePairIndices ############################################################


## HAS_TESTS
## Assume that 'canMakeCompatible' has returned TRUE before function called.
## Class "Intervals" has its own method.
setMethod("makePairIndices",
          signature(e1 = "DimScale", e2 = "DimScale"),
          function(e1, e2, isCounts1, isCounts2) {
              dv1 <- dimvalues(e1)
              dv2 <- dimvalues(e2)
              dv12 <- intersect(dv1, dv2)
              if (isCounts1)
                  ans1 <- match(dv1, dv12, nomatch = 0L)
              else
                  ans1 <- match(dv12, dv1, nomatch = 0L)
              if (isCounts2)
                  ans2 <- match(dv2, dv12, nomatch = 0L)
              else
                  ans2 <- match(dv12, dv2, nomatch = 0L)
              list(ans1, ans2)
          })



## mergeDimScales ############################################################

## HAS_TESTS
## Assume that canMakePairCompatible has returned TRUE, implying that e1 and
## e2 have the same class and same dimvalues (possibly in different orders).
setMethod("mergeDimScales",
          signature(e1 = "DimScale", e2 = "DimScale"),
          function(e1, e2) {
              dv1 <- dimvalues(e1)
              dv2 <- dimvalues(e2)
              dv12 <- intersect(dv1, dv2)
              methods::new(class(e1), dimvalues = dv12)
          })


## stepLengths ##############################################################

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("stepLengths",
          signature(object = "DimScale"),
          function(object)
          stop(gettextf("step lengths not defined for dimscale \"%s\"",
                        class(object))))







