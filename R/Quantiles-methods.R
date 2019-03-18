
## NO_TESTS
setAs(from = "Quantiles", to = "Sexes",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
      else
        methods::new("Sexes")
      })

## NO_TESTS
setAs(from = "Quantiles", to = "Triangles",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
      else
        methods::new("Triangles")
      })

## NO_TESTS
setAs(from = "Quantiles", to = "Intervals",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Intervals")
      })

## NO_TESTS
setAs(from = "Quantiles", to = "Iterations",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Iterations")
      })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("%in%",
          signature(x = "Quantiles", table = "ANY"),
          function(x, table) {
              nms <- deparse(substitute(x))
              table.decimal <- numeric(length = length(table))
              for (i in seq_along(table)) {
                  checkSinglePercent(table[i],
                                     name = "table")
                  table.decimal[i] <- percentToDecimal(table[i])
              }
              indices <- methods::callGeneric(x = dimvalues(x),
                                              table = table.decimal)
              indices <- list(indices)
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Compare",
          signature(e1 = "Quantile", e2 = "ANY"),
          function(e1, e2) {
              nms <- deparse(substitute(e1))
              checkSinglePercent(value = e2,
                                 name = "e2")
              e2 <- percentToDecimal(e2)
              indices <- methods::callGeneric(e1 = dimvalues(e1), e2 = e2)
              indices <- list(indices)
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

## HAS_TESTS
#' @rdname internal-methods
#' @export
setMethod("Compare",
          signature(e1 = "ANY", e2 = "Quantile"),
          function(e1, e2) {
              nms <- deparse(substitute(e2))
              checkSinglePercent(value = e1,
                                 name = "e1")
              e1 <- percentToDecimal(e1)
              indices <- methods::callGeneric(e1 = e1, e2 = dimvalues(e2))
              indices <- list(indices)
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

#' @rdname internal-methods
## NO_TESTS
setMethod("diff",
          signature(x = "Quantiles"),
          function(x) stop(gettext("dimscale \"%s\"", tolower(class(x)))))

## HAS_TESTS
setMethod("dbindDimScales",
          signature(e1 = "Quantiles", e2 = "Quantiles"),
          function(e1, e2, along) {
              dimvalues <- combineDimvaluesForPoints(e1 = e1, e2 = e2, along = along)
              methods::new("Quantiles", dimvalues = dimvalues)
          })

## NO_TESTS
#' @rdname internal-methods
#' @export
setMethod("labels",
          signature(object = "Quantiles"),
          function(object) {
            dimvalues <- dimvalues(object)
            sprintf("%s%%", 100 * dimvalues)
          })

## required to overload method for Points
#' @rdname exported-not-api
#' @export
setMethod("stepLengths",
          signature(object = "Quantiles"),
          function(object)
          stop(gettextf("step lengths not defined for dimscale \"%s\"",
                        class(object))))

setMethod("inferDimvalues",
          signature(DimScale = "Quantiles", labels = "character"),
          function(DimScale, labels) {
            if (all(grepl("%$", labels))) {
              ans <- sub("%$", "", labels)
              ans <- suppressWarnings(as.numeric(ans))
              ans <- ans / 100
            }
            else
              ans <- suppressWarnings(as.numeric(labels))
            if (!any(is.na(ans)) &&
                all(diff(ans) > 0) &&
                all(ans >= 0) &&
                all(ans <= 1))
              ans
            else
              NULL
          })

setMethod("inferDimvalues",
          signature(DimScale = "Quantiles", labels = "NULL"),
          function(DimScale, labels) numeric())




