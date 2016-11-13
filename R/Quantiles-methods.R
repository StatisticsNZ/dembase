
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




