
setAs(from = "Points", to = "Triangles",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Triangles")
      })

setAs(from = "Points", to = "Quantiles",
      function(from) {
        if (length(from) > 0L) {
          dimvalues <- dimvalues(from)
          if (all(dimvalues >= 0) && all(dimvalues <= 1))
            methods::new("Quantiles", dimvalues = dimvalues)
          else
            stop("labels not valid for dimscale")
        }
        else
          methods::new("Quantiles")
      })

setAs(from = "Points", to = "Intervals",
      function(from) {
        if (length(from) > 0L) {
          dimvalues <- dimvalues(from)
          all.integers <- all(as.integer(dimvalues) == dimvalues)
          consecutive <- all(diff(dimvalues) == 1)
          if (all.integers && consecutive) {
            last.value <- dimvalues[length(dimvalues)] + 1
            dimvalues <- c(dimvalues, last.value)
            methods::new("Intervals", dimvalues = dimvalues)
          }
          else
            stop("labels not valid for dimscale")
        }
        else
          methods::new("Intervals")
      })

setAs(from = "Points", to = "Iterations",
      function(from) {
        if (length(from) > 0L) {
          dimvalues <- dimvalues(from)
          all.positive <- all(dimvalues > 0)
          all.integer <- all(dimvalues == as.integer(dimvalues))
          if (all.positive && all.integer)
            methods::new("Iterations", dimvalues = as.integer(dimvalues))
          else
            stop("labels not valid for dimscale")
        }
        else
          methods::new("Iterations")
      })

## HAS_TESTS
setMethod("dbindDimScales",
          signature(e1 = "Points", e2 = "Points"),
          function(e1, e2, along) {
              dimvalues <- combineDimvaluesForPoints(e1 = e1, e2 = e2, along = along)
              methods::new("Points", dimvalues = dimvalues)
          })

## HAS_TESTS
setMethod("e1IsFirstDimScale",
          signature(e1 = "Points", e2 = "Points"),
          function(e1, e2) {
              dv1 <- e1@dimvalues
              dv2 <- e2@dimvalues
              n1 <- length(dv1)
              n2 <- length(dv2)
              has.zero.length <- (n1 == 0L) || (n2 == 0L)
              if (has.zero.length)
                  TRUE
              else
                  max(dv2) >= min(dv1)
          })          

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("incrementDimScale",
          signature(object = "Points"),
          function(object, n) {
              n <- checkAndTidyNIncrement(n)
              dv <- object@dimvalues
              forward <- n > 0L
              n.dv <- length(dv)
              if (n.dv < 2L)
                  stop(gettextf("\"%s\" dimension has length %d",
                                "along", n.dv))
              if (n.dv == 2L)
                  step = diff(dv)
              else {
                  step <- diff(dv[1:2])
                  if (!all(diff(dv[-1L]) == step))
                      stop(gettextf("points on \"%s\" dimension not regularly spaced",
                                   "along"))
              }
              if (forward) {
                  dimvalues <- seq(from = dv[n.dv],
                                   by = step,
                                   length.out = n + 1L)
                  dimvalues <- dimvalues[-1L]
              }
              else {
                  dimvalues <- seq(to = dv[1L],
                                   by = step,
                                   length.out = -n + 1L)
                  dimvalues <- dimvalues[n - 1L]
              }
              methods::new("Points", dimvalues = dimvalues)
          })

setMethod("inferDimvalues",
          signature(DimScale = "Points", labels = "character"),
          function(DimScale, labels) {
            ans <- suppressWarnings(as.numeric(labels))
            if (!any(is.na(ans))) {
                ans <- sort(ans)
                if(all(diff(ans) > 0))
                    ans
                else
                    NULL
            }
            else
              NULL
          })

setMethod("inferDimvalues",
          signature(DimScale = "Points", labels = "NULL"),
          function(DimScale, labels) numeric())

#' @rdname exported-not-api
#' @export
setMethod("stepLengths",
          signature(object = "Points"),
          function(object) {
            dimvalues <- dimvalues(object)
            diff(dimvalues)
          })
