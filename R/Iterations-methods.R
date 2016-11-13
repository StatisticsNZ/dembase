
setAs(from = "Iterations", to = "Sexes",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Sexes")
      })

setAs(from = "Iterations", to = "Triangles",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Triangles")
      })

setAs(from = "Iterations", to = "Points",
      function(from) {
        dimvalues <- dimvalues(from)
        dimvalues <- as.numeric(dimvalues)
        methods::new("Points", dimvalues = dimvalues)
      })

setAs(from = "Iterations", to = "Quantiles",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Quantiles")
      })

setAs(from = "Iterations", to = "Intervals",
      function(from) {
        dimvalues <- dimvalues(from)
        n <- length(dimvalues)
        if (n > 0) {
          if (all(diff(dimvalues) == 1L)) {
            dimvalues <- c(dimvalues, dimvalues[n] + 1L)
            methods::new("Intervals", dimvalues = dimvalues)
          }
          else
            stop("labels not valid for dimscale")
        }
        else
          methods::new("Intervals")
      })

## HAS_TESTS
setMethod("dbindDimScales",
          signature(e1 = "Iterations", e2 = "Iterations"),
          function(e1, e2, along) {
              n1 <- length(e1)
              n2 <- length(e2)
              dimvalues <- seq_len(n1 + n2)
              methods::new("Iterations", dimvalues = dimvalues)
          })

## HAS_TESTS
setMethod("inferDimvalues",
          signature(DimScale = "Iterations", labels = "character"),
          function(DimScale, labels) {
            ans <- suppressWarnings(as.numeric(labels))
            if (!any(is.na(ans)) &&
                all(ans > 0) &&
                !any(duplicated(ans)) &&
                all(ans == as.integer(ans)))
              as.integer(ans)
            else
              NULL
          })

## HAS_TESTS
setMethod("inferDimvalues",
          signature(DimScale = "Iterations", labels = "NULL"),
          function(DimScale, labels) integer())
