
## HAS_TESTS
setAs(from = "Categories", to = "Sexes",
      function(from) {
        dimvalues <- dimvalues(from)
        dimvalues.lower <- tolower(dimvalues)
        valid.singular <- all(dimvalues.lower %in% c("female", "male"))
        valid.plural <- all(dimvalues.lower %in% c("females", "males"))
        if (valid.singular || valid.plural)
            methods::new("Sexes", dimvalues = dimvalues)
        else
          stop("labels not valid for dimscale")
      })

## HAS_TESTS
setAs(from = "Categories", to = "Triangles",
      function(from) {
        dimvalues <- dimvalues(from)
        dimvalues.lower <- tolower(dimvalues)
        if (all(dimvalues.lower %in% c("lower", "upper")) ||
            all(dimvalues.lower %in% c("tl", "tu")))
          methods::new("Triangles", dimvalues = dimvalues)
        else
          stop("labels not valid for dimscale")
      })

## HAS_TESTS
setAs(from = "Categories", to = "Intervals",
      function(from) {
        dimvalues <- inferDimvalues(DimScale = methods::new("Intervals"),
                                    labels = dimvalues(from))
        if (!is.null(dimvalues))
          methods::new("Intervals", dimvalues = dimvalues)
        else
          stop("labels not valid for dimscale")
      })

## HAS_TESTS
setAs(from = "Categories", to = "Points",
      function(from) {
        dimvalues <- dimvalues(from)
        dimvalues <- suppressWarnings(as.numeric(dimvalues))
        if (!any(is.na(dimvalues)) &&
            all(diff(dimvalues) > 0))
          methods::new("Points", dimvalues = dimvalues)
        else
          stop("labels not valid for dimscale")
        })

## HAS_TESTS
setAs(from = "Categories", to = "Quantiles",
      function(from) {
        dimvalues.from <- dimvalues(from)
        if (all(grepl("%$", dimvalues.from))) {
          dimvalues <- sub("%$", "", dimvalues.from)
          dimvalues <- suppressWarnings(as.numeric(dimvalues))
          dimvalues <- dimvalues / 100
        }
        else
          dimvalues <- suppressWarnings(as.numeric(dimvalues.from))
        if (!any(is.na(dimvalues)) &&
            all(dimvalues >= 0) &&
            all(dimvalues <= 1))
          methods::new("Quantiles", dimvalues = dimvalues)
        else
          stop("labels not valid for dimscale")
      })

## HAS_TESTS
setAs(from = "Categories", to = "Iterations",
      function(from) {
        dimvalues <- dimvalues(from)
        dimvalues <- suppressWarnings(as.numeric(dimvalues))
        if (!any(is.na(dimvalues)) &&
            all(dimvalues > 0) &&
            all(is.finite(dimvalues)) &&
            all(dimvalues == as.integer(dimvalues)))
          methods::new("Iterations", dimvalues = as.integer(dimvalues))
        else
          stop("labels not valid for dimscale")
      })

## HAS_TESTS
setMethod("canMakeDimScalesCompatible",
          signature(x = "Categories", y = "Categories"),
          function(x, y, subset, collapse, concordance) {
              dv.x <- dimvalues(x)
              dv.y <- dimvalues(y)
              if (!is.null(concordance)) {
                  if (collapse) # 'x' comes from Counts object
                      dv.x <- translate(dv.x, concordance = concordance)
                  else          # 'x' comes from Values object
                      dv.y <- translate(dv.y, concordance = concordance)
              }
              in.y.not.in.x <- setdiff(dv.y, dv.x)
              n.in.y.not.in.x <- length(in.y.not.in.x)
              if (n.in.y.not.in.x > 0L) {
                  in.y.not.in.x <- dQuote(in.y.not.in.x)
                  in.y.not.in.x <- paste(in.y.not.in.x, collapse = ", ")
                  stop(sprintf(ngettext(n.in.y.not.in.x,
                                        "one dimension has value [%s] that other does not",
                                        "one dimension has values [%s] that other does not"),
                               in.y.not.in.x))
              }
              if (!subset) {
                  in.x.not.in.y <- setdiff(dv.x, dv.y)
                  n.in.x.not.in.y <- length(in.x.not.in.y)
                  if (n.in.x.not.in.y > 0L) {
                      in.x.not.in.y <- dQuote(in.x.not.in.y)
                      in.x.not.in.y <- paste(in.x.not.in.y, collapse = ", ")
                      stop(sprintf(ngettext(n.in.x.not.in.y,
                                            "one dimension has value [%s] that other does not",
                                            "one dimension has values [%s] that other does not"),
                                   in.x.not.in.y))
                  }
              }
              TRUE
          })


setMethod("collapseDimScale",
          signature(object = "Categories", index = "integer"),
          function(object, index, concordance = NULL) {
              dimvalues <- dimvalues(object)
              if (!is.null(concordance))
                  dimvalues <- translate(object = dimvalues,
                                         concordance = concordance)
              s <- seq_along(index)
              i <- match(s, index, nomatch = 0L)
              dimvalues <- dimvalues[i]
              if (any(duplicated(dimvalues)))
                  stop(gettextf("'%s' have duplicates", "dimvalues"))
              methods::new(class(object),
                           dimvalues = dimvalues)
          })


## HAS_TESTS
setMethod("inferDimvalues",
          signature(DimScale = "Categories", labels = "character"),
          function(DimScale, labels) {
            if (!any(is.na(labels)))
              labels
            else
              NULL
          })

## HAS_TESTS
setMethod("inferDimvalues",
          signature(DimScale = "Categories", labels = "NULL"),
          function(DimScale, labels) character())


## assume that canMakeCompatible has returned TRUE
setMethod("makeIndices",
          signature(x = "Categories", y = "Categories", concordance = "ManyToOne"),
          function(x, y, collapse, concordance) {
              dvx <- dimvalues(x)
              dvy <- dimvalues(y)
              if (collapse) {
                  dvx <- translate(dvx, concordance = concordance)
                  match(dvx, dvy, nomatch = 0L)
              }
              else {
                  dvy <- translate(dvy, concordance = concordance)
                  match(dvy, dvx, nomatch = 0L)
              }
          })
              



