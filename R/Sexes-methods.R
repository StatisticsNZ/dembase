
setAs(from = "Sexes", to = "Intervals",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Intervals")
      })

setAs(from = "Sexes", to = "Points",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Points")
      })

setAs(from = "Sexes", to = "Quantiles",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Quantiles")
      })

setAs(from = "Sexes", to = "Iterations",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Iterations")
      })

setAs(from = "Sexes", to = "Triangles",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Triangles")
      })

setMethod("dbindDimScales",
          signature(e1 = "Sexes", e2 = "Sexes"),
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
              methods::new("Sexes", dimvalues = dimvalues)
          })

setMethod("inferDimvalues",
          signature(DimScale = "Sexes", labels = "character"),
          function(DimScale, labels) {
              labels.lower <- tolower(labels)
              no.duplicates <- !any(duplicated(labels.lower))
              no.missing <- !any(is.na(labels.lower))
              valid.singular <- all(labels.lower %in% c("female", "male"))
              valid.plural <- all(labels.lower %in% c("females", "males"))
              valid.letters <- all(labels.lower %in% c("f", "m"))
              if (no.duplicates
                  && no.missing
                  && (valid.singular || valid.plural || valid.letters))
                  labels
              else
                  NULL
          })



