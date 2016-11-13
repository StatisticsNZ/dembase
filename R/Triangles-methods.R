

setAs(from = "Triangles", to = "Intervals",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Intervals")
      })

setAs(from = "Triangles", to = "Points",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Points")
      })

setAs(from = "Triangles", to = "Quantiles",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Quantiles")
      })

setAs(from = "Triangles", to = "Sexes",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Sexes")
      })

setAs(from = "Triangles", to = "Iterations",
      function(from) {
        if (length(from) > 0L)
          stop("labels not valid for dimscale")
        else
          methods::new("Iterations")
      })

setMethod("inferDimvalues",
          signature(DimScale = "Triangles", labels = "character"),
          function(DimScale, labels) {
            labels <- tolower(labels)
            no.duplicates <- !any(duplicated(labels))
            no.missing <- !any(is.na(labels))
            all.lower.upper <- all(labels %in% c("lower", "upper"))
            all.tl.tu <- all(labels %in% c("tl", "tu"))
            valid.values <- all.lower.upper || all.tl.tu
            if (no.duplicates && no.missing && valid.values) {
                if (all.lower.upper)
                    gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", labels, perl=TRUE)
                else
                    toupper(labels)
            }
            else
                NULL
          })



