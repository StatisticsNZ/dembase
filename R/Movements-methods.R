
## NO_TESTS
#' @rdname exported-not-api
setMethod("DimScales",
          signature(object = "Movements"),
          function(object) {
              population <- object@population
              components <- object@components
              first.component <- components[[1L]]
              dimtypes.popn <- dimtypes(population, use.names = FALSE)
              dimtypes.comp <- dimtypes(first.component, use.names = FALSE)
              DimScales.popn <- DimScales(population, use.names = FALSE)
              DimScales.comp <- DimScales(first.component, use.names = FALSE)
              i.time.popn <- match("time", dimtypes.popn)
              i.time.comp <- match("time", dimtypes.comp)
              DS.time.comp <- DimScales.comp[[i.time.comp]]
              replace(DimScales.popn,
                      list = i.time.popn,
                      values = list(DS.time.comp))
          })

## HAS_TESTS
#' @rdname accession
setMethod("accession",
          signature(object = "Movements"),
          function(object, births = TRUE) {
              if (!is.logical(births))
                  stop(gettextf("'%s' does not have type \"%s\"",
                                "births", "logical"))
              if (!identical(length(births), 1L))
                  stop(gettextf("'%s' does not have length %d",
                                "births", 1L))
              if (is.na(births))
                  stop(gettextf("'%s' is missing",
                                "births"))
              population <- object@population
              components <- object@components
              .Data <- population@.Data
              dim <- dim(population)
              names <- names(population)
              dimtypes <- dimtypes(population,
                                   use.names = FALSE)
              DimScales <- DimScales(population,
                                     use.names = FALSE)
              i.age <- match("age", dimtypes, nomatch = 0L)
              has.age <- i.age > 0L
              if (!has.age)
                  return(NULL)
              ans <- agePopnForwardUpperTri(population)
              is.births <- sapply(components, methods::is, "Births")
              for (component in components[!is.births]) {
                  increment.upper.tri <- incrementUpperTri(component = component,
                                                           population = population)
                  ans <- ans + increment.upper.tri
              }
              if (births) {
                  has.births <- any(is.births)
                  names.ans <- names(ans)
                  dimtypes.ans <- dimtypes(ans,
                                           use.names = FALSE)
                  DimScales.ans <- DimScales(ans,
                                             use.names = FALSE)
                  i.age.ans <- match("age", dimtypes.ans)
                  if (has.births) {
                      births.comp <- components[[which(is.births)]]
                      accession.births <- incrementUpperTri(component = births.comp,
                                                            population = population)
                  }
                  else {
                      DimScale.age.acc.bth <- new("Points", dimvalues = 0)
                      DimScales.acc.bth <- replace(DimScales.ans,
                                                   list = i.age.ans,
                                                   values = list(DimScale.age.acc.bth))
                      metadata.acc.bth <- methods::new("MetaData",
                                                       nms = names.ans,
                                                       dimtypes = dimtypes.ans,
                                                       DimScales = DimScales.acc.bth)
                      .Data.acc.bth <- array(0L,
                                             dim = dim(metadata.acc.bth),
                                             dimnames = dimnames(metadata.acc.bth))
                      accession.births <- new("Counts",
                                              .Data = .Data.acc.bth,
                                              metadata = metadata.acc.bth)
                  }
                  name.age <- names.ans[i.age.ans]
                  ans <- dbind(ans,
                               accession.births,
                               along = name.age)
                  ans <- aperm(ans,
                               perm = names)
              }
              ans
          })

## HAS_TESTS
#' @rdname isInternallyConsistent
setMethod("isInternallyConsistent",
          signature(object = "Movements"),
          function(object) {
              population <- object@population
              components <- object@components
              dimtypes <- dimtypes(population,
                                   use.names = FALSE)
              i.age <- match("age", dimtypes, nomatch = 0L)
              i.time <- match("time", dimtypes)
              has.age <- i.age > 0L
              n.time.popn <- dim(population)[i.time]
              ## obtained
              if (has.age)
                  popn.end.obtained <- popnEndWithAge(object)
              else
                  popn.end.obtained <- popnEndNoAge(object)
              ## expected
              elements <- seq.int(from = 2L, to = n.time.popn)
              popn.end.expected <- slab(population,
                                        dimension = "time",
                                        elements = elements)
              popn.end.obtained == popn.end.expected
          })

