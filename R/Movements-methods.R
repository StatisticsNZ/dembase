
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

## NO_TESTS
#' @rdname makeConsistent
setMethod("makeConsistent",
          signature(object = "Movements"),
          function(object, adjust = TRUE, scale = 0.1) {
              checkAdjustAndScale(adjust = adjust,
                                  scale = scale)
              population <- object@population
              components <- object@components
              dimtypes <- dimtypes(population,
                                   use.names = FALSE)
              i.age <- match("age", dimtypes, nomatch = 0L)
              has.age <- i.age > 0L
              if (any(is.na(population)))
                  population <- impute(population)
              for (i in seq_along(components)) {
                  component <- components[[i]]
                  if (any(is.na(component))) {
                      component <- impute(component)
                      components[[i]] <- component
                  }
              }
              object@population <- population
              object@components <- components
              if (has.age)
                  derivePopnMoveHasAge(object = object,
                                       adjust = adjust,
                                       scale = scale)
              else
                  derivePopnMoveNoAge(object = object,
                                      adjust = adjust,
                                      scale = scale)
          })

## HAS_TESTS
#' @rdname isConsistent
setMethod("isConsistent",
          signature(object = "Movements"),
          function(object) {
              population <- object@population
              components <- object@components
              metadata = metadata(object)
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
                                        dimension = i.time,
                                        elements = elements)
              ans <- popn.end.obtained == popn.end.expected
              ans <- array(ans,
                           dim = dim(metadata),
                           dimnames = dimnames(metadata))
              ans
          })

#' @rdname internal-methods
#' @export
setMethod("summary",
          signature(object = "Movements"),
          function(object) {
              population <- object@population
              components <- object@components
              names.components <- object@namesComponents
              metadata <- metadata(object)
              i.time.popn <- match("time", dimtypes(population))
              population <- new("Counts",
                                .Data = population@.Data,
                                metadata = population@metadata)
              population <- collapseDimension(population,
                                              margin = i.time.popn)
              population <- matrix(population@.Data,
                                   nrow = 1L,
                                   ncol = length(population@.Data),
                                   dimnames = list("population",
                                                   dimnames(population)[[1]]))
              signs <- character(length = length(components))
              for (i in seq_along(components)) {
                  component <- components[[i]]
                  name <- names.components[i]
                  i.time.comp <- match("time", dimtypes(component))
                  is.pos <- isPositiveIncrement(component)
                  is.internal <- methods::is(component, "Internal")
                  if (is.internal)
                      sign <- "."
                  else
                      sign <- if (is.pos) "+" else "-"
                  component <- collapseDimension(component,
                                                 margin = i.time.comp)
                  if (i == 1L)
                      colnames <- dimnames(component)[[1L]]
                  component <- as.integer(component)
                  components[[i]] <- component
                  names.components[i] <- name
                  signs[i] <- sign
              }
              max.char <- max(nchar(names.components))
              names.components <- sprintf("%*s (%s)", max.char, names.components, signs)
              components <- do.call(rbind, components)
              dimnames(components) <- list(names.components, colnames)
              cat("An object of class", class(object), "\n")
              showMetaData(metadata)
              cat("\n")
              print(population)
              cat("\n")
              print(components)
              cat("\n")
              cat("all cells consistent :", all(isConsistent(object)), "\n")
          })


