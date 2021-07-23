
## HAS_TESTS
#' @rdname accession
setMethod("accession",
          signature(object = "Movements"),
          function(object, births = TRUE, openAge = FALSE) {
              if (!is.logical(births))
                  stop(gettextf("'%s' does not have type \"%s\"",
                                "births", "logical"))
              if (!identical(length(births), 1L))
                  stop(gettextf("'%s' does not have length %d",
                                "births", 1L))
              if (is.na(births))
                  stop(gettextf("'%s' is missing",
                                "births"))
              if (!is.logical(openAge))
                  stop(gettextf("'%s' does not have type \"%s\"",
                                "openAge", "logical"))
              if (!identical(length(openAge), 1L))
                  stop(gettextf("'%s' does not have length %d",
                                "openAge", 1L))
              if (is.na(openAge))
                  stop(gettextf("'%s' is missing",
                                "openAge"))
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
              ans <- agePopnForwardUpperTri(population = population,
                                            openAge = openAge)
              is.births <- sapply(components, methods::is, "Births")
              for (component in components[!is.births]) {
                  increment.upper.tri <- incrementUpperTri(component = component,
                                                           population = population,
                                                           openAge = openAge)
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
#' @rdname makeConsistent
setMethod("makeConsistent",
          signature(object = "Movements"),
          function(object, adjust = TRUE, scale = 0.1, fixed = character()) {
              checkAdjustAndScale(adjust = adjust,
                                  scale = scale)
              population <- object@population
              components <- object@components
              namesComponents <- object@namesComponents
              iFixed <- pmatch(x = fixed,
                               table = namesComponents,
                               nomatch = NA)
              i.unmatched <- match(NA, iFixed, nomatch = 0L)
              if (i.unmatched > 0L)
                  stop(gettextf("'%s' has element [\"%s\"] that is not the name of a component",
                                "fixed", fixed[i.unmatched]))
              if (identical(length(fixed), length(components)))
                  stop(gettextf("'%s' includes every component",
                                "fixed"))
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
                                       scale = scale,
                                       iFixed = iFixed)
              else
                  derivePopnMoveNoAge(object = object,
                                      adjust = adjust,
                                      scale = scale,
                                      iFixed = iFixed)
          })

## HAS_TESTS
#' @rdname isConsistent
setMethod("isConsistent",
          signature(object = "Movements"),
          function(object) {
              population <- object@population
              components <- object@components
              metadata <- metadata(object)
              dimtypes <- dimtypes(population,
                                   use.names = FALSE)
              i.age <- match("age", dimtypes, nomatch = 0L)
              has.age <- i.age > 0L
              i.time <- match("time", dimtypes)
              n.time.popn <- dim(population)[i.time]
              s.time.popn <- seq.int(from = 2L, to = n.time.popn)
              ## population change consistent with
              ## increments/decrements
              if (has.age)
                  popn.end.obtained <- popnEndWithAge(object)
              else
                  popn.end.obtained <- popnEndNoAge(object)
              elements <- seq.int(from = 2L, to = n.time.popn)
              popn.end.expected <- slab(population,
                                        dimension = i.time,
                                        elements = s.time.popn)
              ans <- popn.end.obtained == popn.end.expected
              if (has.age) {
                  ## accession non-negative (including oldest age group)
                  accession <- accession(object = object,
                                         births = FALSE, 
                                         openAge = TRUE)
                  is.acc.nonneg <- accession >= 0L
                  is.acc.nonneg <- as.integer(is.acc.nonneg)
                  ans <- ans & is.acc.nonneg
                  ## population of oldest age group as least
                  ## as large as final accession
                  n.age <- dim(population)[i.age]
                  popn.last <- slab(population,
                                    dimension = i.age,
                                    elements = n.age,
                                    drop = FALSE)
                  popn.last <- slab(popn.last,
                                    dimension = i.time,
                                    elements = s.time.popn,
                                    drop = FALSE)
                  popn.last <- as.integer(popn.last)
                  acc.last <- slab(accession,
                                   dimension = i.age,
                                   elements = n.age,
                                   drop = FALSE)
                  acc.last <- as.integer(acc.last)
                  is.popn.ge.acc.last <- popn.last >= acc.last
                  is.popn.ge.acc <- array(TRUE,
                                          dim = dim(ans),
                                          dimnames(ans))
                  index.last <- slice.index(is.popn.ge.acc, MARGIN = i.age) == n.age
                  is.popn.ge.acc[index.last] <- is.popn.ge.acc.last
                  ans <- ans & is.popn.ge.acc 
              }
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
              has_iter <- "iteration" %in% dimtypes(population)
              population <- new("Counts",
                                .Data = population@.Data,
                                metadata = population@metadata)
              if (has_iter) {
                  population <- collapseIterations(population, FUN = mean)
                  population <- toInteger(population, force = TRUE)
              }
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
                  if (has_iter) {
                      component <- as(component, "Counts")
                      component <- collapseIterations(component, FUN = mean)
                      component <- toInteger(component, force = TRUE)
                  }
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


