

#' @rdname collapseDimension
## NO_TESTS
setMethod("collapseDimension",
          signature(object = "Pool",
                    dimension = "ANY",
                    margin = "ANY",
                    weights = "missing"),
          function(object, dimension = NULL, margin = NULL, weights) {
              i.direction <- object@iDirection
              i.between <- object@iBetween
              names <- names(object)
              n.dim <- length(names)
              has.dimension <- !is.null(dimension)
              has.margin <- !is.null(margin)
              if (has.dimension) {
                  if (has.margin)
                      stop(gettextf("has '%s' and '%s' arguments", "dimension", "margin"))
                  else {
                      dimension <- tidySubscript(subscript = dimension, nDim = n.dim, names = names)
                      if (any(dimension == i.between))
                          stop(gettextf("attempt to collapse \"%s\" dimension of object of class \"%s\" (consider using function '%s' instead)",
                                        "between", class(object), "collapsePool"))
                      methods::callGeneric()
                  }
              }
              else {
                  if (has.margin) {
                      margin <- tidySubscript(subscript = margin, nDim = n.dim, names = names)
                      if (!any(margin == i.direction))
                          margin <- c(margin, i.direction)
                      methods::callGeneric()
                  }
                  else
                      stop(gettextf("no '%s' or '%s' arguments", "dimension", "margin"))
              }
          })


## incrementLowerTri ################################################################

## HAS_TESTS
## default method
## assume has age dimension
setMethod("incrementLowerTri",
          signature(component = "Component",
                    population = "Population"),
          function(component, population) {
              ans <- incrementLowerTriHelper(component)
              perm <- names(population)
              aperm(ans, perm = perm)
          })

## HAS_TESTS
setMethod("incrementLowerTri",
          signature(component = "BirthsMovements",
                    population = "Population"),
          function(component, population) {
              0L
          })

## HAS_TESTS
## assume has age dimension
setMethod("incrementLowerTri",
          signature(component = "InternalMovementsPool",
                    population = "Population"),
          function(component, population) {
              i.direction <- component@iDirection
              ins <- slab(component,
                          dimension = i.direction,
                          elements = "In")
              outs <- slab(component,
                           dimension = i.direction,
                           elements = "Out")
              ins <- incrementLowerTriHelper(ins)
              outs <- incrementLowerTriHelper(outs)
              ans <- ins - outs
              perm <- names(population)
              aperm(ans, perm = perm)
          })

## HAS_TESTS
setMethod("incrementLowerTri",
          signature(component = "InternalMovementsOrigDest",
                    population = "Population"),
          function(component, population) {
              ins <- collapseOrigDest(component,
                                      to = "in")
              outs <- collapseOrigDest(component,
                                       to = "out")
              ins <- incrementLowerTriHelper(ins)
              outs <- incrementLowerTriHelper(outs)
              ans <- ins - outs
              perm <- names(population)
              aperm(ans, perm = perm)
          })

## HAS_TESTS
setMethod("incrementLowerTri",
          signature(component = "ExitsMovements",
                    population = "Population"),
          function(component, population) {
              ans <- methods::callNextMethod()
              -1L * ans
          })


## incrementInteger ################################################################

## HAS_TESTS
setMethod("incrementInteger",
          signature(object = "Component"),
          function(object) {
              as.integer(object@.Data)
          })

## HAS_TESTS
setMethod("incrementInteger",
          signature(object = "BirthsMovements"),
          function(object) {
              dimtypes <- dimtypes(object,
                                   use.names = FALSE)
              i.parent <- grep("parent", dimtypes)
              has.parent <- length(i.parent) > 0L
              if (has.parent)
                  object <- collapseDimension(object,
                                              dimension = i.parent)
              dimtypes <- dimtypes(object,
                                   use.names = FALSE)
              i.age <- match("age", dimtypes, nomatch = 0L)
              has.age <- i.age > 0L
              if (has.age) {
                  i.triangle <- match("triangle", dimtypes)
                  object <- collapseDimension(object,
                                              dimension = c(i.age, i.triangle))
              }
              as.integer(object@.Data)
          })

## HAS_TESTS
setMethod("incrementInteger",
          signature(object = "InternalMovementsOrigDest"),
          function(object) {
              ans <- collapseOrigDest(object)
              as.integer(ans@.Data)
          })

## HAS_TESTS
setMethod("incrementInteger",
          signature(object = "InternalMovementsPool"),
          function(object) {
              i.direction <- object@iDirection              
              metadata <- object@metadata
              ins <- slab(object,
                          dimension = i.direction,
                          elements = "In",
                          drop = FALSE)
              outs <- slab(object,
                           dimension = i.direction,
                           elements = "Out",
                           drop = FALSE)
              ins <- as.integer(ins@.Data)
              outs <- as.integer(outs@.Data)
              ins - outs
          })


## incrementOpen ###################################################################

## HAS_TESTS
## default method
## assume has age dimension
setMethod("incrementOpen",
          signature(component = "Component",
                    population = "Population"),
          function(component, population) {
              ans <- incrementOpenHelper(component)
              perm <- names(population)
              aperm(ans, perm = perm)
          })

## HAS_TESTS
setMethod("incrementOpen",
          signature(component = "BirthsMovements",
                    population = "Population"),
          function(component, population) {
              0L
          })

## HAS_TESTS
## assume has age dimension
setMethod("incrementOpen",
          signature(component = "InternalMovementsPool",
                    population = "Population"),
          function(component, population) {
              i.direction <- component@iDirection
              ins <- slab(component,
                          dimension = i.direction,
                          elements = "In")
              outs <- slab(component,
                           dimension = i.direction,
                           elements = "Out")
              ins <- incrementOpenHelper(ins)
              outs <- incrementOpenHelper(outs)
              ans <- ins - outs
              perm <- names(population)
              aperm(ans, perm = perm)
          })

## HAS_TESTS
setMethod("incrementOpen",
          signature(component = "InternalMovementsOrigDest",
                    population = "Population"),
          function(component, population) {
              ins <- collapseOrigDest(component,
                                      to = "in")
              outs <- collapseOrigDest(component,
                                       to = "out")
              ins <- incrementOpenHelper(ins)
              outs <- incrementOpenHelper(outs)
              ans <- ins - outs
              perm <- names(population)
              aperm(ans, perm = perm)
          })

## HAS_TESTS
setMethod("incrementOpen",
          signature(component = "ExitsMovements",
                    population = "Population"),
          function(component, population) {
              ans <- methods::callNextMethod()
              -1L * ans
          })



## incrementSquare ################################################################

## HAS_TESTS
## default method
## assume no age dimension
setMethod("incrementSquare",
          signature(component = "Component",
                    population = "Population"),
          function(component, population) {
              ans <- incrementSquareHelper(component)
              perm <- names(population)
              aperm(ans, perm = perm)
          })


## HAS_TESTS
setMethod("incrementSquare",
          signature(component = "BirthsMovements",
                    population = "Population"),
          function(component, population) {
              names.popn <- names(population)
              dimtypes <- dimtypes(component,
                                       use.names = FALSE)
              i.parent <- match("parent", dimtypes, nomatch = 0L)
              has.parent <- i.parent > 0L
              if (has.parent)
                  component <- collapseDimension(component,
                                                 dimension = i.parent)
              ans <- incrementSquareHelper(component)
              aperm(ans,
                    perm = names.popn)
          })


## HAS_TESTS
## assume has age dimension
setMethod("incrementSquare",
          signature(component = "InternalMovementsPool",
                    population = "Population"),
          function(component, population) {
              i.direction <- component@iDirection
              ins <- slab(component,
                          dimension = i.direction,
                          elements = "In")
              outs <- slab(component,
                           dimension = i.direction,
                           elements = "Out")
              ins <- incrementSquareHelper(ins)
              outs <- incrementSquareHelper(outs)
              ans <- ins - outs
              perm <- names(population)
              aperm(ans, perm = perm)
          })

## HAS_TESTS
setMethod("incrementSquare",
          signature(component = "InternalMovementsOrigDest",
                    population = "Population"),
          function(component, population) {
              ins <- collapseOrigDest(component,
                                      to = "in")
              outs <- collapseOrigDest(component,
                                       to = "out")
              ins <- incrementSquareHelper(ins)
              outs <- incrementSquareHelper(outs)
              ans <- ins - outs
              perm <- names(population)
              aperm(ans, perm = perm)
          })

## HAS_TESTS
setMethod("incrementSquare",
          signature(component = "ExitsMovements",
                    population = "Population"),
          function(component, population) {
              ans <- methods::callNextMethod()
              -1L * ans
          })


## incrementUpperTri ###################################################################

## HAS_TESTS
## default method
## assume has age dimension
setMethod("incrementUpperTri",
          signature(component = "Component",
                    population = "Population"),
          function(component, population) {
              ans <- incrementUpperTriHelper(component)
              perm <- names(population)
              aperm(ans, perm = perm)
          })

## HAS_TESTS
setMethod("incrementUpperTri",
          signature(component = "BirthsMovements",
                    population = "Population"),
          function(component, population) {
              names.popn <- names(population)
              dimtypes.comp <- dimtypes(component,
                                        use.names = FALSE)
              dimtypes.popn <- dimtypes(population,
                                        use.names = FALSE)
              i.age.comp <- match("age", dimtypes.comp, nomatch = 0L)
              i.triangle.comp <- match("triangle", dimtypes.comp, nomatch = 0L)
              i.parent.comp <- match("parent", dimtypes.comp, nomatch = 0L)
              has.age.comp <- i.age.comp > 0L
              has.triangle.comp <- i.triangle.comp > 0L
              has.parent.comp <- i.parent.comp > 0L
              if (has.age.comp || has.triangle.comp || has.parent.comp) {
                  dimension <- c(i.age.comp, i.triangle.comp, i.parent.comp)
                  dimension <- dimension[dimension != 0L]
                  component <- collapseDimension(component,
                                           dimension = dimension)
              }
              i.age.popn <- match("age", dimtypes.popn)
              name.age <- names.popn[i.age.popn]
              component <- addDimension(component,
                                        name = name.age,
                                        labels = "0",
                                        dimtype = "age",
                                        dimscale = "Points")
              aperm(component,
                    perm = names.popn)
          })

## HAS_TESTS
## assume has age dimension
setMethod("incrementUpperTri",
          signature(component = "InternalMovementsPool",
                    population = "Population"),
          function(component, population) {
              i.direction <- component@iDirection
              ins <- slab(component,
                          dimension = i.direction,
                          elements = "In")
              outs <- slab(component,
                           dimension = i.direction,
                           elements = "Out")
              ins <- incrementUpperTriHelper(ins)
              outs <- incrementUpperTriHelper(outs)
              ans <- ins - outs
              perm <- names(population)
              aperm(ans, perm = perm)
          })

## HAS_TESTS
setMethod("incrementUpperTri",
          signature(component = "InternalMovementsOrigDest",
                    population = "Population"),
          function(component, population) {
              ins <- collapseOrigDest(component,
                                      to = "in")
              outs <- collapseOrigDest(component,
                                       to = "out")
              ins <- incrementUpperTriHelper(ins)
              outs <- incrementUpperTriHelper(outs)
              ans <- ins - outs
              perm <- names(population)
              aperm(ans, perm = perm)
          })

## HAS_TESTS
setMethod("incrementUpperTri",
          signature(component = "ExitsMovements",
                    population = "Population"),
          function(component, population) {
              ans <- methods::callNextMethod()
              -1L * ans
          })


## isCompatibleWithPopn #####################################################3

## HAS_TESTS
setMethod("isCompatibleWithPopn",
          signature(component = "MovementsComponent",
                    metadata = "MetaData",
                    name = "character"),
          function(component, metadata, name) {
              metadata.comp <- metadata(component)
              if (!isTRUE(all.equal(metadata.comp, metadata)))
                  gettextf("'%s' not compatible with '%s'",
                           name, "population")
              else
                  TRUE
          })

## HAS_TESTS
setMethod("isCompatibleWithPopn",
          signature(component = "TransitionsComponent",
                    metadata = "MetaData",
                    name = "character"),
          function(component, metadata, name) {
              metadata.comp <- metadata(component)
              metadata.comp <- removePairFromMetadata(metadata.comp,
                                                      origDest = TRUE)
              if (!isTRUE(all.equal(metadata.comp, metadata)))
                  gettextf("'%s' not compatible with '%s'",
                           name, "population")
              else
                  TRUE
          })

## HAS_TESTS
setMethod("isCompatibleWithPopn",
          signature(component = "BirthsMovements",
                    metadata = "MetaData",
                    name = "character"),
          function(component, metadata, name) {
              metadata.comp <- metadata(component)
              metadata.comp <- removePairFromMetadata(metadata.comp,
                                                      origDest = FALSE)
              metadata.comp <- removeDimtypesFromMetadata(metadata.comp,
                                                          dimtypes = c("age", "triangle"))
              metadata <- removeDimtypesFromMetadata(metadata,
                                                     dimtypes = c("age", "triangle"))
              if (!isTRUE(all.equal(metadata.comp, metadata)))
                  return(gettextf("'%s' not compatible with '%s'",
                                  name, "population"))
              else
                  TRUE
          })

## HAS_TESTS
setMethod("isCompatibleWithPopn",
          signature(component = "BirthsTransitions",
                    metadata = "MetaData",
                    name = "character"),
          function(component, metadata, name) {
              metadata.comp <- metadata(component)
              metadata.comp <- removePairFromMetadata(metadata.comp,
                                                      origDest = TRUE)
              metadata.comp <- removePairFromMetadata(metadata.comp,
                                                      origDest = FALSE)
              metadata.comp <- removeDimtypesFromMetadata(metadata.comp,
                                                          dimtypes = c("age", "triangle"))
              metadata <- removeDimtypesFromMetadata(metadata,
                                                     dimtypes = c("age", "triangle"))
              if (!isTRUE(all.equal(metadata.comp, metadata)))
                  return(gettextf("'%s' not compatible with '%s'",
                                  name, "population"))
              else
                  TRUE
          })

## HAS_TESTS
setMethod("isCompatibleWithPopn",
          signature(component = "HasOrigDest",
                    metadata = "MetaData",
                    name = "character"),
          function(component, metadata, name) {
              metadata.comp <- metadata(component)
              metadata.comp <- removePairFromMetadata(metadata.comp,
                                                      origDest = TRUE)
              if (!isTRUE(all.equal(metadata.comp, metadata)))
                  return(gettextf("'%s' not compatible with '%s'",
                                  name, "population"))
              else
                  TRUE
          })

## HAS_TESTS
setMethod("isCompatibleWithPopn",
          signature(component = "InternalMovementsPool",
                    metadata = "MetaData",
                    name = "character"),
          function(component, metadata, name) {
              i.direction <- component@iDirection
              metadata.comp <- metadata(component)
              if (!isTRUE(all.equal(metadata.comp[-i.direction], metadata)))
                  return(gettextf("'%s' not compatible with '%s'",
                                  name, "population"))
              else
                  TRUE
          })


## isPositiveIncrement #############################################################

## HAS_TESTS
setMethod("isPositiveIncrement",
          signature(object = "Births"),
          function(object) {
              TRUE
          })

## HAS_TESTS
setMethod("isPositiveIncrement",
          signature(object = "Internal"),
          function(object) {
              TRUE
          })

## HAS_TESTS
setMethod("isPositiveIncrement",
          signature(object = "Entries"),
          function(object) {
              TRUE
          })

## HAS_TESTS
setMethod("isPositiveIncrement",
          signature(object = "Exits"),
          function(object) {
              FALSE
          })

## HAS_TESTS
setMethod("isPositiveIncrement",
          signature(object = "NetMovements"),
          function(object) {
              TRUE
          })


## midpoints ###########################################################

## HAS_TESTS
#' @rdname midpoints
#' @export
setMethod("midpoints",
          signature(object = "Component", dimension = "ANY"),
          function(object, dimension) {
              object <- as(object, "Counts")
              callGeneric()
          })

## HAS_TESTS
#' @rdname midpoints
#' @export
setMethod("midpoints",
          signature(object = "Component", dimension = "missing"),
          function(object) {
              object <- as(object, "Counts")
              callGeneric()
          })


## slab ################################################################

## HAS_TESTS
#' @rdname slab
#' @export
setMethod("slab",
          signature(object = "Component"),
          function(object, dimension, elements, drop = TRUE) {
              ans <- callNextMethod()
              new(class(object),
                  .Data = ans@.Data,
                  metadata = ans@metadata)
          })

## HAS_TESTS
#' @rdname slab
#' @export
setMethod("slab",
          signature(object = "Births"),
          function(object, dimension, elements, drop = TRUE) {
              class <- class(object)
              iMinAge <- object@iMinAge
              object <- new("Counts",
                            .Data = object@.Data,
                            metadata = object@metadata)
              ans <- callGeneric()
              new(class,
                  .Data = ans@.Data,
                  metadata = ans@metadata,
                  iMinAge = iMinAge)
          })

## HAS_TESTS
#' @rdname slab
#' @export
setMethod("slab",
          signature(object = "InternalMovementsPool"),
          function(object, dimension, elements, drop = TRUE) {
              names <- names(object)
              class <- class(object)
              iBetween <- object@iBetween
              iDirection <- object@iDirection
              object <- new("Counts",
                            .Data = object@.Data,
                            metadata = object@metadata)
              dimension <- tidySubscript(subscript = dimension,
                                         nDim = length(names),
                                         names = names)
              ans <- callGeneric()
              if (identical(dimension, iDirection))
                  ans
              else
                  new(class,
                      .Data = ans@.Data,
                      metadata = ans@metadata,
                      iBetween = iBetween,
                      iDirection = iDirection)
          })


## setMethod("addToPopnEnd",
##           signature(object = "Births"),
##           function(object, population) {
##               kCollapse <- c("triangle", "age", "parent", "origin")
##               i.collapse <- which(dimtypes(object) %in% kCollapse)
##               object <- collapseDimension(object, dimension = i.collapse)
##               i.age.popn <- match("age", dimtypes(population))
##               name.age <- names(population)[i.age.popn]
##               DimScale.age <- DimScales(population)[[i.age.popn]]
##               names.ans <- c(names(object), name.age)
##               dimtypes.ans <- c(dimtypes(object, use.names = FALSE), dimtype.age)
##               DimScales.ans <- c(DimScales(object, use.names = FALSE),
##                                  list(DimScale.age))
##               metadata.ans <- methods::new("MetaData",
##                                   nms = names.ans,
##                                   dimtypes = dimtypes.ans,
##                                   DimScales = DimScales.ans)
##               .Data.ans <- array(0L,
##                                  dim = dim(metadata.ans),
##                                  dimnames = dimnames(metadata.ans))
##               i.age.ans <- length(dim(.Data.ans))
##               i.first.age <- slice.index(.Data.ans, MARGIN = i.age.ans) == 1L
##               .Data.ans[i.first.age] <- as.integer(object)
##               methods::new("Counts", .Data = .Data.ans, metadata = metadata.ans)
##           })

## setMethod("subtractFromPopnEnd",
##           signature(object = "Births"),
##           function(object, population) {
##               0L
##           })

## makeDimScaleTimePopnEnd <- function(object) {
##     dimtypes <- dimtypes(object, use.names = FALSE)
##     DimScales <- DimScales(object, use.names = FALSE)
##     i.time <- match("time", dimtypes)
##     DimScale.time <- DimScales[[i.time]]
##     dv.time <- dimvalues(DimScale.time)
##     dv.time <- dv.time[-1L]
##     methods::new("Points", dimvalues = dv.time)
## }
    
    
## setMethod("addToPopnEnd",
##           signature(object = "InternalMovementsNet"),
##           function(object, population) {
##               .Data.obj <- object@.Data
##               names <- names(object)
##               dimtypes <- dimtypes(object, use.names = FALSE)
##               DimScales.obj <- DimScales(object, use.names = FALSE)
##               i.time <- match("time", dimtypes)
##               DimScale.time.obj <- DimScales.obj[[i.time]]
##               dv.time.obj <- dimvalues(DimScale.time.obj)
##               dv.time.ans <- dv.time.obj[-1L]
##               DimScale.time.ans <- methods::new("Points", dimvalues = dv.time.ans)
##               DimScales.ans <- replace(DimScales.obj,
##                                        list = i.time,
##                                        values = DimScale.time.ans)
##               has.age <- "age" %in% dimtypes
##               if (has.age) {
##               i.triangle <- match("triangle", dimtypes)
##               DimScales.ans <- DimScales.ans[-i.triangle]
##               metadata.ans <- methods::new("MetaData",
##                                   nms = names,
##                                   dimtypes = dimtypes,
##                                   DimScales = DimScales.ans)
##               .Data.ans <- array(0L,
##                                  dim = dim(metadata.ans),
##                                  dimnames = dimnames(metadata.ans))
##               is.lower <- slice.index(object, MARGIN = i.triangle) == 1L
##               .Data.ans <- .Data.obj[i.lower]
##               .Data.ans <- .Data.ans +

## setMethod("addToPopnEnd",
##           signature(object = "InternalTransitions"),
##           function(object, population) {
##               ans <- collapseOrigDest(object, to = "in")
##               ans <- timeIntervalsToEndPoints(ans)
##               ans <- ageForward(ans)
##               ans
##           })
              
              
    



## setMethod("increments",
##           signature(object = "Births"),
##           function(object) {
##               0L
##           })

## setMethod("increments",
##           signature(object = "EntriesMovements"),
##           function(object) {
##               dimtypes <- dimtypes(object, use.names = FALSE)
##               i.triangle <- match("triangle", dimtypes)
##               collapseDimension(object, dimension = i.triangle)
##           })

## setMethod("increments",
##           signature(object = "ExitsMovements"),
##           function(object) {
##               dimtypes <- dimtypes(object, use.names = FALSE)
##               i.triangle <- match("triangle", dimtypes)
##               -1 * collapseDimension(object, dimension = i.triangle)
##           })

## ## setMethod("increments",
## ##           signature(object = "EntriesTransitions"),
## ##           function(object) {
## ##               collapseOrigDest(object, to = "net")
## ##           })

## ## setMethod("increments",
## ##           signature(object = "ExitsTransitions"),
## ##           function(object) {
## ##               collapseOrigDest(object, to = "net")
## ##           })


## accessionToDecession <- function(object) {
##     .Data.old <- object@.Data
##     metadata <- object@metadata
##     dim <- dim(.Data.old)
##     dimtypes <- dimtypes(object, use.names = FALSE)
##     i.age <- match("age", dimtypes)
##     n.age <- dim[i.age]
##     .Data.new <- array(0L,
##                        dim = dim(metadata),
##                        dimnames = dimnames(metadata))
##     i.old <- slice.index(.Data.old, MARGIN = i.age) != 1L
##     i.new <- slice.index(.Data.old, MARGIN = i.age) != n.age
##     .Data.new[i.new] <- .Data.old[i.old]
##     methods::new("Counts", .Data = .Data.new, metadata = metadata)
## }
        


              
## ## setMethod("makeIncrements",
## ##           signature(object = "BirthsMovement"),
## ##           function(object, population) {
## ##               transform <- getTransform(object)
## ##               object <- collapse(object, transform = transform)
## ##               i.time <- match("time", dimtypes(population))
## ##               i.age <- match("age", dimtypes(population), nomatch = 0L)
## ##               has.age <- i.age > 0L
## ##               if (has.age) {
## ##                   names <- names(population)
## ##                   dimtypes <- dimtypes(population)
## ##                   DimScales <- replace(DimScales(population),
## ##                                        list = i.time,
## ##                                        values = DimScales(object)[[i.time]])
## ##                   metadata <- methods::new("MetaData",
## ##                                   nms = names,
## ##                                   dimtypes = dimtypes,
## ##                                   DimScales = DimScales)
## ##                   .Data <- array(0L,
## ##                                  dim = dim(metadata),
## ##                                  dimnames = dimnames(metadata))
## ##                   .Data[slice.index(.Data, MARGIN = i.age) == 1L] <- object
## ##                   object <- methods::new("Counts", .Data = .Data, metadata = metadata)
## ##               }
## ##               object
## ##           })

                        
          
              
              
