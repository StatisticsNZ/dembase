
## HAS_TESTS
## default method
setMethod("accessionComponent",
          signature(component = "Movements", population = "Population"),
          function(component, population) {
              dimtypes <- dimtypes(component, use.names = FALSE)
              has.age <- "age" %in% dimtypes
              if (has.age) {
                  ans <- accessionHelper(component)
                  perm <- names(population)
                  aperm(ans, perm = perm)
              }
              else
                  NULL
          })

## HAS_TESTS
setMethod("accessionComponent",
          signature(component = "BirthsMovements",
                    population = "Population"),
          function(component, population) {
              dimtypes.comp <- dimtypes(component, use.names = FALSE)
              i.age.comp <- match("age", dimtypes.comp, nomatch = 0L)
              has.age <- i.age.comp > 0L
              if (has.age) {
                  DimScales.comp <- DimScales(component, use.names = FALSE)
                  i.triangle.comp <- match("triangle", dimtypes.comp, nomatch = 0L)
                  i.parent.comp <- match("parent", dimtypes.comp, nomatch = 0L)
                  i.time.comp <- match("time", dimtypes.comp, nomatch = 0L)
                  DS.time.comp <- DimScales.comp[[i.time.comp]]
                  names.popn <- names(population)
                  dimtypes.popn <- dimtypes(population, use.names = FALSE)
                  DimScales.popn <- DimScales(population, use.names = FALSE)
                  i.age.popn <- match("age", dimtypes.popn)
                  i.time.popn <- match("time", dimtypes.popn)
                  DimScales.ans <- replace(DimScales.popn,
                                           list = i.time.popn,
                                           values = DS.time.comp)
                  metadata.ans <- methods::new("MetaData",
                                      nms = names.popn,
                                      dimtypes = dimtypes.popn,
                                      DimScales = DimScales.ans)
                  dim.ans <- dim(metadata.ans)
                  dimnames.ans <- dimnames(metadata.ans)
                  .Data.ans <- array(0L, dim = dim.ans, dimnames = dimnames.ans)
                  dimension <- c(i.age.comp, i.triangle.comp, i.parent.comp)
                  dimension <- dimension[dimension > 0L]
                  ans.no.age <- collapseDimension(component, dimension = dimension)
                  perm <- names.popn[-i.age.popn]
                  ans.no.age <- aperm(ans.no.age, perm = perm)
                  is.first.age <- slice.index(.Data.ans, MARGIN = i.age.popn) == 1L
                  .Data.ans[is.first.age] <- as.integer(ans.no.age)
                  methods::new("Counts", .Data = .Data.ans, metadata = metadata.ans)
              }
              else
                  NULL
          })

## HAS_TESTS
setMethod("accessionComponent",
          signature(component = "InternalMovementsPool",
                    population = "Population"),
          function(component, population) {
              dimtypes.comp <- dimtypes(component, use.names = FALSE)
              i.age.comp <- match("age", dimtypes.comp, nomatch = 0L)
              has.age <- i.age.comp > 0L
              if (has.age) {
                  i.direction <- component@iDirection
                  ins <- slab(component, dimension = i.direction, elements = 1L)
                  outs <- slab(component, dimension = i.direction, elements = 2L)
                  ins <- accessionHelper(ins)
                  outs <- accessionHelper(outs)
                  ans <- ins - outs
                  perm <- names(population)
                  aperm(ans, perm = perm)
              }
              else
                  NULL
          })

## HAS_TESTS
setMethod("accessionComponent",
          signature(component = "InternalMovementsOrigDest",
                    population = "Population"),
          function(component, population) {
              dimtypes.comp <- dimtypes(component, use.names = FALSE)
              i.age.comp <- match("age", dimtypes.comp, nomatch = 0L)
              has.age <- i.age.comp > 0L
              if (has.age) {
                  ins <- collapseOrigDest(component, to = "in")
                  outs <- collapseOrigDest(component, to = "out")
                  ins <- accessionHelper(ins)
                  outs <- accessionHelper(outs)
                  ans <- ins - outs
                  perm <- names(population)
                  aperm(ans, perm = perm)
              }
              else
                  NULL
          })

## NO_TESTS
setMethod("accessionComponent",
          signature(component = "ExitsMovements",
                    population = "Population"),
          function(component, population) {
              ans <- methods::callNextMethod()
              if (is.null(ans))
                  ans
              else
                  -1L * ans
          })

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

                        
          
              
              
