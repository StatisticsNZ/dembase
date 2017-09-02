
## Do not do validity checking because
## accession is calculated from inputs
## that should already have been checked.
## Accession class itself has lots of
## checks.
Accession <- function(accession) {
    methods::new("Accession",
        .Data = accession@.Data,
        metadata = accession@metadata)
}

## HAS_TESTS
BirthsMovements <- function(births, template) {
    if (!methods::is(births, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "births", class(births)))
    births <- checkAndTidyComponent(births,
                                    name = "births",
                                    requireInteger = TRUE,
                                    allowNegatives = FALSE,
                                    allowOrig = FALSE,
                                    allowParent = TRUE,
                                    allowTriangles = TRUE,
                                    triangles = TRUE)
    dim.bth <- dim(births)
    names.bth <- names(births)
    dimtypes.bth <- dimtypes(births, use.names = FALSE)
    dimtypes.tpt <- dimtypes(template, use.names = FALSE)
    DimScales.bth <- DimScales(births, use.names = FALSE)
    DimScales.tpt <- DimScales(template, use.names = FALSE)
    has.age.bth <- "age" %in% dimtypes.bth
    has.age.tpt <- "age" %in% dimtypes.tpt
    has.parent.bth <- "parent" %in% dimtypes.bth
    if (has.age.bth && has.age.tpt) {
        template.trimmed <- tryCatch(trimAgeIntervalsToMatch(x = template, y = births),
                                     error = function(e) e)
        if (methods::is(template.trimmed, "error"))
            stop(gettextf("'%s' is incompatible with '%s' : %s",
                          "births", "population", template.trimmed$message))
        i.min.age <- iMinAge(current = births, target = template)
        template <- template.trimmed
    }
    else if (!has.age.bth && has.age.tpt) {
        template.trimmed <- trimAgeIntervalsTo1549(template)
        i.age <- match("age", dimtypes.tpt)
        i.triangle <- match("triangle", dimtypes.tpt)
        DimScales.tpt.trim <- DimScales(template.trimmed, use.names = FALSE)
        DimScale.age <- DimScales.tpt.trim[[i.age]]
        DimScale.triangle <- DimScales.tpt[[i.triangle]]
        .Data.births <- births@.Data
        n.extra <- length(DimScale.age) * 2L
        prob <- rep(1L, times = n.extra)
        FUN <- function(x) stats::rmultinom(n = 1L, size = x, prob = prob)
        .Data.births <- vapply(.Data.births,
                               FUN = FUN,
                               FUN.VALUE = prob)
        .Data.births <- t(.Data.births)
        names <- make.unique(c(names.bth, "age", "triangle"))
        dimtypes <- c(dimtypes.bth, c("age", "triangle"))
        DimScales <- c(DimScales.bth, list(DimScale.age, DimScale.triangle))
        metadata.births <- new("MetaData",
                               nms = names,
                               dimtypes = dimtypes,
                               DimScales = DimScales)
        .Data.births <- array(.Data.births,
                              dim = dim(metadata.births),
                              dimnames = dimnames(metadata.births))
        births <- new("Counts",
                      .Data = .Data.births,
                      metadata = metadata.births)
        i.min.age <- iMinAge(current = births, target = template)
        template <- template.trimmed
    }
    else
        i.min.age <- NA_integer_
    if (has.parent.bth)
        births <- tryCatch(makeOrigDestParentChildCompatible(x = births,
                                                             y = template,
                                                             subset = TRUE,
                                                             check = TRUE),
                           error = function(e) e)
    else
        births <- tryCatch(makeCompatible(x = births,
                                          y = template,
                                          subset = TRUE,
                                          check = TRUE),
                           error = function(e) e)
    if (methods::is(births, "error"))
        stop(gettextf("'%s' is incompatible with '%s' : %s",
                      "births", "population", births$message))
    if (has.parent.bth)
        class <- "BirthsMovementsHasParentChild"
    else
        class <- "BirthsMovementsNoParentChild"
    methods::new(class,
                 .Data = births@.Data,
                 metadata = births@metadata,
                 iMinAge = i.min.age)
}

## HAS_TESTS
BirthsTransitions <- function(births, template) {
    if (!methods::is(births, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "births", class(births)))
    births <- checkAndTidyComponent(births,
                                    name = "births",
                                    requireInteger = TRUE,
                                    allowNegatives = FALSE,
                                    allowOrig = TRUE,
                                    allowParent = TRUE,
                                    allowTriangles = FALSE,
                                    triangles = FALSE)
    dimtypes.bth <- dimtypes(births, use.names = FALSE)
    dimtypes.tpt <- dimtypes(template, use.names = FALSE)
    has.age.bth <- "age" %in% dimtypes.bth
    has.age.tpt <- "age" %in% dimtypes.tpt
    has.parent.bth <- "parent" %in% dimtypes.bth
    if (has.age.bth && has.age.tpt) {
        template.trimmed <- tryCatch(trimAgeIntervalsToMatch(x = template, y = births),
                                     error = function(e) e)
        if (methods::is(template.trimmed, "error"))
            stop(gettextf("'%s' is incompatible with '%s' : %s",
                          "births", "population", template.trimmed$message))
        i.min.age <- iMinAge(current = births, target = template)
        template <- template.trimmed
    }
    else
        i.min.age <- NA_integer_
    births <- tryCatch(makeOrigDestParentChildCompatible(x = births,
                                                         y = template,
                                                         subset = TRUE,
                                                         check = TRUE),
                       error = function(e) e)
    if (methods::is(births, "error"))
        stop(gettextf("'%s' is incompatible with '%s' : %s",
                      "births", "population", births$message))
    if (has.parent.bth)
        class <- "BirthsTransitionsHasParentChild"
    else
        class <- "BirthsTransitionsNoParentChild"
    methods::new(class,
                 .Data = births@.Data,
                 metadata = births@metadata,
                 iMinAge = i.min.age)
}

## HAS_TESTS
EntriesMovements <- function(entries, template, name) {
    if (!methods::is(entries, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(entries)))
    entries <- checkAndTidyComponent(object = entries,
                                     name = name,
                                     requireInteger = TRUE,
                                     allowNegatives = FALSE,
                                     allowOrig = FALSE,
                                     allowParent = FALSE,
                                     allowTriangles = TRUE,
                                     triangles = TRUE)
    entries <- tryCatch(makeCompatible(x = entries,
                                       y = template,
                                       subset = TRUE,
                                       check = TRUE),
                        error = function(e) e)
    if (methods::is(entries, "error"))
        stop(gettextf("'%s' is incompatible with '%s' : %s",
                      name, "population", entries$message))
    methods::new("EntriesMovements",
                 .Data = entries@.Data,
                 metadata = entries@metadata)
}

## HAS_TESTS
EntriesTransitions <- function(entries, template, name) {
    if (!methods::is(entries, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(entries)))
    entries <- checkAndTidyComponent(object = entries,
                                     name = name,
                                     requireInteger = TRUE,
                                     allowNegatives = FALSE,
                                     allowOrig = TRUE,
                                     allowParent = FALSE,
                                     allowTriangles = FALSE,
                                     triangles = FALSE)
    entries <- tryCatch(makeOrigDestParentChildCompatible(x = entries,
                                                          y = template,
                                                          subset = TRUE,
                                                          check = TRUE),
                        error = function(e) e)
    if (methods::is(entries, "error"))
        stop(gettextf("'%s' is incompatible with '%s' : %s",
                      name, "population", entries$message))
    methods::new("EntriesTransitions",
                 .Data = entries@.Data,
                 metadata = entries@metadata)
}


## HAS_TESTS
ExitsMovements <- function(exits, template, name) {
    if (!methods::is(exits, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(exits)))
    exits <- checkAndTidyComponent(object = exits,
                                   name = name,
                                   requireInteger = TRUE,
                                   allowNegatives = FALSE,
                                   allowOrig = FALSE,
                                   allowParent = FALSE,
                                   allowTriangles = TRUE,
                                   triangles = TRUE)
    exits <- tryCatch(makeCompatible(x = exits,
                                     y = template,
                                     subset = TRUE,
                                     check = TRUE),
                      error = function(e) e)
    if (methods::is(exits, "error"))
        stop(gettextf("'%s' is incompatible with '%s' : %s",
                      name, "population", exits$message))
    methods::new("ExitsMovements",
                 .Data = exits@.Data,
                 metadata = exits@metadata)
}

## Do not do validity checking because
## accession is calculated from inputs
## that should already have been checked.
## Exposure class itself has lots of
## checks.
Exposure <- function(exposure) {
    methods::new("Exposure",
                 .Data = exposure@.Data,
                 metadata = exposure@metadata)
}

## HAS_TESTS
setMethod("InternalMovements",
          signature(internal = "Counts"),
          function(internal, template) {
              dimtypes <- dimtypes(internal, use.names = FALSE)
              is.orig <- dimtypes == "origin"
              if (!any(is.orig))
                  stop(gettextf("'%s' does not have class \"%s\" or \"%s\" and does not have dimensions with dimtype \"%s\" or \"%s\"",
                                "internal", "Net", "Pool", "origin", "destination"))
              internal <- checkAndTidyComponent(object = internal,
                                                name = "internal",
                                                requireInteger = TRUE,
                                                allowNegatives = FALSE,
                                                allowOrig = TRUE,
                                                allowParent = FALSE,
                                                allowTriangles = TRUE,
                                                triangles = TRUE)
              internal <- tryCatch(makeOrigDestParentChildCompatible(x = internal,
                                                                     y = template,
                                                                     subset = TRUE,
                                                                     check = TRUE),
                                   error = function(e) e)
              if (methods::is(internal, "error"))
                  stop(gettextf("'%s' is incompatible with '%s' : %s",
                                "internal", "population", internal$message))
              methods::new("InternalMovementsOrigDest",
                           .Data = internal@.Data,
                           metadata = internal@metadata)
          })

## HAS_TESTS
setMethod("InternalMovements",
          signature(internal = "Pool"),
          function(internal, template) {
              i.between <- internal@iBetween
              i.direction <- internal@iDirection
              names.between <- names(internal)[i.between]
              name.direction <- names(internal)[i.direction]
              internal <- checkAndTidyComponent(object = internal,
                                                name = "internal",
                                                requireInteger = TRUE,
                                                allowNegatives = FALSE,
                                                allowOrig = FALSE,
                                                allowParent = FALSE,
                                                allowTriangles = TRUE,
                                                triangles = TRUE)
              Out <- slab(internal,
                          dimension = name.direction,
                          elements = 1L,
                          drop = FALSE)
              In <- slab(internal,
                         dimension = name.direction,
                         elements = 2L,
                         drop = FALSE)
              Out <- tryCatch(makeCompatible(x = Out,
                                             y = template,
                                             subset = TRUE,
                                             check = TRUE),
                              error = function(e) e)
              if (methods::is(Out, "error"))
                  stop(gettextf("'%s' is incompatible with '%s' : %s",
                                "internal", "population", Out$message))
              In <- makeCompatible(x = In,
                                   y = template,
                                   subset = TRUE,
                                   check = FALSE)
              internal <- dbind(Out, In, along = "direction")
              i.direction <- length(dim(internal))
              i.between <- match(names.between, names(internal), nomatch = 0L)
              not.collapsed <- i.between > 0L
              if (any(not.collapsed)) {
                  i.between <- i.between[not.collapsed]
                  methods::new("InternalMovementsPool",
                               .Data = internal@.Data,
                               metadata = internal@metadata,
                               iBetween = i.between,
                               iDirection = i.direction)
              }
              else
                  stop(gettextf("no \"%s\" dimensions from '%s' found in '%s'",
                                "between", "internal", "population")) 
          })

## HAS_TESTS
setMethod("InternalMovements",
          signature(internal = "Net"),
          function(internal, template) {
              i.between <- internal@iBetween
              names.between <- names(internal)[i.between]
              internal <- checkAndTidyComponent(object = internal,
                                                name = "internal",
                                                requireInteger = TRUE,
                                                allowNegatives = TRUE,
                                                allowOrig = FALSE,
                                                allowParent = FALSE,
                                                allowTriangles = TRUE,
                                                triangles = TRUE)
              internal <- tryCatch(makeCompatible(x = internal,
                                                  y = template,
                                                  subset = TRUE,
                                                  check = TRUE),
                                   error = function(e) e)
              if (methods::is(internal, "error"))
                  stop(gettextf("'%s' is incompatible with '%s' : %s",
                                "internal", "population", internal$message))
              i.between <- match(names.between, names(internal), nomatch = 0L)
              not.collapsed <- i.between > 0L
              if (any(not.collapsed)) {
                  i.between <- i.between[not.collapsed]
                  methods::new("InternalMovementsNet",
                               .Data = internal@.Data,
                               metadata = internal@metadata,
                               iBetween = i.between)
              }
              else
                  stop(gettextf("no \"%s\" dimensions from '%s' found in '%s'",
                                "between", "internal", "population")) 
          })

## HAS_TESTS
NetMovements <- function(net, template, name) {
    if (!methods::is(net, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(net)))
    net <- checkAndTidyComponent(object = net,
                                 name = name,
                                 requireInteger = TRUE,
                                 allowNegatives = TRUE,
                                 allowOrig = FALSE,
                                 allowParent = FALSE,
                                 allowTriangles = TRUE,
                                 triangles = TRUE)
    net <- tryCatch(makeCompatible(x = net,
                                   y = template,
                                   subset = TRUE,
                                   check = TRUE),
                    error = function(e) e)
    if (methods::is(net, "error"))
        stop(gettextf("'%s' is incompatible with '%s' : %s",
                      name, "population", net$message))
    methods::new("NetMovements",
                 .Data = net@.Data,
                 metadata = net@metadata)
}

#' @rdname net-pool-generators
## HAS_TESTS
setMethod("Net",
          signature(object = "Counts"),
          function(object, between) {
              dim <- dim(object)
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              n.dim <- length(dim)
              between <- tidySubscript(between, nDim = n.dim, names = names)
              if (identical(length(between), 0L))
                  stop(gettextf("'%s' has length %d",
                                "between", length(between)))
              for (i in between) {
                  if (dim[i] < 2L)
                      stop(gettextf("\"%s\" dimension \"%s\" has length %d",
                                    "between", names[i], dim[i]))
                  if (!identical(dimtypes[i], "state"))
                      stop(gettextf("\"%s\" dimension \"%s\" has dimtype \"%s\"",
                                    "between", names[i], dimtypes[i]))
              }
              methods::new("Net",
                           .Data = object@.Data,
                           metadata = object@metadata,
                           iBetween = between)
          })

#' @rdname net-pool-generators
## HAS_TESTS
setMethod("Pool",
          signature(object = "Counts"),
          function(object, direction, between) {
              kDimvaluesDirection <- list(c("out", "in"),
                                          c("outs", "ins"))
              .Data <- object@.Data
              dim <- dim(object)
              names <- names(object)
              dimtypes <- dimtypes(object, use.names = FALSE)
              DimScales <- DimScales(object, use.names = FALSE)
              n.dim <- length(dim)
              direction <- tidySubscript(direction, nDim = n.dim, names = names)
              between <- tidySubscript(between, nDim = n.dim, names = names)
              if (!identical(length(direction), 1L))
                  stop(gettextf("'%s' has length %d",
                                "direction", length(direction)))
              if (!identical(dim[direction], 2L))
                  stop(gettextf("\"%s\" dimension has length %d",
                                "direction", dim[direction]))
              dimvalues <- dimvalues(DimScales[[direction]])
              dimvalues <- tolower(dimvalues)
              is.identical <- sapply(kDimvaluesDirection, identical, y = dimvalues)
              if (!any(is.identical))
                  stop(gettextf("\"%s\" dimension has invalid categories",
                                "direction"))
              if (identical(length(between), 0L))
                  stop(gettextf("'%s' has length %d",
                                "between", length(between)))
              for (i in between) {
                  if (dim[i] < 2L)
                      stop(gettextf("\"%s\" dimension \"%s\" has length %d",
                                    "between", names[i], dim[i]))
                  if (!identical(dimtypes[i], "state"))
                      stop(gettextf("\"%s\" dimension \"%s\" has dimtype \"%s\"",
                                    "between", names[i], dimtypes[i]))
              }
              DimScale.direction <- methods::new("Categories", dimvalues = c("Out", "In"))
              DimScales <- replace(DimScales,
                                   list = direction,
                                   values = list(DimScale.direction))
              metadata <- methods::new("MetaData",
                                       nms = names,
                                       dimtypes = dimtypes,
                                       DimScales = DimScales)
              dimnames(.Data) <- dimnames(metadata)
              methods::new("Pool",
                           .Data = .Data,
                           metadata = metadata,
                           iDirection = direction,
                           iBetween = between)
          })
