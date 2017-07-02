
## HAS_TESTS
#' Create a demographic account based on movements.
#'
#' The only compulsory argument is \code{population}.  All demographic series
#' must have a time dimension.  In the case of \code{population}, the time
#' dimension must have \code{\link{dimscale}} \code{"Points"}; in all other cases
#' it must have \code{\link{dimscale}} \code{"Intervals"}.
#' 
#' @param population A \code{\linkS4class{Counts}} object.
#' @param births A \code{\linkS4class{Counts}} object.
#' @param internal  If \code{internal} uses an origin-destination format,
#' an ordinary \code{\linkS4class{Counts}} object; if it uses a pool or
#' net format, an object created by functions \code{\link{collapseOrigDest}},
#' \code{\link{Pool}}, or \code{\link{Net}}.
#' @param entries A named list of \code{\linkS4class{Counts}} objects.
#' @param exits A named list of \code{\linkS4class{Counts}} objects.
#' @param net A named list of \code{\linkS4class{Counts}} objects.
#'
#' @return An object of class \code{\linkS4class{Movements}}
#'
#' @examples
#' population <- Counts(array(c(10, 15, 13, 16),
#'                            dim = c(2, 2),
#'                            dimnames = list(age = c("0-29", "30+"),
#'                                            time = c(1970, 2000))))
#' births <- Counts(array(13,
#'                        dim = c(1, 1),
#'                        dimnames = list(age = "30+",
#'                                        time = "1971-2000")))
#' deaths <- Counts(array(c(0, 9),
#'                        dim = c(2, 1),
#'                        dimnames = list(age = c("0-29", "30+"),
#'                                        time = c("1971-2000"))))
#' account <- Movements(population = population,
#'                      births = births,
#'                      exits = list(deaths = deaths))
#' account
#' summary(account)
#'
#' @export
Movements <- function(population, births = NULL, internal = NULL,
                      entries = list(), exits = list(), net = list()) {
    population <- Population(population)
    template <- makeTemplateComponent(population)
    components <- list()
    namesComponents <- character()
    if (!is.null(births)) {
        births <- BirthsMovements(births = births, template = template)
        components <- c(components, list(births))
        namesComponents <- c(namesComponents, "births")
    }
    if (!is.null(internal)) {
        internal <- InternalMovements(internal = internal, template = template)
        components <- c(components, list(internal))
        namesComponents <- c(namesComponents, "internal")
    }
    if (length(entries) > 0L) {
        names.entries <- names(entries)
        checkNamesComponents(names = names.entries, componentType = "entries")
        for (i in seq_along(entries)) {
            entries[[i]] <- EntriesMovements(entries = entries[[i]],
                                             template = template,
                                             name = names.entries[i])
        }
        components <- c(components, entries)
        namesComponents <- c(namesComponents, names.entries)
    }
    if (length(exits) > 0L) {
        names.exits <- names(exits)
        checkNamesComponents(names = names.exits, componentType = "exits")
        for (i in seq_along(exits)) {
            exits[[i]] <- ExitsMovements(exits = exits[[i]],
                                         template = template,
                                         name = names.exits[i])
        }
        components <- c(components, exits)
        namesComponents <- c(namesComponents, names.exits)
    }
    if (length(net) > 0L) {
        names.net <- names(net)
        checkNamesComponents(names = names.net, componentType = "net")
        for (i in seq_along(net)) {
            net[[i]] <- NetMovements(net = net[[i]],
                                     template = template,
                                     name = names.net[i])
        }
        components <- c(components, net)
        namesComponents <- c(namesComponents, names.net)
    }
    components <- unname(components)
    if (any(duplicated(namesComponents)))
        stop(gettext("names for components have duplicates"))
    methods::new("Movements",
                 population = population,
                 components = components,
                 namesComponents = namesComponents)
}


    
#' Derive population counts from initial population and components.
#'
#' Given initial population counts, and counts for components such as
#' births, deaths, and migration, derive population counts for subsequent
#' years.
#'
#' Sometimes values for the initial population and components imply
#' negative population counts.  In such cases, an error will be raised
#' if the \code{adjust} is \code{FALSE} (the default). Otherwise,
#' \code{derivePopulation} will randomly revise entries (eg births)
#' upwards and exits (eg deaths) downwards until the population counts
#' become non-negative, via function \code{\link{makeConsistent}}.
#'
#' The \code{initial} argument does not need to have a time dimension,
#' but it does, the dimension must be of length 1, and must have
#' \code{\link{dimscale}} \code{"Points"}.  The components must
#' have identical time dimensions to one another, and these dimensions
#' must have dimscale \code{"Intervals"}.
#'
#' @inheritParams Movements
#' @param initial The starting or jump-off population. An object of
#' class \code{\linkS4class{Counts}}.
#' @param movements Logical. If \code{TRUE} (the default) and
#' \code{\linkS4class{Movements}} account is returned.  If \code{FALSE},
#' a \code{Transitions} account is returned.
#' @param adjust Logical. If \code{FALSE} (the default) an error is
#' raised of the initial population and components imply negative
#' population counts.  If \code{TRUE}, the components are adjusted
#' until negative population counts are eliminated.
#' @param scale A non-negative number governing the size of the steps
#' made when adjusting.
#'
#' @return A \code{\linkS4class{DemographicAccount}}.
#'
#' @seealso Most of the work for \code{derivePopulation} is done by
#' function \code{\link{makeConsistent}}.
#'
#' @examples
#' initial <- Counts(array(c(10, 15),
#'                         dim = 2,
#'                         dimnames = list(age = c("0-29", "30+"))))
#' births <- Counts(array(13,
#'                        dim = c(1, 1),
#'                        dimnames = list(age = "30+",
#'                                        time = "1971-2000")))
#' deaths <- Counts(array(c(0, 9),
#'                        dim = c(2, 1),
#'                        dimnames = list(age = c("0-29", "30+"),
#'                                        time = c("1971-2000"))))
#' derivePopulation(initial = initial,
#'                   births = births,
#'                   exits = list(deaths = deaths))
#'
#' ## Calculations using age-time steps of one quarter.  (Note, incidentally,
#' ## that an account does not have to start from age 0 if it does not
#' ## include births.)
#' initial <- Counts(array(11:15,
#'                         dim = c(5, 1),
#'                         dimnames = list(age = c("20-20.25", "20.25-20.5",
#'                                            "20.5-20.75", "20.75-21", "21+"),
#'                                         time = 2000)),
#'                   dimscales = c(time = "Points"))
#' deaths <- Counts(array(c(0, 2, 1, 3, 4, 1, 2, 3, 1, 5),
#'                        dim = c(5, 2, 1),
#'                        dimnames = list(age = c("20-20.25", "20.25-20.5",
#'                                            "20.5-20.75", "20.75-21", "21+"),
#'                                        triangle = c("TL", "TU"),
#'                                        time = "2000-2000.25")))
#' account <- derivePopulation(initial = initial,
#'                             exits = list(deaths = deaths))
#' account
#' summary(account)
#' @export
derivePopulation <- function(initial, births = NULL, internal = NULL,
                             entries = list(), exits = list(), net = list(),
                             movements = TRUE, adjust = FALSE, scale = 0.1) {
    if (!isTRUE(movements))
        stop("currently can only deal with movements accounts")
    if (!is.null(births))
        DS.time.popn <- getDimScaleTimePopn(births,
                                            name = "births")
    else if (!is.null(internal))
        DS.time.popn <- getDimScaleTimePopn(internal,
                                            name = "internal")
    else if (length(entries) != 0L) {
        names.entries <- names(entries)
        if (is.null(names.entries))
            stop(gettextf("'%s' does not have names",
                          "entries"))
        DS.time.popn <- getDimScaleTimePopn(entries[[1L]],
                                            name = names.entries[1L])
    }
    else if (length(exits) != 0L) {
        names.exits <- names(exits)
        if (is.null(names.exits))
            stop(gettextf("'%s' does not have names",
                          "exits"))
        DS.time.popn <- getDimScaleTimePopn(exits[[1L]],
                                            name = names.exits[1L])
    }
    else if (length(net) != 0L) {
        names.net <- names(net)
        if (is.null(names.net))
            stop(gettextf("'%s' does not have names",
                          "net"))
        DS.time.popn <- getDimScaleTimePopn(net[[1L]],
                                            name = names.net[1L])
    }
    else
        stop(gettextf("no values supplied for '%s', '%s', '%s', '%s', or '%s'",
                      "births", "internal", "entries", "exits", "net"))
    .Data.initial <- initial@.Data
    dim.initial <- dim(initial)
    names.initial <- names(initial)
    dimtypes.initial <- dimtypes(initial,
                                 use.names = FALSE)
    DimScales.initial <- DimScales(initial,
                                   use.names = FALSE)
    i.time.initial <- match("time", dimtypes.initial, nomatch = 0L)
    has.time.initial <- i.time.initial > 0L
    if (has.time.initial) {
        DS.time.initial <- DimScales.initial[[i.time.initial]]
        if (!methods::is(DS.time.initial, "Points"))
            stop(gettextf("dimension of '%s' with %s \"%s\" has dimscale \"%s\"",
                          "initial", "dimtype", "time", class(DS.time.initial)))
        dv.time.initial <- dimvalues(DS.time.initial)
        n.dv.time.initial <- length(dv.time.initial)
        if (n.dv.time.initial == 0L)
            NULL # ignore
        else if (n.dv.time.initial == 1L) {
            dv.time.popn <- dimvalues(DS.time.popn)
            if (!isTRUE(all.equal(dv.time.initial, dv.time.popn[1L])))
                stop(gettextf("time dimension of '%s' not consistent with time dimension of components",
                              "initial"))
        }            
        else
            stop(gettextf("time dimension of '%s' has more than one point",
                          "initial"))
        dim.time.last <- c(dim.initial[-i.time.initial], length(DS.time.popn))
        .Data.population <- array(.Data.initial,
                                  dim = dim.time.last) # replicates data
        n.dim.initial <- length(dim.initial)
        perm <- seq_len(n.dim.initial - 1L)
        perm <- append(perm,
                       values = n.dim.initial,
                       after = i.time.initial - 1L)
        .Data.population <- aperm(.Data.population,
                                  perm = perm)
        DimScales.population <- replace(DimScales.initial,
                                        list = i.time.initial,
                                        values = list(DS.time.popn))
        metadata.population <- methods::new("MetaData",
                                            nms = names.initial,
                                            dimtypes = dimtypes.initial,
                                            DimScales = DimScales.population)
        .Data.population <- array(.Data.population,
                                  dim = dim(metadata.population),
                                  dimnames = dimnames(metadata.population))
    }
    else {
        names.population <- make.unique(c(names.initial, "time"))
        dimtypes.population <- c(dimtypes.initial, "time")
        DimScales.population <- c(DimScales.initial, list(DS.time.popn))
        metadata.population <- new("MetaData",
                                   nms = names.population,
                                   dimtypes = dimtypes.population,
                                   DimScales = DimScales.population)
        .Data.population <- array(.Data.initial,
                                  dim = dim(metadata.population),
                                  dimnames = dimnames(metadata.population)) # replicates data
    }
    population <- new("Counts",
                      .Data = .Data.population,
                      metadata = metadata.population)
    account <- Movements(population = population,
                         births = births,
                         internal = internal,
                         entries = entries,
                         exits = exits,
                         net = net)
    makeConsistent(object = account,
                   adjust = adjust,
                   scale = scale)
}
