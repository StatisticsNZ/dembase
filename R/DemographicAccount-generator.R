
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
    if (any(duplicated(namesComponents)))
        stop(gettext("names for components have duplicates"))
    methods::new("Movements",
                 population = population,
                 components = components,
                 namesComponents = namesComponents)
}
