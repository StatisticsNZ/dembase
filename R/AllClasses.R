
## exported-not-api ####################################################################

#' Classes and functions that are exported but that are not part of the API.
#'
#' The classes functions listed below are exported, so that they can be
#' used internally by other packages such as \code{demest}.  However, they
#' do not form part of API for the package \code{dembase}.  They
#' should not be needed in normal interactive use.
#'
#' @param object Typically an object of class
#' \code{\linkS4class{DemographicArray}}.
#' @param use.names Logical. Whether to return a named vector.
#' @param x First element.  Typically an object of class
#' \code{\linkS4class{DemographicArray}}.
#' @param y Second element.  Typically an object of class
#' \code{\linkS4class{DemographicArray}}.
#' @param subset Logical. Whether to allow transformations that return a
#' subset of all cells.
#' @param concordances  A list of objects of class
#' \code{\link[classconc:Concordance-class]{Concordance}}.
#' @param allowCopyIterDim Logical. Whether to allow an iteration dimension
#' to be copied from one element to the other, if one element has an iterations
#' dimension but the other does not.
#' @param transform An object of class \code{\linkS4class{Transform}}.
#' @param n Number of increments.
#' @param check Logical. Whether to check if transformation can be performed
#' before trying to perform it.
#' @param along Name or index of dimension.
#' @param metadata An object of class \code{\linkS4class{MetaData}}.
#' @param numericDimScales Logical. Whether to require that \code{along}
#' dimension have \code{\link{dimscale}} \code{"Points"} or \code{"Intervals"}.
#' @param i Integer. Index for cell within.
#' @param useC Logical. Whether to call the C function, or use the R equivalent.
#' @param minAges Integer. Minimum length for age dimension.
#' @param regular Logical. Whether age or time steps must have equal lengths.
#' @param openLeftOK Logical. Whether the first age group can be open (ie
#' have no minimum age.)
#' @param openRightOK Logical. Whether the last age group can be open (ie
#' have no minimum age.)
#' @param expectedDimscale Name of a \code{\link{dimscale}}.
#' @param dimtype A \code{\link{dimtype}}.
#' @param dimscale A \code{\link{dimscale}}.
#' @param DimScale An object of class DimScale.
#' @param labels A character vector with labels for a dimension.
#' @param name Character. A dimension name.
#' @param iterations  Integer.  The iteractions to extract.
#' @param mx Mortality rates.
#' @param ax Separation factors.
#' @param value Replacement value.
#' @param checkNumericDimscales Whether to require 'along' dimension
#' to have a numeric dimscale.
#' @name exported-not-api
NULL


## S3 CLASSES ##########################################################################

setOldClass("table")

setOldClass(c("xtabs", "table"))




## MetaData ###########################################################################

#' @rdname exported-not-api
#' @export
setClass("MetaData",
         slots = c(nms = "character",
             dimtypes = "character",
             DimScales = "list"),
         validity = function(object) {
             ## do not use accessor functions because these may
             ## produce confusing error messages
             names <- object@nms
             dimtypes <- object@dimtypes
             DimScales <- object@DimScales
             dimscales <- as.character(sapply(DimScales, class))
             ## names
             return.value <- validNames(names)
             if (!isTRUE(return.value))
                 return(return.value)
             ## dimtypes
             if (any(is.na(dimtypes)))
                 return(gettextf("'%s' has missing values", "dimtypes"))
             is.invalid.dimtype <- !(dimtypes %in% getValidDimtypes())
             if (any(is.invalid.dimtype))
                 return(gettextf("\"%s\" is not a valid dimtype",
                                 dimtypes[is.invalid.dimtype][1L]))
             for (dimtype in getUniqueDimtypes())
                 if (sum(dimtypes == dimtype) > 1L)
                     return(gettextf("more than one dimension with dimtype \"%s\"",
                                     dimtype))
             if (!is.null(names(dimtypes)))
                 return(gettextf("'%s' has names", "dimtypes"))
             ## DimScales
             if (!all(sapply(DimScales, methods::is,"DimScale")))
                 return(gettext("'DimScales' has element not of class \"DimScale\""))
             if (!is.null(names(DimScales)))
                 return(gettextf("'%s' has names", "DimScales"))
             ## dimtypes and names have same length
             if (!identical(length(dimtypes), length(names)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "dimtypes", "names"))
             ## DimScales and names have same length
             if (!identical(length(dimscales), length(names)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "DimScales", "names"))
             ## Prohibit object with length 0.  Test for length 0 after testing
             ## that names, dimtypes, and DimScales all have same length.
             if (identical(length(names), 0L))
                 return(gettext("must have at least 1 dimension"))
             ## dimensions have dimscales permitted for dimtypes
             for (i in seq_along(dimtypes)) {
                 dimtype <- dimtypes[i]
                 dimscale <- dimscales[i]
                 permitted.dimscales <- getPossibleDimscales(dimtype)
                 if (!(dimscale %in% permitted.dimscales))
                     return(gettextf("dimension \"%s\" has dimtype \"%s\" but dimscale \"%s\"",
                                     names[i], dimtype, dimscale))
             }
             ## origin, destination, parent, child dimensions
             dimtypes.with.pairs <- getDimtypesWithPairs()
             for (dimtype in dimtypes.with.pairs) {
                 suffix <- getSuffixes(dimtypes = dimtype)
                 pattern <- paste(suffix, "$", sep = "")
                 has.suffix <- grepl(pattern, names)
                 has.dimtype <- grepl(dimtype, dimtypes)
                 has.suffix.not.dimtype <- has.suffix & !has.dimtype
                 if (any(has.suffix.not.dimtype))
                     return(gettextf("dimension \"%s\" has suffix \"%s\" but not dimtype \"%s\"",
                                     names[has.suffix.not.dimtype][1L],
                                     suffix,
                                     dimtype))
                 has.dimtype.not.suffix <- has.dimtype & !has.suffix
                 if (any(has.dimtype.not.suffix))
                     return(gettextf("dimension \"%s\" has dimtype \"%s\" but not suffix \"%s\"",
                                     names[has.dimtype.not.suffix][1L],
                                     dimtype,
                                     suffix))
                 for (name in names[has.dimtype]) {
                     implied.name.pair <- getNamesPairs(names = name)
                     if (!(implied.name.pair %in% names))
                         return(gettextf("dimension \"%s\" lacks pair", name))
                     name.without.suffix <- removeSuffixes(names = name)
                     if (name.without.suffix %in% names)
                         return(gettextf("dimension named \"%s\" and dimension named \"%s\"",
                                         name, name.without.suffix))
                 }
             }
             ## iteration, quantile dimensions
             has.iteration <- "iteration" %in% dimtypes
             has.quantile <- "quantile" %in% dimtypes
             if (has.iteration && has.quantile)
                 return(gettext("has dimtype \"iteration\" and dimtype \"quantile\""))
             ## triangle dimension
             has.triangle <- "triangle" %in% dimtypes
             if (has.triangle) {
                 has.two.age.time.cohort <- sum(dimtypes %in% c("age", "time", "cohort")) == 2L
                 if (!has.two.age.time.cohort)
                     return(gettextf("has dimtype \"%s\" but does not have two dimensions with dimtype \"%s\", \"%s\", \"%s\"",
                                     "triangle", "age", "time", "cohort"))
                 for (dimtype in c("age", "time")) {
                     i.dimtype <- match(dimtype, dimtypes, nomatch = 0L)
                     has.dimtype <- i.dimtype > 0L
                     if (has.dimtype) {
                         DimScale <- DimScales[[i.dimtype]]
                         if (!methods::is(DimScale, "Intervals"))
                             return(gettextf("has dimension with dimtype \"%s\" but dimension with dimtype \"%s\" has dimscale \"%s\"", "triangle", dimtype, class(DimScale)))
                     }
                 }
                 return.value <- tryCatch(hasRegularAgeTime(object),
                                          error = function(e) e)
                 if (!isTRUE(return.value))
                     return(gettextf("has dimension with dimtype \"%s\" but does not have regular age-time plan : %s",
                                     "triangle", return.value$message))
             }
             TRUE
         })

setClassUnion("MetaDataOrNULL",
              members = c("MetaData", "NULL"))


## SUPERCLASS "DemographicArray" AND SUBCLASSES #########################################


#' Classes "DemographicArray", "Counts", and "Values".
#'
#' Classes for representing demographic arrays: esssentially, arrays plus metadata.
#'
#'
#'   \code{DemographicArray} is a virtual superclass, and
#'  \code{Counts} and \code{Values} its two main subclasses.  For
#'  a discussion of what these terms mean and of R's class system see
#'  \code{\link[methods]{Classes}}.  However, to use package
#'  \pkg{dembase}, it is probably enough to know that the phrase
#'  'objects of class\code{DemographicArray}' is shorthand for 'objects of
#'  any class that is a subclass of \code{DemographicArray}'.  A list of the
#'  subclasses of \code{DemographicArray} can be obtained using
#'  \code{getClass(DemographicArray)}.
#'     
#'  Objects of class \code{DemographicArray} are arrays with some
#'  specialized metadata that are useful when dealing with population data.  For
#'  instance, all objects of class \code{DemographicArray} have
#'  \code{\link{dimtypes}} and \code{\link{dimscales}} describing the type
#'  of variable being measured and the measurement scale.  Objects of
#'  class \code{DemographicArray} also have some specialized behaviours that
#'  arrays do not.  For instance, when two objects of class
#'  \code{DemographicArray} are added together, the dimensions of the two
#'  objects are automatically aligned.
#'
#'   Objects of class \code{Counts} hold data about numbers of people or
#'   events, while objects of class \code{Values} hold information about
#'   characteristics or attributes.  Some functions, such as ones that
#'   aggregate cells, treat objects of class {"Counts"} differently from
#'   objects of class {"Values"}.
#'
#'   Unlike ordinary arrays, objects of class \code{DemographicArray}
#'    must have a complete set of dimnames, meaning that each dimension
#'    must be named, and within a dimension the labels must be
#'    unique.
#'
#' @section Objects from the class:
#'   Objects of class \code{Counts} and \code{Values} are generated using functions
#'  \code{\link{Counts}} and \code{\link{Values}}.  Because \code{DemographicArray}
#'  is a virtual class, no objects may be created from it.
#' 
#' @section Automating reshaping:
#' When demographic arrays are used in arithmetic, or are supplied to
#' a function, one or more of the objects will attempt to reshape themselves
#' so that the objects are compatible.  The reshaping involves the following
#' operations:
#' \describe{
#'  \item{Permuting dimensions}{Dimensions are rearranged so that
#'  they follow the same order.}
#'  \item{Adding dimensions}{If an object of class {"Values"}
#'  lacks a dimension that others have, the missing dimension is added to
#'  that object.}
#'  \item{Collapsing dimensions}{If an object of class
#'  \code{"Counts"} has a dimension that others lack, the extra
#'  dimension is collapsed away.}
#'  \item{Permuting categories}{Categories within each dimension
#'  are rearranged so that they follow the same order.}
#'  \item{Splitting intervals}{If an object of class
#'  \code{"Values"} uses coarser intervals than other objects, the coarser
#'  intervals are split.  Cells within the new intervals have the same
#'  values as cells within the old combined interval.}
#'  \item{Collapsing intervals}{If an object of class
#'  \code{"Counts"} uses finer intervals than other objects, the finer
#'  intervals are collapsed.}
#'  \item{Subsetting}{If on object contains categories that another
#' object does not, the extra categories are typically dropped.}
#' }
#' 
#' If these operations are not sufficient to align objects, then an error
#' is raised.  In particular, an error will be raised if the only way to
#' align objects is to remove cells.
#'
#'  The rules for adding dimensions to objects of class \code{"Values"},
#'  and for splitting intervals within objects of class \code{"Values"},
#'  assume that, within each cell of the original classification, every
#'  person or event is identical.  These sorts of homogeneity assumptions
#'  are standard in applied demography.  The assumptions are more
#'  plausible when more categories are dimensions are used.  Homogeneity
#'  assumptions can be avoided by adding dimensions or splitting intervals
#'  'by hand' with functions such as \code{\link{addDimension}}.
#'  
#'  When there is a mixture of \code{"Counts"} and \code{"Value"} objects,
#'  there is often a choice collapsing the \code{"Counts"} objects and
#'  splitting or adding to the \code{"Values"} objects.  The default it to
#'  split and add to the \code{"Values"} objects, as this preserves all
#'  the original detail while giving the same subtotals.
#'
#' @section Methods for existing functions:
#'   A function that was designed to work with ordinary arrays will
#'   generally gives an equivalent result when used with a demographic array.
#'   For instance, if \code{a} is an array, then \code{sum(a)}
#'   equals \code{sum(Counts(a))}.
#'
#'  Some methods for demographic arrays include options not
#'  available for ordinary arrays.  See, for instance,
#'  \code{\link[=as.data.frame]{as.data.frame}} and
#'  \code{\link[=names-methods]{names}}.
#'  
#'  In some cases, copying the behaviour of ordinary arrays would require
#'  breaking the rules governing dimension names, dimtypes, and
#'  dimscales discussed in \code{\link{dimtypes}}.  See, for instance,
#'  \code{\link[dembase]{drop}}.
#'  
#'  Function \code{\link[=names-methods]{names}} returns \code{NULL} when used
#'  with an ordinary array, but returns the names of the dimensions when
#'  used with a demographic array.
#'
#' @seealso   \code{\link{Counts}}, \code{\link{Values}}, \code{\link{dimtypes}}
#'  \code{\link{dimscales}}.  The main new functions for manipulating
#' demographic arrays are listed in \code{\link{dembase-package}}.
#'
#' @examples
#' a <- array(stats::rpois(n = 6, lambda = 10),
#'           dim = c(3, 2),
#'           dimnames = list(age = c("0-19", "20-64", "65+"),
#'               sex = c("Female", "Male")))
#' x <- Counts(a)
#' x
#' plot(x)
#' x^2
#' mean(x)
#' names(x)
#' collapseDimension(x, dimension = "age")
#' 
#' b <- array(rnorm(n = 6),
#'            dim = c(2, 3),
#'           dimnames = list(sex = c("Male", "Female"),
#'                age = c("0-19", "20-64", "65+")))
#' y <- Values(b)
#' y
#' ## 'y' is automatically reshaped to align to 'x'
#' x * y
#' ## weights are required with objects of class "Values"
#' collapseDimension(y, dimension = "age", weights = x)
#' @name DemographicArray-class
NULL

#' @rdname DemographicArray-class
#' @export
setClass("DemographicArray",
         slots = c(metadata = "MetaData"),
         contains = "VIRTUAL",
         validity = function(object) {
             data <- object@.Data
             metadata <- metadata(object)
             if (!is.numeric(object))
                 return(gettextf("does not have type \"%s\"",
                                 "numeric"))
             if (!identical(dim(data), dim(metadata)))
                 return(gettextf("'%s' and '%s' have different dimensions",
                                 ".Data", "metadata"))
             if (!identical(dimnames(data), dimnames(metadata)))
                 return(gettextf("'%s' and '%s' have different dimnames",
                                 ".Data", "metadata"))
             TRUE
         })

#' @rdname DemographicArray-class
#' @export
setClass("Counts", contains = c("array", "DemographicArray"))

#' @rdname DemographicArray-class
#' @export
setClass("Values", contains = c("array", "DemographicArray"))

#' @rdname DemographicArray-class
#' @export
setClassUnion("DemographicArrayOrNumeric",
              members = c("DemographicArray",
                  "numeric"))

## DIMSCALES ###############################################################

## HAS_TESTS
setClass("DimScale",
         contains = "VIRTUAL",
         validity = function(object) {
             dimvalues <- dimvalues(object)
             if (any(is.na(dimvalues)))
                 return(gettext("missing values"))
             TRUE
         })

## HAS_TESTS
setClass("Categories",
         slots = c(dimvalues = "character"),
         contains = "DimScale",
         validity = function(object) {
             dimvalues <- dimvalues(object)
             if (!all(nzchar(dimvalues)))
                 return(gettext("values with length 0"))
             if (any(duplicated(dimvalues)))
                 return(gettext("duplicated values"))
             TRUE
         })

## HAS_TESTS
setClass("Sexes",
         contains = "Categories",
         validity = function(object) {
             dimvalues <- dimvalues(object)
             dimvalues <- tolower(dimvalues)
             valid.singular <- all(dimvalues %in% c("female", "male"))
             valid.plural <- all(dimvalues %in% c("females", "males"))
             valid.letters <- all(dimvalues %in% c("f", "m"))
             if (!(valid.singular || valid.plural || valid.letters))
                 return(gettext("invalid values"))
             TRUE
         })

## HAS_TESTS
setClass("Triangles",
         contains = "Categories",
         validity = function(object) {
             dimvalues <- dimvalues(object)
             all.lower.upper <- all(dimvalues %in% c("Lower", "Upper"))
             all.tl.tu <- all(dimvalues %in% c("TL", "TU"))
             if (!(all.lower.upper || all.tl.tu))
                 return(gettext("invalid values"))
             TRUE
         })

## HAS_TESTS
setClass("Points",
         slots = c(dimvalues = "numeric"),
         contains = "DimScale",
         validity = function(object) {
             dimvalues <- dimvalues(object)
             if (any(is.infinite(dimvalues)))
                 return(gettext("non-finite values"))
             if (!all(diff(dimvalues) > 0))
                 return(gettext("values not strictly increasing"))
             TRUE
         })

## HAS_TESTS
setClass("Quantiles",
         contains = "Points",
         validity = function(object) {
             dimvalues <- dimvalues(object)
             if (any(dimvalues < 0))
                 return(gettext("values less than 0"))
             if (any(dimvalues > 1))
                 return(gettext("values greater than 1"))
             TRUE
         })

## HAS_TESTS
setClass("Intervals",
         slots = c(dimvalues = "numeric"),
         contains = "DimScale",
         validity = function(object) {
             dimvalues <- dimvalues(object)
             n <- length(dimvalues)
             if (n > 0L) {
                 if (n == 1L)
                     return(gettext("must have 0 or at least 2 values"))
                 if (all(is.infinite(dimvalues)))
                     return(gettext("no finite values"))
                 if (!all(diff(dimvalues) > 0))
                     return(gettext("values not strictly increasing"))
             }
             TRUE
         })

## HAS_TESTS
setClass("Iterations",
         slots = c(dimvalues = "integer"),
         contains = "DimScale",
         validity = function(object) {
             dimvalues <- dimvalues(object)
             if (any(dimvalues <= 0))
                 return(gettext("values less than or equal to 0"))
             if (any(duplicated(dimvalues)))
                 return(gettext("duplicated values"))
             TRUE
         })


## MISCELLANEOUS ###############################################################

#' @rdname exported-not-api
#' @export
setClass("Transform",
         slots = c(indices = "list",
             dims = "integer",
             dimBefore = "integer",
             dimAfter = "integer"),
         contains = "VIRTUAL",
         prototype = prototype(indices = list(integer()),
             dims = 1L,
             dimBefore = 0L,
             dimAfter = 0L),
         validity = function(object) {
             indices <- object@indices
             dims <- object@dims
             dimBefore <- object@dimBefore
             dimAfter <- object@dimAfter
             for (i in seq_along(indices)) {
                 index <- indices[[i]]
                 ## elements of indices have type "integer"
                 if (!is.integer(index))
                     return(gettextf("element %d of '%s' is not of type \"integer\"",
                                     i, "indices"))
                 ## none of the elements of indices contain NAs
                 if (any(is.na(index)))
                     return(gettextf("element %d of '%s' has missing values",
                                     i, "indices"))
             }
             ## dims does not contain NAs
             if (any(is.na(dims)))
                 return(gettextf("'%s' has missing values",
                                 "dims"))
             ## 'dims' does not contain duplicates
             if (any(duplicated(dims[dims != 0])))
                 return(gettextf("'%s' refers more than once to the same dimension",
                                 "dims"))
             ## 'dimBefore' and 'dimAfter' contain no NAs
             if (any(is.na(dimBefore)))
                 return(gettextf("'%s' has missing values", "dimBefore"))
             if (any(is.na(dimAfter)))
                 return(gettextf("'%s' has missing values", "dimAfter"))
             ## indices and dims the same length
             if (!identical(length(dims), length(indices)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "indices", "dims"))
             TRUE
         })

#' @rdname exported-not-api
#' @export
setClass("CollapseTransform",
         contains = "Transform",
         validity = function(object) {
             indices <- object@indices
             dims <- object@dims
             dimBefore <-object@dimBefore
             dimAfter <- object@dimAfter
             ## none of the elements of indices have negative values
             for (i in seq_along(indices)) {
                 index <- indices[[i]]
                 if (any(index < 0L))
                     return(gettextf("element %d of '%s' has negative values",
                                     i, "indices"))
             }
             ## dims does not contain negative values
             if (any(dims < 0L))
                 return(gettextf("'%s' has negative values",
                                 "dims"))
             ## once the 0s are removed, dims consists of 1, 2, ...
             ## in any order.  Note that this implies that
             ## max(dims) equals length(dims[dims != 0]).
             dims.keep <- dims[dims != 0L]
             if (!identical(sort(dims.keep), seq_along(dims.keep)))
                 return(gettext("'dims' has gaps"))
             ## lengths of indices and elements of dimBefore consistent
             if (length(indices) > 0L) {
                 lengths.indices <- sapply(indices, length, USE.NAMES = FALSE)
                 consistent <- identical(lengths.indices, dimBefore)
             }
             else
                 consistent <- identical(dimBefore, 0L)
             if (!consistent)
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "indices", "dimBefore"))
             ## max values of 'indices' consistent with 'dimAfter'
             ## (after permutation and subsetting specified by 'dims')
             maxOrZero <- function(x) if (length(x) > 0L) max(x) else 0L
             if (length(indices) > 0L) {
                 inv.dims <- match(seq_along(dims), dims, nomatch = 0L)
                 max.indices <- sapply(indices[inv.dims], maxOrZero, USE.NAMES = FALSE)
                 consistent <- identical(max.indices, dimAfter)
             }
             else
                 consistent <- identical(dimAfter, 0L)
             if (!consistent)
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "indices", "dimAfter"))
             ## If an element of dims is a 0, meaning that the corresponding
             ## dimension is dropped, the associated element of indices
             ## should consist entirely of 0s and 1s.  Previous tests
             ## imply that if there is a 1 it will be unique.
             for (i in seq_along(dims)) {
                 dim <- dims[[i]]
                 drop <- identical(dim, 0L)
                 if (drop) {
                     index <- indices[[i]]
                     if (identical(length(index), 0L))
                         return(gettextf("dimension %d has length 0 and cannot be dropped", i))
                     if (!identical(max(index), 1L))
                         return(gettextf("if dimension %d is to be dropped then element %d of '%s' must consist of 0s and at least one 1",
                                         i, i, "indices"))
                 }
             }
             TRUE
         })

## HAS_TESTS
## CollapseTransformExtra is just CollapseTransform with
## with some extra quantities pre-computed for the
## speed/convenience of functions 'getIAfter', 'getIBefore',
## 'getIOther', and 'getIShared'.  Slots 'multiplierBefore'
## and 'multiplierAfter' are used to convert from the 'margin'
## slots = list of cells (eg c(2L, 1L, 1L) for a 3D array) to
## the 'pos' slots = list (eg 2L).  'invIndices' is, roughly,
## the inverse of 'indices', and shows how margins from 'after'
## map back to margins of 'before'.  A good way to understand how
## 'invIndices' is defined is to look at function 'makeInvIndices'
## in "helper-functions.R".
#' @rdname exported-not-api
#' @export
setClass("CollapseTransformExtra",
         slots = c(multiplierBefore = "integer",
             multiplierAfter = "integer",
             invIndices = "list"),
         contains = "CollapseTransform",
         validity = function(object) {
             indices <- object@indices
             dims <- object@dims
             dimBefore <- object@dimBefore
             dimAfter <- object@dimAfter
             multiplierBefore <- object@multiplierBefore
             multiplierAfter <- object@multiplierAfter
             invIndices <- object@invIndices
             inv.indices.unlisted <- unlist(invIndices)
             ## 'dimBefore' and 'multiplierBefore' consistent
             mult.before.implied <- c(1L, cumprod(dimBefore[-length(dimBefore)]))
             mult.before.implied <- as.integer(mult.before.implied)
             if (!identical(multiplierBefore, mult.before.implied))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "multiplierBefore", "dimBefore"))
             ## 'dimAfter' and 'multiplierAfter' consistent
             mult.after.implied <- c(1L, cumprod(dimAfter[-length(dimAfter)]))
             mult.after.implied <- as.integer(mult.after.implied)
             if (!identical(multiplierAfter, mult.after.implied))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "multiplierAfter", "dimAfter"))
             ## 'indices' and 'invIndices' have same length
             if (!identical(length(indices), length(invIndices)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "indices", "invIndices"))
             ## elements within 'invIndices' all lists
             if (!all(sapply(invIndices, is.list)))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "invIndices", "list"))
             ## elements of 'invIndices' have expected length
             lengths <- sapply(invIndices, length)
             if (!identical(lengths[dims > 0L], dimAfter[dims]))
                 return(gettextf("'%s' inconsistent with '%s' and '%s'",
                                 "invIndices", "dims", "dimAfter"))
             if (!all(lengths[dims == 0L] == 1L))
                 return(gettextf("'%s' inconsistent with '%s'",
                                 "invIndices", "dims"))
             ## 'invIndices' all integer
             if (!is.integer(inv.indices.unlisted))
                 return(gettextf("'%s' has elements not of type \"%s\"",
                                 "invIndices", "integer"))
             ## 'invIndices' has no missing values
             if (any(is.na(inv.indices.unlisted)))
                 return(gettextf("'%s' has missing values",
                                 "invIndices"))
             ## elements of 'invIndices' are positive
             if (any(inv.indices.unlisted < 1L))
                 return(gettextf("'%s' has non-positive values",
                                 "invIndices"))
             TRUE
         })

setClass("ExtendTransform",
         contains = "Transform",
         validity = function(object) {
             indices <- object@indices
             dims <- object@dims
             dimBefore <- object@dimBefore
             dimAfter <- object@dimAfter
             ## all values in 'dims' between 0 and length(dimBefore), inclusive
             if (!all(dims %in% seq.int(from = 0L, to = length(dimBefore))))
                 return(gettextf("'%s' has values outside the valid range",
                                 "dims"))
             ## 'dims' refers once to each dimension in dimBefore
             if (!identical(sort(dims[dims != 0]), seq_along(dimBefore)))
                 return(gettextf("'%s' must refer exactly once to each dimension in '%s'",
                                 "dims", "dimBefore"))
             ## lengths of 'indices' and elements of 'dimAfter' consistent
             if (length(indices) > 0L) {
                 lengths.indices <- sapply(indices, length, USE.NAMES = FALSE)
                 consistent <- identical(lengths.indices, dimAfter)
             }
             else
                 consistent <- identical(dimAfter, 0L)
             if (!consistent)
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "indices", "dimAfter"))
             for (i in seq_along(indices)) {
                 index <- indices[[i]]
                 add <- identical(dims[i], 0L)
                 if (add) {
                     ## if a new dimension is to be added, then the corresponding element in
                     ## 'indices' should consist entirely of 1s...
                     if (any(index != 1L))
                         return(gettextf("if a new dimension is to be added at position %d then element %d of '%s' must consist entirely of 1s",
                                         i, i, "indices"))
                 }
                 else {
                     ## ...and if not, none of the elements of 'indices'
                     ## must lie outside range implied by 'dimBefore'
                     valid.range <- seq_len(dimBefore[dims[i]])
                     if (!all(index %in% valid.range))
                         return(gettextf("element %d of '%s' has values outside the valid range",
                                         i, "indices"))
                 }
             }
             TRUE
         })

#' Class used by function 'subarray'.
#'
#' For internal use only
#'
#' @keywords internal
#' @name SubArrayIndices-class
## HAS_TESTS
setClass("SubArrayIndices",
         slots = c(nms = "character",
             indices = "list"),
         validity = function(object) {
             nms <- object@nms
             indices <- object@indices
             ## 'nms' is a vector of valid names
             return.value <- validNames(nms)
             if (!isTRUE(return.value))
                 return(return.value)
             ## 'indices' consists of logical vectors
             if (!all(sapply(indices, is.logical)))
                 return(gettextf("'%s' has element not of type \"%s\"",
                                 "indices", "logical"))
             ## 'indices' has no missing values
             hasNA <- function(x) any(is.na(x))
             if (any(sapply(indices, hasNA)))
                 return(gettextf("'%s' has missing values",
                                 "indices"))
             ## 'nms' and 'indices' have same length
             if (!identical(length(nms), length(indices)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "nms", "indices"))
             TRUE
         })


## SUBTOTALS #######################################################

## HAS_TESTS
setClass("HasSubtotals",
         slots = c(subtotals = "integer",
             metadataSubtotals = "MetaDataOrNULL",
             transformSubtotals = "CollapseTransformExtra"),
         contains = "VIRTUAL",
         validity = function(object) {
             .Data <- object@.Data
             metadata <- object@metadata
             dimtypes <- dimtypes(metadata)
             subtotals <- object@subtotals
             metadata.subtotals <- object@metadataSubtotals
             transform.subtotals <- object@transformSubtotals
             ## object has no "iteration" or "quantile" dimensions
             for (dimtype in c("iteration", "quantile")) {
                 if (dimtype %in% dimtypes)
                     return(gettextf("dimension with dimtype \"%s\"",
                                     dimtype))
             }
             ## .Data has type "integer"
             if (!is.integer(.Data))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 ".Data", "integer"))
             ## all non-missing values in .Data are non-negative
             if (any(.Data < 0L, na.rm = TRUE))
                 return(gettextf("'%s' has negative values",
                                 ".Data"))
             ## 'subtotals' has no missing values
             if (any(is.na(subtotals)))
                 return(gettextf("'%s' has missing values",
                                 "subtotals"))
             ## 'subtotals' has no negative values
             if (any(subtotals < 0L))
                 return(gettextf("'%s' has negative values",
                                 "subtotals"))
             ## transform.subtotals not one-to-one
             if (transformIsOneToOne(transform.subtotals))
                 stop(gettextf("'%s' has one-to-one relationship with '%s'",
                               "object", "subtotals"))
             ## length of 'subtotals' consistent with dimensions of 'metadataSubtotals'
             if (!identical(length(subtotals), as.integer(prod(dim(metadata.subtotals)))))
                 return(gettextf("length of '%s' inconsistent with dimensions of '%s'",
                                 "subtotals", "metadataSubtotals"))
             ## 'dimBefore' consistent with dim(object)
             if (!identical(transform.subtotals@dimBefore, dim(object)))
                 return(gettextf("'%s' from '%s' inconsistent with dimensions of '%s'",
                                 "dimBefore", "transformSubtotals", "object"))
             ## 'dimAfter' consistent with dim(metadataSubtotals)
             if (is.null(metadata.subtotals))
                 consistent <- identical(transform.subtotals@dimAfter, 1L)
             else
                 consistent <- identical(transform.subtotals@dimAfter, dim(metadata.subtotals))
             if (!consistent)
                 return(gettextf("'%s' from '%s' inconsistent with metadata for '%s'",
                                 "dimAfter", "transformSubtotals", "subtotals"))
             if (!is.null(metadata.subtotals)) {
                 ## names for object and subtotals consistent
                 names.subtotals <- names(metadata.subtotals)
                 inv.dims <- match(seq_along(names.subtotals), transform.subtotals@dims)
                 names.collapsed <- names(metadata)[inv.dims]
                 if (!identical(names.collapsed, names.subtotals))
                     return(gettextf("names for '%s' and '%s' inconsistent",
                                     "object", "subtotals"))
             }
             TRUE
         })

## HAS_TESTS
setClass("SubtotalsNet",
         slots = c(subtotalsNet = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             .Data <- object@.Data
             subtotalsNet <- object@subtotalsNet
             subtotals <- object@subtotals
             transform <- object@transformSubtotals
             ## 'subtotalsNet' has no missing values
             if (any(is.na(subtotalsNet)))
                 return(gettextf("'%s' has missing values",
                                 "subtotalsNet"))
             ## 'subtotalsNet' has no negative values
             if (any(subtotalsNet < 0L))
                 return(gettextf("'%s' has negative values",
                                 "subtotalsNet"))
             ## 'subtotalsNet' has same length as 'subtotals'
             if (!identical(length(subtotalsNet), length(subtotals)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "subtotalsNet", "subtotals"))
             ## 'subtotalsNet' equals 'subtotals' minus known elements from '.Data'
             .Data.zeros <- .Data
             .Data.zeros[is.na(.Data.zeros)] <- 0L
             .Data.zeros.collapsed <- collapse(.Data.zeros, transform = transform)
             subtotals.net.expected <- subtotals - .Data.zeros.collapsed
             if (!all(subtotalsNet == subtotals.net.expected, na.rm = TRUE))
                 return(gettextf("'%s', '%s', and '%s' inconsistent",
                                 "subtotalsNet", "subtotals", ".Data"))
             TRUE
         })

## HAS_TESTS
#' S4 Class For Counts with Subtotals.
#'
#' A \code{\linkS4class{Counts}} object with a set of subtotals attached.
#' Functions such as \code{\link{impute}} respect these subtotals.
#'
#' The counts must have missing values, and the subtotals must not.
#' All non-missing values must be non-negative integers.
#' 
#' @section Objects from the Class:
#' Objects are typically created by calls to \code{\link{attachSubtotals}}.
#'
#' @seealso
#' \code{impute}
#' @export
setClass("CountsWithSubtotals",
         contains = c("Counts", "HasSubtotals", "SubtotalsNet"))


## HAS_TESTS             
#' @rdname exported-not-api
#' @export
setClass("CountsWithSubtotalsInternal",
         contains = c("Counts", "HasSubtotals"),
         validity = function(object) {
             .Data <- object@.Data
             subtotals <- object@subtotals
             transform <- object@transformSubtotals
             .Data.collapsed <- collapse(.Data, transform = transform)
             if (!all(.Data.collapsed == subtotals, na.rm = TRUE))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 ".Data", "subtotals"))
             TRUE
         })
         
         

## SUMMARY ############################################################################

## HAS_TESTS
#' exported-not-api
#' @export
setClass("SummaryDemographicArray",
         slots = c(metadata = "MetaData",
             stats = "table"))



## DEMOGRAPHIC ACCOUNT ############################################################


## HAS_TESTS
setClass("IsInteger",
         contains = "VIRTUAL",
         validity = function(object) {
             if (!is.integer(object))
                 return(gettextf("does not have type \"%s\"",
                                 "integer"))
             TRUE
         })

## HAS_TESTS
setClass("IsDouble",
         contains = "VIRTUAL",
         validity = function(object) {
             if (!is.double(object))
                 return(gettextf("does not have type \"%s\"",
                                 "double"))
             TRUE
         })

## HAS_TESTS
setClass("NonNegative",
         contains = "VIRTUAL",
         validity = function(object) {
             if (any(object < 0L, na.rm = TRUE))
                 return(gettext("has negative values"))
             TRUE
         })

## HAS_TESTS
setClass("HasTime",
         contains = "VIRTUAL",
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             i.time <- match("time", dimtypes, nomatch = 0L)
             if (i.time == 0L)
                 return(gettextf("no dimension with dimtype \"%s\"",
                                 "time"))
             TRUE
         })

## HAS_TESTS
setClass("AgeIsIntervals",
         contains = "VIRTUAL",
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             DimScales <- DimScales(object, use.names = FALSE)
             i.age <- match("age", dimtypes, nomatch = 0L)
             if (i.age > 0L) {
                 DimScale.age <- DimScales[[i.age]]
                 if (!methods::is(DimScale.age, "Intervals"))
                     return(gettextf("dimension with dimtype \"%s\" has dimscale \"%s\"",
                                     "age", class(DimScale.age)))
             }
             TRUE
         })

## HAS_TESTS
setClass("AgeIsPoints",
         contains = "VIRTUAL",
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             DimScales <- DimScales(object, use.names = FALSE)
             i.age <- match("age", dimtypes, nomatch = 0L)
             if (i.age > 0L) {
                 DimScale.age <- DimScales[[i.age]]
                 if (!methods::is(DimScale.age, "Points"))
                     return(gettextf("dimension with dimtype \"%s\" has dimscale \"%s\"",
                                     "age", class(DimScale.age)))
             }
             TRUE
         })

## HAS_TESTS
setClass("AtLeastTwoAge",
         contains = "VIRTUAL",
         validity = function(object) {
             dim <- dim(object)
             dimtypes <- dimtypes(object, use.names = FALSE)
             i.age <- match("age", dimtypes, nomatch = 0L)
             if (i.age > 0L) {
                 n.age <- dim[i.age]
                 if (n.age < 2L)
                     return(gettextf("dimension with dimtype \"%s\" has length %d",
                                     "age", n.age))
             }
             TRUE
         })

## HAS_TESTS
setClass("AtMostOneSex",
         contains = "VIRTUAL",
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             n.sex <- sum(dimtypes == "sex")
             if (n.sex > 1L)
                 return(gettextf("%d dimensions with %s \"%s\"",
                                 n.sex, "dimtype", "sex"))
             TRUE
         })

## HAS_TESTS
setClass("FirstAgeIntervalClosed",
         contains = "VIRTUAL",
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             DimScales <- DimScales(object, use.names = FALSE)
             i.age <- match("age", dimtypes, nomatch = 0L)
             if (i.age > 0L) {
                 DimScale.age <- DimScales[[i.age]]
                 dimvalues.age <- dimvalues(DimScale.age)
                 if (is.infinite(dimvalues.age[1L]))
                     return(gettextf("first interval of dimension with dimtype \"%s\" is open",
                                     "age"))
             }
             TRUE
         })

## HAS_TESTS
setClass("LastAgeIntervalOpen",
         contains = "VIRTUAL",
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             DimScales <- DimScales(object, use.names = FALSE)
             i.age <- match("age", dimtypes, nomatch = 0L)
             if (i.age > 0L) {
                 DimScale.age <- DimScales[[i.age]]
                 n.age <- length(DimScale.age)
                 dimvalues.age <- dimvalues(DimScale.age)
                 if (is.finite(dimvalues.age[n.age + 1L]))
                     return(gettextf("last interval of dimension with dimtype \"%s\" is closed",
                                     "age"))
             }
             TRUE
         })

## HAS_TESTS
setClass("IsRegular",
         contains = "VIRTUAL",
         validity = function(object) {
             ## regular age-time plan
             return.value <- tryCatch(hasRegularAgeTime(object),
                                      error = function(e) e)
             if (!isTRUE(return.value))
                 return(gettextf("does not have regular age-time plan : %s",
                                 return.value$message))
             TRUE
         })         

## HAS_TESTS
setClass("NoCohort",
         contains = "VIRTUAL",
         validity = function(object) {
             if ("cohort" %in% dimtypes(object))
                 return(gettextf("has dimension with dimtype \"%s\"",
                                 "cohort"))
             TRUE
         })

## HAS_TESTS             
setClass("HasTriangle",
         contains = "VIRTUAL",
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             ## has dimtype "triangle" if has dimtype "age"
             ## (existing tests for DemographicArray object ensure
             ## that has dimtype "age" if has dimtype "triangle")
             has.age <- "age" %in% dimtypes
             has.triangle <- "triangle" %in% dimtypes
             if (has.age & !has.triangle)
                 return(gettextf("has dimension with dimtype \"%s\" but no dimension with dimtype \"%s\"",
                                 "age", "triangle"))
             TRUE
         })

## HAS_TESTS
setClass("NoTriangle",
         contains = "VIRTUAL",
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             if ("triangle" %in% dimtypes)
                 return(gettextf("has dimension with dimtype \"%s\"",
                                 "triangle"))
             TRUE
         })

## HAS_TESTS
setClass("HasOrigDest",
         contains = "VIRTUAL",
         validity = function(object) {
             names <- names(object)
             dimtypes <- dimtypes(object, use.names = FALSE)
             DimScales <- DimScales(object, use.names = FALSE)
             is.origin <- dimtypes == "origin"
             ## has "origin", "destination" dimensions
             if (!any(is.origin))
                 return(gettextf("no dimensions with dimtype \"%s\" or \"%s\"",
                                 "origin", "destination"))
             ## origin and destination dimensions use same categories
             i.orig <- which(is.origin)
             names.orig <- names[i.orig]
             names.dest <- getNamesPairs(names.orig)
             i.dest <- match(names.dest, names)
             for (i in seq_along(i.orig)) {
                 ip <- i.orig[i]
                 ic <- i.dest[i]
                 DS.orig <- DimScales[[ip]]
                 DS.dest <- DimScales[[ic]]
                 dv.orig <- dimvalues(DS.orig)
                 dv.dest <- dimvalues(DS.dest)
                 if (!identical(dv.orig, dv.dest))
                     return(gettextf("dimensions \"%s\" and \"%s\" use different categories",
                                     names.orig[i], names.dest[i]))
             }
             TRUE
         })

## HAS_TESTS
setClass("NoOrigDest",
         contains = "VIRTUAL",
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             if ("origin" %in% dimtypes)
                 return(gettextf("has dimension with dimtype \"%s\"",
                                 "origin"))
             TRUE
         })

## HAS_TESTS
setClass("HasParentChild",
         contains = "VIRTUAL",
         validity = function(object) {
             names <- names(object)
             dimtypes <- dimtypes(object, use.names = FALSE)
             DimScales <- DimScales(object, use.names = FALSE)
             ## has "parent", "child" dimensions
             is.parent <- dimtypes == "parent"
             if (!any(is.parent))
                 return(gettextf("no dimensions with dimtype \"%s\" or \"%s\"",
                                 "parent", "child"))
             ## parent and child dimensions use same categories
             i.parent <- which(dimtypes == "parent")
             names.parent <- names[i.parent]
             names.child <- getNamesPairs(names.parent)
             i.child <- match(names.child, names)
             for (i in seq_along(i.parent)) {
                 ip <- i.parent[i]
                 ic <- i.child[i]
                 DS.parent <- DimScales[[ip]]
                 DS.child <- DimScales[[ic]]
                 dv.parent <- dimvalues(DS.parent)
                 dv.child <- dimvalues(DS.child)
                 if (!identical(dv.parent, dv.child))
                     return(gettextf("dimensions \"%s\" and \"%s\" use different categories",
                                     names.parent[i], names.child[i]))
             }
             TRUE
         })

## HAS_TESTS
setClass("NoParentChild",
         contains = "VIRTUAL",
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             if ("parent" %in% dimtypes)
                 return(gettextf("has dimension with dimtype \"%s\"",
                                 "parent"))
             TRUE
         })

## HAS_TESTS
setClass("IBetween",
         slots = c(iBetween = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iBetween <- object@iBetween
             dim <- dim(object)
             names <- names(object)
             dimtypes <- dimtypes(object, use.names = FALSE)
             DimScales <- DimScales(object, use.names = FALSE)
             ## 'iBetween' has positive length
             if (identical(length(iBetween), 0L))
                 return(gettextf("'%s' has length %d",
                                 "iBetween", 0L))
             ## 'iBetween' has no missing values
             if (any(is.na(iBetween)))
                 return(gettextf("'%s' has missing values",
                                 "iBetween"))
             ## 'iBetween' indexes dimensions
             if (!all(iBetween %in% seq_along(dim)))
                 return(gettextf("'%s' outside valid range",
                                 "iBetween"))
             for (i in iBetween) {
                 ## 'between' dimensions have length of at least 2
                 if (dim[i] < 2L)
                     return(gettextf("\"%s\" dimension \"%s\" has length %d",
                                     "between", names[i], dim[i]))
                 ## 'between' dimensions have dimtype "state"
                 if (!identical(dimtypes[i], "state"))
                     return(gettextf("\"%s\" dimension \"%s\" has dimtype \"%s\"",
                                     "between", names[i], dimtypes[i]))             
             }
             TRUE
         })

## HAS_TESTS
setClass("IDirection",
         slots = c(iDirection = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             kValidDimvalues <- c("Out", "In")
             iDirection <- object@iDirection
             iBetween <- object@iBetween
             names <- names(object)
             dim <- dim(object)
             dimtypes <- dimtypes(object, use.names = FALSE)
             DimScales <- DimScales(object, use.names = FALSE)
             ## 'iDirection' has length 1
             if (!identical(length(iDirection), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "iDirection", 1L))
             ## 'iDirection' is not missing
             if (is.na(iDirection))
                 return(gettextf("'%s' is missing",
                                 "iDirection"))
             ## 'iDirection' indexes a dimension
             if (!(iDirection %in% seq_along(dim)))
                 return(gettextf("'%s' outside valid range",
                                 "iDirection"))
             ## 'direction' dimension has length 2
             if (!identical(dim[iDirection], 2L))
                 return(gettextf("\"%s\" dimension does not have length %d",
                                 "direction", 2L))
             ## 'direction' dimension has dimtype "state"
             if (!identical(dimtypes[iDirection], "state"))
                 return(gettextf("\"%s\" dimension has dimtype \"%s\"",
                                 "direction", dimtypes[iDirection]))             
             ## 'direction' dimension has categories "Out", "In"
             if (!identical(dimvalues(DimScales[[iDirection]]), kValidDimvalues))
                 return(gettextf("\"%s\" dimension does not have categories \"%s\", \"%s\"",
                                 "direction",
                                 kValidDimvalues[1L],
                                 kValidDimvalues[2L]))
             ## 'iBetween' not equal to 'iDirection'
             if (any(iBetween == iDirection))
                 return(gettextf("'%s' and '%s' overlap",
                                 "iBetween", "iDirection"))
             TRUE
         })

## HAS_TESTS
setClass("IMinAge",
         slots = c(iMinAge = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iMinAge <- object@iMinAge
             dimtypes <- dimtypes(object, use.names = FALSE)
             has.age <- "age" %in% dimtypes
             ## 'iMinAge' has length 1
             if (!identical(length(iMinAge), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "iMinAge", 1L))
             if (has.age) {
                 ## if has.age: 'iMinAge' is not missing
                 if (is.na(iMinAge))
                     return(gettextf("'%s' is missing",
                                     "iMinAge"))
                 ## if has.age: 'minAge' positive
                 if (iMinAge <= 1L)
                     return(gettextf("'%s' is less than %d",
                                     "iMinAge", 2L))
             }
             else {
                 ## if !has.age: 'minAge' is missing
                 if (!is.na(iMinAge))
                     return(gettextf("no dimension with dimtype \"%s\" but '%s' is not missing",
                                     "age", "iMinAge"))
             }
             TRUE
         })

## HAS_TESTS
setClass("InsEqualOuts",
         contains = c("VIRTUAL", "IBetween", "IDirection"),
         validity = function(object) {
             iDirection <- object@iDirection
             iBetween <- object@iBetween
             ## ins equal outs
             object <- new("Counts",
                           .Data = object@.Data,
                           metadata = object@metadata)
             outs <- slab(object, dimension = iDirection, elements = 1L, drop = FALSE)
             ins <- slab(object, dimension = iDirection, elements = 2L, drop = FALSE)
             outs <- collapseDimension(outs, dimension = iBetween)
             ins <- collapseDimension(ins, dimension = iBetween)
             outs <- as.integer(outs)
             ins <- as.integer(ins)
             obs <- !is.na(ins) & !is.na(outs)
             if (!identical(ins[obs], outs[obs]))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "ins", "outs"))
             TRUE
         })

## HAS_TESTS
setClass("NetSumsToZero",
         contains = c("VIRTUAL", "IBetween"),
         validity = function(object) {
             iBetween <- object@iBetween
             ## sums across "between" dimensions equal 0
             sums <- collapseDimension(object, dimension = iBetween)
             if (any(sums[!is.na(sums)] != 0))
                 return(gettextf("\"%s\" dimensions do not sum to 0",
                                 "between"))
             TRUE
         })             

## HAS_TESTS
setClass("TimeIsIntervals",
         contains = "VIRTUAL",
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             DimScales <- DimScales(object, use.names = FALSE)
             i.time <- match("time", dimtypes, nomatch = 0L)
             if (i.time > 0L) {
                 DimScale.time <- DimScales[[i.time]]
                 if (!methods::is(DimScale.time, "Intervals"))
                     return(gettextf("dimension with dimtype \"%s\" has dimscale \"%s\"",
                                     "time", class(DimScale.time)))
             }
             TRUE
         })

## HAS_TESTS
setClass("TimeIsPoints",
         contains = "VIRTUAL",
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             DimScales <- DimScales(object, use.names = FALSE)
             i.time <- match("time", dimtypes, nomatch = 0L)
             if (i.time > 0L) {
                 DimScale.time <- DimScales[[i.time]]
                 if (!methods::is(DimScale.time, "Points"))
                     return(gettextf("dimension with dimtype \"%s\" has dimscale \"%s\"",
                                     "time", class(DimScale.time)))
             }
             TRUE
         })

#' Classes to summarise origin-destination flows.
#' 
#' Classes to describe net flows, or 'in' and 'out' flows, typically to be
#' supplied to function \link{Movements}.
#'
#' @aliases Net-class Pool-class
#' 
#' @name net-pool-classes
NULL

## HAS_TESTS
#' @rdname net-pool-classes
#' @export
setClass("Net",
         contains = c("Counts",
                      "NoOrigDest",
                      "NoParentChild",
                      "NetSumsToZero"))

## HAS_TESTS
#' @rdname net-pool-classes
#' @export
setClass("Pool",
         contains = c("Counts",
                      "NoOrigDest",
                      "NoParentChild",
                      "NonNegative",
                      "InsEqualOuts"))


#' Classes used by DemographicAccount.
#'
#' These classes are used internally by DemographicAccount.  They should never
#' be used directly by end users.  The only reason they are exported is to
#' allow other developers to extend classes "Counts" and "Values".
#'
#' @keywords internal
#' @name internal-account

#' @rdname internal-account
NULL

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("Population",
         contains = c("Counts",
                      "IsInteger",
                      "NonNegative",
                      "HasTime",
                      "AgeIsIntervals",
                      "AtLeastTwoAge",
                      "AtMostOneSex",
                      "FirstAgeIntervalClosed",
                      "LastAgeIntervalOpen",
                      "NoCohort",
                      "IsRegular",
                      "NoOrigDest",
                      "NoParentChild",
                      "NoTriangle",
                      "TimeIsPoints"),
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             DimScales <- DimScales(object, use.names = FALSE)
             ## time dimension has dimscale "Points"
             i.time <- match("time", dimtypes)
             DimScale.time <- DimScales[[i.time]]
             if (!methods::is(DimScale.time, "Points"))
                 return(gettextf("dimension with dimtype \"%s\" has dimscale \"%s\"",
                                 "time", class(DimScale.time)))
             ## "time" dimension has at least 2 points
             if (length(DimScale.time) < 2L)
                 return(gettextf("dimension with dimtype \"%s\" has length %d",
                                 "time", length(DimScale.time)))
             TRUE
         })

#' @rdname internal-account
#' @export
setClass("Component",
         contains = c("VIRTUAL",
                      "Counts",
                      "HasTime",
                      "IsInteger",
                      "AgeIsIntervals",
                      "AtMostOneSex",
                      "FirstAgeIntervalClosed",
                      "IsRegular",
                      "NoCohort",
                      "TimeIsIntervals"))

setClass("MovementsComponent",
         contains = c("VIRTUAL",
                      "HasTriangle"))

## HAS_TESTS
setClass("TransitionsComponent",
         contains = c("VIRTUAL",
                      "NonNegative",
                      "NoTriangle"),
         validity = function(object) {
             dimtypes <- dimtypes(object, use.names = FALSE)
             DimScales <- DimScales(object, use.names = FALSE)
             ## time dimension has dimscale "Intervals"
             ## (test not needed by MovementsComponent class, since
             ## implied by existince of triangle dimension)
             i.time <- match("time", dimtypes)
             DimScale.time <- DimScales[[i.time]]
             if (!methods::is(DimScale.time, "Intervals"))
                 return(gettextf("dimension with dimtype \"%s\" has dimscale \"%s\"",
                                 "time", class(DimScale.time)))
             TRUE
         })

#' @rdname internal-account
#' @export
## HAS_TESTS
setClass("Births",
         contains = c("VIRTUAL",
                      "Component",
                      "IMinAge"))

#' @rdname internal-account
#' @export
setClass("BirthsMovements",
         contains = c("VIRTUAL",
                      "Births",
                      "MovementsComponent",
                      "NonNegative",
                      "NoOrigDest"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("BirthsMovementsHasParentChild",
         contains = c("BirthsMovements",
                      "HasParentChild"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("BirthsMovementsNoParentChild",
         contains = c("BirthsMovements", "NoParentChild"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("BirthsTransitions",
         contains = c("Births", "TransitionsComponent"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("BirthsTransitionsHasParentChild",
         contains = c("BirthsTransitions", "HasParentChild"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("BirthsTransitionsNoParentChild",
         contains = c("BirthsTransitions", "NoParentChild"))

#' @rdname internal-account
#' @export
setClass("Internal",
         contains = c("VIRTUAL", "Component", "NoParentChild"))

#' @rdname internal-account
#' @export
setClass("InternalMovements",
         contains = c("VIRTUAL", "Internal", "MovementsComponent"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("InternalMovementsNet",
         contains = c("InternalMovements", "NoOrigDest", "NetSumsToZero"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("InternalMovementsOrigDest",
         contains = c("InternalMovements", "HasOrigDest", "NonNegative"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("InternalMovementsPool",
         contains = c("InternalMovements", "NoOrigDest", "NonNegative",
             "InsEqualOuts"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("InternalTransitions",
         contains = c("Internal", "TransitionsComponent", "HasOrigDest"))

#' @rdname internal-account
#' @export
setClass("Entries",
         contains = c("VIRTUAL", "Component", "NoParentChild"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("EntriesMovements",
         contains = c("Entries", "MovementsComponent", "NonNegative", "NoOrigDest"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("EntriesTransitions",
         contains = c("Entries", "TransitionsComponent"))

#' @rdname internal-account
#' @export
setClass("Exits",
         contains = c("VIRTUAL", "Component", "NoParentChild"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("ExitsMovements",
         contains = c("Exits", "MovementsComponent", "NonNegative", "NoOrigDest"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("ExitsTransitions",
         contains = c("Exits", "TransitionsComponent"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("NetMovements",
         contains = c("Component", "MovementsComponent", "NoOrigDest"))

#' @rdname internal-account
#' @export
## HAS_TESTS
setClass("Accession",
         contains = c("Counts",
                      "HasTime",
                      "IsInteger",
                      "AgeIsPoints",
                      "IsRegular",
                      "NoCohort",
                      "NonNegative",
                      "NoOrigDest",
                      "NoParentChild",
                      "NoTriangle"))

## HAS_TESTS
#' @rdname internal-account
#' @export
setClass("Exposure",
         contains = c("Counts",
                      "HasTime",
                      "IsDouble",
                      "AgeIsIntervals",
                      "FirstAgeIntervalClosed",
                      "IsRegular",
                      "NoCohort",
                      "NonNegative",
                      "NoOrigDest",
                      "NoParentChild",
                      "HasTriangle"))


## HAS_TESTS
#' Classes "DemographicAccount", "Movements", "Transitions"
#'
#' Classes for describing demographic accounts.
#'
#' There are two subclasses: Movements and Transitions
#'
#' @name DemographicAccount-class
#' @export
setClass("DemographicAccount",
         slots = c(population = "Population",
                   components = "list",
                   namesComponents = "character"),
         contains = "VIRTUAL",
         validity = function(object) {
             population <- object@population
             components <- object@components
             namesComponents <- object@namesComponents
             ## population is valid
             value <- tryCatch(validObject(population),
                               error = function(e) e)
             if (is(value, "error"))
                 return(value$message)
             ## components has positive length
             if (identical(length(components), 0L))
                 return(gettextf("'%s' has length %d",
                                 "components", 0L))
             n.births <- sum(sapply(components, methods::is,"Births"))
             ## components all valid
             for (component in components) {
                 value <- tryCatch(validObject(component),
                                   error = function(e) e)
                 if (is(value, "error"))
                     return(value$message)
             }
             ## no more than one births
             if (n.births > 1L)
                 return(gettextf("more than one component with class \"%s\"",
                                 "Births"))
             else {
                 ## if has births, first age group starts at 0
                 if (n.births == 1L) {
                     i.age <- match("age", dimtypes(population), nomatch = 0L)
                     has.age <- i.age > 0L
                     if (has.age) {
                         DS.age <- DimScales(population)[[i.age]]
                         min.age <- dimvalues(DS.age)[1L]
                         if (!isTRUE(all.equal(min.age, 0L)))
                             return(gettextf("has component with class \"%s\", but minimum age for population is not 0",
                                             "Births"))
                     }
                 }
             }
             ## no more than one component with class "Internal"
             n.internal <- sum(sapply(components, methods::is,"Internal"))
             if (n.internal > 1L)
                 return(gettextf("more than one component with class \"%s\"",
                                 "Internal"))
             ## 'namesComponents' has no missing values
             if (any(is.na(namesComponents)))
                 return(gettextf("'%s' has missing values",
                                 "namesComponents"))
             ## 'namesComponents' has no blanks
             if (!all(nzchar(namesComponents)))
                 return(gettextf("'%s' has blanks",
                                 "namesComponents"))
             ## 'namesComponents' has no duplicates
             if (any(duplicated(namesComponents)))
                 return(gettextf("'%s' has duplicates",
                                 "namesComponents"))
             ## 'components' and 'namesComponents' have same length
             if (!identical(length(components), length(namesComponents)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "components", "namesComponents"))
             TRUE
         })

#' @rdname DemographicAccount-class
## HAS_TESTS
setClass("Movements",
         contains = "DemographicAccount",
         validity = function(object) {
             population <- object@population
             components <- object@components
             namesComponents <- object@namesComponents
             ## all components have class "MovementsComponent"
             if (!all(sapply(components, methods::is,"MovementsComponent")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "components", "MovementsComponent"))
             ## all elements of 'components' compatible with 'population'
             template <- makeTemplateComponent(population)
             metadata.template <- metadata(template)
             for (i in seq_along(components)) {
                 component <- components[[i]]
                 name.component <- namesComponents[i]
                 return.value <- isCompatibleWithPopn(component = component,
                                                      metadata = metadata.template,
                                                      name = name.component)
                 if (!isTRUE(return.value))
                     return(return.value)
             }
             TRUE
         })

#' @rdname DemographicAccount-class
## HAS_TESTS
setClass("Transitions",
         contains = "DemographicAccount",
         validity = function(object) {
             population <- object@population
             components <- object@components
             namesComponents <- object@namesComponents
             ## all components have class "TransitionsComponent"
             if (!all(sapply(components, methods::is,"TransitionsComponent")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "components", "TransitionsComponent"))
             ## all elements of 'components' compatible with 'population'
             template <- makeTemplateComponent(population,
                                               triangles = FALSE)
             metadata.template <- metadata(template)
             for (i in seq_along(components)) {
                 component <- components[[i]]
                 name.component <- namesComponents[i]
                 return.value <- isCompatibleWithPopn(component = component,
                                                      metadata = metadata.template,
                                                      name = name.component)
                 if (!isTRUE(return.value))
                     return(return.value)
             }
             TRUE
         })




