

## Existing functions ##########################################################################

#' Internal methods
#'
#' Internal methods for existing functions
#'
#' @param e1 First element in operation or comparison.
#' @param e2 Second element in operation or comparison.
#' @param x Typically an object of class \code{\linkS4class{DemographicArray}}.
#' @param i Index.
#' @param j Index.
#' @param \dots Other arguments.
#' @param drop Logical. Whether to drop dimensions of length 1.
#' @param object Typically an object of class
#' \code{\linkS4class{DemographicArray}}.
#' @param a Typically an array.
#' @param perm A vector of indices or names specifying the permutation.
#' @param resize Logical.  Whether the object should be resized as well
#' as having its elements rearranged.
#' @param keep.class  Logical.  Whether the result should has the same
#' class as the original.
#' @param use.names Logical. Whether the result should include the names
#' of dimensions.
#' @param obj Object of class \code{\linkS4class{DemographicArray}}.
#' @param force A standard argument for \code{\link[base]{unname}} but
#' irrelevant for arrays.
#' @param table Object of class \code{"DimScale"}.
#' @param value Replacement value.
#' 
#' @name internal-methods
NULL

#' Existing statistical functions.
#'
#' Existing statistical functions give the same result when used with a
#' \code{\linkS4class{DemographicArray}} array as they would with an
#' ordinary \code{\link[base]{array}} - except when the demographic array
#' has a dimension with \code{\link{dimtype}} \code{"iteration"} or
#' \code{"quantile"}.  When there is an \code{"iteration"} dimension,
#' the calculations are repeated once for each iteration, so that the
#' return value is also a demographic array with an \code{"iteration"}
#' dimension.
#'
#' When called on an ordinary array, the functions all return a single
#' number, except for \code{range}, which returns 2.  When called on
#' a demographic array with an \code{"iteration"} dimension, \code{range}
#' returns a demographic array with a dimension called \code{"range"},
#' in addition to the \code{"iteration"} dimension.
#'
#' When a demographic array has a dimension with \code{\link{dimtype}}
#' \code{"quantile"}, applying statistical functions typically does
#' not make sense, and an error is raised.  It is generally better to work
#' with the original array, with the complete set of iterations, and to
#' reduce to quantiles once the calculations are complete.
#'
#' @param x Typically an object of class \code{\linkS4class{DemographicArray}}.
#' @param center Function used to calculate central value.  Defaults
#' to \code{\link[stats]{median}}.
#' @param constant See documentation for \code{\link[stats]{mad}}.
#' @param na.rm Logical. Whether to remove missing values before calculating.
#' @param low See documentation for \code{\link[stats]{mad}}.
#' @param high See documentation for \code{\link[stats]{mad}}.
#' @param \dots Other arguments.
#'
#' @return Numeric, if \code{x} does not contain an \code{"iteration"}
#' dimension, and \code{\link{Counts}} or \code{\link{Values}} if it
#' does.
#'
#' @seealso Iteration and quantile dimensions are described in the
#' documentation for \code{\link{dimtypes}}.  To remove iteration
#' dimensions (by summarising across them) use
#' \code{\link{collapseIterations}}.
#'
#' @examples
#' ## no iteration dimension
#' x1 <- Counts(array(1:4,
#'                    dim = 4,
#'                    dimnames = list(region = c("A", "B", "C", "D"))))
#' ## with iteration dimension
#' x2 <- Counts(array(c(1:4, 0:3, 2:5),
#'                    dim = c(4, 3),
#'                    dimnames = list(region = c("A", "B", "C", "D"),
#'                                    iteration = 1:3)))
#' x1
#' x2
#' sum(x1)
#' sum(x2)
#' range(x1)
#' range(x2)
#' @name Statistical-Functions
NULL

setGeneric("%in%")

setGeneric("aperm")

setGeneric("apply")

setGeneric("as.array")

#' Convert a DemographicArray Object to a Data Frame
#'
#' Convert a \code{"\linkS4class{DemographicArray}"} object to a data frame.  The
#' data frame can be a "wide" or "long" format.
#'
#' If \code{direction} is \code{"wide"}, then \code{object} is flattened into a
#' matrix before conversion to a data frame, which is what the method for
#' ordinary arrays does.  If \code{direction} is \code{"long"}, then
#' \code{object} is put into "long" format, with a column to represent each
#' dimension, plus a final column holding the data.  The final column is called
#' \code{"count"} if \code{object} has class \code{\linkS4class{Counts}} and
#' \code{"value"} if \code{object} has class \code{\linkS4class{Counts}}.
#'
#' If \code{midpoints} is \code{FALSE}, interval labels are converted into
#' factors.  If \code{midpoints} is \code{TRUE}, interval labels are replaced
#' by numbers giving their midpoints.  If names of individual dimensions are
#' supplied, then only the named dimensions are converted.
#'
#' @param x Object of class \code{\linkS4class{DemographicArray}}.
#' @param row.names 'NULL' or a character vector giving the row names for the
#' data frame.  See \code{\link[base]{as.data.frame}}.
#' @param optional Logical.  See \code{\link[base]{as.data.frame}}.
#' @param direction Logical.  Governs whether data are converted to
#' \code{"wide"} (the default) or \code{"long"} format.
#' @param stringsAsFactors Logical.  If \code{TRUE} (the default) factors are
#' used to represent qualitative variables.
#' @param responseName Name of column with numeric values when \code{direction}
#' is \code{"long"}.  Defaults to \code{"count"} if \code{object} is
#' \code{\linkS4class{Counts}} and \code{"value"} if \code{object} is
#' \code{\linkS4class{Values}}.
#' @param midpoints \code{FALSE} (the default), \code{TRUE}, or names of
#' dimensions with \code{\link{dimscale}} \code{"Intervals"}.
#' @param \dots Additional arguments to be passed to or from methods.
#' @return A data frame.
#' @seealso \code{\link[base]{as.data.frame}},
#' \code{\link[base]{as.data.frame.table}}, \code{\link[base]{data.frame}},
#' \code{\link{midpoints}}
#' @examples
#' library(demdata)
#' popn <- Counts(VAPopn)
#' as.data.frame(popn)
#' as.data.frame(popn, direction = "long")
#'
#' x <- Values(array(1:4,
#'                   dim = c(2, 2),
#'                   dimnames = list(age = c("0-39", "40+"),
#'                       period = c("2001-2010", "2011-2020"))))
#' as.data.frame(x, direction = "long")
#' as.data.frame(x, direction = "long", midpoints = TRUE)
#' as.data.frame(x, direction = "long", midpoints = "period")
#' @name as.data.frame
NULL

setGeneric("as.data.frame")

setGeneric("as.matrix")

setGeneric("diff")

#' Drop dimensions of length one.
#'
#' Like the \code{\link{drop}} function for ordinary arrays, the method for
#' \code{\linkS4class{DemographicArray}} objects removes dimensions with length 1.
#'
#' A complication sometimes arises when dropping dimensions of a
#' demographic array that does not arise when dropping dimensions of
#' an ordinary array.  When only one dimension from an origin-destination or
#' parent-child pair is dropped, the \code{\link{dimtype}} of the remaining
#' dimension changes to \code{"state"} and the suffix is dropped from its name.
#'
#' @param x Object of class \code{\linkS4class{DemographicArray}}.
#' @seealso \code{\link[base]{drop}}
#' @examples
#' library(demdata)
#' popn <- Counts(VAPopn)
#' popn <- subarray(popn, sex == "Female", drop = FALSE)
#' drop(popn)
#'
#' mig <- Counts(nz.mig)
#' mig <- subarray(mig, island_dest == "North Island", drop = FALSE)
#' ## note the effect on dimension "island_orig"
#' ## of dropping dimension "island_dest"
#' drop(mig)
#' @name drop
NULL

setGeneric("drop")

setGeneric("labels")

setGeneric("mad")

setGeneric("mean")

setGeneric("median")

#' Plot a demographic array
#'
#' Methods for function \code{\link{plot}} which aim to show the main features
#' of a demographic array.  Different formats are used
#' for objects of class \code{\linkS4class{Counts}} and
#' \code{\linkS4class{Values}}.
#'
#' @param x Object of class \code{\linkS4class{DemographicArray}}.
#' @param threshold An integer.  If the number of values being plotted per
#' category is less than or equal to \code{threshold} then a boxplot is used;
#' otherwise a stripchart is used.
#' @param main Title for plot.
#' @param cex.main,col.main,font.main The 'character expansion factor', colour,
#' and font to be used for \code{main}.
#' @param las Parameter governing orientation of axis labels.  One of \code{0},
#' \code{1}, \code{2}, or \code{3}.  See \code{\link[graphics]{par}}.
#' @param \dots Other arguments, which are passed to the underlying plotting
#' functions.
#' @seealso \code{plot} calls functions \code{\link{barchart}},
#' \code{\link{boxplot}}, and \code{\link{stripchart}}
#' @examples
#' library(demdata)
#' popn <- Counts(VAPopn)
#' plot(popn)
#'
#' rates <- Values(VADeaths2)
#' plot(rates)
#' @name plot-methods
NULL

setGeneric("plot")

#' Calculate proportions within dimensions of a demographic array.
#'
#' The is a method for function \code{\link[base]{prop.table}} from package
#' \pkg{base}.  Unlike the original function, dimension names can be used in
#' place of dimension indices.
#'
#' If \code{x} has a dimension with \code{\link{dimtype}} \code{"Iterations"},
#' and that dimension is not explicitly included in \code{margin}, then it is
#' added automatically.
#' @docType methods
#' @param x Object of class \code{\linkS4class{DemographicArray}}.
#' @param margin Indices or names of dimensions within which proportions are to
#' be calculated.
#' @return Object of class \code{\linkS4class{Values}}.
#' @examples
#' library(demdata)
#' x <- Counts(VAPopn)
#' prop.table(x, margin = 2:4)
#' ## the equivalent, using dimension names
#' prop.table(x, margin = c("sex", "residence", "color"))
setGeneric("prop.table")

setGeneric("summary")

setGeneric("t")

setGeneric("unname")


## New functions ########################################################################

#' Create objects of class "Counts" or "Values".
#'
#' Create objects of class \code{\linkS4class{Counts}} or \code{\linkS4class{Values}}
#' from an array-like object with a full set of dimnames.
#'
#' A full set of dimnames means that \code{dimnames(object)} is a named list with
#' no \code{NA}s or (unless the dimension has length 0) \code{NULL}s.
#' In addition, within each dimension there should be no repeated dimnames.
#'
#' If the dimnames for a dimension look like labels for points or intervals,
#' but are not in numeric order, then \code{Counts} and \code{Values} will
#' permute rows to put them in order.  The most common reason for having interval labels
#' out of order is that \code{xtabs} has sorted them lexicographically.
#' See below for an example.
#'
#' Functions \code{Counts} and \code{Values} infer dimtypes and dimscales from
#' \code{dimnames(object)}, plus the \code{dimtypes} and \code{dimscales} arguments,
#' if supplied.  Some ways to assist this process are:
#' \itemize{
#'   \item If the \code{dimtypes} argument is not used, then dimensions should have
#' names that match the dimtypes, or are equivalent, such as \code{"time"} (the dimtype itself),
#' \code{"Year"}, or \code{"period"} (two equivalents).  Case is ignored.
#' If \code{Count} or \code{Values} do not recognize a dimension name,
#' they assume the dimtype is \code{"state"}.
#' \item Age labels should follow the style \code{"<0"}, \code{"0"}, \code{"1-4"},
#' \code{"5-9"}, \code{"10+"}.  \code{"<a"} means the interval \code{(Inf, a)}.
#' When all breaks are integers, \code{"a0-a1"} means the interval between exact ages
#' \code{a0} and \code{a1+1}, and \code{"a"} means the interval between exact ages
#' \code{a} and \code{a+1}. \code{Counts} and \code{Values} will in fact recognize
#' some variations on the standard symbols, but in non-interactive use it is
#' safest to use the standard ones.
#' \item Time labels follow a different style from age labels: when all breaks are integers,
#' the lower limit of each interval is increased by one, instead of the upper limit
#' being reduced by one.  Thus, for example, \code{"2001-2005"} means the interval
#' between exact times \code{2000} and \code{2005}, and \code{"2001"} means interval
#' between exact times \code{2000} and \code{2001}.
#'  \item \code{Counts} and \code{Values} assume that a set of intervals are referring
#' to age if the maximum (finite) break is less than 1000, and are referring to time otherwise.
#'  \item If dimnames for a dimension consist of consecutive integers,such as \code{"0"},
#' \code{"1"}, \code{"2"}, then \code{Counts} and
#'  \code{Values} cannot tell whether the dimscale should be \code{"Intervals"}
#' or \code{"Points"}.  If a dimension has dimtype \code{"age"}, the
#' default is to assume that the dimscale is \code{"Intervals"}, though
#' with a message informing the user that this assumption has been made.
#' If a dimension has dimtype \code{"time"}, an error is raised.
#'  \item As discussed in \code{\link{dimtypes}}, some dimensions come in pairs.
#' The pairs are identified using suffixes \code{"_orig"} and \code{"_dest"}
#' (for \code{"origin"} and \code{"destination"} dimensions) and \code{"_parent"}
#' and \code{"_child"} (for \code{"parent"} and \code{"child"} dimensions).
#' As discussed in \code{\linkS4class{DemographicArray}}, there on restrictions
#' on which dimscales may be used with which dimtypes.  See the Titanic example below.
#' }
#'
#' @param object An array, an array-like object such as a table, or a vector.
#' The object must have a full set of names and dimnames (see below for details.)
#' @param dimtypes A named character vector describing the \code{\link{dimtypes}}
#' of one or more dimensions.
#' @param dimscales A named character vector describing the \code{\link{dimscales}}
#' of one or more dimensions.
#'
#' @return An object of class \code{\linkS4class{Counts}} or \code{\linkS4class{Values}}.
#'
#' @seealso \code{\linkS4class{Counts}}, \code{\linkS4class{Values}},
#' \code{\link{dimtypes}}, \code{\link{dimscales}}
#'
#' @aliases Counts Values
#' 
#' @examples
#' a <- array(rpois(n = 8, lambda = 10),
#'           dim = c(2, 2, 2),
#'           dimnames = list(age = c("0-39", "40+"),
#'                           sex = c("Female", "Male"),
#'                           year = c("2001-2010", "2011-2020")))
#' a
#' Counts(a)
#' names(dimnames(a)) <- c("aGe", "SEX", "perioD")
#' Counts(a)
#'
#' a <- array(rnorm(n = 5), dim = 5, dimnames = list(age = 0:4))
#' a
#' Values(a)
#' Values(a, dimscales = c(age = "Points"))
#'
#' Counts(occupationalStatus)
#'
#' Titanic
#' \dontrun{Counts(Titanic)} ## fails
#' ## 'Counts' assumes that the dimension called "Age"
#' ## has dimtype "age", but is then unable to derive
#' ## "Points" or "Intervals" dimscales from categories
#' ## "Child" and "Adult".
#'
#' ## One solution is to rename the "Age" dimension
#' ## to something that Counts does not recognize
#' ## as an age group
#' Titanic2 <- Titanic
#' names(dimnames(Titanic2))[3] <- "AgeGp"
#' Counts(Titanic2)
#'
#' ## A better solution is to explicitly set the dimtypes
#' Counts(Titanic, dimtypes = c(Age = "state"))
#'
#' ## DemographicArray objects can be created from numeric vectors,
#' ## provided that the vectors have enough names
#' births <- c(Males = 105, Females = 100)
#' Counts(births)
#'
#' ## 'xtabs' gets the labels in the wrong order,
#' ## but 'Counts' put them back into the right order
#' d <- data.frame(age = c("0-4", "5-9", "10+", "0-4", "5-9", "10+"),
#'                 sex = c("F", "F", "F", "M", "M", "M"),
#'                 count = 1:6)
#' xt <- xtabs(count ~ age + sex, data = d)
#' x <- Counts(xt)
#' @name counts-values-generator
NULL

#' @export
#' @rdname counts-values-generator
setGeneric("Counts",
           function(object, dimtypes = NULL, dimscales = NULL)
               standardGeneric("Counts"))

#' @rdname exported-not-api
#' @export
setGeneric("DimScales",
           function(object, use.names = TRUE)
           standardGeneric("DimScales"))

#' @rdname exported-not-api
#' @export
setGeneric("DimScales<-",
           function(object, value)
           standardGeneric("DimScales<-"))

#' Create object of class "Net" and "Pool".
#'
#' Object of class \code{\linkS4class{Net}} and \code{\linkS4class{Pool}}
#' are used to describe net flows, or 'in' and 'out' flows, typically
#' to be supplied to function \link{Movements}.
#'
#' The 'between' dimensions \code{object} are the dimensions between which
#' flows occur.  For instance, with internal migration, the 'between'
#' dimension will be something like region.  There can be more than one
#' 'between' dimension, e.g. region and employment status.
#'
#' The 'direction' dimension, which is used only with \code{Pool} identifies
#' 'in' flows and 'out' flows.  The dimension must have length 2 and must
#' have categories \code{"in"} and \code{"out"} or \code{"ins"} and \code{"outs"}
#' (case insensitive).  \code{Pool} converts the labels to \code{"In"}
#' and \code{"Out"}.
#'
#' @param object An object of class \linkS4class{Counts}.
#' @param between Names or indices of dimensions of \code{object}
#' @param direction Name or index of dimension of \code{object}
#'
#' @aliases Net Pool
#' 
#' @name net-pool-generators
NULL

#' @rdname net-pool-generators
#' @export
setGeneric("Net",
           function(object, between)
               standardGeneric("Net"))

#' @rdname net-pool-generators
#' @export
setGeneric("Pool",
           function(object, direction, between)
               standardGeneric("Pool"))

#' @rdname counts-values-generator
#' @export
setGeneric("Values",
           function(object, dimtypes = NULL, dimscales = NULL)
           standardGeneric("Values"))

#' Calculate accession from a movements account.
#'
#' Calculations accession from a movements account with an age dimension.
#' Accession is the number of people reaching a given age during a given
#' period.  For instance, accession to age 65 during 2019 is the number of
#' people turning 65 during 2019.
#'
#' Births can be considered as accession to age 0.  By default,
#' the return value to \code{accession} includess age 0.
#' If the account does not in fact include births, then accession
#' to age 0 is set to 0.
#' 
#' @param object A \code{\linkS4class{Movements}} account.
#' @param births If \code{TRUE} (the default), births are included in the
#' answer; if \code{FALSE}, they are not.
#'
#' @return An object of class \code{\linkS4class{Counts}}.
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
#' 
#' accession(account)
#' accession(account, births = FALSE)
#' @export
setGeneric("accession",
           function(object, births = TRUE)
               standardGeneric("accession"))

setGeneric("addBreaks",
           function(object, dimension, breaks, weights, ...)
           standardGeneric("addBreaks"))

#' Add one or more dimensions to a demographic array.
#'
#' Add one or more dimensions to an object of class \code{\linkS4class{DemographicArray}}
#'
#' \code{labels}, \code{scale}, \code{dimtype}, and \code{dimscale} are
#' all recycled where appropriate.  For instance, if \code{scale} is shorter than
#' \code{labels}, then \code{scales} is recycled until it matches the
#' length of \code{labels}.  Similarly, if \code{names} has length 2
#' but \code{labels} is a single vector, then the same \code{labels}
#' vector is used for both dimensions.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}
#' @param name Character vector with name of dimension (or names of dimensions) to be added.
#' @param labels Character vector with labels to be used by new dimension - or,
#' if several dimensions are being added, and each dimension has different labels,
#' a list of character vectors.
#' @param scale Optional numeric vector of scaling factors - or, if several dimensions
#' are being added, and each dimension is scaled differently, a list of numeric vectors.
#' @param after Name or index of a dimension of \code{object}.  The new dimension
#'  will be added after this dimension.
#' @param dimtype Optional character with dimtype of new dimension
#' (or dimtypes of new dimensions.)
#' @param dimscale Optional character with dimscale of new dimension (or
#' dimscales of new dimensions.)
#' @param \dots Not currently used.
#'
#' @return Modified version of \code{object}
#'
#' @seealso \code{\link{collapseDimension}}
#' @examples
#' library(demdata)
#' deaths <- Values(VADeaths2)
#' deaths
#'## assume that Democrats and Republicans have the same death rates
#'addDimension(deaths,
#'             name = "party",
#'             labels = c("Democrat", "Republican"))
#'## assume that Democrats have slightly higher death rates
#'addDimension(deaths,
#'             name = "party",
#'             labels = c("Democrat", "Republican"),
#'             scale = c(1.05, 0.95))
#'## put the new dimension in a different place
#'addDimension(deaths,
#'             name = "party",
#'             labels = c("Democrate", "Republican"),
#'             after = "age")
#'
#'income <- Values(array(c(5000, 10000),
#'                       dim = 2,
#'                       dimnames = list(age = c("20-34", "35-64"))))
#'income
#'## add two dimensions
#'addDimension(income,
#'             name = c("residence_orig", "residence_dest"),
#'             labels = c("Urban", "Rural"))
#'## now assume that being in an urban area is associated with
#'## higher income - and note how the scales multiply
#'addDimension(income,
#'             name = c("residence_orig", "residence_dest"),
#'             labels = c("Urban", "Rural"),
#'             scale = c(1.1, 0.9))
#'## now with income depending only on origin
#'addDimension(income,
#'             name = c("residence_orig", "residence_dest"),
#'             labels = c("Urban", "Rural"),
#'             scale = list(c(1.1, 0.9), 1))
#' @export
setGeneric("addDimension",
           function(object, name, labels, after = length(dim(object)),
                    dimtype = NULL, dimscale = NULL, scale = 1L, ...)
           standardGeneric("addDimension"))

setGeneric("addToPopnEnd",
           function(object, population)
               standardGeneric("addToPopnEnd"))


#' Get or set minimum and maximum ages.
#'
#' Extract minimum or maximum ages from the \code{\link{dimscales}} of an object.
#' If the age dimension uses \code{dimscale} \code{"Intervals"}, then the
#' replacement version of \code{minAge} can be used to change the lower limit
#' of the first age group, and the replacement version of \code{maxAge} can be
#' used to change upper limit of the last age group.
#'
#' Functions \code{setAgeMin} and \code{sexAgeMax} do the same thing as the
#' replacement functions, but work with pipes.
#' 
#' @param object An object of class \code{\linkS4class{DemographicArray}}
#' or \code{\linkS4class{DemographicAccount}}.
#' @param value A number. Can be \code{Inf}.
#'
#' @return The extraction functions return numbers.  The replacement
#' functions, \code{setAgeMin}, and \code{setAgeMax} return a modified
#' version of \code{object}.
#'
#' @seealso \code{\link{ageTimeStep}} and \code{\link{hasRegularAgeTime}} also
#' provide information about age groups. Pipes are described in package
#' \code{magrittr}.
#' 
#' @examples
#' library(demdata)
#' death.rates <- Values(VADeaths2)
#' death.rates
#' ageMin(death.rates)
#' ageMax(death.rates)
#' ageMin(death.rates) <- 45
#' death.rates
#' ageMax(death.rates) <- Inf
#' death.rates
#' setAgeMin(death.rates, value = 50)
#' setAgeMax(death.rates, value = 75)
#' @name ageMinMax
NULL

#' @rdname ageMinMax
#' @export
setGeneric("ageMax",
           function(object)
               standardGeneric("ageMax"))

#' @rdname ageMinMax
#' @export
setGeneric("ageMax<-",
           function(object, value)
               standardGeneric("ageMax<-"))

#' @rdname ageMinMax
#' @export
setGeneric("ageMin",
           function(object)
               standardGeneric("ageMin"))

#' @rdname ageMinMax
#' @export
setGeneric("ageMin<-",
           function(object, value)
               standardGeneric("ageMin<-"))

#' Get the age-time step used by a demographic array.
#'
#' If a \code{object} has a \code{\link[=hasRegularAgeTime]{regular}} age-time plan,
#' then \code{ageTimeStep} returns the length of the age-time steps used;
#' otherwise it raises an error.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#'
#' @return A number (i.e. a numeric vector of length 1).
#'
#' @seealso \code{\link{hasRegularAgeTime}}, \code{\linkS4class{DemographicArray}},
#' \code{\link{dimtypes}}, \code{\link{dimscales}}
#'
#' @examples
#' library(demdata)
#'x <- Counts(VAPopn)
#' x
#' hasRegularAgeTime(x)
#' @export
setGeneric("ageTimeStep",
           function(object)
           standardGeneric("ageTimeStep"))

#' Align categories dimensions that come in pairs.
#'
#' Given an object of class \code{\linkS4class{DemographicArray}}, make dimensions with
#' \code{\link{dimtype}} \code{"origin"} and \code{"destination"} or \code{"parent"}
#' and \code{"child"} consistent.  This is done by permuting and inserting rows.
#'
#' The "base name" of a dimension with \code{\link{dimtype}} \code{"origin"},
#' \code{"destination"}, \code{"parent"}, or \code{"child"} is the name minus
#' the \code{"_orig"}, \code{"_dest"}, \code{"_parent"}, or \code{"_child"}
#' suffix.  For instance, the base name of \code{"region_orig"} and
#' \code{"region_dest"} is \code{"region"}.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param base Character vector giving the "base names" for the dimension pairs.
#' If \code{base} is omitted, the base names from all dimensions with
#' \code{\link{dimtype}} \code{"origin"}, \code{"destination"}, \code{"parent"},
#' or \code{"child"} are used.
#' @param omitted Value to be used for omitted origin-destination or parent-child
#' combinations.  Defaults to \code{0} when \code{object} has class
#' \code{\linkS4class{Counts}} and \code{NA} otherwise.
#'
#' @return An object with the same class as \code{object}.
#'
#' @seealso Function \code{\link{pairAligned}} tests whether a pair of dimensions
#' use the same categories in the same order.
#'
#' @examples
#' x <- Counts(array(1:4,
#'                  dim = c(2, 2),
#'                  dimnames = list(region_orig = c("a", "b"),
#'                  region_dest = c("c", "b"))))
#' x
#' alignPair(x, base = "region")
#' alignPair(x)
#' alignPair(x, omitted = NA)
#'
#' y <- Values(array(1:8,
#'                   dim = c(2, 2, 1, 2),
#'                   dimnames = list(region_orig = c("a", "b"),
#'                                   region_dest = c("b", "a"),
#'                                   ethnicity_parent = "q",
#'                                   ethnicity_child = c("q", "r"))))
#' y
#' alignPair(y, base = "region")
#' alignPair(y, base = "ethnicity")
#' alignPair(y)
#' @export
setGeneric("alignPair",
           function(object, base = NULL, omitted = ifelse(methods::is(object, "Counts"), 0L, NA))
           standardGeneric("alignPair"))


#' Attach a set of known subtotals to a Counts object.
#' 
#' Attach a subtotal or set of subtotals to an object of class
#' \code{\linkS4class{Counts}} with missing values.
#' 
#' All values in \code{subtotals} and all non-missing values in \code{object}
#' must be non-negative integers.
#' 
#' Neither \code{object} nor \code{subtotals} may have \code{\link{dimtype}}
#' \code{"iteration"} or \code{"quantile"}.
#'
#' Concordances supplied through the \code{concordances} argument are used
#' to map values from \code{object} on to \code{subtotals}.  The concordances
#' are supplied as a named list, with the names specifying which dimension
#' of \code{object} each concordances should be used with.  See below
#' for an example.
#' 
#' @param object Object of class \code{\linkS4class{Counts}}, with missing
#' values.  All non-missing values must be non-negative integers.
#' @param subtotals Object of class \code{\linkS4class{Counts}}, or a single
#' number.  Missing values are not permitted.  All values must be non-negative
#' integers.
#' @param concordances A named list of
#' \code{\link[classconc:ManyToOne-class]{ManyToOne}} concordances.
#' 
#' @return Object of class \code{\linkS4class{CountsWithSubtotals}}.
#'
#' @seealso \code{\link{impute}}, \code{\linkS4class{CountsWithSubtotals}}
#'
#' @examples
#' ## 'subtotals' have class "Counts"
#' popn <- demdata::VAPopn
#' popn <- Counts(popn)
#' popn <- extrapolate(popn, along = "age", labels = "45-49", type = "missing")
#' subtotals <- Counts(array(c(65000, 64000),
#'                           dim = 1:2,
#' 			  dimnames = list(age = "45-49", sex = c("Male", "Female"))))
#' attachSubtotals(popn, subtotals = subtotals)
#'
#' ## 'subtotals' single number
#' attachSubtotals(popn, subtotals = 500000)
#'
#' ## 'concordances' argument
#' x <- CountsOne(c(1, 4, NA, NA, 7),
#'                labels = c("a", "b", "c", "d", "e"),
#'                name = "region")
#' subtotals <- CountsOne(c(10, 11),
#'                        labels = c("U", "V"),
#'                        name = "region")
#' conc <- data.frame(from = c("a", "b", "c", "d", "e"),
#'                    to = c("U", "U", "U", "V", "V"))
#' conc <- classconc::Concordance(conc)
#' attachSubtotals(x,
#'                 subtotals = subtotals,
#'                 concordances = list(region = conc))
#' @export
setGeneric("attachSubtotals",
           function(object, subtotals, concordances = list())
               standardGeneric("attachSubtotals"))


#' @rdname exported-not-api
#' @export
setGeneric("canMakeCompatible",
           function(x, y, subset = FALSE, concordances = list(), allowCopyIterDim = TRUE)
           standardGeneric("canMakeCompatible"))

setGeneric("canMakeDimScalePairCompatible",
           function(e1, e2, isCounts1, isCounts2)
           standardGeneric("canMakeDimScalePairCompatible"))

setGeneric("canMakeDimScalesCompatible",
           function(x, y, subset = FALSE, collapse, concordance = NULL)
           standardGeneric("canMakeDimScalesCompatible"))

setGeneric("canMakeOrigDestParentChildCompatible",
           function(x, y, subset = FALSE, allowCopyIterDim = TRUE)
               standardGeneric("canMakeOrigDestParentChildCompatible"))

setGeneric("canMakePairCompatible",
           function(e1, e2, allowCopyIterDim = TRUE)
           standardGeneric("canMakePairCompatible"))

setGeneric("canMakeSharedDimScalesCompatible",
           function(x, y, subset = FALSE, concordances)
           standardGeneric("canMakeSharedDimScalesCompatible"))

setGeneric("checkAndTidyWeights",
           function(weights, target, nameWeights = "weights",
                    nameTarget = "object", allowNA = FALSE)
               stop(gettextf("'%s' has class \"%s\"",
                             nameWeights, class(weights))))

#' @rdname exported-not-api
#' @export
setGeneric("collapse",
           function(object, transform)
           standardGeneric("collapse"))

#' Collapse multiple categories into a single category.
#'
#' Merge multiple categories within a dimension or dimensions of
#' a \code{\linkS4class{DemographicArray}} object.
#'
#' The categories to be collapsed, and the categories that result from collapsing can
#' be described through \code{old} and \code{new}, or through a \code{"\linkS4class{ManyToOne}"}
#' concordance:  see below for examples.
#'
#' Pairs of of dimensions with dimtypes \code{origin} and \code{destination} or \code{parent}
#' and \code{child} can be selected by their base name: see the example below.
#' If no dimension is supplied, \code{collapseCategories} attempts to modify all dimensions.
#'
#' \code{weights} are required if \code{object} has class
#' \code{\linkS4class{Values}} and are prohibited if
#' \code{object} has class \code{\linkS4class{Counts}}.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param dimension Character or numeric vector giving the name or index of the
#' dimension where the categories are found - or names or indices if more than
#' one dimension is being changed.
#' @param old Character vector containing the names of the categories to be
#' collapsed.
#' @param new Character vector of length 1 giving the name of the new category
#' formed from the old categories.
#' @param weights Object of class \code{\linkS4class{Counts}}.
#' @param concordance Object of class \code{"\linkS4class{Concordance}"}.
#' @param \dots Not currently used.
#'
#' @seealso   \code{\link{collapseIntervals}}, \code{\link{collapseIterations}},
#' \code{\link{collapseOrigDest}}, \code{\link{collapseDimension}},
#' \code{\link{dimtypes}}, \code{\link{dimscales}}
#'
#' @examples
#' titanic <- Counts(Titanic, dimtypes = c(Age = "state"))
#' collapseCategories(titanic, dimension = "Class",
#'                    old = c("1st", "2nd", "3rd"),
#'                    new = "Passengers")
#' ## can also use the dimension index, though the name is clearer
#' collapseCategories(titanic, dimension = 1,
#'                    old = c("1st", "2nd", "3rd"),
#'                    new = "Passengers")
#' ## set up and use a concordance
#' library(classconc)
#' conc <- data.frame(status = c("1st", "2nd", "3rd", "Crew"),
#'                    wealth = c("Rich", "Mixed", "Poor", "Mixed"))
#' conc <- Concordance(conc)
#' collapseCategories(titanic, dimension = "Class", concordance = conc)
#'
#' library(demdata)
#' rates <- Values(VADeaths2)
#' popn <- Counts(VAPopn)
#' collapseCategories(rates, dimension = "sex", old = c("Male", "Female"),
#'                    new = "Combined", weights = popn)
#'
#' occ <- occupationalStatus
#' names(dimnames(occ)) <- c("occupation_parent", "occupation_child")
#' occ <- Counts(occ)
#' ## select a pair of dimensions by specifying a base name
#' collapseCategories(occ, dimension = "occupation",
#'                    old = c("2", "5"), new = "9")
#' ## no dimension specified, so changes made to all dimensions
#' collapseCategories(occ, old = c("2", "5"), new = "9")
#' @export
setGeneric("collapseCategories",
           function(object, dimension = NULL, old, new, concordance,
                    weights = NULL, ...)
           standardGeneric("collapseCategories"))


#' Collapse one or more dimensions of a demographic array.
#'
#' Aggregate values along one or more dimensions of an object of class
#' \code{\linkS4class{DemographicArray}}.  The aggregated
#' dimensions are dropped.
#'
#' Either \code{dimension} or \code{margin} must be supplied, but not both.
#'
#' \code{weights} are required if \code{object} has class
#' \code{\linkS4class{Values}}, and prohibited if \code{object} has class
#' \code{\linkS4class{Counts}}.
#'
#' If all dimensions are collapsed, the result is equivalent to
#' \code{sum(object)}.
#'
#' As discussed in \code{\link{dimtypes}}, some dimensions come in pairs.
#' Members of a pair have the same base name but different suffixes, e.g.
#' \code{"region_orig"} and \code{"region_dest"}.  Including the base name,
#' e.g. \code{"region"}, in the \code{dimension} or \code{margin} arguments has
#' the same effect as including both names.  If only one member of a pair of
#' dimensions is collapsed, the suffix is removed and from the name of the
#' remaining dimension, and the dimtype is changed to \code{"state"}.  See
#' below for examples.
#'
#' @param object An object of class' \code{\linkS4class{DemographicArray}}.
#' @param dimension Names or indices of the dimensions to be collapsed.
#' @param margin Names or indices of the dimensions that will remain after the
#' collapsing.
#' @param weights Object of class \code{\linkS4class{Counts}}.
#' \code{weights} can only be used if \code{object} has class
#' \code{\linkS4class{Values}}.  It must have at least as much detail as
#' \code{object}.
#' @param \dots Not currently used.
#' @return An object of the same class as \code{object}, but with fewer
#' dimensions, or a single number.
#' @seealso \code{\link{addDimension}}, \code{\link{collapseCategories}}, %%
#' \code{\link{collapseIntervals}}
#' @examples
#' library(demdata)
#' ## collapse Counts object
#' popn <- Counts(VAPopn)
#' popn
#' collapseDimension(popn, dimension = "age")
#' collapseDimension(popn, dimension = c("sex", "color"))
#' collapseDimension(popn, margin = c("sex", "color"))
#'
#' ## collapse Values object, supplying weights
#' rate <- Values(VADeaths2)
#' collapseDimension(rate, dimension = "age", weights = popn)
#'
#' ## special treatment of origin-destination dimensions
#' mig <- Counts(nz.mig)
#' mig
#' collapseDimension(mig, dimension = "island")
#' collapseDimension(mig, margin = "island")
#' ## note dimension name and dimtype:
#' collapseDimension(mig, margin = "island_orig")
#'
#' ## collapse all dimensions
#' popn <- Counts(VAPopn)
#' collapseDimension(popn, dimension = c("age", "sex", "residence"))
#' collapseDimension(popn, margin = character())
#' @export
setGeneric("collapseDimension",
           function(object, dimension = NULL, margin = NULL, weights, ...)
           standardGeneric("collapseDimension"))

setGeneric("collapseDimScale",
           function(object, index)
           standardGeneric("collapseDimScale"))

#' Collapse intervals.
#'
#' Merge intervals within a dimension of a \code{\linkS4class{DemographicArray}}
#' object.  The dimension must have \code{\link{dimscale}} \code{"Intervals"}.
#'
#' Only one of \code{breaks}, \code{width}, and \code{old} should be supplied.
#'
#' \code{breaks} must be a subset of the current breaks between intervals.  The
#' first and last breaks can be omitted, as these are always retained by
#' \code{collapseIntervals}.
#'
#' \code{width} is a shortcut version of \code{breaks} for the common case
#' where all of the collapsed intervals will have the same width (apart from
#' any open intervals).  It is equivalent to setting breaks equal to
#' \code{seq(from = a, to = b, by = width)} where \code{a} is the lowest
#' (finite) break in \code{dimension}, and \code{b} is the highest.
#'
#' \code{old} allows intervals to be identified by their labels, as with
#' \code{\link{collapseCategories}}.  Unlike with
#' \code{\link{collapseCategories}}, however, no \code{new} argument is
#' required (since it can be deduced from \code{old}, and the elements of
#' \code{old} must be consecutive.
#'
#' If \code{weights} equals \code{1}, all cells are receive equal weights.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param dimension Name or index of the dimension where the intervals are
#' found.
#' @param breaks Numeric vector giving the breaks between intervals after the
#' merging has occurred.
#' @param width The length of the intervals after the merging has occurred.
#' @param old The labels of the intervals to be merged.
#' @param weights Object of class \code{\linkS4class{Counts}} or the number
#' \code{1}.  Required when \code{object} has class
#' \code{\linkS4class{Values}} and prohibited when it has class
#' \code{\linkS4class{Counts}}.
#' @param \dots Not currently used.
#' @seealso \code{\link{collapseCategories}}, \code{\link{collapseIterations}},
#' \code{\link{collapseOrigDest}}, \code{\link{collapseDimension}},
#' \code{\link{dimscales}}
#' @examples
#' library(demdata)
#' popn <- Counts(VAPopn)
#' collapseIntervals(popn, dimension = "age", breaks = c(50, 60, 70, 75))
#'
#' ## same results if first and last breaks omitted
#' collapseIntervals(popn, dimension = "age", breaks = c(60, 70))
#'
#' ## won't work since distance between first and last breaks
#' ## not divisible by 10
#' \dontrun{
#' collapseIntervals(popn, dimension = "age", width = 10)
#' }
#'
#' ## works if last age group omitted
#' popn.trunc <- subarray(popn, age < 70)
#' collapseIntervals(popn.trunc, dimension = "age", width = 10)
#'
#' ## selection via labels
#' collapseIntervals(popn, dimension = "age", old = c("60-64", "65-69", "70-74"))
#'
#' ## weights needed when collapsing objects of class "Values"
#' rates <- Values(VADeaths2)
#' collapseIntervals(rates, dimension = "age", breaks = 65, weights = popn)
#' collapseIntervals(rates, dimension = "age", breaks = 65, weights = 1)
#' @export
setGeneric("collapseIntervals",
           function(object, dimension, breaks = NULL, width = NULL,
                    old = NULL, weights, ...)
           standardGeneric("collapseIntervals"))


#' Summarize iterations.
#'
#' Collapse a dimension with \code{\link{dimtype}} \code{"iteration"},
#' typically replacing it with a much shorter dimension with dimtype
#' \code{"quantile"}.
#'
#' Many estimation and forecasting methods produce sets of simulation runs or
#' iterations.  When graphing or reporting results, it is typically necessary
#' to reduce the full set of iterations to a set of summary values.  Function
#' \code{collapseIterations} carries out this reduction.  By default,
#' \code{collapseIterations} calculates quantiles.  However, users can supply
#' alternatives, including their own customized functions.
#'
#' If \code{FUN} returns a vector, then the dimension with dimtype
#' \code{"iteration"} is replaced by one with dimtype \code{"quantile"} or
#' \code{"state"}.  If \code{FUN} returns a single value, then the
#' \code{"iteration"} dimension is dropped.
#'
#' By default, function \code{\link{quantile}} uses quantiles \code{0, 0.25,
#' 0.5, 0.75, 1}.  Quantiles more useful for summarizing iterations, such as
#' \code{0.025, 0.5, 0.975}, can be obtained by using a \code{prob} argument:
#' see below for an example.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param FUN A function.  Defaults to \code{\link{quantile}}, though others
#' can be used.
#' @param \dots Further arguments, passed to \code{FUN}.
#' @seealso \code{\link{collapseCategories}}, \code{\link{collapseIntervals}},
#' \code{\link{collapseOrigDest}}, \code{\link{collapseDimension}},
#' \code{\link{dimtypes}}, \code{\link{dimscales}}
#' @examples
#' x <- Counts(array(rpois(n = 20, lambda = c(8, 12)),
#'                   dim = c(2, 10),
#'                   dimnames = list(sex = c("Female", "Male"),
#'                       iteration = 1:10)))
#' x
#' collapseIterations(x)
#'
#' ## pass different values for 'prob' to 'quantile'
#' collapseIterations(x, prob = c(0.025, 0.5, 0.975))
#'
#' ## summarize with a single value
#' collapseIterations(x, FUN = mean)
#'
#' ## a user-defined function
#' sumvals <- function(x) c(mean = mean(x), sd = sd(x))
#' collapseIterations(x, FUN = sumvals)
#' @export
setGeneric("collapseIterations",
           function(object, FUN = quantile, ...)
           standardGeneric("collapseIterations"))


#' Convert data on movements into a less detailed format.
#'
#' Convert data on movements classifed by state of origin and state of
#' destination into a less detailed format.  The choices of format are
#' \describe{
#'   \item{\code{"out"}}{Total outward moves from each state}
#'   \item{\code{"in"}}{Total inward moves to each state}
#'   \item{\code{"pool"}}{\code{"out"} and \code{"in"}}
#'   \item{\code{"net"}}{\code{"in"} minus \code{"out"}}
#' }
#'
#' \code{collapseOrigDest} calls function \code{\link{alignPair}} before
#' collapsing cells.  Argument \code{omitted} is passed to \code{alignPair}.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param base Character vector giving the "base names" for the
#' origin-destination pairs.  If \code{base} is omitted, the base names from
#' all dimensions with \code{\link{dimtype}} \code{"origin"} and
#' \code{"destination"} are used.
#' @param to The new format(s): \code{"net"} (the default), \code{"out"},
#' and \code{"in"}.
#' @param weights Object of class \code{\linkS4class{Counts}} providing the
#' weights to be used.  Required if \code{object} has class
#' \code{\linkS4class{Values}}, and prohibited if it has class
#' \code{\linkS4class{Counts}}.
#' @param omitted Value to be used for omitted origin-destination or
#' parent-child combinations.  Defaults to \code{0} when \code{object} has
#' class \code{\linkS4class{Counts}} and \code{NA} otherwise.
#' @param \dots Not currently used.
#' @return If \code{object} has class \code{linkS4class{Counts}} and
#' \code{to} is \code{"net"}, then the return value has class
#' \code{\linkS4class{Net}}.  If \code{object} has class
#' \code{linkS4class{Counts}} and' \code{to} is \code{"pool"},
#' then the return value has class \code{\linkS4class{Pool}}.
#' Otherwise the return value has the same class as \code{object}.
#' @section Warning: If \code{ans} is the result of applying
#' \code{collapseOrigDest} to \code{x}, then \code{sum(ans)} does not
#' necessarily equal \code{sum(x)}.  For instance,
#' \code{sum(collapseOrigDest(x, to = "net"))} is always \code{0}.
#' @seealso \code{\link{alignPair}} \code{\link{collapseCategories}},
#' \code{\link{collapseIntervals}}, \code{\link{collapseIterations}},
#' \code{\link{collapseOrigDest}}, \code{\link{collapseDimension}},
#' \code{\link{dimtypes}}, \code{\link{dimscales}}
#' @references Wilson, T. and Bell, M. (2004) Comparative empirical evaluations
#' of internal migration models in subnational population projections.
#' \emph{Journal of Population Research}. 21(2): 127-160.
#' @examples
#' library(demdata)
#' mig <- Counts(nz.mig)
#' collapseOrigDest(mig, base = "island", to = "net")
#'
#' ## base defaults to all origin-destination base names,
#' ## and 'to' defaults to "net"
#' collapseOrigDest(mig)
#'
#' ## multiple formats, distinguished by dimension "direction"
#' collapseOrigDest(mig, to = "pool")
#' collapseOrigDest(mig, to = "net")
#' collapseOrigDest(mig, to = "out")
#'
#' ## different representations imply different totals
#' sum(mig)
#' sum(collapseOrigDest(mig, to = "net"))
#' sum(collapseOrigDest(mig, to = "pool"))
#'
#' x <- Counts(array(1:4,
#'                   dim = c(2, 2),
#'                   dimnames = list(region_orig = c("a", "b"),
#'                   region_dest = c("c", "b"))))
#' x
#' ## extra categories are added to region_orig and
#' ## region_dest via 'alignPair' before collapsing
#' collapseOrigDest(x, to = "pool")
#' @export
setGeneric("collapseOrigDest",
           function(object, base = NULL, to = c("net", "pool", "in", "out"), weights,
                    omitted = ifelse(methods::is(object, "Counts"), 0L, NA_integer_),
                    ...)
               standardGeneric("collapseOrigDest"))

#' Extract one or more components from a demographic account.
#'
#' A \code{\link[=DemographicAccount-class]{demographic account}} contains
#' counts of population, plus one or more components such as births,
#' deaths, or internal migration.  \code{components} can be used to
#' extract one or more of these components, as a named list.
#'
#' @param object An object of class \code{\linkS4class{DemographicAccount}}.
#' @param names Names of the components.  If omitted all components
#' are returned.
#'
#' @return A named list of \code{\linkS4class{Counts}} objects.
#'
#' @seealso Population counts can be extracted using function
#' \code{\link{population}}.
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
#'
#' components(account)
#' components(account, names = "births")
#' components(account, names = c("deaths", "births"))
#' @export
setGeneric("components",
           function(object, names = NULL)
               standardGeneric("components"))

#' Get or set the names of components of a demographic account.
#'
#' A \code{\link[=DemographicAccount-class]{demographic account}} contains
#' counts of population, plus one or more components such as births,
#' deaths, or internal migration.  \code{componentNames} extracts the
#' names of the components.  \code{setNamesComponents} and
#' the replacement form of \code{namesComponents} can be used
#' to specify the names.
#'
#' @param object An object of class \code{\linkS4class{DemographicAccount}}.
#' @param value Names of the components.
#'
#' @return \code{componentNames} returns a vector of names.
#' \code{setComponentNames} and the replacement form of
#' \code{componentNames} return a modified version of \code{object}.
#'
#' @seealso The names can also be set when creating
#' the account using functions \code{\link{Movements}} and
#' \code{Transitions}.
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
#'
#' componentNames(account)
#' componentNames(account) <- c("Births", "Deaths") # capitalized
#' componentNames(account)
#' account <- setComponentNames(account, value = c("BIRTHS", "DEATHS"))
#' account
#' 
#' @name componentNames
NULL

#' @rdname componentNames
#' @export
setGeneric("componentNames",
           function(object)
               standardGeneric("componentNames"))

#' @rdname componentNames
#' @export
setGeneric("componentNames<-",
           function(object, value)
               standardGeneric("componentNames<-"))

setGeneric("dbind2",
           function(e1, e2, name1, name2, along, dimtypeAlong)
           standardGeneric("dbind2"))

setGeneric("dbindDimScales",
           function(e1, e2, along)
           standardGeneric("dbindDimScales"))

setGeneric("decession",
           function(object)
               standardGeneric("decession"))

## HAS_TESTS
#' Get or set the dimscales of a demographic array
#'
#' Query or (to a limited extent) change the dimscales of a demographic array.
#'
#' Each dimension of a \code{\linkS4class{DemographicArray}} object has a
#' 'dimscale' describing the measurement scale used by that dimension.
#' Examples include \code{"Categories"}, \code{"Intervals"}, and
#' \code{"Points"}. See below for details.
#'
#' Every dimension of a \code{\linkS4class{DemographicArray}} object has a
#' dimscale that identifies the measurement scale used by that dimension.
#'
#' The table below lists valid dimscales.  Dimscales \code{"Categories"},
#' \code{"Intervals"}, and \code{"Points"} are used much more often than the
#' other three, which are associated with particular \code{\link{dimtypes}}.
#'
#' \tabular{lll}{ \emph{dimscale} \tab \emph{Description} \tab \emph{Example}
#' \cr \code{"Categories"} \tab Discrete qualitative categories.  No
#' duplicates. \tab \code{"Female", "Male"} \cr \code{"Intervals"} \tab
#' Segments of real line, ordered, with no gaps or overlap.  First interval can
#' extend to negative infinity, and last interval can extend to positive
#' infinity. \tab \code{"0", "1-4", "5-9", "10-14", "15+"}. \cr \code{"Points"}
#' \tab Real numbers, ordered. \tab \code{2000, 2005, 2010} \cr
#' \code{"Iterations"} \tab Positive integers, ordered.  Used exclusively with
#' dimtype \code{"iteration"}. \tab \code{1, 2, 3} \cr \code{"Quantiles"} \tab
#' Real numbers in interval [0,1], ordered.  Used exclusively with dimtype
#' \code{"quantile"}. \tab \code{"Lower", "Upper"} \cr \code{"Triangles"} \tab
#' Upper or lower Lexis triangle.  Used exclusively with dimtype
#' \code{"triangle"}. \tab \code{"Lower", "Upper"} \cr \code{"Pool"} \tab
#' In-migration or out-migration in a 'migrant pool' model.  Used exclusively
#' with dimtype \code{"pool"}. \tab \code{"Ins", "Outs"} \cr }
#'
#' @aliases dimscales dimscale
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param use.names Logical.  Whether to include the names of the dimensions in
#' the return value.
#' @param value Character vector of valid dimscales.  See table below for a
#' list of valid dimscales.
#' @return \code{dimscales} returns a character vector with the dimscales of
#' each dimension.
#'
#' The replacement method changes the dimscales of \code{object}.
#'
#' @note Changing the dimscales of an existing object can be tricky, since the
#' new dimscales must be consistent with the existing dimtypes.  If major
#' changes to the dimtypes and dimscales of an object are required, it may be
#' easiest to create the object again from scratch.
#'
#' @seealso The type of information encoded by a dimension is represented by
#' \code{\link{dimtypes}}.
#'
#' To change elements within a dimension, use functions such as
#' \code{\link{collapseCategories}}. To add extra elements, use
#' \code{\link{dbind}}.
#'
#' @examples
#' library(demdata)
#' x <- Counts(VADeaths2)
#' dimscales(x)
#'
#' a <- array(rnorm(10),
#'            dim = c(5, 2),
#'            dimnames = list(year = 2000:2004, sex = c("Female", "Male")))
#' x <- Values(a, dimscales = c(year = "Intervals"))
#' dimscales(x)
#' dimscales(x)[1] <- "Points"
#' dimscales(x)
#' @export
setGeneric("dimscales",
           function(object, use.names = TRUE)
           standardGeneric("dimscales"))

#' @rdname dimscales
#' @export
setGeneric("dimscales<-",
           function(object, value)
           standardGeneric("dimscales<-"))


#' Get or set the dimtypes of an object.
#'
#' Query or (to a limited extent) change the dimtypes of an object.
#'
#' Each dimension of a \code{\linkS4class{DemographicArray}} object has a
#' 'dimtype' describing the type of information held by that dimension.
#' Examples include \code{"age"}, \code{"time"}, and \code{"state"}. See below
#' for details.
#'
#' Every dimension of a \code{\linkS4class{DemographicArray}} object has a
#' dimtype.  A dimtype identifies the type of variable encoded by the
#' dimension.
#'
#' The table below lists valid dimtypes.  The dimtypes from \code{"age"} to
#' \code{"destination"} are the most commonly-encountered, with the remaining
#' dimtypes being more specialized.  Different dimtypes require different
#' \code{\link{dimscales}}.
#'
#' \tabular{rll}{ dimtype \tab Description \tab Permitted dimscales \cr
#' \code{"state"} \tab Any qualitative attribute \tab \code{"Categories"} \cr
#' \code{"age"} \tab Age \tab \code{"Intervals"}, \code{"Points"} \cr
#' \code{"time"} \tab "Points" in time or extended intervals \tab
#' \code{"Intervals"}, \code{"Points"} \cr \code{"origin"},
#' \code{"destination"} \tab Starting and finishing states \tab
#' \code{"Categories"} \cr \code{"parent"}, \code{"child"} \tab Parent's state
#' versus child's state \tab \code{"Categories"} \cr \code{"cohort"} \tab
#' Cohort \tab \code{"Intervals"} \cr \code{"iteration"} \tab
#' Simulation number \tab \code{"Iterations"} \cr \code{"quantile"} \tab
#' Quantile (between 0 and 1) \tab \code{"Quantiles"} \cr \code{"triangle"}
#' \tab Lexis triangle \tab \code{"Triangle"} \cr }
#'
#' \itemize{
#'   \item \code{"state"}.  The default dimtype, used for any attribute
#' that is not dealt with by the more specialized dimtypes.
#' \item \code{"age"}.  Age, measured on a numerical scale Can be 'exact' ages, or
#' age-groups. If age is measured qualitatively (eg \code{"children"} and
#' \code{"adults"}), then dimtype \code{"state"} should be used instead. An
#' object can have at most one dimension with dimtype \code{"age"}.
#'   \item \code{"time"}.  Time, measured on a numerical scale.  Can be points in time,
#' or intervals between points in time. If time is measured qualitatively (eg
#' \code{"before"} and \code{"after"}), then dimtype \code{"state"} should be
#' used instead.
#'   \item \code{"origin"}, \code{"destination"}.  Starting and
#' finishing values for an attribute that is subject to change, such as country
#' of residence.  \code{"origin"} and \code{"destination"} dimensions must come
#' in pairs with a common base name and suffixes \code{"_orig"} and
#' \code{"_dest"}, each \code{"country_orig"} and \code{"country_dest"}.  There
#' is no limit to the number of origin-destination pairs that an object may
#' have.
#'   \item \code{"parent"}, \code{"child"}.  Parents' attributes versus
#' those of their children.  Used to model transmission of attributes such as
#' ethnicity from parents to children. \code{"parent"} and \code{"child"}
#' dimensions must come in pairs with a common base name and suffixes
#' \code{"_parent"} and \code{"_child"}, e.g. \code{"ethnicity_parent"} and
#' \code{"ethnicity_child"}.  There is no limit to the number of parent-child
#' pairs that an object may have.
#'   \item \code{"iteration"}.  Iteration number
#' from a simulation.  An object can have at most one dimension with dimtype
#' \code{"iteration"}.  If an object has a dimension with dimtype
#' \code{"iteration"} it may not have a dimension with dimtype
#' \code{"quantile"}.
#'   \item \code{"quantile"}.  Sample quantiles, typically
#' summarizing simulation results.  An object can have at most one dimension
#' with dimtype \code{"quantile"}.  If an object has a dimension with dimtype
#' \code{"quantile"} it may not have a dimension with dimtype
#' \code{"iteration"}.
#'   \item \code{"triangle"}.  Lexis triangle.
#' }
#' @aliases dimtype dimtypes
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param use.names Logical.  Whether to include the names of the dimensions in
#' the return value.
#' @param value Character vector of valid dimtypes.  See table below for a list
#' of valid dimtypes.
#' @return \code{dimtypes} returns a character vector with the dimtypes of each
#' dimension.
#'
#' The replacement method changes the dimtypes of \code{object}.
#'
#' @note Changing the dimtype of a dimension in an existing object can be
#' tricky, since the dimension may have \code{\link{dimscales}} that are not
#' permitted for the new dimtype.  If major changes to the dimtypes and
#' dimscales of an object are required, it may be easiest to create the object
#' again from scratch, perhaps using \code{\link{as.array}} on the object
#' first.
#'
#' @seealso \code{\link{dimscales}}
#' @examples
#' library(demdata)
#' x <- Counts(VADeaths2)
#' dimtypes(x)
#'
#' a <- array(1:8,
#'            dim = c(2, 2, 2),
#'            dimnames = list(age = c("0-39", "40+"),
#'                            tri = c("Upper", "Lower"),
#'                            period = c("1961-2000", "2001-2040")))
#' x <- Counts(a)
#' dimtypes(x)
#' dimtypes(x)[2] <- "triangle"
#' dimtypes(x)
#' @export
setGeneric("dimtypes",
           function(object, use.names = TRUE)
           standardGeneric("dimtypes"))

#' @rdname dimtypes
#' @export
setGeneric("dimtypes<-",
           function(object, value)
           standardGeneric("dimtypes<-"))

setGeneric("dimvalues",
           function(object, use.names = TRUE)
           standardGeneric("dimvalues"))

setGeneric("dimvalues<-",
           function(object, value)
           standardGeneric("dimvalues<-"))

#' Construct a lattice plot from a demographic array.
#'
#' Construct a lattice plot from an object of class
#' \code{\linkS4class{DemographicArray}}.  \code{dplot} is much like
#' \code{\link[lattice]{xyplot}}, but with extra facilities for aggregating and
#' summarizing, and slightly different defaults.
#'
#' If \code{object} has class \code{\linkS4class{Counts}}, then the choices
#' for the response on the left of the \code{~} are as follows:
#' \describe{
#'   \item{\code{count}}{Cell counts are plotted, possibly after aggregation.}
#'   \item{Blank}{Equivalent to \code{count}.}
#'   \item{A function of \code{count}, eg \code{log(count)}}{Cell counts
#'     are aggregated, tranformed, and plotted.}
#'   \item{\code{proportion} or \code{percent}}{A \code{groups} argument must be
#'      supplied.  Cell counts are aggregated, then the distribution across groups
#'      is plotted.}
#' }
#' If \code{object} has class \code{\linkS4class{Values}}, then the choices
#' for the response on the left of the \code{~} are as follows:
#' \describe{
#'   \item{\code{value}}{Cell values are plotted, possibly after aggregation.}
#'   \item{Blank}{Equivalent to \code{values}.}
#'   \item{A function of \code{values}, eg \code{log(values)}}{Cell values
#'     are aggregated, tranformed, and plotted.}
#' }
#'
#' If \code{midpoints} is \code{FALSE}, axes representing dimensions with
#' \code{dimscale} \code{"Intervals"} use a label for each interval.  If
#' \code{midpoints} is \code{TRUE}, intervals are replaced by their midpoints
#' before the plot is constructed, which typically results in less cluttered
#' axes.  If names of individual dimensions are supplied, then only these
#' dimensions have their intervals converted to points.
#'
#' If a \code{subset} argument is supplied, this is applied after \code{data}
#' is converted to a data frame.  Having separate \code{subarray} and
#' \code{subset} arguments can be useful, because they have different
#' strengths.  For instance, \code{subarray} allows expressions like \code{age
#' > 60} on intervals, while \code{subset} allows more complicated expressions.
#'
#' The \code{overlay} provides a convenient way of adding extra values to
#' graphs.  Overlays can include quantiles, even if the main plot does not.
#' Any dimensions of \code{overlay} (if \code{overlay} has the same class as
#' \code{object}) or the \code{values} component of \code{overlay} (if
#' (\code{overlay}) is a list) that are not shared by \code{object} will be
#' collapsed away.  When \code{overlay} or the \code{values} component has
#' class \code{\linkS4class{Values}}, the collapsing uses the \code{weights}
#' argument.  The interface for \code{overlay} is likely to change in future.
#'
#' @param formula \code{formula} object.  The symbols to the right of the
#' \code{~} are interpreted like the right hand side of a standard
#' \code{\link[lattice]{xyplot}} formula, but the symbols to the left are
#' interpreted differently.  See below for details.
#' @param data Object of class \code{\linkS4class{DemographicArray}}.
#' @param type Character vector describing the type or types of plot to be
#' drawn, as described in \code{\link[lattice]{panel.xyplot}}.
#' @param panel Panel function.  See \code{\link{xyplot}}.
#' @param weights Object of class \code{\linkS4class{Counts}}.
#' @param groups A dimension of \code{data}.  Levels for this dimension are
#' overplotted within each panel.
#' @param subarray Expression used to select a subarray from within
#' \code{data}.
#' @param midpoints \code{TRUE}, \code{FALSE} (the default), or a character
#' vector.
#' @param probs Numeric vector used by \code{\link{collapseIterations}} when
#' \code{object} has a dimension with \code{\link{dimtype}}
#' \code{"iteration"}.
#' @param horizontal Logical, defaulting to \code{FALSE}. If \code{TRUE},
#' the roles of the 'x' and 'y' axes are reversed.
#' @param overlay A list describing and overlay.
#' @param \dots Other arguments, which are passed to the underlying plotting
#' function, \code{\link[lattice]{xyplot}}.
#'
#' @return Object of class \code{"trellis"}.
#'
#' @section Warning: As discussed in the documentation for
#' \code{\link{subarray}}, the \code{subarray} function often does not work
#' when called from within another function.  The same is true for the
#' \code{subarray} argument in \code{dplot}.  The solution is typically to use
#' \code{subarray} to construct the desired object, and then pass that object
#' to \code{dplot}.  See below for an example. We are hoping to redesign the
# \code{dplot} to reduce some of these problems in future.
#'
#' @seealso Lattice plots are enormously useful, but customizing them can be
#' tricky.  See \code{\link{xyplot}} for an introduction, and the book in the
#' references section for the details.
#'
#' Internally, \code{dplot} calls \code{\link{subarray}} if a \code{subarray}
#' argument is supplied, then \code{\link{collapseDimension}} to remove any
#' dimensions not included in \code{formula} or \code{groups}, then
#' \code{\link{as.data.frame}} to convert the data to a data frame, at
#' which point \link[lattice]{xyplot} takes over.
#'
#' The \code{\link[=plot-methods]{plot}} method for
#' \code{\linkS4class{DemographicArray}} provides a quick graphical summary of a
#' demographic array.
#'
#' @references Sarkar, Deepayan (2008) \emph{Lattice: Multivariate Data
#' Visualization with R}, Springer
#' @examples
#' library(demdata)
#' popn <- Counts(VAPopn)
#'
#' ## basic plot
#' dplot(~ age | residence * color, data = popn, groups = sex)
#'
#' ## with simple key
#' dplot(~ age | residence * color, data = popn,
#'       groups = sex, auto.key = list(points = FALSE, lines = TRUE))
#'
#' ## horizontal = TRUE
#' dplot(~ residence | age * color, data = popn,
#'       groups = sex, horizontal = TRUE)
#' 
#' ## percent distribution by sex
#' dplot(percent ~ age | residence * color, data = popn,
#'       groups = sex, auto.key = list(points = FALSE, lines = TRUE))
#'
#' ## intervals represented by midpoints
#' dplot(count ~ age | residence, data = popn, midpoints = TRUE)
#'
#' ## use of subarray argument
#' dplot(count ~ age | residence, data = popn, subarray = age > 60)
#'
#' rate <- Values(VADeaths2)
#'
#' ## no aggregation, so no weights needed
#' dplot(~ age | residence, data = rate, groups = sex)
#'
#' ## aggregating over residence, so weights needed
#' dplot(~ age, data = rate, groups = sex, weights = popn)
#'
#' ## pass arguments to xyplot to construct a prettier plot
#' dplot(~ age | residence,
#'       data = popn,
#'       groups = sex,
#'       col = c("dark blue", "salmon"),
#'       xlab = "Age",
#'       ylab = "Population",
#'       prepanel = function(y) list(ylim = c(0, max(y))),
#'       key = list(text = list(dimnames(popn)$sex),
#'                  lines = list(col = c("dark blue", "salmon"), type = "o", pch = 21),
#'                  space = "right"))
#'
#' ## calculate age-specific rate for all groups combined, and overlay on plots
#' rate.comb <- collapseDimension(rate, margin = "age", weights = popn)
#' dplot(~ age | sex * residence,
#'       data = rate,
#'       col = "blue",
#'       overlay = list(values = rate.comb, col = "red"),
#'       midpoints = "age",
#'       key = list(text = list(c("Rate for region and sex", "Rate for whole population")),
#'                  lines = list(col = c("blue", "red"), type = "o", pch = 21)))
#'
#' ## example of 'subarray' argument not working when 'dplot'
#' ## called from within another function
#' \dontrun{f <- function(region) {
#' dplot(count ~ age, data = mig, subarray = island_orig == region)
#' }
#' f("South Island")}
#' @export
setGeneric("dplot",
           function(formula, data, ...)
           standardGeneric("dplot"))

setGeneric("e1IsFirstDimScale",
           function(e1, e2)
               standardGeneric("e1IsFirstDimScale"))

#' Calculate exposure from population counts.
#'
#' \code{object} typically has a dimension with \code{\link{dimtype}}
#' \code{time} and \code{dimscale} \code{"Points"}, and a dimension
#' with dimtype \code{"age"} and dimscale \code{"Intervals"}.
#' The complete set of choices for age-time-cohort dimensions and
#' dimscales is
#' \tabular{ccc}{
#'   \code{time} \tab \code{age} \tab \code{cohort} \cr
#'   \code{Points} \tab \code{Intervals} \tab <none> \cr
#'   \code{Points} \tab <none> \tab <none> \cr
#'   <none> \tab \code{Points} \tab <none> \cr
#'   \code{Points} \tab <none> \tab \code{Intervals} \cr
#'   <none> \tab \code{Points} \tab \code{Intervals}
#' }
#'
#' If \code{triangles} is \code{TRUE}, then exposure is calculated
#' separately for lower and upper Lexis triangles.  \code{triangles}
#' can only by \code{TRUE} if  \code{object} has dimensions with
#' \code{\link{dimtype}} \code{"time"} and \code{"age"}.
#'
#' @param object An object of class \code{\linkS4class{Counts}}.
#' @param triangles  Logical.  If \code{TRUE}, the return value
#' includes a dimension with \code{\link{dimtype}} code{"triangle"}.
#' Defaults to \code{FALSE}.
#'
#' @return An object of class \code{\linkS4class{Counts}}.
#' @export
setGeneric("exposure",
          function(object, triangles = FALSE)
              standardGeneric("exposure"))

#' @rdname exported-not-api
#' @export
setGeneric("extend",
           function(object, transform)
           standardGeneric("extend"))

#' Expand a single category into multiple categories.
#'
#' Using a \code{\linkS4class{ManyToOne}} concordance, expand the categories
#' used by one or more dimensions of a demographic array.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param dimension Character or numeric vector giving the name or index of the
#' dimension where the categories are found - or names or indices if more than
#' one dimension is being changed.
#' @param old Character vector of length 1 containing the name of the
#' category to be expanded.
#' @param new Character vector giving the names of the new categories.
#' @param concordance Object of class \code{"\linkS4class{ManyToOne}"}.
#' @param weights Object of class \code{\linkS4class{Counts}}.
#' @param means If \code{FALSE} (the default) the allocation is done randomly;
#' if \code{TRUE}, values are set equal to their means.
#' @param n Number of iterations to generate.  Ignored if \code{counts} or
#' \code{weights} has a dimension with \code{\link{dimtype}}
#' \code{"iteration"}.
#' @param \dots Not currently used.
#'
#' @seealso   \code{\link{collapseCategories}}
#' @export
setGeneric("expandCategories",
           function(object, dimension = NULL, old, new, concordance,
                    weights = NULL, means = FALSE, n = NULL)
               standardGeneric("expandCategories"))


setGeneric("extendDimScale",
           function(object, index)
           standardGeneric("extendDimScale"))

#' Add extrapolated values to demographic array.
#'
#' Given assumptions about the type and amount of change, extend a demographic
#' array along a specified dimension.
#'
#' The \code{along} dimension must have \code{\link{dimscale}}
#' \code{"Intervals"} or \code{"Points"}, which means that it must have
#' \code{\link{dimtype}} \code{"age"}, \code{"cohort"}, or \code{"time"}.  The
#' intervals or points do not need to be evenly spaced.
#'
#' Values may be extrapolated forward, from the end of the \code{along}
#' dimension, or backward, from the start.  See below for examples.
#'
#' If \code{growth} is a demographic array, it will
#' typically have been created by a call to \code{\link{growth}}.  Use of a
#' demographic array object allows growth rates or increments to differ
#' within classifying variables: see \code{\link{growth}} for details.
#'
#' If \code{type} is \code{"exponential"}, then extrapolated values are
#' generated using the formula \eqn{y_n = y_1 (1 + g)^(t_n - t_1)}{y[n] = y[1]
#' (1 + g)^(t[n] - t[1])}, and if \code{type} is \code{"linear"}, values are
#' generated using \eqn{y_n = y_1 + (t_n - t_1) g}{y[n] = y[1] + (t[n] - t[1])
#' g}, where \eqn{g} is \code{growth}.  If \code{type} is \code{"missing"} the
#' extra cells are filled with \code{NA}s.
#'
#' @param object An object of class \code{\linkS4class{DemographicArray}}.
#' @param along Name or index of dimension along which values are extrapolated.
#' If \code{along} is omitted, \code{extrapolate} looks for a dimension with
#' \code{\link{dimtype}} \code{"time"}, \code{"age"}, or \code{"cohort"} (in
#' that order).
#' @param labels Labels for the new elements to be added to dimension
#' \code{along}.
#' @param growth A single number, or a \code{\linkS4class{DemographicArray}}
#' object specifying growth rates or increments.
#' @param type The type of growth assumed: \code{"exponential"},
#' \code{"linear"}, or \code{"missing"}.  Can be abbreviated.  Defaults to
#' \code{"exponential"}.  See below for details.
#' @return An object of the same class as \code{object}.
#' @seealso \code{\link{growth}}
#' @examples
#' ## prepare data
#' library(datasets)
#' spend <- USPersonalExpenditure
#' names(dimnames(spend)) <- c("category", "year")
#' spend <- Values(spend)
#'
#' ## extrapolate assuming expenditure in all
#' ## categories grows at 1% per annum
#' ## (note that 'type' defaults to "exponential")
#' extrapolate(spend, labels = c(1965, 1970), growth = 0.01)
#'
#' ## linear increase of $1 per annum
#' extrapolate(spend, labels = c(1965, 1970), growth = 1, type = "linear")
#' extrapolate(spend, labels = c(1965, 1970), growth = 1, type = "l")
#'
#' ## grows at historial growth rate - same rate for all categories
#' gr <- growth(spend)
#' extrapolate(spend, labels = c(1965, 1970), growth = gr)
#'
#' ## grows at historical growth rates - rate specific to each category
#' gr <- growth(spend, within = "category")
#' extrapolate(spend, labels = c(1965, 1970), growth = gr)
#'
#' ## irregularly-spaced times
#' extrapolate(spend, labels = c(1972.25, 2001), growth = 0.01)
#'
#' ## backward extrapolation
#' extrapolate(spend, labels = c(1930, 1935), growth = 0.01)
#'
#' ## fill extra years with NAs
#' extrapolate(spend, labels = c(1965, 1970), type = "missing")
#'
#'
#' ## example of extrapolation over age intervals
#' library(demdata)
#' deaths <- Values(VADeaths2)
#' gr <- growth(deaths, along = "age", within = c("sex", "residence"))
#' extrapolate(deaths, along = "age", labels = c("75-79", "80+"), growth = gr)
#' @export
setGeneric("extrapolate",
           function(object, along = NULL, labels, growth = 0,
                    type = c("exponential", "linear", "missing"))
           standardGeneric("extrapolate"))


#' Calculate average growth rates or increments.
#'
#' Calculate average growth rates or increments for a
#' \code{\linkS4class{DemographicArray}} object.
#'
#' The \code{along} dimension must have \code{\link{dimscale}}
#' \code{"Intervals"} or \code{"Points"}, which means that it must have
#' \code{\link{dimtype}} \code{"age"}, \code{"cohort"}, or \code{"time"}.  The
#' intervals or points do not need to be evenly spaced.
#'
#' If there is a dimension with \code{\link{dimtype}} \code{"iteration"} and
#' that dimension is not explicitly included in \code{within}, then it is added
#' to \code{within} automatically.
#'
#' All dimensions of \code{object} not included in \code{along} or
#' \code{within} are aggregated before growth rates or increments are
#' calculated.
#'
#' If a \code{weights} argument is supplied when \code{object} has class
#' \code{\linkS4class{Counts}}, the argument is ignored.  If a \code{weights}
#' argument is not supplied when \code{object} has class
#' \code{\linkS4class{Values}}, all elements are given equal weight.
#'
#' If \code{method} is \code{"endpoints"}, then growth rates or increments are
#' calculated from the first and last points only.  If \code{method} is
#' \code{"lm"}, then growth rates are calculated by fitting a linear model to
#' the logged values, and increments are calculated by fitting a linear model
#' to the values.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param along Name or index of dimension along which growth is calculated.
#' If \code{along} is omitted, \code{growth} looks for a dimension with
#' \code{\link{dimtype}} \code{"time"}, \code{"age"}, or \code{"cohort"} (in
#' that order).
#' @param within Name or index of dimensions within which growth is calculated.
#' Value \code{"."} can be used as shorthand for "all dimensions other than
#' \code{along}".  Origin-destination or parent-child pairs can be referred to
#' using their base name.  See below for examples.
#' @param weights Object of class \code{\linkS4class{Counts}}, supplying
#' weights to be used when aggregating dimensions not included in \code{along}
#' or \code{within}.
#' @param type One of \code{"exponential"} or \code{"linear"}.  If
#' \code{"exponential"}, growth rates are calculated; if \code{"linear"},
#' increments are calculated.  Can be abbreviated. Defaults to
#' \code{"exponential"}.
#' @param method One of \code{"endpoints"} or \code{"lm"}.  See below for
#' details.
#' @return If \code{within} is \code{NULL}, a numeric value; otherwise an
#' object with the same class as \code{object}.
#' @seealso \code{\link{extrapolate}}
#' @examples
#'
#' ## construct data
#' spend <- USPersonalExpenditure
#' names(dimnames(spend)) <- c("category", "year")
#' spend <- Counts(spend)
#'
#' ## calculate average growth rate across all categories
#' growth(spend, along = "year")
#' growth(spend) ## defaults to using dimension with dimtype "time"
#'
#' ## growth within categories
#' growth(spend, within = "category")
#' growth(spend, within = ".")
#'
#' ## increments rather than rates
#' growth(spend, within = "category", type = "linear")
#'
#' ## two different methods for calculating
#' growth(spend, within = "category")
#' growth(spend, within = "category", method = "lm")
#'
#' ## use base name to specific origin and destination dimensions
#' a <- array(rpois(n = 45, lambda = 10),
#'            dim = c(3, 3, 5),
#'            dimnames = list(reg_orig = 1:3,
#'                reg_dest = 1:3, time = 2000:2004))
#' x <- Counts(a, dimscales = c(time = "Points"))
#' growth(x, within = c("reg_orig", "reg_dest"))
#' growth(x, within = "reg")
#' @export
setGeneric("growth",
           function(object, along = NULL, within = NULL, weights,
                    type = c("linear", "exponential"),
                    method = c("endpoints", "lm"))
           standardGeneric("growth"))


#' Age steps and time steps of demographic array.
#'
#' \code{hasRegularAgeTime} tests whether an object has a "regular" age-time
#' plan, where regular means that all age steps and/or time steps have the same
#' length.  If an object does have a regular age-time plan, then
#' \code{ageTimeStep} returns then length of the age and/or time steps.
#'
#' Functions such as GIVE EXAMPLES can only be applied to objects that are
#' "regular".
#'
#' Step lengths equals the widths of the intervals if a dimension has
#' \code{\link{dimscale}} \code{"Intervals"}, and the distance between points
#' if the dimension has \code{\link{dimscale}} \code{"Points"}.  Any 'open'
#' intervals (ie intervals beginning with \code{-Inf} or ending with
#' \code{Inf}) are ignored.
#'
#' An 'age step' is a step along a dimension with \code{\link{dimtype}}
#' \code{"age"}, and a 'time step' is a step along a dimension with dimtype
#' \code{"time"}.
#'
#' If \code{object} has both an age dimension and a time dimension, then the
#' age steps and time steps must all be equal for the age-time plan to be
#' regular.
#'
#' If \code{object} does not have either an age dimension or a time dimension,
#' then it is regular.  However, \code{ageTimeStep} throws an error.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @return \code{TRUE} or an error.
#' @seealso \code{\linkS4class{DemographicArray}}, \code{\link{dimtypes}},
#' \code{\link{dimscales}}, FUNCTIONS REQUIRE REGULAR.
#' @examples
#' library(demdata)
#' x <- Counts(VAPopn)
#' x
#' hasRegularAgeTime(x)
#' ageTimeStep(x)
#'
#' x <- Counts(occupationalStatus)
#' x
#' hasRegularAgeTime(x)
#' \dontrun{ageTimeStep(x)} ## no age or time dimensions
#'
#' a <- array(rnorm(3),
#'            dim = 3,
#'            dimnames = list(year = c(2000, 2005, 2020)))
#' x <- Values(a)
#' \dontrun{hasRegularAgeTime}
#' @export
setGeneric("hasRegularAgeTime",
           function(object)
           standardGeneric("hasRegularAgeTime"))

#' Quick and dirty imputation of missing values.
#'
#' Imputes missing values by fitting an additive or multiplicative main effects
#' model to the non-missing data, and then randomly drawing values for the
#' missing values, based on predictions from the model.
#'
#' \code{impute} performs a single imputation based on a simple, and not
#' necessarily appropriate, model.  \code{impute} is useful for generating
#' starting values for iterative calculations, for tidying data with a small
#' proportion of missing values, or for rough approximations.  For more
#' sophisticated approaches to the imputation of demographic data see package
#' \pkg{demest}.
#'
#' If \code{mult} is \code{TRUE}, meaning that the multiplicative model is
#' used, all non-missing values must be non-negative (0s are omitted from the
#' model.)  If the \code{mult} is \code{TRUE} all imputed values are guaranteed
#' to be non-negative.
#'
#' If all non-missing values are non-negative integers, imputed values are
#' drawn from a Poisson distribution.  Other they are drawn from a normal
#' distribution.
#'
#' The \code{max} argument is useful when \code{object} represents draws from a
#' binomial distribution.  Setting \code{max} to the number of trials for each
#' cell ensures that the imputed number of successes does not exceed the number
#' of trials.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param mult If \code{TRUE}, a multiplicative model is used to impute values;
#' if \code{FALSE}, an additive model is used.  Defaults to \code{TRUE} if all
#' non-missing values in \code{object} are non-negative.
#' @param max Numeric vector giving maximum values for \code{object}.  Imputed
#' values respect this maximum; existing values are unaffected.  \code{max} is
#' recycled so that it has the same length as \code{object}.
#' @return An object with the same class as \code{object}, with the missing
#' values filled in.
#' @seealso \code{\linkS4class{DemographicArray}}, \code{\link{perturb}},
#' \code{\link{extrapolate}}
#' @references Little, RJA and Rubin, DB. 2002. \emph{Statistical Analysis with
#' Missing Data}, Wiley.
#' @examples
#' library(demdata)
#' popn <- Counts(VAPopn)
#' popn[sample(length(popn), size = 5)] <- NA
#' popn
#' impute(popn)
#' impute(popn, mult = TRUE)
#' impute(popn, max = 500)
#' @export
setGeneric("impute",
           function(object, mult = NULL, max = NULL)
               standardGeneric("impute"))

#' @rdname exported-not-api
#' @export
setGeneric("incrementDimScale",
           function(object, n)
               standardGeneric("incrementDimScale"))

setGeneric("incrementLowerTri",
           function(component, population)
               standardGeneric("incrementLowerTri"))

setGeneric("incrementInteger",
           function(object)
               standardGeneric("incrementInteger"))

setGeneric("incrementOpen",
           function(component, population)
               standardGeneric("incrementOpen"))

setGeneric("incrementSquare",
           function(component, population)
               standardGeneric("incrementSquare"))

setGeneric("incrementUpperTri",
           function(component, population)
               standardGeneric("incrementUpperTri"))

setGeneric("inferDimvalues",
           function(DimScale, labels, ...)
           standardGeneric("inferDimvalues"))

setGeneric("InternalMovements",
           function(internal, template)
               standardGeneric("InternalMovements"))

setGeneric("isCompatibleWithPopn",
           function(component, metadata, name)
               standardGeneric("isCompatibleWithPopn"))


#' Test whether a demographic account is internally consistent.
#'
#' Test whether the components and population counts counts of a 
#' \code{\linkS4class{DemographicAccount}} conform to the accounting identity
#' that population at the end of a period equals population at the beginning
#' of a period plus entries minus exits.  Entries include events such as
#' births and in-migration, and exits include events such as deaths
#' and out-migration. The accounting identities are applied cell by cell.
#'
#' The return value is an array of logical values. To test whether
#' every cell in an account is consistent, without identifying the
#' inconsistent cells, use function \code{all}, as in
#'
#' \code{all(isConsistent(myaccount))}
#'
#' @param object A \code{\linkS4class{DemographicAccount}}.
#'
#' @return An array of logical values.
#'
#' @examples
#' ## A consistent account
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
#' isConsistent(account)
#' all(isConsistent(account))
#'
#' ## An inconsistent account
#' population <- Counts(array(c(10, 15, 13, 16),
#'                            dim = c(2, 2),
#'                            dimnames = list(age = c("0-29", "30+"),
#'                                            time = c(1970, 2000))))
#' births <- Counts(array(14, # changed from 13
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
#' isConsistent(account)
#' all(isConsistent(account))
#' @export
setGeneric("isConsistent",
           function(object)
               standardGeneric("isConsistent"))

setGeneric("isPositiveIncrement",
           function(object)
               standardGeneric("isPositiveIncrement"))

setGeneric("length")


#' First and last categories used by each dimension.
#'
#' Gives the first and last categories used by each dimension
#' of a \code{\linkS4class{DemographicArray}} object or a
#' \code{\linkS4class{DemographicAccount}}.  It provides a
#' quick way of understanding the structure of an object.
#'
#' @param object An object of class \code{\linkS4class{DemographicArray}}
#' or \code{\linkS4class{DemographicAccount}}.
#' @param components  Logical. Only used if \code{object} is a
#' \code{\linkS4class{DemographicAccount}}.  If \code{TRUE} then
#' limits are shown for all demographic series; if \code{FALSE}
#' (the default) limits are shown only for population.
#'
#' @return If \code{object} is a \code{\linkS4class{DemographicArray}},
#' or if \code{object} is a \code{\linkS4class{DemographicAccount}} and
#' \code{components} is \code{FALSE}, then the return value is a data.frame;
#' otherwise it is a list of data.frames.
#'
#' @examples
#' library(demdata)
#' popn <- Counts(VAPopn)
#' limits(popn)
#' @export
setGeneric("limits",
           function(object, components = FALSE)
               standardGeneric("limits"))

#' @rdname exported-not-api
#' @export
setGeneric("makeCompatible",
           function(x, y, subset = FALSE, check = TRUE)
           standardGeneric("makeCompatible"))

#' Make a demographic account internally consistent.
#'
#' Adjust the population counts and components of a demographic
#' account so that all cells in the account conform to the
#' basic demographic accounting identity:
#' \code{poulation at end of period}
#' \code{= population at beginning of period}
#' \code{+ entries}
#' \code{- exits.}
#' Births and in-migrations are examples of entries, and
#' deaths and out-migrations are examples of exists.
#'
#' \code{makeConsistent} obtains consistency by starting
#' with the population at the start of the period, and
#' working forward, adding entries and subtracting
#' exits.
#'
#' Sometimes the original entries and exits
#' imply negative population counts.  If \code{adjust}
#' is \code{FALSE}, an error is raised.  If \code{adjust}
#' is \code{TRUE}, entries are adjusted upwards, and
#' exits are adjusted downwards, until positive population
#' counts are obtained.  The size of the steps in this
#' adjustment process is governed by \code{scale}.
#'
#' @param object An object of class \code{\linkS4class{DemographicAccount}}.
#' @param adjust If \code{TRUE}, components such as births
#' and deaths are adjusted to avoid negative populations.
#' @param scale A non-negative number governing the size of
#' the steps if components are adjusted.
#'
#' @return A consistent \code{\linkS4class{DemographicAccount}}.
#'
#' @seealso To test whether an account is consistent, use
#' \code{\link{isConsistent}}.
#'
#' population <- Counts(array(c(10, 15, 13, 16),
#'                            dim = c(2, 2),
#'                            dimnames = list(age = c("0-29", "30+"),
#'                                            time = c(1970, 2000))))
#' births <- Counts(array(14, # changed from 13
#'                        dim = c(1, 1),
#'                        dimnames = list(age = "30+",
#'                                        time = "1971-2000")))
#' deaths <- Counts(array(c(0, 9),
#'                        dim = c(2, 1),
#'                        dimnames = list(age = c("0-29", "30+"),
#'                                        time = c("1971-2000"))))
#' inconsistent.account <- Movements(population = population,
#'                                   births = births,
#'                                   exits = list(deaths = deaths))
#' inconsistent.account
#' isConsistent(inconsistent.account)
#' consistent.account <- makeConsistent(inconsistent.account)
#' consistent.account
#' isConsistent(consistent.account)
#' @export
setGeneric("makeConsistent",
           function(object, adjust = TRUE, scale = 0.1)
               standardGeneric("makeConsistent"))

setGeneric("makeIndices",
           function(x, y, collapse, concordance = NULL)
           standardGeneric("makeIndices"))

setGeneric("makeOrigDestParentChildCompatible",
           function(x, y, subset = FALSE, check = TRUE)
               standardGeneric("makeOrigDestParentChildCompatible"))

setGeneric("makeOrigDestParentChildTransform",
           function(x, y, subset = FALSE, check = TRUE)
               standardGeneric("makeOrigDestParentChildTransform"))

setGeneric("makePairCompatible",
           function(e1, e2, check = TRUE)
           standardGeneric("makePairCompatible"))

setGeneric("makePairIndices",
           function(e1, e2, isCounts1, isCounts2)
           standardGeneric("makePairIndices"))

setGeneric("makePairTransforms",
           function(e1, e2, check = TRUE)
               standardGeneric("makePairTransforms"))

setGeneric("makePairTransformsDbind",
           function(e1, e2, along)
               standardGeneric("makePairTransformsDbind"))

#' @rdname exported-not-api
#' @export
setGeneric("makeTransform",
           function(x, y, subset = FALSE, concordances = list(), check = TRUE)
           standardGeneric("makeTransform"))

setGeneric("mergeDimScales",
           function(e1, e2)
           standardGeneric("mergeDimScales"))

setGeneric("metadata",
           function(object)
           standardGeneric("metadata"))

#' Replace intervals by their midpoints.
#'
#' Change \code{"Intervals"} \code{\link{dimscales}} to \code{"Points"},
#' replacing each interval with its midpoint.  This can be useful for plotting
#' or calculating mean values, among other things. \code{midpoints} is
#' typically called by other functions, but it may
#' sometimes be useful to call it directly.
#'
#' An object can only have a dimension with \code{\link{dimtype}}
#' \code{"triangle"} if the object also has age and time dimensions with
#' dimscale \code{"Intervals"}.  When \code{midpoints} is called
#' on an object with a triangles dimension, the dimtype for that
#' dimension is coerced to \code{"state"} and the dimscale is
#' coerced to \code{"Categories"}.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param dimension Names or indices of dimensions to be converted.  If
#' omitted, all dimensions with dimtype \code{"Intervals"} are converted.
#' @return Object of class \code{\linkS4class{DemographicArray}}.
#' @seealso \code{\link{dimscales}}, \code{\link{dplot}}
#' @examples
#' library(demdata)
#' rates <- Values(VADeaths2)
#' midpoints(rates)
#'
#' a <- array(1:4,
#'            dim = c(2, 2),
#'            dimnames = list(age = c("0-39", "40+"),
#'                            period = c("2001-2010", "2011-2020")))
#' x <- Counts(a)
#' midpoints(x)
#' midpoints(x, dimension = c("age", "period"))
#' midpoints(x, dimension = "period")
#' midpoints(x, dimension = 2)
#' @export
setGeneric("midpoints",
           function(object, dimension)
           standardGeneric("midpoints"))

#' Get the number of iterations  in a demographic array.
#'
#' If an object of class \code{\linkS4class{DemographicArray}} has a dimension
#' with \code{\link{dimtype}} \code{"iteration"}, return the lenth of that
#' dimension; if not, raise an error.
#'
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @return An integer.
#' @author John Bryant \email{demographic.packages@@gmail.com}
#' @seealso \code{\link{dimtypes}}, \code{\link{collapseIterations}}
#' @keywords manip
#' @examples
#' library(demdata)
#' x <- Counts(array(replicate(n = 5, rnorm(2)),
#'                   dim = c(2, 5),
#'                   dimnames = list(sex = c("Female", "Male"),
#'                       iteration = 1:5)))
#' nIteration(x)
#'
#' x <- Counts(VAPopn)
#' ## no dimension with dimtype "iteration"
#' \dontrun{nIteration(x)}
#' @export
setGeneric("nIteration",
           function(object)
               standardGeneric("nIteration"))

#' Test whether pairs of dimensions aligned.
#'
#' Dimensions with \code{\link{dimtypes}} \code{"origin"} and
#' \code{"destination"} or \code{"parent"} and \code{"child"} come in pairs.
#' \code{pairAligned} tests whether the members of each pair have
#' the same categories, in the same order.  If they do, \code{pairAligned}
#' returns \code{TRUE}.  If not, it raises an error with a message that
#' describes the differences.
#'
#' By default \code{pairAligned} tests all origin-destination and
#' parent-child pairs.  Argument \code{base} can be used to restrict
#' the test to specific pairs.  See below for an example.
#'
#' @inheritParams alignPair
#' @param object An object of class \code{\linkS4class{DemographicArray}}.
#'
#' @return \code{TRUE} or an error describing any differences.
#'
#' @seealso Pairs of dimensions can be aligned using function
#' \code{\link{alignPair}}. In programming, functions \code{\link{try}} and
#' \code{\link{tryCatch}} may be useful for intercepting and dealing with
#' errors.
#'
#' @examples
#' x <- Counts(array(1:4,
#'                   dim = c(2, 2),
#'                   dimnames = list(reg_orig = c("A", "B"),
#'                                   reg_dest = c("A", "B"))))
#' pairAligned(x)
#' x <- Counts(array(1:6,
#'                   dim = 3:2,
#'                   dimnames = list(reg_orig = c("A", "B", "C"),
#'                                   reg_dest = c("A", "B"))))
#' \dontrun{pairAligned(x)}
#'
#' ## first pair not aligned; second pair OK
#' x <- Counts(array(1:16,
#'                   dim = c(2, 2, 2, 2),
#'                   dimnames = list(reg_orig = c("B", "A"),
#'                                   reg_dest = c("A", "B"),
#'                                   income_parent = c("Low", "High"),
#'                                   income_child = c("Low", "High"))))
#' \dontrun{pairAligned(x)}
#' pairAligned(x, base = "income")
#' \dontrun{pairAligned(x, base = "reg")}
#' @export
setGeneric("pairAligned",
           function(object, base = NULL)
               standardGeneric("pairAligned"))

#' Generate demographic array consisting of random values.
#'
#' If \code{object} has a dimension with \code{\link{dimtype}}
#' \code{"Iterations"}, then \code{perturb} returns \code{n} randomly-selected
#' iterations.  Otherwise, it fits a log-linear model to the data and uses this
#' to randomly generate \code{n} iterations.  The log-linear model by default
#' contains 2-way interactions; higher or lower values can be specified via
#' \code{order}.
#'
#' Arguments \code{order} and \code{phi} only have an effect when a log-linear
#' model is fitted - in other words, when \code{object} does not have dimtype
#' \code{"Iterations"}.
#'
#' \code{object} can contain missing values, though if a log-linear model is
#' fitted, there must be sufficient observations for a model with order-way
#' interations.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param n Number of iterations to generate.
#' @param order Order of interations used in log-linear models.
#' @param phi A positive number governing the amount of random noise: higher
#' values imply more variation.
#' @param subtotals Not implemented yet
#' @return An object of the same class as \code{object}.  If \code{n > 1}, the
#' return value has a dimension with dimtype \code{"Iterations"}; otherwise it
#' does not.
#' @seealso The log-linear model is fitted using \code{\link{loglm}}.
#' @examples
#' library(demdata)
#' x <- Values(VADeaths2)
#' perturb(x)
#' perturb(x, phi = 2) ## greater variance
#' perturb(x, order = 1) ## log-linear model only includes main effects
#' perturb(x, n = 2)
#' x10 <- perturb(x, n = 10)
#' perturb(x10, n = 2)  ## iterations selected, rather than generated
#' @export
setGeneric("perturb",
           function(object, n = 1L, order = 2L, phi = 1, subtotals = NULL)
           standardGeneric("perturb"))

setGeneric("plotSingleDimension",
           function(object, margin, ...)
           standardGeneric("plotSingleDimension"))


#' Extract population counts from a demographic account.
#'
#' A \code{\link[=DemographicAccount-class]{demographic account}} contains
#' counts of population, plus one or more components such as births,
#' deaths, or internal migration.  \code{population} can be used to
#' extract the population counts.
#'
#' @param object An object of class \code{\linkS4class{DemographicAccount}}.
#' 
#' @return A \code{\linkS4class{Counts}} object.
#'
#' @seealso Counts for components can be extracted using function
#' \code{\link{components}}.
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
#'
#' population(account)
#' @export
setGeneric("population",
           function(object)
               standardGeneric("population"))





#' Reallocate counts or values to youngest and oldest age groups.
#'
#' With fertility data, it is common to confine age-specific fertility rates
#' to a limited range of age-gropus, eg ages 12 to 49, or ages 15-19 to 45-49.
#' Births outside these age groups are reallocated to the bottom' and top
#' age groups.  The procedure preserves the total number of birth and total
#' fertility, and has only a minor effect
#' on the age profile for fertility, provided that the number of births that are
#' reallocated is small.
#'
#' \code{reallocateToEndAges} reallocates counts or rates.  There is no
#' requirement' that the counts or rates relate to fertility,
#' though this is by far the
#' most common scenario.
#'
#' If \code{min} is set to a value that is less than the lower limit
#' of the youngest age group in \code{object}, then the return value
#' from \code{reallocateToEndAges} has the same youngest age group as
#' \code{code}.  Similarly, if \code{max} is set to a value higher
#' than the upper limit of the oldest age group,
#' then \code{reallocateToEndAges} leaves the oldest age group untouched.
#' 
#' A \code{weights} argument is prohibited if \code{object} has class
#' \code{\linkS4class{Counts}}, and required if it has class
#' \code{\linkS4class{Values}}.
#' 
#' @param object An object of class \code{\linkS4class{Counts}} or (less commonly)
#' \code{\linkS4class{Values}}.
#' @param min The lower limit of the youngest age group, after restriction of
#' the age range.
#' @param max The upper limit of the oldest age group, after restriction
#' of the age range.
#' @param weights An object of class \code{\linkS4class{Counts}}.
#' @param \dots Not currently used.
#'
#' @return A \code{\linkS4class{Counts}} object in which all
#' age groups lie within the range (\code{min}, \code{max}).
#'
#' @seealso \code{\link{collapseIntervals}} collapses age intervals
#' without reallocating values upwards or downwards.
#' 
#' @examples
#' ## make some fake data on births
#' births <- Counts(array(10,
#'                        dim = c(45, 2),
#'                        dimnames = list(age = 10:54, region = c("A", "B"))),
#'                  dimscales = c(age = "Intervals"))
#'
#' ## default ages
#' reallocateToEndAges(births)
#' ## non-default ages
#' reallocateToEndAges(births, min = 12, max = 45)
#'
#' ## make some fake data on birth rates
#' birth.rates <- Values(array(0.1,
#'                       dim = c(45, 2),
#'                       dimnames = list(age = 10:54, region = c("A", "B"))),
#'                       dimscales = c(age = "Intervals"))
#' women <- Counts(array(100,
#'                       dim = c(45, 2),
#'                       dimnames = list(age = 10:54, region = c("A", "B"))),
#'                 dimscales = c(age = "Intervals"))
#' ## with rates, need to supply weights
#' reallocateToEndAges(birth.rates, weights = women)                       
#' @export
setGeneric("reallocateToEndAges",
           function(object, min = 15, max = 50, weights, ...)
               standardGeneric("reallocateToEndAges"))

setGeneric("reorderCategories",
           function(object, dimension, subset, weights, ...)
           standardGeneric("reorderCategories"))

setGeneric("removeBreaks",
           function(object, dimension, breaks, weights, ...)
               standardGeneric("removeBreaks"))



#' Randomly allocate counts in proportion to weights.
#'
#' \code{weights} should have one or more dimensions that \code{counts} does
#' not.  The contents of each cell in \code{counts} are randomly distributed
#' across these extra dimensions.
#'
#' Each cell in \code{counts} is redistributed by drawing from a multinomial
#' distribution with size equal to the value of the cell and probabiltiies
#' proportional to the values of the corresponding cells in \code{weights}.
#'
#' \code{weights} is typically a set of observed counts, such a
#' cross-tabulation of records with no missing data.  However, \code{weights}
#' could also be a sample from a posterior distribution.
#'
#' @inheritParams expandCategories
#' @param counts Object of class \code{\linkS4class{Counts}} consisting of
#' non-negative integers, or a single non-negative integer.
#' @param weights Object of class \code{\linkS4class{DemographicArray}} with no
#' negative values.
#'
#' @return An \code{\linkS4class{Counts}} object with the same dimensions and
#' metadata as \code{weights}.
#' @seealso \code{\link{rmultinom}}
#' @examples
#' x <- Counts(array(c(10, 12),
#'                   dim = 2,
#'                   dimnames = list(sex = c("Female", "Male"))))
#' y <- Counts(array(0:5,
#'                   dim = c(2, 3),
#'                   dimnames = list(sex = c("Female", "Male"),
#'                       region = c("A", "B", "C"))))
#'
#' redistribute(x, weights = y)
#'
#' ## specify the number of iterations
#' redistribute(x, weights = y, n = 3)
#'
#' ans <- redistribute(x, weights = y)
#' ans
#' ## reverse the effects
#' collapseDimension(ans, dimension = "region")
#'
#' ## give all cells a chance of being non-zero
#' redistribute(x, weights = y + 0.1, n = 10)
#'
#' ## 'counts' is a single value
#' redistribute(10, weights = y)
#' @export
setGeneric("redistribute",
           function(counts, weights, means = FALSE, n = NULL)
               standardGeneric("redistribute"))

#' Redistribute values from one or more categories among the remaining categories.
#'
#' Take counts from one or more categories (eg an "Other" category") and allocate
#' them the remaining categories, in proportion to the current size of those categories.
#'
#' When \code{means} is \code{FALSE} (the default) the allocation is done
#' randomly, via multinomial draws.  When \code{means} is \code{FALSE} the
#' allocation is done deterministically, and exactly matches the existing
#' distribution.
#'
#' Argument \code{epsilon} provides a quick and dirty way of using the function
#' with sparse data, when all of the remaining categories may be zero (so that
#' the weights are undefined.)  The quantity \code{epsilon} is added to all
#' counts before calculating.  With sparse data a better approach would be to
#' fit a model and then redistribute using the fitted values, using function
#' \code{\link{redistribute}}.
#'
#' @inheritParams redistribute
#' @param dimension Names or indices for dimensions whose categories are to be
#' reallocated. If the dimensions have \code{\link{dimtypes}}
#' \code{"origin"} and \code{"destination"} or \code{"parent"} and
#' \code{"child"}, then the base names may be used.
#' @param category Names or indices of the categories to be reallocated.
#' @param epsilon Small quantity added to counts for remaining categories.
#'
#' @return A \code{\linkS4class{Counts}} object without the categories
#' specified by \code{categories}.
#' @export
setGeneric("redistributeCategory",
           function(counts, dimension, category, means = FALSE, epsilon = 0, n = NULL)
               standardGeneric("redistributeCategory"))




#' Reset iteration labels.
#'
#' Reset the labels used by a dimension with \code{\link{dimtype}}
#' \code{"iteration"} to be \code{1:nIteration(object)}.
#'
#' @param object An object of class \code{\linkS4class{DemographicArray}}.
#' @examples
#' x <- ValuesOne(rnorm(10), labels = seq(2, 20, 2), name = "iteration")
#' resetIterations(x)
#' @export
setGeneric("resetIterations",
           function(object)
               standardGeneric("resetIterations"))



#' @rdname ageMinMax
#' @export
setGeneric("setAgeMax",
           function(object, value)
               standardGeneric("setAgeMax"))

#' @rdname ageMinMax
#' @export
setGeneric("setAgeMin",
           function(object, value)
               standardGeneric("setAgeMin"))


#' @rdname componentNames
#' @export
setGeneric("setComponentNames",
           function(object, value)
               standardGeneric("setComponentNames"))


#' Extract or replace a slab from a demographic array.
#'
#' Extract a slab \code{length(elements)} thick from \code{object},
#' perpendicular to the dimension specified by \code{dimension}, or replace the
#' current values of the slab with new values
#'
#' \code{slab} is designed for programming, in situations where the dimension
#' of an object is not fixed in advance (which makes \code{[} inconvenient).
#' In interactive use, \code{\link{subarray}} may be a better choice, as
#' it is clearer and more flexible.
#'
#' The replacement method ignores any attributes attached to \code{code} such
#' as dimensions or dimnames.  In particular, if \code{value} is a
#' \code{\linkS4class{DemographicArray}} object, any metadata is ignored.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param dimension Name or index of dimension.
#' @param elements Labels or indices of elements with in \code{dimension}.
#' @param drop Whether dimensions of length 1 should be dropped from the
#' result.
#' @param value An numeric vector, or an object that can be coerced to a
#' numeric vector.
#' @seealso \code{\link{subarray}}, \code{\link{[}}
#' @examples
#' library(demdata)
#' x <- Values(VADeaths2)
#' slab(x, dimension = "age", elements = 1:3)
#' slab(x, dimension = 1, elements = 1:3)
#' slab(x, dimension = "age", elements = c("50-54", "70-74"))
#' slab(x, dimension = "sex", elements = 1)
#' slab(x, dimension = "sex", elements = 1, drop = FALSE)
#'
#' slab(x, dimension = "sex", elements = "Male") <- 999
#' x
#' slab(x, dimension = "age", elements = "50-54") <- 1:4
#' x
#' @name slab
NULL

#' @rdname slab
#' @export
setGeneric("slab",
           function(object, dimension, elements, drop = TRUE)
           standardGeneric("slab"))

#' @rdname slab
#' @export
setGeneric("slab<-",
           function(object, dimension, elements, drop = TRUE, value)
           standardGeneric("slab<-"))

setGeneric("splitCategory",
           function(object, dimension, old, new, weights, ...)
           standardGeneric("splitCategory"))

#' @rdname exported-not-api
#' @export
setGeneric("stepLengths",
           function(object)
               standardGeneric("stepLengths"))

#' Subset a demographic array.
#'
#' Use a logical expression to select a subarray from within an object of class
#' \code{\linkS4class{DemographicArray}}.
#'
#' Selections involving \code{\link{dimscales}} \code{Points} or
#' \code{Intervals} can be made in the same way as selections involving other
#' dimscales, by specifying labels (in quotes.) However, they can also be made
#' using inequality operators \code{<}, \code{<=}, \code{>}, and \code{>=}.  An
#' intervals is selected by an inequality operator if the operator takes in the
#' entire interval.  For instance, interval \code{(a, a+n)} on dimension
#' \code{age} is selected by the expression \code{age > x} if \code{x <= a}.
#' The interval is not selected by \code{a < x}, even if \code{x < a + n}.
#' Expression \code{age > x} has the same effect as \code{age >= x}, based on
#' the assumption that no one is age \emph{exactly} \code{a} (and similarly for
#' time).
#'
#' \code{subarray} sometimes modifies the names, \code{\link{dimtypes}} or
#' \code{\link{dimscales}} of the return value so that they conform to the
#' rules governing dimtypes and dimscales.  For instance, when the return value
#' has only one dimension from an origin-destination pair, \code{subarray} will
#' change the dimtype of that dimension to \code{"state"}, and will remove the
#' \code{"_orig"} or \code{"_dest"} suffix from its name.  See below for a
#' second example where \code{subarray} changes the dimtypes and dimscales of
#' an \code{"age"} dimension.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param subarray Logical expression picking out a subarray within
#' \code{object}.
#' @param drop Logical, specifying whether dimensions of length 1 should be
#' retained in the result.  Defaults to \code{TRUE}.
#' @param \dots Further arguments to be passed to or from other methods.
#' @return Typically an object of class \code{\linkS4class{DemographicArray}}, but
#' maybe a vector in some cases when \code{drop} is \code{TRUE}.
#' @section Warning: The documentation for function \link[base]{subset} (the
#' inspiration for \code{subarray}) warns that the non-standard evaluation of
#' the \code{subset} argument can have unanticipated consequences.  It
#' recommends that, for programming, subsetting be done via standard functions
#' such as \code{[}.  The same advice applies to \code{subarray}: see below for
#' an example.  However, these sorts of problems normally only occur when using
#' \code{subarray} within another function.  In interactive use, or in simple
#' scripts to extract or manipulate a dataset, the greater transparency and
#' flexibility of \code{subarray} typically make it a better choice than
#' \code{[}.
#' @seealso \code{\link[base]{subset}} provides similar functionality for
#' vectors, matrices, or data frames, though without the requirement that the
#' resulting subsets have all the combinations of values required for an array.
#' @examples
#' library(demdata)
#' rates <- Values(VADeaths2)
#' ## selection using labels
#' subarray(rates, residence == "Rural")
#' subarray(rates, age == "50-54")
#' subarray(rates, age %in% c("50-54", "55-59"))
#'
#' ## selection using inequalities
#' subarray(rates, age > 60)
#' subarray(rates, age > 59)
#' subarray(rates, age >= 60)
#'
#' ## selection creates a gap in the age groups,
#' ## so dimscales changes to "Categories",
#' ## which means dimtypes must change to "state"
#' subarray(rates, age < 55 | age > 65)
#'
#' ## can select from multiple dimensions at once
#' subarray(rates, (age < 60) & (sex == "Male"))
#' ## but only if the selection is 'rectangular'
#' ## - ie results in an array
#' \dontrun{subarray(rates, (age > 60) | (sex == "Male"))}
#'
#' mig <- Counts(nz.mig)
#' subarray(mig, island_orig == "South Island")
#'
#' ## an example of the problems encountered calling
#' ## 'subarray' from within a function
#' f <- function(orig) {
#'     subarray(mig, island_orig = orig)
#' }
#' \dontrun{f()}
#' @export
setGeneric("subarray",
           function(object, ...)
               standardGeneric("subarray"))


#' Get subtotals attached to a Counts object.
#'
#' Extract the subtotals used by an object of class
#' \code{\linkS4class{CountsWithSubtotals}}.
#'
#' @param object An object of class \code{\linkS4class{CountsWithSubtotals}}.
#'
#' @examples
#' library(demdata)
#' popn <- Counts(VAPopn)
#' popn <- extrapolate(popn, along = "age", labels = "45-49", type = "missing")
#' subtotals <- Counts(array(c(65000, 64000),
#'                           dim = 1:2,
#' 			  dimnames = list(age = "45-49", sex = c("Male", "Female"))))
#' popn <- attachSubtotals(popn, subtotals = subtotals)
#' subtotals(popn)
#' @export
setGeneric("subtotals",
           function(object)
           standardGeneric("subtotals"))

setGeneric("subtractFromPopnEnd",
           function(object, population)
               standardGeneric("subtractFromPopnEnd"))

#' Calculate total fertility rates.
#'
#' Calculate total fertility rates from age-specific fertility rates.
#'
#' The total fertility rate (TFR) is a summary measure commonly used by
#' demographers.  It is the number of births that the average woman
#' have if she survived to the end the reproductive ages, and if
#' current age-specific fertility rates were to persist indefinitely.
#'
#' \code{object} will typically contain dimensions other than age,
#' such as time or region.  Separate total fertility rates are calculated
#' for each combination of these other variables.
#'
#' The calculations are done aggregating the "age", and, if present,
#' "sex" and Lexis triangle dimensions.  See \code{\link{dimtypes}}
#' for more on age, sex, and Lexis triangle dimtypes.)
#' If \code{object} containsa "sex" dimension, this is assumed
#' to be the sex of the child,
#' not the parent. Total fertility rates do not distinguish
#' female and male births.
#'
#' If \code{object} has iterations, to capture uncertainty, so will
#' the return value.
#' @param object An object of class \code{\linkS4class{Values}}.
#' \code{object} must have a dimension with \code{\link{dimtype}}
#' \code{"age"}.
#' @return An object of class \code{\linkS4class{Values}}, with no
#' "age", "sex", or Lexis triangle dimensions, or a numeric vector.
#' @examples
#' births <- demdata::nz.births.reg
#' popn <- demdata::nz.popn.reg
#' births <- Counts(births, dimscales = c(year = "Intervals"))
#' ## use mid-year population to approximate person-years lived
#' ## over year
#' popn <- Counts(popn, dimscales = c(year = "Intervals"))
#' females <- subarray(popn, sex == "Female")
#' fert.rates <- births / females
#' tfr(fert.rates)
#' @export
setGeneric("tfr",
           function(object)
               standardGeneric("tfr"))


#' Reduce the number of iterations in demographic array.
#'
#' Given an object of class \code{\linkS4class{DemographicArray}} that has a
#' dimension with \code{\link{dimtype}} \code{"iteration"}, return a new
#' object containing a subset of the original iterations.
#'
#' The \code{n} iterations to retain are chosen at random, and are numbered
#' \code{1}, \code{2}, ..., \code{n}.
#'
#' If \code{object} does not have a dimension with dimtype \code{"iteration"},
#' \code{thinIterations} throws an error.
#'
#' @param object Object of class \code{\linkS4class{DemographicArray}}.
#' @param n The number of iterations to retain.
#' @seealso Other functions that can be used to subset a dimension with
#' \code{\link{dimtype}} \code{"iteration"} are \code{\link{subarray}},
#' \code{\link{slab}}, and \code{\link{[}}.  To remove an \code{"iteration"}
#' dimension entirely, use \code{\link{collapseIterations}}.
#' @examples
#' x <- Counts(array(1:6,
#'                   dim = c(2, 3),
#'                   dimnames = list(sex = c("Female", "Male"), iteration = 1:3)))
#' thinIterations(x, n = 2)
#' @export
setGeneric("thinIterations",
           function(object, n)
           standardGeneric("thinIterations"))


#' Coerce to numeric or integer.
#'
#' Function \code{toInteger} uses \code{\link{round}} followed by
#' \code{\link{as.integer}} to coerce the data part of a
#' \code{\linkS4class{DemographicArray}} object to type \code{"integer"}.
#' Function \code{toDouble} uses \code{\link{as.double}} to coerce to type
#' \code{"double"}.  If the coercion to type \code{"integer"} requires
#' rounding, then an error is raised, unless \code{force} is \code{TRUE}.
#'
#'
#' @param object An object of class \code{\linkS4class{DemographicArray}} or a
#' numeric vector.
#' @param force \code{TRUE} or \code{FALSE} (the default).
#' @seealso \code{\link{as.integer}}, which, unlike \code{toInteger}, removes
#' all attributes.
#' @examples
#' ## A demographic array.
#' x <- Counts(array(c(1, 2, 3, 4),
#'                   dim = c(2, 2),
#'                   dimnames = list(sex = c("Female", "Male"),
#'                       region = c("a", "b"))))
#'
#' ## x consists entirely of whole numbers,
#' ## but has type "double", not "integer":
#' is.integer(x)
#' x <- toInteger(x)
#' is.integer(x)
#' x[1] <- 1.1
#'
#' ## rounding is required, so 'toInteger' won't work
#' \dontrun{toInteger(x)}
#' ## ...unless 'force' is TRUE
#' toInteger(x, force = TRUE)
#'
#' ## A numeric vector
#' x <- c(1, 2, 3)
#' is.integer(x)
#' x <- toInteger(x)
#' x
#' is.integer(x)
#' x <- toDouble(x)
#' x
#' is.integer(x)
#' @name coerce-data
NULL

#' @rdname coerce-data
#' @export
setGeneric("toDouble",
           function(object)
           standardGeneric("toDouble"))

#' @rdname coerce-data
#' @export
setGeneric("toInteger",
           function(object, force = FALSE)
           standardGeneric("toInteger"))



setGeneric("transformInvolvesSubsetting",
           function(object)
               standardGeneric("transformInvolvesSubsetting"))

setGeneric("transformIsOneToOne",
           function(object)
               standardGeneric("transformIsOneToOne"))
