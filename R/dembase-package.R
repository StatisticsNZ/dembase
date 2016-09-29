
#' Analysing cross-classified data about populations
#'
#' General-purpose tools for demographic analysis, broadly defined.
#' Includes facilities for data manipulation, multistate models,
#' projections, and plotting. Uses S4 classes and methods.
#'
#' The basic data structures are
#' \code{\link[=DemographicArray-class]{demographic arrays}} and
#' \code{\link[=DemographicAccount-class]{demographic accounts}}.
#'
#' Packages \code{demest} and \code{demlife} build on \code{dembase}:
#' \code{demest} has facilities for Bayesian estimation and forecasting,
#' and \code{demlife} has facilities for life tables.  Package
#' \code{demdata} has example datasets.
#'
#' @section Creating objects:
#' \code{\link{Counts}},
#' \code{\link{CountsOne}},
#' \code{\link{Movements}},
#' \code{\link{Net}},
#' \code{\link{Pool}},
#' \code{\link{Values}},
#' \code{\link{ValuesOne}}
#'
#' @section Getting or setting metadata:
#' \code{\link{ageTimeStep}},
#' \code{\link{dimnames}},
#' \code{\link{dimscales}},
#' \code{\link{dimtypes}},
#' \code{\link{hasRegularAgeTime}},
#' \code{\link{limits}},
#' \code{\link{midpoints}},
#' \code{\link[=names-methods]{names}},
#' \code{\link{nIteration}},
#' \code{\link{pairAligned}},
#' \code{\link{resetIterations}}
#'
#' @section Visualization:
#' \code{\link{dplot}},
#' \code{\link{plot}}
#'
#' @section Reshaping and manipulating:
#' \code{\link{addDimension}},
#' \code{\link{alignPair}},
#' \code{\link{aperm}},
#' \code{\link{collapseCategories}},
#' \code{\link{collapseDimension}},
#' \code{\link{collapseIntervals}},
#' \code{\link{collapseIterations}},
#' \code{\link{collapseOrigDest}},
#' \code{\link{drop}},
#' \code{\link{expandCategories}}
#'
#' @section Extracting or replacing:
#' \code{[},
#' \code{\link{slab}},
#' \code{\link{subarray}},
#' \code{\link{subtotals}},
#' \code{\link{thinIterations}}
#'
#' @section Combining:
#' \code{\link{attachSubtotals}},
#' \code{\link{dbind}}
#'
#' @section Coercion:
#' \code{\link{as}},
#' \code{\link[=as.data.frame]{as.data.frame}}
#'
#' @section Demographic and statistical calculations:
#' \code{\link{exposure}},
#' \code{\link{extrapolate}},
#' \code{\link{growth}},
#' \code{\link{impute}},
#' \code{\link{perturb}},
#' \code{\link{prop.table}},
#' \code{\link{redistribute}},
#' \code{\link{redistributeCategory}},
#' \code{\link{tfr}}
#'
#' @docType package
#' @name dembase
#' @importFrom stats quantile
#' @import methods
#' @useDynLib dembase, .registration = TRUE
NULL

