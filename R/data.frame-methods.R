
#' Simplified version of 'xtabs' for constructing
#' demographic arrays
#'
#' \code{dtabs} is a simplified version of function
#' \code{\link[stats]{xtabs}} designed specifically for
#' constructing demographic arrays.
#'
#' The \code{data} argument comes first, so that \code{dtabs}
#' works nicely with pipes.
#'
#' The \code{fill} argument makes it easy to control what
#' value used for combinations of classifying variables
#' that do not occur in the original dataset \code{data}.
#' See below for examples.
#'
#' The return value is a plain \code{\link{array}}, not an
#' \code{\link{xtabs}} object.
#'
#' Unlike \code{xtabs}, \code{dtabs} has no \code{subset} argument.
#' Rather than being combined with the tabulation, subsetting
#' should be separated out into its own operation, via a
#' function such as \code{\link{subset}} or \code{\link[dplyr]{filter}}.
#'
#' @param data A data.frame or matrix.
#' @param formula A formula: see \code{\link{xtabs}} for details.
#' @param fill The value to use for combinations of
#' variables that do not occur in \code{data}.
#'
#' @return An array.
#'
#' @seealso \code{dtabs} is based on \code{\link{xtabs}}. To turn
#' a plain array created by \code{dtabs} into a demographic array,
#' use function \code{\link{Counts}} or \code{\link{Values}}.
#' 
#' @examples
#' d <- data.frame(age = c("young", "old", "young", "old"),
#'                 sex = c("Female", "Female", "Male", "Male"),
#'                 count = 1:4)
#' dtabs(d, count ~ age + sex)
#' dtabs(d, count ~ age)
#' dtabs(d, ~ age + sex)
#' dtabs(d, ~ age)
#'
#' d_incomplete <- data.frame(age = c("young", "old", "young"),
#'                            sex = c("Female", "Female", "Male"),
#'                            count = 1:3)
#' ## default value of fill is 0
#' dtabs(d_incomplete, count ~ age + sex)
#' dtabs(d_incomplete, count ~ age + sex, fill = NA)
#' @export
dtabs <- function(data, formula, fill = 0L) {
    if (missing(data))
        stop(gettextf("'%s' is missing with no default",
                      "data"))
    if (is.matrix(data))
        data <- as.data.frame(data)
    if (!is.data.frame(data))
        stop(gettextf("'%s' is not a %s",
                      "data", "data.frame"))
    if (missing(formula))
        stop(gettextf("'%s' is missing with no default",
                      "formula"))
    if (!methods::is(formula, "formula"))
        stop(gettextf("'%s' is not a formula",
                      "formula"))
    if (!identical(length(fill), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "fill", 1L))
    values <- stats::model.frame(formula = formula, data = data)
    has.response <- length(formula) > 2L
    if (has.response) {
        terms <- terms(values)
        i.response <- attr(terms,  "response")
        n.response <- length(i.response)
        if (n.response > 1L)
            stop(gettextf("formula '%s' contains more than one response variable",
                          deparse(formula)))
        INDEX <- values[-i.response]
        X <- values[[i.response]]
    }
    else {
        INDEX <- values
        X <- rep(1L, nrow(data))
    }
    tapply(X = X,
           INDEX = INDEX,
           FUN = sum,
           na.rm = FALSE,
           default = fill,
           simplify = TRUE)
}


