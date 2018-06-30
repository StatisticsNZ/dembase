
## HAS_TESTS
#' Create a concordance.
#' 
#' Create an object of class \code{"\linkS4class{Concordance}"}.
#' 
#' \code{object} should contain two classifications, one in each column.  The
#' names of the classifications are taken from \code{colnames(object)}.
#' Duplicate rows are deleted.  The classifications must have a many-to-one or
#' one-to-one relationship.
#' 
#' @param object A matrix or data frame with two columns and unique colnames.
#' @return An object of class \code{"\linkS4class{Concordance}"}.
#' @seealso \code{"\linkS4class{Concordance}"}, \code{classifications}
#' @examples
#' x <- cbind(c1 = c("a", "b", "c"), c2 = c("x", "y", "x"))
#' x <- Concordance(x)
#' classifications(x)
#' 
#' x <- cbind(c1 = c("a", "b", "c"), c2 = c("x", "y", "x"))
#' x <- Concordance(x)
#' x
#' classifications(x)
#' @export
Concordance <- function(object) {
    if (!identical(length(dim(object)), 2L))
        stop(gettext("does not have two dimensions"))
    if (!identical(ncol(object), 2L))
        stop(gettext("does not have two columns"))
    classifications <- colnames(object)
    if (is.null(classifications))
        stop(gettext("does not have colnames"))
    if (any(is.na(classifications)))
        stop(gettext("colnames have missing values"))
    if (any(duplicated(classifications)))
        stop(gettext("colnames have duplicates"))
    if (any(!nzchar(classifications)))
        stop(gettext("colnames have blanks"))
    if (is.data.frame(object))
        object <- as.matrix(object)
    values <- matrix(as.character(object),
                     nrow = nrow(object),
                     ncol = ncol(object))
    ## include tests here rather than leaving for 'validObject',
    ## to provide more meaningful error messages
    if (any(is.na(values)))
        stop(gettext("missing values"))
    values <- unique(values)
    is.duplicated <- apply(values, 2, anyDuplicated) > 0L
    if (identical(is.duplicated, c(FALSE, FALSE))) {
        class <- "OneToOne"
    }
    else if (identical(is.duplicated, c(TRUE, FALSE))) {
        class <- "ManyToOne"
        values <- values[ , 2:1]
        classifications <- classifications[2:1]
    }
    else if (identical(is.duplicated, c(FALSE, TRUE)))
        class <- "ManyToOne"
    else
        stop(gettext("relationship neither one-to-one nor many-to-one"))
    methods::new(class, values = values, classifications = classifications)
}

