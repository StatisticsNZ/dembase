
## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("collapse",
          signature(object = "array", transform = "CollapseTransform"),
          function(object, transform) {
              methods::validObject(object)
              methods::validObject(transform)
              if (!is.numeric(object))
                  stop(gettextf("'%s' does not have type \"%s\"",
                                "object", "numeric"))
              if (!identical(transform@dimBefore, dim(object)))
                  stop(gettextf("'%s' does not have the dimensions expected by '%s'",
                                  "object", "transform"))
              .Call(collapse_R, object, transform)
          })

## HAS_TESTS
#' @rdname exported-not-api
#' @export
setMethod("extend",
          signature(object = "array", transform = "ExtendTransform"),
          function(object, transform) {
              methods::validObject(object)
              methods::validObject(transform)
              if (!is.numeric(object))
                  gettextf("'%s' does not have type \"%s\"",
                           "object", "numeric")
              if (!identical(transform@dimBefore, dim(object)))
                  return(gettextf("'%s' does not have the dimensions expected by '%s'",
                                  "object", "transform"))
              .Call(extend_R, object, transform)
          })


## HAS_TESTS
#' @rdname round3
#' @export
setMethod("round3",
          signature(object = "array"),
          function(object) {
              dim <- dim(object)
              dimnames <- dimnames(object)
              if (is.integer(object))
                  object <- as.integer(object)
              else
                  object <- as.numeric(object)
              object <- round3(object)
              array(object,
                    dim = dim,
                    dimnames = dimnames)
          })




