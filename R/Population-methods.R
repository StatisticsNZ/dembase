


## HAS_TESTS
#' @rdname midpoints
#' @export
setMethod("midpoints",
          signature(object = "Population", dimension = "ANY"),
          function(object, dimension) {
              object <- as(object, "Counts")
              callGeneric()
          })

## HAS_TESTS
#' @rdname midpoints
#' @export
setMethod("midpoints",
          signature(object = "Population", dimension = "missing"),
          function(object) {
              object <- as(object, "Counts")
              callGeneric()
          })
          
                                 
