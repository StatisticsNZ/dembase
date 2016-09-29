
## HAS_TESTS
setMethod("transformInvolvesSubsetting",
          signature(object = "CollapseTransform"),
          function(object) {
              indices <- object@indices
              for (ind in indices) {
                  if (any(ind == 0L))
                      return(TRUE)
              }
              FALSE
          })

## HAS_TESTS
setMethod("transformInvolvesSubsetting",
          signature(object = "ExtendTransform"),
          function(object) {
              dims <- object@dims
              indices <- object@indices
              dim.before <- object@dimBefore
              for (i in seq_along(dims)) {
                  mar.origin <- dims[i]
                  if (mar.origin > 0L) {
                      length.dim.origin <- dim.before[mar.origin]
                      s.origin <- seq_len(length.dim.origin)
                      indices.origin <- indices[[i]]
                      if (!setequal(indices.origin, s.origin))
                          return(TRUE)
                  }
              }
              FALSE
          })

## HAS_TESTS
setMethod("transformIsOneToOne",
          signature(object = "CollapseTransform"),
          function(object) {
              indices <- object@indices
              nonZeroUnique <- function(x) !any(duplicated(x[x != 0L]))
              non.zero.unique <- sapply(indices, nonZeroUnique)
              all(non.zero.unique)
          })


          

              




