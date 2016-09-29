
#' @rdname SubArrayIndices-class
## HAS_TESTS
setMethod("&",
          signature(e1 = "SubArrayIndices", e2 = "SubArrayIndices"),
          function(e1, e2) {
              nms1 <- e1@nms
              nms2 <- e2@nms
              indices1 <- e1@indices
              indices2 <- e2@indices
              nms <- union(nms1, nms2)
              indices <- vector(mode = "list", length = length(nms))
              for (i in seq_along(indices)) {
                  nm <- nms[i]
                  i1 <- match(nm, nms1, nomatch = 0L)
                  i2 <- match(nm, nms2, nomatch = 0L)
                  if (i1 > 0L) {
                      if (i2 > 0L)
                          indices[[i]] <- indices1[[i1]] & indices2[[i2]]
                      else
                          indices[[i]] <- indices1[[i1]]
                  }
                  else
                      indices[[i]] <- indices2[[i2]]
              }
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

#' @rdname SubArrayIndices-class
## HAS_TESTS
setMethod("|",
          signature(e1 = "SubArrayIndices", e2 = "SubArrayIndices"),
          function(e1, e2) {
              nms1 <- e1@nms
              nms2 <- e2@nms
              indices1 <- e1@indices
              indices2 <- e2@indices
              if (!(identical(length(nms1), 1L) &&
                        identical(length(nms2), 1L) &&
                            identical(nms1, nms2)))
                  stop(gettextf("'%s' operator applied to multiple dimensions",
                                "|"))
              nms <- nms1
              indices <- list(indices1[[1L]] | indices2[[1L]])
              methods::new("SubArrayIndices", nms = nms, indices = indices)
          })

#' @rdname SubArrayIndices-class
## HAS_TESTS
setMethod("!",
  signature(x = "SubArrayIndices"),
  function(x) {
      nms <- x@nms
      indices <- x@indices
      if (length(nms) > 1L)
          stop(gettextf("attempt to apply '%s' operator to expression involving more than one dimension",
                        "!"))
      indices <- list(!indices[[1L]])
      methods::new("SubArrayIndices", nms = nms, indices = indices)
  })
