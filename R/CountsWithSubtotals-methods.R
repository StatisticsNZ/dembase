## HAS_TESTS
## Extract returns ordinary object of class "Counts". In future do something fancier?
#' @rdname internal-methods
#' @export
setMethod("[",
          signature(x = "CountsWithSubtotals", i = "ANY", j = "ANY"),
          function(x, i, j, ..., drop = TRUE) {
              x <- methods::as(x, "Counts")
              ## would like to use callGeneric here, but can't do so because
              ## '[' is a primitive function with tricky dispatch
              nargs <- nargs() - methods::hasArg(drop)
              if (nargs == 1L)
                  return(x)
              else if (nargs == 2L)
                  .Data <- methods::callGeneric(x = x@.Data, i = i, drop = FALSE)
              else
                  .Data <- methods::callGeneric(x = x@.Data, i = i, j = j, ..., drop = FALSE)
              dimnames.without.drop <- dimnames(.Data)
              if (drop)
                  .Data <- drop(.Data)
              dim.after <- dim(.Data)
              if (is.null(dim.after))
                  return(.Data)
              dim.before <- dim(x)
              dims <- match(names(x), names(dimnames(.Data)), nomatch = 0L)
              indices <- mapply(match,
                                x = dimnames(x),
                                table = dimnames.without.drop,
                                nomatch = 0L,
                                USE.NAMES = FALSE,
                                SIMPLIFY = FALSE)
              transform <- methods::new("CollapseTransform",
                               dims = dims,
                               indices = indices,
                               dimBefore = dim.before,
                               dimAfter = dim.after)
              metadata <- collapse(metadata(x), transform = transform)
              .Data <- array(.Data, dim = dim(metadata), dimnames = dimnames(metadata))
              methods::new("Counts", .Data = .Data, metadata = metadata)
          })

## HAS_TESTS
#' @rdname impute
#' @export
setMethod("impute",
          signature(object = "CountsWithSubtotals"),
          function(object, mult = NULL, max = NULL) {
              if (!is.null(mult) && !isTRUE(mult))
                  stop(gettextf("'%s' must be '%s' or '%s'",
                                "mult", "NULL", "TRUE"))
              metadata <- object@metadata
              subtotals <- object@subtotals
              subtotals.net <- object@subtotalsNet
              transform <- object@transformSubtotals
              i.missing <- which(is.na(object@.Data))
              object <- methods::new("Counts", .Data = object@.Data, metadata = metadata)
              object <- impute(object, mult = TRUE, max = max)
              .Data <- object@.Data
              for (i.sub in seq_along(subtotals.net)) {
                  size.gross <- subtotals[i.sub]
                  size.net <- subtotals.net[i.sub]
                  i.obj <- getIBefore(i = i.sub, transform = transform, useC = TRUE)
                  i.obj.missing <- intersect(i.obj, i.missing)
                  if (length(i.obj.missing) > 0L) {
                      if (!is.null(max)) {
                          max.sub.gross <- max[i.obj]
                          if (size.gross > sum(max.sub.gross))
                              stop(gettextf("element %d of '%s' exceeds sum of associated values of '%s'",
                                            i.sub, "subtotals", "max"))
                      }
                      prob <- .Data[i.obj.missing]
                      if (all(prob == 0L))
                          prob <- rep(1L, times = length(prob))
                      imputed.values <- stats::rmultinom(n = 1L, size = size.net, prob = prob)
                      if (!is.null(max)) {
                          max.sub.net <- max[i.obj.missing]
                          imputed.values <- reallocateOvers(x = imputed.values, max = max.sub.net)
                      }
                      .Data[i.obj.missing] <- imputed.values
                  }
              }
              methods::new("Counts", .Data = .Data, metadata = metadata)
          })

#' @rdname internal-methods
#' @export
setMethod("show",
          signature(object = "CountsWithSubtotals"),
          function(object) {
              methods::callNextMethod()
              subtotals <- object@subtotals
              metadata <- object@metadataSubtotals
              if (is.null(metadata))
                  cat("\nsubtotals:", subtotals, "\n")
              else {
                  subtotals <- array(subtotals, dim = dim(metadata), dimnames = dimnames(metadata))
                  cat("\nsubtotals:\n\n")
                  methods::show(subtotals)
              }
          })

## NO_TESTS
#' @rdname subtotals
#' @export
setMethod("subtotals",
          signature(object = "CountsWithSubtotals"),
          function(object) {
              subtotals <- object@subtotals
              metadata <- object@metadataSubtotals
              if (is.null(metadata))
                  subtotals
              else {
                  .Data <- array(subtotals, dim = dim(metadata), dimnames = dimnames(metadata))
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
          })
