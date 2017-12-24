
#' @rdname redistribute
## NO_TESTS - has some, needs more for means = TRUE
setMethod("redistribute",
          signature(counts = "numeric",
                    weights = "DemographicArray"),
          function(counts, weights, means = FALSE, n = NULL) {
              if (!is.null(dim(counts)))
                  stop(gettextf("'%s' has class \"%s\"",
                                "counts", class(counts)))
              if (!identical(length(counts), 1L))
                  stop(gettextf("'%s' does not have length %d",
                                "%s", 1L))
              if (is.na(counts))
                  stop(gettextf("'%s' is missing",
                                "counts"))
              if (round(counts) != counts)
                  stop(gettextf("'%s' is not an integer",
                                "counts"))
              counts <- as.integer(counts)
              if (counts < 0L)
                  stop(gettextf("'%s' is negative",
                                "counts"))
              if ("quantile" %in% dimtypes(weights))
                  stop(gettextf("'%s' has dimension with %s \"%s\"",
                                "weights", "dimtype", "quantile"))
              if (identical(length(weights), 0L))
                  stop(gettextf("'%s' has length %d",
                                "weights", 0L))
              if (any(is.na(weights)))
                  stop(gettextf("'%s' has missing values",
                                "weights"))
              if (any(weights < 0))
                  stop(gettextf("'%s' has negative values",
                                "weights"))
              if (isTRUE(all.equal(sum(weights), 0)))
                  stop(gettextf("'%s' sums to %d",
                                "weights", 0))
              checkMeans(means)
              weights <- methods::as(weights, "Counts")
              has.iter <- "iteration" %in% dimtypes(weights)
              if (!has.iter) {
                  n <- checkAndTidyN(n)
                  if (!is.null(n)) {
                      iterations <- seq_len(n)
                      metadata.wt <- addIterationsToMetadata(metadata(weights),
                                                             iterations = iterations)
                      .Data.wt <- rep(as.double(weights), times = n)
                      .Data.wt <- array(.Data.wt,
                                        dim = dim(metadata.wt),
                                        dimnames = dimnames(metadata.wt))
                      weights <- methods::new("Counts",
                                     .Data = .Data.wt,
                                     metadata = metadata.wt)
                      has.iter <- TRUE
                  }
              }
              if (has.iter) {
                  i.iter <- match("iteration", dimtypes(weights))
                  metadata.ct <- metadata(weights)[i.iter]
                  .Data.ct <- array(counts,
                                    dim = dim(metadata.ct),
                                    dimnames = dimnames(metadata.ct))
                  counts <- methods::new("Counts", .Data = .Data.ct, metadata = metadata.ct)
              }
              transform <- makeTransform(x = weights, y = counts)
              transform <- makeCollapseTransformExtra(transform)
              if (means)
                  .Data <- redistributeInnerMeans(counts = as.integer(counts),
                                                  weights = as.double(weights),
                                                  transform = transform,
                                                  useC = TRUE)
              else
                  .Data <- redistributeInnerDistn(counts = as.integer(counts),
                                                  weights = as.double(weights),
                                                  transform = transform,
                                                  useC = TRUE) 
              metadata <- metadata(weights)
              .Data <- array(.Data, dim = dim(metadata), dimnames = dimnames(metadata))
              methods::new("Counts", .Data = .Data, metadata = metadata)
          })


## HAS_TESTS
#' @rdname round3
#' @export
setMethod("round3",
          signature(object = "numeric"),
          function(object) {
              is.type.integer <- is.integer(object)
              all.integers <- is.type.integer || all(round(object) == object)
              if (!all.integers)
                  stop(gettextf("'%s' has non-integer values",
                                "object"))
              mod.3 <- as.integer(object) %% 3L
              n <- length(object)
              p <- stats::runif(n = n)
              ## deal with NAs - leave untouched
              has.been.processed <- is.na(object)
              ## deal with values divisible by 3 - leave untouched
              is.mod.0 <- !has.been.processed & (mod.3 == 0L)
              has.been.processed <- has.been.processed | is.mod.0
              ## deal with mod 1 - 2/3 chance of rounding down, 1/3 chance of rounding up
              is.mod.1 <- !has.been.processed & (mod.3 == 1L)
              round.down <- is.mod.1 & (p < 2/3)
              round.up <- is.mod.1 & (p >= 2/3)
              object[round.down] <- object[round.down] - 1L
              object[round.up] <- object[round.up] + 2L
              has.been.processed <- has.been.processed | is.mod.1
              ## deal with mod 2 - 1/3 chance of rounding down, 2/3 chance of rounding up
              is.mod.2 <- !has.been.processed
              round.down <- is.mod.2 & (p < 1/3)
              round.up <- is.mod.2 & (p >= 1/3)
              object[round.down] <- object[round.down] - 2L
              object[round.up] <- object[round.up] + 1L
              ## coerce back to numeric if was originally numeric and is now integer
              if (!is.type.integer && is.integer(object))
                  object <- as.numeric(object)
              ## recreate object to trigger validity tests
              object
          })

#' @rdname coerce-data
## HAS_TESTS
setMethod("toDouble",
          signature(object = "numeric"),
          function(object) {
              as.double(object)
          })

#' @rdname coerce-data
## HAS_TESTS
setMethod("toInteger",
          signature(object = "integer"),
          function(object, force = FALSE) {
              object
          })

#' @rdname coerce-data
## HAS_TESTS
setMethod("toInteger",
          signature(object = "numeric"),
          function(object, force = FALSE) {
              if (!force) {
                  values <- object[!is.na(object)]
                  if (!isTRUE(all.equal(values, round(values))))
                      stop(gettext("non-integer values"))
              }
              as.integer(round(object))
          })

## HAS_TESTS
setMethod("checkAndTidyWeights",
          signature(weights = "numeric",
                    target = "DemographicArray"),
          function(weights, target, nameWeights = "weights") {
              if (isTRUE(all.equal(weights, 1.0))) {
                  metadata <- metadata(target)
                  .Data <- array(1.0,
                                 dim = dim(metadata),
                                 dimnames = dimnames(metadata))
                  methods::new("Counts", .Data = .Data, metadata = metadata)
              }
              else
                  stop(gettextf("'%s' invalid",
                                nameWeights))
          })
