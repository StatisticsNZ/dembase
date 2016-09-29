
## HAS_TESTS
#' @method summary DemographicArray
#' @export
summary.DemographicArray <- function(object, ...) {
    metadata <- metadata(object)
    .Data <- object@.Data
    .Data <- as.numeric(.Data)
    stats <- summary(.Data, ...)
    methods::new("SummaryDemographicArray", metadata = metadata, stats = stats)
}

#' @rdname internal-methods
#' @export
setMethod("summary",
          signature(object = "DemographicArray"),
          summary.DemographicArray)

