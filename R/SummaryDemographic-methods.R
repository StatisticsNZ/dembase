
#' @rdname internal-methods
## HAS_TESTS
setMethod("show",
          signature(object = "SummaryDemographicArray"),
          function(object) {
              metadata <- metadata(object)
              stats <- object@stats
              showMetaData(metadata)
              cat("\n")
              methods::show(stats)
          })

## HAS_TESTS
setMethod("metadata",
          signature(object = "SummaryDemographicArray"),
          function(object) object@metadata)
