
#' @rdname attachSubtotals
#' @export
setMethod("attachSubtotals",
          signature(object = "Counts",
                    subtotals = "Counts"),
          function(object, subtotals, concordances = list()) {
              ## 'object' has no "iteration" or "quantile" dimensions
              for (dimtype in c("iteration", "quantile")) {
                  if (dimtype %in% dimtypes(object))
                      stop(gettextf("'%s' has dimension with dimtype \"%s\"",
                                    "object", dimtype))
              }
              ## 'object' has only integer values
              if (any(round(as.numeric(object)) != as.numeric(object), na.rm = TRUE))
                  stop(gettextf("'%s' has non-integer values",
                                "object"))
              ## 'object' has no negative values
              if (any(object < 0, na.rm = TRUE))
                  stop(gettextf("'%s' has negative values",
                                "object"))
              ## subtotals has no "iteration" or "quantile" dimensions
              for (dimtype in c("iteration", "quantile")) {
                  if (dimtype %in% dimtypes(subtotals))
                      stop(gettextf("'%s' has dimension with dimtype \"%s\"",
                                    "subtotals", dimtype))
              }
              ## 'subtotals' has no missing values
              if (any(is.na(subtotals)))
                  stop(gettextf("'%s' has missing values",
                                "subtotals"))
              ## 'subtotals' has only integer values
              if (any(round(as.numeric(subtotals)) != as.numeric(subtotals)))
                  stop(gettextf("'%s' has non-integer values",
                                "subtotals"))
              ## 'subtotals' has no negative values
              if (any(subtotals < 0))
                  stop(gettextf("'%s' has negative values",
                                "subtotals"))
              .Data <- object@.Data
              .Data <- as.integer(.Data)
              metadata <- object@metadata
              transform <- makeTransform(x = object,
                                         y = subtotals,
                                         subset = TRUE,
                                         concordances = concordances)
              transform <- makeCollapseTransformExtra(transform)
              ## transform not one-to-one
              if (transformIsOneToOne(transform))
                  stop(gettextf("'%s' has one-to-one relationship with '%s'",
                                "object", "subtotals"))
              ## 'subtotals' at least as large as collapsed values 
              metadata.subtotals <- metadata(subtotals)
              subtotals <- as.integer(subtotals)
              .Data <- array(.Data, dim = dim(object), dimnames = dimnames(object))
              .Data.zeros <- .Data
              .Data.zeros[is.na(.Data.zeros)] <- 0L
              .Data.zeros.collapsed <- collapse(.Data.zeros, transform = transform)
              subtotals.net <- subtotals - .Data.zeros.collapsed
              subtotals.net <- as.integer(subtotals.net)
              if (any(subtotals.net < 0L))
                  stop(gettextf("'%s' has values that are less than the sum of the associated values from '%s'",
                                "subtotals", "object"))
              object <- methods::new("Counts", .Data = .Data, metadata = metadata)
              methods::new("CountsWithSubtotals",
                           object,
                           subtotals = subtotals,
                           subtotalsNet = subtotals.net,
                           metadataSubtotals = metadata.subtotals,
                           transformSubtotals = transform)
          })

#' @rdname attachSubtotals
#' @export
setMethod("attachSubtotals",
          signature(object = "Counts",
                    subtotals = "Values"),
          function(object, subtotals, concordances = list()) {
              stop(gettextf("'%s' has class \"%s\"",
                            "subtotals", class(subtotals)))
          })

#' @rdname attachSubtotals
#' @export
setMethod("attachSubtotals",
          signature(object = "Counts",
                    subtotals = "numeric"),
          function(object, subtotals, concordances = list()) {
              ## 'object' has no "iteration" or "quantile" dimensions
              for (dimtype in c("iteration", "quantile")) {
                  if (dimtype %in% dimtypes(object))
                      stop(gettextf("'%s' has dimension with dimtype \"%s\"",
                                    "object", dimtype))
              }
              ## 'object' has only integer values
              if (any(round(as.numeric(object)) != as.numeric(object), na.rm = TRUE))
                  stop(gettextf("'%s' has non-integer values",
                                "object"))
              ## 'object' has no negative values
              if (any(object < 0, na.rm = TRUE))
                  stop(gettextf("'%s' has negative values",
                                "object"))
              ## 'object' has length of at least 2
              if (length(object) < 2L)
                  stop(gettextf("'%s' has length %d",
                                "object", length(object)))
              ## 'subtotals' has length 1
              if (!identical(length(subtotals), 1L))
                  stop(gettextf("'%s' does not have length %d",
                                "subtotals", 1L))
              ## 'subtotals' is not missing
              if (is.na(subtotals))
                  stop(gettextf("'%s' is missing",
                                "subtotals"))
              ## 'subtotals' has integer value
              if (round(as.numeric(subtotals)) != as.numeric(subtotals))
                  stop(gettextf("'%s' is not an integer",
                                "subtotals"))
              ## 'subtotals' is not negative
              if (subtotals < 0)
                  stop(gettextf("'%s' is negative",
                                "subtotals"))
              ## non-null 'concordances' ignored
              if (!identical(concordances, list()))
                  warning(gettextf("'%s' argument ignored when '%s' has class \"%s\"",
                                   "concordances", "subtotals", class(subtotals)))
              .Data <- object@.Data
              .Data <- as.integer(.Data)
              .Data <- array(.Data, dim = dim(object), dimnames = dimnames(object))
              subtotals <- as.integer(subtotals)
              subtotals.net <- subtotals - sum(.Data, na.rm = TRUE)
              ## 'subtotals' at least as large as collapsed values
              if (subtotals.net < 0L)
                  stop(gettextf("'%s' is less than the sum of '%s'",
                                "subtotals", "object"))
              metadata <- object@metadata
              transform <- makeTransform(x = object,
                                         y = subtotals,
                                         subset = FALSE,
                                         concordances = list())
              transform <- makeCollapseTransformExtra(transform)
              metadata.subtotals <- NULL
              object <- methods::new("Counts", .Data = .Data, metadata = metadata)
              methods::new("CountsWithSubtotals",
                           object,
                           subtotals = subtotals,
                           subtotalsNet = subtotals.net,
                           metadataSubtotals = metadata.subtotals,
                           transformSubtotals = transform)
          })












