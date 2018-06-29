
## HAS_TESTS
#' @export
#' @method as.matrix Concordance
as.matrix.Concordance <- function(x, ...) {
    ans <- getConcValues(x)
    colnames(ans) <- classifications(x)
    ans
}

#' @rdname coercion
#' @export
setMethod("as.matrix",
          signature(x = "Concordance"),
          as.matrix.Concordance)

## HAS_TESTS
#' @export
#' @method as.data.frame Concordance
as.data.frame.Concordance <- function(x, row.names = NULL, optional = FALSE,
                                      stringsAsFactors = default.stringsAsFactors(), ...) {
    ans <- getConcValues(x)
    colnames(ans) <- classifications(x)
    as.data.frame(ans, row.names = row.names, optional = optional,
                  stringsAsFactors = stringsAsFactors, ...)
}

#' @rdname coercion
#' @export
setMethod("as.data.frame",
          signature(x = "Concordance"),
          as.data.frame.Concordance)

## HAS_TESTS
#' @export
#' @method as.list ManyToOne
as.list.ManyToOne <- function(x, ...) {
    classif.to <- classificationTo(x)
    classif.from <- classificationFrom(x)
    codes.to <- codes(x, classification = classif.to)
    codes.from <- codes(x, classification = classif.from)
    ans <- split(x = codes.from, f = codes.to)
    i <- match(names(ans), unique(codes.to))
    ans[i]
}

#' @rdname coercion
#' @export
setMethod("as.list",
          signature(x = "ManyToOne"),
          as.list.ManyToOne)

## HAS_TESTS
#' @rdname classifications
#' @export
setMethod("classificationFrom",
          signature(object = "ManyToOne"),
          function(object) object@classifications[1L])

## HAS_TESTS
#' @rdname classifications
#' @export
setMethod("classifications",
          signature(object = "Concordance"),
          function(object) object@classifications)

## HAS_TESTS
#' @rdname classifications
#' @export
setReplaceMethod("classifications",
                 signature(object = "Concordance", value = "ANY"),
                 function(object, value) {
                     value <- as.character(value)
                     object@classifications <- value
                     methods::validObject(object)
                     object
                 })

## HAS_TESTS
#' @rdname classifications
#' @export
setMethod("classificationTo",
          signature(object = "ManyToOne"),
          function(object) object@classifications[2L])

## HAS_TESTS
#' @rdname codes
#' @export
setMethod("codes",
          signature(object = "Concordance"),
          function(object, classification) {
              values <- getConcValues(object)
              classifications <- classifications(object)
              if (!identical(length(classification), 1L))
                  stop(gettextf("'%s' does not have length 1", "classification"))
              if (is.na(classification))
                  stop(gettextf("'%s' is missing", "classification"))
              i <- match(classification, classifications, nomatch = 0L)
              if (i == 0L)
                  stop(gettextf("'%s' outside valid range", "classification"))
              values[, i]
          })

## HAS_TESTS
#' @rdname codesAmbiguous
#' @export
setMethod("codesAmbiguous",
          signature(object = "Concordance"),
          function(object) {
              values <- getConcValues(object)
              c1 <- values[ , 1L]
              c2 <- values[ , 2L]
              shared.codes <-  intersect(c1, c2)
              if (length(shared.codes) > 0L) {
                  uses.shared <- (c1 %in% shared.codes) | (c2 %in% shared.codes)
                  !identical(c1[uses.shared], c2[uses.shared])
              }
              else
                  FALSE
          })

## HAS_TESTS
setMethod("getConcValues",
          signature(object = "Concordance"),
          function(object) object@values)

## HAS_TESTS
#' @rdname translate
#' @export
setMethod("translate",
          signature(object = "ANY",
                    concordance = "ManyToOne",
                    to = "missing"),
          function(object, concordance) {
              object <- tidyObjectToTranslate(object)
              classif.from <- classificationFrom(concordance)
              classif.to <- classificationTo(concordance)
              codes.from <- codes(concordance, classification = classif.from)
              codes.to <- codes(concordance, classification = classif.to)
              i <- match(object, codes.from, nomatch = 0L)
              unmatched <- i == 0L
              n.unmatched <- sum(unmatched)
              if (n.unmatched > 0L) {
                  codes.unmatched <- object[unmatched]
                  codes.unmatched <- dQuote(codes.unmatched)
                  codes.unmatched <- paste(codes.unmatched, collapse = ", ")
                  stop(sprintf(ngettext(n.unmatched,
                                        "value not found in classification \"%s\" : %s",
                                        "values not found in classification \"%s\" : %s"),
                               classif.from, codes.unmatched))
              }
              codes.to[i]
          })

## HAS_TESTS
#' @rdname translate
#' @export
setMethod("translate",
          signature(object = "ANY",
                    concordance = "ManyToOne",
                    to = "ANY"),
          function(object, concordance, to) {
              classif.to <- classificationTo(concordance)
              if (!identical(to, classif.to))
                  stop(gettextf("invalid value for '%s'", "to"))
              methods::callGeneric(object = object, concordance = concordance)
          })

## HAS_TESTS
#' @rdname translate
#' @export
setMethod("translate",
          signature(object = "ANY",
                    concordance = "OneToOne",
                    to = "ANY"),
          function(object, concordance, to = NULL) {
              values <- getConcValues(concordance)
              classifications <- classifications(concordance)
              object <- tidyObjectToTranslate(object)
              if (!identical(length(to), 1L))
                  stop(gettextf("'%s' does not have length %d", "to", 1L))
              i.to <- match(to, classifications, nomatch = 0L)
              if (i.to == 0L)
                  stop(gettextf("'%s' outside valid range", "to"))
              codes.to <- values[ , i.to]
              codes.from <- values[ , 3L - i.to]
              i <- match(object, codes.from, nomatch = 0L)
              unmatched <- i == 0L
              n.unmatched <- sum(unmatched)
              if (n.unmatched > 0L) {
                  classif.from <- classifications[3L - i.to]
                  codes.unmatched <- object[unmatched]
                  codes.unmatched <- dQuote(codes.unmatched)
                  codes.unmatched <- paste(codes.unmatched, collapse = ", ")
                  stop(sprintf(ngettext(n.unmatched,
                                        "value not found in classification \"%s\" : %s",
                                        "values not found in classification \"%s\" : %s"),
                               classif.from, codes.unmatched))
              }
              codes.to[i]
          })

## HAS_TESTS
#' @rdname translate
#' @export
setMethod("translate",
          signature(object = "ANY",
                    concordance = "OneToOne",
                    to = "NULL"),
          function(object, concordance, to = NULL) {
              object <- tidyObjectToTranslate(object)
              values <- getConcValues(concordance)
              classifications <- classifications(concordance)
              c1 <- values[ , 1L]
              c2 <- values[ , 2L]
              all.in.c1 <- all(object %in% c1)
              all.in.c2 <- all(object %in% c2)
              if (all.in.c1) {
                  if (all.in.c2) {
                      if (!codesAmbiguous(concordance))
                          return(object)
                      else
                          stop(gettextf("\"%s\" and \"%s\" both contain all values in '%s'",
                                        classifications[1L], classifications[2L], "object"))
                  }
                  else
                      to <- classifications[2L]
              }
              else {
                  if (all.in.c2)
                      to <- classifications[1L]
                  else
                      stop(gettextf("neither \"%s\" nor \"%s\" contain all values in '%s'",
                                    classifications[1L], classifications[2L], "object"))
              }
              methods::callGeneric(object = object, concordance = concordance, to = to)
          })

## HAS_TESTS
#' @rdname translate
#' @export
setMethod("translate",
          signature(object = "ANY",
                    concordance = "OneToOne",
                    to = "missing"),
          function(object, concordance) {
              methods::callGeneric(object = object, concordance = concordance, to = NULL)
          })

## HAS_TESTS
#' @rdname translate
#' @export
setMethod("translate",
          signature(object = "ANY",
                    concordance = "data.frame",
                    to = "ANY"),
          function(object, concordance, to = NULL, ...) {
              concordance <- tryCatch(Concordance(concordance),
                                      error = function(e) e)
              if (methods::is(concordance, "error"))
                  stop(gettextf("could not make concordance from '%s' : %s",
                                "concordance", concordance$message))
              methods::callGeneric()
          })

## HAS_TESTS
#' @rdname translate
#' @export
setMethod("translate",
          signature(object = "ANY",
                    concordance = "matrix",
                    to = "ANY"),
          function(object, concordance, to = NULL, ...) {
              concordance <- tryCatch(Concordance(concordance),
                                      error = function(e) e)
              if (methods::is(concordance, "error"))
                  stop(gettextf("could not make concordance from '%s' : %s",
                                "concordance", concordance$message))
              methods::callGeneric()
          })

## HAS_TESTS
#' @rdname Concordance-class
#' @export
setMethod("show",
          signature(object = "Concordance"),
          function(object) {
              values <- getConcValues(object)
              classifications <- classifications(object)
              colnames(values) <- classifications
              cat("An object of class \"", class(object), "\"\n", sep = "")
              print(values)
          })

