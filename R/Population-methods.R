

## setMethod("accession",
##           signature(object = "Population"),
##           function(object) {
##               .Data.old <- object@.Data
##               dim.old <- dim(object)
##               names <- names(object)
##               dimtypes <- dimtypes(object, use.names = FALSE)
##               DimScales.old <- DimScales(object, use.names = FALSE)
##               i.age <- match("age", dimtypes, nomatch = 0L)
##               has.age <- i.age > 0L
##               if (has.age) {
##                   n.age <- dim.old[i.age]
##                   i.time <- match("time", dimtypes)
##                   DimScale.time.old <- DimScales.old[[i.time]]
##                   dv.time <- dimvalues(DimScale.time.old)
##                   n.time.old <- length(dv.time)
##                   DimScale.time.new <- methods::new("Intervals", dimvalues = dv.time)
##                   DimScales.new <- replace(DimScales,
##                                            list = i.time,
##                                            values = DimScale.time.new)
##                   metadata.new <- methods::new("MetaData",
##                                       nms = names,
##                                       dimtypes = dimtypes,
##                                       DimScales = DimScales.new)
##                   .Data.new <- array(0L,
##                                      dim = dim(metadata.new),
##                                      dimnames = dimnames(metadata.new))
##                   i.new <- slice.index(.Data.new, MARGIN = i.age) != 1L
##                   i.old <- ((slice.index(.Data.old, MARGIN = i.time) != n.time.old)
##                             & (slice.index(.Data.old, MARGIN = i.age) != n.age))
##                   .Data.new[i.new] <- .Data.old[i.old]
##                   i.new <- slice.index(.Data.new, MARGIN = i.age) == n.age
##                   i.old <- ((slice.index(.Data.old, MARGIN = i.time) == n.time.old)
##                             & (slice.index(.Data.old, MARGIN = i.age) == n.age))
##                   .Data.new[i.new] <- .Data.new[i.new] + .Data.old[i.old]
##                   methods::new("Counts", .Data = .Data.new, metadata = metadata.new)
##               }
##               else
##                   NULL
##           })


## setMethod("increments",
##           signature(object = "Population"),
##           function(object) {
##               .Data.old <- object@.Data
##               names <- name(object)
##               dim <- dim(object)
##               dimtypes <- dimtypes(object, use.names = FALSE)
##               DimScales.old <- DimScales(object, use.names = FALSE)
##               i.time <- match("time", dimtypes)
##               n.time <- dim[i.time]
##               DS.time.old <- DimScales.old[[i.time]]
##               dv.time <- dimvalues(DS.time.old)
##               DS.time.new <- methods::new("Intervals", dimvalues = dv.time)
##               DimScales.new <- replace(DimScales.old,
##                                        list = i.time,
##                                        values = DS.time.new)
##               metadata.new <- methods::new("MetaData",
##                                   nms = names,
##                                   dimtypes = dimtypes,
##                                   DimScales = DimScales.new)
##               i.start <- slice.index(.Data.old, MARGIN = i.time) != n.time
##               i.end <- slice.index(.Data.old, MARGIN = i.time) != 1L
##               .Data.new <- .Data.old[i.end] - .Data.old[i.new]
##               .Data.new <- array(.Data.new,
##                                  dim = dim(metadata.new),
##                                  dimnames = dimnames(metadata.new))
##               methods::new("Counts", .Data = .Data.new, metadata = metadata.new)
##           })
                                 
