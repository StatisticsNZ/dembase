

MetaData <- function(object, dimtypes = NULL, dimscales = NULL) {
  dimnames <- dimnames(object)
  if (is.null(dimnames))
      stop(gettextf("'%s' does not have dimnames",
                    "object"))
  checkDimnames(dimnames, includeNames = TRUE)
  is.null.dimnames <- sapply(dimnames, is.null)
  is.zero.length <- dim(object) == 0L
  if (any(is.null.dimnames & !is.zero.length))
      stop(gettext("dimension with no dimnames"))
  nms <- names(dimnames)
  n <- length(nms)
  checkDimtypesOrDimscalesArg(arg = dimtypes,
                              nameArg = "dimtypes",
                              names = nms)
  checkDimtypesOrDimscalesArg(arg = dimscales,
                              nameArg = "dimscales",
                              names = nms)
  valid.dimtypes <- getValidDimtypes()
  for (dimtype in dimtypes) {
      if (!(dimtype %in% valid.dimtypes))
          stop(gettextf("'%s' is not a valid dimtype",
                        dimtype))
  }
  nms.dt <- names(dimtypes)
  nms.ds <- names(dimscales)
  dimtypes.new <- inferDimtypes(nms)
  DimScales.new <- rep(list(NULL), times = n)
  for (i in seq_len(n)) {
      name <- nms[i]
      i.dt <- match(name, nms.dt, nomatch = 0L)
      if (i.dt > 0L) {
          dimtype <- dimtypes[i.dt]
          dimtypes.new[i] <- dimtype
      }
      else
          dimtype <- dimtypes.new[i]
      i.ds <- match(name, nms.ds, nomatch = 0L)
      if (i.ds > 0L)
          dimscale <- dimscales[i.ds]
      else
          dimscale <- NULL
      labels <- dimnames[[i]]
      DimScales.new[[i]] <- inferDimScale(dimtype = dimtype,
                                          dimscale = dimscale,
                                          labels = labels,
                                          name = name)
  }
  methods::new("MetaData",
      nms = nms,
      dimtypes = dimtypes.new,
      DimScales = DimScales.new)
}
        
            
                 
    



