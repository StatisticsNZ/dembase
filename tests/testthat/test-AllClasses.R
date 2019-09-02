

context("AllClasses")

test_that("valid objects of class MetaData are created correctly", {
  expect_true(validObject(new("MetaData",
                              nms = c("region", "sex", "age"),
                              dimtypes = c("state", "sex", "age"),
                              DimScales = list(
                                new("Categories", dimvalues = c("Region 1", "Region 2")),
                                new("Sexes", dimvalues = c("Male", "Female")),
                                new("Intervals", dimvalues = c(0, 5, 10, Inf))))))
  expect_error(validObject(new("MetaData")), "must have at least 1 dimension")
})

## tests for function 'validNames' are in file "test-miscellaneous-functions.R"

test_that("dimtypes validity tests for MetaData work as expected", {
    expect_that(new("MetaData",
                    nms = c("region", "sex", "age"),
                    dimtypes = c(NA, "state", "age"),
                    DimScales = list(
                        new("Categories", dimvalues = c("Region 1", "Region 2")),
                        new("Categories", dimvalues = c("Male", "Female")),
                        new("Intervals", dimvalues = c(0, 5, 10, Inf)))),
                throws_error("'dimtypes' has missing values"))
    expect_that(new("MetaData",
                    nms = c("region", "sex", "age"),
                    dimtypes = c("wrong", "state", "age"),
                    DimScales = list(
                        new("Categories", dimvalues = c("Region 1", "Region 2")),
                        new("Categories", dimvalues = c("Male", "Female")),
                        new("Intervals", dimvalues = c(0, 5, 10, Inf)))),
                throws_error("\"wrong\" is not a valid dimtype"))
    expect_that(new("MetaData",
                    nms = c("region", "sex", "age"),
                    dimtypes = c("wrong1", "wrong2", "age"),
                    DimScales = list(
                        new("Categories", dimvalues = c("Region 1", "Region 2")),
                        new("Categories", dimvalues = c("Male", "Female")),
                        new("Intervals", dimvalues = c(0, 5, 10, Inf)))),
                throws_error("\"wrong1\" is not a valid dimtype"))
    expect_that(new("MetaData",
                    nms = c("age1", "sex", "age2"),
                    dimtypes = c("age", "state", "age"),
                    DimScales = list(
                        new("Intervals", dimvalues = c(0, 5, 10)),
                        new("Categories", dimvalues = c("Male", "Female")),
                        new("Intervals", dimvalues = c(0, 5, 10, Inf)))),
                throws_error("more than one dimension with dimtype \"age\""))
    expect_that(new("MetaData",
                    nms = c("region", "sex", "age"),
                    dimtypes = c(a = "state", b = "sex", c = "age"),
                    DimScales = list(
                        new("Categories", dimvalues = c("Region 1", "Region 2")),
                        new("Sexes", dimvalues = c("Male", "Female")),
                        new("Intervals", dimvalues = c(0, 5, 10, Inf)))),
                throws_error("'dimtypes' has names"))
})

test_that("DimScales validity tests for MetaData work as expected", {
  expect_that(new("MetaData",
                  nms = c("region", "sex", "age"),
                  dimtypes = c("state", "state", "age"),
                  DimScales = list(
                    "wrong",
                    new("Categories", dimvalues = c("Male", "Female")),
                    new("Intervals", dimvalues = c(0, 5, 10, Inf)))),
              throws_error("'DimScales' has element not of class \"DimScale\""))
  expect_that(new("MetaData",
                  nms = c("region", "sex", "age"),
                  dimtypes = c("state", "state", "age"),
                  DimScales = list(
                    a = new("Categories", dimvalues = c("Region 1", "Region 2")),
                    b = new("Categories", dimvalues = c("Male", "Female")),
                    c = new("Intervals", dimvalues = c(0, 5, 10, Inf)))),
              throws_error("'DimScales' has names"))
})

test_that("dimtypes and names have the same length", {
  expect_that(new("MetaData",
                  nms = c("region", "sex", "age"),
                  dimtypes = c("state", "state", "age", "state"),
                  DimScales = list(
                    new("Categories", dimvalues = c("Region 1", "Region 2")),
                    new("Categories", dimvalues = c("Male", "Female")),
                    new("Intervals", dimvalues = c(0, 5, 10, Inf)))),
              throws_error("'dimtypes' and 'names' have different lengths"))
})


test_that("DimScales and names have the same length", {
  expect_that(new("MetaData",
                              nms = c("region", "sex", "age"),
                              dimtypes = c("state", "state", "age"),
                              DimScales = list(
                                new("Categories", dimvalues = c("Region 1", "Region 2")),
                                new("Categories", dimvalues = c("Male", "Female")),
                                new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                                new("Intervals", dimvalues = c(0, 5, 10, Inf)))),
              throws_error("'DimScales' and 'names' have different lengths"))
})


test_that("dimensions have dimscales permitted for dimtypes", {
  expect_that(new("MetaData",
                  nms = c("region", "sex", "age"),
                  dimtypes = c("state", "state", "age"),
                  DimScales = list(
                    new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                    new("Categories", dimvalues = c("Male", "Female")),
                    new("Intervals", dimvalues = c(0, 5, 10, Inf)))),
              throws_error(paste("dimension \"region\" has dimtype \"state\"",
                                 "but dimscale \"Intervals\"")))
  expect_true(validObject(new("MetaData",
                              nms = c("region", "sex", "iteration"),
                              dimtypes = c("state", "state", "iteration"),
                              DimScales = list(
                                new("Categories", dimvalues = c("Region 1", "Region 2")),
                                new("Categories", dimvalues = c("Male", "Female")),
                                new("Iterations", dimvalues = 1:5)))))
  expect_that(new("MetaData",
                  nms = c("region", "sex", "iteration"),
                  dimtypes = c("state", "state", "iteration"),
                  DimScales = list(
                    new("Categories", dimvalues = c("Region 1", "Region 2")),
                    new("Categories", dimvalues = c("Male", "Female")),
                    new("Points", dimvalues = 1:5))),
              throws_error(paste("dimension \"iteration\" has dimtype \"iteration\"",
                                 "but dimscale \"Points\"")))
})


test_that("origin, destination, parent, and child dimensions work as expected", {
  expect_true(validObject(new("MetaData",
                              nms = c("region_orig", "region_dest"),
                              dimtypes = c("origin", "destination"),
                              DimScales = list(
                                new("Categories", dimvalues = c("reg1", "reg2", "reg3")),
                                new("Categories", dimvalues = c("reg1", "reg2", "reg3"))))))
  expect_that(new("MetaData",
                  nms = c("region_orig", "region_dest"),
                  dimtypes = c("state", "destination"),
                  DimScales = list(
                    new("Categories", dimvalues = c("reg1", "reg2", "reg3")),
                    new("Categories", dimvalues = c("reg1", "reg2", "reg3")))),
              throws_error(paste("dimension \"region_orig\" has suffix \"_orig\"",
                                 "but not dimtype \"origin\"")))
  expect_that(new("MetaData",
                  nms = c("region1", "region2"),
                  dimtypes = c("origin", "origin"),
                  DimScales = list(
                    new("Categories", dimvalues = c("reg1", "reg2", "reg3")),
                    new("Categories", dimvalues = c("reg1", "reg2", "reg3")))),
              throws_error(paste("dimension \"region1\" has dimtype \"origin\"",
                                 "but not suffix \"_orig\"")))
  expect_true(validObject(new("MetaData",
                              nms = c("ethnicity_parent", "ethnicity_child"),
                              dimtypes = c("parent", "child"),
                              DimScales = list(
                                new("Categories", dimvalues = c("eth1", "eth2", "eth3")),
                                new("Categories", dimvalues = c("eth1", "eth2", "eth3"))))))
  expect_that(new("MetaData",
                  nms = c("ethnicity_parent", "ethnicity_wrong"),
                  dimtypes = c("parent", "state"),
                  DimScales = list(
                    new("Categories", dimvalues = c("eth1", "eth2", "eth3")),
                    new("Categories", dimvalues = c("eth1", "eth2", "eth3")))),
              throws_error("dimension \"ethnicity_parent\" lacks pair"))
  expect_that(new("MetaData",
                  nms = c("ethnicity_parent", "ethnicity_child", "ethnicity"),
                  dimtypes = c("parent", "child", "state"),
                  DimScales = list(
                    new("Categories", dimvalues = c("eth1", "eth2", "eth3")),
                    new("Categories", dimvalues = c("eth1", "eth2", "eth3")),
                    new("Categories", dimvalues = c("eth1", "eth2", "eth3")))),
              throws_error(paste("dimension named \"ethnicity_parent\" and",
                                 "dimension named \"ethnicity\"")))
})

test_that("iteration and quantile dimensions work as expected", {
  expect_true(validObject(new("MetaData",
                              nms = c("sex", "iteration"),
                              dimtypes = c("state", "iteration"),
                              DimScales = list(
                                new("Categories", dimvalues = c("Male", "Female")),
                                new("Iterations", dimvalues = 1:3)))))
  expect_true(validObject(new("MetaData",
                              nms = c("sex", "quantile"),
                              dimtypes = c("state", "quantile"),
                              DimScales = list(
                                new("Categories", dimvalues = c("Male", "Female")),
                                new("Quantiles", dimvalues = c(0.025, 0.5, 0.975))))))
  expect_that(new("MetaData",
                  nms = c("quantile1", "quantile2"),
                  dimtypes = c("quantile", "quantile"),
                  DimScales = list(
                    new("Quantiles", dimvalues = c(0.025, 0.5, 0.975)),
                    new("Quantiles", dimvalues = c(0.025, 0.5, 0.975)))),
              throws_error("more than one dimension with dimtype \"quantile\""))
  expect_that(new("MetaData",
                  nms = c("iteration", "quantile"),
                  dimtypes = c("iteration", "quantile"),
                  DimScales = list(
                    new("Iterations", dimvalues = 1:2),
                    new("Quantiles", dimvalues = c(0.025, 0.5, 0.975)))),
              throws_error("has dimtype \"iteration\" and dimtype \"quantile\""))
})


test_that("triangle dimension works as expected", {
  expect_true(validObject(new("MetaData",
                              nms = c("age", "time", "triangle"),
                              dimtypes = c("age", "time", "triangle"),
                              DimScales = list(
                                  new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                                  new("Intervals", dimvalues = c(0, 5, 10)),
                                  new("Triangles", dimvalues = c("Upper", "Lower"))))))
  ## expect_error(new("MetaData",
  ##                 nms = c("sex", "triangle"),
  ##                  dimtypes = c("state", "triangle"),
  ##                  DimScales = list(
  ##                      new("Categories", dimvalues = c("Male", "Female")),
  ##                      new("Triangles", dimvalues = c("Upper", "Lower")))),
  ##              "has dimtype \"triangle\" but does not have two dimensions with dimtype \"age\", \"time\", \"cohort\"")
  ## expect_error(new("MetaData",
  ##                  nms = c("age", "triangle"),
  ##                  dimtypes = c("age", "triangle"),
  ##                  DimScales = list(
  ##                      new("Points", dimvalues = 1:2),
  ##                      new("Triangles", dimvalues = c("Upper", "Lower")))),
  ##              "has dimtype \"triangle\" but does not have two dimensions with dimtype \"age\", \"time\", \"cohort\"")
  expect_error(new("MetaData",
                  nms = c("age", "time", "triangle"),
                  dimtypes = c("age", "time", "triangle"),
                  DimScales = list(
                    new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                    new("Points", dimvalues = c(2000, 2005, 2010)),
                    new("Triangles", dimvalues = c("Upper", "Lower")))),
               "has dimension with dimtype \"triangle\" but dimension with dimtype \"time\" has dimscale \"Points\"")
  expect_error(new("MetaData",
                   nms = c("age", "time", "triangle"),
                   dimtypes = c("age", "time", "triangle"),
                   DimScales = list(
                    new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                    new("Intervals", dimvalues = 2000:2005),
                    new("Triangles", dimvalues = c("Upper", "Lower")))),
              "has dimension with dimtype \"triangle\" but does not have regular age-time plan")
})

test_that("validity tests for DemographicArray object work", {
  expect_true(validObject(new("Counts",
                              array(1:12,
                                    dim = c(2, 2, 3),
                                    dimnames = list(region = c("Region 1", "Region 2"),
                                      sex = c("Male", "Female"),
                                      age = c("0-4", "5-9", "10+"))),
                              metadata = new("MetaData",
                                nms = c("region", "sex", "age"),
                                dimtypes = c("state", "state", "age"),
                                DimScales = list(
                                  new("Categories", dimvalues = c("Region 1", "Region 2")),
                                  new("Categories", dimvalues = c("Male", "Female")),
                                  new("Intervals", dimvalues = c(0, 5, 10, Inf)))))))
  expect_that(new("Counts",
                  array("a",
                        dim = c(2, 2, 3),
                        dimnames = list(region = c("Region 1", "Region 2"),
                          sex = c("Male", "Female"),
                          age = c("0-4", "5-9", "10+"))),
                  metadata = new("MetaData",
                    nms = c("region", "sex", "age"),
                    dimtypes = c("state", "state", "age"),
                    DimScales = list(
                      new("Categories", dimvalues = c("Region 1", "Region 2")),
                      new("Categories", dimvalues = c("Male", "Female")),
                      new("Intervals", dimvalues = c(0, 5, 10, Inf))))),
              throws_error("does not have type \"numeric\""))
  expect_that(new("Counts",
                  array(1:12,
                        dim = c(2, 2, 3),
                        dimnames = list(region = c("Region 1", "Region 2"),
                          sex = c("Male", "Female"),
                          age = c("0-4", "5-9", "10+"))),
                  metadata = new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "state"),
                    DimScales = list(
                      new("Categories", dimvalues = c("Region 1", "Region 2")),
                      new("Categories", dimvalues = c("Male", "Female"))))),
              throws_error("'.Data' and 'metadata' have different dimensions"))
  expect_that(new("Counts",
                  array(1:12,
                        dim = c(2, 2, 3),
                        dimnames = list(region = c("Region 1", "Region 2"),
                          sex = c("Male", "Female"),
                          age = c("0-4", "5-9", "10+"))),
                  metadata = new("MetaData",
                    nms = c("region", "sex", "age"),
                    dimtypes = c("state", "state", "age"),
                    DimScales = list(
                      new("Categories", dimvalues = c("Region 1", "Region 2", "extra")),
                      new("Categories", dimvalues = c("Male", "Female")),
                      new("Intervals", dimvalues = c(0, 5, 10, Inf))))),
              throws_error("'.Data' and 'metadata' have different dimensions"))
  expect_that(new("Counts",
                  array(1:12,
                        dim = c(2, 2, 3),
                        dimnames = list(region = c("Region 1", "Region 2"),
                          sex = c("Male", "Female"),
                          age = c("0-4", "5-9", "10+"))),
                  metadata = new("MetaData",
                    nms = c("wrong", "sex", "age"),
                    dimtypes = c("state", "state", "age"),
                    DimScales = list(
                      new("Categories", dimvalues = c("Region 1", "Region 2")),
                      new("Categories", dimvalues = c("Male", "Female")),
                      new("Intervals", dimvalues = c(0, 5, 10, Inf))))),
              throws_error("'.Data' and 'metadata' have different dimnames"))
})

test_that("can create valid object of class CountsWithSubtotals", {
    metadata <- dembase:::metadata
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    ## whole row missing
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    subtotals <- Counts(array(5, dim = 1, dimnames = list(age = "0-4")))
    x[1 , ] <- NA
    transform <- makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE))
    x <- new("CountsWithSubtotals",
             x,
             subtotals = as.integer(subtotals),
             subtotalsNet = as.integer(subtotals),
             metadataSubtotals = metadata(subtotals),
             transformSubtotals = transform)
    expect_true(validObject(x))
    ## two values from first column missing; subtotals has metadata
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    subtotals <- Counts(array(5, dim = 1, dimnames = list(sex = "f")))
    x[1:2 , 1] <- NA
    transform <- makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE))
    x <- new("CountsWithSubtotals",
             x,
             subtotals = as.integer(subtotals),
             subtotalsNet = 2L,
             metadataSubtotals = metadata(subtotals),
             transformSubtotals = transform)
    expect_true(validObject(x))
    ## two values from first column missing; subtotals does not have metadata
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    x[1:2 , 1] <- NA
    subtotalsNet <- 4L
    subtotals <- subtotalsNet + sum(x, na.rm = TRUE) 
    transform <- makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = FALSE))
    x <- new("CountsWithSubtotals",
             x,
             subtotals = subtotals,
             subtotalsNet = subtotalsNet,
             metadataSubtotals = NULL,
             transformSubtotals = transform)
    expect_true(validObject(x))
    ## zero-length dimension
    x <- Counts(array(0L,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = character())))
    subtotals <- collapseDimension(x, dimension = "age")
    transform <- makeCollapseTransformExtra(makeTransform(x = x, y = subtotals))
    x <- new("CountsWithSubtotals",
             x,
             subtotals = as.integer(subtotals),
             subtotalsNet = integer(),
             metadataSubtotals = metadata(subtotals),
             transformSubtotals = transform)
    expect_true(validObject(x))
    ## all missing
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    subtotals <- sum(x)
    x[] <- NA
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 3), rep(1L, 2)),
                     dims = c(1L, 0L),
                     dimBefore = c(3L, 2L),
                     dimAfter = 1L)
    transform <- makeCollapseTransformExtra(transform)
    x <- new("CountsWithSubtotals",
             x,
             subtotals = subtotals,
             subtotalsNet = subtotals,
             transformSubtotals = transform)
    expect_true(validObject(x))
})

test_that("validity tests for CountsWithSubtotals inherited from HasSubtotals work", {
    metadata <- dembase:::metadata
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    subtotals <- Counts(array(5, dim = 1, dimnames = list(age = "0-4")))
    x[1 , ] <- NA
    transform <- makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE))
    x <- new("CountsWithSubtotals",
             x,
             subtotals = as.integer(subtotals),
             subtotalsNet = as.integer(subtotals),
             metadataSubtotals = metadata(subtotals),
             transformSubtotals = transform)
    ## object has no "iteration" or "quantile" dimensions
    x.wrong <- Counts(array(1:6,
                            dim = c(3, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"), iteration = 1:2)))
    expect_error(new("CountsWithSubtotals",
                     x.wrong,
                     subtotals = as.integer(subtotals),
                     subtotalsNet = 0L,
                     metadataSubtotals = metadata(subtotals),
                     transformSubtotals = transform),
                 "dimension with dimtype \"iteration\"")
    ## .Data has type "integer"
    x.wrong <- x
    x.wrong[3] <- 1.1
    expect_error(validObject(x.wrong),
                 ".Data' does not have type \"integer\"")
    ## all non-missing values in .Data are non-negative
    x.wrong <- x
    x.wrong[6] <- -1L
    expect_error(validObject(x.wrong),
                 ".Data' has negative values")
    ## subtotals has no missing values
    x.wrong <- x
    x.wrong@subtotals[1] <- NA
    expect_error(validObject(x.wrong),
                 "subtotals' has missing values")
    ## subtotals has no negative values
    x.wrong <- x
    x.wrong@subtotals[1] <- -1L
    x.wrong@subtotalsNet <- -17L
    expect_error(validObject(x.wrong))
    ## 'transform.subtotals.not one-to-one
    x.wrong <- Counts(array(1:6,
                            dim = c(3, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    subtotals <- Counts(array(1:3, dim = c(3, 1), dimnames = list(age = c("0-4", "5-9", "10+"), sex = "f")))
    x.wrong[ , 1] <- NA
    transform <- makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE))
    expect_error(new("CountsWithSubtotals",
                     x.wrong,
                     subtotals = as.integer(subtotals),
                     subtotalsNet = as.integer(subtotals),
                     metadataSubtotals = metadata(subtotals),
                     transformSubtotals = transform),
                 "'object' has one-to-one relationship with 'subtotals'")
    ## length of 'subtotals' consistent with dimensions of 'metadataSubtotals'
    x.wrong <- x
    x.wrong@metadataSubtotals <- new("MetaData",
                                     nms = "age",
                                     dimtypes = "age",
                                     DimScales = list(new("Intervals", dimvalues = c(0, 5, 10))))
    expect_error(validObject(x.wrong),
                 "length of 'subtotals' inconsistent with dimensions of 'metadataSubtotals'")
    ## 'dimBefore' consistent with dim(object)
    x.wrong <- x
    x.wrong@.Data <- x.wrong@.Data[1:2,]
    x.wrong@metadata <- metadata(x[1:2,])
    expect_error(validObject(x.wrong))
    ## 'dimAfter' consistent with dim(metadataSubtotals)
    x.wrong <- x
    x.wrong@metadataSubtotals <- new("MetaData",
                                     nms = "age",
                                     dimtypes = "age",
                                     DimScales = list(new("Intervals", dimvalues = c(0, 5, 10))))
    x.wrong@subtotals <- 1:2
    x.wrong@.Data[c(1,2,4,5)] <- NA
    expect_error(validObject(x.wrong))
    ## names for object and subtotals consistent
    x.wrong <- x
    x.wrong@metadataSubtotals@nms <- "wrong"
    expect_error(validObject(x.wrong),
                 "names for 'object' and 'subtotals' inconsistent")
})

test_that("validity tests for CountsWithSubtotals inherited from SubtotalsNet work", {
    metadata <- dembase:::metadata
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    subtotals <- Counts(array(10, dim = 1, dimnames = list(sex = "f")))
    x[1:2 , 1] <- NA
    transform <- makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE))
    x <- new("CountsWithSubtotals",
             x,
             subtotals = as.integer(subtotals),
             subtotalsNet = 7L,
             metadataSubtotals = metadata(subtotals),
             transformSubtotals = transform)
    ## 'subtotalsNet' has no missing values
    x.wrong <- x
    x.wrong@subtotalsNet <- NA_integer_
    expect_error(validObject(x.wrong),
                 "'subtotalsNet' has missing values")
    ## 'subtotalsNet' has no negative values
    x.wrong <- x
    x.wrong@subtotalsNet <- -1L
    expect_error(validObject(x.wrong),
                 "'subtotalsNet' has negative values")
    ## 'subtotalsNet' has same length as 'subtotals'
    x.wrong <- x
    x.wrong@subtotalsNet <- 1:2
    expect_error(validObject(x.wrong),
                 "'subtotalsNet' and 'subtotals' have different lengths")
    ## 'subtotalsNet' equals 'subtotals' minus known elements from '.Data'
    x.wrong <- x
    x.wrong@subtotalsNet <- 8L
    expect_error(validObject(x.wrong),
                 "'subtotalsNet', 'subtotals', and '.Data' inconsistent")
})

test_that("can create valid object of class CountsWithSubtotalsInternal", {
    metadata <- dembase:::metadata
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    subtotals <- Counts(array(5, dim = 1, dimnames = list(age = "0-4")))
    transform <- makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE))
    x <- new("CountsWithSubtotalsInternal",
             x,
             subtotals = as.integer(subtotals),
             metadataSubtotals = metadata(subtotals),
             transformSubtotals = transform)
    expect_true(validObject(x))
    x <- Counts(array(0L,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = character())))
    subtotals <- collapseDimension(x, dimension = "age")
    transform <- makeCollapseTransformExtra(makeTransform(x = x, y = subtotals))
    x <- new("CountsWithSubtotalsInternal",
             x,
             subtotals = as.integer(subtotals),
             metadataSubtotals = metadata(subtotals),
             transformSubtotals = transform)
    expect_true(validObject(x))
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    subtotals <- sum(x)
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 3), rep(1L, 2)),
                     dims = c(1L, 0L),
                     dimBefore = c(3L, 2L),
                     dimAfter = 1L)
    transform <- makeCollapseTransformExtra(transform)
    x <- new("CountsWithSubtotalsInternal",
             x,
             subtotals = subtotals,
             transformSubtotals = transform)
    expect_true(validObject(x))
})

test_that("can create valid object of class CountsWithSubtotalsInternal", {
    metadata <- dembase:::metadata
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    subtotals <- Counts(array(5, dim = 1, dimnames = list(age = "0-4")))
    transform <- makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE))
    x <- new("CountsWithSubtotalsInternal",
             x,
             subtotals = as.integer(subtotals),
             metadataSubtotals = metadata(subtotals),
             transformSubtotals = transform)
    expect_true(validObject(x))
    x@subtotals[1] <- x@subtotals[1] + 1L
    expect_error(validObject(x),
                 "'.Data' and 'subtotals' inconsistent")
})


## CONCORDANCES ############################################################

test_that("can create valid object of class ManyToOne", {
    x <- new("ManyToOne",
             values = cbind(c("a", "b", "c"), c("x", "y", "x")),
             classifications = c("c1", "c2"))
    expect_true(validObject(x))
    x <- new("ManyToOne",
             values = matrix(character(), nrow = 0, ncol = 2),
             classifications = c("c1", "c2"))
    expect_true(validObject(x))
})

test_that("validity tests inherited from Concordance work", {
    x <- new("ManyToOne",
             values = cbind(c("a", "b", "c"), c("x", "y", "x")),
             classifications = c("c1", "c2"))
    ## 'values' has two columns
    x.wrong <- x
    x.wrong@values <- cbind(x.wrong@values, c("e", "f", "g"))
    expect_error(validObject(x.wrong),
                 "'values' does not have 2 columns")
    ## 'values' is character
    x.wrong <- x
    x.wrong@values <- matrix(1:6, nrow = 3)
    expect_error(validObject(x.wrong),
                 "'values' does not have type \"character\"")
    ## 'values' does not have missing values
    x.wrong <- x
    x.wrong@values[1] <- NA
    expect_error(validObject(x.wrong),
                 "'values' has missing values")
    ## 'values' does not have dimnames
    x.wrong <- x
    colnames(x.wrong@values) <- c("a", "b")
    expect_error(validObject(x.wrong),
                 "'values' has dimnames")
    ## every row of 'values' is unique
    x.wrong <- x
    x.wrong@values[3,] <- c("a", "x")
    expect_error(validObject(x.wrong),
                 "'values' has duplicate rows")
    ## 'classifications' has length 2
    x.wrong <- x
    x.wrong@classifications <- c("c1", "c2", "wrong")
    expect_error(validObject(x.wrong),
                 "'classifications' does not have length 2")
    ## 'classifications' does not have missing values
    x.wrong <- x
    x.wrong@classifications[1] <- NA
    expect_error(validObject(x.wrong),
                 "'classifications' has missing values")
    ## 'classifications' does not have blanks
    x.wrong <- x
    x.wrong@classifications[1] <- ""
    expect_error(validObject(x.wrong),
                 "'classifications' has blanks")
    ## 'classifications' does not have duplicates
    x.wrong <- x
    x.wrong@classifications[2] <- "c1"
    expect_error(validObject(x.wrong),
                 "'classifications' has duplicates \\[\"c1\"\\]")
})

test_that("validity tests inherited from ManyToOne work", {
    x <- new("ManyToOne",
             values = cbind(c("a", "b", "c"), c("x", "y", "x")),
             classifications = c("c1", "c2"))
    ## labels.from has no duplicates
    x.wrong <- x
    x.wrong@values[1] <- "b"
    expect_error(validObject(x.wrong),
                 "'from' classification \\[\"c1\"\\] has duplicates")
    ## labels.from has no duplicates
    x.wrong <- x
    x.wrong@values[3,2] <- "z"
    expect_error(validObject(x.wrong),
                 "'to' classification \\[\"c2\"\\] has no duplicates")
})

test_that("can create valid object of class OneToOne", {
    x <- new("OneToOne",
             values = cbind(c("a", "b", "c"), c("x", "y", "z")),
             classifications = c("c1", "c2"))
    expect_true(validObject(x))
    x <- new("OneToOne",
             values = matrix(character(), nrow = 0, ncol = 2),
             classifications = c("c1", "c2"))
    expect_true(validObject(x))
})

test_that("validity tests inherited from OneToOne work", {
    x <- new("OneToOne",
             values = cbind(c("a", "b", "c"), c("x", "y", "z")),
             classifications = c("c1", "c2"))
    ## neither column has duplicates
    x.wrong <- x
    x.wrong@values[1,1] <- "b"
    expect_error(validObject(x.wrong),
                 "classification \"c1\" has duplicates")
    x.wrong <- x
    x.wrong@values[1,2] <- "y"
    expect_error(validObject(x.wrong),
                 "classification \"c2\" has duplicates")
})



## DIMSCALES ###############################################################

test_that("class Categories works", {
  expect_true(validObject(new("Categories", dimvalues = c("a", "b"))))
  expect_error(new("Categories", dimvalues = c(NA, "b")))
  expect_error(new("Categories", dimvalues = c("", "b")),
               "values with length 0")
  expect_error(new("Categories", dimvalues = c("a", "a")),
              "duplicated values")
  expect_true(validObject(new("Categories")))
})

test_that("class Sexes works", {
  expect_true(validObject(new("Sexes", dimvalues = c("Females", "Males"))))
  expect_true(validObject(new("Sexes", dimvalues = c("males", "females"))))
  expect_true(validObject(new("Sexes", dimvalues = c("Male", "Female"))))
  expect_error(new("Sexes", dimvalues = c(NA, "Female")), "missing values")
  expect_error(new("Sexes", dimvalues = c("male", "females")), "invalid values")
  expect_error(new("Sexes", dimvalues = c("Male", "Female", "Female")), "duplicated values")
  expect_error(new("Sexes", dimvalues = c("Male", "Wrong")), "invalid values")
})

test_that("class Triangles works", {
  expect_true(validObject(new("Triangles", dimvalues = c("Lower", "Upper"))))
  expect_true(validObject(new("Triangles", dimvalues = c("Lower", "Upper"))))
  expect_true(validObject(new("Triangles", dimvalues = "Upper")))
  expect_error(new("Triangles", dimvalues = c(NA, "Upper")), "missing values")
  expect_error(new("Triangles", dimvalues = c("TU", "Lower")), "invalid values")
  expect_error(new("Triangles", dimvalues = c("Upper", "Lower", "Lower")), "duplicated values")
  expect_true(validObject(new("Triangles")))
  expect_error(new("Triangles", dimvalues = c("a", "b")), "invalid values")
})

test_that("class Points works", {
    expect_true(validObject(new("Points", dimvalues = 1:2)))
    expect_error(new("Points", dimvalues = c(NA, 2)),
                 "missing values")
    expect_error(new("Points", dimvalues = c(1, 1)),
                 "values not strictly increasing")
    expect_true(validObject(new("Points")))
    expect_error(new("Points", dimvalues = c(1, Inf)),
                 "non-finite values")
})

test_that("class Quantiles works", {
    expect_true(validObject(new("Quantiles", dimvalues = c(0.025, 0.5, 0.975))))
    expect_true(validObject(new("Quantiles", dimvalues = c(0.025, 0.05, 0.95, 0.975))))
    expect_error(new("Quantiles", dimvalues = c(0.025, NA, 0.975)),
                 "missing values")
    expect_error(new("Quantiles", dimvalues = c(0.5, 0.25, 0.75)),
                 "values not strictly increasing")
    expect_error(new("Quantiles", dimvalues = c(-0.1, 0.5, 0.95)),
                 "values less than 0")
    expect_error(new("Quantiles", dimvalues = c(0, 0.5, 1.01)),
                 "values greater than 1")
    expect_true(validObject(new("Quantiles")))
})

test_that("class Intervals works", {
    expect_true(validObject(new("Intervals", dimvalues = c(-Inf, 1:2, Inf))))
    expect_error(new("Intervals", dimvalues = c(NA, 2)),
                 "missing values")
    expect_error(new("Intervals", dimvalues = 1),
                 "must have 0 or at least 2 values")
    expect_error(new("Intervals", dimvalues = c(1, 1)),
                 "values not strictly increasing")
    expect_error(new("Intervals", dimvalues = c(-Inf, Inf)),
                 "no finite values")
    expect_true(validObject(new("Intervals")))
})

test_that("class Iterations works", {
  expect_true(validObject(new("Iterations", dimvalues = 1:2)))
  expect_error(new("Iterations", dimvalues = c(NA, 2L)),
              "missing values")
  expect_error(new("Iterations", dimvalues = c(1L, 1L)),
              "duplicated values")
  expect_error(new("Iterations", dimvalues = 0L),
              "values less than or equal to 0")
  expect_true(validObject(new("Iterations")))
})




## MISCELLANEOUS ###############################################################

test_that("valid objects of class CollapseTransform pass validity tests", {
    expect_true(validObject(new("CollapseTransform")))
    expect_true(validObject(new("CollapseTransform",
                                indices = list(1:4, 1:3),
                                dims = 1:2,
                                dimBefore = 4:3,
                                dimAfter = 4:3)))
    expect_true(validObject(new("CollapseTransform",
                                indices = list(c(0L, 1L), c(1L, 2L)),
                                dims = c(2L, 1L),
                                dimBefore = c(2L, 2L),
                                dimAfter = c(2:1))))
  })

test_that("invalid objects of class CollapseTransform throw appropriate errors", {
  expect_that(new("CollapseTransform",
                  indices = list(c(0, 1, 2, 3), 1:3),
                  dims = 1:2,
                  dimBefore = 4:3,
                  dimAfter = c(3L, 3L)),
              throws_error("element 1 of 'indices' is not of type \"integer\""))
  expect_that(new("CollapseTransform",
                  indices = list(c(NA, 1L, 2L, 3L), 1:3),
                  dims = 1:2,
                  dimBefore = 4:3,
                  dimAfter = 4:3),
              throws_error("element 1 of 'indices' has missing values"))
  expect_that(new("CollapseTransform",
                  indices = list(c(-1L, 1L, 2L, 3L), 1:3),
                  dims = 1:2,
                  dimBefore = 4:3,
                  dimAfter = c(3L, 3L)),
              throws_error("element 1 of 'indices' has negative values"))
  expect_that(new("CollapseTransform",
                  indices = list(1:4, 1:3),
                  dims = c(NA, 1L),
                  dimBefore = 4:3,
                  dimAfter = 3:4),
              throws_error("'dims' has missing values"))
  expect_that(new("CollapseTransform",
                  indices = list(1:4, 1:3),
                  dims = c(-1L, 1L),
                  dimBefore = 4:3,
                  dimAfter = 3L),
              throws_error("'dims' has negative values"))
  expect_that(new("CollapseTransform",
                  indices = list(1:4, 1:3),
                  dims = c(1L, 3L),
                  dimBefore = 4:3,
                  dimAfter = 4:3),
              throws_error("'dims' has gaps"))
  expect_that(new("CollapseTransform",
                  indices = list(1:4, 1:3),
                  dims = c(1L, 1L),
                  dimBefore = 4:3,
                  dimAfter = c(4L, 4L)),
              throws_error("'dims' refers more than once to the same dimension"))
  expect_that(new("CollapseTransform",
                  indices = list(1:4, 1:3, 1:3, 1:3),
                  dims = c(0L, 0L, 2L, 3L),
                  dimBefore = c(4L, 3L, 3L, 3L),
                  dimAfter = c(3L, 3L)),
              throws_error("'dims' has gaps"))
  expect_that(new("CollapseTransform",
                  indices = list(1:4, 1:3),
                  dims = 1:2,
                  dimBefore = c(NA, 3L),
                  dimAfter = 4:3),
              throws_error("'dimBefore' has missing values"))
  expect_that(new("CollapseTransform",
                  indices = list(1:4, 1:3),
                  dims = 1:2,
                  dimBefore = c(4L, 3L),
                  dimAfter = c(4L, NA)),
              throws_error("'dimAfter' has missing values"))
  expect_that(new("CollapseTransform",
                  indices = list(1:4, 1:3, 1:2),
                  dims = c(1L, 2L),
                  dimBefore = 4:2,
                  dimAfter = 4:3),
              throws_error("'indices' and 'dims' have different lengths"))
  expect_that(new("CollapseTransform",
                  indices = list(1:4, 1:3),
                  dims = 1:2,
                  dimBefore = c(4L, 2L),
                  dimAfter = 4:3),
              throws_error("'indices' and 'dimBefore' inconsistent"))
  expect_that(new("CollapseTransform",
                  indices = list(0:3, 1:3),
                  dims = 1:2,
                  dimBefore = 4:3,
                  dimAfter = 4:3),
              throws_error("'indices' and 'dimAfter' inconsistent"))
  expect_that(new("CollapseTransform",
                  indices = list(1:4, 1:3),
                  dims = c(1L, 0L),
                  dimBefore = 4:3,
                  dimAfter = 4L),
              throws_error(paste("if dimension 2 is to be dropped then element 2 of",
                                 "'indices' must consist of 0s and at least one 1")))
  expect_that(new("CollapseTransform",
                  dims = c(1L, 0L),
                  indices = list(1:5, integer()),
                  dimBefore = c(5L, 0L),
                  dimAfter = 5L),
              throws_error("dimension 2 has length 0 and cannot be dropped"))
})


test_that("can create valid object of class CollapseTransformExtra", {
    x <- new("CollapseTransformExtra",
             indices = list(1:4, 1:3),
             dims = 2:1,
             dimBefore = c(4L, 3L),
             dimAfter = c(3L, 4L),
             multiplierBefore = c(1L, 4L),
             multiplierAfter = c(1L, 3L),
             invIndices = list(list(1L, 2L, 3L, 4L), list(1L, 2L, 3L)))
    expect_true(validObject(x))
})

test_that("validity tests for CollapseTransformExtra inherited from CollapseTransformExtra work", {
    x <- new("CollapseTransformExtra",
             indices = list(1:4, 1:3),
             dims = 2:1,
             dimBefore = c(4L, 3L),
             dimAfter = c(3L, 4L),
             multiplierBefore = c(1L, 4L),
             multiplierAfter = c(1L, 3L),
             invIndices = list(list(1L, 2L, 3L, 4L), list(1L, 2L, 3L)))
    ## 'dimBefore' and 'multiplierBefore' consistent
    x.wrong <- x
    x.wrong@multiplierBefore <- c(1L, 5L)
    expect_error(validObject(x.wrong),
                 "'multiplierBefore' and 'dimBefore' inconsistent")
             ## 'dimAfter' and 'multiplierAfter' consistent
    x.wrong <- x
    x.wrong@multiplierAfter <- c(1L, 3L, 5L)
    expect_error(validObject(x.wrong),
                 "'multiplierAfter' and 'dimAfter' inconsistent")
    ## indices and invIndices the same length
    x.wrong <- x
    x.wrong@invIndices <- c(x.wrong@invIndices, list(1:2))
    expect_error(validObject(x.wrong),
                 "'indices' and 'invIndices' have different lengths")
    ## elements within 'invIndices' all lists
    x.wrong <- x
    x.wrong@invIndices[[1]] <- "wrong"
    expect_error(validObject(x.wrong),
                 "'invIndices' has elements not of class \"list\"")
    ## elements of 'invIndices' have expected length
    x.wrong <- x
    x.wrong@invIndices[[1L]] <- list(1L, 2L, 3L)
    expect_error(validObject(x.wrong),
                 "'invIndices' inconsistent with 'dims' and 'dimAfter'")
    ## 'invIndices' all integer
    x.wrong <- x
    x.wrong@invIndices[[1L]][[1]] <- 1.0
    expect_error(validObject(x.wrong),
                 "'invIndices' has elements not of type \"integer\"")
    ## 'invIndices' has no missing values
    x.wrong <- x
    x.wrong@invIndices[[1L]][[1]] <- NA
    expect_error(validObject(x.wrong),
                 "'invIndices' has missing values")
    ## elements of 'invIndices' are positive
    x.wrong <- x
    x.wrong@invIndices[[1L]][[1]] <- 0L
    expect_error(validObject(x.wrong),
                 "'invIndices' has non-positive values")
})

test_that("can create valid objects of class ExtendTransform", {
    expect_true(validObject(new("ExtendTransform")))
    expect_true(validObject(new("ExtendTransform",
                                indices = list(1:4, 1:3),
                                dims = 1:2,
                                dimBefore = 4:3,
                                dimAfter = 4:3)))
    expect_true(validObject(new("ExtendTransform",
                                indices = list(c(1L, 1L, 2L, 2L), c(1L, 2L)),
                                dims = c(2L, 1L),
                                dimBefore = c(2L, 2L),
                                dimAfter = c(4L, 2L))))
})

test_that("invalid objects of class ExtendTransform throw appropriate errors", {
    expect_error(new("ExtendTransform",
                     indices = list(c(1, 2, 3, 3), 1:3),
                     dims = 1:2,
                     dimBefore = c(3L, 3L),
                     dimAfter = c(4L, 3L)),
                 "element 1 of 'indices' is not of type \"integer\"")
    expect_error(new("ExtendTransform",
                     indices = list(c(NA, 1L, 2L, 3L), 1:3),
                     dims = 1:2,
                     dimBefore = c(3L, 3L),
                     dimAfter = 4:3),
                 "element 1 of 'indices' has missing values")
    expect_error(new("ExtendTransform",
                     indices = list(c(-1L, 1L, 2L, 3L), 1:3),
                     dims = 1:2,
                     dimBefore = c(3L, 3L),
                     dimAfter = c(4L, 3L)),
                 "element 1 of 'indices' has values outside the valid range")
    expect_error(new("ExtendTransform",
                     indices = list(1:4, 1:3),
                     dims = c(NA, 1L),
                     dimBefore = 4:3,
                     dimAfter = 3:4),
                 "'dims' has missing values")
    expect_error(new("ExtendTransform",
                     indices = list(1:4, 1:3),
                     dims = c(-1L, 1L),
                     dimBefore = 4:3,
                     dimAfter = 3L),
                 "'dims' has values outside the valid range")
    expect_error(new("ExtendTransform",
                     indices = list(1:4, 1:3),
                     dims = c(1L, 3L),
                     dimBefore = 4:3,
                     dimAfter = 4:3),
                 "'dims' has values outside the valid range")
    expect_error(new("ExtendTransform",
                     indices = list(1:4, rep(1L, 3)),
                     dims = c(1L, 0L),
                     dimBefore = 4:3,
                     dimAfter = 4:3),
                 "'dims' must refer exactly once to each dimension in 'dimBefore'")
    expect_error(new("ExtendTransform",
                     indices = list(1:4, 1:3),
                     dims = c(1L, 1L),
                     dimBefore = 4:3,
                     dimAfter = c(4L, 4L)),
                 "'dims' refers more than once to the same dimension")
    expect_error(new("ExtendTransform",
                     indices = list(1:4, 1:3),
                     dims = 1:2,
                     dimBefore = c(NA, 3L),
                     dimAfter = 4:3),
                 "'dimBefore' has missing values")
    expect_error(new("ExtendTransform",
                     indices = list(1:4, 1:3),
                     dims = 1:2,
                     dimBefore = c(4L, 3L),
                     dimAfter = c(4L, NA)),
                 "'dimAfter' has missing values")
    expect_error(new("ExtendTransform",
                     indices = list(1:4, 1:3, 1:2),
                     dims = c(1L, 2L),
                     dimBefore = 4:2,
                     dimAfter = 4:2),
                 "'indices' and 'dims' have different lengths")
    expect_error(new("ExtendTransform",
                     indices = list(1:4, 1:3),
                     dims = 1:2,
                     dimBefore = c(4L, 3L),
                     dimAfter = c(4L, 2L)),
                 "'indices' and 'dimAfter' inconsistent")
    expect_error(new("ExtendTransform",
                     indices = list(0:3, 1:3),
                     dims = 1:2,
                     dimBefore = 4:3,
                     dimAfter = 4:3),
                 "element 1 of 'indices' has values outside the valid range")
    expect_error(new("ExtendTransform",
                     indices = list(1:4, 1:3, 1:2),
                     dims = c(1L, 2L, 0L),
                     dimBefore = 4:3,
                     dimAfter = 4:2),
                 paste("if a new dimension is to be added at position 3 then element 3 of",
                       "'indices' must consist entirely of 1s"))
})

test_that("SubArrayIndices works", {
    expect_true(validObject(new("SubArrayIndices",
                                nms = "age",
                                indices = list(c(TRUE, TRUE, FALSE)))))
    expect_true(validObject(new("SubArrayIndices",
                                nms = c("age", "sex"),
                                indices = list(c(TRUE, TRUE, FALSE),
                                c(TRUE, TRUE)))))
    expect_error(new("SubArrayIndices",
                     nms = c(NA, "sex"),
                     indices = list(c(TRUE, TRUE, FALSE),
                     c(TRUE, TRUE))),
                 "'names' has missing values")
    expect_error(new("SubArrayIndices",
                     nms = c("age", "sex"),
                     indices = list(c(1, 1, 0),
                     c(TRUE, TRUE))),
                 "'indices' has element not of type \"logical\"")
    expect_error(new("SubArrayIndices",
                     nms = c("age", "sex"),
                     indices = list(c(TRUE, NA, TRUE),
                     c(TRUE, TRUE))),
                 "'indices' has missing values")
    expect_error(new("SubArrayIndices",
                     nms = "sex",
                     indices = list(c(TRUE, TRUE, FALSE),
                     c(TRUE, TRUE))),
                 "'nms' and 'indices' have different lengths")
})



## SUMMARY ############################################################################

test_that("SummaryDemographicArray works", {
    a <- array(1:4, dim = c(2, 2))
    m <- new("MetaData",
             nms = c("age", "sex"),
             dimtypes = c("age", "state"),
             DimScales = list(new("Intervals", dimvalues = c(0, 5, Inf)),
             new("Categories", dimvalues = c("f", "m"))))
    x <- new("SummaryDemographicArray",
             metadata = m,
             stats = summary(a))
    expect_true(validObject(x))
})



## DEMOGRAPHIC ACCOUNT ############################################################

test_that("can create valid object of class Pool", {
    x <- Counts(array(1.0,
                      dim = c(2, 2, 1, 1),
                      dimnames = list(reg = c("a", "b"),
                          direction = c("Out", "In"),
                          age = "20-24",
                          time = "2001-2005")))
    x <- new("Pool", .Data = x@.Data, metadata = x@metadata,
             iBetween = 1L, iDirection = 2L)
    expect_true(validObject(x))
})

test_that("can create valid object of class Net", {
    x <- Counts(array(c(-2, 2),
                      dim = c(2, 1, 1),
                      dimnames = list(reg = c("a", "b"),
                          age = "20-24",
                          time = "2001-2005")))
    x <- new("Net", .Data = x@.Data, metadata = x@metadata,
             iBetween = 1L)
    expect_true(validObject(x))
})

test_that("can create valid object of class Population", {
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c(0,1,"2+"),
                          sex = c("f", "m"),
                          time = 2000:2002)),
                dimscales = c(time = "Points"))
    x <- new("Population", x)
    expect_true(validObject(x))
    expect_is(x, "Population")
    x <- Counts(array(1:18,
                      dim = c(3, 5, 3),
                      dimnames = list(age = c("0-9","10-19","20+"),
                          iteration = 1:5,
                          time = c(2000, 2010, 2020))))
    x <- new("Population", x)
    expect_true(validObject(x))
    expect_is(x, "Population")
    x <- Counts(array(1:5, dim = 5, dimnames = list(time = c(0, 5, 10, 15, 20))))
    x <- new("Population", x)
    expect_true(validObject(x))
    expect_is(x, "Population")
})

test_that("validity tests for Population inherited from IsInteger work", {
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c(0,1,"2+"),
                          sex = c("f", "m"),
                          time = 2000:2002)),
                dimscales = c(time = "Points"))
    x <- new("Population", x)
    x.wrong <- x
    x.wrong@.Data <- array(as.double(x.wrong@.Data),
                           dim = dim(x.wrong@.Data),
                           dimnames = dimnames(x.wrong@.Data))
    expect_error(validObject(x.wrong),
                 "does not have type \"integer\"")
})

test_that("validity tests for Population inherited from NonNegative work", {
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c(0,1,"2+"),
                          sex = c("f", "m"),
                          time = 2000:2002)),
                dimscales = c(time = "Points"))
    x <- new("Population", x)
    x.wrong <- x
    x.wrong[1] <- -1L
    expect_error(validObject(x.wrong),
                 "has negative values")
})

test_that("validity tests for Population inherited from HasTime work", {
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c(0,1,"2+"),
                          sex = c("f", "m"),
                          iteration = 1:3)))
    expect_error(new("Population", x),
                 "no dimension with dimtype \"time\"")
})

test_that("validity tests for Population inherited from AgeIsIntervals work", {
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c(0, 5, 10),
                          sex = c("f", "m"),
                          time = c(2000, 2005, 2010))))
    expect_error(new("Population", x),
                 "dimension with dimtype \"age\" has dimscale \"Points\"")
})

test_that("validity tests for Population inherited from AtLeastTwoAge work", {
    x <- Counts(array(1:6,
                      dim = c(1, 2, 3),
                      dimnames = list(age = "0+",
                          sex = c("f", "m"),
                          time = 2000:2002)),
                dimscale = c(time = "Points"))
    expect_error(new("Population", x),
                 "dimension with dimtype \"age\" has length 1")
})

test_that("validity tests for Population inherited from AtMostOneSex work", {
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(sex = c("f", "m"),
                                      gender = c("f", "m"))))
    expect_error(new("Population", x),
                 "2 dimensions with dimtype \"sex\"")
})

test_that("validity tests for Population inherited from FirstAgeIntervalClosed work", {
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c("<5", "5-9", "10+"),
                          sex = c("f", "m"),
                          time = c(2000, 2005, 2010))))
    expect_error(new("Population", x),
                 "first interval of dimension with dimtype \"age\" is open")
})

test_that("validity tests for Population inherited from LastAgeIntervalOpen work", {
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c("0-4", "5-9", "10-14"),
                          sex = c("f", "m"),
                          time = c(2000, 2005, 2010))))
    expect_error(new("Population", x),
                 "last interval of dimension with dimtype \"age\" is closed")
})

test_that("validity tests for Population inherited from isRegular work", {
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c(0,1,"2+"),
                          sex = c("f", "m"),
                          time = c(0, 1, 3))))
    expect_error(new("Population", x),
                 "does not have regular age-time plan")
})

test_that("validity tests for Population inherited from NoCohort work", {
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(cohort = c(0,1,2),
                          sex = c("f", "m"),
                          time = c(0, 1, 2))),
                dimscales = c(time = "Points"))
    expect_error(new("Population", x),
                 "has dimension with dimtype \"cohort\"")
})

test_that("validity tests for Population inherited from NoTriangle work", {
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c(0,1,"2+"),
                          triangle = c("Lower", "Upper"),
                          time = c(0, 1, 2))),
                dimscales = c(time = "Intervals"))
    expect_error(new("Population", x))
})

test_that("validity tests for Population inherited from NoOrigDest work", {
    x <- Counts(array(1:12,
                      dim = c(3, 2, 2),
                      dimnames = list(time = 0:2,
                          reg_orig = c("a", "b"),
                          reg_dest = c("a", "b"))),
                dimscales = c(time = "Points"))
    expect_error(new("Population", x),
                 "has dimension with dimtype \"origin\"")
})

test_that("validity tests for Population inherited from NoParentChild work", {
    x <- Counts(array(1:12,
                      dim = c(3, 2, 2),
                      dimnames = list(time = 0:2,
                          reg_parent = c("a", "b"),
                          reg_child = c("a", "b"))),
                dimscales = c(time = "Points"))
    expect_error(new("Population", x),
                 "has dimension with dimtype \"parent\"")
})

test_that("validity tests for Population inherited from TimeIsPoints work", {
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c(0,1,"2+"),
                          sex = c("f", "m"),
                          time = c("2001", "2002", "2003"))),
                dimscales = c(time = "Intervals"))
    expect_error(new("Population", x),
                 "dimension with dimtype \"time\" has dimscale \"Intervals\"")
})


test_that("validity tests for Population inherited from Population work", {
    ## time dimension has dimscale "Points"
    x <- Counts(array(1L,
                      dim = c(2, 2, 2),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          reg = c("a", "b"),
                          age = c("0-4", "5+"))))
    expect_error(new("Population", .Data = x@.Data, metadata = x@metadata),
                 "dimension with dimtype \"time\" has dimscale \"Intervals\"")
    ## "time" dimension has at least 2 points
    x <- Counts(array(1:6,
                      dim = c(1, 2, 2),
                      dimnames = list(time = "2000.5",
                          reg = c("a", "b"),
                          age = c("0-4", "5+"))))
    expect_error(new("Population", .Data = x@.Data, metadata = x@metadata),
                 "dimension with dimtype \"time\" has length 1")    
})

test_that("can create valid object of class BirthsMovementsNoParentChild", {
    x <- Counts(array(1L,
                      dim = c(2, 2, 2),
                      dimnames = list(age = c("20-24", "25-29"),
                          time = c("2001-2005", "2006-2010"),
                          triangle = c("Lower", "Upper"))))
    x <- new("BirthsMovementsNoParentChild",
             .Data = x@.Data,
             metadata = x@metadata,
             iMinAge = 5L)
    expect_true(validObject(x))
    expect_is(x, "BirthsMovements")
    x <- Counts(array(1L,
                      dim = 2L,
                      dimnames = list(time = c("2001-2005", "2006-2010"))))
    x <- new("BirthsMovementsNoParentChild",
             .Data = x@.Data,
             metadata = x@metadata,
             iMinAge = NA_integer_)
    expect_true(validObject(x))
    expect_is(x, "BirthsMovements")
})

test_that("validity tests for BirthsMovementsNoParentChild inherited from NoParentChild work", {
    x <- Counts(array(1L,
                      dim = c(2, 2, 1, 2, 2, 1, 2),
                      dimnames = list(reg_parent = c("a", "b"),
                          reg_child = c("a", "b"),
                          age = "20-24",
                          eth_parent = c("A", "B"),
                          eth_child = c("A", "B"),
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    expect_error(new("BirthsMovementsNoParentChild",
                     .Data = x@.Data,
                     metadata = x@metadata,
                     iMinAge = 5L),
                 "has dimension with dimtype \"parent\"")
})

test_that("validity tests for BirthsMovementsNoParentChild inherited from iMinAge work", {
    x <- Counts(array(1L,
                      dim = c(2, 1, 2, 1, 2),
                      dimnames = list(reg = c("a", "b"),
                          age = "20-24",
                          eth = c("A", "B"),
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    x <- new("BirthsMovementsNoParentChild",
             .Data = x@.Data,
             metadata = x@metadata,
             iMinAge = 5L)
    ## if has.age: 'iMinAge' is not missing
    x.wrong <- x
    x.wrong@iMinAge <- NA_integer_
    expect_error(validObject(x.wrong),
                 "'iMinAge' is missing")
    ## if has.age: 'minAge' positive
    x.wrong <- x
    x.wrong@iMinAge <- 0L
    expect_error(validObject(x.wrong),
                 "'iMinAge' is less than 2")
    x <- Counts(array(1L,
                      dim = 2L,
                      dimnames = list(time = c("2001-2005", "2006-2010"))))
    x <- new("BirthsMovementsNoParentChild",
             .Data = x@.Data,
             metadata = x@metadata,
             iMinAge = NA_integer_)
    ## if !has.age: 'minAge' missing
    x.wrong <- x
    x.wrong@iMinAge <- 1L
    expect_error(validObject(x.wrong),
                 "no dimension with dimtype \"age\" but 'iMinAge' is not missing")
})

test_that("validity tests for BirthsMovementsNoParentChild inherited from TimeIsIntervals work", {
    x <- Counts(array(1L,
                      dim = c(2, 2, 1),
                      dimnames = list(reg = c("a", "b"),
                          eth = c("A", "B"),
                          time = "2001")),
                dimscales = c(time = "Points"))
    expect_error(new("BirthsMovementsNoParentChild",
                     .Data = x@.Data,
                     metadata = x@metadata,
                     iMinAge = NA_integer_),
                 "dimension with dimtype \"time\" has dimscale \"Points\"")
})

test_that("can create valid object of class BirthsMovementsHasParentChild", {
    x <- Counts(array(1L,
                      dim = c(2, 2, 1, 2, 2, 1, 2),
                      dimnames = list(reg_parent = c("a", "b"),
                          reg_child = c("a", "b"),
                          age = "20-24",
                          eth_parent = c("A", "B"),
                          eth_child = c("A", "B"),
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    x <- new("BirthsMovementsHasParentChild", .Data = x@.Data, metadata = x@metadata,
             iMinAge = 5L)
    expect_true(validObject(x))
    expect_is(x, "BirthsMovements")
})

test_that("validity tests for BirthsMovementsHasParentChild inherited from HasParentChild work", {
    x <- Counts(array(1L,
                      dim = c(2, 2, 2),
                      dimnames = list(age = c("20-24", "25-29"),
                          time = c("2001-2005", "2006-2010"),
                          triangle = c("Lower", "Upper"))))
    expect_error(new("BirthsMovementsHasParentChild",
                     .Data = x@.Data,
                     metadata = x@metadata,
                     iMinAge = 5L),
                 "no dimensions with dimtype \"parent\" or \"child\"")
    x <- Counts(array(1L,
                      dim = c(2, 2, 1, 2, 2, 1, 2),
                      dimnames = list(reg_parent = c("a", "b"),
                          reg_child = c("b", "a"),
                          age = "20-24",
                          eth_parent = c("A", "B"),
                          eth_child = c("A", "B"),
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    expect_error(new("BirthsMovementsHasParentChild", .Data = x@.Data, metadata = x@metadata,
                     iMinAge = 5L),
                 "dimensions \"reg_parent\" and \"reg_child\" use different categories")
})

test_that("validity tests for BirthsMovementsHasParentChild inherited from HasTriangle work", {
    ## has dimtype "triangle" iff has dimtype "age"
    x <- Counts(array(1L,
                      dim = c(2, 2, 1, 2, 2, 1),
                      dimnames = list(reg_parent = c("a", "b"),
                          reg_child = c("b", "a"),
                          age = "20-24",
                          eth_parent = c("A", "B"),
                          eth_child = c("A", "B"),
                          time = "2001-2005")))
    expect_error(new("BirthsMovementsHasParentChild", .Data = x@.Data, metadata = x@metadata),
                 "has dimension with dimtype \"age\" but no dimension with dimtype \"triangle\"")
})

test_that("can create valid object of class BirthsTransitionsNoParentChild", {
    ## has orig-dest
    x <- Counts(array(1L,
                      dim = c(2, 2, 1, 1),
                      dimnames = list(reg_orig = c("a", "b"),
                          reg_dest = c("a", "b"),
                          age = "20-24",
                          time = "2001-2005")))
    x <- new("BirthsTransitionsNoParentChild", .Data = x@.Data, metadata = x@metadata,
             iMinAge = 5L)
    expect_true(validObject(x))
    expect_is(x, "BirthsTransitions")
    ## no orig-dest
    x <- Counts(array(1L,
                      dim = c(2, 1, 1),
                      dimnames = list(reg = c("a", "b"),
                          age = "20-24",
                          time = "2001-2005")))
    x <- new("BirthsTransitionsNoParentChild", .Data = x@.Data, metadata = x@metadata,
             iMinAge = 5L)
    expect_true(validObject(x))
    expect_is(x, "BirthsTransitions")
})

test_that("validity tests for BirthsTransitionsNoParentChild inherited from Transitions work", {
    ## time dimension has dimscale "Intervals"
    x <- Counts(array(1L,
                      dim = c(2, 2, 1),
                      dimnames = list(reg_orig = c("a", "b"),
                          reg_dest = c("a", "b"),
                          time = "2001")),
                dimscales = c(time = "Points"))
    expect_error(new("BirthsTransitionsNoParentChild", .Data = x@.Data, metadata = x@metadata,
                     iMinAge = NA_integer_),
                 "dimension with dimtype \"time\" has dimscale \"Points\"")
})

test_that("can create valid object of class BirthsTransitionsHasParentChild", {
    ## has orig-dest
    x <- Counts(array(1L,
                      dim = c(2, 2, 1, 2, 2, 1),
                      dimnames = list(reg_orig = c("a", "b"),
                          reg_dest = c("a", "b"),
                          age = "20-24",
                          eth_parent = c("A", "B"),
                          eth_child = c("A", "B"),
                          time = "2001-2005")))
    x <- new("BirthsTransitionsHasParentChild", .Data = x@.Data, metadata = x@metadata,
             iMinAge = 5L)
    expect_true(validObject(x))
    expect_is(x, "BirthsTransitions")
    ## no orig-dest
    x <- Counts(array(1L,
                      dim = c(2, 1, 2, 2, 1),
                      dimnames = list(reg = c("a", "b"),
                          age = "20-24",
                          eth_parent = c("A", "B"),
                          eth_child = c("A", "B"),
                          time = "2001-2005")))
    x <- new("BirthsTransitionsHasParentChild", .Data = x@.Data, metadata = x@metadata,
             iMinAge = 5L)
    expect_true(validObject(x))
    expect_is(x, "BirthsTransitions")
})

test_that("can create valid object of class InternalMovementsNet", {
    x <- Counts(array(c(-2L, 2L, 3L, -3L),
                      dim = c(2, 1, 1, 2),
                      dimnames = list(reg = c("a", "b"),
                          age = "20-24",
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    x <- new("InternalMovementsNet", .Data = x@.Data, metadata = x@metadata,
             iBetween = 1L)
    expect_true(validObject(x))
    expect_is(x, "InternalMovementsNet")
    x <- Counts(array(c(-2L, 1L, 1L, 3L, 0L, -3L),
                      dim = c(3, 1, 1, 2),
                      dimnames = list(eth = c("a", "b", "c"),
                          age = "20-24",
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    x <- aperm(x, perm = c(2, 1, 3, 4))
    x <- new("InternalMovementsNet", .Data = x@.Data, metadata = x@metadata,
             iBetween = 2L)
    expect_true(validObject(x))
    expect_is(x, "InternalMovementsNet")
    x <- Counts(array(rpois(n = 162, lambda = 10),
                      dim = c(3, 3, 3, 3, 2, 2, 1L),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                          reg_dest = c("a", "b", "c"),
                          eth_orig = c("a", "b", "c"),
                          eth_dest = c("a", "b", "c"),
                          age = c("0-4", "5+"),
                          triangle = c("Lower", "Upper"),
                          time = c("2001-2005"))))
    x <- collapseOrigDest(x, base = c("reg", "eth"))
    x <- new("InternalMovementsNet", .Data = x@.Data, metadata = x@metadata,
             iBetween = 1:2)
    expect_true(validObject(x))
    expect_is(x, "InternalMovementsNet")
})

test_that("validity tests for InternalMovementsNet inherited from iBetween work", {
    x <- Counts(array(c(-2L, 2L, 3L, -3L),
                      dim = c(2, 1, 1, 2),
                      dimnames = list(reg = c("a", "b"),
                          age = "20-24",
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    x <- new("InternalMovementsNet", .Data = x@.Data, metadata = x@metadata,
             iBetween = 1L)
    ## 'iBetween' has positive length
    x.wrong <- x
    x.wrong@iBetween <- integer()
    expect_error(validObject(x.wrong),
                 "'iBetween' has length 0")
    ## 'iBetween' has no missing values
    x.wrong <- x
    x.wrong@iBetween <- c(1L, NA)
    expect_error(validObject(x.wrong),
                 "'iBetween' has missing values")
    ## 'iBetween' indexes dimensions
    x.wrong <- x
    x.wrong@iBetween <- c(1L, 6L)
    expect_error(validObject(x.wrong),
                 "'iBetween' outside valid range")
    ## 'between' dimensions have length of at least 2
    x.wrong <- Counts(array(c(0L, 0L),
                      dim = c(1, 1, 1, 2),
                      dimnames = list(reg = "a",
                          age = "20-24",
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    expect_error(new("InternalMovementsNet",
                     .Data = x.wrong@.Data, metadata = x.wrong@metadata,
                     iBetween = 1L),
                 "\"between\" dimension \"reg\" has length 1")
    ## 'between' dimensions have dimtype "state"
    expect_error(new("InternalMovementsNet",
                     .Data = x@.Data, metadata = x@metadata,
                     iBetween = 4L),
                 "\"between\" dimension \"triangle\" has dimtype \"triangle\"")
})

test_that("validity tests for InternalMovementsNet inherited from NetSumsToZero work", {
    x <- Counts(array(c(-2L, 2L, 3L, -3L),
                      dim = c(2, 1, 1, 2),
                      dimnames = list(reg = c("a", "b"),
                          age = "20-24",
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    x <- new("InternalMovementsNet", .Data = x@.Data, metadata = x@metadata,
             iBetween = 1L)
    ## sums across "between" dimensions equal 0
    x.wrong <- x
    x.wrong[1] <- x.wrong[1] + 1L
    expect_error(validObject(x.wrong),
                 "\"between\" dimensions do not sum to 0")
})

test_that("can create valid object of class InternalMovementsPool", {
    x <- Counts(array(c(1L, 2L, 2L, 1L, 2L, 2L, 4L, 0L),
                      dim = c(2, 2, 1, 1, 2),
                      dimnames = list(reg = c("a", "b"),
                          direction = c("Out", "In"),
                          age = "20-24",
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    x <- new("InternalMovementsPool", .Data = x@.Data, metadata = x@metadata,
             iBetween = 1L, iDirection = 2L)
    expect_true(validObject(x))
    expect_is(x, "InternalMovementsPool")
    x <- Counts(array(c(1L, 2L, 2L, 1L, 2L, 2L, 4L, 0L),
                      dim = c(2, 2, 1, 1, 2),
                      dimnames = list(reg = c("a", "b"),
                          direction = c("Out", "In"),
                          age = "20-24",
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    x <- aperm(x, perm = c(3, 4, 2, 5, 1))
    x <- new("InternalMovementsPool", .Data = x@.Data, metadata = x@metadata,
             iBetween = 5L, iDirection = 3L)
    expect_true(validObject(x))
    expect_is(x, "InternalMovementsPool")
    x <- Counts(array(c(2L, 1L, 6L, 3L, 3L, 3L,
                        0L, 5L, 1L, 2L, 2L, 2L),
                      dim = c(3, 2, 1, 1, 2),
                      dimnames = list(eth = c("a", "b", "c"),
                          direction = c("Out", "In"),
                          age = "20-24",
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    x <- new("InternalMovementsPool", .Data = x@.Data, metadata = x@metadata,
             iBetween = 1L, iDirection = 2L)
    expect_true(validObject(x))
    expect_is(x, "InternalMovementsPool")
    x <- Counts(array(rpois(n = 162, lambda = 10),
                      dim = c(3, 3, 3, 3, 2, 2, 1L),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                          reg_dest = c("a", "b", "c"),
                          eth_orig = c("a", "b", "c"),
                          eth_dest = c("a", "b", "c"),
                          age = c("0-4", "5+"),
                          triangle = c("Lower", "Upper"),
                          time = c("2001-2005"))))
    x <- collapseOrigDest(x, base = c("reg", "eth"), to = "pool")
    x <- new("InternalMovementsPool", .Data = x@.Data, metadata = x@metadata,
             iBetween = 1:2, iDirection = 6L)
    expect_true(validObject(x))
    expect_is(x, "InternalMovementsPool")
    x <- Counts(array(rpois(n = 162, lambda = 10),
                      dim = c(3, 3, 3, 3, 2, 2, 1L),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                          reg_dest = c("a", "b", "c"),
                          eth_orig = c("a", "b", "c"),
                          eth_dest = c("a", "b", "c"),
                          age = c("0-4", "5+"),
                          triangle = c("Lower", "Upper"),
                          time = c("2001-2005"))))
    x <- collapseOrigDest(x, base = c("reg", "eth"), to = "pool")
    x <- new("InternalMovementsPool",
             .Data = x@.Data,
             metadata = x@metadata,
             iDirection = 6L,
             iBetween = 1:2)
    expect_true(validObject(x))
    expect_is(x, "InternalMovementsPool")
})

test_that("validity tests for InternalMovementsPool inherited from IDirection work", {
    x <- Counts(array(c(1L, 2L, 2L, 1L, 2L, 2L, 4L, 0L),
                      dim = c(2, 2, 1, 1, 2),
                      dimnames = list(reg = c("a", "b"),
                          direction = c("Out", "In"),
                          age = "20-24",
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    x <- new("InternalMovementsPool", .Data = x@.Data, metadata = x@metadata,
             iBetween = 1L, iDirection = 2L)
      ## 'iDirection' has length 1
    x.wrong <- x
    x.wrong@iDirection <- c(2L, 2L)
    expect_error(validObject(x.wrong),
                 "'iDirection' does not have length 1")
    ## 'iDirection' is not missing
    x.wrong <- x
    x.wrong@iDirection <- as.integer(NA)
    expect_error(validObject(x.wrong),
                 "'iDirection' is missing")
    ## 'iDirection' indexes a dimension
    x.wrong <- x
    x.wrong@iDirection <- 0L
    expect_error(validObject(x.wrong),
                 "'iDirection' outside valid range")
    ## 'direction' dimension has length 2
    x.wrong <- x
    x.wrong@iDirection <- 3L
    expect_error(validObject(x.wrong),
                 "\"direction\" dimension does not have length 2")
    ## 'direction' dimension has dimtype "state"
    x.wrong <- x
    x.wrong@iDirection <- 5L
    expect_error(validObject(x.wrong),
                 "\"direction\" dimension has dimtype \"triangle\"")
    ## 'direction' dimension has categories "Out", "In"
    x.wrong <- x
    dimnames(x.wrong@.Data)[[2]][1] <- "wrong"
    x.wrong@metadata@DimScales[[2]]@dimvalues[1] <- "wrong"
    expect_error(validObject(x.wrong),
                 "\"direction\" dimension does not have categories \"Out\", \"In\"")
    ## 'iBetween' not equal to 'iDirection'
    x.wrong <- x
    x.wrong@iBetween <- 2L
    expect_error(validObject(x.wrong),
                 "'iBetween' and 'iDirection' overlap")
})

test_that("validity tests for InternalMovementsPool inherited from InsEqualOuts work", {
    x <- Counts(array(c(2L, 1L, 1L, 2L, 4L, 0L, 2L, 2L),
                      dim = c(2, 2, 1, 1, 2),
                      dimnames = list(reg = c("a", "b"),
                          direction = c("Out", "In"),
                          age = "20-24",
                          time = "2001-2005",
                          triangle = c("Lower", "Upper"))))
    x <- new("InternalMovementsPool", .Data = x@.Data, metadata = x@metadata,
             iBetween = 1L, iDirection = 2L)
    ## ins equal outs
    x.wrong <- x
    x.wrong[1] <- x.wrong[1] + 1L
    expect_error(validObject(x.wrong),
                 "'ins' and 'outs' inconsistent")
})

test_that("can create valid object of class InternalMovementsOrigDest", {
    x <- Counts(array(rpois(n = 162, lambda = 10),
                      dim = c(3, 3, 3, 3, 2, 2, 1L),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                          reg_dest = c("a", "b", "c"),
                          eth_orig = c("a", "b", "c"),
                          eth_dest = c("a", "b", "c"),
                          age = c("0-4", "5+"),
                          triangle = c("Lower", "Upper"),
                          time = c("2001-2005"))))
    x <- new("InternalMovementsOrigDest", .Data = x@.Data, metadata = x@metadata)
    expect_true(validObject(x))
    expect_is(x, "InternalMovementsOrigDest")
})

test_that("validity tests for InternalMovementsOrigDest inherited from HasOrigDest work", {
    x <- Counts(array(1L,
                      dim = c(3, 3, 2, 2, 1L),
                      dimnames = list(reg = c("a", "b", "c"),
                          eth = c("a", "b", "c"),
                          age = c("0-4", "5+"),
                          triangle = c("Lower", "Upper"),
                          time = c("2001-2005"))))
    expect_error(new("InternalMovementsOrigDest",
                     .Data = x@.Data,
                     metadata = x@metadata),
                 "no dimensions with dimtype \"origin\" or \"destination\"")
    x <- Counts(array(rpois(n = 162, lambda = 10),
                      dim = c(3, 3, 3, 3, 2, 2, 1L),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                          reg_dest = c("a", "b", "c"),
                          eth_orig = c("a", "b", "c"),
                          eth_dest = c("a", "b", "wrong"),
                          age = c("0-4", "5+"),
                          triangle = c("Lower", "Upper"),
                          time = c("2001-2005"))))
    expect_error(new("InternalMovementsOrigDest",
                     .Data = x@.Data,
                     metadata = x@metadata),
                 "dimensions \"eth_orig\" and \"eth_dest\" use different categories")
})


test_that("can create valid object of class InternalTransitions", {
    x <- Counts(array(rpois(n = 162, lambda = 10),
                      dim = c(3, 3, 3, 3, 2, 1L),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                          reg_dest = c("a", "b", "c"),
                          eth_orig = c("a", "b", "c"),
                          eth_dest = c("a", "b", "c"),
                          age = c("0-4", "5+"),
                          time = c("2001-2005"))))
    x <- new("InternalTransitions", .Data = x@.Data, metadata = x@metadata)
    expect_true(validObject(x))
    expect_is(x, "InternalTransitions")
})

test_that("can create valid object of class EntriesMovements", {
    x <- Counts(array(rpois(n = 24, lambda = 10),
                      dim = c(3, 2, 2, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                          triangle = c("Lower", "Upper"),
                          age = c("0-4", "5+"),
                          time = c("2001-2005", "2006-2010"))))
    x <- new("EntriesMovements", .Data = x@.Data, metadata = x@metadata)
    expect_true(validObject(x))
    expect_is(x, "EntriesMovements")
})

test_that("can create valid object of class EntriesTransitions", {
    ## no orig-dest
    x <- Counts(array(rpois(n = 12, lambda = 10),
                      dim = c(3, 2, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                          age = c("0-4", "5+"),
                          time = c("2001-2005", "2006-2010"))))
    x <- new("EntriesTransitions", .Data = x@.Data, metadata = x@metadata)
    expect_true(validObject(x))
    expect_is(x, "EntriesTransitions")
    ## has orig-dest
    x <- Counts(array(rpois(n = 36, lambda = 10),
                      dim = c(3, 3, 2, 2),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                          reg_dest = c("a", "b", "c"),
                          age = c("0-4", "5+"),
                          time = c("2001-2005", "2006-2010"))))
    x <- new("EntriesTransitions", .Data = x@.Data, metadata = x@metadata)
    expect_true(validObject(x))
    expect_is(x, "EntriesTransitions")
})

test_that("can create valid object of class ExitsMovements", {
    x <- Counts(array(rpois(n = 24, lambda = 10),
                      dim = c(3, 2, 2, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                          triangle = c("Lower", "Upper"),
                          age = c("0-4", "5+"),
                          time = c("2001-2005", "2006-2010"))))
    x <- new("ExitsMovements", .Data = x@.Data, metadata = x@metadata)
    expect_true(validObject(x))
    expect_is(x, "ExitsMovements")
})

test_that("can create valid object of class ExitsTransitions", {
    ## no orig-dest
    x <- Counts(array(rpois(n = 12, lambda = 10),
                      dim = c(3, 2, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                          age = c("0-4", "5+"),
                          time = c("2001-2005", "2006-2010"))))
    x <- new("ExitsTransitions", .Data = x@.Data, metadata = x@metadata)
    expect_true(validObject(x))
    expect_is(x, "ExitsTransitions")
    ## has orig-dest
    x <- Counts(array(rpois(n = 36, lambda = 10),
                      dim = c(3, 3, 2, 2),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                          reg_dest = c("a", "b", "c"),
                          age = c("0-4", "5+"),
                          time = c("2001-2005", "2006-2010"))))
    x <- new("ExitsTransitions", .Data = x@.Data, metadata = x@metadata)
    expect_true(validObject(x))
    expect_is(x, "ExitsTransitions")
})

test_that("can create valid object of class NetMovements", {
    x <- Counts(array(as.integer(rnorm(n = 24, mean = 0, sd = 5)),
                      dim = c(3, 2, 2, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                          triangle = c("Lower", "Upper"),
                          age = c("0-4", "5+"),
                          time = c("2001-2005", "2006-2010"))))
    x <- new("NetMovements", .Data = x@.Data, metadata = x@metadata)
    expect_true(validObject(x))
    expect_is(x, "NetMovements")
})

test_that("can create valid object of class Accession", {
    x <- Counts(array(rpois(n = 24, lambda = 10),
                      dim = c(3, 2, 2, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                          sex = c("f", "m"),
                          age = c("5", "10"),
                          time = c("2001-2005", "2006-2010"))))
    x <- new("Accession", .Data = x@.Data, metadata = x@metadata)
    expect_true(validObject(x))
    expect_is(x, "Accession")
})

test_that("tests for Accession inherited from AgeIsPoints work", {
    x <- Counts(array(rpois(n = 24, lambda = 10),
                      dim = c(3, 2, 2, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                          sex = c("f", "m"),
                          age = c("0-4", "5-9"),
                          time = c("2001-2005", "2006-2010"))))
    expect_error(new("Accession", .Data = x@.Data, metadata = x@metadata),
                 "dimension with dimtype \"age\" has dimscale \"Intervals\"")
})

test_that("can create valid object of class Exposure", {
    x <- Counts(array(runif(n = 48, max = 10),
                      dim = c(3, 2, 2, 2, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                          sex = c("f", "m"),
                          age = c("0-4", "5+"),
                          triangle = c("Lower", "Upper"),
                          time = c("2001-2005", "2006-2010"))))
    x <- new("Exposure", .Data = x@.Data, metadata = x@metadata)
    expect_true(validObject(x))
    expect_is(x, "Exposure")
})

test_that("validity tests for Exposure inherited from IsDouble work", {
    x <- Counts(array(1L,
                      dim = c(3, 2, 2, 2, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                          sex = c("f", "m"),
                          age = c("0-4", "5+"),
                          triangle = c("Lower", "Upper"),
                          time = c("2001-2005", "2006-2010"))))
    expect_error(new("Exposure", .Data = x@.Data, metadata = x@metadata),
                 "does not have type \"double\"")
})

test_that("can create valid object of class Movements", {
    population <- CountsOne(values = 20:30,
                            labels = 2000:2010,
                            name = "time",
                            dimscale = "Points")
    population <- new("Population",
                      .Data = population@.Data,
                      metadata = population@metadata)
    births <- CountsOne(values = rep(1L, times = 10), labels = 2001:2010,
                        name = "time",
                        dimscale = "Intervals")
    births <- new("BirthsMovementsNoParentChild",
                  .Data = births@.Data,
                  metadata = births@metadata,
                  iMinAge = NA_integer_)
    x <- new("Movements",
             population = population,
             components = list(births),
             namesComponents = "Births")
    expect_true(validObject(x))
    expect_is(x, "Movements")
    population <- Counts(array(10L,
                               dim = c(3, 3),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                   time = c("2000", "2005", "2010"))))
    population <- new("Population",
                      .Data = population@.Data,
                      metadata = population@metadata)
    births <- Counts(array(1L,
                           dim = c(1, 2, 2),
                           dimnames = list(age = "5-9",
                               triangle = c("Lower", "Upper"),
                               time = c("2001-2005", "2006-2010"))))
    births <- new("BirthsMovementsNoParentChild",
                  .Data = births@.Data,
                  metadata = births@metadata,
                  iMinAge = 2L)
    deaths <- Counts(array(c(0L, 1L, 2L),
                           dim = c(3, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    deaths <- new("ExitsMovements",
                  .Data = deaths@.Data,
                  metadata = deaths@metadata)
    x <- new("Movements",
             population = population,
             components = list(births, deaths),
             namesComponents = c("births", "deaths"))
    expect_true(validObject(x))
    expect_is(x, "Movements")
})

test_that("validity tests for Movements inherited from DemographicAccount work", {
    population <- Counts(array(10L,
                               dim = c(3, 2, 3),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               reg = c("a", "b"),
                                               time = c("2000", "2005", "2010"))))
    population <- new("Population",
                      .Data = population@.Data,
                      metadata = population@metadata)
    births <- Counts(array(1L,
                           dim = c(1, 2, 2, 2),
                           dimnames = list(age = "5-9",
                                           reg = c("a", "b"),
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    births <- new("BirthsMovementsNoParentChild",
                  .Data = births@.Data,
                  metadata = births@metadata,
                  iMinAge = 2L)
    internal <- Counts(array(1L,
                             dim = c(3, 2, 2, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             reg_orig = c("a", "b"),
                                             reg_dest = c("a", "b"),
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("Lower", "Upper"))))
    internal <- new("InternalMovementsOrigDest",
                    .Data = internal@.Data,
                    metadata = internal@metadata)
    deaths <- Counts(array(c(0L, 1L, 2L),
                           dim = c(3, 2, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           reg = c("a", "b"),
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    deaths <- new("ExitsMovements",
                  .Data = deaths@.Data,
                  metadata = deaths@metadata)
    x <- new("Movements",
             population = population,
             components = list(births, internal, deaths),
             namesComponents = c("births", "internal", "deaths"))
    ## components has positive length
    x.wrong <- x
    x.wrong@components <- list()
    expect_error(validObject(x.wrong),
                 "'components' has length 0")
    ## no more than one births
    x.wrong <- x
    x.wrong@components <- c(x.wrong@components, x.wrong@components[1])
    expect_error(validObject(x.wrong),
                 "more than one component with class \"Births\"")
    ## if has births, first age group starts at 0
    x.wrong <- x
    wrong.popn <- Counts(array(10L,
                               dim = c(2, 2, 3),
                               dimnames = list(age = c("5-9", "10+"),
                                               reg = c("a", "b"),
                                               time = c("2000", "2005", "2010"))))
    wrong.popn <- new("Population",
                      .Data = wrong.popn@.Data,
                      metadata = wrong.popn@metadata)
    x.wrong@population <- wrong.popn
    expect_error(validObject(x.wrong),
                 "has component with class \"Births\", but minimum age for population is not 0")
    ## no more than one component with class "Internal"
    x.wrong <- x
    x.wrong@components <- c(x.wrong@components,
                            x.wrong@components[2])
    expect_error(validObject(x.wrong),
                 "more than one component with class \"Internal\"")
    ## 'namesComponents' has no missing values
    x.wrong <- x
    x.wrong@namesComponents[1] <- NA
    expect_error(validObject(x.wrong),
                 "'namesComponents' has missing values")
    ## 'namesComponents' has no blanks
    x.wrong <- x
    x.wrong@namesComponents[1] <- ""
    expect_error(validObject(x.wrong),
                 "'namesComponents' has blanks")
    ## 'namesComponents' has no duplicates
    x.wrong <- x
    x.wrong@namesComponents[2] <- "births"
    expect_error(validObject(x.wrong),
                 "'namesComponents' has duplicates")
    ## 'components' and 'namesComponents' have same length
    x.wrong <- x
    x.wrong@namesComponents <- x.wrong@namesComponents[-3]
    expect_error(validObject(x.wrong),
                 "'components' and 'namesComponents' have different lengths")
    ## all elements of 'components' compatible with 'population'
    x.wrong <- x
    wrong.deaths <- Counts(array(c(0L, 1L, 2L),
                                 dim = c(3, 2, 2, 3),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                                 triangle = c("Lower", "Upper"),
                                                 time = c("2001-2005", "2006-2010"),
                                                 reg = c("a", "b", "c"))))
    wrong.deaths <- new("ExitsMovements",
                        .Data = wrong.deaths@.Data,
                        metadata = wrong.deaths@metadata)
    x.wrong@components[[3]] <- wrong.deaths
    expect_error(validObject(x.wrong),
                 "'deaths' not compatible with 'population'")
})

test_that("validity tests for Movements inherited from Movements work", {
    population <- CountsOne(values = 20:30, labels = 2000:2010, name = "time",
                            dimscale = "Points")
    population <- new("Population",
                      .Data = population@.Data,
                      metadata = population@metadata)
    births <- CountsOne(values = rep(1L, times = 10), labels = 2001:2010,
                        name = "time", dimscale = "Intervals")
    births <- new("BirthsTransitionsNoParentChild",
                  .Data = births@.Data,
                  metadata = births@metadata,
                  iMinAge = NA_integer_)
    expect_error(new("Movements",
                     population = population,
                     components = list(births),
                     namesComponents = "Births"),
                 "'components' has elements not of class \"MovementsComponent\"")
})

test_that("can create valid object of class Transitions", {
    ## no age
    population <- CountsOne(values = 20:30, labels = 2000:2010, name = "time",
                            dimscale = "Points")
    population <- new("Population",
                      .Data = population@.Data,
                      metadata = population@metadata)
    births <- CountsOne(values = rep(1L, times = 10), labels = 2001:2010,
                        name = "time", dimscale = "Intervals")
    births <- new("BirthsTransitionsNoParentChild",
                  .Data = births@.Data,
                  metadata = births@metadata,
                  iMinAge = NA_integer_)
    x <- new("Transitions",
             population = population,
             components = list(births),
             namesComponents = "Births")
    expect_true(validObject(x))
    expect_is(x, "Transitions")
    ## has age
    population <- Counts(array(10L,
                               dim = c(3, 3, 2),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                   time = c("2000", "2005", "2010"),
                                               eth = c("a", "b"))))
    population <- new("Population",
                      .Data = population@.Data,
                      metadata = population@metadata)
    births <- Counts(array(1L,
                           dim = c(1, 2, 2, 2),
                           dimnames = list(age = "5-9",
                               time = c("2001-2005", "2006-2010"),
                               eth_orig = c("a", "b"),
                               eth_dest = c("a", "b"))))
    births <- new("BirthsTransitionsNoParentChild",
                  .Data = births@.Data,
                  metadata = births@metadata,
                  iMinAge = 2L)
    deaths <- Counts(array(c(0L, 1L, 2L),
                           dim = c(3, 2, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           time = c("2001-2005", "2006-2010"),
                                           eth_orig = c("a", "b"),
                                           eth_dest = c("a", "b"))))
    deaths <- new("ExitsTransitions",
                  .Data = deaths@.Data,
                  metadata = deaths@metadata)
    x <- new("Transitions",
             population = population,
             components = list(births, deaths),
             namesComponents = c("births", "deaths"))
    expect_true(validObject(x))
    expect_is(x, "Transitions")
})

test_that("validity tests for Transitions inherited from Transitions work", {
    population <- CountsOne(values = 20:30, labels = 2000:2010, name = "time",
                            dimscale = "Points")
    population <- new("Population",
                      .Data = population@.Data,
                      metadata = population@metadata)
    births <- CountsOne(values = rep(1L, times = 10), labels = 2001:2010,
                        name = "time", dimscale = "Intervals")
    births <- new("BirthsMovementsNoParentChild",
                  .Data = births@.Data,
                  metadata = births@metadata,
                  iMinAge = NA_integer_)
    expect_error(new("Transitions",
                     population = population,
                     components = list(births),
                     namesComponents = "Births"),
                 "'components' has elements not of class \"TransitionsComponent\"")
})

