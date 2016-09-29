
context("SummaryDemographicArray-generator")

test_that("summary works", {
    x <- Counts(array(rpois(n = 120, lambda = 1:12),
                      dim = c(2, 2, 3, 10),
                      dimnames = list(region = c("Region 1", "Region 2"),
                      sex = c("Male", "Female"),
                      age = c("0-4", "5-9", "10+"),
                      iteration = 1:10)))
    sx <- summary(x)
    expect_true(validObject(sx))
    expect_identical(x@metadata, sx@metadata)
    expect_identical(summary(x@.Data), sx@stats)
    x <- Counts(array(0, dim = 0, dimnames = list(sex = NULL)))
    sx <- summary(x)
    expect_true(validObject(sx))
    expect_identical(x@metadata, sx@metadata)
    expect_identical(summary(x@.Data), sx@stats)
})
