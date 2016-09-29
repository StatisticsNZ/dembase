

context("Transform-methods")
n.test <- 5
test.identity <- FALSE


test_that("transformInvolvesSubsetting works with CollapseTransform", {
    transformInvolvesSubsetting <- dembase:::transformInvolvesSubsetting
    ## 3x2 matrix, first dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L, 1L), 1:2),
                     dims = c(0L, 1L),
                     dimBefore = 3:2,
                     dimAfter = 2L)
    expect_false(transformInvolvesSubsetting(transform))
    ## 3x2 matrix, first dimension permuted
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 3L, 2L), 1:2),
                     dims = c(1L, 2L),
                     dimBefore = 3:2,
                     dimAfter = 3:2)
    expect_false(transformInvolvesSubsetting(transform))
    ## 3x2 matrix, transposed
    transform <- new("CollapseTransform",
                     indices = list(1:3, 1:2),
                     dims = 2:1,
                     dimBefore = 3:2,
                     dimAfter = 2:3)
    expect_false(transformInvolvesSubsetting(transform))
    ## 3x2 matrix, first row dropped, then result transposed
    transform <- new("CollapseTransform",
                     indices = list(c(0L, 1L, 2L), 1:2),
                     dims = c(2L, 1L),
                     dimBefore = 3:2,
                     dimAfter = c(2L, 2L))
    expect_true(transformInvolvesSubsetting(transform))
})

test_that("transformInvolvesSubsetting works with ExtendTransform", {
    transformInvolvesSubsetting <- dembase:::transformInvolvesSubsetting
    transform <- new("ExtendTransform",
                     indices = list(1:4, 1:3),
                     dims = 1:2,
                     dimBefore = 4:3,
                     dimAfter = 4:3)
    expect_false(transformInvolvesSubsetting(transform))
    transform <- new("ExtendTransform",
                     indices = list(c(1L, 1L, 2L, 2L), c(1L, 2L)),
                     dims = c(2L, 1L),
                     dimBefore = c(2L, 2L),
                     dimAfter = c(4L, 2L))
    expect_false(transformInvolvesSubsetting(transform))
    transform <- new("ExtendTransform",
                     indices = list(c(1:3, 3L), 1:3),
                     dims = 1:2,
                     dimBefore = 4:3,
                     dimAfter = 4:3)
    expect_true(transformInvolvesSubsetting(transform))
    transform <- new("ExtendTransform",
                     indices = list(c(1L, 1L), c(1L, 2L)),
                     dims = c(2L, 1L),
                     dimBefore = c(2L, 2L),
                     dimAfter = c(2L, 2L))
    expect_true(transformInvolvesSubsetting(transform))
})

test_that("transformIsOneToOne works", {
    transformIsOneToOne <- dembase:::transformIsOneToOne
    ## 3x2 matrix, first dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L, 1L), 1:2),
                     dims = c(0L, 1L),
                     dimBefore = 3:2,
                     dimAfter = 2L)
    expect_false(transformIsOneToOne(transform))
    ## 3x2 matrix, first dimension permuted
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 3L, 2L), 1:2),
                     dims = c(1L, 2L),
                     dimBefore = 3:2,
                     dimAfter = 3:2)
    expect_true(transformIsOneToOne(transform))
    ## 3x2 matrix, transposed
    transform <- new("CollapseTransform",
                     indices = list(1:3, 1:2),
                     dims = 2:1,
                     dimBefore = 3:2,
                     dimAfter = 2:3)
    expect_true(transformIsOneToOne(transform))
    ## 3x2 matrix, first row dropped, then result transposed
    transform <- new("CollapseTransform",
                     indices = list(c(0L, 1L, 2L), 1:2),
                     dims = c(2L, 1L),
                     dimBefore = 3:2,
                     dimAfter = c(2L, 2L))
    expect_true(transformIsOneToOne(transform))
})

