
context("SubArrayIndices-methods")

test_that("& works", {
    e1 <- new("SubArrayIndices",
              nms = c("region", "age"),
              indices = list(c(TRUE, FALSE), c(TRUE, TRUE, TRUE)))
    e2 <- new("SubArrayIndices",
              nms = c("sex", "age"),
              indices = list(c(TRUE, TRUE), c(TRUE, TRUE, FALSE)))
    e3 <- new("SubArrayIndices",
              nms = c("region", "age", "sex"),
              indices = list(c(TRUE, FALSE), c(TRUE, TRUE, FALSE), c(TRUE , TRUE)))
    expect_identical(e1 & e2, e3)
    e1 <- new("SubArrayIndices",
              nms = "region",
              indices = list(c(TRUE, FALSE)))
    e2 <- new("SubArrayIndices",
              nms = "sex",
              indices = list(c(TRUE, TRUE)))
    e3 <- new("SubArrayIndices",
              nms = c("region", "sex"),
              indices = list(c(TRUE, FALSE), c(TRUE , TRUE)))
    expect_identical(e1 & e2, e3)
})

test_that("| works", {
    e1 <- new("SubArrayIndices",
              nms = "region",
              indices = list(c(TRUE, FALSE)))
    e2 <- new("SubArrayIndices",
              nms = "region",
              indices = list(c(TRUE, TRUE)))
    expect_identical(e1 | e2, e2)
    e1 <- new("SubArrayIndices",
              nms = c("region", "age"),
              indices = list(c(TRUE, FALSE), c(TRUE, TRUE, TRUE)))
    e2 <- new("SubArrayIndices",
              nms = c("sex", "age"),
              indices = list(c(TRUE, TRUE), c(TRUE, TRUE, FALSE)))
    expect_error(e1 | e2,
                 "'|' operator applied to multiple dimensions")
})

test_that("! works", {
    x <- new("SubArrayIndices",
              nms = "region",
              indices = list(c(TRUE, FALSE)))
    y <- new("SubArrayIndices",
              nms = "region",
              indices = list(c(FALSE, TRUE)))
    expect_identical(!x, y)
    x <- new("SubArrayIndices",
              nms = c("region", "age"),
              indices = list(c(TRUE, FALSE), c(TRUE, FALSE)))
    expect_error(!x,
                 "attempt to apply '!' operator to expression involving more than one dimension")
})
