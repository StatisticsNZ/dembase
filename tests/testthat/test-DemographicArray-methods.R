

context("DemographicArray-methods")

test_that("coercion functions work", {
  a <- array(1:6,
             dim = c(3, 2),
             dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
  x <- Counts(a)
  expect_that(as(x, "matrix"), is_identical_to(a))
  expect_that(as(x, "array"), is_identical_to(a))
  as(x, "matrix") <- matrix(6:1, nrow = 3)
  expect_that(as(x, "integer"), is_identical_to(6:1))
  a <- array(1:12,
             dim = c(3, 2, 2),
             dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female"),
               year = c(2000, 2010)))
  x <- Counts(a)
  expect_that(as(x, "matrix"),
              throws_error("object does not have two dimensions"))
  expect_that(as(x, "array"), is_identical_to(a))
  y <- Values(a)
  expect_identical(as(x, "Values"), y)
  expect_identical(as(y, "Counts"), x)
})

test_that("methods inherited from array work", {
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_that(dim(x), is_identical_to(dim(a)))
    expect_that(length(x), is_identical_to(length(a)))
    expect_that(ncol(x), is_identical_to(ncol(a)))
    expect_that(nrow(x), is_identical_to(nrow(a)))
    expect_that(NCOL(x), is_identical_to(NCOL(a)))
    expect_that(NROW(x), is_identical_to(NROW(a)))
    expect_that(diag(x), is_identical_to(diag(a)))
})

test_that("Extract method for DemographicArray works", {
    a <- array(1:12,
               dim = c(2, 2, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
                   sex = c("Male", "Female"),
                   age = c("0-4", "5-9", "10+")))
    x <- Counts(a)
    expect_identical(x[1, , ],
                     Counts(a[1, , ]))
    expect_identical(x[1, , 1],
                     a[1, , 1])
    expect_identical(x[2:1, 2:1, 2:1],
                     Counts(a[2:1, 2:1, 2:1], dimtypes = c(age = "state")))
    expect_identical(x[1, , -1],
                     Counts(a[1, , -1]))
    expect_identical(x[1:5],
                     a[1:5])
    expect_identical(x[rbind(c(1,1,2), c(2,2,3))],
                     a[rbind(c(1,1,2), c(2,2,3))])
    expect_identical(x[c(TRUE, FALSE)],
                     a[c(TRUE, FALSE)])
    expect_identical(x[F,,],
                     Counts(array(0L,
                                  dim = c(0, 2, 3),
                                  dimnames = list(region = NULL,
                                      sex = c("Male", "Female"),
                                      age = c("0-4", "5-9", "10+")))))
    expect_identical(x[,,F],
                     Counts(array(0L,
                                  dim = c(2, 2, 0),
                                  dimnames = list(region = c("Region 1", "Region 2"),
                                      sex = c("Male", "Female"),
                                      age = NULL))))
    expect_identical(x[F,,F],
                     Counts(array(0L,
                                  dim = c(0, 2, 0),
                                  dimnames = list(region = NULL,
                                      sex = c("Male", "Female"),
                                      age = NULL))))
    x <- Counts(array(1:2,
                      dim = c(2, 2),
                      dimnames = list(sex = c("f", "m"), pool = c("Ins", "Outs"))))
    y <- Counts(array(1:2,
                      dim = c(2, 1),
                      dimnames = list(sex = c("f", "m"), pool = "Outs")))
    z <- c(f = 1L, m = 2L)
    expect_identical(x[,2, drop = FALSE], y)
    expect_identical(x[,c(FALSE, TRUE), drop = FALSE], y)
    expect_identical(x[,"Outs", drop = FALSE], y)
    expect_identical(x[,2], z)
    expect_identical(x[,c(FALSE, TRUE)], z)
    expect_identical(x[,"Outs"], z)
    x <- Counts(array(1:8,
                      dim = c(2, 2, 2),
                      dimnames = list(reg_orig = c("a", "b"),
                          reg_dest = c("a", "b"), sex = c("f", "m"))))
    y <- Counts(array(c(1L, 3L, 5L, 7L),
                      dim = c(2, 2),
                      dimnames = list(reg = c("a", "b"), sex = c("f", "m"))))
    expect_identical(x[1,,], y)
    x <- Counts(array(c(1:4, 2:1, 4:3),
                      dim = c(2, 2, 2),
                      dimnames = list(reg = c("a", "b"), age = c("0-4", "5+"), pool = c("Ins", "Outs"))))
    y <- Counts(array(c(1L, 3L, 2L, 4L),
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"), pool = c("Ins", "Outs"))))
    z <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg = c("a", "b"), age = c("0-4", "5+"))))
    expect_identical(x[1, , ], y)
    expect_identical(x[, , 1], z)
    ## Extract inside loop
    x <- Values(array(1:16,
                      dim = c(2, 2, 2, 2),
                      dimnames = list(age = 0:1,
                          sex = c("f", "m"),
                          reg = 1:2,
                          eth = 1:2)))
    for (i in 1:2)
        for (j in 1:2)
            expect_identical(length(x[,,i,j]), 4L)
})

test_that("replacement methods for Extract work", {
    x <- Counts(array(1:12,
               dim = c(2, 2, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
                   sex = c("Male", "Female"),
                   age = c("0-4", "5-9", "10+"))))
    ## single element, single index
    x.tmp <- x
    x.tmp[1] <- 99
    expect_identical(x.tmp[1,1,1], 99)
    ## single element, multiple index
    x.tmp <- x
    x.tmp[1,2,3] <- 99
    expect_identical(x.tmp[1,2,3], 99)
    ## single element inside loop
    x.tmp <- x
    for (i in 1:2)
        x.tmp[i] <- 99
    expect_identical(as.numeric(x.tmp[,1,1]), rep(99, 2))
    ## multiple elements, single index
    x.tmp <- x
    x.tmp[10:12] <- 100:102
    expect_identical(x.tmp[10:12], 100:102)
    ## multiple elements, multiple indices
    x.tmp <- x
    x.tmp[2,2,] <- 100:102
    expect_identical(x.tmp[c(4,8,12)], 100:102)
    ## multiple elements, multiple indices, inside loop
    x.tmp <- x
    for (i in 1:3)
        x.tmp[,,i] <- 1:4
    expect_identical(x.tmp[1:12], c(1:4, 1:4, 1:4))
})

test_that("Summary works - with iterations", {
    ## min, mixture of classes
    a <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   iteration = 1:2))
    x <- Counts(a)
    ans.obtained <- min(x, 100, 0.5*aperm(x, 3:1))
    ans.expected <- CountsOne(c(0.5, 3.5), labels = 1:2, name = "iteration")
    expect_identical(ans.obtained, ans.expected)
    ## max, with NA
    a <- array(c(NA, 2:12),
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   iteration = 1:2))
    x <- Values(a)
    ans.obtained <- max(x, x)
    ans.expected <- ValuesOne(c(NA, 12L), labels = 1:2, name = "iteration")
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- max(x, x, na.rm = TRUE)
    ans.expected <- ValuesOne(c(6L, 12L), labels = 1:2, name = "iteration")
    expect_identical(ans.obtained, ans.expected)
    ## zero-length
    a <- array(c(NA, 2:12),
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   iteration = 1:2))
    x <- Values(a)
    ans.obtained <- max(x, numeric())
    ans.expected <- ValuesOne(c(NA, 12L), labels = 1:2, name = "iteration")
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- max(numeric(), x, numeric(), x, na.rm = TRUE)
    ans.expected <- ValuesOne(c(6, 12), labels = 1:2, name = "iteration")
    expect_identical(ans.obtained, ans.expected)
    a <- array(0,
               dim = c(0, 2, 2),
               dimnames = list(age = character(),
                   sex = c("Male", "Female"),
                   iteration = 1:2))
    x <- Values(a,
                dimscales = c(age = "Intervals"))
    ans.obtained <- max(x, 1, na.rm = TRUE)
    ans.expected <- ValuesOne(c(1, 1), labels = 1:2, name = "iteration")
    expect_identical(ans.obtained, ans.expected)
})

test_that("Summary works - range with iterations", {
    ## min, mixture of classes
    a <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   iteration = 1:2))
    x <- Counts(a)
    ans.obtained <- range(x, 100, aperm(x, 3:1))
    ans.expected <- Counts(array(c(range(a[,,1], 100), range(a[,,2], 100)),
                                 dim = c(2, 2),
                                 dimnames = list(range = c("min", "max"),
                                     iteration = 1:2)))
    expect_identical(ans.obtained, ans.expected)
    ## max, with NA
    a <- array(c(NA, 2:12),
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   iteration = 1:2))
    x <- Values(a)
    ans.obtained <- range(x, x)
    ans.expected <- Values(array(c(range(a[,,1]), range(a[,,2])),
                                 dim = c(2, 2),
                                 dimnames = list(range = c("min", "max"),
                                     iteration = 1:2)))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- range(x, x, na.rm = TRUE)
    ans.expected <- Values(array(c(range(a[,,1], na.rm = T), range(a[,,2])),
                                 dim = c(2, 2),
                                 dimnames = list(range = c("min", "max"),
                                     iteration = 1:2)))
    expect_identical(ans.obtained, ans.expected)
    ## zero-length
    a <- array(c(NA, 2:12),
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   iteration = 1:2))
    x <- Values(a)
    ans.obtained <- range(x, numeric())
    ans.expected <- Values(array(c(range(a[,,1]), range(a[,,2])),
                                 dim = c(2, 2),
                                 dimnames = list(range = c("min", "max"),
                                     iteration = 1:2)))
    expect_equal(ans.obtained, ans.expected)
    ans.obtained <- range(numeric(), x, numeric(), x, na.rm = TRUE)
    ans.expected <- Values(array(c(range(a[,,1], na.rm = T), range(a[,,2])),
                                 dim = c(2, 2),
                                 dimnames = list(range = c("min", "max"),
                                     iteration = 1:2)))
    expect_equal(ans.obtained, ans.expected)
    a <- array(0,
               dim = c(0, 2, 2),
               dimnames = list(age = character(),
                   sex = c("Male", "Female"),
                   iteration = 1:2))
    x <- Values(a,
                dimscales = c(age = "Intervals"))
    ans.obtained <- range(x, 1, na.rm = TRUE)
    ans.expected <- Values(array(c(range(a[,,1], 1, na.rm = T), range(a[,,2], 1)),
                                 dim = c(2, 2),
                                 dimnames = list(range = c("min", "max"),
                                     iteration = 1:2)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("Summary works - no iterations", {
    ## min, mixture of classes
    a <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   region = 1:2))
    x <- Counts(a)
    ans.obtained <- min(x, 100, 0.5*aperm(x, 3:1))
    ans.expected <- min(1:12, 100, 0.5*(1:12))
    expect_identical(ans.obtained, ans.expected)
    ## max, with NA
    a <- array(c(NA, 2:12),
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   region = 1:2))
    x <- Values(a)
    ans.obtained <- max(x, x)
    ans.expected <- as.integer(NA)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- max(x, x, na.rm = TRUE)
    ans.expected <- 12L
    expect_identical(ans.obtained, ans.expected)
    ## zero-length
    a <- array(c(NA, 2:12),
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   region = 1:2))
    x <- Values(a)
    ans.obtained <- max(x, numeric())
    ans.expected <- as.integer(NA)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- max(numeric(), x, numeric(), x, na.rm = TRUE)
    ans.expected <- 12
    expect_identical(ans.obtained, ans.expected)
    a <- array(0,
               dim = c(0, 2, 2),
               dimnames = list(age = character(),
                   sex = c("Male", "Female"),
                   region = 1:2))
    x <- Values(a,
                dimscales = c(age = "Intervals"))
    ans.obtained <- max(x, 1, na.rm = TRUE)
    ans.expected <- 1
    expect_identical(ans.obtained, ans.expected)
})

test_that("Summary works - range, no iterations", {
    ## min, mixture of classes
    a <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   region = 1:2))
    x <- Counts(a)
    ans.obtained <- range(x, 100, 0.5*aperm(x, 3:1))
    ans.expected <- range(1:12, 100, 0.5*(1:12))
    expect_identical(ans.obtained, ans.expected)
    ## max, with NA
    a <- array(c(NA, 2:12),
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   region = 1:2))
    x <- Values(a)
    ans.obtained <- range(x, x)
    ans.expected <- c(as.integer(NA), as.integer(NA))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- range(x, x, na.rm = TRUE)
    ans.expected <- c(2L, 12L)
    expect_identical(ans.obtained, ans.expected)
    ## zero-length
    a <- array(c(NA, 2:12),
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   region = 1:2))
    x <- Values(a)
    ans.obtained <- range(x, numeric())
    ans.expected <- c(as.integer(NA), as.integer(NA))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- range(numeric(), x, numeric(), x, na.rm = TRUE)
    ans.expected <- c(2, 12)
    expect_identical(ans.obtained, ans.expected)
    a <- array(0,
               dim = c(0, 2, 2),
               dimnames = list(age = character(),
                   sex = c("Male", "Female"),
                   region = 1:2))
    x <- Values(a,
                dimscales = c(age = "Intervals"))
    ans.obtained <- range(x, 1, na.rm = TRUE)
    ans.expected <- c(1, 1)
    expect_identical(ans.obtained, ans.expected)
})


test_that("Summary works - with quantiles", {
    a <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   quantile = c(0.025, 0.975)))
    x <- Counts(a)
    expect_error(all(x),
                 "dimension with dimtype \"quantile\"")
    expect_error(any(1L, x, 1L),
                 "dimension with dimtype \"quantile\"")
})

test_that("DimScales method works", {
  DimScales <- dembase:::DimScales
  a <- array(1:6,
             dim = c(3, 2),
             dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
  x <- Counts(a)
  expect_that(DimScales(x),
              is_identical_to(list(age = new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                                   sex = new("Sexes", dimvalues = c("Male", "Female")))))
  expect_that(DimScales(x, use.names = FALSE),
              is_identical_to(list(new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                                   new("Sexes", dimvalues = c("Male", "Female")))))
})

test_that("ageMax works", {
    x <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("Male", "Female"))))
    expect_identical(ageMax(x), Inf)
})

test_that("ageMax<- works", {
    x <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("Male", "Female"))))
    ageMax(x) <- 15
    expect_identical(x,
                     Values(array(1:6,
                                  dim = c(3, 2),
                                  dimnames = list(age = c("0-4", "5-9", "10-14"),
                                                  sex = c("Male", "Female")))))
})

test_that("ageMin works", {
    x <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("Male", "Female"))))
    expect_identical(ageMin(x), 0)
})

test_that("ageMin<- works", {
    x <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("Male", "Female"))))
    ageMin(x) <- -Inf
    expect_identical(x,
                     Values(array(1:6,
                                  dim = c(3, 2),
                                  dimnames = list(age = c("<5", "5-9", "10+"),
                                                  sex = c("Male", "Female")))))
})

test_that("ageTimeStep works", {
  a <- array(1:6,
             dim = c(3, 2),
             dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
  x <- Counts(a)
  expect_identical(ageTimeStep(x), 5)
  a <- array(1:6,
             dim = c(3, 2),
             dimnames = list(age = c("0-4", "5-9", "10+"),
               year = c(2000, 2005)))
  x <- Counts(a)
  expect_identical(ageTimeStep(x), 5)
  a <- array(1,
             dim = c(101, 11),
             dimnames = list(age = c(0:99, "100+"),
               time = 2000:2010))
  x <- Counts(a,
              dimscales = c(time = "Intervals"))
  expect_identical(ageTimeStep(x), 1)
  a <- array(1:6,
             dim = c(3, 2),
             dimnames = list(age = c("0-4", "5-9", "10+"),
               year = c(2000, 2010)))
  x <- Counts(a)
  expect_error(ageTimeStep(x),
               "age step \\[5\\] does not equal time step \\[10\\]")
  a <- array(1:8,
             dim = c(4, 2),
             dimnames = list(age = c("0", "1-4", "5-9", "10+"),
               year = c(2000, 2005)))
  x <- Counts(a)
  expect_error(ageTimeStep(x),
               "age steps unequal")
  x <- Counts(array(0, dim = 2, dimnames = list(sex = c("m", "f"))))
  expect_error(ageTimeStep(x),
               "does not have age or time dimensions")
  x <- Counts(array(0, dim = 1, dimnames = list(time = 2000)),
              dimscales = c(time = "Points"))
  expect_error(ageTimeStep(x),
               "time dimension does not have any steps")
  x <- Counts(array(0, dim = 0, dimnames = list(age = NULL)))
  expect_error(ageTimeStep(x),
               "age dimension does not have any steps")
  x <- Counts(array(0, dim = c(0, 1), dimnames = list(age = NULL, year = 2000)),
              dimscales = c(age = "Intervals", year = "Points"))
  expect_error(ageTimeStep(x),
               "neither age dimension nor time dimension has any steps")
})

test_that("alignPair works", {
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b"))))
    y <- Counts(array(c(1:6, 0L, 0L, 0L),
                      dim = c(3, 3),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"))))
    z <- Counts(array(c(1:6, -1L, -1L, -1L),
                      dim = c(3, 3),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"))))
    expect_identical(alignPair(x),
                     y)
    expect_identical(alignPair(x, base = "reg"),
                     y)
    expect_identical(alignPair(x, base = "reg", omitted = -1L),
                     z)
    expect_error(alignPair(x, base = c("wrong1", "wrong2")),
                 "\"wrong1\" is not a valid base name")
    x <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b"))))
    y <- Values(array(c(1:6, NA, NA, NA),
                      dim = c(3, 3),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"))))
    expect_identical(alignPair(x),
                     y)
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "c"))))
    y <- Counts(array(c(1:3, 0L, 0L, 0L, 4:6),
                      dim = c(3, 3),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"))))
    expect_identical(alignPair(x),
                     y)
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("c", "b"))))
    y <- Counts(array(c(0L, 0L, 0L, 4:6, 1:3),
                      dim = c(3, 3),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"))))
    expect_identical(alignPair(x),
                     y)
    x <- Counts(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(reg_child = c("c", "a"),
                      reg_parent = c("a", "b", "c"))))
    y <- Counts(array(c(2L, 0L, 1L, 4L, 0L, 3L, 6L, 0L, 5L),
                      dim = c(3, 3),
                      dimnames = list(reg_child = c("a", "b", "c"),
                      reg_parent = c("a", "b", "c"))))
    expect_identical(alignPair(x),
                     y)
    x <- Counts(array(1:16,
                      dim = c(2, 2, 2, 2),
                      dimnames = list(reg_child = c("c", "b"),
                      reg_parent = c("a", "b"),
                      reg_orig = c("d", "e"),
                      reg_dest = c("e", "f"))))
    res <- alignPair(x)
    expect_identical(sum(res), sum(x))
    expect_identical(dimnames(res),
                      list(reg_child = c("a", "b", "c"),
                           reg_parent = c("a", "b", "c"),
                           reg_orig = c("d", "e", "f"),
                           reg_dest = c("d", "e", "f")))
})

test_that("aperm method for DemographicArray works", {
  a <- array(1:12,
             dim = c(2, 2, 3),
             dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
  x <- Counts(a)
  expect_that(aperm(aperm(x, 3:1), 3:1), is_identical_to(x))
  expect_that(aperm(x), is_identical_to(x))
  expect_that(aperm(x, perm = NULL), is_identical_to(x))
  expect_that(aperm(x, c("age", "region", "sex")),
              is_identical_to(aperm(x, c(3,1,2))))
  expect_that(aperm(x, 3:1, resize = FALSE),
              is_identical_to(aperm(a, 3:1, resize = FALSE)))
  expect_that(aperm(x, 3:1, keep.class = FALSE),
              is_identical_to(aperm(a, 3:1, keep.class = FALSE)))
  expect_that(aperm(x, 3), throws_error("'perm' is of wrong length"))
  expect_that(aperm(x, 3:5), throws_error("value out of range in 'perm'"))
  expect_that(aperm(x, rep("wrong", 3)),
              throws_error("'perm\\[1\\]' does not match a dimension name"))
})

test_that("as.array method for DemographicArray works", {
  a <- array(1:12,
             dim = c(2, 2, 3),
             dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
  x <- Counts(a)
  expect_identical(as.array(x), a)
})

test_that("as.matrix method for DemographicArray works", {
  a <- array(1:12,
             dim = c(2, 2, 3),
             dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
  x <- Counts(a)
  expect_identical(as.matrix(x), as.matrix(a))
  m <- array(1:12,
             dim = c(4, 3),
             dimnames = list(region = 1:4,
               age = c("0-4", "5-9", "10+")))
  x <- Counts(m)
  expect_identical(as.matrix(x), m)
})

test_that("collapseIterations works", {
    a <- array(rpois(n = 120, lambda = 1:12),
               dim = c(2, 2, 3, 10),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+"),
               iteration = 1:10))
    x <- Counts(a)
    b <- apply(a, 1:3, quantile)
    b <- aperm(b, perm = c(2, 3, 4, 1))
    names(dimnames(b))[4] <- "quantile"
    y <- Counts(b)
    expect_identical(collapseIterations(x), y)
    a <- array(rpois(n = 120, lambda = 1:12),
               dim = c(2, 2, 10, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               iteration = 1:10,
               age = c("0-4", "5-9", "10+")))
    x <- Counts(a)
    b <- apply(a, c(1,2,4), quantile, probs = c(0.025, 0.5, 0.975))
    b <- aperm(b, perm = c(2, 3, 1, 4))
    names(dimnames(b))[3] <- "quantile"
    y <- Counts(b)
    expect_identical(collapseIterations(x, probs = c(0.025, 0.5, 0.975)), y)
    a <- array(rpois(n = 120, lambda = 1:12),
               dim = c(2, 2, 10, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               iteration = 1:10,
               age = c("0-4", "5-9", "10+")))
    x <- Counts(a)
    f <- function(x) c(mean = mean(x), median = median(x))
    b <- apply(a, c(1,2,4), f)
    b <- aperm(b, perm = c(2, 3, 1, 4))
    names(dimnames(b))[3] <- "f"
    y <- Counts(b)
    expect_identical(collapseIterations(x, f), y)
    a <- array(rpois(n = 120, lambda = 1:12),
               dim = c(2, 2, 10, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               iteration = 1:10,
               age = c("0-4", "5-9", "10+")))
    x <- Counts(a)
    expect_identical(collapseIterations(x, FUN = function(y) rep(1L, max(y))),
                     apply(a, c(1, 2, 4), function(y) rep(1L, max(y))))
    a <- array(rpois(n = 20, lambda = 1:2),
               dim = c(2, 10),
               dimnames = list(sex = c("Male", "Female"), iteration = 1:10))
    x <- Counts(a)
    y <- Counts(array(rowMeans(a), dim = 2, dimnames = list(sex = c("Male", "Female"))))
    expect_identical(collapseIterations(x, mean), y)
    f <- function(x) 1:3
    expect_error(collapseIterations(x, f),
                 "return values of 'FUN' do not have names")
    f <- function(x) c(a = 1, 2)
    expect_error(collapseIterations(x, f),
                 "return values of 'FUN' do not have valid names")
    f <- function(x) c(a = 1, a = 2)
    expect_error(collapseIterations(x, f),
                 sprintf("return values of 'FUN' have duplicated names \\[%s\\]",
                         dQuote("a")))
    f <- function(x) c(a = 1, a = 2, b = 3, b = 4)
    expect_error(collapseIterations(x, f),
                 sprintf("return values of 'FUN' have duplicated names \\[%s, %s\\]",
                         dQuote("a"), dQuote("b")))
    x <- CountsOne(1:10, labels = 1:10, name = "iteration")
    ans.obtained <- collapseIterations(x, prob = c(0.05, 0.95))
    ans.expected <- CountsOne(quantile(1:10, prob = c(0.05, 0.95)),
                              labels = c("5%", "95%"),
                              name = "quantile")
    expect_identical(ans.obtained, ans.expected)
})



test_that("replacement method for dim raises correct error", {
  a <- array(1:6,
             dim = c(3, 2),
             dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
  x <- Counts(a)
  expect_that(dim(x) <- 2:3,
              throws_error("dimensions of object of class \"Counts\" cannot be modified directly"))
})

test_that("dimnames method work", {
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(dimnames(x), dimnames(a))
    expect_error(dimnames(x) <- "wrong",
                 paste("dimnames of object of class \"Counts\"",
                       "cannot be modified directly"))
})

test_that("dimscales methods work", {
  a <- array(1:6,
             dim = c(3, 2),
             dimnames = list(age = 0:2,
               sex = c("Male", "Female")))
  x <- Counts(a, dimscales = c(age = "Points"))
  expect_that(dimscales(x),
              is_identical_to(c(age = "Points", sex = "Sexes")))
  expect_that(dimscales(x, use.names = FALSE),
              is_identical_to(c("Points", "Sexes")))
  dimscales(x)[1] <- "Intervals"
  expect_that(dimscales(x),
              is_identical_to(c(age = "Intervals", sex = "Sexes")))
  expect_that(dimscales(x)[1] <- "Categories",
              throws_error("dimension \"age\" has dimtype \"age\" but dimscale \"Categories\""))
  a <- array(1:6,
             dim = c(3, 2),
             dimnames = list(region = c("a", "b", "c"),
               sex = c("Male", "Female")))
  x <- Counts(a)
  dimscales(x) <- c("Categories", "Sexes")
  expect_that(dimscales(x),
              is_identical_to(c(region = "Categories", sex = "Sexes")))
})

test_that("dimtypes methods work", {
  a <- array(1:6,
             dim = c(3, 2),
             dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
  x <- Counts(a)
  expect_identical(dimtypes(x),
                   c(age = "age", sex = "sex"))
  expect_identical(dimtypes(x, use.names = FALSE),
                   c("age", "sex"))
  dimtypes(x)[1] <- "state"
  expect_identical(dimtypes(x)[[2]], "sex")
})

test_that("drop works", {
    x <- Values(array(1:4,
                      dim = c(2, 2, 1),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("m", "f"),
                      reg = "a")))
    y <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("m", "f"))))
    expect_identical(drop(x), y)
    x <- Counts(array(1:3,
                      dim = c(3, 1),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = "Female")))
    y <- c("0-4" = 1L, "5-9" = 2L, "10+" = 3L)
    expect_identical(drop(x), y)
    x <- Counts(array(1, dim = 1, dimnames = list(sex = "Male")))
    expect_identical(drop(x), c(Male = 1))
    x <- Counts(array(0, dim = 0, dimnames = list(sex = NULL)))
    expect_identical(drop(x), x)
    x <- Counts(array(c(1L, 3L, 5L, 7L),
                      dim = c(1, 2, 2),
                      dimnames = list(reg_orig = "a",
                      reg_dest = c("a", "b"), sex = c("f", "m"))))
    y <- Counts(array(c(1L, 3L, 5L, 7L),
                      dim = c(2, 2),
                      dimnames = list(reg = c("a", "b"), sex = c("f", "m"))))
    expect_identical(drop(x), y)

})

test_that("extrapolate works", {
    x <- Counts(array(1:12,
                      dim = c(3, 4),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          year = c(2000, 2005, 2010, 2015))))
    expect_identical(extrapolate(x, labels = 2020, growth = 0.2),
                     extrapolate(x, labels = 2016:2020, growth = 0.2)[,c(1:4, 9)])
    expect_identical(extrapolate(x, labels = 2020, growth = 0.2, type = "l"),
                     extrapolate(x, labels = 2016:2020, growth = 0.2, type = "l")[,c(1:4, 9)])
    expect_identical(extrapolate(x, labels = c(2020, 2025), type = "missing"),
                     dbind(x,
                           Counts(array(as.numeric(NA),
                                        dim = c(3, 2),
                                        dimnames = list(age = c("0-4", "5-9", "10+"),
                                            year = c(2020, 2025)))),
                           along = "year"))
    expect_identical(extrapolate(x, along = "age", labels = "-5--1", growth = 0),
                     dbind(x,
                           Counts(array(c(1, 4, 7, 10),
                                        dim = c(1, 4),
                                        dimnames = list(age = "-5--1", year = seq(2000, 2015, 5)))),
                           along = "age"))
    x <- ValuesOne(c(2, 2),
                   labels = c("5-9", "10+"),
                   name = "age")
    ans.obtained <- extrapolate(x, labels = "0-4")
    ans.expected <- ValuesOne(c(2, 2, 2),
                              labels = c("0-4", "5-9", "10+"),
                              name = "age")
    expect_identical(ans.obtained, ans.expected)
    x <- ValuesOne(c(2, 2), 
                   labels = c("5-9", "10-14"),
                   name = "age")
    ans.obtained <- extrapolate(x, labels = c("15-19", "20-24"), growth = 0.05, type = "exponential")
    ans.expected <- ValuesOne(c(2, 2, 2*1.05, 2*1.05^2),
                              labels = c("5-9", "10-14", "15-19", "20-24"),
                              name = "age")
    x <- ValuesOne(c(2, 2), 
                   labels = c("5-9", "10-14"),
                   name = "age")
    ans.obtained <- extrapolate(x, labels = c("15-19", "20-24"), type = "missing")
    ans.expected <- ValuesOne(c(2, 2, NA, NA),
                              labels = c("5-9", "10-14", "15-19", "20-24"),
                              name = "age")
    expect_identical(ans.obtained, ans.expected)
    x <- Values(array(1, dim = c(1, 1), dimnames = list(age = "0", sex = "f")),
                dimscales = c(age = "Intervals"))
    ans.obtained <- extrapolate(x, along = 1L, labels = c("1", "2", "3"), type = "missing")
    ans.expected <- Values(array(c(1, NA, NA, NA),
                                 dim = c(4, 1), dimnames = list(age = c("0", "1", "2", "3"), sex = "f")),
                           dimscales = c(age = "Intervals"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("extrapolate throws appropriate errors", {
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(quantile = c("1%", "50%", "99%"), time = 2000:2001)),
                dimscales = c(time = "Points"))
    expect_error(extrapolate(x, labels = c(2000, 2005), growth = 1),
                 "dimension with dimtype \"quantile\"")
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(iteration = 1:3, time = 2000:2001)),
                dimscales = c(time = "Points"))
    expect_error(extrapolate(x, labels = 4:5, growth = 1, along = "iteration"),
                 "'along' dimension \\[\"iteration\"\\] has dimtype \"iteration\"")
    x <- Counts(array(0L, dim = c(0, 2), dimnames = list(time = character(), sex = c("f", "m"))),
                dimscales = c(time = "Intervals"))
    expect_error(extrapolate(x, labels = c(2000, 2005), growth = 1),
                 "cannot extrapolate along dimension \"time\" because dimension has length 0")
    x <- Counts(array(1:12,
                  dim = c(3, 4),
                  dimnames = list(age = c("0-4", "5-9", "10+"),
                  year = c(2000, 2005, 2010, 2015))))
    gr <- growth(x, along = "age", within = "year")
    expect_error(extrapolate(x, labels = c(2020, 2025), growth = gr),
                 "extrapolating along dimension \"year\" but 'growth' has dimension named \"year\"")
    expect_error(extrapolate(x, labels = c(2020, 2025), growth = 1:2),
                 "'growth' does not have length 1")
    expect_error(extrapolate(x, labels = c(2020, 2025), growth = "wrong"),
                 "'growth' has class \"character\"")
    expect_error(extrapolate(x, labels = c(2015, 2025)),
                 "extrapolated and existing points overlap")
    x <- Counts(array(1:12,
                  dim = c(3, 4),
                  dimnames = list(age = c("0-4", "5-9", "10+"),
                  period = c("2001-2005", "2006-2010", "2011-2015", "2016-2020"))))
    expect_error(extrapolate(x, labels = "2026-2030"),
                 "gap or overlap between extrapolated and existing intervals")
    expect_error(extrapolate(x, labels = "2006-2015"),
                 "gap or overlap between extrapolated and existing intervals")
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(year = 2000:2002, sex = c("f", "m"))),
                dimscales = c(year = "Intervals"))
    expect_error(extrapolate(x, labels = "wrong", along = "sex"),
                 "'along' dimension \\[\"sex\"\\] has dimscale \"Sexes\"")
})

test_that("hasRegularAgeTime works", {
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("Male", "Female"))))
    expect_true(hasRegularAgeTime(x))
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      year = c(2000, 2010))))
    expect_error(hasRegularAgeTime(x),
                 "age step \\[5\\] does not equal time step \\[10\\]")
    x <- Counts(array(1:8,
                      dim = c(4, 2),
                      dimnames = list(age = c("0", "1-4", "5-9", "10+"),
                      year = c(2000, 2005))))
    expect_error(hasRegularAgeTime(x),
                 "age steps unequal")
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg_orig = c("Region 1", "Region 2"),
                      reg_dest = c("Region 1", "Region 2"))))
    expect_true(hasRegularAgeTime(x))
    x <- Values(array(0, dim = 1, dimnames = list(time = 2001)),
                dimscales = c(time = "Intervals"))
    expect_true(hasRegularAgeTime(x))
    x <- Values(array(0, dim = 3, dimnames = list(time = c(2001, 2005, 2011))))
    expect_error(hasRegularAgeTime(x),
                 "time steps unequal")
    x <- Values(array(0, dim = c(1, 1), dimnames = list(time = "2000+", age = "<0")))
    expect_true(hasRegularAgeTime(x))
    x <- Values(array(0,
                      dim = c(2, 2),
                      dimnames = list(time = c("1991-2000", "2001+"), age = c("<0", "0-9"))))
    expect_true(hasRegularAgeTime(x))
    x <- Values(array(1,
                      dim = c(1, 2),
                      dimnames = list(time = "2001+", age = c("0-9", "10+"))))
    expect_true(hasRegularAgeTime(x))
    x <- Values(array(1,
                      dim = c(1, 1),
                      dimnames = list(time = "2001+", age = "10+")))
    expect_true(hasRegularAgeTime(x))
})

test_that("impute works", {
    ## additive
    x <- as.integer(rpois(n = 5*6*7, lambda = outer(1:5, outer(1:6, 1:7, "+"), "+")))
    x <- Counts(array(x, dim = 5:7, dimnames = list(a = 1:5, b = 1:6, c = 1:7)))
    is.missing <-  seq_len(length(x)) %in% sample(length(x), size = 50)
    x[is.missing] <- NA
    xi <- impute(x)
    expect_true(validObject(xi))
    expect_true(is.integer(xi))
    ## d <- data.frame(as.data.frame(xi, direction = "long"), is.missing)
    ## lattice:::xyplot(count ~ c | a + b, data = d, groups = is.missing, col = c("red", "black"))
    ## multiplicative
    x <- as.integer(rpois(n = 5*6*7, lambda = outer(1:5, outer(1:6, 1:7))))
    x <- Counts(array(x, dim = 5:7, dimnames = list(a = 1:5, b = 1:6, c = 1:7)))
    is.missing <-  seq_len(length(x)) %in% sample(length(x), size = 50)
    x[is.missing] <- NA
    xi <- impute(x, mult = TRUE)
    expect_true(validObject(xi))
    expect_true(is.integer(xi))
    ## d <- data.frame(as.data.frame(xi, direction = "long"), is.missing)
    ## lattice:::xyplot(count ~ c | a + b, data = d, groups = is.missing, col = c("red", "black"))
    ## additive - all a = 5 missing
    x <- as.integer(rpois(n = 5*6*7, lambda = outer(1:5, outer(1:6, 1:7, "+"), "+")))
    x <- Counts(array(x, dim = 5:7, dimnames = list(a = 1:5, b = 1:6, c = 1:7)))
    x[sample(length(x), size = 50)] <- NA
    x[5,,] <- NA
    is.missing <- as.logical(is.na(x))
    xi <- impute(x)
    expect_true(validObject(xi))
    expect_true(is.integer(xi))
    ## d <- data.frame(as.data.frame(xi, direction = "long"), is.missing)
    ## lattice:::xyplot(count ~ c | a + b, data = d, groups = is.missing, col = c("red", "black"))
    ## multiplicative, non.integer - all b = 6 missing
    x <- rpois(n = 5*6*7, lambda = outer(1:5, outer(1:6, 1:7))) + 0.1
    x <- Counts(array(x, dim = 5:7, dimnames = list(a = 1:5, b = 1:6, c = 1:7)))
    x[sample(length(x), size = 50)] <- NA
    x[,6,] <- NA
    is.missing <- as.logical(is.na(x))
    xi <- impute(x, mult = TRUE)
    expect_true(validObject(xi))
    expect_false(is.integer(xi))
    ## d <- data.frame(as.data.frame(xi, direction = "long"), is.missing)
    ## lattice:::xyplot(count ~ c | a + b, data = d, groups = is.missing, col = c("red", "black"))
    ## can cope with factors with only one level
    x <- Counts(array(0:1, dim = 2, dimnames = list(sex = c("m", "f"))))
    x[1] <- NA
    x <- impute(x)
    expect_true(!any(is.na(x)))
    expect_true(is.integer(x))
    ## no non-missing values
    expect_error(impute(Counts(array(as.numeric(NA), dim = 2, dimnames = list(sex = c("m", "f"))))),
                 "no non-missing values")
    ## if no missing values, returns object unchanged
    x <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("m", "f"))))
    expect_identical(impute(x), x)
    ## 'mult' has length 1
    x <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("m", "f"))))
    x[1] <- NA
    expect_error(impute(x, mult = c(FALSE, TRUE)),
                 "'mult' does not have length 1")
    ## 'mult' has type "logical"
    x <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("m", "f"))))
    x[1] <- NA
    expect_error(impute(x, mult = "FALSE"),
                 "'mult' does not have type \"logical\"")
    ## mult is not missing
    x <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("m", "f"))))
    x[1] <- NA
    expect_error(impute(x, mult = NA),
                 "'mult' is missing")
    ## all non-negative if mult is TRUE
    x <- Counts(array(c(-1, 1), dim = 2, dimnames = list(sex = c("m", "f"))))
    x[2] <- NA
    expect_error(impute(x, mult = TRUE),
                 "'mult' is TRUE but 'object' has negative values")
    ## at least one positive if mult is TRUE
    x <- Counts(array(c(NA, 0), dim = 2, dimnames = list(sex = c("m", "f"))))
    x[1] <- NA
    expect_error(impute(x, mult = TRUE),
                 "'mult' is TRUE but 'object' has no positive values")
    ## 'max' argument works
    x <- rpois(n = 5*6*7, lambda = outer(1:5, outer(1:6, 1:7))) + 0.1
    x <- Counts(array(x, dim = 5:7, dimnames = list(a = 1:5, b = 1:6, c = 1:7)))
    x[sample(length(x), size = 50)] <- NA
    x[,6,] <- NA
    is.missing <- as.logical(is.na(x))
    xi <- impute(x, mult = TRUE, max = 5)
    expect_true(validObject(xi))
    expect_true(all(xi[is.missing] <= 5))
    xi <- impute(x, mult = TRUE, max = rep(5, length(x)))
    expect_true(validObject(xi))
    expect_true(all(xi[is.missing] <= 5))
    expect_error(impute(x, mult = TRUE, max = "five"),
                 "'max' does not have type \"numeric\"")
    expect_error(impute(x, mult = TRUE, max = c(5, NA)),
                 "'max' has missing values")
})

test_that("intervalScore works when 'values' has iterations dimension", {
    ## 'truth' has same dimension as 'values'
    values <- Counts(array(rnorm(200),
                           dim = c(2, 100),
                           dimnames = list(sex = c("f", "m"),
                                           iter = 1:100)))
    truth <- Counts(array(rnorm(2),
                          dim = 2,
                          dimnames = list(sex = c("f", "m"))))
    alpha <- 0.1
    ans.obtained <- intervalScore(values = values,
                                  truth = truth,
                                  alpha = alpha)
    ul <- collapseIterations(values, prob = c(0.05, 0.95))
    u <- ul[,2]
    l <- ul[,1]
    ans.expected <- (u-l) + 20*(l-truth)*(truth<l) + 20*(truth-u)*(truth>u)
    expect_identical(ans.obtained, ans.expected)
    ## 'truth' needs to be collapsed
    values <- Counts(array(rnorm(200),
                           dim = c(2, 100),
                           dimnames = list(sex = c("f", "m"),
                                           iter = 1:100)))
    truth <- Counts(array(rnorm(4),
                          dim = c(2, 2),
                          dimnames = list(sex = c("f", "m"),
                                          reg = c("a", "b"))))
    alpha <- 0.1
    ans.obtained <- intervalScore(values = values,
                                  truth = truth,
                                  alpha = alpha)
    ul <- collapseIterations(values, prob = c(0.05, 0.95))
    u <- ul[,2]
    l <- ul[,1]
    tt <- collapseDimension(truth, dim = "reg")
    ans.expected <- (u-l) + 20*(l-tt)*(tt<l) + 20*(tt-u)*(tt>u)
    expect_identical(ans.obtained, ans.expected)
    ## 'values' has missing value
    values <- Counts(array(rnorm(200),
                           dim = c(2, 100),
                           dimnames = list(sex = c("f", "m"),
                                           iter = 1:100)))
    values[1] <- NA
    truth <- Counts(array(rnorm(2),
                          dim = 2,
                          dimnames = list(sex = c("f", "m"))))
    alpha <- 0.1
    ans.obtained <- intervalScore(values = values,
                                  truth = truth,
                                  alpha = alpha,
                                  na.rm = TRUE)
    ul <- collapseIterations(values, prob = c(0.05, 0.95), na.rm = TRUE)
    u <- ul[,2]
    l <- ul[,1]
    ans.expected <- (u-l) + 20*(l-truth)*(truth<l) + 20*(truth-u)*(truth>u)
    expect_identical(ans.obtained, ans.expected)
})

test_that("intervalScore works when 'values' has quantile dimension", {
    ## 'truth' has same dimension as 'values'
    values <- Counts(array(rnorm(200),
                           dim = c(2, 100),
                           dimnames = list(sex = c("f", "m"),
                                           iter = 1:100)))
    values <- collapseIterations(values, prob = c(0.1, 0.9))
    truth <- Counts(array(rnorm(2),
                          dim = 2,
                          dimnames = list(sex = c("f", "m"))))
    ans.obtained <- intervalScore(values = values,
                                  truth = truth)
    u <- values[,2]
    l <- values[,1]
    ans.expected <- (u-l) + 10*(l-truth)*(truth<l) + 10*(truth-u)*(truth>u)
    expect_identical(ans.obtained, ans.expected)
    ## 'truth' needs to be collapsed
    values <- Counts(array(rnorm(200),
                           dim = c(2, 100),
                           dimnames = list(sex = c("f", "m"),
                                           iter = 1:100)))
    values <- collapseIterations(values, prob = c(0.1, 0.9))
    truth <- Counts(array(rnorm(4),
                          dim = c(2, 2),
                          dimnames = list(sex = c("f", "m"),
                                          reg = c("a", "b"))))
    ans.obtained <- intervalScore(values = values,
                                  truth = truth)
    u <- values[,2]
    l <- values[,1]
    tt <- collapseDimension(truth, dim = "reg")
    ans.expected <- (u-l) + 10*(l-tt)*(tt<l) + 10*(tt-u)*(tt>u)
    expect_identical(ans.obtained, ans.expected)
})


test_that("intervalScore throws appropriate errors", {
    values <- Counts(array(rnorm(200),
                           dim = c(2, 100),
                           dimnames = list(sex = c("f", "m"),
                                           iter = 1:100)))
    truth <- Counts(array(rnorm(2),
                          dim = 2,
                          dimnames = list(sex = c("f", "m"))))
    alpha <- 0.1
    expect_error(intervalScore(values = truth,
                               truth = truth,
                               alpha = alpha),
                 "'values' does not have a dimension with dimtype \"iteration\" or \"quantile\"")
    values.wrong  <- Counts(array(rnorm(200),
                                  dim = c(2, 100),
                                  dimnames = list(reg = c("a", "n"),
                                                  iter = 1:100)))
    expect_error(intervalScore(values = values.wrong,
                               truth = truth,
                               alpha = alpha),
                 "'truth' and 'values' not compatible")
    expect_error(intervalScore(values = values,
                               truth = truth,
                               alpha = NULL),
                 "'values' has dimension with dimtype \"iteration\" but 'alpha' is NULL")
    expect_error(intervalScore(values = values,
                               truth = truth,
                               alpha = "1"),
                 "'alpha' is non-numeric")
    expect_error(intervalScore(values = values,
                               truth = truth,
                               alpha = c(0.1, 0.1)),
                 "'alpha' does not have length 1")
    expect_error(intervalScore(values = values,
                               truth = truth,
                               alpha = as.numeric(NA)),
                 "'alpha' is missing")
    expect_error(intervalScore(values = values,
                               truth = truth,
                               alpha = 0.5),
                 "'alpha' must be greater than 0 and less than 0.5")
    values.q <- collapseIterations(values,
                                   prob = c(0.1, 0.9))
    expect_warning(intervalScore(values = values.q,
                                 truth = truth,
                                 alpha = 0.5),
                   "'values' has dimension with dimtype \"quantile\" but 'alpha' is not NULL")
    values.wrong <- collapseIterations(values,
                                       prob = c(0.1, 0.5, 0.8))
    expect_error(intervalScore(values = values.wrong,
                               truth = truth),
                 "'values' does not have 2 quantiles")
    values.wrong <- collapseIterations(values,
                                       prob = c(0.1, 0.8))
    expect_error(intervalScore(values = values.wrong,
                               truth = truth),
                 "quantiles for 'values' not symmetric")
    values <- Counts(array(rnorm(200),
                           dim = c(2, 100),
                           dimnames = list(sex = c("f", "m"),
                                           iter = 1:100)))
    values[1] <- NA
    expect_error(intervalScore(values = values,
                               truth = truth,
                               alpha = 0.2,
                               na.rm = 1),
                 "'na.rm' does not have type \"logical\"")
    expect_error(intervalScore(values = values,
                               truth = truth,
                               alpha = 0.2,
                               na.rm = c(TRUE, FALSE)),
                 "'na.rm' does not have length 1")
    expect_error(intervalScore(values = values,
                               truth = truth,
                               alpha = 0.2,
                               na.rm = NA),
                 "'na.rm' is missing")
    expect_error(intervalScore(values = values,
                               truth = truth,
                               alpha = 0.2,
                               na.rm = FALSE),
                 "'na.rm' is FALSE but 'values' contains missing values")
})
    
test_that("limits works", {
    x <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          sex = c("Male", "Female"))))
    ans.obtained <- limits(x)
    ans.expected <- data.frame(age = c("0-4", "10+"),
                               sex = c("Male", "Female"),
                               row.names = c("first", "last"))
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(0,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          sex = NULL)))
    ans.obtained <- limits(x)
    ans.expected <- data.frame(age = c("0-4", "10+"),
                               sex = as.character(c(NA, NA)),
                               row.names = c("first", "last"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("mad works - no iterations", {
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(mad(x), mad(a))
    expect_identical(max(x, constant = 1.5), max(a, constant = 1.5))
    expect_identical(max(x, low = TRUE), max(a, low = TRUE))
    expect_identical(max(x, high = TRUE), max(a, high = TRUE))
    a <- array(0,
               dim = c(3, 0),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = NULL))
    x <- Counts(a)
    expect_identical(mad(x), mad(a))
    a <- array(c(NA, 1:5),
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(mad(x), mad(a))
    expect_identical(mad(x, na.rm = TRUE), mad(a, na.rm = TRUE))
    expect_identical(mad(x, na.rm = TRUE, center = 3), mad(a, na.rm = TRUE, center = 3))
    a <- array(0L,
               dim = c(0, 2),
               dimnames = list(age = character(),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(mad(x), mad(a))
})

test_that("mad works - with quantiles", {
    a <- array(1:18,
               dim = c(3, 2, 3),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   quantile = c("5%", "50%", "95%")))
    x <- Counts(a)
    expect_error(mad(x),
                 "'x' has dimension with dimtype \"quantile\"")
})

test_that("mad works - with iterations", {
    a <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   iter = 1:2))
    x <- Counts(a)
    ans.obtained <- mad(x)
    ans.expected <- apply(a, 3, mad)
    ans.expected <- CountsOne(ans.expected, labels = 1:2, name = "iter")
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- mad(x, constant = 1.5)
    ans.expected <- apply(a, 3, mad, constant = 1.5)
    ans.expected <- CountsOne(ans.expected, labels = 1:2, name = "iter")
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- mad(x, low = TRUE)
    ans.expected <- apply(a, 3, mad, low = TRUE)
    ans.expected <- CountsOne(ans.expected, labels = 1:2, name = "iter")
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- mad(x, high = TRUE)
    ans.expected <- apply(a, 3, mad, high = TRUE)
    ans.expected <- CountsOne(ans.expected, labels = 1:2, name = "iter")
    expect_identical(ans.obtained, ans.expected)
    a <- array(c(NA, 2:12),
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   iter = 1:2))
    x <- Counts(a)
    ans.obtained <- mad(x)
    ans.expected <- apply(a, 3, mad)
    ans.expected <- CountsOne(ans.expected, labels = 1:2, name = "iter")
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- mad(x, na.rm = TRUE)
    ans.expected <- apply(a, 3, mad, na.rm = TRUE)
    ans.expected <- CountsOne(ans.expected, labels = 1:2, name = "iter")
    expect_identical(ans.obtained, ans.expected)
})

test_that("mean method works - no iterations", {
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(mean(x), mean(a))
    a <- array(0,
               dim = c(3, 0),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = NULL))
    x <- Counts(a)
    expect_identical(mean(x), mean(a))
    a <- array(c(NA, 1:5),
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(mean(x), mean(a))
    expect_identical(mean(x, na.rm = TRUE), mean(a, na.rm = TRUE))
    expect_identical(mean(x, na.rm = TRUE, trim = 25), mean(a, na.rm = TRUE, trim = 25))
})

test_that("mean method works - iterations", {
    a <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   iteration = 1:2))
    x <- Counts(a)
    ans.obtained <- mean(x)
    ans.expected <- apply(a, 3, mean)
    ans.expected <- CountsOne(ans.expected, labels = 1:2, name = "iteration")
    expect_identical(ans.obtained, ans.expected)
    a <- array(c(NA, 2:12),
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   iteration = 1:2))
    x <- Counts(a)
    ans.obtained <- mean(x)
    ans.expected <- apply(a, 3, mean)
    ans.expected <- CountsOne(ans.expected, labels = 1:2, name = "iteration")
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- mean(x, na.rm = TRUE)
    ans.expected <- apply(a, 3, mean, na.rm = TRUE)
    ans.expected <- CountsOne(ans.expected, labels = 1:2, name = "iteration")
    expect_identical(ans.obtained, ans.expected)
})


test_that("median method works - no iterations", {
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(median(x), median(a))
    a <- array(0,
               dim = c(3, 0),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = NULL))
    x <- Counts(a)
    expect_identical(median(x), median(a))
    a <- array(c(NA, 1:5),
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(median(x), median(a))
    expect_identical(median(x, na.rm = TRUE), median(a, na.rm = TRUE))
})

test_that("median method works - quantiles", {
    a <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
               quantile = c(0.05, 0.95)))
    x <- Counts(a)
    expect_error(median(x),
                "'x' has dimension with dimtype \"quantile\"")
})

test_that("median method works - iterations", {
    a <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   iteration = 1:2))
    x <- Counts(a)
    ans.obtained <- median(x)
    ans.expected <- apply(a, 3, median)
    ans.expected <- CountsOne(ans.expected, labels = 1:2, name = "iteration")
    expect_identical(ans.obtained, ans.expected)
    a <- array(c(NA, 2:12),
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female"),
                   iteration = 1:2))
    x <- Counts(a)
    ans.obtained <- median(x)
    ans.expected <- apply(a, 3, median)
    ans.expected <- CountsOne(ans.expected, labels = 1:2, name = "iteration")
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- median(x, na.rm = TRUE)
    ans.expected <- apply(a, 3, median, na.rm = TRUE)
    ans.expected <- CountsOne(ans.expected, labels = 1:2, name = "iteration")
    expect_identical(ans.obtained, ans.expected)
})

test_that("metadata works", {
  metadata <- dembase:::metadata
  a <- array(1:12,
             dim = c(2, 2, 3),
             dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
  x <- Counts(a)
  expect_identical(metadata(x), x@metadata)
})

test_that("midpoints works", {
    a <- array(1:12,
               dim = c(2, 2, 3),
               dimnames = list(period = c("2001-2005", "2006-2010"),
                               sex = c("Male", "Female"),
                               age = c("0-4", "5-9", "10+")))
    b <- array(1:12,
               dim = c(2, 2, 3),
               dimnames = list(period = c("2002.5", "2007.5"),
                               sex = c("Male", "Female"),
                               age = c("2.5", "7.5", "12.5")))
    d <- array(1:12,
               dim = c(2, 2, 3),
               dimnames = list(period = c("2002.5", "2007.5"),
                               sex = c("Male", "Female"),
                               age = c("0-4", "5-9", "10+")))
    x <- Counts(a)
    y <- Counts(b)
    z <- Counts(d)
    expect_identical(midpoints(x, dimension = c(1, 3)), y)
    expect_identical(midpoints(x, dimension = c("age", "period")), y)
    expect_identical(midpoints(x), y)
    expect_identical(midpoints(x, dimension = "period"), z)
    expect_identical(midpoints(x, dimension = 1), z)
    x <- Counts(array(0, dim = 0, dimnames = list(age = NULL)))
    y <- Counts(array(0, dim = 0, dimnames = list(age = NULL)), dimscales = c(age = "Points"))
    expect_identical(midpoints(x), y)
    x <- Counts(array(0,
                      dim = c(2, 2),
                      dimnames = list(sex = c("f", "m"), region = c("a", "b"))))
    expect_identical(midpoints(x), x)
    expect_error(midpoints(x, dimension = "sex"),
                 sprintf("dimension %s does not have dimscale \"Intervals\"",
                         dQuote("sex")))
    expect_error(midpoints(x, dimension = c("sex", "region")),
                 sprintf("dimensions %s, %s do not have dimscale \"Intervals\"",
                         dQuote("sex"), dQuote("region")))
    x <- Counts(array(1:12,
                      dim = c(2, 2, 3),
                      dimnames = list(period = c("2001-2005", "2006-2010"),
                                      triangle = c("Lower", "Upper"),
                                      age = c("0-4", "5-9", "10+"))))
    ans.obtained <- midpoints(x)
    ans.expected <- Counts(array(1:12,
                                 dim = c(2, 2, 3),
                                 dimnames = list(period = c("2002.5", "2007.5"),
                                                 triangle = c("Lower", "Upper"),
                                                 age = c("2.5", "7.5", "12.5"))),
                           dimtypes = c(triangle = "state"),
                           dimscales = c(triangle = "Categories"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("nIteration works", {
    x <- Values(array(1:27,
                      dim = c(3, 3, 3),
                      dimnames = list(iteration = 1:3,
                      reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"))))
    expect_identical(nIteration(x), 3L)
    x <- Values(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    expect_error(nIteration(x),
                 "no dimension with dimtype \"iteration\"")
})

test_that("names methods work", {
  a <- array(1:6,
             dim = c(3, 2),
             dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
  x <- Counts(a)
  expect_that(names(x), is_identical_to(c("age", "sex")))
  names(x) <- toupper(names(x))
  expect_that(names(x), is_identical_to(c("AGE", "SEX")))
  expect_that(names(x) <- "wrong",
              throws_error("'dimtypes' and 'names' have different lengths"))
})

test_that("pairAligned works", {
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg_orig = 1:2,
                          reg_dest = 1:2)))
    expect_true(pairAligned(x))
    x <- Counts(array(1:6,
                      dim = 3:2,
                      dimnames = list(reg_orig = 1:3,
                          reg_dest = 1:2)))
    expect_error(pairAligned(x),
                 "dimensions \"reg_orig\" and \"reg_dest\" have different lengths")
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg_orig = 1:2,
                          reg_dest = 2:3)))
    expect_error(pairAligned(x),
                 "dimensions \"reg_orig\" and \"reg_dest\" have different categories")
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg_orig = 1:2,
                          reg_dest = 2:1)))
    expect_error(pairAligned(x),
                 "dimensions \"reg_orig\" and \"reg_dest\" have same categories, but in different order")
    x <- Counts(array(1:16,
                      dim = c(2, 2, 2, 2),
                      dimnames = list(reg_orig = 1:2,
                          reg_dest = 1:2,
                          eth_dest = 1:2,
                          eth_orig = 1:2)))
    expect_true(pairAligned(x))
    x <- Counts(array(1:16,
                      dim = c(2, 2, 2, 2),
                      dimnames = list(reg_orig = 1:2,
                          reg_dest = 2:1,
                          eth_dest = 1:2,
                          eth_orig = 1:2)))
    expect_true(pairAligned(x, base = "eth"))
})

test_that("perturb throws appropriate errors", {
    set.seed(100)
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    expect_error(perturb(x, n = 1:2),
                 "'n' does not have length 1")
    expect_error(perturb(x, order = NA),
                 "'order' is missing")
    expect_error(perturb(x, phi = 0.9),
                 "'phi' is less than 1")
})

test_that("prop.table works", {
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(prop.table(x, margin = 1),
                     Values(prop.table(a, margin = 1)))
    expect_error(prop.table(x, margin = "wrong"),
                 "'margin' outside valid range")
    expect_identical(prop.table(x, margin = c("sex", "age")),
                     Values(array(1,
                                  dim = c(3, 2),
                                  dimnames = list(age = c("0-4", "5-9", "10+"),
                                  sex = c("Male", "Female")))))
    a <- array(1:27,
               dim = c(3, 3, 3),
               dimnames = list(iteration = 1:3,
               reg_orig = c("a", "b", "c"),
               reg_dest = c("a", "b", "c")))
    x <- Counts(a)
    expect_identical(prop.table(x, margin = 1:2),
                     Values(prop.table(a, margin = 1:2)))
    expect_identical(prop.table(x, margin = "reg_dest"),
                     Values(prop.table(a, margin = c(1, 3))))
    x <- Values(array(1:3, dim = 3, dimnames = list(quantile = c("1%", "50%", "99%"))))
    expect_error(prop.table(x, margin = 1),
                 "dimension with dimtype \"quantile\"")
})

test_that("quantiles works", {
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(quantile(x), quantile(a))
    a <- array(0,
               dim = c(3, 0),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = NULL))
    x <- Counts(a)
    expect_identical(quantile(x), quantile(a))
    a <- array(c(NA, 1:5),
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(quantile(x, na.rm = TRUE), quantile(a, na.rm = TRUE))
})

test_that("resetIterations works", {
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          sex = c("Male", "Female"))))
    expect_identical(resetIterations(x), x)
    x0 <- Counts(array(1:6,
                       dim = c(3, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                           iteration = 2:3)))
    x1 <- Counts(array(1:6,
                       dim = c(3, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                           iteration = 1:2)))
    expect_identical(resetIterations(x0), x1)
})


test_that("rotateAgeTime works with age-time to age-cohort - no triangles", {
    x <- Counts(array(1:6,
                      dim = 2:3,
                      dimnames = list(age = 0:1,
                                      time = 2000:2002)),
                dimscale = c(age = "Intervals", time = "Points"))
    ans.obtained <- rotateAgeTime(x, to = "ac")
    ans.expected <- Counts(array(c(NA, 2L, 1L, 4L, 3L, 6L, 5L, NA),
                                 dim = c(2, 4),
                                 dimnames = list(age = 0:1,
                                                 cohort = 1999:2002)),
                           dimscale = c(age = "Intervals", cohort = "Intervals"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("slab works with numeric dimension and elements", {
    x <- Counts(array(rpois(n = 6, lambda = 5),
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    expect_identical(slab(x, dimension = 1, elements = 1:2),
                     x[1:2, ])
    expect_identical(slab(x, dimension = 2, elements = 2:1),
                     x[, 2:1])
    expect_identical(slab(x, dimension = 2, elements = 1, drop = FALSE),
                     x[, 1, drop = FALSE])
    expect_error(slab(x, dimension = 1:2, elements = 1),
                 "'dimension' does not have length 1")
    expect_error(slab(x, dimension = -1, elements = 1),
                 "'dimension' outside valid range")
    expect_error(slab(x, dimension = 1, elements = c(1, 1)),
                 "'elements' has duplicates")
    expect_error(slab(x, dimension = 1, elements = 1:4),
                 "'elements' outside valid range")
})

test_that("slab works with objects with single dimension", {
    x <- Counts(array(1:5, dim = 5, dimnames = list(reg = letters[1:5])))
    expect_identical(slab(x, dim = 1, elements = "e"), 5L)
    expect_identical(slab(x, dim = 1, elements = "e", drop = FALSE),
                     Counts(array(5L, dim = 1, dimnames = list(reg = "e"))))
})          

test_that("slab works with non-numeric dimension and elements", {
    x <- Counts(array(rpois(n = 6, lambda = 5),
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    expect_identical(slab(x, dimension = "age", elements = 1:2),
                     x[1:2, ])
    expect_identical(slab(x, dimension = 2, elements = c("m", "f")),
                     x[, 2:1])
    expect_error(slab(x, dimension = c("age", "sex"), elements = 1),
                 "'dimension' does not have length 1")
    expect_error(slab(x, dimension = "wrong", elements = 1),
                 "'dimension' outside valid range")
    expect_error(slab(x, dimension = 1, elements = c("0", "0")),
                 "'elements' has duplicates")
    expect_error(slab(x, dimension = 1, elements = c("0", "wrong")),
                 "'elements' outside valid range")
})

test_that("slab works with 0-length elements", {
    x <- Counts(array(rpois(n = 6, lambda = 5),
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    expect_identical(slab(x, dimension = "age", elements = integer()),
                     x[integer(), ])
    expect_identical(slab(x, dimension = 2, elements = character()),
                     x[, integer()])
})

test_that("logical drop argument for slab works correctly", {
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    ans.obtained <- slab(x, dimension = "age", elements = 1)
    ans.expected <- Counts(array(c(1L, 4L),
                                 dim = 2,
                                 dimnames = list(sex = c("f", "m"))))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- slab(x, dimension = "age", elements = 1, drop = FALSE)
    ans.expected <- Counts(array(c(1L, 4L),
                                 dim = c(1, 2),
                                 dimnames = list(age = 0, sex = c("f", "m"))))
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(age = character(), sex = c("f", "m"))))
    ans.obtained <- slab(x, dimension = "sex", elements = "f")
    ans.expected <- Counts(array(0,
                                 dim = 0,
                                 dimnames = list(age = character())))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- slab(x, dimension = "sex", elements = "f", drop = FALSE)
    ans.expected <- Counts(array(0,
                                 dim = c(0, 1),
                                 dimnames = list(age = character(), sex = "f")))
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(1:5,
                      dim = 5,
                      dimnames = list(age = 0:4)))
    ans.obtained <- slab(x, dimension = "age", elements = 1)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- slab(x, dimension = "age", elements = 1, drop = FALSE)
    ans.expected <- Counts(array(1L,
                                 dim = 1,
                                 dimnames = list(age = 0)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("'dimension' drop argument for slab works correctly", {
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    ans.obtained <- slab(x, dimension = "age", elements = 1, drop = "dimension")
    ans.expected <- Counts(array(c(1L, 4L),
                                 dim = 2,
                                 dimnames = list(sex = c("f", "m"))))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- slab(x, dimension = "age",
                         elements = 1:2,
                         drop = "dimension")
    ans.expected <- Counts(array(c(1:2, 4:5),
                                 dim = c(2, 2),
                                 dimnames = list(age = 0:1, sex = c("f", "m"))))
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(age = character(), sex = c("f", "m"))))
    ans.obtained <- slab(x, dimension = "sex", elements = "f", drop = "dimension")
    ans.expected <- Counts(array(0,
                                 dim = 0,
                                 dimnames = list(age = character())))
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(1:5,
                      dim = 5,
                      dimnames = list(age = 0:4)))
    ans.obtained <- slab(x, dimension = "age", elements = 1, drop = "dim")
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(1:6,
                      dim = c(3, 1, 2),
                      dimnames = list(age = 0:2, region = "a", sex = c("f", "m"))))
    ans.obtained <- slab(x, dimension = "sex", elements = 1, drop = "dimension")
    ans.expected <- Counts(array(1:3,
                                 dim = c(3, 1),
                                 dimnames = list(age = 0:2, region = "a")))
    expect_identical(ans.obtained, ans.expected)
})


test_that("replacement method for slab works with valid inputs", {
    ## character dimension; single numeric element
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    slab(x, dimension = "age", elements = 1) <- 99:100
    expect_identical(x[1,], c(f = 99L, m = 100L))
    expect_identical(x[2,], c(f = 2L, m = 5L))
    ## numeric dimension; single character element
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    slab(x, dimension = 2, elements = "m") <- 99:101
    expect_identical(x,
                     Counts(array(c(1:3, 99:101),
                                  dim = c(3, 2),
                                  dimnames = list(age = 0:2, sex = c("f", "m")))))
    ## character dimension; multiple numeric elements
    x <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, reg = 1:4)))
    slab(x, dimension = "reg", elements = c(1, 4)) <- NA
    expect_true(all(is.na(x) == (slice.index(x, 3) == 1) | (slice.index(x, 3) == 4)))
    ## all data replaced
    x <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, reg = 1:4)))
    slab(x, dimension = "sex", elements = 1:2) <- 24:1
    expect_identical(x,
                     Counts(array(24:1,
                                  dim = 2:4,
                                  dimnames = list(sex = c("f", "m"),
                                      age = 0:2, reg = 1:4))))
    ## object is Values; replacement value is Counts
    x <- Values(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, reg = 1:4)))
    y <- Counts(array(12:1,
                      dim = 3:4,
                      dimnames = list(age = 0:2, reg = 1:4)))
    slab(x, dimension = 1, elements = 1) <- y
    expect_identical(x,
                     Values(array(rbind(12:1, seq(2L,24L,2L)),
                                  dim = 2:4,
                                  dimnames = list(sex = c("f", "m"),
                                      age = 0:2, reg = 1:4))))
    ## elements and replacement value both have length 0
    x0 <- Values(array(1:24,
                       dim = 2:4,
                       dimnames = list(sex = c("f", "m"),
                           age = 0:2, reg = 1:4)))
    x1 <- x0
    slab(x1, dimension = 2, elements = integer()) <- integer()
    expect_identical(x1, x0)
    ## replication
    x <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, reg = 1:4)))
    slab(x, dimension = "reg", elements = c(1, 4)) <- 1:2
    y <- Counts(array(c(rep(1:2, 3), 7:18, rep(1:2, 3)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, reg = 1:4)))
    expect_identical(x, y)
})

test_that("replacement method for slab raises appropriate errors", {
    x <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"),
                          age = 0:2, reg = 1:4)))
    expect_error(slab(x, dimension = 1:2, elements = 1) <- 2,
                 "'dimension' does not have length 1")
    expect_error(slab(x, dimension = 1.1, elements = 1) <- 2,
                 "'dimension' outside valid range")
    expect_error(slab(x, dimension = 0, elements = 1) <- 2,
                 "'dimension' outside valid range")
    expect_error(slab(x, dimension = 1, elements = rep(1, 2)) <- 2,
                 "'elements' has duplicates")
    expect_error(slab(x, dimension = 1, elements = 0) <- 2,
                 "'elements' outside valid range")
    expect_warning(slab(x, dimension = 1, elements = 1, drop = TRUE) <- 2,
                   "'drop' argument ignored by replacement method for 'slab")
    expect_error(slab(x, dimension = 1, elements = 1) <- "2",
                 "replacement value is non-numeric")
    expect_error(slab(x, dimension = 1, elements = 1) <- 1:25,
                 "length of replacement value greater than length of slab")
    expect_error(slab(x, dimension = 1, elements = 1) <- integer(),
                 "replacement value has length 0")
    expect_error(slab(x, dimension = 1, elements = 1) <- 1:5,
                 "length of replacement value not multiple of length of slab")
})    

test_that("setAgeMax works", {
    x <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("Male", "Female"))))
    expect_identical(setAgeMax(x, value = 15),
                     Values(array(1:6,
                                  dim = c(3, 2),
                                  dimnames = list(age = c("0-4", "5-9", "10-14"),
                                                  sex = c("Male", "Female")))))
})

test_that("setAgeMin works", {
    x <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("Male", "Female"))))
    expect_identical(setAgeMin(x, value = -Inf),
                     Values(array(1:6,
                                  dim = c(3, 2),
                                  dimnames = list(age = c("<5", "5-9", "10+"),
                                                  sex = c("Male", "Female")))))
})

test_that("subarray works", {
    a <- array(1:12,
               dim = c(2, 2, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
    x <- Counts(a)
    expect_identical(subarray(x, age > 5),
                     Counts(a[,,2:3]))
    expect_identical(subarray(x, age > 5, drop = FALSE),
                     Counts(a[,,2:3, drop = FALSE]))
    expect_identical(subarray(x, region == "Region 1" & sex == "Female", drop = FALSE),
                     x["Region 1", "Female",,drop = FALSE])
    expect_identical(subarray(x, age < 12.5),
                     x[,,1:2])
    expect_identical(subarray(x, age %in% c("0-4", "10+")),
                     x[,,c(1, 3)])
    expect_identical(subarray(x, (age < 6 | age > 9) & sex != "Female"),
                     x[,1,c(1, 3)])
    expect_error(subarray(x, age < 5 | sex == "Female"),
                 "'|' operator applied to multiple dimensions")
    expect_error(subarray(x, age == 5),
                 "invalid use of '==' operator")
    x <- Counts(array(1:8,
                      dim = c(2, 2, 2),
                      dimnames = list(reg_orig = c("a", "b"),
                      reg_dest = c("a", "b"), sex = c("f", "m"))))
    y <- Counts(array(c(1L, 3L, 5L, 7L),
                      dim = c(2, 2),
                      dimnames = list(reg = c("a", "b"), sex = c("f", "m"))))
    expect_identical(subarray(x, reg_orig == "a"), y)
    x <- Counts(array(c(1:4, 2:1, 4:3),
                      dim = c(2, 2, 2),
                      dimnames = list(reg = c("a", "b"), age = c("0-4", "5+"), pool = c("Ins", "Outs"))))
    y <- Counts(array(c(1L, 3L, 2L, 4L),
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"), pool = c("Ins", "Outs"))))
    z <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg = c("a", "b"), age = c("0-4", "5+"))))
    expect_identical(subarray(x, reg == "a"), y)
    expect_identical(subarray(x, pool == "Ins"), z)
    x <- Values(array(1:6, dim = c(3,2), dimnames = list(age = c(0, 5, 10), sex = c("f", "m"))))
    expect_identical(subarray(x, age %in% c(0, 5)),
                     x[1:2,])
    expect_identical(subarray(x, age %in% c(0, 10)),
                     x[c(1, 3),])
    x <- Values(array(1:6,
                      dim = c(3,2),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    expect_identical(subarray(x, age %in% c(0, 5)),
                     x[FALSE, ])
    expect_identical(subarray(x, subarray = !(age > 5)),
                              x[1,])
    expect_identical(subarray(x, !(age < 5) & !(sex == "f")),
                     x[2:3, 2])
    expect_error(subarray(x, !((age < 5) & (sex == "f"))),
                          "attempt to apply '!' operator to expression involving more than one dimension")
})

test_that("t method for DemographicArray works", {
  a <- array(1:12,
             dim = c(2, 2, 3),
             dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
  x <- Counts(a)
  expect_that(t(x), throws_error("does not have 2 dimensions"))
  a <- array(1:8,
             dim = c(4, 2),
             dimnames = list(age = c("0", "1-4", "5-9", "10+"),
               year = c(2000, 2005)))
  x <- Counts(a)
  expect_that(dim(t(x)), is_identical_to(c(2L, 4L)))
  expect_that(t(t(x)), is_identical_to(x))
})

test_that("thinIterations works", {
    x <- Counts(array(1:6, dim = c(2, 3), dimnames = list(sex = c("f", "m"), iteration = 1:3)))
    y <- Counts(array(1:6, dim = c(2, 3), dimnames = list(sex = c("f", "m"), age = 0:2)))
    expect_identical(dimnames(thinIterations(x, n = 2)), dimnames(x[,1:2]))
    expect_error(thinIterations(y, n = 1),
                 "'object' does not have a dimension with dimtype \"iteration\"")
    expect_error(thinIterations(x, n = 1:2),
                 "'n' does not have length 1")
    expect_error(thinIterations(x, n = "1"),
                 "'n' does not have type \"numeric\"")
    expect_error(thinIterations(x, n = as.numeric(NA)),
                 "'n' is missing")
    expect_error(thinIterations(x, n = 1.1),
                 "'n' is not an integer")
    expect_error(thinIterations(x, n = 0),
                 "'n' is less than 1")
})

test_that("toDouble works", {
    a <- array(1:12,
               dim = c(2, 2, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
    x <- Counts(a)
    b <- array(as.numeric(1:12),
               dim = c(2, 2, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
    y <- Counts(b)
    expect_identical(toDouble(x), y)
    expect_identical(toDouble(y), y)
})

test_that("toInteger works", {
    a <- array(1:12,
               dim = c(2, 2, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
    x <- Counts(a)
    b <- array(as.numeric(1:12),
               dim = c(2, 2, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
    y <- Counts(b)
    expect_identical(toInteger(x), x)
    expect_identical(toInteger(y), x)
    a <- array(c(1:11, NA),
               dim = c(2, 2, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
    x <- Counts(a)
    b <- array(c(as.numeric(1:11), NA),
               dim = c(2, 2, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
    y <- Counts(b)
    expect_identical(toInteger(x), x)
    expect_identical(toInteger(y), x)
    a <- array(c(1:10, 11.1, NA),
               dim = c(2, 2, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
    x <- Counts(a)
    b <- array(c(1:11, NA),
               dim = c(2, 2, 3),
               dimnames = list(region = c("Region 1", "Region 2"),
               sex = c("Male", "Female"),
               age = c("0-4", "5-9", "10+")))
    y <- Counts(b)
    expect_error(toInteger(x),
                 "non-integer values")
    expect_identical(toInteger(x, force = TRUE), y)
})

test_that("valueInInterval works when value is a demographic array", {
    value <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.05", "0.95"),
                                             sex = c("F", "M"),
                                             region = 2:1)))
    ans.obtained <- valueInInterval(value = value,
                                    interval = interval)
    ans.expected <- array(c(TRUE, FALSE, TRUE, FALSE),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M")))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- valueInInterval(value = value,
                                    interval = aperm(interval, 3:1))
    ans.expected <- array(c(TRUE, FALSE, TRUE, FALSE),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M")))
    expect_identical(ans.obtained, ans.expected)
    value <- Values(array(1:2,
                          dim = 2,
                          dimnames = list(sex = c("F", "M"))))
    interval <- Counts(array(1:2,
                             dim = c(2, 2),
                             dimnames = list(quantile = c("0.05", "0.95"),
                                             sex = c("F", "M"))))
    ans.obtained <- valueInInterval(value = value,
                                    interval = interval)
    ans.expected <- array(c(TRUE, TRUE),
                          dim = 2,
                          dimnames = list(sex = c("F", "M")))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- valueInInterval(value = value,
                                    interval = interval,
                                    lower.inclusive = FALSE)
    ans.expected <- array(c(FALSE, TRUE),
                          dim = 2,
                          dimnames = list(sex = c("F", "M")))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- valueInInterval(value = value,
                                    interval = interval,
                                    lower.inclusive = FALSE,
                                    upper.inclusive = FALSE)
    ans.expected <- array(c(FALSE, FALSE),
                          dim = 2,
                          dimnames = list(sex = c("F", "M")))
    expect_identical(ans.obtained, ans.expected)
})

test_that("valueInInterval throws appropriate errors when value is a demographic array", {
    value <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(quantile = c("5%", "90%"),
                                          sex = c("F", "M"))))
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.05", "0.95"),
                                             sex = c("F", "M"),
                                             region = 1:2)))
    expect_error(valueInInterval(value = value,
                                 interval = interval),
                 "'value' has dimension with dimtype \"quantile\"")
    value <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c(2000, 2005),
                                             sex = c("F", "M"),
                                             region = 1:2)))
    expect_error(valueInInterval(value = value,
                                 interval = interval),
                 "'interval' does not have dimension with dimtype \"quantile\"")
    value <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    interval <- Counts(array(0:1,
                             dim = c(3, 2, 2),
                             dimnames = list(quantile = c("0.025", "0.5", "0.975"),
                                             sex = c("F", "M"),
                                             region = 1:2)))
    expect_error(valueInInterval(value = value,
                                 interval = interval),
                 "dimension of 'interval' with dimtype \"quantile\" does not have length 2")
    value <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.025", "0.975"),
                                             sex = c("F", "M"),
                                             wrong = 1:2)))
    expect_error(valueInInterval(value = value,
                                 interval = interval),
                 "'value' and 'interval' have incompatible dimensions : \"region\", \"sex\" vs \"quantile\", \"sex\", \"wrong\"")
    value <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(3, 2),
                          dimnames = list(region = 1:3,
                                          sex = c("F", "M"))))
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.025", "0.975"),
                                             sex = c("F", "M"),
                                             region = 1:2)))
    expect_error(valueInInterval(value = value,
                                 interval = interval),
                 "\"region\" dimensions of 'value' and 'interval' have different lengths")
    value <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.025", "0.975"),
                                            sex = c("Female", "Male"),
                                             region = 1:2)))
    expect_error(valueInInterval(value = value,
                                 interval = interval),
                 "dimscales for \"sex\" dimension of 'value' and 'interval' incompatible")
})




test_that("unname works", {
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"), sex = c("f", "m"))))
    expect_identical(unname(x), unname(as(x, "array")))
})









