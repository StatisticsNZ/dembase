
context("Population-generator")

test_that("Population works", {
    Population <- dembase:::Population
    object <- Counts(array(as.numeric(1:18),
                           dim = c(3, 2, 3),
                           dimnames = list(age = c(0,1,"2+"),
                               sex = c("f", "m"),
                               time = 2000:2002)),
                     dimscales = c(time = "Points"))
    ans.obtained <- Population(object)
    ans.expected <- new("Population", toInteger(object))
    expect_identical(ans.obtained, ans.expected)
    ## 'object' does not have class "Counts"
    object <- Values(array(1:18,
                           dim = c(3, 2, 3),
                           dimnames = list(age = c(0,1,"2+"),
                               sex = c("f", "m"),
                               time = 2000:2002)),
                     dimscales = c(time = "Points"))
    expect_error(Population(object),
                 "'population' has class \"Values\"")
    ## 'object' has non-integer values
    object <- Counts(array(c(1:17, 1.1),
                           dim = c(3, 2, 3),
                           dimnames = list(age = c(0,1,"2+"),
                               sex = c("f", "m"),
                               time = 2000:2002)),
                     dimscales = c(time = "Points"))
    expect_error(Population(object),
                 "'population' invalid : non-integer values")
    ## time, age, cohort dimensions
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c(0,1,"2+"),
                          sex = c("f", "m"),
                          iteration = 1:3)))
    expect_error(Population(x),
                 "'population' does not have dimension with dimtype \"time\"")
    x <- Counts(array(1L,
                      dim = c(2, 2, 2),
                      dimnames = list(time = c("2000-2005", "2005-2010"),
                          reg = c("a", "b"),
                          age = c("0-4", "5+"))))
    expect_error(Population(x),
                 "dimension of 'population' with dimtype \"time\" has dimscale \"Intervals\"")
    x <- Counts(array(1:6,
                      dim = c(1, 2, 2),
                      dimnames = list(time = "2000.5",
                          reg = c("a", "b"),
                          age = c("0-4", "5+"))),
                     dimscales = c(time = "Points"))
    expect_error(Population(x),
                 "dimension of 'population' with dimtype \"time\" has length 1")
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c(0, 5, 10),
                          sex = c("f", "m"),
                          time = c(2000, 2005, 2010))))
    expect_error(Population(x),
                 "dimension of 'population' with dimtype \"age\" has dimscale \"Points\"")
    x <- Counts(array(1:6,
                      dim = c(1, 2, 3),
                      dimnames = list(age = "0+",
                          sex = c("f", "m"),
                          time = 2000:2002)),
                     dimscales = c(time = "Points"))
    expect_error(Population(x),
                 "dimension of 'population' with dimtype \"age\" has length 1")
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c("<5", "5-9", "10+"),
                          sex = c("f", "m"),
                          time = c(2000, 2005, 2010))))
    expect_error(Population(x),
                 "'population' invalid : first interval of dimension with dimtype \"age\" is open")
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c("0-4", "5-9", "10-14"),
                          sex = c("f", "m"),
                          time = c(2000, 2005, 2010))))
    expect_error(Population(x),
                 "'population' invalid : last interval of dimension with dimtype \"age\" is closed")
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(cohort = c(0,1,2),
                          sex = c("f", "m"),
                          time = c(0, 1, 2))),
                     dimscales = c(time = "Points"))
    expect_error(Population(x),
                 "'population' has dimension with dimtype \"cohort\"")
    ## origin-destination
    x <- Counts(array(1:12,
                      dim = c(3, 2, 2),
                      dimnames = list(time = 0:2,
                          reg_orig = c("a", "b"),
                          reg_dest = c("a", "b"))),
                     dimscales = c(time = "Points"))
    expect_error(Population(x),
                 "has dimensions with dimtypes \"origin\" and \"destination\"")
    ## parent-child
    x <- Counts(array(1:12,
                      dim = c(3, 2, 2),
                      dimnames = list(time = 0:2,
                          reg_parent = c("a", "b"),
                          reg_child = c("a", "b"))),
                     dimscales = c(time = "Points"))
    expect_error(Population(x),
                 "has dimensions with dimtypes \"parent\" and \"child\"")
    ## regular
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(age = c(0,1,"2+"),
                          sex = c("f", "m"),
                          time = c(0, 1, 3))))
    expect_error(Population(x),
                 "'population' does not have regular age-time plan")
    ## positive length
    x <- Counts(array(0,
                      dim = c(3, 0, 3),
                      dimnames = list(age = c(0,1,"2+"),
                          sex = character(),
                          time = 0:2)),
                     dimscales = c(time = "Points"))
    expect_error(Population(x),
                 "'population' has length 0")
    object <- Counts(array(c(1:17, -1),
                           dim = c(3, 2, 3),
                           dimnames = list(age = c(0,1,"2+"),
                               sex = c("f", "m"),
                               time = 2000:2002)),
                     dimscales = c(time = "Points"))
    expect_error(Population(object),
                 "'population' has negative values")
})
