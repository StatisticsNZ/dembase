
context("Population-methods")

test_that("midpoints works with Population", {
    Population <- dembase:::Population
    population <- Counts(array(1:6,
                              dim = c(3, 2),
                              dimnames = list(age = c("0-4", "5-9", "10+"),
                                  time = c("2000", "2005"))))
    population <- Population(population)
    ans.obtained <- midpoints(population)
    ans.expected <- midpoints(as(population, "Counts"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- midpoints(population, dimension = "age")
    ans.expected <- midpoints(as(population, "Counts"), dimension = "age")
    expect_identical(ans.obtained, ans.expected)
})
