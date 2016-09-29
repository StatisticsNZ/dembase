
## context("project-function")
## n.test <- 5
## test.identity <- FALSE


## test_that("project works", {
##     ## only accession
##     initial <- Counts(array(1:12,
##                             dim = 4:3,
##                             dimnames = list(age = 0:3, reg = 1:3)))
##     death <- Values(array(0,
##                           dim = c(4, 3, 5),
##                           dimnames = list(age = 0:3, reg = 1:3, time = 2000:2004)))
##     ans.obtained <- project(initial = initial, death = death,
##                             sex = NULL, dominant = NULL, n = 2)

##     initial <- Counts(array(1:12,
##                             dim = 4:3,
##                             dimnames = list(age = c(0:2, "3+"), reg = 1:3)))
##     death <- Values(array(0,
##                           dim = c(4, 3, 5),
##                           dimnames = list(age = c(0:2, "3+"), reg = 1:3,
##                               time = 2000:2004)))
##     ans.obtained <- project(initial = initial, death = death,
##                             sex = NULL, dominant = NULL)
## }

## context("project-function")
## n.test <- 5
## test.identity <- FALSE


## test_that("project works", {
##     ## only accession
##     initial <- Counts(array(1:12,
##                             dim = 4:3,
##                             dimnames = list(age = 0:3, reg = 1:3)))
##     death <- Values(array(0,
##                           dim = c(4, 3, 5),
##                           dimnames = list(age = 0:3, reg = 1:3, time = 2000:2004)))
##     ans.obtained <- project(initial = initial, death = death,
##                             sex = NULL, dominant = NULL)

##     initial <- Counts(array(1:12,
##                             dim = 4:3,
##                             dimnames = list(age = c(0:2, "3+"), reg = 1:3)))
##     death <- Values(array(0,
##                           dim = c(4, 3, 5),
##                           dimnames = list(age = c(0:2, "3+"), reg = 1:3,
##                               time = 2000:2004)))
##     ans.obtained <- project(initial = initial, death = death,
##                             sex = NULL, dominant = NULL)


    
##     initial <- Counts(array(rpois(n = 12, lambda = 100),
##                             dim = 4:3,
##                             dimnames = list(age = c(0:2, "3+"), reg = 1:3)))
##     death <- Values(array(runif(n = 60, max = 0.2),
##                           dim = c(4, 3, 5),
##                           dimnames = list(age = c(0:2, "3+"), reg = 1:3, time = 2000:2004)))
##     birth <- Values(array(runif(n = 30, max = 0.5),
##                           dim = c(2, 3, 5),
##                           dimnames = list(age = 1:2, reg = 1:3, time = 2000:2004)))    
##     internalIn <- Values(array(runif(n = 60, max = 0.2),
##                                dim = c(4, 3, 5),
##                                dimnames = list(age = c(0:2, "3+"), reg = 1:3, time = 2000:2004)))
##     internalOut <- Values(array(runif(n = 60, max = 0.2),
##                                 dim = c(4, 3, 5),
##                                 dimnames = list(age = c(0:2, "3+"), reg = 1:3, time = 2000:2004)))
##     externalIn <- Values(array(runif(n = 60, max = 0.2),
##                                dim = c(4, 3, 5),
##                                dimnames = list(age = c(0:2, "3+"), reg = 1:3, time = 2000:2004)))
##     externalOut <- Values(array(runif(n = 60, max = 0.2),
##                                 dim = c(4, 3, 5),
##                                 dimnames = list(age = c(0:2, "3+"), reg = 1:3, time = 2000:2004)))
    
##     ans.obtained <- project(initial = initial,
##                             birth = birth,
##                             death = death,
##                             internalIn = internalIn,
##                             internalOut = internalOut,
##                             externalIn = externalIn,
##                             externalOut = externalOut,
##                             sex = NULL, 
##                             dominant = NULL,
##                             internalDims = "reg")
    
    
    
##     initial <- Counts(array(rpois(n = 24, lambda = 100),
##                             dim = 4:2,
##                             dimnames = list(age = c(0:2, "3+"),
##                                 reg = 1:3,
##                                 sex = c("f", "m"))))
##     death <- Values(array(runif(n = 60, max = 0.2),
##                           dim = c(4, 3, 5),
##                           dimnames = list(age = c(0:2, "3+"), reg = 1:3, time = 2000:2004)))
##     birth <- Values(array(runif(n = 60, max = 0.5),
##                           dim = c(2, 3, 2, 5),
##                           dimnames = list(age = 1:2, reg = 1:3, sex = c("f", "m"),
##                               time = 2000:2004)))    
##     internalIn <- Values(array(runif(n = 60, max = 0.2),
##                                dim = c(4, 3, 5),
##                                dimnames = list(age = c(0:2, "3+"), reg = 1:3, time = 2000:2004)))
##     internalOut <- Values(array(runif(n = 60, max = 0.2),
##                                 dim = c(4, 3, 5),
##                                 dimnames = list(age = c(0:2, "3+"), reg = 1:3, time = 2000:2004)))
##     externalIn <- Values(array(runif(n = 60, max = 0.2),
##                                dim = c(4, 3, 5),
##                                dimnames = list(age = c(0:2, "3+"), reg = 1:3, time = 2000:2004)))
##     externalOut <- Values(array(runif(n = 60, max = 0.2),
##                                 dim = c(4, 3, 5),
##                                 dimnames = list(age = c(0:2, "3+"), reg = 1:3, time = 2000:2004)))
##         ans.obtained <- project(initial = initial,
##                             birth = birth,
##                             death = death,
##                             internalIn = internalIn,
##                             internalOut = internalOut,
##                             externalIn = externalIn,
##                             externalOut = externalOut,
##                             sex = "sex", 
##                             dominant = "f",
##                             internalDims = "reg")
