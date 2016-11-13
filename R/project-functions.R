
## HAS_TESTS
## assume 'sex' and 'initial' valid
checkDominant <- function(dominant, sex, initial) {
    if (is.null(dominant)) {
        if (is.null(sex))
            NULL
        else {
            i.sex <- match(sex, names(initial))
            n.sex <- dim(initial)[i.sex]
            if (n.sex == 1L)
                NULL
            else
                stop(gettextf("'%s' is %s, but '%s' dimension has length %d",
                              "dominant", "NULL", "sex", n.sex))
        }
    }
    else {
        if (!is.character(dominant))
            stop(gettextf("'%s' does not have type \"%s\"",
                          "dominant", "character"))
        if (!identical(length(dominant), 1L))
            stop(gettextf("'%s' does not have length %d",
                          "dominant", 1L))
        if (is.na(dominant))
            stop(gettextf("'%s' is missing",
                          "dominant"))
        sex.labels <- dimnames(initial)[[sex]]
        if (!(dominant %in% sex.labels)) {
            stop(gettextf("'%s' dimension of '%s' does not have category specified by '%s'",
                          "sex", "initial", "dominant"))
        }
        NULL
    }
}

## test_that("checkDominant works", {
##     checkDominant <- dembase:::checkDominant
##     initial <- Counts(array(1:6, 
##                             dim = 3:2,
##                             dimnames = list(age = 0:2, sex = c("f", "m"))))
##     expect_identical(checkDominant(dominant = NULL, sex = NULL, initial = initial), 
##                      NULL)
##     expect_identical(checkDominant(dominant = NULL, sex = "sex", 
##                                    initial = subarray(initial, sex == "1", drop = FALSE)), 
##                      NULL)
##     expect_identical(checkDominant(dominant = "1", sex = "sex", initial),
##                      NULL)
##     expect_error(checkDominant(dominant = 1, sex = "sex", initial = initial),
##                  "'dominant' does not have type \"character\"")
##     expect_error(checkDominant(dominant = c("Male", "Female"), sex = "sex", initial = initial),
##                  "'dominant' does not have length 1")
##     expect_error(checkDominant(dominant = as.character(NA), sex = "sex", initial = initial),
##                  "'dominant' is missing")
##     expect_error(checkDominant(dominant = "wrong", sex = "sex", initial = initial),
##                  "'sex' dimension of 'initial' does not have category specified by 'dominant'")
## })

## project <- function(initial, birth = NULL, death = NULL,
##                     externalIn = NULL, externalOut = NULL,
##                     internalIn = NULL, internalOut = NULL,
##                     n = NULL, countsModel = FALSE,
##                     sex = "sex", dominant = "Female",
##                     internalDims = NULL, maxAttempt = 1000L) {
##     initial <- checkAndTidyInitial(initial)
##     checkSex(sex = sex, initial = initial)
##     checkDominant(dominant = dominant, sex = sex, initial = initial)
##     checkInternalDims(internalDims = internalDims,
##                       initial = initial,
##                       internalIn = internalIn)
##     param <- checkAndTidyParam(birth = birth,
##                                death = death,
##                                externalIn = externalIn,
##                                externalOut = externalOut,
##                                internalIn = internalIn,
##                                internalOut = internalOut)
##     l <- checkAndTidyIterationsProject(initial = initial, param = param, n = n)
##     initial <- l$initial
##     param <- l$param
##     project.forward <- makeProjectForward(initial = initial, param = param)
##     param <- makeParamCompatibleWithInitial(param = param, initial = initial)
##     population <- makePopulationObj(initial = initial, param = param)
##     names.param <- names(param)
##     has.births <- "birth" %in% names.param
##     if (has.births)
##         births <- convertToCountsObj(param$birth)
##     has.deaths <- "death" %in% names.param
##     if (has.deaths)
##         deaths <- convertToCountsObj(param$death)
##     has.internal <- "internalIn" %in% names.param
##     if (has.internal) {
##         internalIns <- convertToCountsObj(param$internalIn)
##         internalOuts <- convertToCountsObj(param$internalOut)
##     }
##     has.external <- "externalIn" %in% names.param
##     if (has.external) {
##         externalIns <- convertToCountsObj(param$externalIn)
##         externalOuts <- convertToCountsObj(param$externalOut)
##     }
##     i.age <- match("age", dimtypes(population), nomatch = 0L)
##     has.age <- i.age > 0L
##     i.time <- match("time", dimtypes(population))
##     n.time <- dim(population)[i.time]
##     step <- ageTimeStep(population)
##     popn.tmp <- initial
##     if (project.forward) {
##         slab(population, dimension = i.time, elements = 1L) <- initial
##         for (i in seq_len(n.time - 1L)) {
##             ## extract demographic rates and probabilitites for period
##             if (has.births) {
##                 birth.tmp <- slab(param$birth,
##                                    dimension = i.time,
##                                    elements = i)
##             }
##             if (has.deaths) {
##                 death.tmp <- slab(param$death,
##                                    dimension = i.time,
##                                    elements = i)
##             }
##             if (has.internal) {
##                 internal.in.tmp <- slab(param$internalIn,
##                                          dimension = i.time,
##                                          elements = i)
##                 internal.out.tmp <- slab(param$internalOut,
##                                           dimension = i.time,
##                                           elements = i)
##             }
##             if (has.external) {
##                 external.in.tmp <- slab(param$externalIn,
##                                          dimension = i.time,
##                                          elements = i)
##                 external.out.tmp <- slab(param$externalOut,
##                                           dimension = i.time,
##                                           elements = i)
##             }
##             ## make first increment of external migration
##             if (has.external) {
##                 l <- makeExternal(externalIn = external.in.tmp,
##                                   externalOut = external.out.tmp,
##                                   population = popn.tmp,
##                                   step = step,
##                                   maxAttempt = maxAttempt)
##                 slab(externalIns, dimension = i.time, elements = i) <- l$externalIns
##                 slab(externalOuts, dimension = i.time, elements = i) <- l$externalOuts
##             }
##             ## adjust population for external migration
##             if (has.external)
##                 popn.tmp <- popn.tmp + l$externalNet
##             ## make first increment of deaths
##             if (has.deaths) {
##                 deaths.tmp <- makeDeaths(death = death.tmp,
##                                          population = popn.tmp,
##                                          upper = TRUE,
##                                          step = step)
##                 slab(deaths, dimension = i.time, elements = i) <- deaths.tmp
##             }
##             ## adjust population for deaths
##             if (has.deaths)
##                 popn.tmp <- popn.tmp - deaths.tmp
##             ## make first increment of births
##             if (has.births) {
##                 births.tmp <- makeBirths(birth = birth.tmp,
##                                          population = popn.tmp,
##                                          step = step,
##                                          sex = sex,
##                                          dominant = dominant)
##                 slab(births, dimension = i.time, elements = i) <- births.tmp
##             }
##             ## age forward
##             if (has.age)
##                 popn.tmp <- ageForward(population = popn.tmp)
##             ## adjust population for births
##             if (has.births) {
##                 if (has.age) {
##                     births.tmp <- collapseDimension(births.tmp, dimension = "age")
##                     slab(popn.tmp, dimension = i.age, elements = 1L) <- births.tmp
##                 }
##                 else
##                     popn.tmp <- popn.tmp + births.tmp
##             }
##             ## make internal migration
##             if (has.internal) {
##                 l <- makeInternal(internalIn = internal.in.tmp,
##                                   internalOut = internal.out.tmp,
##                                   population = popn.tmp,
##                                   countsModel = countsModel,
##                                   internalDims = internalDims)
##                 slab(internalIns, dimension = i.time, elements = i) <- l$internalIns
##                 slab(internalOuts, dimension = i.time, elements = i) <- l$internalOuts
##             }
##             ## adjust population for internal migration
##             if (has.internal)
##                 popn.tmp <- popn.tmp + l$internalNet
##             ## make second increment of births
##             if (has.births) {
##                 births.tmp <- makeBirths(birth = birth.tmp,
##                                          population = popn.tmp,
##                                          step = step,
##                                          sex = sex,
##                                          dominant = dominant)
##                 slab(births, dimension = i.time, elements = i) <-
##                     slab(births, dimension = i.time, elements = i) + births.tmp
##             }
##             ## adjust population for births
##             if (has.births) {
##                 if (has.age) {
##                     births.tmp <- collapseDimension(births.tmp, dimension = "age")
##                     slab(popn.tmp, dimension = i.age, elements = 1L) <- 
##                         slab(popn.tmp, dimension = i.age, elements = 1L) + births.tmp
##                 }
##                 else
##                     popn.tmp <- popn.tmp + births.tmp
##             }
##             ## make second increment of deaths
##             if (has.deaths) {
##                 deaths.tmp <- makeDeaths(death = death.tmp,
##                                          population = popn.tmp,
##                                          upper = FALSE,
##                                          step = step)
##                 slab(deaths, dimension = i.time, elements = i) <-
##                     slab(deaths, dimension = i.time, elements = i) + deaths.tmp
##             }
##             ## adjust population for deaths
##             if (has.deaths)
##                 popn.tmp <- popn.tmp - deaths.tmp
##             ## make second increment of external migration
##             if (has.external) {
##                 l <- makeExternal(externalIn = external.in.tmp,
##                                   externalOut = external.out.tmp,
##                                   population = popn.tmp,
##                                   step = step,
##                                   maxAttempt = maxAttempt)
##                 slab(externalIns, dimension = i.time, elements = i) <-
##                     slab(externalIns, dimension = i.time, elements = i) +
##                         l$externalIns
##                 slab(externalOuts, dimension = i.time, elements = i) <- 
##                     slab(externalOuts, dimension = i.time, elements = i) + 
##                         l$externalOuts
##             }
##             ## adjust population for external migration
##             if (has.external)
##                 popn.tmp <- popn.tmp + l$externalNet
##             ## record population
##             slab(population, dimension = i.time, elements = i + 1L) <- popn.tmp
##         }
##     }
##     else {
##         stop("backward projections not implemented yet")
##     }
##     ## make return value
##     ans <- list(population = population)
##     if (has.births)
##         ans <- c(ans, list(births = births))
##     if (has.deaths)
##         ans <- c(ans, list(deaths = deaths))
##     if (has.external)
##         ans <- c(ans, list(externalIns = externalIns, externalOuts = externalOuts))
##     if (has.internal)
##         ans <- c(ans, list(internalIns = internalIns, internalOuts = internalOuts))
##     ans
## }


## randomPermute <- function(n) {
##     ans <- seq_len(n)
##     for (t in seq.int(from = n, to = 1L)) {
##         S <- as.integer(runif(n = 1L) * n)
##         if (S < n)
##             S <- S + 1L
##         tmp <- ans[t]
##         ans[t] <- ans[S]
##         ans[S] <- tmp
##     }
##     ans
## }
