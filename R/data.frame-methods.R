

dtabs <- function(data, formula, fill = 0L) {
    if (missing(data))
        stop(gettextf("'%s' is missing with no default",
                      "data"))
    if (is.matrix(data))
        data <- as.data.frame(data)
    if (!is.data.frame(data))
        stop(gettextf("'%s' is not a %s",
                      "data", "data.frame"))
    if (missing(formula))
        stop(gettextf("'%s' is missing with no default",
                      "formula"))
    if (!methods::is(formula, "formula"))
        stop(gettextf("'%s' is not a formula",
                      "formula"))
    if (!identical(length(fill), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "fill", 1L))
    call <- match.call()
    call$fill <- NULL
    call[[1L]] <- quote(stats::model.frame)
    values <- eval(call)
    has.response <- length(formula) > 2L
    if (has.response) {
        terms <- terms(values)
        i.response <- attr(terms,  "response")
        n.response <- length(i.response)
        if (n.response > 1L)
            stop(gettextf("formula '%s' contains more than one response variable",
                          deparse(formula)))
        INDEX <- values[-i.response]
        X <- values[[i.response]]
    }
    else {
        INDEX <- values
        X <- rep(1L, nrow(data))
    }
    tapply(X = X,
           INDEX = INDEX,
           FUN = sum,
           na.rm = FALSE,
           default = fill,
           simplify = TRUE)
}


## d <- data.frame(y = 1:20, f1 = rep(1:10, times = 2), f2 = rep(c("a", "b"), each = 10))
## formula <- y ~ f1 + f2
## ans.obtained <- dtabs(d, formula)
## ans.expected <- as(xtabs(formula, d), "array")
## expect_identical(ans.obtained, ans.expected)
## formula <-  ~ f1 + f2
## ans.obtained <- dtabs(d, formula)
## ans.expected <- as(xtabs(formula, d), "array")
## expect_identical(ans.obtained, ans.expected)
## formula <-  ~ f1
## ans.obtained <- dtabs(d, formula)
## ans.expected <- as(xtabs(formula, d), "array")
## expect_identical(ans.obtained, ans.expected)
## formula <- y ~ .
## ans.obtained <- dtabs(d, formula)
## ans.expected <- as(xtabs(formula, d), "array")
## expect_identical(ans.obtained, ans.expected)
## formula <-  ~ .
## ans.obtained <- dtabs(d, formula)
## ans.expected <- as(xtabs(formula, d), "array")
## expect_identical(ans.obtained, ans.expected)
## formula <- y ~ f1 + f2
## d.miss <- d[-20,]
## ans.obtained <- dtabs(d.miss, formula)
## ans.expected <- dtabs(d, formula)
## ans.expected[20] <- 0L
## expect_identical(ans.obtained, ans.expected)
## formula <- y ~ f1 + f2
## d.miss <- d[-20,]
## ans.obtained <- dtabs(d.miss, formula, fill = NA)
## ans.expected <- dtabs(d, formula)
## ans.expected[20] <- NA
## expect_identical(ans.obtained, ans.expected)
## formula <- y ~ f1
## d.miss <- d[-20,]
## ans.obtained <- dtabs(d.miss, formula, fill = NA)
## ans.expected <- dtabs(d, formula)
## ans.expected[10] <- 10L
## expect_identical(ans.obtained, ans.expected)
## formula <- y ~ f1
## d.miss <- d
## d.miss$f1 <- factor(d.miss$f1)
## d.miss <- d.miss[-c(10, 20),]
## ans.obtained <- dtabs(d.miss, formula, fill = 0L)
## ans.expected <- dtabs(d, formula)
## ans.expected[10] <- 0L
## expect_identical(ans.obtained, ans.expected)
## d.miss <- d
## d.miss$f1 <- factor(d.miss$f1)
## d.miss <- d.miss[-c(10, 20),]
## ans.obtained <- dtabs(d.miss, formula, fill = NA)
## ans.expected <- dtabs(d, formula)
## ans.expected[10] <- NA
## expect_identical(ans.obtained, ans.expected)
