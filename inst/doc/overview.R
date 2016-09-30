## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(dembase)
ind.popn <- demdata::india.popn # 'india.popn' is an array in package 'demdata'
ind.popn <- Counts(ind.popn)
ind.popn

## ------------------------------------------------------------------------
ind.fert <- demdata::india.fert
ind.fert <- Values(ind.fert)
ind.fert

## ------------------------------------------------------------------------
va.rates <- demdata::VADeaths2
va.rates <- Values(va.rates)
va.rates

## ------------------------------------------------------------------------
income.df <- demdata::nz.income # a data.frame
head(income.df)
total.income <- xtabs(income ~ ethnicity + sex, 
                      data = income.df)
total.income <- Counts(total.income)
total.income

## ------------------------------------------------------------------------
mean.income <- tapply(income.df$income, 
                      INDEX = income.df[c("ethnicity", "sex")], 
                      FUN = mean)
mean.income <- Values(mean.income)
round(mean.income)

## ------------------------------------------------------------------------
rus.births <- demdata::russia.births
rus.births.subset.08 <- xtabs(count ~ age + sex,
                              data = rus.births,
                              subset = year == 2008 & age %in% 15:19,
                              drop.unused.levels = TRUE)
rus.births.subset.08
Counts(rus.births.subset.08) # 'Counts' issues a message

## ------------------------------------------------------------------------
Counts(rus.births.subset.08,
       dimscales = c(age = "Intervals")) # no message

## ------------------------------------------------------------------------
Counts(rus.births.subset.12,
       dimscales = c(year = "Intervals")) # no message

## ------------------------------------------------------------------------
va.popn <- demdata::VAPopn
va.popn <- Counts(va.popn)
va.popn.values <- as(va.popn, "Values")
class(va.popn.values)

## ------------------------------------------------------------------------
va.popn.array <- as(va.popn, "array")
class(va.popn.array)

## ------------------------------------------------------------------------
va.popn.array.2 <- as.array(va.popn)
class(va.popn.array.2)

## ------------------------------------------------------------------------
va.popn.df <- as.data.frame(va.popn, 
                            direction = "long")
head(va.popn.df)

## ------------------------------------------------------------------------
va.popn.mid <- as.data.frame(va.popn, 
                             direction = "long",
                             midpoints = "age")
head(va.popn.mid)

## ------------------------------------------------------------------------
ind.popn
collapseDimension(ind.popn, dimension = "age")

## ------------------------------------------------------------------------
collapseDimension(va.rates, dimension = "age", weights = va.popn)

## ------------------------------------------------------------------------
summary(va.popn)
summary(va.rates)

## ------------------------------------------------------------------------
collapseIntervals(ind.popn, dimension = "age", width = 10)

## ------------------------------------------------------------------------
ind.popn.2 <- subarray(ind.popn, age < 60)
ind.popn.2 <- collapseIntervals(ind.popn.2,
                                dimension = "age", 
                                breaks = c(10, 25, 50))
ind.popn.2 <- t(ind.popn.2)
ind.popn.2
ind.popn - ind.popn.2

## ------------------------------------------------------------------------
subarray(ind.fert, age > 30)
subarray(va.rates, residence == "Urban" & age < 62)

## ------------------------------------------------------------------------
slab(va.rates, 
     dimension = "residence",
     elements = "Urban")

## ------------------------------------------------------------------------
births <- demdata::nz.births
popn <- demdata::nz.popn.reg
births <- Counts(births,
                 dimscales = c(year = "Intervals"))
popn <- Counts(popn,
               dimscales = c(year = "Intervals"))
females <- subarray(popn, sex == "Female")
limits(births)
limits(females)

## ------------------------------------------------------------------------
rates <- births / females  # message
limits(rates)

## ------------------------------------------------------------------------
births.sub <- subarray(births, year > 2005 & year < 2014)
females.sub <- subarray(females, age > 15 & age < 45)
rates.sub <- births.sub / females.sub  # no message
limits(rates)

## ------------------------------------------------------------------------
ind.popn.young <- subarray(ind.popn, age < 40)
ind.popn.old <- subarray(ind.popn, age > 40)
ind.popn.young
ind.popn.old
dbind(ind.popn.young, ind.popn.old, along = "age")

## ------------------------------------------------------------------------
ind.popn.young <- t(ind.popn.young)
ind.popn.young
dbind(ind.popn.old, ind.popn.young, along = "age")

## ---- fig.width = 6------------------------------------------------------
plot(va.popn)

## ---- fig.width = 6------------------------------------------------------
dplot(~ age | period, 
      data = ind.fert,
      midpoints = "age")

## ---- fig.width = 6------------------------------------------------------
dplot(~ age | sex,
      data = va.rates,
      weights = va.popn) # collapses 'residence' dimension

## ---- fig.width = 6------------------------------------------------------
va.rates.collapsed <- collapseDimension(va.rates, 
                                        margin = c("age", "sex"),
                                        weights = va.popn)
va.rates.df <- as.data.frame(va.rates.collapsed,
                             direction = "long",
                             midpoints = "age")
lattice::xyplot(value ~ age | sex, 
                data = va.rates.df,
                type = "b")

## ------------------------------------------------------------------------
tfr <- demdata::waikato.tfr
tfr <- Values(tfr, dimscales = c(year = "Intervals"))
summary(tfr)
subarray(tfr, iteration <= 5)

## ------------------------------------------------------------------------
mean.tfr <- mean(tfr)
summary(mean.tfr)
subarray(mean.tfr, iteration <= 5)

## ---- fig.width = 5------------------------------------------------------
density.mean.tfr <- density(mean.tfr)
plot(density.mean.tfr)

## ------------------------------------------------------------------------
collapseIterations(tfr)

## ------------------------------------------------------------------------
collapseIterations(tfr, prob = c(0.025, 0.5, 0.975))

## ------------------------------------------------------------------------
collapseIterations(tfr, FUN = mean)

meanAndCI <- function(x) {
  mean <- mean(x)
  sd <- sd(x)
  lower <- mean - 2*sd
  upper <- mean + 2*sd
  c(mean = mean, lower = lower, upper = upper)
}
collapseIterations(tfr, FUN = meanAndCI)

## ------------------------------------------------------------------------
quant.tfr <- collapseIterations(tfr, prob = c(0.025, 0.5, 0.975))
round(quant.tfr, digits = 2)

## ---- fig.width = 6------------------------------------------------------
dplot(~ year,
      data = tfr)

## ---- fig.width = 6------------------------------------------------------
dplot(~ year,
      data = tfr,
      prob = c(0.05, 0.5, 0.95))

## ------------------------------------------------------------------------
mean.tfr <- mean(tfr)
collapseIterations(mean.tfr, 
                   prob = c(0.025, 0.5, 0.975))

