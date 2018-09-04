## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
injuries.df <- demdata::nz.injuries

## ------------------------------------------------------------------------
head(injuries.df)
summary(injuries.df)

## ---- echo = FALSE-------------------------------------------------------
death.rates <- demdata::VADeaths2
death.rates <- dembase::Values(death.rates)

## ------------------------------------------------------------------------
print(death.rates)

## ------------------------------------------------------------------------
dim(death.rates)
dimnames(death.rates)

## ---- echo = FALSE-------------------------------------------------------
dembase:::showMetaData(death.rates)

## ---- eval = FALSE-------------------------------------------------------
#  ?dimtypes

## ------------------------------------------------------------------------
as.data.frame(death.rates, direction = "long")

## ---- eval = FALSE-------------------------------------------------------
#  ?xtabs

## ------------------------------------------------------------------------
injuries.xt <- xtabs(count ~ age + sex + cause + year,
                     data = injuries.df,
                     subset = age != "Total all ages",
                     drop.unused.levels = TRUE)

## ---- eval = FALSE-------------------------------------------------------
#  injuries.df <- subset(injuries.df, age != "Total all ages")
#  injuries.xt <- xtabs(count ~ age + sex + cause + year,
#                       data = injuries.df,
#                       drop.unused.levels = TRUE)

## ------------------------------------------------------------------------
dim(injuries.xt)
dimnames(injuries.xt)

## ------------------------------------------------------------------------
injuries2013.xt <- injuries.xt[ , , , "2013"]
dimnames(injuries2013.xt)
injuries2013.xt

## ------------------------------------------------------------------------
library(dembase)
injuries2013.ct <- Counts(injuries2013.xt)
injuries2013.ct

## ---- error = TRUE-------------------------------------------------------
library(dembase)
injuries.ct <- Counts(injuries.xt)

## ------------------------------------------------------------------------
injuries.ct <- Counts(injuries.xt,
                      dimscales = c(year = "Intervals"))

## ------------------------------------------------------------------------
summary(injuries.ct)

## ---- fig.height = 4, fig.width = 4--------------------------------------
plot(injuries.ct)

## ------------------------------------------------------------------------
library(dplyr)
injuries.ct2 <- demdata::nz.injuries %>%
  filter(age != "Total all ages") %>%
  xtabs(count ~ age + sex + cause + year,
        data = .,
        drop.unused.levels = TRUE) %>%
  Counts(dimscales = c(year = "Intervals"))

all.equal(injuries.ct, injuries.ct2)

