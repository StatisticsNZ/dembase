



rotateAgeTimePlan <- function(object, to, drop) {
    ## most have two of age/time/cohort
    ## is population: time must be points; age and cohort can be points or intervals
    ## is events: time, age, cohort must all be intervals; must have triangles
    ## has triangles, and two of time, age, cohort

    
}

periodCohort <- function(object) {
    ## has triangles, and two of time, age, cohort
    
}
    




makeConsistent <- function(object, adjust = TRUE, mult = 0.1) {

}



    


internal <- Net(internal, between = "region")
internal <- Pool(ins = internalIn, outs = internalOuts, between = "region")

births <- Births(births, sex = "sex", dominant = "Female")


p <- project(population ~ births - deaths + internal + externalIn - externalOut,
             initial = initial,
             values = vals)

a <- Movements(population ~ births - deaths + internal + externalIn - externalOut,
               values = vals)



reorderCategories(x, dimension = "region") ## defaults to FUN = sum

reorderCategories(x, dimension = "region", FUN = median)

