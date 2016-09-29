
setClass("CollapseTransformAgeToCohort",
         representation(iAgeBefore = "integer",
                        iTriangle = "integer",
                        addToAgeAfter = "integer"),
         contains = "CollapseTransformExtra")



## returns index of cell in 'after', given index of cell in 'before',
## going from time-age-triangle plan to period-cohort plan
getIAfterAgeToCohort <- function(i, transform, useC = FALSE) {
    ## i
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## transform
    stopifnot(is(transform, "CollapseTransformAgeToCohort"))
    stopifnot(validObject(transform))
    ## i and transform
    stopifnot(i <= prod(transform@dimBefore))
    if (useC) {
        .Call(getIAfterAgeToCohort_R, i, transform)
    }
    else {
        indices <- transform@indices
        dims <- transform@dims
        dim.before <- transform@dimBefore
        dim.after <- transform@dimAfter
        multiplier.after <- transform@multiplierAfter
        i.time.before <- transform@iTimeBefore
        i.age.before <- transform@iAgeBefore
        i.triangle <- transform@iTriangle
        add.to.age.after <- transform@addToAgeAfter
        margin.before <- posToMar(pos = i, dim = dim.before)
        margin.after <- marBeforeToMarAfter(mar = margin.before,
                                            indices = indices,
                                            dims = dims,
                                            dimAfter = dim.after)
        if (all(margin.after > 0L)) {
            i.age.after <- dims[i.age.before]
            n.age.after <- dim.after[i.age.after]
            margin.time.before <- margin.before[i.time.before]
            is.upper <- margin.before[i.triangle] == 2L
            margin.after[i.age.after] <- min(margin.after[i.age.after]
                                             + add.to.age.after[margin.time.before]
                                             + is.upper,
                                             n.age.after)
            pos <- marToPos(mar = margin.after, multiplier = multiplier.after)
        }
        else
            0L
    }
}


## Returns indices of cells in 'before' that map to cell in 'after',
## given the index of one of the cells in 'before'.  Returns a length-0
## integer vector if the cell in 'before' does not map into 'after'.
getISharedAgeToCohort <- function(i, transform, useC = FALSE) {
    ## i
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## transform
    stopifnot(is(transform, "CollapseTransformExtra"))
    ## i and transform
    stopifnot(i <= prod(transform@dimBefore))
    if (useC) {
        .Call(getIShared_R, i, transform)
    }
    else {
        pos <- getIShared(i = i, transform = transform)
        indices <- transform@indices
        dims <- transform@dims
        dim.before <- transform@dimBefore
        dim.after <- transform@dimAfter
        multiplier.before <- transform@multiplierBefore
        inv.indices <- transform@invIndices
        margin.before <- posToMar(pos = i, dim = dim.before)
        margin.after <- marBeforeToMarAfter(mar = margin.before,
                                            indices = indices,
                                            dims = dims,
                                            dimAfter = dim.after)
        if (all(margin.after == 0L))
            integer()
        else {
            i.age.after <- dims[i.age.before]
            n.age.after <- dim.after[i.age.after]
            margin.age.before <- margin.before[i.age.before]
            is.upper <- margin.before[i.triangle] == 2L
            add.to.age <- add.to.age.after[margin.age.before] + is.upper
            margin.after[i.age.after] <- margin.after[i.age.after] + add.to.age
            if (margin.after[i.age.after] > n.age)
                margin.after[i.age.after] <- n.age
            marAfterToPosBeforeCohortToAge(mar = margin.after,
                                           dims = dims,
                                           multiplierBefore = multiplier.before,
                                           invIndices = inv.indices)
        }
    }
}

marAfterToPosBeforeCohortToAge <- function(mar, dims, multiplierBefore, invIndices, iTimeBefore,
                                           nAgeBefore, iAgeBefore useC = FALSE) {
    ## mar
    stopifnot(is.integer(mar))
    stopifnot(!any(is.na(mar)))
    stopifnot(all(mar >= 1L))
    ## dims
    stopifnot(is.integer(dims))
    stopifnot(!any(is.na(dims)))
    stopifnot(all(dims >= 0L))
    ## multiplierBefore
    stopifnot(is.integer(multiplierBefore))
    stopifnot(!any(is.na(multiplierBefore)))
    stopifnot(all(multiplierBefore >= 1L))
    ## invIndices
    stopifnot(is.list(invIndices))
    stopifnot(all(sapply(invIndices, is.list)))
    stopifnot(is.integer(unlist(invIndices)))
    stopifnot(!any(is.na(unlist(invIndices))))
    stopifnot(all(unlist(invIndices) >= 1L))
    ## mar and dims
    stopifnot(identical(length(mar), sum(dims > 0L)))
    ## dims and multiplierBefore
    stopifnot(identical(length(dims), length(multiplierBefore)))
    ## dims and invIndices
    stopifnot(identical(length(dims), length(invIndices)))
    if (useC) {
        .Call(marAfterToPosBefore_R, mar, dims, multiplierBefore, invIndices)
    }
    else {
        n.dim.before <- length(dims)
        ## create 'inv.indices.margin' containing indices
        ## back into 'before' specific to 'mar'
        ## - not allowing for adjustments from period-cohort
        ## format back to age-triangle
        inv.indices.margin <- vector(mode = "list", length = length(invIndices))
        for (i.dim.before in seq_len(n.dim.before)) {
            inv.index <- invIndices[[i.dim.before]]
            i.dim.after <- dims[i.dim.before]
            if (i.dim.after > 0L)
                i.inv.index <- mar[i.dim.after]
            else
                i.inv.index <- 1L
            inv.indices.margin[[i.dim.before]] <- inv.index[[i.inv.index]]
        }
        ## 'mar' for 'before' are formed by taking cartesian
        ## product of 'inv.indices.mar'.  Coordinates of individual
        ## elements of 'inv.indices.mar' can be obtained using
        ## posToMar on 'n.indices'.
        n.indices <- sapply(inv.indices.margin, length)
        n.margin.before <- prod(n.indices)
        ans <- integer(length = n.margin.before)
        margin.before <- integer(length = n.dim.before)
        n.age.before <- dim.before[i.age.before]
        for (i.ans in seq_len(n.margin.before)) {
            i.indices <- posToMar(pos = i.ans, dim = n.indices)
            for (i.dim.before in seq_len(n.dim.before)) {
                inv.index <- inv.indices.margin[[i.dim.before]]
                i.index <- i.indices[i.dim.before]
                margin.before[i.dim.before] <- inv.index[i.index]
            }
            ## correct for cohort-to-age translation
            decrement <- addToAgeBefore[margin.before[iTimeBefore]]
            margin.before[iAgeBefore] <- margin.before[iAgeBefore] - decrement
            if (margin.before[iAgeBefore] < 0L)
                margin.before[iAgeBefore] <- margin.before[i.age.before] + nAgeBefore
            ans[i.ans] <- marToPos(margin.before, multiplier = multiplierBefore)
        }
        ans
    }
}



    


makeCollapseTransformAgeToCohort <- function(transform, iTimeBefore, iAgeBefore, iTriangle) {
    ans <- makeCollapseTransformExtra(transform)
    indices <- ans@indices
    index.age <- indices[[iAgeBefore]]
    s <- seq_along(index.age)
    nFromEndAgeGp <- function(i) sum(index.age[s > i] == index.age[i])
    add.to.age.after <- sapply(s, nFromEndAgeGp)
    add.to.age.after[index.age == 0L] <- 0L
    new("CollapseTransformAgeToCohort",
        ans,
        iAgeBefore = iAgeBefore,
        iTriangle = iTriangle,
        addToAgeAfter = add.to.age.after)
}
    
    
