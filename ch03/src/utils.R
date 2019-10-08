#################################
# individual mobility and health
# auxiliary functions
# author: sebastian daza
################################

# overwrite table and cor function to include missing data
table = function (...) base::table(..., useNA = 'ifany')
cor = function (...) stats::cor(..., use = "complete.obs")


# other functions
impute_locf = function(x) {
    output = zoo::na.locf(zoo::na.locf(x, FALSE), fromLast=TRUE)
    return(output)
}

renameColumns = function(dat, hash) {
    oldnames = hash::keys(hash)
    newnames = hash::values(hash)

    if (length(oldnames) != length(newnames)) {
        stop("Vector of names should have the same length")
    }

    setnames(dat, oldnames, newnames)
}


fillMissingColumns = function(dat, expression, expected) {
    varnames = names(dat)
    observed = grep(expression,  varnames, value = TRUE)
    dat[, ( expected[!expected %in% observed] ) := NA]
}

replaceMissing = function(x) {
    return(
        ifelse(x < 0, NA, x)
    )
}

get_max = function(x) {
    x = na.omit(x)
    if (length(x) == 0) {
        return(NA_real_)
    } else {
        return(max(x))
    }
}
