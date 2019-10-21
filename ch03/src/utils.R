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

fillWithFirstValue = function(x) {
  fv = head(na.omit(x), 1)
  x = ifelse(is.na(x), fv, x)
  return(x)
}

fillWithLastValue = function(x) {
  fv = tail(na.omit(x), 1)
  x = ifelse(is.na(x), fv, x)
  return(x)
}

renameColumns = function(dat, hash) {
    oldnames = as.vector(hash::keys(hash))
    newnames = as.vector(hash::values(hash))

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

getMax = function(x) {
    x = na.omit(x)
    if (length(x) == 0) {
        return(NA_real_)
    } else {
        return(max(x))
    }
}

getMin = function(x) {
    x = na.omit(x)
    if (length(x) == 0) {
        return(NA_real_)
    } else {
        return(min(x))
    }
}


createQuantiles = function(x, groups = 5) {
    output = cut(x,
                 breaks = quantile(x, probs = seq(0, 1, by = 1 / groups), na.rm = TRUE),
                 labels = 1:groups,
                 include.lowest = TRUE,
                 right = FALSE)
    return(output)
}


reverseScale = function(x) {
    max_x = getMax(x)
    return( (max_x - x) + 1 )
}


hashHHColumns = function(myhash, years, newheader) {

    old_names = NULL
    new_names = NULL
    mkeys = keys(myhash)

    for (i in seq_along(myhash)) {

        header = mkeys[i]
        vvalues = as.vector(values(myhash[header]))
        seqvalues = seq(1:length(vvalues))

        old_names = c(old_names, paste0(header, vvalues, "00"))
        new_names = c(new_names, paste0(newheader, seqvalues, "_", years[i]))

    }
    return(hash(old_names, new_names))
}

getCoefficients = function(micombine, position = 2, coeff = TRUE) {
    if (coeff) {
        v = summary(micombine)$results[position]
        names(v)
    } else { return ( summary(micombine)$se[position]) }
}

