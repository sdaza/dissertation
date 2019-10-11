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

apply(expand.grid("test", 1:3, c("a", "b")), 1, paste0, collapse="_")

as.vector(outer("test", 1:3, "_", 1:4, paste0, sep="_"))



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

get_min = function(x) {
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
    max_x = get_max(x)
    return( (max_x - x) + 1 )
}
