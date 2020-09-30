#################################
# generative model of income mobility and mortality
# auxiliary functions
# author: sebastian daza
################################


readMultipleFiles = function(pattern, path) {
    files = list.files(path, pattern)
    files = paste0(path, files)
    l = lapply(files, fread)
    m =  rbindlist(l)
    return(m)
}


savepdf <- function(file, width = 16, height = 10) {
  fname <- paste0(file, ".pdf")
  pdf(fname, width = width / 2.54, height = height / 2.54,
      pointsize = 10)
  par(mgp = c(2.2, 0.45, 0), tcl = -0.4, mar = c(3.3, 3.6, 1.1, 1.1))
}

