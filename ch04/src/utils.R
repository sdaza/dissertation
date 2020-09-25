# auxiliary functions

readMultipleFiles = function(pattern, path) {
    files = list.files(path, pattern)
    files = paste0(path, files)
    l = lapply(files, fread)
    m =  rbindlist(l)
    return(m)
}