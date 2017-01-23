#' @export
mat2array <- function(samples, varname) {
    UseMethod('mat2array')
}

#' @export
mat2array.mcmc <- function(samples, varname) {
    nsamp <- nrow(samples)
    samples_sub <- samples[, grep(varname, colnames(samples))]
    dim_names <- colnames(samples_sub)
    inds <- gsub('.*\\[(.*)\\]$', '\\1', dim_names)
    dimnums <- sapply(strsplit(inds, split = ', '), as.numeric)
    dims <- apply(dimnums, 1, max)
    ndims <- length(dims)
    out_array <- array(numeric(), c(dims, nsamp))
    if (!ndims %in% c(2, 3)) {
        stop('Unsupported number of dimensions: ', ndims)
    }
    for (i in seq_len(ncol(dimnums))) {
        if (ndims == 2) {
            out_array[dimnums[1,i], dimnums[2,i], ] <- samples_sub[,i]
        } else if (ndims == 3) {
            out_array[dimnums[1,i], dimnums[2,i], dimnums[3,i], ] <- samples_sub[,i]
        }
    }
    return(out_array)
}

#' @export
mat2array.mcmc.list <- function(samples_list, varname) {
    out_list <- lapply(samples_list, mat2array.mcmc, varname = varname)
    return(out_list)
}
