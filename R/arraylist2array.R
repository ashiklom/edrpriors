#' @export
arraylist2array <- function(arr_list) {
    nlist <- length(arr_list)
    list_dims <- sapply(arr_list, dim)
    ndims <- nrow(list_dims)
    if (ndims == 3) {
        return(arraylist2array3D(arr_list))
    } else if (ndims == 4) {
        return(arraylist2array4D(arr_list))
    } else {
        stop(ndims, ' dimensions currently unsupported')
    }
}

arraylist2array3D <- function(arr_list) {
    nlist <- length(arr_list)
    list_dims <- sapply(arr_list, dim)
    out_dims <- c(apply(list_dims[-3,], 1, unique), sum(list_dims[3,]))
    out_array <- array(numeric(), out_dims)
    nend <- cumsum(list_dims[3,])
    nstart <- 1 + (0:(nlist-1) * list_dims[3,])
    for (n in seq_len(nlist)) {
        out_array[, , nstart[n]:nend[n]] <- arr_list[[n]]
    }
    return(out_array)
}

arraylist2array4D <- function(arr_list) {
    nlist <- length(arr_list)
    list_dims <- sapply(arr_list, dim)
    out_dims <- c(apply(list_dims[-4,], 1, unique), sum(list_dims[4,]))
    out_array <- array(numeric(), out_dims)
    nend <- cumsum(list_dims[4,])
    nstart <- 1 + (0:(nlist-1) * list_dims[4,])
    for (n in seq_len(nlist)) {
        out_array[, , , nstart[n]:nend[n]] <- arr_list[[n]]
    }
    return(out_array)
}
