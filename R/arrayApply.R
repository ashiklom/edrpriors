#' @export
arrayApply <- function(arr, fun, ...) {
    UseMethod('arrayApply')
}

#' @export
arrayApply.array <- function(arr, fun, ...) {
    in_dims <- dim(arr)
    ndims <- length(in_dims)
    if (ndims == 3) {
        return(arrayApply3D.array(arr, fun, ...))
    } else if (ndims == 4) {
        return(arrayApply4D.array(arr, fun, ...))
    } else {
        stop('arrayApply for ', ndims, ' dimensions currently unsupported')
    }
}

#' @export
arrayApply.list <- function(arr_list, fun, ...) {
    arr <- arraylist2array(arr_list)
    out_array <- arrayApply.array(arr, fun, ...)
    return(out_array)
}

arrayApply3D.array <- function(arr, fun, to_each_matrix = FALSE, ...) {
    in_dims <- dim(arr)
    if (to_each_matrix) { 
        out_dims <- in_dims
    } else {
        out_dims <- in_dims[-3]
    }
    out_array <- array(numeric(), out_dims)
    if (to_each_matrix) {
        for (s in seq_len(out_dims[3])) {
            out_array[,,s] <- fun(arr[,,s], ...)
        }
    } else {
        for (i in seq_len(out_dims[1])) {
            for (j in seq_len(out_dims[2])) {
                out_array[i,j] <- fun(arr[i,j,], ...)
            }
        }
    }
    return(out_array)
}

arrayApply4D.array <- function(arr, fun, to_each_matrix = FALSE, ...) {
    in_dims <- dim(arr)
    if (to_each_matrix) { 
        out_dims <- in_dims
    } else {
        out_dims <- in_dims[-4]
    }
    out_array <- array(numeric(), out_dims)
    if (to_each_matrix) {
        for (k in seq_len(out_dims[3])) {
            for (s in seq_len(out_dims[4])) {
                out_array[,,k,s] <- fun(arr[,,k,s], ...)
            }
        }
    } else {
        for (i in seq_len(out_dims[1])) {
            for (j in seq_len(out_dims[2])) {
                for (k in seq_len(out_dims[3])) {
                    out_array[i, j, k] <- fun(arr[i,j,k,], ...)
                }
            }
        }
    }
    return(out_array)
}
