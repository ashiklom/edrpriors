#' @export
edr_prior_hier <- function(hier_df, niter = 10000, nchain = 3, nburn = 7000) {
    params <- grep('pft.name', colnames(hier_df), value = TRUE, invert = TRUE)
    pfts <- factor(hier_df$pft.name)
    pft_levels <- levels(pfts)
    hier_df$pft <- as.integer(pfts)
    hier_data <- as.matrix(hier_df[,-1])

    hier_output <- fit_hierarchical(hier_data, niter = niter, nchain = nchain)
    hier_burned <- window(hier_output, start = nburn)

    M_pft_list <- mat2array(hier_burned, 'M_pft')
    M_pft_mean <- arrayApply(M_pft_list, mean)

    Sigma_pft_list <- mat2array(hier_burned, 'Sigma_pft')
    Sigma_pft_mean <- arrayApply(Sigma_pft_list, mean)

    # Prepare data for sending
    dimnames(M_pft_mean) <- list(params, pft_levels)
    dimnames(Sigma_pft_mean) <- list(params, params, pft_levels)
    prior_info <- list(means = list(M = M_pft_mean, Sigma = Sigma_pft_mean),
                       samples = hier_burned)
    return(prior_info)
}
