#' @export
summarize_stan <- function(results, 
                           params,
                           pft_levels,
                           mu_name = 'mu_pft',
                           sigma_name = 'Sigma_pft') {

    samples <- extract(results, c('mu_pft', 'Sigma_pft'))

    mu_samples <- samples[[mu_name]] %>%
        melt %>%
        acast(Var3 ~ Var2 ~ iterations)

    sigma_samples <- samples[[sigma_name]] %>%
        melt %>%
        acast(Var4 ~ Var3 ~ Var2 ~ iterations)

    mu_mean <- arrayApply(mu_samples, mean)
    dimnames(mu_mean) <- list(params, pft_levels)
    sigma_mean <- arrayApply(sigma_samples, mean)
    dimnames(sigma_mean) <- list(params, params, pft_levels)

    return(list(means = list(M = mu_mean, Sigma = sigma_mean),
                samples = samples))
}
