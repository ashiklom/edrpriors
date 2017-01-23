library(edrpriors)
#devtools::load_all('../..')

pft_levels <- c('temperate.Early_Hardwood', 
                'temperate.Late_Hardwood',
                'temperate.Late_Conifer')

params <- c('N.mu', 'Cab.mu', 'Car.mu', 'Cw.mu', 'Cm.mu', 'leaf_mass_per_area')

data('prospect5_results', package = 'edrpriors')

dat <- prospect5_results[, c('pft.name', params)]

hier_df <- dat
pfts <- factor(dat$pft.name, levels = pft_levels)
hier_df$pft <- as.integer(pfts)
hier_data <- as.matrix(hier_df[,-1])

hier_output <- fit_hierarchical(hier_data, niter = 10000, nchain = 3)

#M_global <- summary(hier_output[, sprintf('M_global[%d]', 1:6)])

M_pft_list <- mat2array(hier_output, 'M_pft')
M_pft_mean <- arrayApply(M_pft_list, mean)
M_pft_sd <- arrayApply(M_pft_list, sd)

#Sigma_global_list <- mat2array(hier_output, 'Sigma_global')
#Sigma_global_mean <- arrayApply(Sigma_global_list, mean)
#Sigma_global_sd <- arrayApply(Sigma_global_list, sd)

Sigma_pft_list <- mat2array(hier_output, 'Sigma_pft')
Sigma_pft_mean <- arrayApply(Sigma_pft_list, mean)
#Sigma_pft_sd <- arrayApply(Sigma_pft_list, sd)

#corr_samples <- arrayApply(Sigma_global_list, cov2cor, to_each_matrix = TRUE)
#corr_mean <- arrayApply(corr_samples, mean)
#corr_sd <- arrayApply(corr_samples, sd)
#corr_q025 <- arrayApply(corr_samples, quantile, probs = 0.025)
#corr_q975 <- arrayApply(corr_samples, quantile, probs = 0.975)

# Prepare data for sending
dimnames(M_pft_mean) <- list(params, pft_levels)
dimnames(Sigma_pft_mean) <- list(params, params, pft_levels)
prior_info <- list(means = list(M = M_pft_mean, Sigma = Sigma_pft_mean),
                   samples = window(hier_output, start = 7000))
save(prior_info, file = 'hier_fit.RData')
