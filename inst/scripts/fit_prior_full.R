library(edrpriors)

data('prospect5_results', package = 'edrpriors')

dat <- prospect5_results[, c('pft.name', 'N.mu', 'Cab.mu',
                             'Car.mu', 'Cw.mu', 'Cm.mu',
                             'leaf_mass_per_area')]

eh_data <- as.matrix(dat[dat$pft.name == 'temperate.Early_Hardwood', -1])
lh_data <- as.matrix(dat[dat$pft.name == 'temperate.Late_Hardwood', -1])
lc_data <- as.matrix(dat[dat$pft.name == 'temperate.Late_Conifer', -1])

eh_output <- fit_prior_multivariate(eh_data)
lh_output <- fit_prior_multivariate(lh_data)
lc_output <- fit_prior_multivariate(lc_data)

