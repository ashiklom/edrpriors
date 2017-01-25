library(edrpriors)
#devtools::load_all('../..')
data('prospect5_results', package = 'edrpriors')

pft_levels <- c('temperate.Early_Hardwood', 
                'temperate.North_Mid_Hardwood',
                'temperate.Late_Hardwood',
                'temperate.Northern_Pine',
                'temperate.Southern_Pine',
                'temperate.Late_Conifer')

params <- c('N.mu', 'Cab.mu', 'Car.mu', 'Cw.mu', 'Cm.mu', 'leaf_mass_per_area')
dat_all <- prospect5_results[, c('pft.name', params)]
prior_all <- edr_prior_hier(dat_all)
save(prior_all, file = 'prior_all.RData')

dat_sun <- dat_all[prospect5_results$SunShade %in% c('sun', NA), ]
prior_sun <- edr_prior_hier(dat_sun)
save(prior_sun, file = 'prior_sun.RData')

