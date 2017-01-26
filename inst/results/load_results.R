library(edrpriors)

params <- c('N.mu', 'Cab.mu', 'Car.mu', 'Cw.mu', 'Cm.mu', 'leaf_mass_per_area')

pft_levels <- c('temperate.Early_Hardwood', 
                'temperate.North_Mid_Hardwood',
                'temperate.Late_Hardwood',
                'temperate.Northern_Pine',
                'temperate.Southern_Pine',
                'temperate.Late_Conifer')

results <- readRDS('mvtraits_raw_output.rds')

priors_all <- summarize_stan(results, params, pft_levels)
save(priors_all, file = 'stan_priors.RData')
