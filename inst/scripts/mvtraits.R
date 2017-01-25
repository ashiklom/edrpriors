library(mvtraits)

data('prospect5_results', package = 'edrpriors')

pft_levels <- c('temperate.Early_Hardwood', 
                'temperate.North_Mid_Hardwood',
                'temperate.Late_Hardwood',
                'temperate.Northern_Pine',
                'temperate.Southern_Pine',
                'temperate.Late_Conifer')

params <- c('N.mu', 'Cab.mu', 'Car.mu', 'Cw.mu', 'Cm.mu', 'leaf_mass_per_area')
dat_all <- prospect5_results[, c('pft.name', params), with = FALSE]
dat_all[, pft := as.integer(factor(pft.name, pft_levels))]

input <- dat_all[, c('pft', params), with = F]
out <- runModel('hier', input, NA, chains = 3)
saveRDS(out, 'mvtraits_raw_output.rds')

##debugonce(runModel)
##debugonce(mvtraits:::buildModel_hier)
##out <- runModel('hier', input, NA, chains = 3)
#dat <- as.matrix(input, nrow = nrow(input), ncol = ncol(input))
#model_name <- 'prospect'
#modbuild <- buildModel('hier', dat, model_name)

