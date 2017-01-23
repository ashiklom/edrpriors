#' @export
fit_hierarchical <- function(data_matrix, ...) {

    x_obs <- data_matrix[,colnames(data_matrix) != 'pft']
    pfts <- data_matrix[,'pft']

    nc <- ncol(x_obs)
    nr <- nrow(x_obs)
    k <- nc
    npft <- length(unique(pfts))

    nimble_code <- nimbleCode({
        # Data model
        for (i in 1:nr) {
            for (j in 1:nc) {
                x_obs[i,j] ~ dnorm(mean = x_pred[i,j], 
                                   tau = obs_error_tau)
            }
        }

        # Process model -- PFT
        for (i in 1:nr) {
            x_pred[i,1:nc] ~ dmnorm(mean = M_pft[1:nc,pfts[i]], 
                                    prec = Omega_pft[1:nc,1:nc,pfts[i]])
        }

        # Process model -- global
        for (j in 1:npft) {
            # Among-PFT structure
            M_pft[1:nc,j] ~ dmnorm(mean = M_global[1:nc], 
                                   prec = Omega_global[1:nc,1:nc])
        }

        # Prior on global mean
        for (i in 1:nc) {
            M_global[i] ~ dnorm(mean = mu0, tau = tau0)
        }

        # Prior on global covariance
        Omega_global[1:nc,1:nc] ~ dwish(R = R[1:nc,1:nc], df = k)
        Sigma_global[1:nc,1:nc] <- inverse(Omega_global[1:nc,1:nc])

        # Prior on PFT covariance
        for (j in 1:npft) {
            Omega_pft[1:nc,1:nc,j] ~ dwish(R = R[1:nc,1:nc], df = k)
            Sigma_pft[1:nc,1:nc,j] <- inverse(Omega_pft[1:nc,1:nc,j])
        }
    })

    Omega_init <- array(0, c(nc, nc, npft))
    for (i in 1:npft) {
        diag(Omega_init[,,i]) <- 1
    }

    mvmodel <- nimbleModel(code = nimble_code, 
                           constants = list(nc = nc, nr = nr, npft = npft,
                                            pfts = pfts),
                           data = list(x_obs = x_obs,
                                       mu0 = 0,
                                       tau0 = 0.01,
                                       obs_error_tau = 100,
                                       R = diag(k/0.01, nc),
                                       k = k),
                           inits = list(M_global = rep(0, nc),
                                        Omega_global = diag(1, nc),
                                        M_pft = matrix(0, nc, npft),
                                        Omega_pft = Omega_init,
                                        x_pred = matrix(0, nr, nc)))

    mvConf <- configureMCMC(mvmodel, monitors = c('M_global', 'Sigma_global',
                                                  'M_pft', 'Sigma_pft'))
    mv_mcmc <- buildMCMC(mvConf)
    Cmv <- compileNimble(model = mvmodel, mcmc = mv_mcmc)

    samples <- runMCMC(Cmv$mcmc, returnCodaMCMC = TRUE, ...)
    #colnames(samples)[1:nc] <- colnames(x_obs)

    return(samples)
    #means <- colMeans(samples)

    #M_mean <- means[colnames(x_obs)]

    #Sigma_mean <- matrix(means[grepl('Sigma', names(means))], 
                         #ncol = nc, 
                         #byrow = TRUE)
    #colnames(Sigma_mean) <- rownames(Sigma_mean) <- colnames(x_obs)

    #output <- list(means = list(M = M_mean, Sigma = Sigma_mean),
                   #samples = samples)
    #return(output)
}
