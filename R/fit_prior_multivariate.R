#' @export
fit_prior_multivariate <- 
    function(data_matrix,
             niter = 10000,
             nburn = 5000) {

    nc <- ncol(data_matrix)
    nr <- nrow(data_matrix)
    k <- nc

    nimble_code <- nimbleCode({
        for (i in 1:nr) {
            # Data model (missing data)
            for (j in 1:nc) {
                x_obs[i,j] ~ dnorm(mean = x_pred[i,j], 
                                   tau = obs_error_tau)
            }
            # Process model
            x_pred[i,1:nc] ~ dmnorm(mean = M[1:nc], 
                                    prec = Omega[1:nc,1:nc])
        }
        # Prior on mean
        for (i in 1:nc) {
            M[i] ~ dnorm(mean = mu0, tau = tau0)
        }
        # Prior on variance
        Omega[1:nc,1:nc] ~ dwish(R = R[1:nc,1:nc], df = k)
        # Derived covariance
        Sigma[1:nc,1:nc] <- inverse(Omega[1:nc,1:nc])
    })


    mvmodel <- nimbleModel(code = nimble_code, 
                           constants = list(nc = nc, nr = nr),
                           data = list(x_obs = data_matrix,
                                       mu0 = 0,
                                       tau0 = 0.01,
                                       obs_error_tau = 100,
                                       R = diag(k/0.01, nc),
                                       k = k),
                           inits = list(M = rep(0, nc),
                                        Omega = diag(1, nc),
                                        x_pred = matrix(0, nr, nc)))

    mvConf <- configureMCMC(mvmodel)
    mvConf$addMonitors(c('M', 'Sigma'))
    mv_mcmc <- buildMCMC(mvConf)
    CmvMCMC <- compileNimble(mvmodel, mv_mcmc)

    CmvMCMC$mv_mcmc$run(niter)
    samples <- as.matrix(CmvMCMC$mv_mcmc$mvSamples)[-nburn:0,]
    colnames(samples)[1:nc] <- colnames(data_matrix)

    means <- colMeans(samples)

    M_mean <- means[colnames(data_matrix)]

    Sigma_mean <- matrix(means[grepl('Sigma', names(means))], 
                         ncol = nc, 
                         byrow = TRUE)
    colnames(Sigma_mean) <- rownames(Sigma_mean) <- colnames(data_matrix)

    output <- list(means = list(M = M_mean, Sigma = Sigma_mean),
                   samples = samples)
    return(output)
}
