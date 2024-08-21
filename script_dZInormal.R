

library(nimble)
library(ggmcmc)

dZInormal <- nimbleFunction(
  run = function(x = double(1),
                 prob = double(1),
                 mu = double(1),
                 sigma = double(2),
                 log = integer(0, default = 0)){
    returnType(double(0))
    non_nul_indexes <- which(x!=0)
    nul_indexes <- which(x == 0)
    p_term <- sum(log(prob[nul_indexes])) +
      sum(log(1 - prob[non_nul_indexes])) 
    mu_term <- 0
    if(length(non_nul_indexes) > 0){
      chol_mat <- chol(sigma[non_nul_indexes, non_nul_indexes])
      restricted_x = x[non_nul_indexes]
      restricted_mu = mu[non_nul_indexes]
      mu_term <- dmnorm_chol(restricted_x,
                             restricted_mu,
                             chol_mat, prec_param = FALSE, log = TRUE)
    }
    log_output <- p_term + mu_term
    if(log){
      return(log_output)
    }
    else{
      return(exp(log_output))
    }
  }
)


my_code <- nimbleCode({
  for(j in 1:p){
    prob[j] ~ dunif(0, 1)
    mu[j] ~ dnorm(0, 1)
  }
  sigma[1:p, 1:p] ~ dwish(Ip[1:p, 1:p], p)
  for(i in 1:n){
    Y[i, 1:p] ~ dZInormal(prob[1:p], mu[1:p], sigma[1:p, 1:p])
  }
})

registerDistributions(list(
  dZInormal = list(BUGSdist = "dZInormal(prob, mu, sigma)", # How to call in nimble
                   discrete = FALSE, # Distribution is not discrete
                   pqAvail = FALSE, # CDF and quantile function are not available
                   types = c('value = double(1)', # The random variable is a vector
                             'prob = double(1)', # a vector
                             'mu = double(1)', # vector
                             'sigma = double(2)')) # double(2) is a matrix
))


# Generating data
set.seed(123)
n_obs <- 1000
n_species <- 3
# Values
U <- mixtools::rmvnorm(n = n_obs, 
                       mu = 0:(n_species - 1), 
                       sigma = diag(1, n_species))
# Mask (matrix of zeros and ones)
Z <- rbinom(n = n_obs * n_species, size = 1, prob = .8) %>% 
  matrix(nrow = n_obs)
# Observations
Y <- round(U * Z, 9)



my_model <- nimbleModel(code = my_code,
                        constants = list(p = n_species,
                                         n = n_obs,
                                         Ip = diag(1, n_species)),
                        data = list(Y = Y),
                        inits = list(mu = rep(0, n_species),
                                     prob = rep(0.5, n_species),
                                     sigma = diag(1, n_species)))
results <- nimbleMCMC(my_model,
                      samplesAsCodaMCMC = TRUE,
                      nchains = 2, niter = 10000,
                      nburnin = 1000, thin = 10)

ggs(results) %>% 
  ggplot(aes(x = Iteration, y = value, color = factor(Chain))) +
  facet_wrap(~Parameter, scales = "free") +
  geom_line()

