# my_function <- function(x, prob, mu, sigma){
#   pos_indexes <- which(x != 0)
#   zero_indexes <- which(x == 0)
#   n_zeros <- length(x) - length(pos_indexes)
#   p_term <- sum(log(prob[zero_indexes])) + 
#     sum(log(1 - prob[pos_indexes]))
#   mu_term <- 0
#   if(length(pos_indexes) > 0){
#     mu_term <- mixtools::logdmvnorm(x[pos_indexes],
#                                     mu[pos_indexes],
#                                     sigma[pos_indexes, pos_indexes])
#   }
#   p_term + mu_term
# }

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
      return(exp(log_output))
    }
    else{
      return(log_output)
    }
  }
)
# 
# sum(apply(Y, 1, 
#       function(y) 
#         dZInormal(y, rep(0.4, 5), mu = 0:4, sigma = diag(1, 5)))) 
# 
# sum(apply(Y, 1, 
#           function(y) 
#             dZInormal(y, rep(0.4, 5), mu = rep(0, 5), sigma = diag(1, 5)))) 

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
  dZInormal = list(BUGSdist = "dZInormal(prob, mu, sigma)",
                   discrete = FALSE, pqAvail = FALSE,
                   types = c('value = double(1)', 'prob = double(1)', 
                             'mu = double(1)', 'sigma = double(2)'))
))

n_obs <- 5000
n_species <- 3
my_sigma <- diag(1, n_species)
U <- mixtools::rmvnorm(n = n_obs, mu = 0:(n_species - 1), sigma = my_sigma)
Z <- rbinom(n = n_obs * n_species, size = 1, prob = .8) %>% 
  matrix(nrow = n_obs)
Y <- round(U * Z, 9)

my_model <- nimbleModel(code = my_code, 
                        constants = list(p = n_species, 
                                         n = n_obs, 
                                         Ip = diag(1, n_species)),
                        data = list(Y = Y),
                        inits = list(mu = 0:n_species, 
                                     prob = rep(0.5, n_species),
                                     sigma = diag(1, n_species)))

results <- nimbleMCMC(my_model,
                      nchains = 1, niter = 10000, nburnin = 1000, thin = 10)
hist(results[,"prob[]"])


y <- c(0, 0, 2, 1)

ps <- runif(length(y))
mus <- rnorm(length(y))
sigma <- rWishart(n = 1, Sigma = diag(1, length(y)), df = length(y))[,,1]
my_function(y, ps, mus, sigma)
dZInormal(y, ps, mus, sigma)