# I want as parameters n, mu1, mu2, sigma and 1 simulation is a couple of
# sequences.
## @knitr models
make_my_model_normal <- function(n, mu1, mu2, sig) {
  new_model(
    name = "normal", 
    label = sprintf("normal"), 
    params = list(n = n, mu1 = mu1, mu2 = mu2, sig = sig),
    simulate = function(n, mu1,mu2, sig, nsim) {
      # this function must return a list of length nsim
      x1 <- mu1 + sig * matrix(rnorm(nsim * n), n, nsim)
      x2 <- mu2 + sig * matrix(rnorm(nsim * n), n, nsim)
      li1 <- split(x1, col(x1))
      li2 <- split(x2, col(x2))
      lapply(1:nsim, function(i) {
        cbind(li1[[i]], li2[[i]])
      })
    }
  )
}
