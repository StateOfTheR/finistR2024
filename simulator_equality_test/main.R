# This is the main simulator file

library(simulator) # this file was created under simulator version 0.2.5

source("model_functions.R")
source("method_functions.R")
source("eval_functions.R")

## @knitr init

name_of_simulation <- "normal-mean-test"

## @knitr main

sim <- new_simulation(name = name_of_simulation,
                      label = "Test of mean") %>%
  generate_model(make_my_model_normal, seed = 13,
                 n = 20,
                 mu2 = as.list(seq(0,10,by=0.5)),
                 mu1=0,
                 sig=5,
                 vary_along="mu2")                 %>%
  simulate_from_model(nsim = 1000) %>%
  run_method(list(t_test)) %>%
  evaluate(list(pval_loss))

## @knitr plots


## @knitr tables

tabulate_eval(sim, "p_value", output_type = "markdown",
              format_args = list(digits = 5))

plot_eval_by(sim, "p_value", varying = "mu2", main = "Power curve with mu1=0 and varying mu2")

