## @knitr metrics


pval_loss <- new_metric("p_value", "pval<0.05",
                       metric = function(model, out) {
                         return(mean(out$pvalue<0.05))
                       })
