## @knitr metrics
pval_loss <- new_metric(
  name = "p_value", 
  label = "pval<0.05",
  metric = function(model, out) {
    mean(out$pvalue < 0.05)
  }
)
