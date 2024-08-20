## @knitr methods

t_test<-new_method("t-test","Mean equality test",
                   method = function(model,draw){
                     list(pvalue = t.test(draw[,1], draw[,2],paired=TRUE)$p.value)
                   })
