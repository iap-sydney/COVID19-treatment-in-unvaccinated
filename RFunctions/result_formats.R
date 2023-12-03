odds.to.prob <- function (
    o
) { o/(1+o)
  }

prob.to.odds <- function (
    p
) { p/(1-p)
}

#collates results, need to change column names outside of this function
collate.results <- function (
    model
) { 
  results <- as.data.frame(cbind(summary(model)$beta,
                                 summary(model)$se,
                                 summary(model)$zval,
                                 summary(model)$pval,
                                 exp(summary(model)$beta),
                                 exp(summary(model)$ci.lb),
                                 exp(summary(model)$ci.ub)))
  results <- rownames_to_column(results,var = "variable")
}


#generate effpred for various VL effect sizes

gen.effpred <- function (
    model,x.start,x.end
) { 
  rrpred <- as.data.frame(predict(model,newmods = seq(x.start,x.end,length.out = num_x_evals),transf=exp,addx=TRUE))
  rrpred |> 
    mutate(eff = 1 - pred,
           eff.low = 1 - ci.ub,
           eff.high = 1 - ci.lb)
}
