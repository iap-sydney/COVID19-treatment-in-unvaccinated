
calc.i2 <- function (
    model
) { 
  X <- model.matrix(model)
  W <- diag(1/model$vi)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  i2 <- 100 * model$sigma2 / (model$sigma2 + (model$k-model$p)/sum(diag(P)))
  i2.low <- 100 * confint(model)[[1]][1,2] / (confint(model)[[1]][1,2] + (model$k-model$p)/sum(diag(P)))
  i2.upp <- 100 * confint(model)[[1]][1,3] / (confint(model)[[1]][1,3] + (model$k-model$p)/sum(diag(P)))
  as.data.frame(cbind(i2,i2.low,i2.upp))
  
}