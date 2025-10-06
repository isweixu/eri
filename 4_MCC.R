# This script contains the function for calculating multiple correlation
# coefficient, which is applied in the Environmental Racism Index paper to
# calculate the three-way correlation between environmental exposure index
# (EEI), mortgage disinvestment index (MDI), and proportion of minority
# residents (PMR).
# Author: Kai Yang
# Organization: Medical College of Wisconsin
# Date: October, 2023


## MCC function ------------------------------------------------------------

mcc <- function(data, var1, var2, var3){
  
  X <- as.data.frame(data[,c(var1, var2, var3)]) # the three variables
  X <- X[which(complete.cases(X)),]
  X <- as.matrix(X)
  
  n <- dim(X)[1]; p <- dim(X)[2]
  for(i in 1:p) {
    X[,i] <- X[,i]-mean(X[,i])
  }
  Sigma <- cov(X); V <- cor(X)
  psi.o <- sqrt(1-det(V)^(2/p))
  psi.o 
  
  #### Theoretical CI and p-value
  
  Eig <- eigen(V)$vectors; eig <- eigen(V)$values
  V.h <- Eig%*%diag(sqrt(eig))%*%t(Eig)
  V.h2 <- V.h^2
  I <- matrix(0,p,p); diag(I) <- rep(1,p); V.d <- V-I
  tau <- sum(diag(V.h2%*%V.h2))-1/n*(sum(diag(V.h2)))^2
  eta <- sum(diag(V.d%*%V.d))-1/n*(sum(diag(V.d)))^2
  Z <- rep(0,n)
  for(i in 1:n) {
    Z[i] <- sum(X[i,]^2)
  }
  nu <- var(Z)
  varsigma <- sum(diag(Sigma%*%Sigma))-1/n*(sum(diag(Sigma)))^2
  omega <- sum(diag(Sigma)^2)
  kappa <- max(1,3+(nu-2*varsigma)/omega)
  
  delta <- 2*(1-n/p+1.5/p)*log(1-p/n)-2+2/n+1/n*(kappa-3)*(tau/p-1)
  sigma <- sqrt(-8*log(1-p/n)/(p^2)-8/(n*p)+8*eta/(n*p^2))
  
  ## the bias-corrected MCC
  val.psi <- 1-(1-psi.o^2)*exp(-delta)
  val.psi <- max(0,val.psi); val.psi <- min(1,val.psi)
  psi.bc <- sqrt(val.psi)
  psi.bc
  
  ## the 95% CI
  val.l <- 1-(1-psi.bc^2)*exp(1.96*sigma)
  val.u <- 1-(1-psi.bc^2)*exp(-1.96*sigma)
  val.l <- max(val.l,0); val.u <- min(val.u,1)
  L <- sqrt(val.l); U <- sqrt(val.u)
  L; U
  
  ## the p-value
  num <- log(1-psi.o^2)-2*(1-n/p+1.5/p)*log(1-p/n)+2-2/n
  den <- sqrt(-8*log(1-p/n)/(p^2)-8/(n*p))
  Z <- num/den
  pval <- 2*(1-pnorm(abs(Z)))
  pval
  
  tt <- data.frame(psi.bc, L, U, pval)
  names(tt) <- c("mcc" ,"ll", "ul","p")
  
  return(tt)
}
