

# function for correlation matrix plot ------
corr_fun <- function(data){
  # Compute a correlation matrix
  corr <- round(cor(data, use = "pairwise.complete.obs"), 2)
  
  # Compute a matrix of correlation p-values
  p.mat <- cor_pmat(data)
  
  # Barring the no significant coefficient
  p <- ggcorrplot(corr, hc.order = TRUE,lab = TRUE,
                  type = "full", p.mat = p.mat)
  
  return(p)
}



## MCC function ------------------------------------------------------------
# 
# i="Montgomery, AL"
# data <- df |> filter(cbsa_title == i)
# var1<- "environment_s"
# var2<- "pct_nonwhite"
# var3<- "Redline"

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


# calculate cbsa mcc -----

# data <- df_merged
# env_var <- "env_efa_s"
# i="Montgomery, AL"
# mcc_name="mcc_efa"
cbsa_mcc_fun <- function(data, env_var, mcc_name){
  
  # extract list of cbsa
  cbsa_list <- data |> 
    distinct(cbsa_code, cbsa_title, metro_micro_type) |> 
    drop_na(cbsa_code)
  
  # calculate cbsa mcc
  cbsa_mcc <- data.frame()
  for (i in cbsa_list$cbsa_title) {
    
    t <- mcc(data |> filter(cbsa_title == i), env_var, "pct_nonwhite", "Redline")
    t1 <- cbind(cbsa_list |> filter(cbsa_title == i), t)
    cbsa_mcc <- rbind(cbsa_mcc, t1)
  }
  names(cbsa_mcc)[4] <- mcc_name
  return(cbsa_mcc)
}


# calculate state MCC -----

state_mcc_fun <- function(data, env_var, mcc_name){
  # extract list of states
  states <- unique(data$statefips)
  
  # calculate state mcc
  state_mcc <- data.frame()
  for (i in states) {
    t <- mcc(data |> 
               filter(statefips == i), env_var, "pct_nonwhite", "Redline")
    t1 <- c(i, t) |> as.data.frame() 
    names(t1)[1]<-"GEOID"
    state_mcc <- rbind(state_mcc, t1) 
  }
  
  names(state_mcc) <- c("GEOID", mcc_name, "LL", "UL", "P")
  return(state_mcc)
}

