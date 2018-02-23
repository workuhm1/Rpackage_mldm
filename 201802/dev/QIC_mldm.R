QIC.mldm <-
function(model_R, model_indep){
  
	require(MASS)
	
  # Fitted and observed values for quasi likelihood
  mu_R <- model_R$fitted.values
  y <- model_R$y
  
  # Quasi Likelihood for binomial
  quasi_R <- sum(y*log(mu_R/(1 - mu_R)) + log(1 - mu_R))    
  
  # Trace Term (penalty for model complexity)
  AIinverse <- ginv(model_indep$var_naive)    # Omega-hat(I) via Moore-Penrose generalized inverse of a matrix in MASS package

  Vr <- model_R$var_robust
  trace_R <- sum(diag(AIinverse%*%Vr))
  px <- as.numeric(model_R$npar)    # number non-redunant columns in design matrix
  
  # QIC
  QIC <- (-2)*quasi_R + 2*trace_R
  QICu <- (-2)*quasi_R + 2*px        # Approximation assuming model structured correctly 
  output <- data.frame(QIC=round(QIC, 2), QICu=round(QICu, 2), 
                       QuasiLik=round(quasi_R, 2), Trace=round(trace_R, 2),
                       px=px)
  output
}
