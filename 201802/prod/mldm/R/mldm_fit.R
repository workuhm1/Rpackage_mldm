#' Fits a Multivariate Logistic Distance (MLD) model on multivariate binary data (i.e., multiple binary responses with one or many independent explanatory variables). 
#' 
#' @param formula A glm formula, should be specified for each dimension.
#' @param index An identifier of the response variable.
#' @param resp.dim.ind Response Indicator matrix. It indicates to which dimension 
#' every response belongs.
#' @param resp.var.labels Labels of the reponse variable.
#' @param data Input data frame.
#' @param id Subject identification number.
#' @param center Apply centering on explanatory variables.
#' @param scale Apply standard normal transformation on explanatory variables.
#' @param Rusr A (working) correlation coeffiecient.
#' @param conf.interval Confidence interval level.
#' @param bootstrap Apply Clustered Bootrstrap procedure to obtain model standard errors.
#' 
#' @author Hailemichael M. Worku (aka, Haile) and Mark de Rooij.
#' 2018 | Leiden university, M&S unit.
#' 
#' @examples 
#' ## Load package
#' # library(mldm)
#' 
#' ## Load dataset
#' # data(NESDA)
#' 
#' ## Specifiy response indicator matrix 
#' # Z <- matrix(c(1,1,0,0,0,0,0,1,1,1), 5, 2, byrow = FALSE)
#' 
#' ## Specify model formula
#' # mf <- Outcome | Outcome ~ EDU + GEN + AGE + N + E | EDU + GEN + AGE + N + E
#' # mf <- Formula(mf)
#' 
#' ## Fit MLD model
#' # fit1 <- mldm.fit(formula = mf, index = Index, resp.dim.ind = Z, 
#' #                  data = NESDA, id = pident, scale = TRUE)
#'                    
#' ## display result
#' # fit1
#' 
#' 

mldm.fit <- function(formula, index, resp.dim.ind, resp.var.labels = NULL, 
                     data = NULL, id, center = TRUE, scale = FALSE, 
                     Rusr = "independence", 
                     conf.interval = .95, bootstrap = FALSE){
  
  if(missing(data)) 
    data <- environment(formula)

  scall <- match.call()
  
  # Get other variables, not included model formula
  mm <- match.call(expand = FALSE)
  mmf <- match(c("formula", "index", "id", "data"), names(mm), 0)
  mm <- mm[c(1, mmf)]
  mm$drop.unused.levels <- TRUE
  mm[[1]] <- as.name("model.frame")     # 'model.frame' uses the 'formula' parameter
  mm <- eval(mm, parent.frame())
  
  index <- as.matrix(model.extract(mm, index))
  id <- as.matrix(model.extract(mm, id))
  
  index0 <- unique(index)
  
  # 0. LOAD PACKAGES AND FILES
#   require(geepack)      # Not necessary 
  z <- resp.dim.ind
  K <- nrow(z) # number of response variables
  M <- ncol(z) # number of dimensions
  vnames <- vector("list",M)
  XX <- vector("list",M)
  
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  # 1. MAKE Y AND X FOR GEESE.FIT
  
  # Main effect of response variables
  X0 <- model.matrix(~ -1 + index, data = data)
  n <- nrow(X0)
  zz <- kronecker(matrix(1,(n/K),1),z)
  p <- matrix(NA,(M+1),1)
  p[1] <- 0
  p[2] <- ncol(X0)
  
  for (m in 1:length(formula)[1]){
      vnames[[m]] <- all.vars(formula(formula, lhs=m, rhs=m))
      vnames[[m]] <- vnames[[m]][2:length(vnames[[m]])]
      Xn <- model.matrix(formula(formula, lhs=m, rhs=m), data= data)
      
      # Take out the intercept
      Xn <- as.matrix(Xn[,2:ncol(Xn)])
      p[m+2] <- ncol(Xn)
      
      # Standardize continuous variables of X
      for (j in 1:ncol(Xn)){
      if (!all((Xn[,j]==0) | (Xn[,j]==1))) 
        Xn[,j] <- as.numeric(scale(Xn[,j],center = center,scale = scale))
      }
    
    XX[[m]] <- Xn
    colnames(Xn) <- paste(vnames[[m]],m,sep="")
    X0 <- as.matrix(cbind(X0,zz[,m]*Xn))
  }
  X <- X0
  
  # Define response
  a <- model.frame(formula(formula, lhs=1, rhs=1), data = data)
  a <- model.extract(a,"response")
  y <- model.matrix(~ -1 + a)
  
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  # 2. USE GEESE TO FIT THE MODEL
  out <- geese.fit(X,y,id, family = binomial(), corstr = Rusr)
  out <- c(out, list(call = scall, formula = formula))
  class(out) <- "geese"
  smv <- summary(out)
  coef.out <- as.matrix(smv$mean[1])
  npar <- dim(coef.out)[1]
  var_naive <- out$vbeta.naiv
  var_robust <- out$vbeta
  
  # QIC
  eta <- X%*%coef.out
  P <- exp(eta)/(1+ exp(eta))
  
  # Quasi Likelihood for binomial
  QL <- sum(y*log(P/(1 - P)) + log(1 - P))
	QL.null <- sum(y*log(mean(y)/(1 - mean(y)) + log(1 - mean(y))))
  QD <- -2*QL
	QD.null <- -2*QL.null
  QIC <- QD + 2*npar
    
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  # 2a. IF BOOTSTRAP is requested.  
  if(bootstrap){
    B <- bootstrap
    prgbar <- txtProgressBar(min=1, max=B, style=3)
    
    # Get code for clusbootglm
    data <- as.matrix(cbind(id,y,X))
    repl.pars  <- matrix(NA, B, nrow(smv$mean))
    cluster <- as.character(id)
    clusters <- unique(cluster)
    nc <- length(clusters)
    Obsno <- split(1:n, cluster)
    
    # Get a balanced bootstrap
    f <- matrix(clusters,length(clusters),B)
    ff <- matrix(f,prod(dim(f)),1)
    fff <- sample(ff)
    f <- matrix(fff,length(clusters),B)
    for (b in 1:B){
      setTxtProgressBar(prgbar, b)
      j <- f[,b]
      obs <- unlist(Obsno[j])
      outb <- geese.fit(X[obs,],y[obs],id[obs], b = smv$mean[,1] ,family = binomial(), corstr = "independence")
      outb <- c(outb, list(call = scall, formula = formula))
      class(outb) <- "geese"
      repl.pars[b,] <- summary(outb)$mean[,1]    
    }
    lo <- (1-conf.interval)/2
    hi <- 1 - (1-conf.interval)/2
    
    # Get nonparametric confidence interval
    npar95ci <- t(apply(repl.pars,2,quantile,probs=c(lo,hi)))
    sdcoefs <- apply(repl.pars,2,sd)
    thres <- qnorm(lo, mean = 0, sd = 1, lower.tail = F)
    
    # Get parametric confidence interval
    par95ci <- cbind(coef.out - thres * sdcoefs,coef.out + thres * sdcoefs)
    boot.sd <- sdcoefs
    boot.wald <- (coef.out/sdcoefs)**2
    boot.pval <- pchisq(boot.wald,1,lower.tail=FALSE)       
  }
  
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  # 3. TRANSFORM BACK TO DISTANCE MODEL
  
  # Define class points
  gamma0 <- -(smv$mean[1:K,1] + .5)
  gamma1 <- gamma0 + 1
  gamma <- cbind(z*gamma0, z*gamma1)
  gamma <- matrix(t(gamma), 2*K, M, byrow = T)
  if (is.null(resp.var.labels)) {
    rownames(gamma) <- rbind(paste("gamma0", 1:K, sep=""),paste("gamma1", 1:K, sep=""))  
  }
  else {
    rownames(gamma) <- rbind(paste(resp.var.labels, 0),paste(resp.var.labels, 1))  
  }
  colnames(gamma) <- paste("dim", 1:M, sep="")
  
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  
  # Compute subject points
  N <- matrix(NA,n,M)
  for (m in 2:(M+1)){
    N[,m-1] <- XX[[m-1]] %*% smv$mean[(sum(p[1:m])+1):sum(p[1:(m+1)]),1]
  } 
  colnames(N) <- paste("dim", 1:M, sep="")
  CorDim <- cor(N)    
  
  #--------------------------------------------------------------------------
  #--------------------------------------------------------------------------
  # 4. CREATE OUTPUT
	
	index0 <- as.factor(index0)
	index <- as.factor(index)
	
  coef.beta <- round(as.matrix(coef.out[-c(1:K), ]), 3)
  coef.gamma <- round(gamma, 3)
  
  if(bootstrap){      
    fit <- list(call = scall, formula=formula, coef.out = coef.out,
               coef.beta = coef.beta, coef.gamma = coef.gamma, 
               boot.sd = boot.sd, boot.wald = boot.wald, 
               npar95ci = npar95ci, par95ci = par95ci, boot.pval = boot.pval, 
               boot.coefs = colMeans(repl.pars), coef.repl = repl.pars, 
               Bootstrap.Samples =f, data = data, resp.dim.indic = resp.dim.ind, 
               X = X, y = y, XX = XX, K = K, subjects = N, 
    					 index0 = index0, index = index,
               M = M, names = vnames, p = p, npar = npar, 
    					 deviance = QD, deviance.null = QD.null,
    					 n = n, QIC = QIC, CorDim = CorDim, geepack.out = out, 
               var_naive = var_naive, var_robust=var_robust,
    					 fitted.values=P, geesummary = smv)
  }else{
    fit <- list(call = scall, formula=formula, coef.out = coef.out,
               coef.beta = coef.beta, coef.gamma = coef.gamma,
               data = data, resp.dim.indic = resp.dim.ind, X = X, y = y,
               XX = XX, subjects = N, 
    					 index0 = index0, index = index,
               M = M, names = vnames, p = p, npar = npar, 
    					 deviance = QD, deviance.null = QD.null,
    					 n = n, QIC = QIC, CorDim = CorDim, geepack.out = out, 
               var_naive = var_naive, var_robust=var_robust,
    					 fitted.values=P, geesummary = smv)     
  }
  
  class(fit) <- "mldm"
  
  fit
}
