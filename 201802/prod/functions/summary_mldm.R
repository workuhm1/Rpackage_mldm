#' Summary of model fit.
#' 
#' @param object An object of fitted model.
#' @param bootstrap Boolean value (TRUE/FALSE) to indicate a Clustered Bootstrap procedure is used for obtaining model standard errors.
#' @param boot.nonparam Boolean value (TRUE/FALSE) to ask for non-parametric confidence intervals.
#' 
#' 

summary.mldm <- function (object, bootstrap = FALSE, boot.nonparam = FALSE, ...) {
  cat("\n Call: \n")
  print(object$call)
  cat("\n Formula: \n")
  print(object$formula)
  cat("\n Class Coordinates: \n")
  print(object$coef.gamma)
  
  if(bootstrap){    
    if(boot.nonparam){
      boot.mean <- round(cbind(object$coef.out, object$boot.sd, object$boot.wald, 
                              object$boot.pval, object$npar95ci), 4)
      colnames(boot.mean) <- NULL
      colnames(boot.mean) <- c("estimate", "boot.se", "boot.wald", "p", 
                               "boot.ll", "boot.ul")
        
      for(m in 2:(object$M + 1)){
        cat("\n")
        print(paste("Regression coefficients for Dimension", m-1))    
        table <- boot.mean[(sum(object$p[1:m])+1):sum(object$p[1:(m+1)]),]
        print(table)
        cat("\n")
      }
    }
    else{
      boot.mean <- round(cbind(object$coef.out, object$boot.sd, object$boot.wald, 
                              object$boot.pval, object$par95ci), 4)
      colnames(boot.mean) <- NULL
      colnames(boot.mean) <- c("estimate", "boot.se", "boot.wald", "p", 
                               "boot.ll", "boot.ul")
        
      for(m in 2:(object$M + 1)){
        cat("\n")
        print(paste("Regression coefficients for Dimension", m-1))    
        table <- boot.mean[(sum(object$p[1:m])+1):sum(object$p[1:(m+1)]),]
        print(table)
        cat("\n")
      }
    }
  }    
  else{
    for(m in 2:(object$M + 1)){
      cat("\n")
      print(paste("Regression coefficients for Dimension", m-1))
      table <- round(object$geesummary$mean[(sum(object$p[1:m])+1):sum(object$p[1:(m+1)]),], 4)
      print(table)
    }
  }
  
  cat("\n Correlation among dimensions: \n")
  print(round(object$CorDim, 2))
  
  cat("\n QIC: \n")
  print(object$QIC)
}
