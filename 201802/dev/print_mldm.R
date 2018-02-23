print.mldm <-
function (object, ...) 
{
  cat("\n Call: \n")
  print(object$call, row.names=FALSE)
  cat("\n Formula: \n")
  print(object$formula, row.names=FALSE)
  cat("\n Residual Deviance: ") 
  print(object$deviance.null, row.names=FALSE)
  cat("\n QIC: ")
  print(object$QIC, row.names=FALSE)
}
