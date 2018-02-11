\name{mldm.fit}
\alias{mldm.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fits Multivariate Logistic Distance Models
}
\usage{
mldm.fit(formula, index, resp.dim.ind, resp.var.labels = NULL, data, 
            id, center = TRUE, scale = FALSE, Rusr="independence", 
                conf.interval = 0.95, bootstrap = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
glm formula for every dimension 
}
  \item{index}{
response variables indicator
}
  \item{resp.dim.ind}{
indicates for every response to which dimension it belongs. It assumes the same order of the responses for every subjects. Moreover, it also assumes complete data
}
  \item{resp.var.labels}{
labels for the response variables
}
  \item{data}{
is a data frame
}
  \item{id}{
subjects identification number
}
  \item{center}{
whether the predictor variables should be centered (demeaned)
}
  \item{scale}{
whether the predictor variables should be transformed to z-scores
}
\item{Rusr}{
a character string specifying the correlation structure. The following are allowed: "independence", "exchangeable", "ar1", "unstructured", and "userdefined". The default is "independence".
}
  \item{conf.interval}{
level of confidence interval when a clustered bootstrap technique is used to correct for biased standard errors of the parameters
}
  \item{bootstrap}{
whether a clustered bootstrap procedure should be employed (in that case, specify the number of bootstraps)
}
}

\details{
The mldm.fit function fits multivariate logistic distance models on clustered binary data. More detailed discussion about the MLD models can be found in Worku and De Rooij (submitted, 2015). The function provides users with either clustered bootstrap technique or sandwitch estimators to correct the biased standard errors. By default, the function uses the sandwitch estimator procedure.
}
\value{
\code{mldm.fit} returns an object of class "mldm" which inherits from the class "geeglm", "gee", and "glm".

The function \code{\link{summary}} (i.e., \code{\link{summary.mldm}}) can be used to obtain or print a summary of the fitted model results.

An object of class "mldm" is a list containing at least the following components:

\item{coef.beta}{
    regression coefficients per dimension
}
\item{coef.gamma}{
    class coordinates/points
}
\item{npar}{
    number of parameters of the fitted model
}
\item{deviance}{
    deviance of the fitted mode, i.e., minus twice the maximized log-likelihood
}
\item{deviance.null}{
    deviance for the null model, comparable with the \code{deviance}. The null       
        model contains only the intercept
}
\item{n}{
    the total number of observations used in the analysis
}
\item{QIC}{
    quasi information criterian for the fitted model
}
\item{var.naive}{
    the naive variance-covariance matrix, used for calculating model-based 
        standard errors
}
\item{var.robust}{
    the robust variance-covariance matrix, used for calculating emperically
        -corrected standard errors, i.e., sandwich estimator
}
\item{y}{
    the \code{y} vector used
}
\item{X}{
    the model design matrix
}
\item{call}{
    the matched call
}
\item{formula}{
    the formula supplied by the user
}


}


\references{
Worku, H. M., & De Rooij, M. (submitted, 2015). A Multivariate logistic distance model for analyzing multiple binary responses.

Worku, H. M. & De Rooij, M. (submitted, 2015). Properties of ideal point classification models for bivariate binary data.

Liang, K.-Y., & Zeger, S.L. (1986). Longitudinal data analysis using generalized linear models. Biometrika, 73, 13 - 22.

De Rooij, M. & Worku, H. M. (2012). A warning concerning the estimation of multinomial logistic models with correlated responses in SAS. Computer
Methods and Programs in Biomedicine, 107 (2), 341-346.

Halekoh, U., Hojsgaard, S., and Yan, J. (2006). The R package geepack for Generalized Estimating Equations. \emph{Journal of Statistical Software}, 15(2).

Sherman, M. & Le Cessie, S. (1997). A comparison between bootstrap methods and generalized estimating equations for correlated outcomes in generalized linear models. Simulation and Computation, 26 , 901-925.

Ziegler, A., Kastner, C., & Blettner, M. (1998). The generalized estimating equations: An annotated bibliography. Biometrical Journal, 40 (2), 115-139.

}
\author{
Hailemichael M. Worku and Prof. Mark de Rooij

Maintainer: Hailemichael M. Worku <h.m.worku@fsw.leidenuniv.nl> <hmetiku@yahoo.com>
}
\examples{

data(NESDA)
#attach(NESDA)

## specify response indicator matrix
Z = matrix(c(1,1,0,0,0,0,0,1,1,1),5,2,byrow = FALSE)

## specify model formula, for each dimension
require(Formula)
mf <- Outcome | Outcome ~ EDU + GEN + AGE + N + E | EDU + GEN + AGE + N + E 
mF <- Formula(mf)

## fit MLD model
out = mldm.fit(formula=mF, index = Index, resp.dim.ind = Z, 
            data = NESDA, id = pident, scale=TRUE)

out
}