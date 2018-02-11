\name{QIC.mldm}
\alias{QIC.mldm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quasi-Information Criterion 
}
\description{
Function for calculating the quasi-likelihood under the independence model information criterion (QIC) for one or several fitted multivariate logistic distance model(s) object from the mldm package.
}
\usage{
QIC.mldm(model_R, model_indep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model_R}{
an object of fitted MLD model under some working correlation structure (other than 'independence')
}
  \item{model_indep}{
an object of fitted MLD model under 'independence' correlation structure
}
}
\details{
Pan (2001) proposed the quasi-likelihood under the independence model criterion (QIC) as an extension of Akaike Information Criterion (AIC) to GEE. QIC is used to select a correlation structure, set of predictor variables, and dimensionality of the MLD models. 
}
\references{
Pan, W. (2001). Akaike's information criterion in Generalized Estimating Equations. \emph{Biometrics, 57}, 120-125.

Yu, H. T. and De Rooij, M. (2013). Model selection for trend vector model. \emph{Journal of Classification, 30}, 338-369.

Takane, Y. (1994). A review of applications of AIC in psychometrics. In H. Bozdogan (Ed.), \emph{Proceedings of the US/Japan modeling conference} (p. 379-403). Dordrecht: Kluwer Academic Publishers.
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

## fit MLD model, independence working correlation
out_indep = mldm.fit(formula=mF, index = Index, resp.dim.ind = Z, 
            data = NESDA, id = pident, scale=TRUE)

## fit MLD model, exchangeable working correlation
out_exch = mldm.fit(formula=mF, index = Index, resp.dim.ind = Z, 
            data = NESDA, id = pident, scale=TRUE, Rusr="exchangeable")

QIC.mldm(out_exch, out_indep)
}
