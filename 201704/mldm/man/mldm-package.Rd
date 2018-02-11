\name{mldm-package}
\alias{mldm}
\docType{package}
\title{
Multivariate Logistic Distance Model for analyzing clustered binary data in R
}
\description{This package fits a Multivariate Logistic Distance (MLD) model proposed for analyzing multivariate binary data. The MLD model extends the marginal model for clustered binary data within a distance framework. The results of the MLD model can be graphically represented in a biplot, which enhance interpretation of the model.
}
\details{
\tabular{ll}{
Package: \tab mldm\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-05-20\cr
License: \tab GPL\cr
}

The distance model (De Rooij, 2009) for a single binary response variable, \eqn{y}, is defined by
  \deqn{p(y_{i} = 1) = \frac{e^(-0.5 \delta_{i1})}{
                    e^(-0.5 \delta_{i0}) + e^(-0.5 \delta_{i1})}}{
                    p(y_{i} = 1) = exp(-0.5 \delta_{i1})/
                    [exp(-0.5 \delta_{i0}) + exp(-0.5 \delta_{i1})]
                    },
where \eqn{\delta_{.}} is the squared Euclidean distance in one dimensional space between a point representing the subject and a point representing the categories, i.e., \eqn{\delta_{i0} = (\eta_{i} - \gamma_{0})^2} and \eqn{\delta_{i1} = (\eta_{i} - \gamma_{1})^2} in which \eqn{\gamma_{1}} and \eqn{\gamma_{0}} represent coordinate of the two categories. The subject point, \eqn{\eta}, is defined as linear combinations of the predictor variable, i.e., \eqn{\eta_{i} = \beta_{0} + X_{i}\beta_{1}}.

The package has the capability of handling both clustered bootstrap method and sandwich estimators, for adjusting the standard errors of model parameters. The most important functions are shown below under the \bold{See also}. 

%\deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}}{p(x) = lambda^x exp(-lambda)/x!}

}

\author{
Hailemichael M. Worku and Prof. Mark de Rooij

Maintainer: Hailemichael M. Worku <h.m.worku@fsw.leidenuniv.nl> <hmetiku@yahoo.com>
}
\references{
Worku, H. M. and De Rooij, M. (submitted, 2015). A Multivariate Logistic Distance model for analyzing multiple binary responses.

De Rooij, M. (2009). Ideal point discriminant analysis with a special emphasis on visualization. \emph{Psychometrika}, 74, 317-330.

De Rooij, M. and Worku, H. M. (2012). A warning concerning the estimation of multinomial logistic models with correlated responses in SAS. \emph{Computer methods and Programs in Biomedicine}, 107(2), 341-346.   

Worku, H. M. and De Rooij, M. (submitted, 2015). Properties of ideal point classification models for bivariate binary data.

Borg, I. and Groenen, P. J. F. (2005). \emph{Modern multidimensional scaling: Theory and applications} (Second ed.). New York: Springer.

Agresti, A. (2002) \emph{Categorical data analysis} (Second ed.). New York: John Wiley and Sons.

Liang, K.-Y. and Zeger, S.L. (1986). Longitudinal data analysis using generalized linear models. \emph{Biometrika}, 73, 13 - 22.

Ziegler, A., Kastner, C., and Blettner, M. (1998). The generalized estimating equations: An annotated bibliography. \emph{Biometrical Journal}, 40 (2), 115-139.

Pan, W. (2001). Akaike's information criterion in Generalized Estimating Equations. \emph{Biometrics}, 57, 120-125.

Halekoh, U., Hojsgaard, S., and Yan, J. (2006). The R package geepack for Generalized Estimating Equations. \emph{Journal of Statistical Software}, 15(2).

Gower, J. C. and Hand, D. J. (1996). \emph{Biplots}. London: Chapman and Hall.

Gower, J. C., Lubbe, S., and Le Roux, N. (2011). \emph{Understanding biplots}. Chichester: John Wiley and Sons Ltd.

}
\keyword{ package }
\seealso{
	\code{\link[mldm]{mldm.fit}, \link[mldm]{biplot.mldm}, \link[mldm]{QIC.mldm}}
}
