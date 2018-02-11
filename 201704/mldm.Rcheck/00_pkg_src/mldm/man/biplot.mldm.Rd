\name{biplot.mldm}
\alias{biplot.mldm}
\title{
Biplot for the MLD model
}
\description{
The class points for the response variables and subject points, are projected in a two dimensional Euclidean space. The predictors are also diplayed using vectors.
}

\usage{
biplot(object, myX = c(-4, 4), myY = c(-4, 4), resp.var.labels = NULL, ...)
}
\arguments{
  \item{object}{
an object of fitted MLD model
}
  \item{myX}{
limits for the horizontal axis
}
  \item{myY}{
limits for the vertical axis  
}
  \item{resp.var.labels}{
labels for the response variables
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\references{
De Leeuw, J. (2006). Pseudo-voronoi diagrams for multicategory exponential representations. UCLA: Statistics Preprints, 463.

Gabriel, K. R. (1971). The biplot graphical display of matrices with application to principal component analysis. \emph{Biometrika, 58}, 453-467.

Gower, J. C., & Hand, D. J. (1996). Biplots. London: Chapman and Hall.

Gower, J. C., Lubbe, S., & Le Roux, N. (2011). Understanding Biplots. Chichester: John Wiley and Sons Ltd.
}
\author{
Hailemichael M. Worku and Prof. Mark de Rooij

Maintainer: Hailemichael M. Worku <h.m.worku@fsw.leidenuniv.nl> <hmetiku@yahoo.com>
}
\examples{
data(NESDA)
# attach(NESDA)

## specify response indicator matrix
Z <- matrix(c(1,1,0,0,0,0,0,1,1,1),5,2,byrow = FALSE)

## specify model formula
require(Formula)
mf <- Outcome | Outcome ~ EDU + GEN + AGE + N + E | EDU + GEN + AGE + N + E 
mF <- Formula(mf)

## fit MLD model
fit = mldm.fit(formula=mF, index = Index, resp.dim.ind = Z, 
            data = NESDA, id = pident, scale=TRUE)

biplot(fit)

}