
## Install mldm package
# install.packages("<package path>/mldm_0.84.tgz", repos = NULL, type = .Platform$pkgType)

## load package
library(mldm)

##################################################
## Run example (type in command: ?mldm.fit).    ##
##################################################

## Load dataset
data(NESDA)

## Specifiy response indicator matrix 
Z <- matrix(c(1,1,0,0,0,0,0,1,1,1), 5, 2, byrow = FALSE)

## Specify model formula
mf <- Outcome | Outcome ~ EDU + GEN + AGE + N + E | EDU + GEN + AGE + N + E
mf <- Formula(mf)

## Fit MLD model
fit1 <- mldm.fit(formula = mf, index = Index, resp.dim.ind = Z,
                 data = NESDA, id = pident, scale = TRUE)

## display result
fit1

## biplot
biplot(fit1)

#####################################
## Clustered bootstrap example.    ##
#####################################
## fit model
fit_boot <- mldm.fit(formula = mf, index = Index, resp.dim.ind = Z,
                     data = NESDA, id = pident, scale = TRUE, 
                     bootstrap = 10)

## display result
summary(fit_boot, bootstrap = TRUE)


