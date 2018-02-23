
# load the functions
source("mldm.R")      
source("plot_mldm.R")
source("summary_mldm.R")
source("print_mldm.R")

# data - excercise
library(foreign)
#oefen = read.spss("oefen.sav", to.data.fram = T)
#attach(oefen)
NESDA = read.spss("NESDA_Long.sav", to.data.fram = T)
# attach(NESDA)


## -------------------------  ;
## Testing the DbMLR package  ;
## -------------------------  ;
# install.packages("E:/MyFolders/Laboratory/Packages/STAT/R/DevPackages/DbMLRpackage/Test-3/DbMLR_1.0.tar.gz", repos = NULL, type = "source")
# library(DbMLR)


## ---- Model (2 - a/d) -----
resp.dim.ind.2 = matrix(c(1,1,0,0,0,0,0,1,1,1),5,2,byrow = F)
# resp.dim.ind.2 = matrix(c(1,1,1,0,0,0,0,0,1,1),5,2,byrow = F)
# m1.2 = list(2)

## ALL
# m1.2[[1]] = Outcome ~ EDU + GEN + AGE + N + E + O + A + C
# m1.2[[2]] = Outcome ~ EDU + GEN + AGE + N + E + O + A + C

## Final
# m1.2[[1]] = Outcome ~ EDU + GEN + AGE + N + E 
# m1.2[[2]] = Outcome ~ EDU + GEN + AGE + N + E 

## extended formula
# require(Formula)
mf <- Outcome | Outcome ~ EDU + GEN + AGE + N + E | EDU + GEN + AGE + N + E 
mF <- Formula(mf)

# m1.2[[1]] = Outcome ~ Gender 
# m1.2[[2]] = Outcome ~ Edu + Gender 


# oefen
#m1.2[[1]] = resp ~ Sexe + neurot + extrave
#m1.2[[2]] = resp ~ Sexe + neurot + extrave

# fit MLDM
# out.2 = MLDM(formula=m1.2, index = Index2, resp.dim.ind = resp.dim.ind.2, 
#               data = NESDA, id = pident, scale=TRUE)
# require(geepack)
out.2 = mldm(formula=mF, index = Index2, resp.dim.ind = resp.dim.ind.2, 
              data = NESDA, id = pident, scale=FALSE)

# oefen
# out.2 = DbMLR(formula=m1.2, index = Index, resp.dim.ind = resp.dim.ind.2, 
#              data = NESDA, id = pident, scale=TRUE)

# print(out.2)
summary(out.2)

# Biplot
plot(out.2)

# fit MLDM with clustered bootstrap
out.3 = MLDM(formula=m1.2, index = Index2, resp.dim.ind = resp.dim.ind.2, 
              data = NESDA, id = pident, scale=TRUE,
              bootstrap=100)

#print.dME(out.3, bootstrap=TRUE, boot.nonparam=TRUE)
#print.dME(out.3, bootstrap=TRUE)
print.dME(out.3)
summary.dME(out.3, bootstrap=TRUE)
summary.dME(out.3, bootstrap=TRUE, boot.nonparam=TRUE)

# Biplot
plot.dME(out.3)

## Test: only intercept model
m1.2[[1]] = Outcome ~ 1
m1.2[[2]] = Outcome ~ 1

out.4 = DbMLR(formula=m1.2, index = Index, resp.dim.ind = resp.dim.ind.2, 
              data = NESDA, id = pident, scale=TRUE)




