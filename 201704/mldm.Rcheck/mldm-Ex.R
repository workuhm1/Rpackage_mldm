pkgname <- "mldm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "mldm-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('mldm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("NESDA")
### * NESDA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NESDA
### Title: The NESDA Data
### Aliases: NESDA
### Keywords: datasets

### ** Examples

data(NESDA)
## maybe str(NESDA) ; plot(NESDA) ...



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NESDA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("QIC.mldm")
### * QIC.mldm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: QIC.mldm
### Title: Quasi-Information Criterion
### Aliases: QIC.mldm

### ** Examples


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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("QIC.mldm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("biplot.mldm")
### * biplot.mldm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: biplot.mldm
### Title: Biplot for the MLD model
### Aliases: biplot.mldm

### ** Examples

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




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("biplot.mldm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mldm.fit")
### * mldm.fit

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mldm.fit
### Title: Fits Multivariate Logistic Distance Models
### Aliases: mldm.fit

### ** Examples


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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mldm.fit", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
