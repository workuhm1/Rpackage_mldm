## -------------------------------------------------------------------- ;
## An R-package for fitting Multivariate Logistic Distance (MLDM) Model ;
##      \copyright. HM Worku & de Rooij (21-Apr-2017)                   ;
## -------------------------------------------------------------------- ;

## Environmental settings
setwd(dir = "/Users/workuhm/Desktop/PhD Folders_bkp_20170310/Thesis_20170310/mldm_package_paper_20170419/package_dev_area/Draft_201704")

## we need to source at least one function before creating pkg skeleton
# source("mldm.R")
# source("print_mldm.R")
# source("summary_mldm.R")
# source("plot_mldm.R")

source("mldm.fit.R")
source("print.mldm.R")
source("summary.mldm.R")
source("biplot.mldm.R")
source("QIC.mldm.R")


# devtools::load_all()

## load data before pkg skeleton, otherwise no 'data' folder is created in the skeleton
library(foreign)
NESDA_orig <- read.spss("NESDA_Long.sav", to.data.fram = T)
NESDA_orig <- NESDA_orig[, -c(10)]
colnames(NESDA_orig)[11] <- "Index"

## Get random sample of 600 subjects from original NESDA data
# library(sampling)
# 
# nbr_cluster <- 2
# samplecluster <- cluster(NESDA_orig, clustername=c("pident"), size=nbr_cluster, method="srswr")
# datasample <- getdata(NESDA_orig, samplecluster)
# NESDA <- datasample[, -c(13,14,15)]

set.seed(123)
nbr_cluster <- 600
samp_id <- sample(x = unique(NESDA_orig$pident), nbr_cluster, replace = FALSE)
samp_id <- c(rep(samp_id, each=5))
datasample <- NESDA_orig[NESDA_orig$pident %in% c(samp_id), ]
NESDA <- datasample

colnames(NESDA)[1] <- "pident2"
NESDA$pident <- rep(1:nbr_cluster, each=5); NESDA <- NESDA[, -c(1)]
head(NESDA)


## Step-1: create the package files
package.skeleton(name="mldm")
# package.skeleton(name = "mldm", environment = .GlobalEnv, 
#                  force = FALSE, code_files = character())

## Step-2: Edit the package files - DESCRIPTION
## Step-2: Edit the package files - NAMESPACE
## Step-2: Edit the package files - man files

## Step-2: Edit the package files - adding data
# save the data object to a file
#save(NESDA, file="./MLDM/data/NESDA.rda")

# create a help file
#prompt(NESDA, "./MLDM/man/NESDA")    # no need to define the file extension 


## Step-2: Edit the package files - adding functions
#source("MDLMfn.R")
#save(summary.dME, file="./MLDM/R/MLDM.R")
#prompt(MLDM, "./MLDM/man/MLDM", force.function=TRUE)
#prompt(MLDM, "./MLDM/man/MLDM")

## step-3: Build, Check & Install the package
# Then, at the command line: 
R CMD build MLDM 
R CMD check --as-cran MLDM_1.0.tar.gz 



