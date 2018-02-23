## reference:
## https://www.rstudio.com/wp-content/uploads/2015/03/devtools-cheatsheet.pdf
## https://support.rstudio.com/hc/en-us/articles/200486508-Building-Testing-and-Distributing-Packages

## load library
library(devtools)
library(roxygen2)

## create test dataset
source("../create dataset.R")

##########################
## Create package.      ##
##########################

## step 1. create package using RStudio. No need to load the functions becuase it will be done manually
# devtools::create("./mldm")

## step 2: manually, put your functions in the R folder

## step 3: load dataset using pkg-devtools
devtools::use_data(NESDA, pkg = "./../mldm/")

## step 4: Manually, edit both the DESCRIPTION and the NAMESPACE files.

## step 5: load
devtools::load_all()

## step 6: create manual for your R functions
roxygen2::roxygenise(package.dir = "./")

## step 7: Build package using "Build & Reload" menu in RStudio

## step 8: Check package using pkg-devtools
devtools::check()

## step 9: Build binary package
## Go to "More ->Build Binary package"






