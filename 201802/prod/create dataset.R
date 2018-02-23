
###############################################
## Resamle 600 subjects from NESDA dataset.  ##
###############################################

library(foreign)
NESDA_orig <- read.spss("../../../../datasets/NESDA_Long.sav", to.data.fram = T)
NESDA_orig <- NESDA_orig[, -c(10)]
colnames(NESDA_orig)[11] <- "Index"

set.seed(123)
nbr_cluster <- 600
samp_id <- sample(x = unique(NESDA_orig$pident), nbr_cluster, replace = FALSE)
samp_id <- c(rep(samp_id, each=5))
datasample <- NESDA_orig[NESDA_orig$pident %in% c(samp_id), ]
NESDA <- datasample

colnames(NESDA)[1] <- "pident2"
NESDA$pident <- rep(1:nbr_cluster, each=5); NESDA <- NESDA[, -c(1)]
head(NESDA)

## clear temp datasets
rm(datasample)
rm(NESDA_orig)
rm(nbr_cluster)
rm(samp_id)



