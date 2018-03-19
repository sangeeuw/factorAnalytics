## ----message=FALSE-------------------------------------------------------
library(factorAnalytics)
options(digits=3)

## ------------------------------------------------------------------------
data(managers)
colnames(managers)
range(index(managers))

## ------------------------------------------------------------------------
sub.data <- managers[,c(2, 5, 6),drop=FALSE]
head(sub.data)
factors.data  <- managers[,c(3, 8, 9, 10),drop=FALSE]

## ----tidy=TRUE-----------------------------------------------------------
args(fmmc)

## ------------------------------------------------------------------------
objs <- fmmc(sub.data, factors.data, parallel=FALSE, variable.selection="subsets")

## ------------------------------------------------------------------------
es <- function(r, alpha = 0.05) {
    r <- sort(r)
    cutoff <- ifelse( alpha == 0, 1, round(alpha*length(r)))
    -1/cutoff * sum(r[which((1:length(r)) < cutoff)])
}

## ------------------------------------------------------------------------
result <- fmmc.estimate.se(objs, fun = es, se= TRUE, nboot = 50, 
                           parallel = FALSE)
result

