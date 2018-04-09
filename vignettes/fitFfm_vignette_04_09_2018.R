## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(concordance=TRUE)

## ----eval=FALSE----------------------------------------------------------
## library(devtools)
## install_github("sangeeuw/factorAnalytics")

## ----message=FALSE, warning=FALSE----------------------------------------
# load the package and its dependencies
library(factorAnalytics)
options(digits=3)

## ------------------------------------------------------------------------
methods(class="ffm")

## ------------------------------------------------------------------------
# load the dataset into the environment
data(Stock.df)
# get a list of the variable names
colnames(stock)
# time period covered in the data
range(stock[,"DATE"])
# number of stocks
length(unique(stock[,"TICKER"]))
# count stocks by GICS sector as of the last time period
stocklist<-subset(stock,DATE=="2003-12-31")
table(stocklist$GICS.SECTOR)

## ----tidy=TRUE-----------------------------------------------------------
args(fitFfm)

## ------------------------------------------------------------------------
# Single Factor Model
fit.single <- fitFfm(data=stock, asset.var="TICKER", ret.var="RETURN",
                     date.var="DATE", exposure.vars="BOOK2MARKET")

## ------------------------------------------------------------------------
class(fit.single)
names(fit.single)

## ------------------------------------------------------------------------
# print the fitted "ffm" object
fit.single

## ----fig.cap="Single factor model: Residual correlations", warning=FALSE, fig.show='asis', fig.width=6, fig.height=6----
# plot residual correlations for the single factor model
# default is to plot the 1st 6 assets
plot(fit.single, which=6, f.sub=1)
# GICS industry/sector classification (1st 6 stocks; penultimate time period)
subset(stock,DATE=="2003-11-28")[1:6,c("TICKER","GICS.INDUSTRY","GICS.SECTOR")]

## ------------------------------------------------------------------------
# Sector Factor Model
fit.sector <- fitFfm(data=stock, asset.var="TICKER", ret.var="RETURN",
                     date.var="DATE", exposure.vars="GICS.SECTOR")
# compare r2: single factor vs. sector model
summary(fit.single$r2)
summary(fit.sector$r2)
# compare avg. non-diagonal correlations: single factor vs. sector model
mean(cor(residuals(fit.single))[cor(residuals(fit.single))!=1])
mean(cor(residuals(fit.sector))[cor(residuals(fit.sector))!=1])

## ------------------------------------------------------------------------
# print the summary from the last period's fit
num.periods <- length(fit.sector$time.periods)
summary(fit.sector$factor.fit[[num.periods]])

## ----fig.cap="Sector model: Distribution of factor returns sorted by mean", fig.show='asis', fig.width=5.5, fig.height=3.5----
# plot distribution of factor returns by sector sorted by means
plot(fit.sector, which=1, colorset="black", f.sub=1:10, lwd=1, sort.by="mean")

## ------------------------------------------------------------------------
# Market + Sector Factor Model
fit.mkt.sector <- fitFfm(data=stock, asset.var="TICKER", ret.var="RETURN",
                         date.var="DATE", exposure.vars="GICS.SECTOR",
                         add.intercept=TRUE)

# coefficients (factor exposures) for first 10 assets
t(coef(fit.mkt.sector)[1:10,])

## ----fig.cap="Market + Sector model: Distribution of factor returns sorted by mean", fig.show='asis', fig.width=5.5, fig.height=3.5----
# plot distribution of factor returns by sector sorted by means
plot(fit.mkt.sector, which=1, colorset="black", f.sub=1:10, lwd=1, sort.by="mean")

## ------------------------------------------------------------------------
# Market + Sector Factor Model
fit.style.sector <- fitFfm(data=stock, asset.var="TICKER", ret.var="RETURN",
  date.var="DATE", exposure.vars=c("GICS.SECTOR","LOG.MARKETCAP","BOOK2MARKET"))

# check if average adjusted R-squared improved vs. pure sector model
# adjusted r2 = 1 - ((n-1)*(1-r2)/(n-p-1))
print(adj.r2_style.sector <- 1-((447-1)*(1-mean(fit.style.sector$r2))/(447-12-1)))
print(adj.r2_sector <- 1-((447-1)*(1-mean(fit.sector$r2))/(447-10-1)))

## ----fig.cap="Factor exposures from the last time period (1st 10 assets)", fig.show='asis', fig.width=8.5, fig.height=10----
plot(fit.style.sector, which=2, f.sub=1:12, a.sub=1:10)

## ----fig.cap="Time series of R-squared values", fig.show='asis', fig.width=6.5, fig.height=3----
plot(fit.style.sector, which=4, las=2)

## ----fig.cap="Time series of factor returns (2 style and 1 sector factors)", fig.show='asis', fig.width=7, fig.height=4.75----
plot(fit.style.sector, which=12, f.sub=1:3, las=2, legend.loc="bottom", cex.legend=0.75)

## ----fig.cap="Non-parametric density of residuals with normal overlaid for MSFT", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.style.sector, plot.single=TRUE, which=10, asset.name="MSFT")

## ----fig.cap="Non-parametric density of residuals with skew-t overlaid for MSFT", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.style.sector, plot.single=TRUE, which=11, asset.name="MSFT")

## ----fig.cap="Factor model return correlation (pairwise complete obs)", fig.show='asis', fig.width=5, fig.height=5----
fmCov(fit.style.sector)[1:6,1:6]
# factor model return correlation plot (for 1st 6 assets by default)
plot(fit.style.sector, which=8)

## ----fig.cap="Percentage factor contribution to SD"----------------------
decomp <- fmSdDecomp(fit.style.sector)
names(decomp)
# get the factor model standard deviation for 1st 6 assets
decomp$Sd.fm[1:6]
# get the component contributions to Sd for (1st 6 assets, relevant factors)
decomp$cSd[1:6, c(1,2,4,9)]
# get the marginal factor contributions to Sd (1st 6 assets, relevant factors)
decomp$mSd[1:6, c(1,2,4,9)]
# get the % component contributions to Sd (1st 6 assets, relevant factors)
decomp$pcSd[1:6, c(1,2,4,9)]
# plot the % component contributions to Sd (1st 6 assets, relevant factors)
plot(fit.style.sector, which=9, f.sub=c(1,2,4,9))

## ----fig.cap="Percentage factor contribution to VaR"---------------------
decomp1 <- fmVaRDecomp(fit.style.sector, type="normal", p=0.10)
names(decomp1)
# get the factor model value-at-risk for 1st 6 assets
decomp1$VaR.fm[1:6]
# print the number of VaR exceedences for 1st 6 assets
decomp1$n.exceed[1:6]
# plot the % component contributions to VaR (1st 6 assets, relevant factors)
plot(fit.style.sector, which=11, f.sub=c(1,2,4,9))

## ----fig.cap="Percentage factor contribution to ES"----------------------
decomp2 <- fmEsDecomp(fit.style.sector, type="normal")
names(decomp2)
# get the factor model expected shortfall for 1st 6 assets
decomp2$ES.fm[1:6]
# get the component contributions to ES for (1st 6 assets, relevant factors)
decomp2$cES[1:6, c(1,2,4,9)]
# plot the % component contributions to ES (1st 6 assets, relevant factors)
plot(fit.style.sector, which=10, f.sub=c(1,2,4,9))

## ----eval=FALSE----------------------------------------------------------
## ## S3 method for class "ffm"
## plot (x, which=NULL, f.sub=1:2, a.sub=1:6, plot.single=FALSE, asset.name,
##       colorset=c("royalblue","dimgray","olivedrab","firebrick", "goldenrod",
##                  "mediumorchid","deepskyblue","chocolate","darkslategray"),
##       legend.loc="topleft", las=1, lwd=2, maxlag=15, ...)

## ----eval=FALSE, results='hide'------------------------------------------
## plot(fit.sector)
## 
## # Make a plot selection (or 0 to exit):
## #
## #  1: Distribution of factor returns
## #  2: Factor exposures from the last period
## #  3: Actual and Fitted asset returns
## #  4: Time-series of R-squared values
## #  5: Residual variance across assets
## #  6: Scatterplot matrix of residuals, with histograms, density overlays,
## #     correlations and significance stars
## #  7: Factor Model Residual Correlation
## #  8: Factor Model Return Correlation
## #  9: Factor Contribution to SD
## # 10: Factor Contribution to ES
## # 11: Factor Contribution to VaR
## # 12: Time series of factor returns
## #
## # Selection:

## ----fig.cap="Actual (blue) and fitted (grey) factor model returns for the 1st 3 assets", fig.show='asis', fig.width=7.5, fig.height=6----
# Examples of group plots: looping disabled & no. of assets displayed = 4.
plot(fit.style.sector, which=3, a.sub=1:3, legend.loc=NULL, lwd=1)

## ----eval=FALSE, results='hide'------------------------------------------
## plot(fit.style.sector, plot.single=TRUE, asset.name="MSFT")
## 
## # Make a plot selection (or 0 to exit):
## #
## #  1: Actual and fitted asset returns
## #  2: Actual vs. fitted asset returns
## #  3: Residuals vs. fitted asset returns
## #  4: Residuals with standard error bands
## #  5: Time series of squared residuals
## #  6: Time series of absolute residuals
## #  7: SACF and PACF of residuals
## #  8: SACF and PACF of squared residuals
## #  9: SACF and PACF of absolute residuals
## # 10: Non-parametric density of residuals with normal overlaid
## # 11: Non-parametric density of residuals with skew-t overlaid
## # 12: Histogram of residuals with non-parametric density and normal overlaid
## # 13: QQ-plot of residuals
## #
## # Selection:

## ----fig.cap="Time series plot of residuals with standard error bands: MSFT", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.style.sector, plot.single=TRUE, asset.name="MSFT", which=4)

## ----fig.cap="SACF and PACF of absolute residuals: MSFT", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.style.sector, plot.single=TRUE, asset.name="MSFT", which=9)

## ----fig.cap="QQ-plot of residuals: MSFT", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.style.sector, plot.single=TRUE, asset.name="MSFT", which=13)
grid()

