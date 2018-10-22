# Online Applied Data Mining, IUB, 2017
# Case Study: Predicting Stock Market Returns
# Data Mining with R by Luis Torgo
# Modified by Hasan Kurban 
########################################## Section 1: The Available Data ########################################## 
#book package
install.packages("DMwR2")
library(DMwR2)
#load the implementation of the time series class that we use to store the daily quotes data
install.packages("xts")
library(xts)
#GSPC is an object of xts class that contains the daily quotes for S&P500
#GSPC contains roughly 45 years of daily quotes
data(GSPC, package="DMwR2")
#size of data
dim(GSPC)
#first observation
first(GSPC)
#last observation
last(GSPC)
#variables
names(GSPC)

#### sub-section:  Reading the Data from the CSV File:
# the csv file can be obtained from the book page 
#load the implementation of the time series class that we use to store the daily quotes data
require(xts)
#read the data and create an xts object with the data
#read.zoo(): read a CSV file and transforms the data into a zoo object 
#assuming the first column contains the time tags
#as.xts(): coerces the resulting object into an object of class xts
GSPC <- as.xts(read.zoo("sp500.csv", header = TRUE))


#### sub-section:  Getting the Data from the Web
#quantmod:Package that contains implementation of several functions
#for financial data analysis. 
install.packages("quantmod")
library(quantmod)
#more information about the package
?quantmod 
# load the data using getSymbols() function of quantmod package
GSPC <- getSymbols("^GSPC",auto.assign=FALSE)
?getSymbols #more information about the function
GSPC <- getSymbols("^GSPC",from="1970-01-02",to="2016-01-25",auto.assign=FALSE)

########################################## Section 2: Defining the Prediction Tasks ########################################## 
require(ggplot2)
require(grid)
require(DMwR2) 
require(xts)
require(quantmod)
data(GSPC, package="DMwR2")

#### sub-section:  What to Predict?
#implementation of an indicator of tendency -- For more information, refer to the pages 244-246
T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) {
  v <- apply(HLC(quotes), 1, mean)
  v[1] <- Cl(quotes)[1]
  
  r <- matrix(NA, ncol = n.days, nrow = NROW(quotes))
  for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x)
  
  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
  
  if (is.xts(quotes)) xts(x, time(quotes)) else x
}

#to draw candlestick of stock quotes
candleChart(last(GSPC,'3 months'),theme='white', TA=NULL)
avgPrice <- function(p) apply(HLC(p),1,mean)
addAvgPrice <- newTA(FUN=avgPrice,col=1,legend='AvgPrice')
addT.ind <- newTA(FUN=T.ind,col='red', legend='tgtRet')
addAvgPrice(on=1) 
addT.ind()

#### sub-section:  Which Predictors?
install.packages("TTR") #technical indicators package
library(TTR)
#some of the indicators in the TTR package. Refer to page 248 for more information
# These indicators will be used to make trading decisions
myATR        <- function(x) ATR(HLC(x))[,'atr']
mySMI        <- function(x) SMI(HLC(x))[, "SMI"]
myADX        <- function(x) ADX(HLC(x))[,'ADX']
myAroon      <- function(x) aroon(cbind(Hi(x),Lo(x)))$oscillator
myBB         <- function(x) BBands(HLC(x))[, "pctB"]
myChaikinVol <- function(x) Delt(chaikinVolatility(cbind(Hi(x),Lo(x))))[, 1]
myCLV        <- function(x) EMA(CLV(HLC(x)))[, 1]
myEMV        <- function(x) EMV(cbind(Hi(x),Lo(x)),Vo(x))[,2]
myMACD       <- function(x) MACD(Cl(x))[,2]
myMFI        <- function(x) MFI(HLC(x),  Vo(x))
mySAR        <- function(x) SAR(cbind(Hi(x),Cl(x))) [,1]
myVolat      <- function(x) volatility(OHLC(x),calc="garman")[,1]


#Using random forest to estimate importance of the variables -- Feature selection
install.packages("randomForest")
library(randomForest)
# Specify the data
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) + 
                             myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) + 
                             myBB(GSPC)  + myChaikinVol(GSPC) + myCLV(GSPC) + 
                             CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + 
                             myVolat(GSPC)  + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
                             mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
set.seed(1234)
# Random Forests model
rf <- buildModel(data.model,method='randomForest',
                 training.per=c("1995-01-01","2005-12-30"),
                 ntree=1000, importance=TRUE)


ex.model <- specifyModel(T.ind(IBM) ~ Delt(Cl(IBM), k = 1:3))
data <- modelData(ex.model, data.window = c("2009-01-01",  "2009-08-10"))

# Variable importance according to the the random forest
varImpPlot(rf@fitted.model, type = 1)

# Selecting the most important features using importance() function
imp <- importance(rf@fitted.model, type = 1)
rownames(imp)[which(imp > 30)]

#Final data set
data.model <- specifyModel(T.ind(GSPC) ~ myATR(GSPC) + mySMI(GSPC) +  myADX(GSPC) + 
                             myAroon(GSPC) + myEMV(GSPC) + myVolat(GSPC) + 
                             myMACD(GSPC) + myMFI(GSPC) + mySAR(GSPC) + 
                             runMean(Cl(GSPC)) + runSD(Cl(GSPC)))


#### sub-section:  The Prediction Tasks -- Buy, Sell, Hold: Pages  251-253

## The regression task
Tdata.train <- as.data.frame(modelData(data.model,
                                       data.window=c('1970-01-02','2005-12-30')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model,
                                              data.window=c('2006-01-01','2016-01-25'))))
Tform <- as.formula('T.ind.GSPC ~ .')
## The classification task
buy.thr <- 0.1
sell.thr <- -0.1
Tdata.trainC <- cbind(Signal=trading.signals(Tdata.train[["T.ind.GSPC"]],
                                             buy.thr,sell.thr),
                      Tdata.train[,-1])
Tdata.evalC <-  cbind(Signal=trading.signals(Tdata.eval[["T.ind.GSPC"]],
                                             buy.thr,sell.thr),
                      Tdata.eval[,-1])
TformC <- as.formula("Signal ~ .")


########################################## Section 3: The Prediction Models ########################################## 
#### sub-section:  How Will the Training Data Be Used? -- The Modeling Tools
# A. Artificial Neural Networks -- ANNs
set.seed(1234)
library(nnet)
## The first column is the target variable
# scale for normalizing the data
norm.data <- data.frame(T.ind.GSPC=Tdata.train[[1]],scale(Tdata.train[,-1]))
#fit a ANN to data
nn <- nnet(Tform, norm.data[1:1000, ], size = 5, decay = 0.01, 
           maxit = 1000, linout = TRUE, trace = FALSE)
#use the model to make predictions
preds <- predict(nn, norm.data[1001:2000, ])
#Measure the performance of the ANN model -- nn
sigs.nn <- trading.signals(preds,0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"], 0.1, -0.1)
sigs.PR(sigs.nn,true.sigs)

# B. SVM
set.seed(1234)
library(e1071)
# train an SVM 
sv <- svm(Tform, Tdata.train[1:1000, ], gamma = 0.001, cost = 100)
# use the trained SVM to make predictions
s.preds <- predict(sv, Tdata.train[1001:2000, ])
# measure the performance of the model
sigs.svm <- trading.signals(s.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"], 0.1, -0.1)
sigs.PR(sigs.svm, true.sigs)

# C. SVM and classification
install.packages("kernlab")
library(kernlab)
#training
ksv <- ksvm(Signal ~ ., Tdata.trainC[1:1000, ], C = 10)
#testing
ks.preds <- predict(ksv, Tdata.trainC[1001:2000, ])
# performance of the moel
sigs.PR(ks.preds, Tdata.trainC[1001:2000, 1])


# D. Multivariate Adaptive Regression Splines
install.packages("earth")
library(earth)
#training
e <- earth(Tform, Tdata.train[1:1000, ])
#testing
e.preds <- predict(e, Tdata.train[1001:2000, ])
#performance of the model
sigs.e <- trading.signals(e.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"],  0.1, -0.1)
sigs.PR(sigs.e, true.sigs)
# to get more information about the model
summary(e)
# importance of the variables (feature selection) using model "e"
evimp(e, trim=FALSE)

########################################## Section 4: From Predictions into Actions ########################################## 

#### sub-section:  Putting Everything Together: A Simulated Trader

# First, user-defined trading policy function: trading strategy -- Pages 265 - 271
policy.1 <- function(signals,market,opened.pos,money,
                     bet=0.2,hold.time=10,
                     exp.prof=0.025, max.loss= 0.05
)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] == 'b' && !nOs) {
    quant <- round(bet*money/Cl(market)[d],0)
    if (quant > 0) 
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1+exp.prof),
                                         Cl(market)[d]*(1-max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
    
    # ii) short positions  
  } else if (signals[d] == 's' && !nOs) {
    # this is the nr of stocks we already need to buy 
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                               "N.stocks"])*Cl(market)[d]
    quant <- round(bet*(money-need2buy)/Cl(market)[d],0)
    if (quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1-exp.prof),
                                         Cl(market)[d]*(1+max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
  }
  
  # Now lets check if we need to close positions
  # because their holding time is over
  if (nOs) 
    for(i in 1:nOs) {
      if (d - opened.pos[i,'Odate'] >= hold.time)
        orders <- rbind(orders,
                        data.frame(order=-opened.pos[i,'pos.type'],
                                   order.type=1,
                                   val = NA,
                                   action = 'close',
                                   posID = rownames(opened.pos)[i]
                        )
        )
    }
  
  orders
}

# Second, user-defined trading policy function: trading strategy -- Pages 265 - 271
policy.2 <- function(signals,market,opened.pos,money,
                     bet=0.2,exp.prof=0.025, max.loss= 0.05
)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] == 'b') {
    quant <- round(bet*money/Cl(market)[d],0)
    if (quant > 0) 
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1+exp.prof),
                                         Cl(market)[d]*(1-max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
    
    # ii) short positions  
  } else if (signals[d] == 's') {
    # this is the money already committed to buy stocks
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                               "N.stocks"])*Cl(market)[d]
    quant <- round(bet*(money-need2buy)/Cl(market)[d],0)
    if (quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1-exp.prof),
                                         Cl(market)[d]*(1+max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
  }
  
  orders
}

## Trading using the policy functions
## Select a sample of the data and train an SVM and use it to make prediction for a subsequent period
## Train and test periods
start <- 1
len.tr <- 1000
len.ts <- 500
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)
## getting the quotes for the testing period
data(GSPC)
date <- rownames(Tdata.train[start+len.tr,])
marketTP <- GSPC[paste(date,'/',sep='')][1:len.ts]
## learning the model and obtaining its signal predictions for the test period
s <- svm(Tform, Tdata.train[tr,], cost=10,gamma=0.01)
p <- predict(s, Tdata.train[ts,])
sig <- trading.signals(p, 0.1, -0.1)
## now using the simulated trader during the testing period
t1 <- trading.simulator(marketTP, signals=sig, policy.func='policy.1',
                        policy.pars=list(exp.prof=0.05,bet=0.2,hold.time=30))

## more information about t1
t1 
summary(t1) 

## Obtain a series economic indicators of the perfomance 
tradingEvaluation(t1)  

## A graphical overview of the performance of the trader
plot(t1,marketTP, theme = "white",  name = "SP500")

## Use the second policy for trading
t2 <- trading.simulator(marketTP, sig, "policy.2", list(exp.prof = 0.05, bet = 0.3))
summary(t2)
tradingEvaluation(t2)

## Repeating the experiment for different training and testing periods
start <- 2000
len.tr <- 1000
len.ts <- 500
tr <- start:(start + len.tr - 1)
ts <- (start + len.tr):(start + len.tr + len.ts - 1)
data(GSPC)
date <- rownames(Tdata.train[start+len.tr,])
marketTP <- GSPC[paste(date,'/',sep='')][1:len.ts]
s <- svm(Tform, Tdata.train[tr, ], cost = 10, gamma = 0.01)
p <- predict(s, Tdata.train[ts, ])
sig <- trading.signals(p, 0.1, -0.1)
t2 <-  trading.simulator(marketTP, sig, 
                         "policy.2", list(exp.prof = 0.05, bet = 0.3))
summary(t2) 
tradingEvaluation(t2)

########################################## Section 5:  Model Evaluation and Selection ########################################## 

#### sub-section:  Monte Carlo Estimates, Experimental Comparisons

require("performanceEstimation")

# Trading workflow
tradingWF <- function(form, train, test, 
                      quotes, pred.target="signals",
                      learner, learner.pars=NULL,
                      predictor.pars=NULL,
                      learn.test.type='fixed', relearn.step=30,
                      b.t, s.t,
                      policy, policy.pars,
                      trans.cost=5, init.cap=1e+06)
{
  ## obtain the model(s) and respective predictions for the test set
  if (learn.test.type == 'fixed') {  # a single fixed model
    m <- do.call(learner,c(list(form,train),learner.pars))
    preds <- do.call("predict",c(list(m,test),predictor.pars))
  } else {  # either slide or growing window strategies
    data <- rbind(train,test)
    n <- NROW(data)
    train.size <- NROW(train)
    sts <- seq(train.size+1,n,by=relearn.step)
    preds <- vector()
    for(s in sts) {  # loop over each relearn step
      tr <- if (learn.test.type=='slide') data[(s-train.size):(s-1),] 
      else data[1:(s-1),]
      ts <- data[s:min((s+relearn.step-1),n),]
      
      m <- do.call(learner,c(list(form,tr),learner.pars))
      preds <- c(preds,do.call("predict",c(list(m,ts),predictor.pars)))
    }    
  } 
  
  ## Getting the trading signals
  if (pred.target != "signals") {  # the model predicts the T indicator
    predSigs <- trading.signals(preds,b.t,s.t)
    tgtName <- all.vars(form)[1]
    trueSigs <- trading.signals(test[[tgtName]],b.t,s.t)
  } else {  # the model predicts the signals directly
    tgtName <- all.vars(form)[1]
    if (is.factor(preds))
      predSigs <- preds
    else {
      if (preds[1] %in% levels(train[[tgtName]]))
        predSigs <- factor(preds,labels=levels(train[[tgtName]]),
                           levels=levels(train[[tgtName]]))
      else 
        predSigs <- factor(preds,labels=levels(train[[tgtName]]),
                           levels=1:3)
    }
    trueSigs <- test[[tgtName]]
  }
  
  ## obtaining the trading record from trading with the signals
  date <- rownames(test)[1]
  market <- get(quotes)[paste(date,"/",sep='')][1:length(preds),]
  tradeRec <- trading.simulator(market,predSigs,
                                policy.func=policy,policy.pars=policy.pars,
                                trans.cost=trans.cost,init.cap=init.cap)
  
  return(list(trueSigs=trueSigs,predSigs=predSigs,tradeRec=tradeRec))
}

# Evaluation metrics
tradingEval <- function(trueSigs,predSigs,tradeRec,...) 
{
  ## Signals evaluation
  st <- sigs.PR(predSigs,trueSigs)
  dim(st) <- NULL
  names(st) <- paste(rep(c('prec','rec'),each=3),c('s','b','sb'),sep='.')
  
  ## Trading record evaluation
  tradRes <- tradingEvaluation(tradeRec)
  return(c(st,tradRes))
}

#### sub-section:  Results Analysis

#Files containing the results of the experiments described in pages 275-276
load("svm_res_regr.Rdata")
load("nnet_res_regr.Rdata")
load("earth_res_regr.Rdata")
load("svm_res_class.Rdata")
load("nnet_res_class.Rdata")
allResults <- mergeEstimationRes(svm_res_regr, earth_res_regr, nnet_res_regr, 
                                 svm_res_class, nnet_res_class,
                                 by="workflows")
rm(svm_res_regr, earth_res_regr, nnet_res_regr, svm_res_class, nnet_res_class)

#top workflows for the evaluation metrics 
tgtStats <- c('NTrades','prec.sb','Ret','RetOverBH','PercProf',
              'MaxDD','SharpeRatio')
toMax <- c(rep(TRUE,5),FALSE,TRUE)
rankWorkflows(subset(allResults,
                     metrics=tgtStats,
                     partial=FALSE),
              top=3,
              maxs=toMax)

#more information about "svmRegr.v138" workflow
getWorkflow("svmRegr.v138",analysisSet)

#top 100 workflowss according to the previously mentioned main metrics
best <- rankWorkflows(subset(allResults,
                             metrics=tgtStats,
                             partial=FALSE),
                      top=100,
                      maxs=toMax)
bestWFs <- unique(as.vector(sapply(best$SP500,function(x) x$Workflow)))
analysisSet <- subset(allResults, workflows=bestWFs, partial=FALSE)
rm(allResults)

#best performers for each of the more important metrics
(tps <- topPerformers(subset(analysisSet,metrics=tgtStats,partial=FALSE),
                      maxs=toMax))

# more information about these workflows on some of the metrics
summary(subset(analysisSet,
               workflows=tps$SP500[c("prec.sb","Ret","PercProf","MaxDD"),
                                   "Workflow"],
               metrics=tgtStats[-c(1,4,7)],
               partial=FALSE))

#Select a small a set of workflows that are either in the top 3 of one of these metrics or 
# are part of the top 15 of both.
ms <- metricsSummary(subset(analysisSet,
                            metrics=c("NTrades","Ret","PercProf"),
                            partial=FALSE),
                     summary="median")[["SP500"]]
candidates <- subset(analysisSet,
                     workflows=colnames(ms)[which(ms["NTrades",] > 120)],
                     partial=FALSE)
ms <- metricsSummary(subset(candidates,
                            metrics=c("Ret","PercProf"),
                            partial=FALSE),
                     summary="median")[["SP500"]]
(sms <- apply(ms,1,function(x) names(x[order(x,decreasing=TRUE)][1:15])))
(winners <- unique(c(intersect(sms[,1],sms[,2]),sms[1:3,1],sms[1:3,2])))
winnersResults <- subset(analysisSet,
                         metrics=tgtStats,workflows=winners,
                         partial=FALSE)
########################################## Section 6:  The Trading System ########################################## 

#### sub-section:  Evaluation of the Final Test Data
set.seed(1234)
data <- tail(Tdata.train, 2540) # the last 10 years of the training dataset
results <- list()
wfsOut <- list()
for (name in winners) {
  sys <- getWorkflow(name, analysisSet)
  wfsOut[[name]] <- runWorkflow(sys, Tform, data, Tdata.eval)
  results[[name]] <- do.call("tradingEval",wfsOut[[name]])
}
results <- t(as.data.frame(results))

#inspect the values of some of the main statistics
results[, c("NTrades","Ret","RetOverBH","PercProf","MaxDD")]

## The best workflow: nnetRegr.v203
getWorkflow("nnetRegr.v203", analysisSet)

#the result on the final evaluation period of the "nnetRegr.v203" workflow
date <- rownames(Tdata.eval)[1]
market <- GSPC[paste(date, "/", sep = "")][1:nrow(Tdata.eval), ]
plot(wfsOut[["nnetRegr.v203"]]$tradeRec, market, 
     theme = "white", name = "SP500 - final test")

## The cumulative returns on the final evaluation period of the "nnetRegr.v203" system
library(PerformanceAnalytics)
equityWF <- as.xts(wfsOut[["nnetRegr.v203"]]$tradeRec@trading$Equity)
rets <- Return.calculate(equityWF)
chart.CumReturns(rets, main="Cumulative returns of the workflow", ylab = "returns")
yearlyReturn(equityWF) # returns on an annunal or even monthly basis
# yearly percentage returns of the "nnetRegr.v203" system
plot(100*yearlyReturn(equityWF), 
     main='Yearly percentage returns of the trading system')
# risk analysis of the strategy
table.DownsideRisk(rets)