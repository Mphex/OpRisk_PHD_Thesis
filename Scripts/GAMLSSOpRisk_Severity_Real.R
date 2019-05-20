
options(scipen = 999)
# Load packages
library(rattle, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(Hmisc, quietly = TRUE)
library(chron, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(caret)
library(ggplot2)

crv$SEED <- 123 # set random seed to make your partition reproducible

#==========================================================================================

# Load the dataset OPriskDataSet_exposure_severity


Fname <- "file:///C:/Users/User/Documents/R PROJECT/OpRiskPHDGitHub/OpRisk_PHD_Dissertate/OpRisk_PHD_Dissertation/OPriskDataSet_exposure_severity.csv"
crs$DATASET <- read.csv(Fname,
                        sep=";",
                        dec=",",
                        na.strings=c(".", "NA", "", "?"),
                        strip.white=TRUE, encoding="UTF-8")

exposure <- crs$DATASET[,ncol(crs$DATASET)] 

crs$DATASET <- as.data.frame(crs$DATASET)

# The following varaible selections have been noted 

crs$INPUT <- crs$DATASET %>%
  group_by(UpdatedDay,
           UpdatedTime,
           TradedDay,
           TradedTime,
           Desk,
           CapturedBy,
           TradeStatus,
           TraderId,
           Instrument,
           Reason,
           EventTypeCategoryLevel1,
           BusinessLineLevel1) %>% 
transmute(LossesIndicator = LossIndicator,
            Losses = Loss,
            exposure = exposure)

crs$target  <- "Losses"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("UpdateTime", "TradeTime", "Nominal", "FloatRef", "LastResetDate", "LastResetRate", "Theta", "Unexplained", "exposure")
crs$weights <- NULL

#==============================================================================================

# Build the training/validation/testing datasets
# nobs=2331 training=1632 validation=350 testing=349

set.seed(crv$SEED)

crs$NOBS <- nrow(crs$INPUT)

crs$TRAIN <- sample(crs$NOBS, 0.7*crs$NOBS)

crs$NOBS %>%
  seq_len() %>%
  setdiff(crs$TRAIN) %>%
  sample(0.15*crs$NOBS) ->
  crs$VALIDATE

crs$NOBS %>%
  seq_len() %>%
  setdiff(crs$TRAIN) %>%
  setdiff(crs$VALIDATE) ->
  crs$TEST

crs$TRAINING <- as.data.frame(crs$INPUT[crs$TRAIN,])
crs$VALIDATION <- as.data.frame(crs$INPUT[crs$VALIDATE,])
crs$TESTING <- as.data.frame(crs$INPUT[crs$TEST,])

### Let us fit a GAMLSS to our data. This will be our global model. We will use "Losses" as the dependent variable, while the other variables will be predictor variables.

library(gamlss)

severfit <- gamlss(Losses~pb(UpdatedDay+Desk),
             sigma.fo=~pb(UpdatedDay+Desk),
             nu.fo=~pb(UpdatedDay+Desk),
             tau.fo=~pb(UpdatedDay+Desk),
             data=crs$TRAINING, mu.start = NULL,  sigma.start = NULL, nu.start = NULL,
             tau.start = NULL, family=NET)
# 
# 
 summary(severfit)

# names(severfit)
# class(severfit)
# drop1(severfit)
# wp(severfit)
# term.plot(severfit, what="sigma", ask=FALSE)

### Let us fit a GAMLSS to our data. This will be our global model. We will use "Losses" as the
### dependent variable, while the other variables will be predictor variables.

Severfit <- gamlss(Losses~cs(UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus 
             + TraderId + Instrument + Reason + LossesIndicator + EventTypeCategoryLevel1 + BusinessLineLevel1), 
sigma.formula=~cs(UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus 
             + TraderId + Instrument + Reason + LossesIndicator + EventTypeCategoryLevel1 + BusinessLineLevel1),
nu.formula=~cs(UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus 
             + TraderId + Instrument + Reason + LossesIndicator + EventTypeCategoryLevel1 + BusinessLineLevel1),
 tau.formula=~cs(UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus 
             + TraderId + Instrument + Reason + LossesIndicator + EventTypeCategoryLevel1 + BusinessLineLevel1),
data=crs$TRAINING, mu.start = NULL,  sigma.start = NULL, nu.start = NULL, tau.start = NULL, family=BCPE)

summary(Severfit)


### Load "MuMIn" package

require(MuMIn)

### Then, we use "dredge" function to generate models using combinations of the terms in the global model. The
### function will also calculate AICc values and rank models according to it. Note that AICc is AIC corrected for
### finite sample sizes

options(na.action=na.fail)
severfits <- dredge(severfit)

### Ok, let us use "get.models" function to generate a list in which its objects are the fitted models. We will
### also use the "model.avg" function to do a model averaging based on AICc. Note that "subset=TRUE" will make the
### function calculate the average model (or mean model) using all models.

# summary(model.avg(get.models(severfits, subset = TRUE)))

### However, if you want to get only the models that have delta AICc < 2, use "subset=delta<2"

summary(model.avg(get.models(severfits, subset=delta<2)))

### That's it! Now we have AICc values for our models and we have the average model (or mean model).


AV.PRED <-predict(severfits, newdata=crs$TESTING, type="response")

