options(scipen = 999)

# Load packages
library(rattle, quietly = TRUE)
library(magrittr, quietly = TRUE) # Utilize the %>% and %>% pipeline operators
library(Hmisc, quietly = TRUE)
library(chron, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(ggplot2)
library(caTools)
library(caret)


building <- TRUE
scoring <- ! building

# A predefined value is used to reset the random seed so that results are repeatable

crv$seed <- 41 # set random seed to make your partition reproducible

#==========================================================================================
#crv$training.proportion <- 0.7 # proportion of data used for training
#crv$validation.proportion <- 0.15 # proportion of data used for validation

# Load the dataset OPriskDataSet_exposure


fname <- "file:///C:/Users/User/Documents/OpRiskPHDGitHub/OpRisk_PHD_Thesis/Data/OPriskDataSet_exposure.csv"
crs$dataset <- read.csv(fname,
              sep=";",
              dec=",",
              na.strings=c(".", "NA", "", "?"),
              strip.white=TRUE, encoding="UTF-8")

exposure <- crs$dataset[,ncol(crs$dataset)]

#class(exposure)
#length(exposure)

crs$dataset <- as.data.frame(crs$dataset)

# The following varaible selections have been noted

crs$input <- crs$dataset %>%
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
            Exposure = exposure)

getmode <- function(x){
  u <- unique(x)
  as.integer(u[which.max(tabulate(match(x,u)))])
}

for (i in 5:(ncol(crs$input) - 3)){
     crs$input[[i]] <- relevel(crs$input[[i]], getmode(crs$input[[i]]))
}

crs$target  <- "LossesIndicator"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("UpdateTime", "TradeTime", "Nominal", "FloatRef", "LastResetDate", "LastResetRate", "Theta", "Loss", "Unexplained")
crs$weights <- NULL

# Build the training/validation/testing datasets
# nobs=2331 training=1632 validation=350 testing=349

set.seed(crv$seed)

crs$nobs <- nrow(crs$input)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
  crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
  crs$test

# NewData <- crs$input
# set.seed(crv$seed)
# split = sample.split(NewData$LossesIndicator, SplitRatio = 0.7)
# split
# training_set = subset(NewData, split == TRUE)
# test_set = subset(NewData, split == FALSE)

crs$training <- as.data.frame(crs$input[crs$train,])
crs$validation <- as.data.frame(crs$input[crs$validate,])
crs$testing <- as.data.frame(crs$input[crs$test,])

### Let us fit a GLM to our data. This will be our global model. We will use "LossesIndicator" as the dependent variable, while the other variables will be predictor variables.

freqfit <- glm(LossesIndicator ~ UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus + TraderId + Instrument + Reason + EventTypeCategoryLevel1 + BusinessLineLevel1, data=crs$training, family=poisson(link="log"), offset=log(Exposure))

summary(freqfit)

### Load "MuMIn" package

require(MuMIn)

## Then, we use "dredge" function to generate models using combinations of the terms in the global model. The function will also calculate AICc values and rank models according to it. Note that AICc is AIC corrected for finite sample sizes

options(na.action=na.fail)
freqfits <- dredge(freqfit)
freqfits


# dim(freqfits)
# freqfits[1]
# freqfits[,1]
# class(freqfits)

### Ok, let us use "get.models" function to generate a list in which its objects are the fitted models. We will also use the "model.avg" function to do a model averaging based on AICc. Note that "subset=TRUE" will make the function calculate the average model (or mean model) using all models.

# Amodel <- model.avg(get.models(freqfits, subset = TRUE))
#summary(Amodel)


### However, if you want to get only the models that have delta AICc < 2, use "subset=delta<2"

amodel <- (model.avg(get.models(freqfits, subset=delta<2)))
summary(amodel)

### That's it! Now we have AICc values for our models and we have the average model (or mean model).

## predicting test set results

av.pred <- predict(amodel, crs$training, type = "response")
av.pred

library(R2HTML)
HTMLStart(); HTML(data.frame(av.pred)); w <- HTMLStop()
browseURL(w)

shell(paste("start excel", w))


MASS::fitdistr(exp(av.pred), "Poisson")


MASS::fitdistr(crs[["training"]][["Exposure"]]*(exp(av.pred)), "Poisson")

Est <- "file:///C:/Users/User/Documents/R PROJECT/OpRiskPHDGitHub/OpRisk_PHD_Dissertate/OpRisk_PHD_Dissertation/OPriskDataSet_training.csv"
pred <- read.csv(Est,
                  sep=";",
                  dec=",",
                  na.strings=c(".", "NA", "", "?"),
                  strip.white=TRUE, encoding="UTF-8")
head(pred)

confusionMatrix(table(pred$response_8581, crs[["training"]][["LossesIndicator"]]))

ExtraNewData <- read.csv("Hoohlo1.csv",
               sep=";",
               dec=",",
               na.strings=c(".", "NA", "", "?"),
               strip.white=TRUE, encoding="UTF-8",header=T)
