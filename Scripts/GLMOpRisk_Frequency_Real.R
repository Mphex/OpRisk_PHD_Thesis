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

crv$seed <- 42 # set random seed to make your partition reproducible

#==========================================================================================
#crv$training.proportion <- 0.7 # proportion of data used for training
#crv$validation.proportion <- 0.15 # proportion of data used for validation

# Load the dataset OPriskDataSet_exposure


fname <- "file:///C:/Users/User/Documents/R PROJECT/OpRiskPHDGitHub/OpRisk_PHD_Thesis/Data/OPriskDataSet_exposure.csv"
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


crs$training <- as.data.frame(crs$input[crs$train,])
crs$validation <- as.data.frame(crs$input[crs$validate,])
crs$testing <- as.data.frame(crs$input[crs$test,])

### Let us fit a GLM to our data. This will be our global model. We will use "LossesIndicator" as the dependent variable, while the other variables will be predictor variables.

freqfit <- glm(LossesIndicator ~ UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus + TraderId + Instrument + Reason + EventTypeCategoryLevel1 + BusinessLineLevel1, data=crs$training, family=poisson(link="log"), offset=log(Exposure))

summary(freqfit)

### Load "MuMIn" package

require(MuMIn)

## Then, we use "dredge" function to generate models using combinations of the terms in the global model. The function will also calculate AICc values and rank models according to it. Note that AICc is AIC corrected for finite sample sizes

library(parallel)
cl <- makeCluster(4) # Assign R cores to the job

options(na.action=na.fail)
freqfits <- dredge(freqfit)
freqfits

stopCluster(cl)

### Ok, let us use "get.models" function to generate a list in which its objects are the fitted models. We will also use the "model.avg" function to do a model averaging based on AICc. Note that "subset=TRUE" will make the function calculate the average model (or mean model) using all models.

# cl <- makeCluster(4) # Assign R cores to the job
#
# Amodel <- model.avg(get.models(freqfits, subset = TRUE))
# summary(Amodel)
#
# stopCluster(cl)

### However, if you want to get only the models that have delta AICc < 2, use "subset=delta<2"

cl <- makeCluster(4) # Assign R cores to the job
 amodel <- (model.avg(get.models(freqfits, subset=delta<2)))
 summary(amodel)
 stopCluster(cl)

### That's it! Now we have AICc values for our models and we have the average model (or mean model).
##________________________________________________________________________________________________
## Evaluate model performance on the validation dataset G

## Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

av.pred <- predict(amodel, crs$validation, type = "response")
av.pred

# Export into excel

library(R2HTML)
HTMLStart(); HTML(data.frame(av.pred)); w <- HTMLStop()
browseURL(w)

#shell(paste("start excel", w))

MASS::fitdistr(av.pred, "Poisson")

#MASS::fitdistr(exp(av.pred), "Poisson")
#MASS::fitdistr(crs[["training"]][["Exposure"]]*(exp(av.pred)), "Poisson")

Est <- "file:///C:/Users/User/Documents/R PROJECT/OpRiskPHDGitHub/OpRisk_PHD_Thesis/Data/OPriskDataSet_GoF.csv"
pred <- read.csv(Est,
                  sep=";",
                  dec=",",
                  na.strings=c(".", "NA", "", "?"),
                  strip.white=TRUE, encoding="UTF-8")
head(pred)

## Generate the confusion matrix showing counts.

library(e1071)
confusionMatrix(table(pred$response, crs[["validation"]][["LossesIndicator"]]))

# Guage model performance of ROCR on the validation dataset.

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the glm model on OPriskDataSet_exposure.csv [validate].

crs$pr <- av.pred

# Remove observations with missing target.

no.miss   <- na.omit(crs[["validation"]][["LossesIndicator"]])
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  predic <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  predic <- prediction(crs$pr, no.miss)
}

pe <- performance(predic, "tpr", "fpr")
au <- performance(predic, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Linear OPriskDataSet_exposure.csv [validate] LossesIndicator")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs[["validation"]][["LossesIndicator"]])
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  predic <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  predic <- prediction(crs$pr, no.miss)
}
performance(predic, "auc")


ExtraNewData <- read.csv("Hoohlo1.csv",
               sep=";",
               dec=",",
               na.strings=c(".", "NA", "", "?"),
               strip.white=TRUE, encoding="UTF-8",header=T)
