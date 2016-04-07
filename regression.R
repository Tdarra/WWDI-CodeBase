#Analysis

library(forecast)

####Load the Data
First_World_Resp <- read.csv('FirstWorldEmissionData.csv', header = FALSE, row.names = 1)
First_World_Abs_Explanatory <- read.csv('FirstWorldAbsoluteExplantoryData.csv', header = TRUE, row.names = 1)
First_World_Rel_Explanatory <- read.csv('FirstWorldRelativeExplantoryData.csv', header = TRUE, row.names = 1)
Third_World_Resp <- read.csv('ThirsdWorldEmissionData.csv', header = FALSE, row.names = 1)
Third_World_Abs_Explanatory <- read.csv('ThirdWorldAbsoluteExplanatoryData.csv', header = TRUE, row.names = 1)
Third_World_Rel_Explanatory <- read.csv('ThirdWorldRelativeExplanatoryData.csv', header = TRUE, row.names = 1)



#Take note of respnose set size, no need for explanatory data that does is not within response bounds
y <- length(First_World_Resp$V2)
j <- length(Third_World_Resp$V2)

# trim sample data based on response set
fw_rel <- First_World_Rel_Explanatory[1:y,]
fw_abs <- First_World_Abs_Explanatory[1:y,]
tw_rel <- Third_World_Rel_Explanatory[1:j,]
tw_abs <- Third_World_Abs_Explanatory[1:j,]

# augment the df's by the response sets
fw_rel$emission <- First_World_Resp$V2/1000000
fw_abs$emission <- First_World_Resp$V2/1000000
tw_rel$emission <- Third_World_Resp$V2/1000000
tw_abs$emission <- Third_World_Resp$V2/1000000

attach(fw_abs) # remember to detach later, just makes using columns easier

fw_abs_fit_allvars <- lm(formula = emission ~ ., data = fw_abs) # all variable fit
summary(fw_abs_fit_allvars) # beauty
detach(fw_abs)
#delete all columns that have CO2E has they are defined as fractions of the response data
fw_abs$CO2E_GF_KT...Value <- NULL
fw_abs$CO2E_KD_GD...Value <- NULL
fw_abs$CO2E_LF_KT...Value <- NULL
fw_abs$CO2E_SF_KT...Value <- NULL
fw_abs$CO2_BLDG_MT...Value <- NULL
fw_abs$CO2_ETOT_MT...Value <- NULL
fw_abs$ATM_CO2E_KT...Value <- NULL
fw_abs$ATM_CO2E_PC...Value <- NULL
fw_abs$CO2_MANF_MT...Value <- NULL
fw_abs$CO2_OTHX_MT...Value <- NULL
fw_abs$CO2_TRAN_MT...Value <- NULL

fw_rel$CO2E_EG <- NULL
fw_rel$CO2E_GF <- NULL
fw_rel$CO2E_LF <- NULL
fw_rel$CO2E_SF<- NULL
fw_rel$CO2_BLDG <- NULL
fw_rel$CO2_ETOT <- NULL
fw_rel$CO2_MANF <- NULL
fw_rel$CO2_TRAN <- NULL
fw_rel$CO2_OTHX <- NULL

tw_abs$CO2E_GF_KT...Value <- NULL
tw_abs$CO2E_KD_GD...Value <- NULL
tw_abs$CO2E_LF_KT...Value <- NULL
tw_abs$CO2E_SF_KT...Value <- NULL
tw_abs$CO2_BLDG_MT...Value <- NULL
tw_abs$CO2_ETOT_MT...Value <- NULL
tw_abs$ATM_CO2E_KT...Value <- NULL
tw_abs$ATM_CO2E_PC...Value <- NULL
tw_abs$CO2_MANF_MT...Value <- NULL
tw_abs$CO2_OTHX_MT...Value <- NULL
tw_abs$CO2_TRAN_MT...Value <- NULL

tw_rel$CO2E_EG <- NULL
tw_rel$CO2E_GF <- NULL
tw_rel$CO2E_LF <- NULL
tw_rel$CO2E_SF<- NULL
tw_rel$CO2_BLDG <- NULL
tw_rel$CO2_ETOT <- NULL
tw_rel$CO2_MANF <- NULL
tw_rel$CO2_TRAN <- NULL
tw_rel$CO2_OTHX <- NULL

fw_abs <- fw_abs[rowSums(is.na(fw_abs)) < ncol(fw_abs)/10,]
tw_abs <- tw_abs[rowSums(is.na(tw_abs)) < ncol(tw_abs)/10,]

fw_rel <- fw_rel[rowSums(is.na(fw_rel)) < ncol(fw_rel)/10,]
tw_rel <- tw_rel[rowSums(is.na(tw_rel)) < ncol(tw_rel)/10,]

# get lengths for training and holdout data sets
# assign most recent 10 years (ie. last in the data) to the holdout

fw_rel_training_length <- length(fw_rel$emission) - 10
fw_abs_training_length <- length(fw_abs$emission) - 10
tw_rel_training_length <- length(tw_rel$emission) - 10
tw_abs_training_length <- length(tw_abs$emission) - 10

# split data into training and holdout sets
fw_rel_train <- fw_rel[1:fw_rel_training_length,]
fw_rel_holdout <- tail(fw_rel, n = 10)
fw_abs_train <- fw_abs[1:fw_abs_training_length,]
fw_abs_holdout <- tail(fw_abs, n = 10)
tw_rel_train <- tw_rel[1:tw_rel_training_length,]
tw_rel_holdout <- tail(tw_rel, n = 10)
tw_abs_train <- tw_abs[1:tw_abs_training_length,]
tw_abs_holdout <- tail(tw_abs, n = 10)

# Explorartory fits and variable selection
attach(fw_abs)
fw_abs_fit <- lm(formula = emission ~ ., data = fw_abs, na.action = na.omit) # all variable fit
summary(fw_abs_fit)
detach(fw_abs)

library(leaps)
fw_abs_var_selection <- regsubsets(emission ~., data = fw_abs)
summary(fw_abs_var_selection)
tw_abs_var_selection <- regsubsets(emission ~., data = tw_abs)
summary(tw_abs_var_selection)
fw_rel_var_selection <- regsubsets(emission~.,data = fw_rel)
summary(fw_rel_var_selection)
tw_rel_var_selection <- regsubsets(emission ~., data = tw_rel)
summary(tw_rel_var_selection)

fw_abs_fit8 <- lm(emission ~ GHGO_KT_CE...Value + EN_URB_MCTY...Value + AGR_TRAC_NO...Value + AGR_TOTL_KN...Value + AGR_TOTL_CD...Value + AGR_TOTL_CN...Value + ADJ_DCO2_CD...Value + ADJ_DFOR_CD...Value, data = fw_abs)
summary(fw_abs_fit8)

fw_abs_fit3 <- lm(emission ~ AGR_TRAC_NO...Value + ADJ_DCO2_CD...Value + AGR_TOTL_KN...Value, data = fw_abs)
summary(fw_abs_fit3)

temp_fit2 <- lm(emission ~ AGR_TOTL_CD...Value + EN_URB_LCTY...Value + EN_URB_MCTY...Value, data = fw_abs)
summary(temp_fit2)

tw_abs_fit3 <- lm(emission ~ ADJ_DCO2_CD...Value + AGR_TOTL_CN...Value + EN_POP_DNST...Value, data = tw_abs)
summary(tw_abs_fit3)



detach(fw_abs)
attach(fw_rel)
fw_rel_fit <- lm(formula = emission ~ ., data = fw_rel, na.action = na.omit) # all variable fit
summary(fw_rel_fit)

fc_fw_abs_fit3 <- predict.lm(fw_abs_fit3, fw_abs_train, n.ahead = 10, level = c(90, 95, 99, 99.9))
fc_fw_abs_fit3

detach(fw_rel)

# checkout pairwise scatterplots
pair_fw_rel <- pairs(fw_rel)
pair_fw_abs <- pairs(fw_abs)
pair_tw_rel <- pairs(tw_rel)
pair_tw_abs <- pairs(tw_abs)

# Look at correlation matrix to check for interaction terms
cor_fw_rel <- cor(fw_rel)
cor_tw_rel <- cor(tw_rel)
cor_fw_abs <- cor(fw_abs)
cor_tw_abs <- cor(tw_abs)

## 
#VARIANCE INFLATION FACTOR
fw_abs_fit_allvars <- lm(formula = emission ~ ., data = fw_abs)
tw_abs_fit_allvars <- lm(formula = emission ~ ., data = tw_abs)
fw_rel_fit_allvars <- lm(formula = emission ~ ., data = fw_rel)
tw_rel_fit_allvars <- lm(formula = emission ~ ., data = tw_rel)
fw_abs_fit1 <- lm(formula = emission ~ EN_URB_LCTY...Value, data = tw_abs)
tw_abs_fit1 <- lm(formula = emission ~ ADJ_DCO2_CD...Value, data = tw_abs)
fw_rel_fit1 <- lm(formula = emission ~ EMPL_FE, data = fw_rel)
tw_rel_fit1 <- lm(formula = emission ~ DFOR_GN, data = tw_rel)
#SE value
fw_abs_s<-summary(fw_abs_fit_allvars) #1.528e-07
fw_abs_s1<-summary(fw_abs_fit1) # 6.996e-11
tw_abs_s<-summary(tw_abs_fit_allvars) #1.244e-11
tw_abs_s1<-summary(tw_abs_fit1) #1.190e-12
fw_rel_s<-summary(fw_rel_fit_allvars) # 3.473e-01
fw_rel_s1<-summary(fw_rel_fit1) #0.03625
tw_rel_s<-summary(tw_rel_fit_allvars) #6.722e-04
tw_rel_s1<-summary(tw_rel_fit1) #0.0004233 

#VIF fw_abs
fw_abs_c <- fw_abs[,-12]
fw_abs_regffvsothers <- lm(EN_URB_LCTY...Value~., data=fw_abs_c)
fw_abs_regffvsothers.summ <- summary(fw_abs_regffvsothers)
1/(1-fw_abs_regffvsothers.summ$adj.r.squared)
#VIF tw_abs
tw_abs_c <- tw_abs[,-12]
tw_abs_regffvsothers <- lm(ADJ_DCO2_CD...Value~., data=tw_abs_c)
tw_abs_regffvsothers.summ <- summary(tw_abs_regffvsothers)
1/(1-tw_abs_regffvsothers.summ$adj.r.squared)
#VIF fw_rel
fw_rel_c <- fw_rel[,-12]
fw_rel_regffvsothers <- lm(EMPL_FE~., data=fw_rel_c)
fw_rel_regffvsothers.summ <- summary(fw_rel_regffvsothers)
1/(1-fw_rel_regffvsothers.summ$adj.r.squared)
#VIF tw_rel
tw_rel_c <- tw_rel[,-12]
tw_rel_regffvsothers <- lm(DFOR_GN~., data=tw_rel_c)
tw_rel_regffvsothers.summ <- summary(tw_rel_regffvsothers)
1/(1-tw_rel_regffvsothers.summ$adj.r.squared)

#CROSS-VALIDATION
ls.cvrmse <- function(ls.out)
{
  res.cv <- ls.out$residuals / (1.0 - ls.diag(ls.out)$hat)
  # Identify NA's and remove them.
  is.na.res <- is.na(res.cv)
  res.cv <- res.cv[!is.na.res]
  cvrmse <- sqrt(sum(res.cv^2) / length(res.cv))
  return(cvrmse)
}



fw_abs_var_selection <- regsubsets(emission ~., data = fw_abs)
fw_abs_ss<-summary(fw_abs_var_selection)
tw_abs_var_selection <- regsubsets(emission ~., data = tw_abs)
tw_abs_ss<-summary(tw_abs_var_selection)
fw_rel_var_selection <- regsubsets(emission~.,data = fw_rel)
fw_rel_ss<-summary(fw_rel_var_selection)
tw_rel_var_selection <- regsubsets(emission ~., data = tw_rel)
tw_rel_ss<-summary(tw_rel_var_selection)

#adjr2 & cp values
which.max(fw_abs_ss$adjr2) #3
which.min(fw_abs_ss$cp) #3
fw_abs_fit3 <- lm(emission ~ AGR_TRAC_NO...Value + ADJ_DCO2_CD...Value + AGR_TOTL_KN...Value, data = fw_abs)

which.max(tw_abs_ss$adjr2) #7
which.min(tw_abs_ss$cp) #6
tw_abs_fit7 <- lm(emission ~ ADJ_DCO2_CD...Value + AGR_TOTL_CN...Value + AGR_TOTL_KD...Value + AGR_TOTL_KN...Value + EN_POP_DNST...Value + EN_URB_LCTY...Value + EN_URB_MCTY...Value , data = tw_abs)
tw_abs_fit6 <- lm(emission ~ ADJ_DCO2_CD...Value + AGR_TOTL_CN...Value + AGR_TOTL_KN...Value + EN_POP_DNST...Value + EN_URB_LCTY...Value + EN_URB_MCTY...Value , data = tw_abs)

which.max(fw_rel_ss$adjr2) #4
which.min(fw_rel_ss$cp) #4
fw_rel_fit4 <- lm(emission ~ AGR_TOTL+EMPL_FE+FRST_RT+LCTY_UR, data = fw_rel)

which.max(tw_rel_ss$adjr2) #5
which.min(tw_rel_ss$cp) #5
tw_rel_fit5 <- lm(emission ~ DCO2_GN+DFOR_GN+FRST_RT+LCTY_UR+MCTY_TL, data = tw_rel)

#Leave one-out cross validation
fw_abs_fit_cvrmse <- ls.cvrmse(fw_abs_fit_allvars)
fw_abs_fit3_cvrmse <- ls.cvrmse(fw_abs_fit3)
print(c(fw_abs_fit_cvrmse, fw_abs_fit3_cvrmse))

tw_abs_fit_cvrmse <- ls.cvrmse(tw_abs_fit_allvars)
tw_abs_fit6_cvrmse <- ls.cvrmse(tw_abs_fit6)
print(c(tw_abs_fit_cvrmse, tw_abs_fit6_cvrmse))
tw_abs_fit7_cvrmse <- ls.cvrmse(tw_abs_fit7)
print(c(tw_abs_fit6_cvrmse, tw_abs_fit7_cvrmse))

fw_rel_fit_cvrmse <- ls.cvrmse(fw_rel_fit_allvars)
fw_rel_fit4_cvrmse <- ls.cvrmse(fw_rel_fit4)
print(c(fw_rel_fit_cvrmse, fw_rel_fit4_cvrmse))

tw_rel_fit_cvrmse <- ls.cvrmse(tw_rel_fit_allvars)
tw_rel_fit5_cvrmse <- ls.cvrmse(tw_rel_fit5)
print(c(tw_rel_fit_cvrmse, tw_rel_fit5_cvrmse))

#5-fold validation
#fw_abs
n <- nrow(fw_abs)
sn <- floor(n/5)
set.seed(306)
B <- 500 #Do 500 random splits
errMx <- matrix(NA, B, 2) #matrix to store the results
colnames(errMx) <- c("FullModel", "BestModel")
for (i in 1:B)
{
  testInd <- sample(1:n, sn, replace=FALSE)
  
  tTestDat <- fw_abs[testInd, ] #Treat the sampled index as testing set
  tTrainDat <- fw_abs[-testInd, ] #The rest is training set.
  
  tFullModel <- lm(formula = emission ~ ., data = fw_abs, na.action=na.omit)
  tFullModel.pred <- predict(tFullModel, tTestDat)
  errMx[i, 1] <- sqrt(sum((tTestDat$emission - tFullModel.pred)^2)/sn)
  
  
  tBestModel <- lm(emission ~ AGR_TRAC_NO...Value + ADJ_DCO2_CD...Value + AGR_TOTL_KN...Value, data = fw_abs, na.action=na.omit)
  tBestModel.pred <- predict(tBestModel, tTestDat)
  errMx[i, 2] <- sqrt(sum((tTestDat$emission - tBestModel.pred)^2)/sn)
  }

apply(na.omit(errMx), 2, mean) 

#tw_abs
n <- nrow(tw_abs)
sn <- floor(n/5)
set.seed(306)
B <- 500 #Do 500 random splits
errMx <- matrix(NA, B, 2) #matrix to store the results
colnames(errMx) <- c("FullModel", "BestModel")
for (i in 1:B)
{
  testInd <- sample(1:n, sn, replace=FALSE)
  
  tTestDat <- tw_abs[testInd, ] #Treat the sampled index as testing set
  tTrainDat <- tw_abs[-testInd, ] #The rest is training set.
  
  tFullModel <- lm(formula = emission ~ ., data = tw_abs, na.action=na.omit)
  tFullModel.pred <- predict(tFullModel, tTestDat)
  errMx[i, 1] <- sqrt(sum((tTestDat$emission - tFullModel.pred)^2)/sn)
  
  
  tBestModel <- lm(emission ~ ADJ_DCO2_CD...Value + AGR_TOTL_CN...Value + AGR_TOTL_KN...Value + EN_POP_DNST...Value + EN_URB_LCTY...Value + EN_URB_MCTY...Value , data = tw_abs)
  tBestModel.pred <- predict(tBestModel, tTestDat)
  errMx[i, 2] <- sqrt(sum((tTestDat$emission - tBestModel.pred)^2)/sn)
}

apply(na.omit(errMx), 2, mean) 

#fw_rel
n <- nrow(fw_rel)
sn <- floor(n/5)
set.seed(306)
B <- 500 #Do 500 random splits
errMx <- matrix(NA, B, 2) #matrix to store the results
colnames(errMx) <- c("FullModel", "BestModel")
for (i in 1:B)
{
  testInd <- sample(1:n, sn, replace=FALSE)
  
  tTestDat <- fw_rel[testInd, ] #Treat the sampled index as testing set
  tTrainDat <- fw_rel[-testInd, ] #The rest is training set.
  
  tFullModel <- lm(formula = emission ~ ., data = fw_rel, na.action=na.omit)
  tFullModel.pred <- predict(tFullModel, tTestDat)
  errMx[i, 1] <- sqrt(sum((tTestDat$emission - tFullModel.pred)^2)/sn)
  
  
  tBestModel <- lm(emission ~ AGR_TOTL+EMPL_FE+FRST_RT+LCTY_UR, data = fw_rel)
  tBestModel.pred <- predict(tBestModel, tTestDat)
  errMx[i, 2] <- sqrt(sum((tTestDat$emission - tBestModel.pred)^2)/sn)
}

apply(na.omit(errMx), 2, mean) 

#tw_rel
n <- nrow(tw_rel)
sn <- floor(n/5)
set.seed(306)
B <- 500 #Do 500 random splits
errMx <- matrix(NA, B, 2) #matrix to store the results
colnames(errMx) <- c("FullModel", "BestModel")
for (i in 1:B)
{
  testInd <- sample(1:n, sn, replace=FALSE)
  
  tTestDat <- tw_rel[testInd, ] #Treat the sampled index as testing set
  tTrainDat <- tw_rel[-testInd, ] #The rest is training set.
  
  tFullModel <- lm(formula = emission ~ ., data = tw_rel, na.action=na.omit)
  tFullModel.pred <- predict(tFullModel, tTestDat)
  errMx[i, 1] <- sqrt(sum((tTestDat$emission - tFullModel.pred)^2)/sn)
  
  
  tBestModel <- lm(emission ~ DCO2_GN+DFOR_GN+FRST_RT+LCTY_UR+MCTY_TL, data = tw_rel)
  tBestModel.pred <- predict(tBestModel, tTestDat)
  errMx[i, 2] <- sqrt(sum((tTestDat$emission - tBestModel.pred)^2)/sn)
}

apply(na.omit(errMx), 2, mean) 

