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

library(leaps)
temp_fit <- regsubsets(emission ~., data = fw_abs)
summary(temp_fit)

fw_abs_fit3 <- lm(emission ~ AGR_TRAC_NO...Value + ADJ_DCO2_CD...Value + AGR_TOTL_KN...Value, data = fw_abs)
summary(fw_abs_fit3)


temp_fit2 <- lm(emission ~ AGR_TOTL_CD...Value + EN_URB_LCTY...Value + EN_URB_MCTY...Value, data = fw_abs)
summary(temp_fit2)

detach(fw_abs)
attach(fw_rel)
fw_rel_fit <- lm(formula = emission ~ ., data = fw_rel, na.action = na.omit) # all variable fit
summary(fw_rel_fit)

fc_fw_abs_fit3 <- predict.lm(fw_abs_fit3, fw_abs_train, n.ahead = 10, level = c(90, 95, 99, 99.9))
fc_fw_abs_fit3
