#Analysis

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

# checkout pairwise scatterplots
pair_fw_rel <- pairs(fw_rel)
pair_fw_abs <- pairs(fw_abs)
pair_tw_rel <- pairs(tw_rel)
pair_tw_abs <- pairs(tw_abs)

# Look at correlation matrix to check for interaction terms
cor_fw_rel <- cor(fw_rel)
cor_tw_rel <- cor(tw_rel)
cor_fw_abs <- cor(fw_abs)
fw_abs$ADJ_DCO2_CD...Value <- NULL
fw_abs$AGR_TOTL_CD...Value <- NULL
fw_abs$AGR_TOTL_CN...Value <- NULL
fw_abs$AGR_TOTL_KN...Value <- NULL
cor_fw_abs1 <- cor(fw_abs)

cor_tw_abs <- cor(tw_abs)

cor_fw_abs # ADJ_DCO2 should be dropped along with 3 of the AGR_TOTL vars

fw_abs$ADJ_DCO2_CD...Value <- NULL
fw_abs$AGR_TOTL_CD...Value <- NULL
fw_abs$AGR_TOTL_CN...Value <- NULL
fw_abs$AGR_TOTL_KN...Value <- NULL

tw_abs$ADJ_DCO2_CD...Value <- NULL
tw_abs$AGR_TOTL_CD...Value <- NULL
tw_abs$AGR_TOTL_CN...Value <- NULL
tw_abs$AGR_TOTL_KN...Value <- NULL


fw_abs <- fw_abs[rowSums(is.na(fw_abs)) < ncol(fw_abs)/10,]
tw_abs <- tw_abs[rowSums(is.na(tw_abs)) < ncol(tw_abs)/10,]

fw_rel <- fw_rel[rowSums(is.na(fw_rel)) < ncol(fw_rel)/10,]
tw_rel <- tw_rel[rowSums(is.na(tw_rel)) < ncol(tw_rel)/10,]






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





detach(fw_abs)
attach(fw_rel)
fw_rel_fit <- lm(formula = emission ~ ., data = fw_rel, na.action = na.omit) # all variable fit
summary(fw_rel_fit)
detach(fw_rel)


fw_abs_fit_allvars <- lm(formula = emission ~ ., data = fw_abs)
tw_abs_fit_allvars <- lm(formula = emission ~ ., data = tw_abs)
fw_rel_fit_allvars <- lm(formula = emission ~ ., data = fw_rel)
tw_rel_fit_allvars <- lm(formula = emission ~ ., data = tw_rel)


fw_abs_var_selection <- regsubsets(emission ~., data = fw_abs)
fw_abs_ss<-summary(fw_abs_var_selection)
tw_abs_var_selection <- regsubsets(emission ~., data = tw_abs)
tw_abs_ss<-summary(tw_abs_var_selection)
fw_rel_var_selection <- regsubsets(emission~.,data = fw_rel)
fw_rel_ss<-summary(fw_rel_var_selection)
tw_rel_var_selection <- regsubsets(emission ~., data = tw_rel)
tw_rel_ss<-summary(tw_rel_var_selection)

fw_abs_var_selection_f <- regsubsets(emission ~., data = fw_abs, method = "forward")
fw_abs_ss_f<-summary(fw_abs_var_selection_f)
tw_abs_var_selection_f <- regsubsets(emission ~., data = tw_abs, method = "forward")
tw_abs_ss_f<-summary(tw_abs_var_selection_f)
fw_rel_var_selection_f <- regsubsets(emission~.,data = fw_rel, method = "forward")
fw_rel_ss_f<-summary(fw_rel_var_selection_f)
tw_rel_var_selection_f <- regsubsets(emission ~., data = tw_rel, method = "forward")
tw_rel_ss_f<-summary(tw_rel_var_selection_f)

fw_abs_var_selection_b <- regsubsets(emission ~., data = fw_abs, method = "backward")
fw_abs_ss_b<-summary(fw_abs_var_selection_b)
tw_abs_var_selection_b <- regsubsets(emission ~., data = tw_abs, method = "backward")
tw_abs_ss_b<-summary(tw_abs_var_selection_b)
fw_rel_var_selection_b <- regsubsets(emission~.,data = fw_rel, method = "backward")
fw_rel_ss_b<-summary(fw_rel_var_selection_b)
tw_rel_var_selection_b <- regsubsets(emission ~., data = tw_rel, method = "backward")
tw_rel_ss_b<-summary(tw_rel_var_selection_b)

#best exhaustive models
best_fw_abs_model_r <- which.max(fw_abs_ss$adjr2) #3
best_fw_abs_model_cp <- which.min(fw_abs_ss$cp) #3
best_tw_abs_model_r <- which.max(tw_abs_ss$adjr2)#4
best_tw_abs_model_cp <- which.min(tw_abs_ss$cp) #2
best_fw_rel_model_r <- which.max(fw_rel_ss$adjr2) #4
best_fw_rel_model_cp <- which.min(fw_rel_ss$cp) #4
best_tw_rel_model_r <- which.max(tw_rel_ss$adjr2) #5
best_tw_rel_model_cp <- which.min(tw_rel_ss$cp) #5

#best forward models
best_fw_abs_model_r_f <- which.max(fw_abs_ss_f$adjr2) #4
best_fw_abs_model_cp_f <- which.min(fw_abs_ss_f$cp) #4
best_tw_abs_model_r_f <- which.max(tw_abs_ss_f$adjr2) #4
best_tw_abs_model_cp_f <- which.min(tw_abs_ss_f$cp) #2
best_fw_rel_model_r_f <- which.max(fw_rel_ss_f$adjr2) #4
best_fw_rel_model_cp_f <- which.min(fw_rel_ss_f$cp)#4
best_tw_rel_model_r_f <- which.max(tw_rel_ss_f$adjr2) #5
best_tw_rel_model_cp_f <- which.min(tw_rel_ss_f$cp)#5

#best backward models
best_fw_abs_model_r_b <- which.max(fw_abs_ss_b$adjr2)#3
best_fw_abs_model_cp_b <- which.min(fw_abs_ss_b$cp)#3
best_tw_abs_model_r_b <- which.max(tw_abs_ss_b$adjr2)#4
best_tw_abs_model_cp_b <- which.min(tw_abs_ss_b$cp)#2
best_fw_rel_model_r_b <- which.max(fw_rel_ss_b$adjr2)#7
best_fw_rel_model_cp_b <- which.min(fw_rel_ss_b$cp)#4
best_tw_rel_model_r_b <- which.max(tw_rel_ss_b$adjr2)#5
best_tw_rel_model_cp_b <- which.min(tw_rel_ss_b$cp)#5


