#Analysis

####Load the Data
First_World_Resp <- read.csv('FirstWorldEmissionData.csv', header = TRUE, row.names = 1)
First_World_Abs_Explanatory <- read.csv('FirstWorldAbsoluteExplantoryData.csv', header = TRUE, row.names = 1)
First_World_Rel_Explanatory <- read.csv('FirstWorldRelativeExplantoryData.csv', header = TRUE, row.names = 1)
Third_World_Resp <- read.csv('ThirsdWorldEmissionData.csv', header = TRUE, row.names = 1)
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

# augment the df's by the response sets, convert emission data from kilotonens to gigatonnes
fw_rel$emission <- First_World_Resp$V2/1000000
fw_abs$emission <- First_World_Resp$V2/1000000
tw_rel$emission <- Third_World_Resp$V2/1000000
tw_abs$emission <- Third_World_Resp$V2/1000000

attach(fw_abs) # remember to detach later, just makes using columns easier

fw_abs_fit_allvars <- lm(formula = emission ~ ., data = fw_abs) # all variable fit
summary(fw_abs_fit_allvars) # beauty



