#Reading the data in from local directory, setting row.names = 1 will make the Date our row index
first_world_absolute_exp <- read.csv('FirstWorldAbsoluteExplantoryData.csv', row.names = 1)
first_world_resp <- read.csv('FirstWorldEmissionData.csv', row.names = 1)
first_world_relative_exp <- read.csv('FirstWorldRelativeExplantoryData.csv', row.names = 1)
third_world_relative_exp <- read.csv('ThirdWorldRelativeExplanatoryData.csv', row.names = 1)
third_world_absolute_exp <- read.csv('ThirdWorldAbsoluteExplanatoryData.csv', row.names = 1)
third_world_response <- read.csv('ThirsdWorldEmissionData.csv', row.names = 1)