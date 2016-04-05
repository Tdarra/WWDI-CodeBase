%matplotlib inline

import Quandl
import pandas as pd
import matplotlib.pyplot as plt
import os

countrycodes = []
searchfile = open("country_codes", "r")
for line in searchfile:
    if "|" in line: 
        if "COUNTRY|CODE" not in line:
            countrycodes.append(line[-4:].strip(' \t\n\r'))
searchfile.close()

indicatorcodes = []
key = []
value = []
otherfile = open("wwdi_indicators", "r")
for line in otherfile:
    if "|" in line: 
        if "|CODE" not in line:
            key.append(str(line.split("|")[1].strip(' \t\n\r')))
            value.append(str(line.split("|")[0].strip('\n')))
            indicatorcodes.append(line[line.index("|")+1:].strip(' \t\n\r'))
otherfile.close()

quandl_code_hash = {}
for i in range(len(key)):
    quandl_code_hash[key[i]] = value[i]

### All possible permutation of Quandl API codes
codes = []
QUANDL_PREFIX = "WWDI/"
for i in indicatorcodes:
    for c in countrycodes:
        codestring = QUANDL_PREFIX + c + "_" + i
        codes.append(codestring)

def getCol(df):
    notfound = "- NOT FOUND"
    for column in df.columns:
        if "WWDI" in column:
            col = str(column)
            country = column[5:8]
#             print country
            if notfound in col:
                c = col[:len(col)-len(notfound)]
            else:
                c = col[:len(col)-7] # here we chop off the ends of the name 
#     print c
#     print col
#     trans_col_name = c.replace(".","/")
#     indicator_name = trans_col_name[9+len(notfound):]
            indicator_name = c[9:] #here we chop off the beginning
#     print trans_col_name
            indicator_name =indicator_name.strip()
            desc = quandl_code_hash.get(indicator_name) + " in" + " " + country
            df.rename(columns= {column : desc }, inplace= True)

#Example of retrieve
## I will retrieve the first world countries

wwdi_str = 'WWDI/'

# countries of the first world http://www.nationsonline.org/oneworld/third_world_countries.htm
# Luxembourg, Norway, US, Ireland, Bermuda, Iceland, Denmark, San Marino, Canada, Switzerland
firstWorldCountries= ['LUX', 'NOR', 'USA', 'IRL', 'DNK', 'SMR', 'CAN','CHE']

# Third world countries:
# Timor-Leste, Malawi, Somalia, Congo, Tanzania, Yemen, Burundi, Afghanistan, Guinea-Bissau, Ethiopia, Niger,
# Liberia, Sierra Leone, Madagascar, Zambia, Eritrea
thirdWorldCountries = ['TLS','MWI','SOM','COD', 'TZA','YEM','BDI','AFG','GNB','ETH','NER','LBR', 'SLE', 'MDG', 'ZMB','ERI']

#carbon emissions
variables = ['_EN_', '_ATM_''_EG_', '_FRST_','DCO2', 'DFOR', '_ENVR_', '_AGR_']

fw_energy_codes=[]
for c in codes:
    if any(v in c for v in variables):
        for firstWorld in firstWorldCountries:
            if firstWorld in c:
                fw_energy_codes.append(c)

tw_energy_codes=[]
for c in codes:
    if any(v in c for v in variables):
        for thirdWorld in thirdWorldCountries:
            if thirdWorld in c:
                tw_energy_codes.append(c)

fw_response_var = []
for c in codes:
    if "EN_ATM_CO2E_KT" in c:
        for f in firstWorldCountries:
            if f in c:
                fw_response_var.append(c)
                
tw_response_var = []
for c in codes:
    if "EN_ATM_CO2E_KT" in c:
        for t in thirdWorldCountries:
            if t in c:
                tw_response_var.append(c)


fw_response_emission = Quandl.get(fw_response_var, authtoken = "i6p4EbYvvD82eQktMuzy")
tw_response_emission = Quandl.get(tw_response_var, authtoken = "i6p4EbYvvD82eQktMuzy")
fw_total_emission = fw_response_emission.sum(axis = 1)
tw_total_emisison = tw_response_emission.sum(axis =1)
fw_total_emission.to_csv('FirstWorldEmissionData.csv')
tw_total_emisison.to_csv('ThirsdWorldEmissionData.csv')

# all the energy codes that have relative values (zs,zg) are placed in one df, and the remainder in another
fw_energy_zs = []
fw_energy_nazs = []
for f in fw_energy_codes:
    if "_ZS" in f:
        fw_energy_zs.append(f)
    elif "_ZG" in f:
        fw_energy_zs.append(f)
    else:
        fw_energy_nazs.append(f)
        
tw_energy_zs = []
tw_energy_nazs = []
for t in tw_energy_codes:
    if "_ZS" in t:
        tw_energy_zs.append(t)
    elif "_ZG" in t:
        tw_energy_zs.append(t)
    else:
        tw_energy_nazs.append(t)
fw_df_zs = Quandl.get(fw_energy_zs, authtoken = "i6p4EbYvvD82eQktMuzy")
fw_df_nazs = Quandl.get(fw_energy_nazs, authtoken = "i6p4EbYvvD82eQktMuzy")
tw_df_zs = Quandl.get(tw_energy_zs, authtoken = "i6p4EbYvvD82eQktMuzy" )
tw_df_nazs = Quandl.get(tw_energy_nazs, authtoken = "YQuENVPAuCTd2ivckrjp")

#### for c in fw_df_zs.columns:
#     code = c.split('_')
#     print code[len(code)-2]

grouped_fw_zs = fw_df_zs.groupby(lambda x :x.split('_')[len(x.split('_'))-3]+"_"+ x.split('_')[len(x.split("_"))-2], axis = 1)
grouped_fw_nazs = fw_df_nazs.groupby(lambda x : x.split('_')[len(x.split('_'))-3]+'_'+x.split('_')[len(x.split('_'))-2]+'_'+x.split('_')[len(x.split('_'))-1], axis = 1)
grouped_tw_zs = tw_df_zs.groupby(lambda x : x.split('_')[len(x.split('_'))-3]+'_'+x.split('_')[len(x.split('_'))-2], axis = 1)
grouped_tw_nazs = tw_df_nazs.groupby(lambda x : x.split('_')[len(x.split('_'))-3]+"_"+x.split('_')[len(x.split('_'))-2]+"_"+x.split('_')[len(x.split('_'))-1], axis =1)

df_grouped_fw_zs_mean =grouped_fw_zs.mean()
df_grouped_fw_nazs_sum = grouped_fw_nazs.sum()
df_grouped_tw_zs_mean = grouped_tw_zs.mean()
df_grouped_tw_nazs_sum = grouped_tw_nazs.sum()

# We toss out columns that are null or have less than half of the data we need
first_world_zs_data = df_grouped_fw_zs_mean.dropna(thresh= (len(df_grouped_fw_zs_mean)/2), axis =1)
first_world_nazs_data = df_grouped_fw_nazs_sum.dropna(thresh= (len(df_grouped_fw_nazs_sum)/2), axis =1)
third_world_zs_data = df_grouped_tw_zs_mean.dropna(thresh= (len(df_grouped_tw_zs_mean)/2), axis =1)
third_world_nazs_data = df_grouped_tw_nazs_sum.dropna(thresh= (len(df_grouped_tw_zs_sum)/2), axis =1)



first_world_zs_data.to_csv('FirstWorldRelativeExplantoryData.csv')
first_world_nazs_data.to_csv('FirstWorldAbsoluteExplantoryData.csv')
third_world_zs_data.to_csv('ThirdWorldRelativeExplanatoryData.csv')
third_world_nazs_data.to_csv('ThirdWorldAbsoluteExplanatoryData.csv')