import Quandl
import pandas
import matplotlib.pyplot as plt

wwdi_str = 'WWDI/'


# countries of the first world http://www.nationsonline.org/oneworld/third_world_countries.htm
# Luxembourg, Norway, US, Ireland, Bermuda, Iceland, Denmark, San Marino, Canada, Switzerland
firstWorldCountries= ['LUX', 'NOR', 'USA', 'IRL', 'DNK', 'SMR', 'CAN','CHE']

# Third world countries:
# Timor-Leste, Malawi, Somalia, Congo, Tanzania, Yemen, Burundi, Afghanistan, Guinea-Bissau, Ethiopia, Niger,
# Liberia, Sierra Leone, Madagascar, Zambia, Eritrea
thirdWorldCountries = ['TLS','MWI','SOM','COD', 'TZA','YEM','BDI','AFG','GNB','ETH','NER','LBR', 'SLE', 'MDG', 'ZMB','ERI']

#carbon emissions
co2_pc = '_EN_ATM_CO2E_PC'
co2_tot = '_EN_ATM_CO2E_KT'

fw_co2pc=[]
fw_co2tot=[]
for country in firstWorldCountries:
    getCode_co2pc = wwdi_str+country+co2_pc
    getCode_co2tot = wwdi_str+country+co2_tot
    fw_co2pc.append(getCode_co2pc)
    fw_co2tot.append(getCode_co2tot)


tw_co2pc=[]
tw_co2tot=[]
for country in thirdWorldCountries:
    getCode_co2pc = wwdi_str+country+co2_pc
    getCode_co2tot = wwdi_str+country+co2_tot
    tw_co2pc.append(getCode_co2pc)
    tw_co2tot.append(getCode_co2tot)



fw_co2pc_data = Quandl.get(fw_co2pc)
#fw_co2tot_data = Quandl.get(fw_co2tot)
tw_co2pc_data = Quandl.get(tw_co2pc)
#tw_co2tot_data = Quandl.get(tw_co2tot)


