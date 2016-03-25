# WWDI-CodeBase
===============

###Mining the Data:
- Running WWDI-2.ipynb (must have jupyter installed) will locally download datasets of interest
- wwdi_indicators will have all the variables while country_codes has their respective countries
- In order to call a dataset from Quandl you must use a Quandl code of the form: "WWDI/" + [country_code]_[indicator_code]
- The last part of the indicator code is generally the unit of measurement("KT", "MT", "ZS") while the first substring is the sector if interest ("EN", "EG", "NY")

###Analysis:
- all analysis will be done in R on reg.r
- In order to begin analysis, clone repository and run R in the same directory
