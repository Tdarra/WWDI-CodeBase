



countrycodes = []
searchfile = open("country_codes", "r")
for line in searchfile:
    if "|" in line: 
         countrycodes.append(line[-4:].strip(' \t\n\r'))
searchfile.close()

indicatorcodes = []
temp = []
otherfile = open("wwdi_indicators", "r")
for line in otherfile:
    if "|" in line: 
        indicatorcodes.append(line[line.index("|")+1:].strip(' \t\n\r'))
otherfile.close()

codes = []
QUANDL_PREFIX = "WWDI/"
for i in indicatorcodes:
    for c in countrycodes:
        codestring = QUANDL_PREFIX + c + "_" + i
        codes.append(codestring)
print codes

### All the codes haha
