#-----------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
print("Working directory, data and settings")
#-----------------------------------------------------------------------------------------------------
source(file.path(FUNC.DIR,"fPackages.R"))
source(file.path(FUNC.DIR,"fDetectDoy.R"))
TS.SHP = ".../BIKASA_DetectDoy/_input/ST_LO_epsg31468.shp"
PP.DIR = ".../Dropbox/_git/BIKASA_DetectDoy/_input/"
OUT.DIR = ".../Dropbox/_git/BIKASA_DetectDoy/_output/"
FUNC.DIR = ".../Dropbox/_git/BIKASA_DetectDoy/_functions/"
YEARS=1993:2018

PLANT=202#Winter Wheat
PHASES=c(10,12,15,18,19,21,24)
fDetectTempSum(TS.SHP=TS.SHP,
               PP.DIR=PP.DIR,
               OUT.DIR=OUT.DIR,
               YEARS=YEARS,
               PLANT=PLANT,
               PHASES=PHASES)
PLANT=215#Maize
PHASES <- c(10,12,67,5,65,19,20,21,24)
fDetectTempSum(TS.SHP=TS.SHP,
               PP.DIR=PP.DIR,
               OUT.DIR=OUT.DIR,
               YEARS=YEARS,
               PLANT=PLANT,
               PHASES=PHASES)

