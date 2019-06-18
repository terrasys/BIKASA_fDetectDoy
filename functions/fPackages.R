#-----------------------------------------------------------------------------------------------------
print("Load all required packages and dowload/install non-existent packages")
#-----------------------------------------------------------------------------------------------------
loadandinstall <- function(mypkg=c("automap",
                                   "gtools",
                                   "chron",
                                   "data.table",
                                   "doSNOW",
                                   "foreign",
                                   "geosphere",
                                   "grid",
                                   "gstat",
                                   "lattice",
                                   "parallel",
                                   "raster",
                                   "RCurl",
                                   "rgdal",
                                   "RSAGA",
                                   "stringr",
                                   "utils",
                                   "sf")) {
  print('Loading required R-Packages or installing them, if not already installed')
  for(i in seq(along=mypkg)){
    if (!is.element(mypkg[i],installed.packages()[,1])){install.packages(mypkg[i])}
    library(mypkg[i], character.only=TRUE)
  }
}
loadandinstall()
#-----------------------------------------------------------------------------------------------------
#rsaga environment
#-----------------------------------------------------------------------------------------------------
myenv <- rsaga.env(workspace=OUT.DIR, 
                   path="c:/_saga_222_x64",
                   modules = "c:/_saga_222_x64/modules")
rsaga.get.libraries(path = myenv$modules)
rsaga.get.lib.modules("grid_gridding", env =  myenv, interactive = FALSE)
rsaga.get.usage("grid_gridding",0,env =  myenv)
