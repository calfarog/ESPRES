################################################################################
# Author: Angel Udias     all complaints to: angel.udias-moinelo@ec.europa.eu  #
################################################################################
# Started: 21-Jan-2018 ->    ;                                                 #
# Updates:                                                                     #
#                                                                              #
# ESPRES: Efficient Strategies for anthropogenic Pressure Reduction in European waterSheds                                                                             #
#                  Loading Data file                                           #
#                                                                              #
# ## THE QUANTITY MODULES ##                                       #
################################################################################



####################################### 
## 1.Loading some required libraries  #
####################################### 
Loading_libraries <- function(verbose=TRUE){
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  Loading_libraries' ...                             ]")
  if (verbose) message("=======================================================") 
  

  if (!require(maptools)){   install.maptools("maptools", lib="C:/R/R-3.0.1/library")  } 

  library(ncdf4)
  library(plyr)  #para usar ddply
  library(Hmisc)  #para las medianas ponderadas
  library(pmr)   #for the ahp, weithing the user opinion
  library(mco)
  library(nsga2R)
  library(reshape)   #para melt
  library(reshape2)   # y los dcast
  library(ggplot2)
  library(networkD3)
  
  #TODAS LAS SIGUIENTES SON PARA LOS MAPAS
  library("sp")
  library("maptools")
  library("rgdal")
  
  library("fields")
  library("RColorBrewer")
  library("classInt")
  
  library(rgeos)
  
  
}


# Path <- BASE_Shapes
# N_Shapename <- NUTS2_shape       
# B_Shapename <- Basin_shape
# C_Shapename  <- Catch_shape
###############################################
## 2. Loading the shapes to create the Maps
###############################################
Loading_MapShapes <- function(Path,N_Shapename,B_Shapename,C_Shapename,verbose=TRUE){
  #  library(maptools)
  
  #  library(sp)
  #  if (!require(rgdal)){  install.rgdal("rgdal", lib="C:/R/R-3.0.1/library")  } 
  #  if (!require(ggplot2)){  install.ggplot2("ggplot2", lib="C:/R/R-3.0.1/library")  } 
  #  if (!require(plyr)){ install.plyr("maptools", lib="C:/R/R-3.0.1/library")  }
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  Loading shapes to plot the maps ...                 ]")
  if (verbose) message("=======================================================")
  
  # Lectura de shapes (como poligonos y como lineas)  NUTS2
  ### LAS REGIONES (NUS2)  (cargo los poligonos)  NUTS2
  MyPolReg <- readShapePoly(paste(Path,N_Shapename,sep="/"))
  class(MyPolReg)
 # plot(MyPolReg)
  # see projection
  summary(MyPolReg)
  
  #lo mismo pero leido de otra forma, por ahora no lo uso
#  MyPolReg2 <- readShapeSpatial(paste(Path,N_Shapename,sep="/"))
#  summary(MyPolReg2)
  # plot(MyPolReg2)  #
  # names(MyPolReg)
  #  MyPolReg$objectid  
  MyLinReg <- readShapeLines(paste(Path,N_Shapename,sep="/"))   #LINEAS DE CONTORNO DE LOS NUTS
  plot(MyLinReg)  #linea de contorno de los N2
  
  #library(rgdal)
  # readOGR(dsn = Path,   layer = "nuts2esrilaea_polygons/nuts2esrilaea_polygons")
  
  
  ### LAS CUENCAS (BASIN), POR AHORA EMPLEO EL MISMO SHAPE FILE QUE USO PARA WATER QUALITY (deberian ir bien)
      #  los poligonos)
    loadshapepath <- paste(Path,B_Shapename,sep="/")
    MyPolBas <- readShapePoly(loadshapepath)
  
  
    # LAS LINEAS
    loadshapepath <- paste(Path,B_Shapename,sep="/")     # LAS lineaS del borde
    MyLinBas <- readShapeLines(loadshapepath)      
    
    class(MyLinBas)
    MyLinBas@bbox
    summary(MyLinBas)
    attributes(MyLinBas@data)
    attributes(MyLinBas@data)$names
    str(MyLinBas@data)
  

  

  
  ### LAS SUBCUENCAS  (carga los poligonos)
  MyPolCatch <- readShapePoly(paste(Path,C_Shapename,sep="/"))
  # names(MyPolCatch)
  # MyPolCatch$hydroid
  # dim(MyPolCatch)
  #kk<-MyPolCatch[MyPolCatch$hydroid=="2000001",]
  #dim(kk)
  
  ### LAS SUCUENCASCUENCAS  (carga los lineas de contorno)
  MyLinCatch <- readShapeLines(paste(Path,C_Shapename,sep="/"))
  # names(MyLinCatch)
  # MyLinCatch$hydroid
  # dim(MyLinCatch)
  

  
  
  ## los limites de los MyLinReg en undata frame
  DF_lim_map <- data.frame(contry="PL",xlim1=4600000, xlim2=5300000,ylim1=2900000, ylim2=3550000)
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="HU",xlim1=4790000, xlim2=5280000,ylim1=2550000, ylim2=3000000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="FR",xlim1=3250000, xlim2=4300000,ylim1=2000000, ylim2=3100000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="ES",xlim1=2600000, xlim2=3800000,ylim1=1550000, ylim2=2450000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="PT",xlim1=2600000, xlim2=3000000,ylim1=1700000, ylim2=2300000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="UK",xlim1=3150000, xlim2=3800000,ylim1=3100000, ylim2=4200000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="IT",xlim1=4000000, xlim2=5100000,ylim1=1500000, ylim2=2700000))  
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="DE",xlim1=4050000, xlim2=4700000,ylim1=2650000, ylim2=3550000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="SI",xlim1=4500000, xlim2=4900000,ylim1=2450000, ylim2=2700000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="BE",xlim1=3750000, xlim2=4100000,ylim1=2900000, ylim2=3200000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="RO",xlim1=5100000, xlim2=5900000,ylim1=2350000, ylim2=2950000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="NL",xlim1=3800000, xlim2=4200000,ylim1=3050000, ylim2=3400000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="AT",xlim1=4000000, xlim2=5000000,ylim1=2500000, ylim2=3000000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="BG",xlim1=5000000, xlim2=6000000,ylim1=2000000, ylim2=2500000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="CZ",xlim1=4300000, xlim2=5000000,ylim1=2700000, ylim2=3200000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="DK",xlim1=4200000, xlim2=4600000,ylim1=3400000, ylim2=4500000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="EE",xlim1=5000000, xlim2=5500000,ylim1=3900000, ylim2=4300000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="FI",xlim1=4500000, xlim2=5500000,ylim1=4000000, ylim2=6000000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="IE",xlim1=3000000, xlim2=3500000,ylim1=3200000, ylim2=3900000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="LT",xlim1=5000000, xlim2=5500000,ylim1=3400000, ylim2=3900000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="LU",xlim1=2652500, xlim2=7942500,ylim1=1457500, ylim2=4787500))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="LV",xlim1=2652500, xlim2=7942500,ylim1=1457500, ylim2=4787500))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="HL",xlim1=2652500, xlim2=7942500,ylim1=1457500, ylim2=4787500))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="SE",xlim1=4000000, xlim2=5000000,ylim1=3400000, ylim2=6000000))
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="SK",xlim1=2652500, xlim2=7942500,ylim1=1457500, ylim2=4787500))  
  
  DF_lim_map <- rbind(DF_lim_map, data.frame(contry="ALL",xlim1=2652500, xlim2=7942500,ylim1=1457500, ylim2=4787500))
  
  lista <- list(MyPolReg,MyLinReg,MyPolBas,MyLinBas,MyPolCatch,MyLinCatch,DF_lim_map)
  return(lista)  
}








###############################################
## 3. Loading Water Quantity Information
###############################################
# Path: Directorio donde esta el fichero ncdf
# netfilename: nombre del fichero netcdf
#  Path <- path_ncdf
#  netfilename <- ncname
Loading_WQTnInfo <- function(Path,netfilename,verbose=TRUE  ){
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  Loading Water Quantity Information ...             ]")
  if (verbose) message("=======================================================")
  
  ncfname <- paste(Path,netfilename, ".nc", sep = "")
  
  # open a NetCDF4 file
  ncin <- nc_open(ncfname)
  #  class(ncin)
  # print(ncin)  #informacion contenida en el fichero
  # summary(ncin)
  #  names(ncin$var)  #los nombre de las variables
  #podria cargarlas todas automaticas, pero por el momento cargo algunas
  
  #tenemos dos primary keys : NUTS2 y Catchment
  dname <- "nuts2esrilaea.objectid" 
  Nuts2ObjID <- ncvar_get(ncin, dname)
  #  class(Nuts2ObjID)
  #  dim(Nuts2ObjID)   #331 nuts2
  #  head(Nuts2ObjID)  #recordar que el 0 es el que no tiene identificacion
  
  dname <- "catchment.hydroid"
  CatchmentID <- ncvar_get(ncin, dname)
  #  class(CatchmentID)
  #  dim(CatchmentID)   #32960 catchment
  #  head(CatchmentID)
  
  #TODAS LAS SIGUIENTES SON matriz tiene un valor para cada Catchment y NUTS2. Muchos ceros
  
  # ###### LA VARIABLE DE INTERSECCION: Fraction of catchment area intersected by NUTS2
  dname <- "intersect_fraction" 
  intersec_fracc <- ncvar_get(ncin, dname)
  rownames(intersec_fracc) <-CatchmentID  #ponemos el nombre del catchmen a cada fila
  colnames(intersec_fracc) <-Nuts2ObjID   #nombre de provincia a cada columna
  # class(intersec_fracc)
  #  dim(intersec_fracc)   #filas son los catchment colums los NUTS3
  # head(intersec_fracc)
  
  # LA VARIABLE routed_Freshwater
  dname <- "routed_Freshwater" 
  r_FresW <- ncvar_get(ncin, dname)
  # head(r_fresW)
  r_FresW <- rowSums(r_FresW) #sumo fila a fila, y tengo un valor por cada Subcatchment
  #  head(rowSums(r_fresW))  #La relacion de subcatchment y NUTS2 la saco de intersec_fracc. Asi que solo quiero el total
  names(r_FresW) <- CatchmentID  #ponemos el nombre del catchmen a cada fila
#  head(r_FresW[1:5,1:7])
  
  #Las DEMANDAS SON MATRICES, Ya que en cada columna contienen lo que influye la demanda de dicha provincia aguas abajo en el routing de las demanda
  #Tengo que mantenerlas asi para cuando aplique las reducciones por provincias necesito aplicarlo a cada columana
  #Luego lo restares al fresWater y luego sumo todas las columnas de la fila
  # LA VARIABLE routed_Domestic Water Demand
  dname <- "routed_Domestic" 
  r_DomestWD <- ncvar_get(ncin, dname)
  #  r_DomestWD <- rowSums(r_DomestWD)
  rownames(r_DomestWD) <- CatchmentID  #ponemos el nombre del catchmen a cada fila
  colnames(r_DomestWD) <- Nuts2ObjID   #y el nombre del nuts a cada columna
 # head(r_DomestWD[1:5,180:188])

  
  # LA VARIABLE routed_Industry Water Demand
  dname <- "routed_Industry" 
  r_IndustWD <- ncvar_get(ncin, dname)
  #  r_IndustWD <- rowSums(r_IndustWD)
  rownames(r_IndustWD) <-CatchmentID  #ponemos el nombre del catchmen a cada fila
  colnames(r_IndustWD) <- Nuts2ObjID
  
  # LA VARIABLE routed_Energy  Water Demand
  dname <- "routed_Energy" 
  r_EnergWD <- ncvar_get(ncin, dname)
  # r_EnergWD <- rowSums(r_EnergWD)
  rownames(r_EnergWD) <-CatchmentID  #ponemos el nombre del catchmen a cada fila
  colnames(r_EnergWD) <- Nuts2ObjID
  
  # LA VARIABLE routed_Livestock  Water Demand
  dname <- "routed_Livestock" 
  r_LivesWD <- ncvar_get(ncin, dname)
  # r_LivesWD <- rowSums(r_LivesWD)
  rownames(r_LivesWD) <-CatchmentID  #ponemos el nombre del catchmen a cada fila
  colnames(r_LivesWD) <- Nuts2ObjID
  
  # LA VARIABLE routed_Irrigation  Water Demand
  dname <- "routed_Irrigation" 
  r_IrrWD <- ncvar_get(ncin, dname)
  # r_IrrWD <- rowSums(r_IrrWD)
  rownames(r_IrrWD) <-CatchmentID  #ponemos el nombre del catchmen a cada fila
  colnames(r_IrrWD) <- Nuts2ObjID
  
  #Examino informacion variable Catchmet Area
  dname <- "catchment_area" 
  c_area <- ncvar_get(ncin, dname)
  names(c_area)<-CatchmentID
  
  #devuelvo todos
  mylist <- list(Nuts2ObjID,CatchmentID,intersec_fracc,r_FresW,r_DomestWD,r_EnergWD,r_IndustWD,r_IrrWD,r_LivesWD,c_area)  
  return(mylist) 
  
}



###############################################
## 4. Loading Catchment - Subcatchment pertenence
###############################################
#  Path <- PATH_C_SC
#  filename <- filename
Loading_C_SC_Rel <- function(Path,filename,verbose=TRUE  ){
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  Loading Catchment - Subcatchment pertenence ...     ]")
  if (verbose) message("=======================================================")
  
  fname <- paste(Path,filename, sep = "")
  DatosC_SC_Rel <- read.csv(file=fname,head=TRUE,sep=",")
  # head(DatosC_SC_Rel)
  
  return(DatosC_SC_Rel)   
}

###############################################
## 5. Loading NUTS long names and correspondence with Emiliano ID
###############################################
#  Path <- BASE_IN_DATA
#  filename <- filename
Loading_NUTS_Corr <- function(Path,filename,verbose=TRUE  ){
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  Loading Nuts long names and Emiliano ID correspondece ...     ]")
  if (verbose) message("=======================================================")
  
  fname <- paste(Path,filename, sep = "")
  Nuts_base <- read.csv(file=fname,head=TRUE,sep=",")
  # solo las columnas que me interesa 
  Nuts_base <- Nuts_base[,c(1,2,4,5)]
  #y por ahora dejo solo cuando la fila tiene blancos

  Nuts_corr <-Nuts_base[!(Nuts_base$NUTS_ID==" "), ]
  
  return(Nuts_corr)   
}



# theBasin <- BasinName
# Path <- BASE_IN_DATA 
###############################################
## 5. Loading the maximun pressure reduction
###############################################
Loading_Max_PressRed <- function(Path,theBasin,verbose=TRUE  ){
  
    if (verbose) message("================================================================================")
    if (verbose) message("[                Loading maximun rate preassures reduction' ...                   ]")
    if (verbose) message("================================================================================") 
    # 2.1 Loading Maximun rate reduction (by NUTS2 & TYPE OF PRESSURE)
    MaxRateRed <- MyCSV_load(Path,"RateMaxRedByPressNut.csv" )
    
    MaxRateRed2  <- MaxRateRed[MaxRateRed$CatchName == toupper(theBasin),  ]
    
    #por si no venian reordenadas, las ordeno alfabeticamente de acuerdo a ambos factores
    MaxRateRed3 <- with(MaxRateRed2, MaxRateRed2[order( Press,Nuts2),])  #para ordenar de acuerdo a multiples
    
    return(MaxRateRed3)
}



################################################################################
MyCSV_load <- function(dirBLS, csvfile, verbose=FALSE  ){ 
  
  textmesage <- paste("[                Loading ",csvfile ,"  ...                   ]") 
  textmesage2 <- paste("There are not:   ",  csvfile,   "  file")
  
  if (verbose) message("================================================================================")
  if (verbose) message (textmesage)
  if (verbose) message("================================================================================") 
  
  
  filenames <-  list.files(as.vector(dirBLS), full.names = TRUE, pattern= csvfile)
  if(length(filenames) == 0 ) stop(textmesage2)
  df <- read.table(filenames, header = TRUE, sep = ",", quote = "\"'",    dec = ".")
  return(df)
  
  
} # MyCSV_load


