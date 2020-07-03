################################################################################
# Author: Angel Udias     all complaints to: angel.udias-moinelo@ec.europa.eu  #
################################################################################
# Started: 21-Jan-2018 ->    ;                                                 #
# Updates:                                                                     #
#                                                                              #
# ESPRES: Efficient Strategies for anthropogenic Pressure Reduction in European waterSheds                                                                             #
#                  Loading Data file                                           #
#                                                                              #
# DESCRIPTION :                                                                #
# Afther choose a catchment, the program detect the NUTS contained             #
# Then iteratively it apply individual reduction for each NUTS and tipe of pressure in the 
# previously selected catchment. The water quality related type of pressure reduction considered are:
# CoeffManN : Nitrates manure agricola                                         #
# CoeffMinN : Nitrates mineral agricola 
# CoeffPsN  : Nitrates point sources (urban & industrial) 
# CoeffSdW  : Nitrates scatter dwelling                                        #
################################################################################


  
####################################### 
## 1.Loading some required libraries  #
####################################### 
Loading_libraries <- function(verbose=TRUE){
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  Loading_libraries' ...                             ]")
  if (verbose) message("=======================================================") 
  
 
#  if (!require(maptools)){   install.maptools("maptools", lib="C:/R/R-3.0.1/library")  } 

  library("ncdf4")
  library("mco")
  library("nsga2R")
  
  library("plyr")  #para usar ddply
  library("Hmisc")  #para las medianas ponderadas wtd.quantile
  library("pmr")   #for the ahp, weithing the user opinion
  library("reshape")   #para melt
  library("reshape2")   # y los dcast
  library("ggplot2")
  library("networkD3")   #para el sankey

  
  #TODAS LAS SIGUIENTES SON PARA LOS MAPAS
  library("sp")
  library("maptools")
  library("rgdal")

  library("fields")
  library("RColorBrewer")
  library("classInt")

  library("rgeos")
  
}


# Path <- BASE_Shapes 
# NameShNuts <- "Nuts2Vr2013.shp" 
# NameShBas <- "basin.shp"
# NameCatchNuts <- "catchment.shp"
###############################################
## 2. Loading the shape to plot the Maps
###############################################
Loading_MapShapes <- function(Path,NameShNuts,NameShBas,NameCatchNuts,verbose=TRUE){
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  Loading shapes to plot the maps ...                 ]")
  if (verbose) message("=======================================================")
  
  #########################
  ### LAS REGIONES (NUS2)  
  
    #  POLIGONOS N2
    loadshapepath<-paste(Path,"Nuts2",NameShNuts,sep="/")
    MyPolReg <- readShapePoly(loadshapepath)
    
    class(MyPolReg)
    MyPolReg@bbox
    summary(MyPolReg)
    attributes(MyPolReg@data)
    attributes(MyPolReg@data)$names
    str(MyPolReg@data)
  
    # LINEAS DE CONTORNO N2
    loadshapepath<-paste(Path,"Nuts2",NameShNuts,sep="/")
    MyLinReg <- readShapeLines(loadshapepath)   #LINEAS DE CONTORNO DE LOS NUTS
    plot(MyLinReg)
    attributes(MyLinReg@data)$names
    str(MyLinReg@data)

    
  #########################
  ### LAS CUENCAS (BASIN)
    
    #  POLIGONOS BASIN
    loadshapepath<-paste(Path,"basin",NameShBas,sep="/")
    MyPolBas <- readShapePoly(loadshapepath)
    
    # LINEAS DE CONTORNO BASIN
    loadshapepath<-paste(Path,"basin",NameShBas,sep="/")
    MyLinBas <- readShapeLines(loadshapepath)      
    plot(MyLinBas)
    
    class(MyLinBas)
    MyLinBas@bbox
    summary(MyLinBas)
    attributes(MyLinBas@data)
    attributes(MyLinBas@data)$names
    str(MyLinBas@data)
  

  ##########################
  #### LAS SUBCUENCAS . ATENCION: las subcuencas aqui son mucho mas pequenas que en la version cantidad
   
    # POLIGONOS SUBCUENCAS
    loadshapepath<-paste(Path,"catchment",NameCatchNuts,sep="/")
    MyPolCatch <- readShapePoly(loadshapepath)
  
    # LINEAS SUBCUENCAS
    loadshapepath<-paste(Path,"catchment",NameCatchNuts,sep="/")
    MyLinCatch <- readShapeLines(loadshapepath) 
    plot(MyLinCatch)
  
  lista <- list(MyPolReg,MyLinReg,MyPolBas,MyLinBas,MyPolCatch,MyLinCatch)
  return(lista)  
}



# pathBLS <- BASE_IN_DATA
# ctchName <- BasinName
LoadDataCatch <- function(pathBLS,ctchName, verbose=TRUE ){ 
  
  
  if (verbose) message("================================================================================")
  if (verbose) message("[                Loading NUTS Catch Relation and area and lengt' ...         ]")
  if (verbose) message("================================================================================") 
  
  pathcommon <-paste0(pathBLS,"inputs","/","common")
  
  setwd(pathcommon)   #por si acaso 
  
  # 1.1 Loading the relational and area table
  Relation <- MyCSV_load(pathcommon,"SubcatchNutsArea.csv" )
  nrow(Relation)
  
  # 1.2. Subseting by the study Catchmen
  ctchNUTSRel <- Relation[Relation$CatchName ==toupper(ctchName),  ]     #atencion con los upper o lower case
  str(ctchNUTSRel)
  nrow(ctchNUTSRel)
  head(ctchNUTSRel)
  unique(ctchNUTSRel$Nuts2)

  
  toBeRemoved <- which(ctchNUTSRel$Ctry=="NULL")  #seran los hYdroID de Andorra o similares
  if( length(toBeRemoved) != 0 )    ctchNUTSRel <- ctchNUTSRel[-toBeRemoved,  ]  #solo elimina los registros cuando los hay, sino daria un problema
      

  # 1.3. Removing levels that no data
  ctchNUTSRel$Nuts2 <- factor(ctchNUTSRel$Nuts2)
  ctchNUTSRel$Ctry <- factor(ctchNUTSRel$Ctry)
  ctchNUTSRel$CtryCode <- factor(ctchNUTSRel$CtryCode)
  str(ctchNUTSRel)
  
  

 
  #  unique(ctchNUTSRel$Nuts2)
  ## ATENCION CON LOS NULL, son andorra
   message("================================================================================")
   message("[                Loading BLS Concentration' ...                   ]")
   message("================================================================================") 
  
  pathcatch <- paste0(pathBLS,"inputs/",ctchName)
  setwd(pathcatch)   #por si acaso 
  
  # 1. Loading BLS subcatch concentration
  BLS_Subc_Conc <- MyCSV_load(pathcatch,"BLS_SubCatch_Conc.csv" )
  
  # 2. Mezclo en un unico DF la concentracion y la informacion correspondiente NUTS2, Ctry , Area, Longitud
  DFConcentration <- merge(BLS_Subc_Conc, ctchNUTSRel, by="HydroID")                   #  TheBLSConc$Nuts3 <- TheCathNut$Nuts2[ match( TheBLSConc$HydroID,TheCathNut$HydroID) ]  #cuidado 
  nrow(DFConcentration)
  # str(DFConcentration)

  # 3.  Loading Concentration Reduction by pression and subcatch 
  message("================================================================================")
  message("[           Loading la reduccion de concentraciones al reduccir las presiones  ]")
  message("================================================================================")
  
  # it is the reduction of the concentration in each subcatchment when apply a 1% of reduction of one pressure in one Nut
  ConcReduct <- MyCSV_load(pathcatch,"ConcReductByPress_Nuts.csv" )
  nrow(ConcReduct)
  
  #elimino las columnas con NA (andorra)
  ConcReduct2 <- ConcReduct[ , !grepl( "NA_" , names( ConcReduct ) ) ]
  #LOS NA DEL EBRO SE DEBEN A ANDORRA
  #### ATENCION ####
  # Para que la operacion de evaluacion (multiplicacion matricial) funcione bien, el orden de las cosas en esta matriz debe ser correcta
  # Es decir:           
  #    1 :"Man"    
  #    2 :"Min"
  #    3 :"PS"
  #    4 :"SDW"
  # Si las matrices no fueron cargadas de esa forma, la evaluacion no sera correcta
  

  
  # 4. Loading reduccion de LOAD (presiones) por tipo de presion y subcuenca
  message("================================================================================")
  message("[           Loading la reduccion de LOAD (presiones)   ]")
  message("================================================================================")
  
  # it is the reduction of LOAD in each subcatchment when apply a 1% of reduction of one pressure in one Nut
  LoadReduct <- MyCSV_load(pathcatch,"LoadNoCumulativReductByPress_SubC.csv" )

  #elimino las columnas con NA (andorra)
  LoadReduc2 <- LoadReduct[ , !grepl( "NA_" , names( LoadReduct ) ) ]
  
  
  # 5. Extracting the NUTS from the previous outpus
  strTemp <- colnames(ConcReduct2)
  strTemp2 <- strTemp[3:length(strTemp)]  #elimino los dos primeros que son el HydroID y el year
  strTemp3 <- matrix(unlist( strsplit(strTemp2, "_") ), ncol=2, byrow=TRUE)[,1] # separo el nombre del nuts del id de la presion
  TheNuts <- unique(strTemp3)  

  # 5. Removing the Nuts where there apply pressure reduction there is not effect in the subcatchment 
  for (nut in TheNuts){  #evaluo para cada Nuts si todas las reducciones son cero
    # nut<-"ES13"
    # print(nut)
    sub_ConcReduct <- ConcReduct2[ , grepl( nut , names( ConcReduct2 ) ) ] #subset de cada nuts
    if( sum(sub_ConcReduct,na.rm=TRUE)== 0 )  ConcReduct2 <- ConcReduct2[ , !grepl( nut , names(ConcReduct2) ) ]  #si no hay reduccion elimino sus columnas 
  }
  #ahora en el DF ConcReduct, ya solo hay NUTS en los que se producen reducciones
  
  # 4. Extracting the NUTS donde aplicando reduccion de presiones no se produce algun efecto    
  strTemp <- colnames(ConcReduct2)
  strTemp2 <- strTemp[3:length(strTemp)]  #elimino los dos primeros que son el HydroID y el year
  strTemp3 <- matrix(unlist( strsplit(strTemp2, "_") ), ncol=2, byrow=TRUE)[,1] # separo el nombre del nuts del id de la presion
  TheNuts_EF <- unique(strTemp3)  #ya me quedo con los NUTS, ATENCION A RESOLVER LOS NA
  

  ####################################################################
  ####################################################################
  # 4.3. verifico coincidencia de los hydroID  (NO DEBERIA HABER NINGUNA LINEA)
  subset(ctchNUTSRel, !(DFConcentration$HydroID %in% ctchNUTSRel$HydroID))
  subset(ctchNUTSRel, !(ctchNUTSRel$HydroID %in% DFConcentration$HydroID))
  
  
  return(list (DFConcentration=DFConcentration , ConcReduct=ConcReduct2,
               TheNuts=TheNuts, TheNuts_EF=TheNuts_EF, LoadReduct=LoadReduc2))
  
}




# theBasin <- BasinName
# Path <- BASE_IN_DATA 
###############################################
## 5. Loading the maximun pressure reduction
###############################################
Loading_Max_PressRed_QL <- function(Path,theBasin,verbose=TRUE  ){
  
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
# Author: Angel Udias                                          #
################################################################################
# Started: 15-Nov-2016 -> 03 Feb 20017;                                         #
# Updates: 06-Mar-16                                                           #
#  #funcion to real many of the csv files                                      #
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






# Path <- pathShapefile 
# Shapename <- nameShapefile
###############################################
## 2. Loading Info to create the Maps
###############################################
Loading_shapeFile <- function(Path,Shapename,verbose=TRUE){
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  Loading shape file to create the Maps ...                 ]")
  if (verbose) message("=======================================================")
  
  # Lectura de shapes (como poligonos y como lineas)  NUTS2
  poligonos <- readShapePoly(paste(Path,Shapename,sep="/"))
  class(poligonos)
  summary(poligonos)    # see projection
  
  superficies <-readShapeSpatial(paste(Path,Shapename,sep="/"))
  summary(superficies)
  # names(pol.paises)
  #  pol.paises$objectid
  lineas<- readShapeLines(paste(Path,Shapename,sep="/"))
  
  #library(rgdal)
  # readOGR(dsn = Path,   layer = "nuts2esrilaea_polygons/nuts2esrilaea_polygons")
  
  lista <- list(poligonos,superficies,lineas)
  return(lista)  
}






# # pathBLS <- IN_DIR
# # ctchName <- catchName
# LoadCatchNutsRel <- function(pathBLS,ctchName, verbose=TRUE ){ 
# 
#   if (verbose) message("================================================================================")
#   if (verbose) message("[                Loading NUTS Catch Relation and area' ...         ]")
#   if (verbose) message("================================================================================") 
#   
#   setwd(pathBLS)   #por si acaso 
#   
#   # 1. Loading the relational and area table
#   Relation <- MyCSV_load(pathBLS,"SubcatchNutsArea.csv" )
#   
#   # 2. Subseting by the study Catchmen
#   ctchNUTSRel <- Relation[Relation$CatchName ==ctchName,  ]
#   str(ctchNUTSRel)
# 
#   toBeRemoved<-which(ctchNUTSRel$Ctry=="NULL")  #seran los hYdroID de Andorra o similares
#   ctchNUTSRel <- ctchNUTSRel[-toBeRemoved,  ]
#   
#   # 3. Removing levels that no data
#   ctchNUTSRel$Nuts2 <- factor(ctchNUTSRel$Nuts2)
#   ctchNUTSRel$Ctry <- factor(ctchNUTSRel$Ctry)
#   ctchNUTSRel$CtryCode <- factor(ctchNUTSRel$CtryCode)
#   str(ctchNUTSRel)
# 
#   unique(ctchNUTSRel$Nuts2)
#   ## ATENCION CON LOS NULL, preguntar a ALBERTO QUE ES ESTO
#   
#   return(ctchNUTSRel=ctchNUTSRel )
# }