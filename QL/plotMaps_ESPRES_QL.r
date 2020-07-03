

# ## los limites de los paises en undata frame
# DF_lim_map <- data.frame(contry="PL",xlim1=4600000, xlim2=5300000,ylim1=2900000, ylim2=3550000)
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="HU",xlim1=4790000, xlim2=5280000,ylim1=2550000, ylim2=3000000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="FR",xlim1=3250000, xlim2=4300000,ylim1=2000000, ylim2=3100000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="ES",xlim1=2600000, xlim2=3800000,ylim1=1550000, ylim2=2450000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="PT",xlim1=2600000, xlim2=3000000,ylim1=1700000, ylim2=2300000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="UK",xlim1=3150000, xlim2=3800000,ylim1=3100000, ylim2=4200000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="IT",xlim1=4000000, xlim2=5100000,ylim1=1500000, ylim2=2700000))  
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="DE",xlim1=4050000, xlim2=4700000,ylim1=2650000, ylim2=3550000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="SI",xlim1=4500000, xlim2=4900000,ylim1=2450000, ylim2=2700000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="BE",xlim1=3750000, xlim2=4100000,ylim1=2900000, ylim2=3200000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="RO",xlim1=5100000, xlim2=5900000,ylim1=2350000, ylim2=2950000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="NL",xlim1=3800000, xlim2=4200000,ylim1=3050000, ylim2=3400000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="AT",xlim1=4000000, xlim2=5000000,ylim1=2500000, ylim2=3000000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="BG",xlim1=5000000, xlim2=6000000,ylim1=2000000, ylim2=2500000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="CZ",xlim1=4300000, xlim2=5000000,ylim1=2700000, ylim2=3200000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="DK",xlim1=4200000, xlim2=4600000,ylim1=3400000, ylim2=4500000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="EE",xlim1=5000000, xlim2=5500000,ylim1=3900000, ylim2=4300000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="FI",xlim1=4500000, xlim2=5500000,ylim1=4000000, ylim2=6000000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="IE",xlim1=3000000, xlim2=3500000,ylim1=3200000, ylim2=3900000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="LT",xlim1=5000000, xlim2=5500000,ylim1=3400000, ylim2=3900000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="LU",xlim1=2652500, xlim2=7942500,ylim1=1457500, ylim2=4787500))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="LV",xlim1=2652500, xlim2=7942500,ylim1=1457500, ylim2=4787500))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="HL",xlim1=2652500, xlim2=7942500,ylim1=1457500, ylim2=4787500))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="SE",xlim1=4000000, xlim2=5000000,ylim1=3400000, ylim2=6000000))
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="SK",xlim1=2652500, xlim2=7942500,ylim1=1457500, ylim2=4787500))  
# 
# DF_lim_map <- rbind(DF_lim_map, data.frame(contry="ALL",xlim1=2652500, xlim2=7942500,ylim1=1457500, ylim2=4787500))
# 




###############################################
## 1. Loading Info to create the Maps
###############################################
# Catchname <- BasinName
# Nustnames <- BLScen$TheNuts
# listShapes <-listTotalShapes
# TheHID <- BLScen$DFConcentration$HydroID
Subsetingshapes <- function(Catchname,Nustnames,TheHID,listShapes,verbose=TRUE){

  # [[1]]: NUTS2 poligons
  # [[2]]: NUTS2 lines
  # [[3]]: Basin pligons      
  # [[4]]: Basin lines  
  # [[5]]: Catchment poligons
  # [[6]]: Catchment lines
  # [[7]]: Informacion  limites para cada pais (para el box)
  
  #########################
  ### LAS REGIONES (NUS2)  
  
    #  POLIGONOS N2  (solo las regiones que intersectan la cuenca
    MyPoligonsRsub1 <- listShapes[[1]] [listShapes[[1]]$NUTS_ID %in% Nustnames ,]   #el primer elemento de la lista contiene todas las regiones
    plot(MyPoligonsRsub1)
    summary(MyPoligonsRsub1)
    MyPoligonsRsub1@bbox
    MyPoligonsRsub1@data
  
    # LINEAS DE CONTORNO N2
    MyLinesRsub1 <- listShapes[[2]] [listShapes[[2]]$NUTS_ID %in% Nustnames ,] 
    plot(MyLinesRsub1)

  
  ### LAS CUENCAS (BASIN)
    #como viene con la primera en mayuscula en la shape, tengo que transformar a eso
    Catchname <- paste0( toupper(substring(Catchname, 1, 1)), tolower(substring(Catchname,2) )   )
    
    #  POLIGONOS BASIN  
    MyPoligonsBsub1 <- listShapes[[3]][listShapes[[3]]$Name %in% Catchname ,] 
    plot(MyPoligonsBsub1)
    
    # LINEAS DE CONTORNO BASIN
    MyLinesBsub1 <- listShapes[[4]][listShapes[[4]]$Name %in% Catchname ,] 
    plot(MyLinesBsub1)
  
   
  #### LAS SUBCUENCAS . ATENCION: las subcuencas aqui son mucho mas pequenas que en la version cantidad
    
    # POLIGONOS SUBCUENCAS
    MyPoligonsSCsub1 <- listShapes[[5]][listShapes[[5]]$HydroID %in% BLScen$DFConcentration$HydroID ,] 
    plot(MyPoligonsSCsub1)
    # summary(MyoligonsSCsub1)
    # MyPoligonsSCsub1@bbox
    # MyPoligonsSCsub1@data
    # str(MyPoligonsSCsub1@data)
    
    # LINEAS DE CONTORNO SUBCUENCAS
    MyLineSCsub1 <- listShapes[[6]][listShapes[[6]]$HydroID %in% BLScen$DFConcentration$HydroID ,] 
    plot(MyLineSCsub1)
    

   lista <- list( MyPoligonsRsub1,MyLinesRsub1,MyPoligonsBsub1,MyLinesBsub1,MyPoligonsSCsub1,MyLineSCsub1)
  return(lista)
}


  
### NOTA : Copia de la funcion de QT
###############################################
## Dibuja SubCatchments (recibe ya el subseting de los poligonos de las subcuencas del basin)
###############################################
# nombre<-BasinName
# Shapes <- LstMyShapes
Map_SubCatch_new <- function(nombre=FALSE,Shapes,verbose=TRUE){
  #  Plot_SubCatch(CATCHPOL,listofCH_ID,nameCat)
  if (verbose) message("=======================================================")
  if (verbose) message("[  Ploting SubCatchment Map                  ]")
  if (verbose) message("=======================================================")
  print(nombre)
  
  SubcatchPOL <- Shapes[[5]]    # poligonos de las subcuencas
  
  # class(SubcatchPOL)
  #  names(SubcatchPOL)
  #  dim(SubcatchPOL)
  # summary(SubcatchPOL)
  # SubcatchPOL@data
  
  # limites del mapa
  xlim<-SubcatchPOL@bbox[1,]
  ylim<-SubcatchPOL@bbox[2,]
  nombrecatch<- " "    #si  estoy dibujando varios subcatchments
  if( nombre!=FALSE ) nombrecatch <- nombre      #si es una cuenca con nombre
  plot(xlim, ylim, type='n', xlab='', ylab='',cex.axis=0.7, main= nombrecatch)
  plot(SubcatchPOL, add=TRUE, col='aliceblue')
}


###############################################
## Dibuja N2 & SubCatchments (recibe ya el subseting de los poligonos de N2 & de las subcuencas del basin)
###############################################
# nombre <-BasinName
# Shapes <- LstMyShapes
Map_N2_SubCatch_Tab1 <- function(nombre=FALSE,Shapes,etiquetas=FALSE,verbose=TRUE){
  
  N2POL <- Shapes[[1]]        # poligonos de los Nuts 2
  N2Lines <- Shapes[[2]]      # lineas de los Nuts 2
  BasinPOL <- Shapes[[3]]    #  lineas de la cuenca
  BasinLines <- Shapes[[4]]    #  lineas de la cuenca
  SubcatchPOL <- Shapes[[5]]    # poligonos de las subcuencas
  SubcatchLines <- Shapes[[6]]   # lineas de las subcuencas
  
  caja <- bbox(N2POL) 
  plot(caja[1,], caja[2,], type='n', xlab=expression(paste('Lon (', degree, ')'), sep='')
       , ylab=expression(paste('Lat (', degree, ')'), sep='') )
  plot(N2POL, col='aliceblue', add=T)
  plot(SubcatchLines,add=T,col="red",lwd=1)
  plot(N2Lines, col='black',lwd=2, add=T)     # contorno de las provincias
  plot(BasinLines, col='red',lwd=3, add=T)    # contorno de la cuenca
  title(main=paste0(nombre),  sub=" N2 & subcatchments")
  # si queremos que anada el Id de cada region
  if (etiquetas==TRUE) text(coordinates(N2POL), labels=N2POL@data$NUTS_ID, cex=1.2)   #etiqueta con nombre de la region
}


###############################################
## Dibuja N2 & Basin (recibe ya el subseting de todos los poligonos)
###############################################
# nombre <-BasinName
# Shapes <- LstMyShapes
Map_N2_SubCatch_Tab2 <- function(nombre=FALSE,Shapes,etiquetas=FALSE,verbose=TRUE){  
  
  N2POL <- Shapes[[1]]        # poligonos de los Nuts 2
  N2Lines <- Shapes[[2]]      # lineas de los Nuts 2
  BasinPOL <- Shapes[[3]]    #  lineas de la cuenca
  BasinLines <- Shapes[[4]]    #  lineas de la cuenca
  SubcatchPOL <- Shapes[[5]]    # poligonos de las subcuencas
  SubcatchLines <- Shapes[[6]]   # lineas de las subcuencas
  
  class(N2POL)
  class(BasinPOL)
  inter  = gIntersection( N2POL, BasinPOL, byid=T, drop_not_poly=T)
  
  inter2 = gIntersection( N2POL, BasinPOL, byid=T, drop_not_poly=T)
  
  caja<-bbox(inter)    #DIFERENCIA respecto a la otra version
  plot(caja[1,], caja[2,], type='n', xlab=expression(paste('Lon (', degree, ')'), sep='')
       , ylab=expression(paste('Lat (', degree, ')'), sep='') )
  plot(inter, col="aliceblue", add=T,lwd=3)
  
  # la siguiente linea podria ser una opcion (quizas esto ya se ve bien en la otra figura)
  plot(SubcatchLines,add=T,col="red",lwd=0.1)  
  
  plot(inter2,  add=T,lwd=3)   #le anado otra vez los vordes de las NUTS
  
  title(main=paste0(nombre),  sub=" N2 & Basin")
  # si queremos que anada el Id de cada region
  if (etiquetas==TRUE)  text(coordinates(N2POL), labels=N2POL@data$NUTS_ID, cex=1.2)   #etiqueta con nombre de la region
  
#  class(inter)
#  inter@data$objectid   #atencion" inter es de la clase" :SpatialPolygons  no de la clase: SpatialPolygonsDataFrame 
  
}



# Mapeo el valor de una variable para cada subcuenca, con SPPLOT
# catchNa <-BasinName
# LstMySh <-LstMyShapes
# TheScen <- Evaluated_BLScen[[1]]      BLS_WEI
# ListVar  <- ListIndexUses
# nclr <- NumClassInterv
# pal <- paleta
# style <- estilo 
# scal <-  scale   
# scaleRel <- scaleRelation
# VME <-valorMaxEscala
# FALTA MEJORAR LA FUNCION PARA QUE SIRVA PARA DIBUJAR CUALQUIER VARIABLE (PASANDO EL INDICE)
Map_var_SubCatch_SPPLOT_QL <- function (catchNa,LstMySh,TheScen,ListVar,nclr,pal,style,scal,scaleRel,VME,verbose=TRUE){     
  
  #indice de la variable a mapear
  index <- ListVar   # 3: concentracion en BLS; 13: Concent reduction; 14: New Concentration  
  # Lo primero cargar los shapefiles
  MyPoligonsRsub1_orig <- LstMySh[[5]]  #los poligonos de las subcuencas
  MyPLinesCsub1 <- LstMySh[[2]]   #el borde de la cuenca lo empleo en la primera version
  MyPPoligonsCsub1 <- LstMySh[[3]]  # el poligono de la cuenca lo empleo en la segunda version version
  
  #Anado los valores de los ratios de reduccion de las 4 variables (sectores) a mapear (que se han agregado en cada region) al shape del mapa
  str(MyPoligonsRsub1_orig@data)  #el shape
  
  # Preparar la tabla con todas las variables
  TheTable <- TheScen  #copia todas las columnas
  # todos los valores mayores que un limite que impone el usuario se ponen a ese valor. Para que los mapas sean un poco mas chulos)
  if(!(is.null(VME)) ) 
     { TheTable$newconc[ !is.na(TheTable$newconc) & (TheTable$newconc > VME)] <- VME   #linea para limitar el valor maximo a pintar
       TheTable$Concentration[ !is.na(TheTable$Concentration) & (TheTable$Concentration > VME)] <- VME   
       TheTable$redconc[ !is.na(TheTable$redconc) & (TheTable$redconc > VME)] <- VME   } 
     print(max(TheTable$newconc,na.rm = TRUE ))
  
  #if the user select logarithmic scale, we transfor the original data (les sumo 1, para evitar infinitos o negativos)
  print (scal ==  "logarithmic")
  if (scal ==  "logarithmic")   TheTable[,index] <- log(TheTable[,index]+1)
  
  #lo mezclo con el shape                                        "hydroid"       "Row.names"
  MyDFRsub1 <- merge(MyPoligonsRsub1_orig@data, TheTable, by.x ="HydroID",by.y ="HydroID") #los esfuerzos by rownames. #junto los datos del espatialdataframe, con mi data frame de concentracion
  MyDFRsub1_lg<-MyDFRsub1
  # MyDFRsub1_lg[,5:8] <- log( MyDFRsub1[,5:8]+0.1 )
  #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
  MyDFRsub1_lg <- MyDFRsub1_lg[order(match(MyDFRsub1_lg[,1],MyPoligonsRsub1_orig@data[,1])),]  #reordena en el mismo orden que en el objeto original
  MyPoligonsRsub1 <- MyPoligonsRsub1_orig #sobre una copia el original
  MyPoligonsRsub1@data <- MyDFRsub1_lg  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 
  

  # En la version QL unicamente se dibuja una variable (concentracion, nueva concentracion o reduccion de concentracion)
  if (index == 13)  UsesNames="N reduction"    else    UsesNames="N Concentration"
  indexplot <- index+9  #las columnas despues de mezcladas para el mapa estan 9 a la derecha
  allvarsvalues <- unlist(MyPoligonsRsub1@data[,(indexplot)]) 
  breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
  #para que el ultimo intervalo no de problemas, le aumento algo al mayor valor 
  breaks.qt$brks[length(breaks.qt$brks)]<-breaks.qt$brks[length(breaks.qt$brks)]+breaks.qt$brks[length(breaks.qt$brks)]/100
  
  plotclr.palette <- brewer.pal(n =nclr, name = pal)
  
  spBasin <- list("sp.polygons", MyPPoligonsCsub1,first=FALSE,col="black")
  
  print(  spplot(MyPoligonsRsub1[,indexplot],  main = paste0(UsesNames, " (", scal, ")"  ), 
                  sub = paste0(style," breaks"),
                  col = "transparent",
                  colorkey=list(space="bottom"),
                  col.regions=plotclr.palette,
                  at = breaks.qt$brks,
                  sp.layout = list(spBasin)  )   )
  

#  boxplot(allvarsvalues)
#  boxplot(allvarsvalues,outline = F    )
  
#  boxplot(log(allvarsvalues+1))
#  min(allvarsvalues)

}




# Mapeo el valor de una variable para cada subcuenca, con SPPLOT
# catchNa <-BasinName
# LstMySh <-LstMyShapes
# TheScen <- Evaluated_BLScen[[1]]      BLS_WEI   names(TheScen[[1]])
# ListVar  <- ListIndexUses
# nclr <- NumClassInterv
# pal <- paleta
# style <- estilo 
# scal <-  scale   
# scaleRel <- scaleRelation
# VME <-valorMaxEscala
Map_var_SubCatch_SPPLOT_LOAD <- function (catchNa,LstMySh,TheScen,ListVar,nclr,pal,style,scal,scaleRel,VME,verbose=TRUE){     
  #indice de la variable a mapear
  index <- ListVar   # 3: concentracion en BLS; 13: Concent reduction; 14: New Concentration  
  # Lo primero cargar los shapefiles
  MyPoligonsRsub1_orig <- LstMySh[[5]]  #los poligonos de las subcuencas
  MyPLinesCsub1 <- LstMySh[[2]]   #el borde de la cuenca lo empleo en la primera version
  MyPPoligonsCsub1 <- LstMySh[[3]]  # el poligono de la cuenca lo empleo en la segunda version version
  
  #Anado los valores de los ratios de reduccion de las 4 variables (sectores) a mapear (que se han agregado en cada region) al shape del mapa
  str(MyPoligonsRsub1_orig@data)  #el shape
  
  # Preparar la tabla con todas las variables
  TheTable <- TheScen  #copia todas las columnas
  names(TheTable)

  #if the user select logarithmic scale, we transfor the original data (les sumo 1, para evitar infinitos o negativos)
  print (scal ==  "logarithmic")
  if (scal ==  "logarithmic")   TheTable[,index] <- log(TheTable[,index]+1)
  if (scal ==  "logarithmic")   TheTable[,15:18] <- log(TheTable[,15:18]+1)
  
  # todos los valores mayores que un limite que impone el usuario se ponen a ese valor. Para que los mapas sean un poco mas chulos)
  if(!(is.null(VME)) ) 
  {    #lineas para limitar el valor maximo a pintar
    TheTable$Concentration[ !is.na(TheTable$Concentration) & (TheTable$Concentration > VME)] <- VME   
    TheTable$redconc[ !is.na(TheTable$redconc) & (TheTable$redconc > VME)] <- VME    
    TheTable$Man[ !is.na(TheTable$Man) & (TheTable$Man > VME)] <- VME  
    TheTable$Min[ !is.na(TheTable$Min) & (TheTable$Min > VME)] <- VME
    TheTable$PS[ !is.na(TheTable$PS) & (TheTable$PS > VME)] <- VME
    TheTable$SD[ !is.na(TheTable$SD) & (TheTable$SD > VME)] <- VME    } 
  print(max(TheTable$Man,na.rm = TRUE ))

  
  #lo mezclo con el shape                                        "hydroid"       "Row.names"
  MyDFRsub1 <- merge(MyPoligonsRsub1_orig@data, TheTable, by.x ="HydroID",by.y ="HydroID") #los esfuerzos by rownames. #junto los datos del espatialdataframe, con mi data frame de concentracion
  MyDFRsub1_lg <- MyDFRsub1
  # MyDFRsub1_lg[,5:8] <- log( MyDFRsub1[,5:8]+0.1 )
  #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
  MyDFRsub1_lg <- MyDFRsub1_lg[order(match(MyDFRsub1_lg[,1],MyPoligonsRsub1_orig@data[,1])),]  #reordena en el mismo orden que en el objeto original
  MyPoligonsRsub1 <- MyPoligonsRsub1_orig #sobre una copia el original
  MyPoligonsRsub1@data <- MyDFRsub1_lg  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 
  
  names(MyPoligonsRsub1)
  # En la version QL unicamente se dibuja una variable (concentracion, nueva concentracion o reduccion de concentracion)
  if (index == 13)  UsesNames="N reduction"    else    UsesNames="N Concentration"
  indexplot <- index+9  #las columnas despues de mezcladas para el mapa estan 9 a la derecha
 # indexplot <-27
   allvarsvalues <- unlist(MyPoligonsRsub1@data[,(indexplot)]) 
  breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
  #para que el ultimo intervalo no de problemas, le aumento algo al mayor valor 
  breaks.qt$brks[length(breaks.qt$brks)]<-breaks.qt$brks[length(breaks.qt$brks)]+breaks.qt$brks[length(breaks.qt$brks)]/100
  
  plotclr.palette <- brewer.pal(n =nclr, name = pal)
  
  spBasin <- list("sp.polygons", MyPPoligonsCsub1,first=FALSE,col="black")
  
  print(  spplot(MyPoligonsRsub1[,indexplot],  main = paste0(UsesNames, " (", scal, ")"  ), 
                 sub = paste0(style," breaks"),
                 col = "transparent",
                 colorkey=list(space="bottom"),
                 col.regions=plotclr.palette,
                 at = breaks.qt$brks,
                 sp.layout = list(spBasin)  )   )
  
  
  
  
  # TODOS LOS MAPAS EN LA MISMA ESCALA
  if (scaleRel == "Common") {
    # ListVar           names(MyPoligonsRsub1@data)
    index <- ListVar+9
    allvarsvalues <- unlist(MyPoligonsRsub1@data[,index])   #selecciono solo una columna a la vez
    breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
    #para que el ultimo intervalo no de problemas, le aumento algo al mayor valor 
    breaks.qt$brks[length(breaks.qt$brks)]<-breaks.qt$brks[length(breaks.qt$brks)]+breaks.qt$brks[length(breaks.qt$brks)]/100
    
    plotclr.palette <- brewer.pal(n =nclr, name = pal)
    
    print(   spplot(MyPoligonsRsub1[,index], main = paste0("N Load ", " (", scal, ")"  ), 
                    sub = paste0(style," breaks"),
                    col = "transparent",
                    colorkey=list(space="bottom"),
                    col.regions=plotclr.palette, layout = c(2, 2),
                    at = breaks.qt$brks )   )
  }  #if scale relation is common
  
  
  #  boxplot(allvarsvalues)
  #  boxplot(allvarsvalues,outline = F    )
  
  #  boxplot(log(allvarsvalues+1))
  #  min(allvarsvalues)
  
}





#######################################################################
#### DOS ALTERNATIVAS DE MAPAS PARA DIBUJAR CONCENTRACION POR NUTS2 ###
#### UNA CON LA LIBRERIA BASE Y OTRA CON SPPLOT    ####################
#######################################################################


# FUNCION QUE PREPROCESA LOS DATOS A MAPEAR CON SPPLOT para ver valores por N2
# TheScen <- newConcbyNUTW    listWEI    t(stg_n1_strateg)
# scal <-  scale
# agrm <-  agregation
# type <- "EF"      # "EF":esfuerzo  "CONC": concentracion de nitratos
Prepro_to_Map_N2_QL <- function(TheScen,scal,agrm,type,verbose=TRUE){

  if(type == "EF"){
    TheTable <- t(TheScen)  #atencion que los Nuts pasan a ser los nombre de las filas
    
  }
  
  if(type == "EFT"){
      temp <- TheScen[,c(1,3,4,5,6)]
      rownames(temp) <- temp$Nuts2
      temp$Nuts2 <- NULL
      colnames(temp) <- c("Man","Min","PS","SD")
      TheTable <- temp
  }
  
  if(type == "CONC"){
    TheTable <- TheScen[,c(1,2)] #creo una tabla en la que meto las dos primeras columnas comunes
    thegrm <- c("Mean","Median","Q75","Max")
    index <- 2+match(agrm,thegrm)  #el indice de la columna con  la metrica seleccionada
    # solo se anade una columna correspondiente a la metrica que se desea emplear para ver la concentracion
    TheTable <- cbind (TheTable , TheScen[,index] )
    names(TheTable)[3] <- agrm
  }
  

  #if the user select logarithmic scale, we transfor the original data
  #print (scal ==  "logarithmic")
  if (scal ==  "logarithmic") TheTable[ ,(2:length(TheTable))] <- log(TheTable[ ,(2:length(TheTable))]+1)
  
  # El formato de la tabla devuelta debe ser: 
  #  en cada fila valores por N2
  #  en la primera columna el nombre del N2, segunda couts, tercera : la variable 

  
  return (TheTable)
}


# Mapeo el valor de una variable para cada NUTS2, con SPPLOT
# catchNa <- BasinName
# LstMySh <- LstMyShapes
# TheScen <- TheVariables   newExtraccNUT    listWEI      TheVariables
# nclr <- NumClassInterv
# pal <- paleta
# style <- estilo 
# scal <- scale
# mtype <-  "CONC"      # "NCONC" : concentracion   "EF esfuerzos
Map_var_NUTS_SPPLOT_QL <- function(catchNa,LstMySh,TheScen,nclr,pal,style,scal,mtype,verbose=TRUE){
  
  # Carga los shapefiles
  MyPoligonsRsub1_orig <- LstMySh[[1]]  #los poligonos de las NUTS2
  MyPLinesCsub1 <- LstMySh[[2]]   #el borde de la cuenca lo empleo en la primera version
  MyPPoligonsCsub1 <- LstMySh[[4]]  # el poligono de la cuenca lo empleo en la segunda version version
  
  #Anado los valores de los ratios de reduccion de las 4 variables (sectores) a mapear (que se han agregado en cada region) al shape del mapa
  str(MyPoligonsRsub1_orig@data)  #el shape

  # si quiero mapear los load (hago el logaritmo para tener una escala mas razonable)
  
  if (mtype == "CONC") {strCol="Nuts2"}
  if (mtype == "EF") {strCol="row.names"}
  #                                                              "objectid"        "N2"
  MyDFRsub1 <- merge(MyPoligonsRsub1_orig@data, TheScen, by.x ="NUTS_ID",by.y = strCol) #los esfuerzos by rownames. #junto los datos del espatialdataframe, con mi data frame de concentracion
  MyDFRsub1_lg<-MyDFRsub1
  # MyDFRsub1_lg[,5:8] <- log( MyDFRsub1[,5:8]+0.1 )
  #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
  MyDFRsub1_lg <- MyDFRsub1_lg[order(match(MyDFRsub1_lg[,1],MyPoligonsRsub1_orig@data[,1])),]  #reordena en el mismo orden que en el objeto original
  MyPoligonsRsub1 <- MyPoligonsRsub1_orig #sobre una copia el original
  MyPoligonsRsub1@data <- MyDFRsub1_lg  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 
  
  
  if (mtype == "CONC") {    
      i <- 6 
      agre <- colnames(MyPoligonsRsub1@data)[i]
      allvarsvalues <- unlist(MyPoligonsRsub1@data[,i:i])   #selecciono solo una columna a la vez
      if(length(allvarsvalues)==1) allvarsvalues <- c(allvarsvalues/1.01,allvarsvalues)  #si solo hay un Nuts, solo habra una categoria, creo una artificial para que no pete el mapa
      breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
      #para que el ultimo intervalo no de problemas, le aumento algo al mayor valor 
      breaks.qt$brks[length(breaks.qt$brks)]<-breaks.qt$brks[length(breaks.qt$brks)]+breaks.qt$brks[length(breaks.qt$brks)]/100
      
      plotclr.palette <- brewer.pal(n =nclr, name = pal)
      
      #para poder anadir un segundo mapa, con el que resaltar el contorno de la cuenca, lo anado como lista
      spBasin <- list("sp.polygons", MyPPoligonsCsub1,first=FALSE,col="black")
      # y otra lista para anadir los indices de los nuts (luego anado las listas como list en sp.layout)
      tps <- list("sp.text", coordinates(MyPoligonsRsub1), MyPoligonsRsub1@data$NUTS_ID, cex=0.7, col="black")
      
      print(    spplot(MyPoligonsRsub1[,i],  main = paste0(  agre ," N concentration", " (", scal, ")"  ), 
                         sub = paste0(style," breaks"),
                         col = "transparent",
                         colorkey=list(space="bottom"),
                         col.regions=plotclr.palette,
                         at = breaks.qt$brks,
                         sp.layout = list(spBasin,tps)  )    )
  }
  

  if (mtype == "EF") {
    ListVar <- c(5,6,7,8)
    agre <- colnames(MyPoligonsRsub1@data)[ListVar]
    allvarsvalues <- unlist(MyPoligonsRsub1@data[,ListVar])
    if(length(allvarsvalues)==1) allvarsvalues <- c(allvarsvalues/1.01,allvarsvalues)  #si solo hay un Nuts, solo habra una categoria, creo una artificial para que no pete el mapa
    breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
    #para que el ultimo intervalo no de problemas, le aumento algo al mayor valor 
    breaks.qt$brks[length(breaks.qt$brks)]<-breaks.qt$brks[length(breaks.qt$brks)]+breaks.qt$brks[length(breaks.qt$brks)]/100
    plotclr.palette <- brewer.pal(n =nclr, name = pal)
    
    #para poder anadir un segundo mapa, con el que resaltar el contorno de la cuenca, lo anado como lista
    spBasin <- list("sp.polygons", MyPPoligonsCsub1,first=FALSE,col="black")
     
    print(    spplot(MyPoligonsRsub1[,ListVar],  main = paste0(  " Effort", " (", scal, ")"  ), 
                     sub = paste0(style," breaks"),
                     col = "transparent",
                     colorkey=list(space="bottom"),
                     col.regions=plotclr.palette,    layout = c(4, 1),
                     at = breaks.qt$brks,
                     sp.layout = list(spBasin)  )    )
  }
  
  
}



# catchNa <- BasinName    
# LstMySh <- LstMyShapes      # LstShapes[[1]]
# TheScen <- TheVariables     #data frame con todos los tipos de agregacion
# IndAgr <-AgrIndex
# nclr <- NumClassInterv
# pal <- paleta
# style <- estilo       
Map_var_NUTS_BASE_QL <- function(catchNa,LstMySh,TheScen,IndAgr,nclr,pal,style,verbose=TRUE){
  
    MyPoligonsRsub1_orig <- LstMySh[[1]]
    MyPLinesCsub1 <- LstMySh[[2]]   #el borde de la cuenca lo empleo en la primera version
    MyPPoligonsCsub1 <- LstMySh[[3]]  # el poligono de la cuenca lo empleo en la segunda version version
    
    #Anado el valor de la metrica con la que se han agregado las concentraciones en cada region
    str(MyPoligonsRsub1_orig@data)
    #cbind.data.frame(coordinates(MyPoligonsRsub1_orig),(MyPoligonsRsub1_orig@data$NUTS_ID))
    #head(MyPoligonsRsub1_orig@data)
    #class(MyPoligonsRsub1_orig)
    MyDFRsub1 <- merge(MyPoligonsRsub1_orig@data, TheScen, by.x ="NUTS_ID",by.y ="Nuts2") #junto los datos del espatialdataframe, con mi data frame de concentracion
    #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
    MyDFRsub1<- MyDFRsub1[order(match(MyDFRsub1[,1],MyPoligonsRsub1_orig@data[,1])),]  #reordena en el mismo orden que en el objeto original
    
    MyPoligonsRsub1 <- MyPoligonsRsub1_orig #sobre una copia el original
    MyPoligonsRsub1@data <- MyDFRsub1  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 
    # cbind.data.frame(coordinates(MyPoligonsRsub1),(MyPoligonsRsub1@data$NUTS_ID))
    

    class(MyPoligonsRsub1)
    plotvar <- MyPoligonsRsub1@data[,6]  #la variable a plotear ocupa la posicion quinta
    nameIAgr<-names(MyPoligonsRsub1@data)[6]  #el nombre de la variable
    
    
    #preparando la paleta de colores  #TENGO QUE REDUCIR LOS DECIMALES Y PONER FUERA LA ESCALA, MIRAR EJEMPLOS MAS ABAJO
    #   nclr <- 8
    if(IndAgr==1) {precis=0}
    if(IndAgr>1) {precis=1}
    plotclr <- brewer.pal(nclr,pal)
    class <- classIntervals(plotvar, n=nclr, style= style ,dataPrecision=precis)  #Data precision, o 0 o 1. Dependiendo de la tecnica de agregacion
    colcode <- findColours(class, plotclr)
    
    
    ############  
    #### VERSION 2 que pinta solo la interseccion de los N2 con la cuenca
    
    #Por interseccion entre los shape files, me quedo solo con le hay dentro de la cuenca  (emplea la siguiente funcion de la libreria rgeos)
    inter = gIntersection(MyPoligonsRsub1, MyPPoligonsCsub1, byid=T, drop_not_poly=T)
    
    caja<-bbox(inter)
    plot(caja[1,], caja[2,], type='n', xlab=expression(paste('Lon (', degree, ')'), sep='')
         , ylab=expression(paste('Lat (', degree, ')'), sep='') )
    
    plot(inter, col=colcode, add=T)
    
    title(main=paste0(nameIAgr, " Concentrationcion"), 
          sub="Quantile (Equal-Frequency) Class Intervals")
    
    text(coordinates(MyPoligonsRsub1), labels=MyPoligonsRsub1@data$NUTS_ID, cex=0.8,col="green") 
    legend("bottomleft", legend=names(attr(colcode, "table")), 
           fill=attr(colcode, "palette"), cex=0.8, bty="n", title="Concentracion", text.font=4, bg='lightblue')
    
    
    
        # 
        # 
        # ############  
        # #### VERSION 1
        # caja<-bbox(MyPoligonsRsub1)       #preparando la caja y el plot
        # plot(caja[1,], caja[2,], type='n', xlab=expression(paste('Lon (', degree, ')'), sep='')
        #      , ylab=expression(paste('Lat (', degree, ')'), sep='') )
        # plot(MyPoligonsRsub1, col=colcode, add=T)
        # plot(MyPLinesCsub1,add=T,col="white",lwd=3)
        # title(main=paste0(nameIAgr, " Concentrationcion"), 
        #       sub="Quantile (Equal-Frequency) Class Intervals")
        # 
        # text(coordinates(MyPoligonsRsub1), labels=MyPoligonsRsub1@data$NUTS_ID, cex=0.7) 
        # legend("bottomleft", legend=names(attr(colcode, "table")), 
        #        fill=attr(colcode, "palette"), cex=0.8, bty="n", title="Concentracion", text.font=4, bg='lightblue')
        # 
        # cbind.data.frame(coordinates(MyPoligonsRsub1),(MyPoligonsRsub1@data$NUTS_ID))
        # 
        # 
        # coordinates(MyPoligonsRsub1)
        # 
        # ############  
        # #### VERSION 2
        # 
        # #Por interseccion entre los shape files, me quedo solo con le hay dentro de la cuenca  (emplea la siguiente funcion de la libreria rgeos)
        # inter = gIntersection(MyPoligonsRsub1, MyPPoligonsCsub1, byid=T, drop_not_poly=T)
        # 
        # caja<-bbox(inter)
        # plot(caja[1,], caja[2,], type='n', xlab=expression(paste('Lon (', degree, ')'), sep='')
        #      , ylab=expression(paste('Lat (', degree, ')'), sep='') )
        # 
        # plot(inter, col=colcode, add=T)
        # 
        # title(main=paste0(nameIAgr, " Concentrationcion"), 
        #       sub="Quantile (Equal-Frequency) Class Intervals")
        # 
        # text(coordinates(MyPoligonsRsub1), labels=MyPoligonsRsub1@data$NUTS_ID, cex=0.8,col="green") 
        # legend("bottomleft", legend=names(attr(colcode, "table")), 
        #        fill=attr(colcode, "palette"), cex=0.8, bty="n", title="Concentracion", text.font=4, bg='lightblue')
        # 
        # 
        # 
        # ############  
        # #### VERSION 3  (lo mismo que la anterior pero poniendo la leyenda fuera)
        # 
        # #Por interseccion entre los shape files, me quedo solo con le hay dentro de la cuenca  (emplea la siguiente funcion de la libreria rgeos)
        # inter = gIntersection(MyPoligonsRsub1, MyPPoligonsCsub1, byid=T, drop_not_poly=T)
        # 
        # #Divido en parte para el mapa y parte para la leyenda (al fondo)
        # layout(matrix(c(1,2),  byrow = TRUE),heights=c(14,2,1,1) )
        # #El MAPA
        # par(mar=c(2.2,2.5,2,0.2),mgp=c(1.5,0.5,0))   #mgp: 1 indice: distancia xlab; 2 indice: distancia numero ; 3index: dist eje
        # caja<-bbox(MyPLinesCsub1)
        # plot(caja[1,], caja[2,], type='n', xlab=expression(paste('Lon (', degree, ')'), sep='')
        #      , ylab=expression(paste('Lat (', degree, ')'), sep='') ,cex.axis=0.7,cex.lab=0.7 )
        # expression(paste('Lon (', degree, ')'), sep='')
        # 
        # plot(inter, col=colcode, add=T)
        # 
        # title(main=paste0(nameIAgr, " Concentration")  )
        # # ,sub="Quantile (Equal-Frequency) Class Intervals")
        # text(coordinates(MyPoligonsRsub1), labels=MyPoligonsRsub1@data$NUTS_ID, cex=1.0,col="green")   #los ID de las regiones
        # 
        # 
        # #LA LEYENDA
        # par(mar=c(0,0,0,0))   #changing the margins
        # 
        # # plot(c(0,1),c(0,4),type = 'n', axes=F ,xlab = "", ylab = "", main = "") 
        # plot.new()
        # 
        # legend("center", legend=names(attr(colcode, "table")), 
        #        fill=attr(colcode, "palette"), cex=0.9, bty="n", title= " " , text.font=3,
        #        bg='lightblue',ncol=4)
        # 
        # text_title <- paste( style, " class intervals"," " , "concentracion" )    
        # legend("center",legend= "",  title= text_title , cex=1.1, bty='n', title.adj=0.17)  #segunda legend para titulo mas grande
        # 
  
}





  



# catchNa <- catchName
# LstMySh <- LstMyShapes
# BLS <- BLScen
# StgToCompare <- Estrategia1
# IndAgr <- AgrIndex
# nclr <- NumClassInterv
# pal <- paleta
# style <- estilo
Map_RegConcComp <- function(catchNa,LstMySh,BLS,StgToCompare,IndAgr,nclr,pal,style,verbose=TRUE){
  
  # 1. Generamos una BLS estrategia (todas las reducciones a cero) para evaluar su concentracion
  dimx <- ncol(StgToCompare)  #el numero de columnas de la matriz
  BLS_stg <- matrix( c( rep(0,dimx),     # Nitrates mineral agricola
                        rep(0,dimx),       # Nitrates manure agricola
                        rep(0,dimx),       # Nitrates scatter dweling 
                        rep(0,dimx)),      # Nitrates point sources (urban & industrial) 
                     byrow = TRUE, ncol = dimx)
  rownames(BLS_stg) <- c("Man","Min","PS","SD")   #cada fila es un tipo de presion
  colnames(BLS_stg) <- colnames(StgToCompare)
  
  # 2. Evaluo ambas estrategias
  BLS_Evaluated <- EvalStg (BLS,BLS_stg) 
  Estrategia1_Evaluated <- EvalStg (BLS,Estrategia1) 
  
  # 3. Concentracion agregada de cada una de las estrategias (una es siempre la BLS)
  weightBYlength <-"F"     #F: false (sin ponderar); A: by area ; DA: by Drained Area
  BLSConcbyNUTW <- NutsConcFromSubc(BLS_Evaluated[[1]] ,weightBYlength)   #Ponderando por longitud
  BLSConcbyNUTW
  BLSConcbyNUTW[,3:6] <- lapply(BLSConcbyNUTW[,3:6] , round, 1)  #reduzco numero de decimales para imprimir
  
  Estrategia1ConcbyNUTW <- NutsConcFromSubc(Estrategia1_Evaluated[[1]] ,weightBYlength)   #Ponderando por longitud
  Estrategia1ConcbyNUTW
  Estrategia1ConcbyNUTW[,3:6] <- lapply(Estrategia1ConcbyNUTW[,3:6] , round, 1)  #reduzco numero de decimales para imprimir
  
  Diff_ConcbyNUT <- BLSConcbyNUTW #hago una copia del BLS (para las columnas que no cambia)
  Diff_ConcbyNUT[,3:6] <- BLSConcbyNUTW[,3:6] - Estrategia1ConcbyNUTW[,3:6]  #todo signo positivo, pero son reducciones, mayor valor mas reduccion
  
  # 4. Ahora llamo a la funcion que hace el MAPA que compara las concentraciones de ambas estrategias
  Map_RegConc(catchNa,LstMySh,Diff_ConcbyNUT,IndAgr,nclr,pal,style)
  
}


# catchNa <- catchName
# LstMySh <- LstMyShapes
# BLS <- BLScen
# StgtoPlot <- Estrategia1
# nclr  <- NumClassInterv
# pal <- paleta
# style <- estilo
# rates <- "T"  #Para mostrar los rates en porcentajes o en valor de esfuerzo (contaminante reduccido)
Map_RegEffortRed <- function (catchNa,LstMySh,BLS,StgtoPlot,nclr,pal,style,rates,verbose=TRUE){     
  
  # Lo primero cargar los shapefiles
  MyPoligonsRsub1_orig <- LstMySh[[1]]
  MyPLinesCsub1 <- LstMySh[[2]]   #el borde de la cuenca lo empleo en la primera version
  MyPPoligonsCsub1 <- LstMySh[[3]]  # el poligono de la cuenca lo empleo en la segunda version version
  
  # Queremos dibujar 4 mapas: Man, Min,  PS, SDW.
  #Anado los valores de los ratios de reduccion de las 4 variables (sectores) a mapear (que se han agregado en cada region) al shape del mapa
  str(MyPoligonsRsub1_orig@data)  #el shape
  
  
  if(rates=="T"){
    #Si queremos hacer Map de los porcentajes
    # Si quiere verse el esfuerzo como rates (no tengo que hacer ninguna evaluacion), dibuja directamente
    StgValues <- t(StgtoPlot)
  }
  
  if(rates=="F"){
    #Para dibujarlo en valores de reduccion de esfuerzo, tenemos que evaluar la estrategia
    myStg_Evaluated <- EvalStg (BLS,StgtoPlot) 
    str(myStg_Evaluated[[2]]) #el segundo elemento de la lista de evaluacion son las reducciones todales de conentracion por cada tipo en cada subcuenca
    head( myStg_Evaluated[[2]][1:4,1:5])  
    
    #tengo que agregarlo por cada NUTS
    Mattemp <- NutsSectEffortFromSubc (myStg_Evaluated)  
    rownames(Mattemp) <- Mattemp$Nuts2
    Mattemp <- Mattemp[,3:6]   ## LORO ATENCION A MODIFICAR LA EVALUACION, POR EL ORDEN DEL TIPO DE REDUCCIONES
    colnames(Mattemp)<- rownames(StgtoPlot)   #############################   ###################
    #################################
    StgValues <- t(Mattemp)
  }
  

  MyDFRsub1 <- merge(MyPoligonsRsub1_orig@data, StgValues, by.x ="NUTS_ID",by.y ="row.names") #los esfuerzos by rownames. #junto los datos del espatialdataframe, con mi data frame de concentracion
  #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
  MyDFRsub1<- MyDFRsub1[order(match(MyDFRsub1[,1],MyPoligonsRsub1_orig@data[,1])),]  #reordena en el mismo orden que en el objeto original
  MyPoligonsRsub1<-MyPoligonsRsub1_orig #sobre una copia el original
  MyPoligonsRsub1@data <- MyDFRsub1  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 
  # cbind.data.frame(coordinates(MyPoligonsRsub1),(MyPoligonsRsub1@data$NUTS_ID))
  
  #  La paleta es la misma, con el mismo numero de clases. Pero en cada mapa las clases tienen distinto intervalo
  plotclr <- brewer.pal(nclr,pal)
  
  # Preparamos el layout para el mapa
  #Divido en parte para el mapa y parte para la leyenda (al fondo)
  layout(matrix(c(1,3,2,4,5,7,6,8), 4, 2, byrow = TRUE),heights=c(4,1,4,1) )
  layout.show(n=8)
  
  #Hacemos un loop para plotar cada una de las variables
  for (i in 1:4){
    # i=2
    class(MyPoligonsRsub1)
    plotvar <- MyPoligonsRsub1@data[,(4+i)]  #la variable a plotear ocupa la posicion quinta
    nameIAgr<-names(MyPoligonsRsub1@data)[(4+i)]  #el nombre de la variable
    precis<-0  #fijo la precision a 0, podria ser a 1
    class <- classIntervals(plotvar, n=nclr, style= style ,dataPrecision=precis)  #Data precision, o 0 o 1. Dependiendo de la tecnica de agregacion
    colcode <- findColours(class, plotclr)
    
    #Por interseccion entre los shape files, me quedo solo con le hay dentro de la cuenca  (emplea la siguiente funcion de la libreria rgeos)
    inter = gIntersection(MyPoligonsRsub1, MyPPoligonsCsub1, byid=T, drop_not_poly=T)
    
    #Margenes para el mapa
    par(mar=c(2.2,2.5,2,0.2),mgp=c(1.5,0.5,0))   #mgp: 1 indice: distancia xlab; 2 indice: distancia numero ; 3index: dist eje
    
    caja<-bbox(MyPLinesCsub1)
    plot(caja[1,], caja[2,], type='n', xlab=expression(paste('Lon (', degree, ')'), sep='')
         , ylab=expression(paste('Lat (', degree, ')'), sep='') ,cex.axis=0.7,cex.lab=0.7 )
    expression(paste('Lon (', degree, ')'), sep='')
    
    plot(inter, col=colcode, add=T)
    
    title(main=paste0(nameIAgr, " rate pressure reductions")  )
    # ,sub="Quantile (Equal-Frequency) Class Intervals")
    text(coordinates(MyPoligonsRsub1), labels=MyPoligonsRsub1@data$NUTS_ID, cex=1.0,col="green")   #los ID de las regiones
    
    #LA LEYENDA
    par(mar=c(0,0,0,0))   #changing the margins
    
    # plot(c(0,1),c(0,4),type = 'n', axes=F ,xlab = "", ylab = "", main = "") 
    plot.new()
    
    legend("center", legend=names(attr(colcode, "table")), 
           fill=attr(colcode, "palette"), cex=0.9, bty="n", title= " " , text.font=3,
           bg='lightblue',ncol=4)
    
    text_title <- paste( style, " class intervals"," " , "load reduction" )    
    legend("center",legend= "",  title= text_title , cex=1.1, bty='n', title.adj=0.17)  #segunda legend para titulo mas grande
    
  }  #fin del for
  
  
}





# catchNa <- catchName
# LstMySh <- LstMyShapes
# BLS <- BLScen
# StgtoPlot <- Estrategia1
# nclr  <- NumClassInterv
# pal <- paleta
# style <- estilo
# rates <- "T" 
Map_RegEffortRed_SPPLOT <- function (catchNa,LstMySh,BLS,StgtoPlot,nclr,pal,style,rates,verbose=TRUE){     
  
  # Lo primero cargar los shapefiles
  MyPoligonsRsub1_orig <- LstMySh[[1]]
  MyPLinesCsub1 <- LstMySh[[2]]   #el borde de la cuenca lo empleo en la primera version
  MyPPoligonsCsub1 <- LstMySh[[3]]  # el poligono de la cuenca lo empleo en la segunda version version
  
  # Queremos dibujar 4 mapas: Man, Min,  PS, SDW.
  #Anado los valores de los ratios de reduccion de las 4 variables (sectores) a mapear (que se han agregado en cada region) al shape del mapa
  str(MyPoligonsRsub1_orig@data)  #el shape
  
  
  if(rates=="T"){
    #Si queremos hacer Map de los porcentajes
    # Si quiere verse el esfuerzo como rates (no tengo que hacer ninguna evaluacion), dibuja directamente
    StgValues <- t(StgtoPlot)
    
    
    MyDFRsub1 <- merge(MyPoligonsRsub1_orig@data, StgValues, by.x ="NUTS_ID",by.y ="row.names") #los esfuerzos by rownames. #junto los datos del espatialdataframe, con mi data frame de concentracion
    #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
    MyDFRsub1 <- MyDFRsub1[order(match(MyDFRsub1[,1],MyPoligonsRsub1_orig@data[,1])),]  #reordena en el mismo orden que en el objeto original
    MyPoligonsRsub1 <- MyPoligonsRsub1_orig #sobre una copia el original
    MyPoligonsRsub1@data <- MyDFRsub1  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 
    # cbind.data.frame(coordinates(MyPoligonsRsub1),(MyPoligonsRsub1@data$NUTS_ID))
    
    plotclr.palette <- brewer.pal(n =nclr, name = pal)
    
    #para dibujar todas la variables al mismo tiempo
    allvarsvalues <- unlist(MyPoligonsRsub1@data[,5:8])
    #intervalos que se adaptem 
    breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
    
    theplot<-spplot(MyPoligonsRsub1, c("PS","SD","Man","Min"),                     # "PS","SDW","Man","Min"
                    colorkey=list(space="bottom"), 
                    col.regions=plotclr.palette, 
                    at = round(breaks.qt$brks,2)  )     #    #atencion, si no pongo el round, alguna region sale a valor cero incorrectaemente?????
    # cuts = nclr-1,  cuts = cuts   , col = "transparent"  #do.log = TRUE     ,pretty=T
    
    
    #     class(MyPoligonsRsub1)
    #Por interseccion entre los shape files, me quedo solo con le hay dentro de la cuenca  (emplea la siguiente funcion de la libreria rgeos)
    #      inter = gIntersection(MyPoligonsRsub1, MyPPoligonsCsub1, byid=T, drop_not_poly=T)
    #     class(inter)
    
    
    theplot
    
    
    # #Por interseccion entre los shape files, me quedo solo con le hay dentro de la cuenca  (emplea la siguiente funcion de la libreria rgeos)
    # inter = gIntersection(MyPoligonsRsub1, MyPPoligonsCsub1, byid=T, drop_not_poly=T)
    # class(inter)
    # 
    # theplot <- spplot(inter, c("PS","SDW","Man","Min"),                     # "PS","SDW","Man","Min"
    #                 colorkey=list(space="bottom"), 
    #                 col.regions=plotclr.palette, 
    #                 at = round(breaks.qt$brks,2)  ) 
    # 
    # 
    # inter_df <- as.data.frame(inter)
    # # do some staff with "poly_df" that doesn't support SpatialPolygonsDataFrame
    # # then convert it to SPDF back again
    # s_inter <- SpatialPolygonsDataFrame(inter, inter_df)
    # 
    # theplot<-spplot(s_inter, c("PS","SDW","Man","Min"),                     # "PS","SDW","Man","Min"
    #                 colorkey=list(space="bottom"), 
    #                 col.regions=plotclr.palette, 
    #                 at = round(breaks.qt$brks,2)  )
    # 
    # plot(inter)
    # plot(inter, col = "blue", add = T)
    # inter@data
    # spplot(inter, "PrCpInc", main = "Palo Alto Demographics", sub = "Average Per Capita Income", 
    #        col = "transparent")
    
  }
  
  
  if(rates=="F"){
    #Para dibujarlo en valores de reduccion de esfuerzo, tenemos que evaluar la estrategia
    myStg_Evaluated <- EvalStg (BLS,StgtoPlot) 
    str(myStg_Evaluated[[2]]) #el segundo elemento de la lista de evaluacion son las reducciones todales de conentracion por cada tipo en cada subcuenca
    head( myStg_Evaluated[[2]][1:4,1:5])  
    
    #tengo que agregarlo por cada NUTS
    Mattemp <- NutsSectEffortFromSubc (myStg_Evaluated)  
    rownames(Mattemp) <- Mattemp$Nuts2
    Mattemp <- Mattemp[,3:6]   ## LORO ATENCION A MODIFICAR LA EVALUACION, POR EL ORDEN DEL TIPO DE REDUCCIONES
    colnames(Mattemp)<- rownames(StgtoPlot)   #############################   ###################   #################################
    StgValues <- Mattemp
    
    # si quiero mapear los load (hago el logaritmo para tener una escala mas razonable)
    MyDFRsub1 <- merge(MyPoligonsRsub1_orig@data, StgValues, by.x ="NUTS_ID",by.y ="row.names") #los esfuerzos by rownames. #junto los datos del espatialdataframe, con mi data frame de concentracion
    MyDFRsub1_lg<-MyDFRsub1
    MyDFRsub1_lg[,5:8] <- log( MyDFRsub1[,5:8]+0.1 )
    #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
    MyDFRsub1_lg <- MyDFRsub1_lg[order(match(MyDFRsub1_lg[,1],MyPoligonsRsub1_orig@data[,1])),]  #reordena en el mismo orden que en el objeto original
    MyPoligonsRsub1 <- MyPoligonsRsub1_orig #sobre una copia el original
    MyPoligonsRsub1@data <- MyDFRsub1_lg  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 
    
    plotclr.palette <- brewer.pal(n =nclr, name = pal)
    
    #para dibujar todas la variables al mismo tiempo
    allvarsvalues <- (unlist(MyPoligonsRsub1@data[,5:8]) )
    #intervalos que se adaptem 
    breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
    
    theplot <- spplot(MyPoligonsRsub1, c("PS","SD","Man","Min"),                    # "PS","SDW","Man","Min"
           colorkey=list(space="bottom"), 
           col.regions=plotclr.palette, 
           at = round(breaks.qt$brks,2)  )     #    #atencion, si no pongo el round, alguna region sale a valor cero incorrectaemente?????
    
  }
  

      

  
  
  
  # nclr <- 8
  # my.palette <- brewer.pal(n = nclr, name = "OrRd")
  # allvarsvalues <- unlist(MyPoligonsRsub1@data[,5:8])
  # breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = "quantile", intervalClosure = "right")
  # #  cuts = c( 4.305089 , 8.120350, 10.713820, 20.049481, 21.990815, 23.126604, 24.061641, 24.815260)
  # 
  # spplot(MyPoligonsRsub1, c("PS","SDW","Man","Min"),                        # "PS","SDW","Man","Min"
  #        colorkey=list(space="bottom"), 
  #        col.regions=my.palette, 
  #        at = round(breaks.qt$brks,2)  )     #    #atencion, si no pongo el round, alguna region sale a valor cero incorrectaemente?????
  # # cuts = nclr-1,  cuts = cuts   , col = "transparent"  #do.log = TRUE     ,pretty=T
  # 

  
  
  
  # clases <- class$brks
  # clases <- clases[-2]
  # plotclr2 <- c(plotclr,"red")
  # 
  # 
  # spplot(MyPoligonsRsub1, c("PS","SDW","Man","Min"),
  #        colorkey=list(space="bottom") ,col.regions=plotclr2,at=round(clases, digits=1) )
  # 
  # 
  # spplot(MyPoligonsRsub1, c("PS","SDW","Man","Min"),
  #        colorkey=list(space="bottom")   )         #,  scales=list(draw=TRUE)
  # 
  # library(RColorBrewer)
  # display.brewer.all()
  
  # nclr <- 8
  # my.palette <- brewer.pal(n = 8, name = "OrRd")
  # 
  # spplot( MyPoligonsRsub1, c("PS","SDW","Man","Min"),
  #        colorkey=list(space="bottom"), 
  #        col.regions = my.palette, 
  #        cuts = 7, col = "transparent")
  

#   nclr <- 8
#   my.palette2 <- brewer.pal(n = nclr,name ="BuPu")
# #  class <- classIntervals(plotvar, nclr, style="equal")
# #  colcode <- findColours(class, plotclr)
#   spplot(MyPoligonsRsub1, c("PS","SDW","Man","Min"),
#          colorkey=list(space="bottom"),
#          col.regions = my.palette2,
#          cuts = 7, col = "transparent")       # at=round(class$brks, digits=1)
  
  
#   nclr <- 8
#   my.palette <- brewer.pal(n = nclr, name = "OrRd")
#   allvarsvalues <- unlist(MyPoligonsRsub1@data[,5:8])
#   breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = "quantile", intervalClosure = "right")
# #  cuts = c( 4.305089 , 8.120350, 10.713820, 20.049481, 21.990815, 23.126604, 24.061641, 24.815260)
#   
#   spplot(MyPoligonsRsub1, c("PS","SDW","Man","Min"),                        # "PS","SDW","Man","Min"
#          colorkey=list(space="bottom"), 
#          col.regions=my.palette, 
#          at = round(breaks.qt$brks,2)  )     #    #atencion, si no pongo el round, alguna region sale a valor cero incorrectaemente?????
#          # cuts = nclr-1,  cuts = cuts   , col = "transparent"  #do.log = TRUE     ,pretty=T
  
              
  
  
  
# class(breaks.qt$brks)
# cuts = c( 4.305089 , 8.120350, 10.713820, 20.049481, 21.990815, 23.126604, 24.061641, 24.815260)
# class(cuts)
# breaks.qt$brks==cuts
# class(breaks.qt$brks[1])
# class(cuts[1])
         
         

  
  
  # MyPoligonsRsub1@data
  # 
  # 
  # 
  # 
  # my.palette <- brewer.pal(n = 7, name = "OrRd")
  # breaks.qt <- classIntervals(allvarsvalues, n = 6, style = "quantile", intervalClosure = "right")
  # 
  # breaks.qt$brks 
  
  
  
  return(theplot)
  
}



























###############################################
## 4. Dibuja NUTS2
###############################################
# N2POL: los poligonos de todos NUTS2
# List_N2_ID: La lista de los ID de los NUTS2 que queremos que dibuje
#  N2POL <-InfoMaps[[1]]
#  List_N2_ID <-ListnameNuts
Plot_NUTS2 <- function(N2POL,List_N2_ID,verbose=TRUE){
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  Ploting NUTS 2  Map ...                 ]")
  if (verbose) message("=======================================================")
  
  # class(N2POL)
  #  names(N2POL)
  #  dim(N2POL)
  # hace el subset de las provincias a dibujar
  # Subset_N2POL<-N2POL[N2POL$objectid==List_N2_ID,]
  Subset_N2POL<-N2POL[N2POL$objectid %in% List_N2_ID,]
  #  dim(Subset_N2POL)
  
  # summary(Subset_N2POL)
  #  Subset_N2POL@bbox
  # Subset_N2POL@bbox[1,1]
  # limites del mapa
  xlim<-Subset_N2POL@bbox[1,]
  ylim<-Subset_N2POL@bbox[2,]
  tmp_list<-paste(List_N2_ID,collapse="; ")
  nombre<- paste("NUTS2 list",tmp_list,sep=": ")
  plot(xlim, ylim, type='n', xlab='', ylab='',cex.axis=0.7, main=nombre)
  plot(Subset_N2POL, add=TRUE, col='white')
  
}

###############################################
## 5. Dibuja NUTS2 con un color que depende del valor de la variable
###############################################
# N2POL: los poligonos de todos NUTS2
# List_N2_ID <- listofCH_ID  La lista de los ID de los NUTS2 que queremos que dibuje
#  N2POL <-InfoMaps[[1]]
#  List_N2_ID <-ListnameNuts
#  ValuesVar<- valvarNUTS
# N2Contor <- InfoMaps[[2]]  #info para los contornos de los nuts2
Plot_NUTS2_var <- function(N2POL,List_N2_ID,ValuesVar,N2Contor=NULL,nombrevar=FALSE,verbose=TRUE){
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  Ploting NUTS2  Map in color for the variable        ]")
  if (verbose) message("=======================================================")
  
  # class(N2POL)
  #  names(N2POL)
  #  dim(N2POL)
  # hace el subset de las provincias a dibujar
  # Subset_N2POL<-N2POL[N2POL$objectid==List_N2_ID,]
  Subset_N2POL<-N2POL[N2POL$objectid %in% List_N2_ID,]
  #  dim(Subset_N2POL)
  # class(List_N2_ID)
  ValuesVar_ordered<- ValuesVar[List_N2_ID] #tienen que estar ordenados de acuerdo a como se pintan
  
  numcolores<- 8
  brks <- quantile(ValuesVar, seq(0,1,1/numcolores))
  brks<-format(brks,digits=1, nsmall=1) 
  cols <- grey((length(brks):2)/length(brks)-0.01)
  # brks<-seq(5,95,100/numcolores)
  
  # limites del mapa
  xlim<-Subset_N2POL@bbox[1,]
  ylim<-Subset_N2POL@bbox[2,]
  tmp_list<-paste(List_N2_ID,collapse="; ")
  nombre<- paste("NUTS2 list",tmp_list,sep=": ")
  if( nombrevar!=FALSE ) nombre <- paste(nombrevar,sep=":  "  )
  
  #EL MAPA
  layout(matrix(c(1,2),  byrow = TRUE),heights=c(10,2,1,1) )
  #layout.show(2)
  par(mar=c(0,2,1,0.2))
  
  plot(xlim, ylim, type='n', xlab='', ylab='',cex.axis=0.7, main=nombre)
  plot(Subset_N2POL, add=TRUE, col=cols[findInterval(ValuesVar_ordered, brks, all.inside=TRUE)],border=NA)
  
  #ralentiza un poco pero es interesante ver los contornos de las Nuts2 por encima
  if(!is.null(N2Contor)){
    Subset_N2Contornos<-N2Contor[N2Contor$objectid %in% List_N2_ID,]
    plot( Subset_N2Contornos, add=TRUE, col='white')  
  } 
  
  #LA LEYENDA
  #write the color bar leyend
  par(mar=c(0,0,0,0))   #changing the margins
  # plot.new()
  
  legend_image <- as.raster(matrix(cols, nrow=1))
  par(mar=c(0.2,0,0.5,1.0),cex=0.8)     #Bottom, left, top and right
  plot(c(0,1),c(0,4),type = 'n', axes=F ,xlab = "", ylab = "", main = "")  #con la ultima cordenada regulo grosor barra
  
  text(x=seq(0,1,l=(numcolores+1)) , y=1, labels = format(brks,digits=2, nsmall=2) )    #five labels, no rotate ,srt=90
  rasterImage(legend_image, 0, 0, 1,0.55)   #(image,xleft, ybottom, xright, ytop,angle = 0, interpolate = TRUE, ...)
  par(mfrow=c(1,1))  #para evitar que las siguientes figuras tengal el layout
  
}



###############################################
## 5. Dibuja SubCatchments
###############################################
# CATCHPOL<-InfoMaps[[3]] : los poligonos de todos Catchments
# List_CATCH_ID <- listofCH_ID  :La lista de los ID de los Catchments que queremos que dibuje
# nombre<-nameCat
Plot_SubCatch <- function(CATCHPOL,List_CATCH_ID,nombre=FALSE,verbose=TRUE){
  #  Plot_SubCatch(CATCHPOL,listofCH_ID,nameCat)
  if (verbose) message("=======================================================")
  if (verbose) message("[  Ploting SubCatchment Map 333...                 ]")
  if (verbose) message("=======================================================")
  print(nombre)
  
  
  # class(N2POL)
  #  names(N2POL)
  #  dim(N2POL)
  # hace el subset de las cuencas a dibujar
  # Subset_N2POL<-N2POL[N2POL$objectid==List_N2_ID,]
  Subset_CATCHPOL<-CATCHPOL[CATCHPOL$hydroid %in% List_CATCH_ID,]
  #  dim(Subset_N2POL)
  
  # summary(Subset_N2POL)
  #  Subset_N2POL@bbox
  # Subset_N2POL@bbox[1,1]
  # limites del mapa
  xlim<-Subset_CATCHPOL@bbox[1,]
  ylim<-Subset_CATCHPOL@bbox[2,]
  nombrecatch<- " "    #si  estoy dibujando varios subcatchments
  if( nombre!=FALSE ) nombrecatch <- nombre      #si es una cuenca con nombre
  plot(xlim, ylim, type='n', xlab='', ylab='',cex.axis=0.7, main= nombrecatch)
  plot(Subset_CATCHPOL, add=TRUE, col='aliceblue')
}


###############################################
## 6. Dibuja SubCatchments con un color que depende del valor de la variable
###############################################
# CATCHPOL<-InfoMaps[[3]] : los poligonos de todos Catchments
# List_CATCH_ID <- listofCH_ID  :La lista de los ID de los Catchments que queremos que dibuje
# nombre <- namecatch
# ValuesVar <- valvar2   : valores de la variable a representar mediante colores
# nombrevar <- nombreV
# logFormat <- FALSE
Plot_SubCatch_Var <- function(CATCHPOL,List_CATCH_ID,nombre=FALSE,ValuesVar,nombrevar=FALSE,logFormat=TRUE,BR=NULL, verbose=TRUE){
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  Ploting SubCatchment Map in Color ...              ]")
  if (verbose) message("=======================================================")
  
  #  List_CATCH_ID<-List_CATCH_ID[1:8]
  # class(N2POL)
  #  names(N2POL)
  #  dim(N2POL)
  # hace el subset de las cuencas a dibujar
  #names(ValuesVar)
  #length(ValuesVar)
  Subset_CATCHPOL<-CATCHPOL[CATCHPOL$hydroid %in% List_CATCH_ID,]
  Subset_CATCHVar<-log10(ValuesVar[names(ValuesVar) %in% List_CATCH_ID])  #solo una dimension, vector de valores
  if(logFormat==FALSE) Subset_CATCHVar<-(ValuesVar[names(ValuesVar) %in% List_CATCH_ID])  #NO APLICA EL LOGARITMO
  
  #  dim(Subset_N2POL)
  #length(Subset_CATCHVar)  
  #min(Subset_CATCHVar)
  #max(Subset_CATCHVar)
  
  #quantile(Subset_CATCHVar, seq(0,1,1/7))
  #rrt <- nc1$SID74/nc1$BIR74
  
  numcolores<- 9
  brks <- quantile(Subset_CATCHVar, seq(0,1,1/numcolores))
  brks<-format(brks,digits=1, nsmall=1) 
  if(!is.null(BR)){  #si me mandase una escala la sobreescribo
    brks<-BR
  }
  
  
  print(paste("los breaks ",brks,sep=""))
  #hist(Subset_CATCHVar)
  #hist(Subset_CATCHVar,breaks=brks)
  
  cols <- grey((length(brks):2)/length(brks)-0.01)   #le resto 0.01 para eviatar el blanco
  cols <- rev(heat.colors(length(brks),alpha = 1.0) )
  
  # limites del mapa
  xlim<-Subset_CATCHPOL@bbox[1,]
  ylim<-Subset_CATCHPOL@bbox[2,]
  # nombre<- "CATCHMENT"
  #  plot(xlim, ylim, type='n', xlab='', ylab='',cex.axis=0.7, main=nombre) 
  #  plot(Subset_CATCHPOL, add=TRUE, col='aliceblue',border=NA) #todos el mismo color
  #class(Subset_CATCHPOL)
  # plot(xlim, ylim, type='n', xlab='', ylab='',cex.axis=0.7, main=nombre) 
  #  plot(Subset_CATCHPOL, add=TRUE,col=cols[findInterval(Subset_CATCHVar, brks, all.inside=TRUE)],border=NA) #no se ve el blanco
  nombrecatch<- " "    #si  estoy dibujando varios subcatchments
  if( nombre!=FALSE ) nombrecatch <- nombre      #si es una cuenca con nombre
  if( nombrevar!=FALSE ) nombrecatch <- paste(nombre,nombrevar,sep=":  "  )
  #EL MAPA
  layout(matrix(c(1,2),  byrow = TRUE),heights=c(10,2,1,1) )
  #layout.show(2)
  par(mar=c(0,2,1,0.2))
  plot(xlim, ylim, type='n', xlab='', ylab='',cex.axis=0.7, main=nombrecatch) 
  print(paste("el intervalo :",findInterval(Subset_CATCHVar, brks, all.inside=TRUE),sep=""))
  plot(Subset_CATCHPOL, add=TRUE,col=cols[findInterval(Subset_CATCHVar, brks, all.inside=TRUE)],border=NA) #no se ve el blanco
  
  #LA LEYENDA
  #write the color bar leyend
  par(mar=c(0,0,0,0))   #changing the margins
  # plot.new()
  
  legend_image <- as.raster(matrix(cols, nrow=1))
  par(mar=c(0.2,0,0.5,1.0),cex=0.8)     #Bottom, left, top and right
  plot(c(0,1),c(0,4),type = 'n', axes=F ,xlab = "", ylab = "", main = "")  #con la ultima cordenada regulo grosor barra
  
  text(x=seq(0,1,l=(numcolores+1)) , y=1, labels = format(brks,digits=2, nsmall=2) )    #five labels, no rotate ,srt=90
  
  rasterImage(legend_image, 0, 0, 1,0.55)   #(image,xleft, ybottom, xright, ytop,angle = 0, interpolate = TRUE, ...)
  
  par(mfrow=c(1,1))  #para evitar que las siguientes figuras tengal el layout
  
}

###############################################
## 7. Dibuja NUTS2+SubCatchments
###############################################
# N2POL <- InfoMaps[[1]]
# List_N2_ID <- ListnameNuts
# CATCHPOL <- InfoMaps[[3]]
# List_CATCH_ID <-ListSubCatCh 
# N2Contor <- InfoMaps[[2]]  #info para los contornos de los nuts2
Plot_NUTS2_SubCatch <- function(N2POL,List_N2_ID,CATCHPOL,List_CATCH_ID,N2Contor=NULL,verbose=TRUE){
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  Ploting NUTS2 + Catchment  Map ...                 ]")
  if (verbose) message("=======================================================")
  
  Subset_N2POL<-N2POL[N2POL$objectid %in% List_N2_ID,]
  Subset_CATCHPOL<-CATCHPOL[CATCHPOL$hydroid %in% List_CATCH_ID,]
  
  #si le llegan los contornos de las provincias los filtra
  if(!is.null(N2Contor)) Subset_N2Contornos<-N2Contor[N2Contor$objectid %in% List_N2_ID,]
  
  xlim<-Subset_N2POL@bbox[1,]
  ylim<-Subset_N2POL@bbox[2,]
  nombre<- "NUTS2 + Catch "
  plot(xlim, ylim, type='n', xlab='', ylab='',cex.axis=0.7, main=nombre)
  plot(Subset_N2POL, add=TRUE, col='white')
  plot(Subset_CATCHPOL, add=TRUE, col='aliceblue',border="red",lwd=0.1,lty=3)
  
  #ralentiza un poco pero es interesante ver los contornos de las Nuts2 por encima
  if(!is.null(N2Contor)) plot( Subset_N2Contornos, add=TRUE, col='black')  
}


###############################################
## 8. Dibuja CATCHMENT
###############################################
# Dibuja toda una cuenca, es decir todos los subcatchment de un Catcment
# Lista todos los subcatchment de un catchment y llama a el de imprimir todos los subcatment
# CATCHPOL<-InfoMaps[[3]] : los poligonos de todos SubCatchments
# Catch_subCathc<-  C_SC_Rel  :    Catchment - Subcatchment pertenence
# nameCat <- nameCathc     #nombre de la cuenca principal
# var<- r_fresW
Plot_Catch <- function(CATCHPOL,Catch_subCathc,nameCat,var,nombrevar=FALSE,logFormat=TRUE,BR=NULL){
  
  #lista de todos los subcatchment en ese catchment
  listofCH_ID<-Catch_subCathc[Catch_subCathc$basin_name == nameCat ,]$cathment_hydroid   #ATENCION DEBERIA CONVERTIR A STRING
  print(nameCat)
  
  # Instancia a dibujar todos los subcatch de un catchmet
  if( nombrevar ==FALSE )  Plot_SubCatch(CATCHPOL,listofCH_ID,nombre=nameCat )       
  
  # Instancia a dibujar una variable todos los subcatch de un catchmet
  if( nombrevar!=FALSE )  Plot_SubCatch_Var(CATCHPOL,listofCH_ID,nombre=nameCat,var,nombrevar,logFormat,BR)
  
}


###############################################
## 10. NUTS2 por los que pasa una CUENCA
###############################################
# Esta funcion tiene dos posibilidades:
# 1) Devolver un DF listando Subcatch a subcathc a que Nuts2 pertenecen
# 2) Devolver la lista de los Nuts2
# nameCatch <- nameCatch
# CrSC <- C_SC_Rel
# Ct_ID <-   CatchmentID
# Intersecc <- intersec_fracc
# ATENCION CONSIDERO !=0, Es decir que pueden estar en dos NUTS2
# Mode: 1) DF relacione SC-N2  2) Ratio AreaSubcatch in each N2.   Por defecto devuelve la lista
NUTS2_in_Catch <- function(nameCatch,Ct_ID,CrSC,Intersecc,Mode=2){
  
  ListSubCathc<- as.character( CrSC[CrSC$basin_name == nameCatch ,]$cathment_hydroid )
  
  if (Mode==1){
    #DF con el listado entre cada Subcatchment y el NUTS2 en el que esta
    df_SC_N2 <- data.frame(SC = character(), N2 = character() )
    for (sc in ListSubCathc){
      # sc<-52001308
      nameCathc<-as.character(sc)
      Listnametemp<-names(which(Intersecc[nameCathc,]!=0))
      if(length(Listnametemp)>1){   # Si un subcatch esta en varios N2, busca en el que hay mas area
        indiceMaxArea <- which.max( Intersecc[nameCathc, Listnametemp[seq(1:length(Listnametemp))] ] )
        df_SC_N2<- rbind(df_SC_N2, data.frame(SC=nameCathc , N2=Listnametemp[indiceMaxArea]) )
      }else{
        for(ln in Listnametemp){  #si el subcatch esta en varios los meto todos
          df_SC_N2<- rbind(df_SC_N2, data.frame(SC=nameCathc , N2=ln) )
        } #fin del segundo for
      } #fin del else
    }  #fin del primer for
    
    result<-df_SC_N2
  } #fin del If de Mode==1
  if (Mode==2){
    # head(ListSubCathc)
    # Lista de todos los Nuts2 que contienen esos Subcatch
    tempMatrix <- Intersecc[Ct_ID %in% ListSubCathc, ,drop=FALSE]  #hago un subset con solo las filas que estan en esas cuencas
    # which(colSums(tempMatrix)!=0)  # !=0 si quiero tambien los que estan en dos
    #atencion lo anterior devuelve el nombre del N2, no el indice de la columna
    ListnameNuts2 <-  names(which(colSums(tempMatrix)!=0)  )
    ratioSubCinN2<-round(colSums(tempMatrix[,ListnameNuts2])/length(ListSubCathc), digits = 4) *100
    result<-ratioSubCinN2
  }  #fin del if Mode==2
  
  #ATENCION. Futura mejora, cuando haya un SC en dos N2, seleccionare en el que tenga mas area
  return(result)
}

