

###############################################
## 2. Loading Info to create the Maps
###############################################
# Catchname <- BasinName  
# Nustnames <- The_N2
# Catch_Bas <- C_SC_Rel
# listShapes <-listTotalShapes    
          # [[1]]: NUTS2 poligons
          # [[2]]: NUTS2 lines
          # [[3]]: Basin pOligons      
          # [[4]]: Basin lines  
          # [[5]]: Catchment poligons
          # [[6]]: Catchment lines
          # [[7]]: Informacion  limites para cada pais (para el box)
Subsetingshapes_QT <- function(Catchname,Nustnames,listShapes,Catch_Bas,verbose=TRUE){
  
  
  # class(listShapes[[1]])
  # listShapes[[1]]@bbox
  # summary(listShapes[[1]])
  # attributes(listShapes[[1]]@data)
  # attributes(listShapes[[1]]@data)$names
  # str(listShapes[[1]]@data)
  # 
  # listShapes[[1]]$NUTS_ID
  
  
  #########################
  ### LAS REGIONES (NUtS2)  (las que intersectan la cuenca)
  MyPoligonsNuts <- listShapes[[1]] [listShapes[[1]]$NUTS_ID %in% Nustnames ,]   #el primer elemento de la lista contiene todas las regiones
  # [[1]]: NUS2 poligons de la cuenca
  
  plot(MyPoligonsNuts)
  summary(MyPoligonsNuts)
  MyPoligonsNuts@bbox
  MyPoligonsNuts@data
  
  ### LAS REGIONES (las LINEAS): listShapes[[2]] 
  MyPLinesNuts <- listShapes[[2]] [listShapes[[2]]$NUTS_ID %in% Nustnames ,]   #el primer elemento de la lista contiene todas las regiones
  plot(MyPLinesNuts)
  
  #############################################
  ### LA SUPERFICIE CUENCA, BASIN  (subseting por el nombre de la cuenca), la superficie de la cuenca
  MyPoligonsBasin <- listShapes[[3]][listShapes[[3]]$Name %in% Catchname ,] 
  # [[3]]: Catchment poligons
  plot(MyPoligonsBasin)  
  summary(MyPoligonsBasin)
  MyPoligonsBasin@bbox
  MyPoligonsBasin@data
  str(MyPoligonsBasin@data)
  
  summary(listShapes[[3]])
  attributes(listShapes[[3]]@data)$names
  listShapes[[3]]$Name
  
  ### LAS LINEAS DE BORDE DE LA CUENCA, BASIN 
  MyPLinesBasin <- listShapes[[4]][listShapes[[4]]$Name %in% Catchname ,]   #subseting la cuenca
  # [[2]]: Catchment lines
  plot(MyPLinesBasin)
  summary(MyPLinesBasin)
  MyPLinesBasin@bbox
  MyPLinesBasin@data
  str(MyPLinesBasin@data)
  
  
  
  #########################
  ### LAS SUBCUENCAS  (subsetting por el nombre de la cuenca), LOS POLIGONOS
  summary(listShapes[[5]])
  str(listShapes[[5]]@data)
  
  df_hydroID <- Catch_Bas[ Catch_Bas$basin_name == Catchname, ]   #busco los hydro de las subcuencas de ese Basin
  MyPoligonsSubCth <- listShapes[[5]][listShapes[[5]]$hydroid %in% df_hydroID$cathment_hydroid ,]   #subseting la cuenca

  plot(MyPoligonsSubCth) #todas las subcuencas
  summary(MyPoligonsSubCth)
  MyPoligonsSubCth@bbox
  MyPoligonsSubCth@data
  str(MyPoligonsSubCth@data)
  
  ### LAS SUBCUENCAS  (subsetting por el nombre de la cuenca), LAS LINEAS DE CONTORNOS
  df_hydroID <- Catch_Bas[ Catch_Bas$basin_name == Catchname, ]   #busco los hydro de las subcuencas de ese Basin
  MyPLinesSubCth <- listShapes[[6]][listShapes[[6]]$hydroid %in% df_hydroID$cathment_hydroid ,]   #subseting la cuenca
  
  plot(MyPLinesSubCth) #todas las subcuencas
  summary(MyPLinesSubCth)
  MyPLinesSubCth@bbox
  MyPLinesSubCth@data
  str(MyPLinesSubCth@data)
  
  
  lista <- list(MyPoligonsNuts,MyPLinesNuts,MyPoligonsBasin,MyPLinesBasin,MyPoligonsSubCth,MyPLinesSubCth)
  
  return(lista)
}





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
  inter = gIntersection( N2POL, BasinPOL, byid=T, drop_not_poly=T)
  
  caja<-bbox(inter)    #DIFERENCIA respecto a la otra version
  plot(caja[1,], caja[2,], type='n', xlab=expression(paste('Lon (', degree, ')'), sep='')
       , ylab=expression(paste('Lat (', degree, ')'), sep='') )
  plot(inter, col="aliceblue", add=T,lwd=3)
  
  # la siguiente linea podria ser una opcion (quizas esto ya se ve bien en la otra figura)
  plot(SubcatchLines,add=T,col="red",lwd=0.1)  
  
  title(main=paste0(nombre),  sub=" N2 & Basin")
  # si queremos que anada el Id de cada region
  if (etiquetas==TRUE)  text(coordinates(N2POL), labels=N2POL@data$NUTS_ID, cex=1.2)   #etiqueta con nombre de la region
  
  class(inter)
  inter@data$NUTS_ID   #atencion" inter es de la clase" :SpatialPolygons  no de la clase: SpatialPolygonsDataFrame 

}


# Mapeo el valor de una variable para cada subcuenca, con SPPLOT
# catchNa <-BasinName    BasinName, LstMyShapes, Evaluated_BLScen[[1]], ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation
# LstMySh <-LstMyShapes
# TheScen <- Evaluated_BLScen[[1]]    BLS_WEI   Evaluated_BLScen      
# ListVar  <- ListIndexUses
# nclr <- NumClassInterv
# pal <- paleta
# style <- estilo 
# scal <-  scale   
# scaleRel <- scaleRelation
#  FALTA MEJORAR LA FUNCION PARA QUE SIRVA PARA DIBUJAR CUALQUIER VARIABLE (PASANDO EL INDICE)
Map_var_SubCatch_SPPLOT <- function (catchNa,LstMySh,TheScen,ListVar,nclr,pal,style,scal,scaleRel,verbose=TRUE){     
  
  # Lo primero cargar los shapefiles
  MyPoligonsRsub1_orig <- LstMySh[[5]]  #los poligonos de las subcuencas
  MyPLinesCsub1 <- LstMySh[[2]]   #el borde de la cuenca lo empleo en la primera version
  MyPPoligonsCsub1 <- LstMySh[[3]]  # el poligono de la cuenca lo empleo en la segunda version version
  
  #Anado los valores de los ratios de reduccion de las 4 variables (sectores) a mapear (que se han agregado en cada region) al shape del mapa
  str(MyPoligonsRsub1_orig@data)  #el shape
  
  # Preparar la tabla con todas las variables
  TheTable <- TheScen  #copia todas las columnas

  #if the user select logarithmic scale, we transfor the original data (les sumo 1, para evitar infinitos o negativos)
  print (scal ==  "logarithmic")
  if (scal ==  "logarithmic")   TheTable[,2:7] <- log(TheTable[,2:7]+1)
  if(scal == "logarithmic"  &  ListVar==9) TheTable[,c(10)] <- log(TheTable[,c(10)]+1)  #si es WEI, tambien cambia esa columna
    
  #lo mezclo con el shape
  MyDFRsub1 <- merge(MyPoligonsRsub1_orig@data, TheTable, by.x ="hydroid",by.y ="Row.names") #los esfuerzos by rownames. #junto los datos del espatialdataframe, con mi data frame de concentracion
  MyDFRsub1_lg <- MyDFRsub1
  # MyDFRsub1_lg[,5:8] <- log( MyDFRsub1[,5:8]+0.1 )
  #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
  MyDFRsub1_lg <- MyDFRsub1_lg[order(match(MyDFRsub1_lg[,1],MyPoligonsRsub1_orig@data[,1])),]  #reordena en el mismo orden que en el objeto original
  MyPoligonsRsub1 <- MyPoligonsRsub1_orig #sobre una copia el original
  MyPoligonsRsub1@data <- MyDFRsub1_lg  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 

  if (scaleRel == "Independent") {
     # LOOP PARA LOS MAPAS CON ESCALA INDEPENDIENTE
      UsesNames <- c("Fresh Water","Domestic Water","Energy Water","Industrial Water","Irrigation Water","Livestock Water","---","---","WEI")
      for (index in ListVar){ 
       # i=2   #indice del fresW
        i=index+1  #la columna donde esta cada variable es una mas
        allvarsvalues <- unlist(MyPoligonsRsub1@data[,i:i])   #selecciono solo una columna a la vez
        breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
        #para que el ultimo intervalo no de problemas, le aumento algo al mayor valor 
        breaks.qt$brks[length(breaks.qt$brks)]<-breaks.qt$brks[length(breaks.qt$brks)]+breaks.qt$brks[length(breaks.qt$brks)]/100
        
        plotclr.palette <- brewer.pal(n =nclr, name = pal)
        
        print(   spplot(MyPoligonsRsub1[,i],  main = paste0(UsesNames[index], " (", scal, ")"  ), 
                        sub = paste0(style," breaks"),
                        col = "transparent",
                        colorkey=list(space="bottom"),
                        col.regions=plotclr.palette,
                        at = breaks.qt$brks )   )
        
      }  #fin del loop para dibujar los usos
  
  }  #if scale relation is independent
  
  
  # TODOS LOS MAPAS EN LA MISMA ESCALA
  if (scaleRel == "Common") {
      # ListVar
      index <- ListVar+1
      allvarsvalues <- unlist(MyPoligonsRsub1@data[,index])   #selecciono solo una columna a la vez
      breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
      #para que el ultimo intervalo no de problemas, le aumento algo al mayor valor 
      breaks.qt$brks[length(breaks.qt$brks)]<-breaks.qt$brks[length(breaks.qt$brks)]+breaks.qt$brks[length(breaks.qt$brks)]/100
      
      plotclr.palette <- brewer.pal(n =nclr, name = pal)
      
      print(   spplot(MyPoligonsRsub1[,index], main = paste0("Water Uses ", " (", scal, ")"  ), 
                      sub = paste0(style," breaks"),
                      col = "transparent",
                      colorkey=list(space="bottom"),
                      col.regions=plotclr.palette, layout = c(5, 1),
                      at = breaks.qt$brks )   )
  }  #if scale relation is common
  
}





# Mapeo el valor de una variable para cada NUTS2, con SPPLOT
# FUNCION QUE PREPROCESA LOS DATOS A MAPEAR CON SPPLOT
# type  <- "WA" # WA: Water Availability  ; WU: Water Uses  ; WEI: Water extraction index ; EF: Effort Reduction 
# TheScen <-  t(stg_n1_strateg)    newExtraccNUT    listWEI    t(stg_n1_strateg)
# scal <-  scale
# agrm <-  agregation
Prepro_to_Map_N2 <- function(type,TheScen,scal,agrm,verbose=TRUE){
  
  if(type == "EF"){
    #lo convierto a data frame
    DF_TheScen <- as.data.frame(TheScen)
    #anado una columna con el nombre del row
    DF_TheScen$N2 <- rownames(DF_TheScen)
    #temporalmente anado una columna con un valor fijo (para tener igual estructura con el resto de usos)
    DF_TheScen$n <- 3
    #reordeno columnas
    TheTable <- DF_TheScen[,c(6,7,1,2,3,4,5)]
  }
  
  if(type != "EF"){         # All rest not EF:   WA  ;WU; WEI: 
    #Llega una lista, cada una es una tabla correspondiente a cada uso. Y en cada tabla estan todas las metricas de agregacion
    # Preprocesamos la informacion de esta lista:
    TheTableMetric <- TheScen[[1]][,c(1,2)] #creo una tabla en la que meto las dos primeras columnas comunes
    thegrm <- c("Mean","Median","Q75","Max")
    index <- 2+match(agrm,thegrm)  #el indice de la columna con  la metrica seleccionada
    #anado todas columnas para la metrica seleccionada
    for (i in 1:length(TheScen) ){    #para cada uno de los elementos de la lista (usos)
      TheTableMetric <- cbind (TheTableMetric , TheScen[[i]][,index] )
    }
    if( length(TheTableMetric) == 3 )  colnames(TheTableMetric) <- c("N2","n","WEI")  #para mapa del WEI
    if( length(TheTableMetric) > 3 )  colnames(TheTableMetric) <- c("N2","n","Fresh","Domestic","Energy","Industrial","Irrigation","Livestock")  #para el caso de todos los mapas de uso
    
    TheTable <- TheTableMetric  
  }
  
  #if the user select logarithmic scale, we transfor the original data
  print (scal ==  "logarithmic")
  if (scal ==  "logarithmic") TheTable[ ,3:length(TheTable)] <- log(TheTable[ ,3:length(TheTable)]+1)
  
  # El formato de la tabla devuelta debe ser: 
  #  en cada fila valores por N2
  #  en la primera columna el nombre del N2, segunda couts, tercera : primera variable, y asi para las siguientes  
  
  return (TheTable)
  
}


# Mapeo el valor de una variable para cada NUTS2, con SPPLOT
# catchNa <- BasinName
# LstMySh <- LstMyShapes
# type <- mytype      #tipo  "WA" # WA: Water Availability  ; WU: Water Uses  ; WEI: Water extraction index ; EF: Effort Reduction 
# TheScen <- TheVariables   newExtraccNUT    listWEI      TheVariables
# ListVar <- ListIndexUses
# nclr <- NumClassInterv
# pal <- paleta
# style <- estilo 
# scal <-  scale
# agrm <-  agregation
# scaleRel <- scaleRelation
Map_var_NUTS_SPPLOT <- function(catchNa,LstMySh,type,TheScen,ListVar,nclr,pal,style,scal,agrm,scaleRel,verbose=TRUE){
  
  # Lo primero cargar los shapefiles
  MyPoligonsRsub1_orig <- LstMySh[[1]]  #los poligonos de las NUTS2
  MyPLinesCsub1 <- LstMySh[[2]]   #el borde de la cuenca lo empleo en la primera version
  MyPPoligonsCsub1 <- LstMySh[[3]]  # el poligono de la cuenca lo empleo en la segunda version version
  
  #Anado los valores de los ratios de reduccion de las 4 variables (sectores) a mapear (que se han agregado en cada region) al shape del mapa
  str(MyPoligonsRsub1_orig@data)  #el shape
  
  TheTable <- TheScen
  
  # si quiero mapear los load (hago el logaritmo para tener una escala mas razonable)
  MyDFRsub1 <- merge(MyPoligonsRsub1_orig@data, TheTable, by.x ="NUTS_ID",by.y ="N2") #los esfuerzos by rownames. #junto los datos del espatialdataframe, con mi data frame de concentracion
  MyDFRsub1_lg<-MyDFRsub1
  # MyDFRsub1_lg[,5:8] <- log( MyDFRsub1[,5:8]+0.1 )
  #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
  MyDFRsub1_lg <- MyDFRsub1_lg[order(match(MyDFRsub1_lg[,1],MyPoligonsRsub1_orig@data[,1])),]  #reordena en el mismo orden que en el objeto original
  MyPoligonsRsub1 <- MyPoligonsRsub1_orig #sobre una copia el original
  MyPoligonsRsub1@data <- MyDFRsub1_lg  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 
  

  if (scaleRel == "Independent") {
    # LOOP PARA LOS MAPAS CON ESCALA INDEPENDIENTE
    UsesNames <- colnames(TheTable)[3:length(TheTable)] 
    for (index in ListVar){ 
      i=index+5  #la columna donde esta cada variable es una mas
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
      
      print(  spplot(MyPoligonsRsub1[,i],  main = paste0(UsesNames[index], " (", scal, ")"  ), 
                      sub = paste0(style," breaks"),
                      col = "transparent",
                      colorkey=list(space="bottom"),
                      col.regions=plotclr.palette,
                      at = breaks.qt$brks,
                      sp.layout = list(spBasin,tps) )   )
      
    }  #fin del loop para dibujar los usos
    
  }  #if scale relation is independent
  
  
  # TODOS LOS MAPAS EN LA MISMA ESCALA
  if (scaleRel == "Common") {
    # ListVar
    index <- ListVar+5
    allvarsvalues <- unlist(MyPoligonsRsub1@data[,index])   #
    if(length(allvarsvalues)==1) allvarsvalues <- c(allvarsvalues/1.01,allvarsvalues)  #si solo hay un Nuts, solo habra una categoria, creo una artificial para que no pete el mapa
    breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
    #para que el ultimo intervalo no de problemas, le aumento algo al mayor valor 
    breaks.qt$brks[length(breaks.qt$brks)]<-breaks.qt$brks[length(breaks.qt$brks)]+breaks.qt$brks[length(breaks.qt$brks)]/100
    
    plotclr.palette <- brewer.pal(n =nclr, name = pal)
    
    #para poder anadir un segundo mapa, con el que resaltar el contorno de la cuenca, lo anado como lista
    spBasin <- list("sp.polygons", MyPPoligonsCsub1,first=FALSE,col="black")   #, which = 1
    # y otra lista para anadir los indices de los nuts (luego anado las listas como list en sp.layout)
    tps <- list("sp.text", coordinates(MyPoligonsRsub1), MyPoligonsRsub1@data$NUTS_ID, cex=0.3, col="black")   #, which = 1
    
    
    if(  type %in% c("EF", "WU") ){
      if(type == "EF") txtlab <- " Effort "
      if(type == "WU") txtlab <- " Water Uses "
      print(   spplot(MyPoligonsRsub1[,index], main = paste0(txtlab, " (", scal, ")"  ), 
                      sub = paste0(style," breaks"),
                      col = "transparent",
                      colorkey=list(space="bottom"),
                      col.regions=plotclr.palette,     layout = c(5, 1),
                      at = breaks.qt$brks ,
                     sp.layout = list(spBasin,tps)) )   #en el de multiples no me funciona
      
    } else {          #solo cambio el layout
      print(   spplot(MyPoligonsRsub1[,index], main = paste0("Water Uses ", " (", scal, ")"  ), 
                      sub = paste0(style," breaks"),
                      col = "transparent",
                      colorkey=list(space="bottom"),
                      col.regions=plotclr.palette,  
                      at = breaks.qt$brks ,
                      sp.layout = list(spBasin,tps) ) )
    }
    
  }  #if scale relation is common
  
  
}










# catchNa <- BasinName
# LstMySh <- LstMyShapes
# BLS <- BLScen
# StgtoPlot <- My_strateg2      # My_strateg2[4,4]=16 , 
# nclr  <- NumClassInterv
# pal <- paleta
# style <- estilo
# rates <- "T" 
Map_EffortRed_SPPLOT_QT <- function (catchNa,LstMySh,BLS,StgtoPlot,nclr,pal,style,rates,verbose=TRUE){     
  
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
    allvarsvalues <- unlist(MyPoligonsRsub1@data[,2:6])
    #intervalos que se adaptem 
    breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
    
    theplot<-spplot(MyPoligonsRsub1, c("Domest","Energ" ,"Indust","Irrig","Livest"),                     # "PS","SDW","Man","Min"
                    colorkey=list(space="bottom"),  layout = c(5, 1),
                    col.regions=plotclr.palette,  
                    at = round(breaks.qt$brks,2)  )     #    #atencion, si no pongo el round, alguna region sale a valor cero incorrectaemente?????
   
    theplot

    
     # cuts = nclr-1,  cuts = cuts   , col = "transparent"  #do.log = TRUE     ,pretty=T
    
    
    #     class(MyPoligonsRsub1)
    #Por interseccion entre los shape files, me quedo solo con le hay dentro de la cuenca  (emplea la siguiente funcion de la libreria rgeos)
    #      inter = gIntersection(MyPoligonsRsub1, MyPPoligonsCsub1, byid=T, drop_not_poly=T)
    #     class(inter)
    
    
  
    
    
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
    MyEval_Scen <- EvalStg (BLS,StgtoPlot)  
    head(MyEval_Scen[[2]]) #  #devuelve [[1]]el nuevo escenario de demandas, y [[2]]la diferencia de dichas demandas con relacion al BLS
    #el segundo nos da ya metros cubicos de reduccion de demanda. 
    # Falta agregarlos por nuts (ya tengo una funcion que los suma por nuts y tipo)
    PonderEffor <- "D"        #  D: Default (todos igual importancia) . En caso contrario pasa el array de pesos de ponderacion
    MyEffort_N2_sect <- ComputeEffort(MyEval_Scen,PonderEffor)
    
    Mattemp <- MyEffort_N2_sect[,c(3,4,5,6,7)]   #  ATENCION ESTOY REORDENANDOLOS, PARA EVITAR PROBLEMAS
    rownames(Mattemp) <- MyEffort_N2_sect$N2  #asigno nombres de N2
    colnames(Mattemp) <-  rownames(StgtoPlot)
    StgValues <- Mattemp
    
    # si quiero mapear los load (hago el logaritmo para tener una escala mas razonable)
    MyDFRsub1 <- merge(MyPoligonsRsub1_orig@data, StgValues, by.x ="NUTS_ID",by.y ="row.names") #los esfuerzos by rownames. #junto los datos del espatialdataframe, con mi data frame de concentracion
    MyDFRsub1_lg<-MyDFRsub1
    MyDFRsub1_lg[,2:6] <- log( MyDFRsub1[,2:6]+0.1 )
    #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
    MyDFRsub1_lg <- MyDFRsub1_lg[order(match(MyDFRsub1_lg[,1],MyPoligonsRsub1_orig@data[,1])),]  #reordena en el mismo orden que en el objeto original
    MyPoligonsRsub1 <- MyPoligonsRsub1_orig #sobre una copia el original
    MyPoligonsRsub1@data <- MyDFRsub1_lg  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 
    
    plotclr.palette <- brewer.pal(n =nclr, name = pal)
    
    #para dibujar todas la variables al mismo tiempo
    allvarsvalues <- (unlist(MyPoligonsRsub1@data[,2:6]) )
    #intervalos que se adaptem 
    breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
    
    theplot <- spplot(MyPoligonsRsub1, c("Domest","Energ" ,"Indust","Irrig","Livest"),                    # "PS","SDW","Man","Min"
                      colorkey=list(space="bottom"),  layout = c(5, 1),
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










########
# YA NO LA USAMOS, PERO ES OTRA FORMA DE DIBUJAR VARIABLES POR NUTS2 
# DONDE SOLO PINTA LA PARTE DEL NUTS2 QUE ESTA EN ESE BASIN


###############################################
## 4. Plot variable by Regions (es similar que le de la version de concentracion)
###############################################
# catchNa <- catchname    
# LstMySh <- LstMyShapes      # LstShapes[[1]]
# indexpress <- PresIndes
# estrac <- newExtraccNUT[[1]]     #data frame con todos los tipos de agregacion
# IndAgr <- AgrIndex
# nclr <- NumClassInterv
# pal <- paleta
# style <- estilo      
Map_RegWater_QT <- function(catchNa,LstMySh,indexpress,estrac,IndAgr,nclr,pal,style,verbose=TRUE){
  
  MyPoligonsRsub1_orig <- LstMySh[[1]]
  MyPLinesCsub1 <- LstMySh[[2]]   #el borde de la cuenca lo empleo en la primera version
  MyPPoligonsCsub1 <- LstMySh[[3]]  # el poligono de la cuenca lo empleo en la segunda version version
  
  #Anado el valor de la metrica con la que se han agregado las concentraciones en cada region
  str(MyPoligonsRsub1_orig@data)
  #cbind.data.frame(coordinates(MyPoligonsRsub1_orig),(MyPoligonsRsub1_orig@data$NUTS_ID))
  #head(MyPoligonsRsub1_orig@data)
  #class(MyPoligonsRsub1_orig)
  MyDFRsub1 <- merge(MyPoligonsRsub1_orig@data, estrac[,c(1,(2+IndAgr))], by.x ="NUTS_ID",by.y ="N2") #junto los datos del espatialdataframe, con mi data frame de concentracion
  #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
  MyDFRsub1<- MyDFRsub1[order(match(MyDFRsub1[,1],MyPoligonsRsub1_orig@data[,1])),]  #reordena en el mismo orden que en el objeto original
  
  MyPoligonsRsub1<-MyPoligonsRsub1_orig #sobre una copia el original
  MyPoligonsRsub1@data <- MyDFRsub1  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 
  # cbind.data.frame(coordinates(MyPoligonsRsub1),(MyPoligonsRsub1@data$NUTS_ID))
  
  # names(MyPoligonsRsub1@data)[5] <- "median"   #lo renombro
  class(MyPoligonsRsub1)
  plotvar <- MyPoligonsRsub1@data[,2]  #respecto a la rutina que uso en el de concentraciones, es otra columna 
  nameIAgr<-names(MyPoligonsRsub1@data)[2]  #el nombre de la variable
  
  
  #preparando la paleta de colores  #TENGO QUE REDUCIR LOS DECIMALES Y PONER FUERA LA ESCALA, MIRAR EJEMPLOS MAS ABAJO
  #   nclr <- 8
  if(IndAgr==1) {precis=0}
  if(IndAgr>1) {precis=1}
  plotclr <- brewer.pal(nclr,pal)
  class <- classIntervals(plotvar, n=nclr, style= style ,dataPrecision=precis)  #Data precision, o 0 o 1. Dependiendo de la tecnica de agregacion
  colcode <- findColours(class, plotclr)
  
  # Dependiendo de la presion que este dibujando, cambia le caption 
  strWater<-c("Fresh Water", "Domestic Water", "Industrial Water", "Energy Water", "Livestock Water","Irrigation Water","WEI")
  text_legend <- paste( style, " class intervals"," " , strWater[indexpress] )
  
  
  ############  
  #### VERSION 1
  caja <- bbox(MyPoligonsRsub1)       #preparando la caja y el plot
  plot(caja[1,], caja[2,], type='n', xlab=expression(paste('Lon (', degree, ')'), sep='')
       , ylab=expression(paste('Lat (', degree, ')'), sep='') )
  plot(MyPoligonsRsub1, col=colcode, add=T)
  plot(MyPLinesCsub1,add=T,col="black",lwd=3)
  title(main=paste0(nameIAgr, " Concentrationcion"), 
        sub="Quantile (Equal-Frequency) Class Intervals")
  
  text(coordinates(MyPoligonsRsub1), labels=MyPoligonsRsub1@data$NUTS_ID, cex=0.7) 
  legend("bottomleft", legend=names(attr(colcode, "table")), 
         fill=attr(colcode, "palette"), cex=0.8, bty="n", title="Concentracion", text.font=4, bg='lightblue')
  
  #  cbind.data.frame(coordinates(MyPoligonsRsub1),(MyPoligonsRsub1@data$NUTS_ID))
  
  
  coordinates(MyPoligonsRsub1)
  
  ############  
  #### VERSION 2
  #Por interseccion entre los shape files, me quedo solo con le hay dentro de la cuenca  (emplea la siguiente funcion de la libreria rgeos)
  inter = gIntersection(MyPoligonsRsub1, MyPPoligonsCsub1, byid=T, drop_not_poly=T)
  
  caja<-bbox(inter)    #DIFERENCIA respecto a la otra version
  plot(caja[1,], caja[2,], type='n', xlab=expression(paste('Lon (', degree, ')'), sep='')
       , ylab=expression(paste('Lat (', degree, ')'), sep='') )
  
  plot(inter, col=colcode, add=T)
  
  title(main=paste0(nameIAgr," " , strWater[indexpress]), 
        sub="Quantile (Equal-Frequency) Class Intervals")
  
  text(coordinates(MyPoligonsRsub1), labels=MyPoligonsRsub1@data$NUTS_ID, cex=0.8,col="green") 
  legend("bottomleft", legend=names(attr(colcode, "table")), 
         fill=attr(colcode, "palette"), cex=0.8, bty="n", title="Concentracion", text.font=4, bg='lightblue')
  
  
  
  ############  
  #### VERSION 3  (lo mismo que la anterior pero poniendo la leyenda fuera)
  par(mar=c(5.4,3,2,1))
  
  
  #Por interseccion entre los shape files, me quedo solo con le hay dentro de la cuenca  (emplea la siguiente funcion de la libreria rgeos)
  inter = gIntersection(MyPoligonsRsub1, MyPPoligonsCsub1, byid=T, drop_not_poly=T)
  
  #Divido en parte para el mapa y parte para la leyenda (al fondo)
  layout(matrix(c(1,2),  byrow = TRUE),heights=c(14,2,1,1) )
  #El MAPA
  par(mar=c(2.2,2.5,2,0.2),mgp=c(1.5,0.5,0))   #mgp: 1 indice: distancia xlab; 2 indice: distancia numero ; 3index: dist eje
  caja<-bbox(inter)  #diferencia respecto al otro
  plot(caja[1,], caja[2,], type='n', xlab=expression(paste('Lon (', degree, ')'), sep='')
       , ylab=expression(paste('Lat (', degree, ')'), sep='') ,cex.axis=0.7,cex.lab=0.7 )
  expression(paste('Lon (', degree, ')'), sep='')
  
  plot(inter, col=colcode, add=T)
  
  title(main=paste0(nameIAgr," " , strWater[indexpress])  )
  # ,sub="Quantile (Equal-Frequency) Class Intervals")
  text(coordinates(MyPoligonsRsub1), labels=MyPoligonsRsub1@data$NUTS_ID, cex=1.0,col="green")   #los ID de las regiones
  
  #LA LEYENDA
  par(mar=c(0,0,0,0))   #changing the margins
  
  # plot(c(0,1),c(0,4),type = 'n', axes=F ,xlab = "", ylab = "", main = "") 
  plot.new()
  
  legend("center", legend=names(attr(colcode, "table")), 
         fill=attr(colcode, "palette"), cex=0.9, bty="n", title= " " , text.font=3,
         bg='lightblue',ncol=4)
  
  # text_title <- paste( style, " class intervals"," " , "concentracion" )    
  legend("center",legend= "",  title= text_legend , cex=1.1, bty='n', title.adj=0.17)  #segunda legend para titulo mas grande
  
  
}





























#######################################
### Map_WEI_QT_SPPLOT   YA NO LO USO
######################################

# catchNa <- catchName
# LstMySh <- LstMyShapes
# TheTable <- WEISumary
# StgtoPlot <- Estrategia1   #no incluyo el tipo de estrategia en esta
# nclr  <- NumClassInterv
# pal <- paleta
# style <- estilo
Map_WEI_QT_SPPLOT <- function (catchNa,LstMySh,TheTable,nclr,pal,style,verbose=TRUE){     
  
  # Lo primero cargar los shapefiles
  MyPoligonsRsub1_orig <- LstMySh[[1]]
  MyPLinesCsub1 <- LstMySh[[2]]   #el borde de la cuenca lo empleo en la primera version
  MyPPoligonsCsub1 <- LstMySh[[3]]  # el poligono de la cuenca lo empleo en la segunda version version
  
  # Queremos dibujar 4 mapas: Man, Min,  PS, SDW.
  #Anado los valores de los ratios de reduccion de las 4 variables (sectores) a mapear (que se han agregado en cada region) al shape del mapa
  str(MyPoligonsRsub1_orig@data)  #el shape
  
  #cambio los nombre de las columnas con los valores para que no den luego problemas
  colnames(TheTable) <- c("N2", "n", "Mean", "Median", "Q_75","Max", "Sum")
  
  # si quiero mapear los load (hago el logaritmo para tener una escala mas razonable)
  MyDFRsub1 <- merge(MyPoligonsRsub1_orig@data, TheTable, by.x ="NUTS_ID",by.y ="N2") #los esfuerzos by rownames. #junto los datos del espatialdataframe, con mi data frame de concentracion
  MyDFRsub1_lg<-MyDFRsub1
  # MyDFRsub1_lg[,5:8] <- log( MyDFRsub1[,5:8]+0.1 )
  #el merge lo junta bien, pero al desordenarlos, luego no se corresponden con el resto de atributos del mapa y pinta las cosas en otro nuts
  MyDFRsub1_lg <- MyDFRsub1_lg[order(match(MyDFRsub1_lg[,1],MyPoligonsRsub1_orig@data[,1])),]  #reordena en el mismo orden que en el objeto original
  MyPoligonsRsub1 <- MyPoligonsRsub1_orig #sobre una copia el original
  MyPoligonsRsub1@data <- MyDFRsub1_lg  #sobreescribo en el spatial poligon el DF de los data (por el que tiene las concentraciones pero esta desordenado) 
  
  # DOS PRUEBAS, para ir ajustando los graficos, con spplot 
  #   spplot(MyPoligonsRsub1, "Mean", main = "WEI agregated by average", 
  #       col = "transparent",colorkey=list(space="bottom") )
  #   spplot(MyPoligonsRsub1, "Max", main = "WEI agregated by Max", 
  #       col = "transparent")
  
  # Un ejemplo que se aproxima mas a lo que busco
  # plotclr.palette <- brewer.pal(n =nclr, name = pal)
  # allvarsvalues <- (unlist(MyPoligonsRsub1@data[,3:3]) )  #la media
  # breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
  # length(breaks.qt$brks)  #para que pinte bien el mayor de todos, el ultimo intervalo le anado algo
  # breaks.qt$brks[length(breaks.qt$brks)]<-breaks.qt$brks[length(breaks.qt$brks)]+breaks.qt$brks[length(breaks.qt$brks)]/100
  # 
  # plotclr.palette <- brewer.pal(n =nclr, name = pal)
  # 
  # spplot(MyPoligonsRsub1["Mean"],  main = "WEI agregated by average", 
  #        col = "transparent",
  #        colorkey=list(space="bottom"),
  #        col.regions=plotclr.palette,
  #        at = breaks.qt$brks )
  
  
  #quiero hacer un mapa de WEI, para cada metrica de agregacion, de acuerdo a la configuracion que se le pase a la funcion
  #por eso hago un loop y juego con los indices de las columnas
  
  nameMaps <- c("mean","median","75Q","Max","Sum")
  for (i in 3:7){     #3: mean, 4: median, 5: 75Q, 6: Max, 7: Sum
    plotclr.palette <- brewer.pal(n =nclr, name = pal)
    allvarsvalues <- (unlist(MyPoligonsRsub1@data[,i:i]) )  #selecciono solo una columna a la vez
    breaks.qt <- classIntervals( allvarsvalues, n = (nclr-1), style = style, intervalClosure = "right")
    #para que el ultimo intervalo no de problemas, le aumento algo al mayor valor 
    length(breaks.qt$brks)  
    breaks.qt$brks[length(breaks.qt$brks)]<-breaks.qt$brks[length(breaks.qt$brks)]+breaks.qt$brks[length(breaks.qt$brks)]/100
    
    plotclr.palette <- brewer.pal(n =nclr, name = pal)
    
    print(   spplot(MyPoligonsRsub1[,i],  main = paste0("WEI agregated by ", nameMaps[i-2]," (",style," breaks )"  ), 
                    col = "transparent",
                    colorkey=list(space="bottom"),
                    col.regions=plotclr.palette,
                    at = breaks.qt$brks )   )
  }
  
  
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
  # Subset_N2POL<-N2POL[N2POL$NUTS_ID==List_N2_ID,]
  names(N2POL)
  head(N2POL)
  Subset_N2POL<-N2POL[N2POL$NUTS_ID %in% List_N2_ID,]
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
  # Subset_N2POL<-N2POL[N2POL$NUTS_ID==List_N2_ID,]
  Subset_N2POL<-N2POL[N2POL$NUTS_ID %in% List_N2_ID,]
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
    Subset_N2Contornos<-N2Contor[N2Contor$NUTS_ID %in% List_N2_ID,]
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
  # class(CATCHPOL)
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
  
  Subset_N2POL<-N2POL[N2POL$NUTS_ID %in% List_N2_ID,]
  Subset_CATCHPOL<-CATCHPOL[CATCHPOL$hydroid %in% List_CATCH_ID,]
  
  #si le llegan los contornos de las provincias los filtra
  if(!is.null(N2Contor)) Subset_N2Contornos<-N2Contor[N2Contor$NUTS_ID %in% List_N2_ID,]
  
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

