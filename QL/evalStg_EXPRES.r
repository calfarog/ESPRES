################################################################################
# Author: Angel Udias     all complaints to: angel.udias-moinelo@ec.europa.eu  #
################################################################################
# Started: 21-Jan-2018 ->    ;                                                 #
# Updates:                                                                     #
#                                                                              #
# ESPRES: Efficient Strategies for anthropogenic Pressure Reduction in European waterSheds                                                                             #
#        MULTI OBJECTIVE OTPIMIZATION functions file                           #
#                                                                              #
# DESCRIPTION :                                                                #
# Funciones para evaluar las estrategias en lo relativo a Nuevas concentraciones y presiones    #
# 
#
#                                       #
#                                         #
#                                         #
#                      #
################################################################################



################################################################################################
################################################################################################
############################   EVAL STRATEGY ######################################################
################## REDUCTION CONCENTRATION AND NEW CONCENTRATIO BY SUBCATCHMENT ####################
#############################################################################################

# L_BLS <- BLScen 
# Mx_stg <-  stg_A_steg

# Mx_stg <- rep(0,length(myPareto$parameters[1,])) 
# L_BLS <- BLScen    #Escenario base, le hace solo falta por los nombres de los nuts
# Convierte una estrategia de formato vectorial a matricial 
DecodeStg_QL <- function(L_BLS,Mx_stg){
  dimx <- length(L_BLS$TheNuts_EF)
  My_strateg <- matrix(Mx_stg, byrow = TRUE, ncol = dimx)
  rownames(My_strateg) <- c("Man","Min","PS","SD")   #cada fila es un tipo de presion
  colnames(My_strateg) <- L_BLS$TheNuts_EF
  return(My_strateg)
}



# L_BLS <- BLScen
# Mx_stg <- BLSstg
# Mx_stg[pressure type,NUT] <- Mtx_strateg.  primer indice tipo de presion, segundo indice el nuts
# Calcula la Nueva concentracion en cada SUBCUENCA despues de aplicar LINEALMENTE una estrategia de reduccion
# tambien calcula la reduccion de Load en cada subcuenca despues aplicar reduccion
# En esta funcion se considera que las reducciones de presiones se aplican de forma lineal aditivo
# Las reducciones son el resultado de multiplicar la matriz de reducciones por la strategia
# Computacionalmente es la rutina que requiere (seguramente) mas esfuerzo
#   
EvalStg <- function(L_BLS,Mx_stg){


                    #version poco eficiente de la multiplicacion y la suma vectorial
                    # ptm <- proc.time()
                    # for (i in 1:20) {
                    # #ya multiplico la matriz por el vector
                    # df_reductions  <- data.frame(mapply(`*`, L_BLS$ConcReduct[3:length(colnames(L_BLS$ConcReduct))],Vc_stg2))  
                    # rowSums(df_reductions)
                    # }
                    # proc.time() - ptm
                    
                    # 
                    # m <- matrix(c(1,2,3,4,5,6),ncol=3)
                    # v <- c(2,4,6)
                    # m*v
                    # v*m
                    # m%*%v
                    # 
                    # df_kk  <- data.frame(mapply(`*`, data.frame(m),v))  
                    # 
                    # rowSums(df_kk)
  
  

  
    # # 1. Conversion de la matriz de reduccion de presiones (la estrategia) en un vector
    # Vc_stg <- c(t(Mx_stg))
    # #genero los nombres de cada reduccion: combinacion de NUTS y del tipo de pression
    # names(Vc_stg) <- paste0 ( colnames(Mx_stg) ,"_", rep(1:nrow(Mx_stg), each = length(Mx_stg)/nrow(Mx_stg), len =length(Mx_strateg))  )
    # #Ordeno los elementos del vector de acuerdo al orden de las columnas de la matriz de reducciones (para que la multiplicacion sea correcta)
    # strTemp2 <- colnames(L_BLS$ConcReduct)[3:length(colnames(L_BLS$ConcReduct))]  #elimino los dos primeros que son el HydroID y el year
    # Vc_stg2 <-Vc_stg[order(match(names(Vc_stg), strTemp2))]
    # 
    # 
    # #2. Eficiente MULTIPLICACION vectorial de la matriz por el vector (ya me da sumada la reduccion total de concentraciones)
    # df_reductions  <-  as.matrix(L_BLS$ConcReduct[3:length(colnames(L_BLS$ConcReduct))]) %*% Vc_stg2
    # 
    # #3. New Colum with the REDUCTIONS by subcatchment
    # L_BLS$DFConcentration$redconc <- as.numeric(df_reductions)  #VERIFICAR QUE AMBOS ESTAN EN LAS MISMAS POSICIONES (SEGURO QUE SI)
    # L_BLS$DFConcentration$newconc <- as.numeric( L_BLS$DFConcentration$Concentration - df_reductions)
   
#####
 
   
  # 1. Matriz vacia para reduccion de concentraciones (de la estrategia) en 4 vectores (uno por cada tipo de presion)
  reductions <- matrix(, nrow = 4, ncol = nrow(L_BLS$ConcReduct))  # nrow(L_BLS$ConcReduct) )      #the empty matrix (each row will be a tipe of preassures reduction)
  #   reductions[1:4,1:20]
  # 2. Matriz vacia para la reduccion de Load para la estrategia (multiplico reduccion 1% por typo de presion y Nut y luego sumo todos los Nuts y dejo agrupado por tipo sector)
  reductLoad <- matrix(, nrow = 4, ncol = nrow(L_BLS$ConcReduct))

  namesPres <- c("Man", "Min", "PS", "SD" )
  for (nr in  1:nrow(Mx_stg)){      #loop para cada tipo de presion: Manure, Mineral, PS, SDW, 
    # nr<-1
  #   print(nr)
    # 1.1  Vector de reducciones que en cada iteracion corresponde a un numero de presiones
    Vc_stg1 <- Mx_stg[nr, ]  #de esta forma conserva ya los nombres de las columnas
    
    # 1.2 Reduccion por cada tipo de presion (sectores)
       #subseting de la matriz de reducciones, para ese tipo de reduccion (ordenado adecuadamente)
      strTemSelect <- paste0( colnames(Mx_stg) ,"_", rep(namesPres[nr], length(Vc_stg1))  )
    
      SubsConcReduct <- L_BLS$ConcReduct[,colnames(L_BLS$ConcReduct) %in% strTemSelect]        #cuidado que deben estar ordenados igual para que la multiplicacion sea correcta
      SubsLoadReduct <- L_BLS$LoadReduct[,colnames(L_BLS$LoadReduct) %in% strTemSelect]    #tambien en este
     
      colnames(SubsLoadReduct)
      names(Vc_stg1)
    # 1.3. Eficiente MULTIPLICACION vectorial de la matriz por el vector (ya me da sumada la reduccion total de concentraciones)
     # reductions[nr,]  <-  as.matrix(SubsConcReduct[,]) %*% Vc_stg1
      if( dim(Mx_stg)[2]==1){                           #cuando hay solo 1 nuts
          reductions[nr,]  <-  SubsConcReduct * Vc_stg1 
          reductLoad[nr,]  <-  SubsLoadReduct * Vc_stg1 
      } 
      
      if( dim(Mx_stg)[2]>1){                         #las cuencas con mas de un NUT
        reductions[nr,] <- as.matrix(SubsConcReduct[,]) %*% Vc_stg1   
        reductLoad[nr,] <- as.matrix(SubsLoadReduct[,]) %*% Vc_stg1 
      }
      
    }
    #cada fila de reductions contiene reducciones para cada subcuenca por tipo de presion
  rownames(reductions) <- c("Man","Min","PS","SD")     
  colnames(reductions) <- L_BLS$DFConcentration$HydroID  #verificar que estan en el orden correcto
  rownames(reductLoad) <- c("Man","Min","PS","SD")     
  colnames(reductLoad) <- L_BLS$DFConcentration$HydroID   #un poco peligrosa esta operacion
  #  reductions[1:4,100:110]
  #  reductLoad[1:4,100:110]
  # dim(reductions)
  
  #anado dos columnas
  L_BLS$DFConcentration$redconc <- as.numeric(colSums(reductions))  #VERIFICAR QUE AMBOS ESTAN EN LAS MISMAS POSICIONES (SEGURO QUE SI)
  L_BLS$DFConcentration$newconc <- as.numeric( L_BLS$DFConcentration$Concentration - colSums(reductions))
  
  return (list(L_BLS$DFConcentration,reductions,reductLoad ) )  #devuelve DF con la suma concentraciones y las nuevas concentraciones; matriz de reduccion de concentraciones y matriz de reduccion de load (ambas por cada sucuenca y tipo) 
}






    #########################################################################
    #########################################################################
    ######################  CONCENTRATION  #####################################
    ####################################################################
    ######################################################################


# n_BLS <-  Evaluated_BLScen[[1]]   Eval_Scen[[1]]
# W <- "F": no pondero; "A", by area; "DA": By Drained Area
# Aplica diferentes tipos de metricas para devolver un valor para cada NUTS de la cuenca relativo al estado de las concentraciones
# Las metricas son mean, median, 3Q, max, treshold, sum. Existira la opcion de PONDERAR o no por el LONGITUD 
NutsConcFromSubc <- function(n_BLS,W="F"){

  #####
  ## 1. COPIA DEL DF CON LA INFORMACION

    myNewConcDF <- n_BLS
#  class(myNewConcDF)
    names(myNewConcDF)
    
  
  ####### 
  ## 1. SIN PONDERAR POR Area (aplicacion de las diferentes metricas de agrupamiento)
    #funcion para todos las metricas de agrupacion              #falta el treshold y la suma
  if(W=="F"){
    my.summary <- function(x, na.rm=TRUE){result <- c(n=as.integer(length(x)),
                                                      Mean=mean(x, na.rm=TRUE),     # SD=sd(x, na.rm=TRUE), 
                                                      Median=median(x,na.rm=TRUE), Q=quantile(x,0.75,na.rm=TRUE), 
                                                       Max=max(x,na.rm=TRUE), Sum=sum(x,na.rm=TRUE))}
  
    # library(plyr)  #para usar ddply
    ConcbyNUTS  <- ddply(myNewConcDF, c("Nuts2"), function(x) my.summary(x$newconc))

  }  
    
  ####### 
  ## 2. PONDERANDO POR Area (aplicacion de las diferentes metricas de agrupamiento)
    #hay varias dificultades, para los valores ponderados por Areas
    #para las medianas y quantiles, si hay valores decimales, no los considera
    #en nuestro caso hay muchas Areas menores que 1 y introduce fuerte error, por ello multiplicamos
    # todas las Areas por 100 y entonces se reduce mucho el error
    # empleo la libreria library(Hmisc)
    # Y el concepto de maximo es solo el valor de multiplicar
  if(W=="A"){
    my.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                         Mean=weighted.mean(x$newconc, x$Area, na.rm=TRUE),
                                                         median= wtd.quantile(x$newconc, 100*x$Area)[3] ,
                                                         Q=wtd.quantile(x$newconc, 100*x$Area)[4] ,
                                                         Max=max(x$newconc),
                                                         Sum= sum(x$newconc * x$Area) )}
      
      
    ConcbyNUTS <- ddply(myNewConcDF, c("Nuts2"), function(x) my.summaryW(x))
      
  } 

  ## 3. PONDERANDO POR Drained Area (  
    if(W=="DA"){
      my.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                         Mean=weighted.mean(x$newconc, x$DrainedArea, na.rm=TRUE),
                                                         median= wtd.quantile(x$newconc, 100*x$DrainedArea)[3] ,
                                                         Q=wtd.quantile(x$newconc, 100*x$DrainedArea)[4] ,
                                                         Max=max(x$newconc),
                                                         Sum= sum(x$newconc * x$DrainedArea)   )}
      
      
      ConcbyNUTS <- ddply(myNewConcDF, c("Nuts2"), function(x) my.summaryW(x))
      
  #   my.sumary2 <- function(x, na.rm=TRUE){result <- c(Mean=sum(x$newconc*x$Area, na.rm=TRUE),     # SD=sd(x, na.rm=TRUE), 
  #                                                      Median=median(x$newconc), Q=quantile(x$newconc,0.75), 
  #                                                     Min=min(x$newconc), Max=max(x$newconc),Suma=sum(x$newconc)  )}
      
      # library(plyr)  #para usar ddply
  #  ConcbyNUTS2 <- ddply(myNewConcDF, c("Nuts2"), function(x) my.sumary2(x))
      
  #   kk<- aggregate (myNewConcDF$Area, by=list(myNewConcDF$Nuts2),FUN=sum,na.rm=TRUE )
   
  #    ConcbyNUTS2$Mean/kk$x
      
      
  #    myNewConcDF$DrainedArea
  #    write.csv(myNewConcDF, file = "myNewConcDF.csv")
      
    } 
    
    
      
  ## 4. Area EN CADA NUTS (que tiene tramos de catchment), lo anadimos como columna a el DF anterior  
    TotAreaByNUTS <-aggregate(myNewConcDF$Area, by=list(myNewConcDF$Nuts2), FUN=sum, na.rm=TRUE)  #calculo del Area de cada NUT
    TotDrAreaByNUTS <-aggregate(myNewConcDF$DrainedArea, by=list(myNewConcDF$Nuts2), FUN=sum, na.rm=TRUE)  #calculo del Drained Area de cada NUT
    names(TotAreaByNUTS) <- c("Nuts2", "TotArea")   #nombre a las columnas
    names(TotDrAreaByNUTS) <- c("Nuts2", "TotDrainArea") 
    ConcbyNUTS_tem<- merge(ConcbyNUTS,TotAreaByNUTS , by=c("Nuts2") ) #los mezclo
    ConcbyNUTS_F<- merge(ConcbyNUTS_tem,TotDrAreaByNUTS , by=c("Nuts2") ) #los mezclo
    
    # getwd()
    # write.csv(ConcbyNUTS_F, file = "ConcSumary.csv")
    
           # CALCULO MEDIA PONDERADA ALTERNATIVA (menos eficiente)
      
                    # # si quiero hacer los calculos considerando la PONDERACION de Area
                    # myNewConcDF$newconcweighted <- myNewConcDF$newconc *  myNewConcDF$Area
                    # 
                    # # las operaciones son distintas (para la media sumo todos y divido por el Area total)
                    # agregnewConcWByNUTS <-aggregate(myNewConcDF$newconcweighted, by=list(myNewConcDF$Nuts2), FUN=sum, na.rm=TRUE)
                    # names(agregnewConcWByNUTS) <- c("Nuts2", "concWArea")
                    # agregAreaByNUTS <-aggregate(myNewConcDF$Area, by=list(myNewConcDF$Nuts2), FUN=sum, na.rm=TRUE)
                    # names(agregAreaByNUTS) <- c("Nuts2", "TotArea")
                    # ConcAgreAreabyNuts<- merge(agregnewConcWByNUTS,agregAreaByNUTS , by=c("Nuts2") )
                    # ConcAgreAreabyNuts$concWArea/ConcAgreAreabyNuts$TotArea
      
      
              #ANALISIS GRAFICO DE LA DISTRIBUCION DE CONCENTRACIONES DE ACUERDO AL Area 
      
                    # plot(myNewConcDF$Area,myNewConcDF$newconc)
                    # #Separo en dos grupos, Area grande y Area pequena.
                    # peque <-myNewConcDF[myNewConcDF$Area < 0.01,] 
                    # grande <- myNewConcDF[myNewConcDF$Area >= 1,]
                    # dim(peque)
                    # dim(grande)
                    # boxplot(peque$newconc,grande$newconc)
                    # boxplot(peque$newconc,grande$newconc,outline = F)
                    # #y de los supercontaminados
                    # highcont <-myNewConcDF[myNewConcDF$newconc > 50,] 
                    # dim(myNewConcDF)
                    # dim(highcont)
                    # boxplot(myNewConcDF$Area,highcont$Area)   #de los muy contaminados el Area es menor
                    # mean(myNewConcDF$Area)
                    # mean(highcont$Area)
      return(ConcbyNUTS_F)
      
}


# ST_N<- ConcbyNUT
# AgrIndex <- NutI
# W <- LengthPonderation
# En este se agrupa los valores de cada NUTS de acuerdo a todas las metricas
# Hay que tener en cuenta que dichos valores se pueden haber calculado con diferentes metricas y pesos
# Tambien aqui el calculo se hace con diferentes metricas y pesos, pero el usuario debe indicar tambien EN BASE A QUE METRICA
# de la primera agrupacion quiere que se realize esta nueva agrupacion
TotConcFromNuts <- function(ST_N,AgrIndex,W="T"){

  
  #####
  ## 1. COPIA DEL DF CON LA INFORMACION deseada (es decir NUT, el Area, y el indicador de concentracion base que desea usar)
  myNewConcDF <- ST_N[,c(1,AgrIndex,length(ST_N)-1, length(ST_N))] 
  names(myNewConcDF) <- c("Nuts2","newconc","Area","DrainArea")   #renombro las columnas para poder reutilizar el resto del codigo
  
  
  ####### 
  ## 2. SIN PONDERAR POR Area (aplicacion de las diferentes metricas de agrupamiento)
  #funcion para todos las metricas de agrupacion              #falta el treshold y la suma
  if(W=="F"){
    my.summary <- function(x, na.rm=TRUE){result <- c(n=as.integer(length(x)),
                                                      Mean=mean(x, na.rm=TRUE),     # SD=sd(x, na.rm=TRUE), 
                                                      Median=median(x), Q=quantile(x,0.75,na.rm=TRUE),
                                                      Max=max(x),
                                                      Sum=sum(x)
                                                      )}

    #solo lo tiene que aplicar una vez (no es por factores)
    ConcbyNUTS<- my.summary( myNewConcDF$newconc)
  }  
  
  ####### 
  ## 3. PONDERANDO POR Area (aplicacion de las diferentes metricas de agrupamiento)
  #hay varias dificultades, para los valores ponderados por Areas
  #para las medianas y quantiles, si hay valores decimales, no los considera
  #en nuestro caso hay muchas Areas menores que 1 y introduce fuerte error, por ello multiplicamos
  # ahora ya no es necesario multiplicar las Areas
  # empleo la libreria library(Hmisc)
  # Y el concepto de maximo es solo el valor de multiplicar
  if(W=="T"){
    my.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                       Mean=weighted.mean(x$newconc, x$Area, na.rm=TRUE),
                                                       median= wtd.quantile(x$newconc, x$Area)[3] ,
                                                       Q=wtd.quantile(x$newconc, x$Area)[4] ,
                                                       Max=max(x$newconc),
                                                       Sum= sum(x$newconc * x$Area) )}
    
    ConcbyNUTS<- my.summaryW( myNewConcDF)
  } 
  
  
  ## 4. AREA TOTAL. lo anadimos como ULTIMO VALOR  
  TotLong <-sum(myNewConcDF$Area)  #calcuo del Area de cada NUT
  ConcbyNUTS <- c(ConcbyNUTS,TotLong) #lo anado como ultimo elemento del vector
  names(ConcbyNUTS)[length(ConcbyNUTS)] <- c("TotArea")   #nombre 

  return(ConcbyNUTS)
  
}



# si no recibe el valor del threshold toma ese valor por defecto
# En este se agrupa los valores de las subcuencas de acuerdo a todas las metricas
# Tener en cuenta que no es lo mismo que las dos funciones anteriores ejecutadas en serie
# salvo para el total sumado
# n_BLS <- newSCEN[[1]] 
# W <- "F"
# thresh <- threshold
TotConcFromSubc <- function(n_BLS,W="T",thresh=5){
 

  print(thresh)
  #####
  ## 1. COPIA DEL DF CON LA INFORMACION deseada (es decir NUT, el Area, y el indicador de concentracion base que desea usar)
  myNewConcDF <- n_BLS

  ####### 
  ## 2. SIN PONDERAR POR Area (aplicacion de las diferentes metricas de agrupamiento)
  #funcion para todos las metricas de agrupacion              #falta el treshold y la suma
  if(W=="F"){
    my.summary <- function(x, na.rm=TRUE){result <- c(n=as.integer(length(x)),
                                                      Mean=mean(x, na.rm=TRUE),     # SD=sd(x, na.rm=TRUE), 
                                                      Median=median(x), Q=quantile(x,0.75,na.rm=TRUE), 
                                                      Max=max(x)  ,
                                                      Thr= sum(x[x>thresh]) ,
                                                      Sum= sum(x) )}    
    
#    thresh<-14
#   sum(  myNewConcDF$newconc[myNewConcDF$newconc>thresh] ) #suma de los valores que superan el umbral
    
    #solo lo tiene que aplicar una vez (no es por factores)
    ConcbyNUTS<- my.summary( myNewConcDF$newconc)
  }  

  ####### 
  ## 3. PONDERANDO POR Area (aplicacion de las diferentes metricas de agrupamiento)
  #hay varias dificultades, para los valores ponderados por Areas
  #para las medianas y quantiles, si hay valores decimales, no los considera
  #en nuestro caso hay muchas Areas menores que 1 y introduce fuerte error, por ello multiplicamos
  # ahora vuelve a ser necesario multiplicar las Areas por 100, para evitar cuando la subcuenca es muy pequena
  # empleo la libreria library(Hmisc)
  # El maximo resultante de la multiplicacion, no de el valor por si mismo, se podria dificir a posteriory
  # La suma resultante de multiplicar, pero sin dividir por la suma de longitudes
  if(W=="T"){
    my.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                       Mean=weighted.mean(x$newconc, x$Area, na.rm=TRUE),
                                                       median= wtd.quantile(x$newconc, 100*x$Area)[3] ,
                                                       Q=wtd.quantile(x$newconc, 100*x$Area)[4] ,
                                                       Max=max(x$newconc*x$Area) ,
                                                       Thr= sum(x$newconc[x$newconc>thresh]*x$Area[x$WEI>thresh]),
                                                       Sum= sum(x$newconc * x$Area)  )}
    
    ConcbyNUTS <- my.summaryW(myNewConcDF)
  } 
  
####jemplo 1
        # median(c(1.5,1.5,1.5,1.5,2.3,3,6,9))
        # wtd.quantile(c(1.5,2.3,3,6,9), c(4,1,1,1,1))[3]
        # wtd.quantile(c(1.5,2.3,3,6,9), c(4,0.9999,0.9999,0.9999,0.9999))[3]
        # wtd.quantile(c(1.5,2.3,3,6,9), 100*c(4,0.9999,0.9999,0.9999,0.9999))[3]
        # 
        # median(c(1.5,1.5,1.5,1.5, 7, 8.1, 8.4, 9))
        # wtd.quantile(c(1.5,7, 8.1, 8.4, 9), c(4,1,1,1,1))[3]
        # median(c(1.5,1.5,1.5,1.5, 7, 8.1, 8.4))
        # wtd.quantile(c(1.5,7, 8.1, 8.4, 9), c(4,1,1,1,0.01))[3]
        # wtd.quantile(c(1.5,7, 8.1, 8.4, 9), 100*c(4,1,1,1,0.01))[3]
  
  
  ## 4. LONGITUD TOTAL. lo anadimos como ULTIMO VALOR  
  TotLong <-sum(myNewConcDF$Area)  #calculo de la longitud de cada NUT
  ConcbyNUTS <- c(ConcbyNUTS,TotLong) #lo anado como ultimo elemento del vector
  names(ConcbyNUTS)[length(ConcbyNUTS)] <- c("TotArea")   #nombre 
  
  
  return(ConcbyNUTS)
  
}






      #########################################################################
      #########################################################################
      ######################  EFFORT  #####################################
      ####################################################################
      ######################################################################



# El esfuerzo lo evaluamos mediante la cantidad de LOAD (de N) no aplicado.
# No hacemos ningun tipo de metrica, simplemente sumamos (Por NUTS y Sectores y como suma total)

# Para el calculo solo necesita un Escenario Evaluado (donde esta la situacion del BLS y la situacion despues de aplicar la STG)
# Scen <- Evaluated_BLScen    
NutsSectEffortFromSubc <- function(Scen){
  
  myNewReductions <- as.data.frame(t(Scen[[3]])) 
  #hago el merge para saber a que NUT pertenece casa subcuenca
  # head(Scen[[3]])
  # head(myNewReductions)
  myReductions <- merge(Scen[[1]],myNewReductions, by.x="HydroID",by.y=0 , all=TRUE)
  # head(myReductions)
  
  # agrego ya por nut y tipo de presion anadiendo el area al final
  LoadByNut_Press <- aggregate(myReductions[,c(15,16,17,18,9)], by=list(Category=myReductions$Nuts2), FUN=sum)
  
  latabla <- data.frame(table(myReductions$Nuts2))  #para anadir una columna con el numero de datos
  
  output <- merge(latabla,LoadByNut_Press,by.x="Var1",by.y="Category"  )
  colnames(output)[1:2] <- c("Nuts2","n")  #renombro las dos primeras columnas para que se mantenga estructura

  
  return(output)
}


#Esta funcion llamara a NutsSectEffortFromSubc  y hara una suma por columnas
# Scen <- Evaluated_BLScen  
TotEffortFromSubc <-  function(Scen){
  
  TheEffortByNuts <-NutsSectEffortFromSubc(Scen)
  # Suma todas las columnas y listo
  output <- colSums( TheEffortByNuts[2:7]  )
  
  return(output)
}








# BLS <- BLScen   
# Concval <- MedianConc
# thePareto <- myParetoPesosIg
# Funcion para encontrar la estrategia de un frente de pareto mas proxima a un valor concentracion
closestParetStrategByConcentration <- function(BLS,thePareto,Concval){ 
  
  #Busca la estrategia que este mas cerca (min) del valor de esfuerzo
  IndexClosest <- which(abs(thePareto$objectives[,2]-Concval)==min(abs(thePareto$objectives[,2]-Concval))) 
  
  #DECODIFICAR LAS ESTRATEGIAS A FORMATO MATRICIAL
  # StgBLS <- DecodeStg (BLS, rep(0,length(thePareto$parameters[1,]))  )  #una estrategia en la que no se reduce nada
  StgMedian <- DecodeStg_QL (BLS, thePareto$parameters[IndexClosest[1],] )  #le anado el 1 por si hay mas de un punto a la misma distancia
  
  return(StgMedian)
}


# thedir<-thepath
Analisis_Sensibilidad <- function(thedir,theScen,MaxRateRed,numMaxIter){ 
  
  ### Codigo para realizar el calculo de sensibilidad para una cuenca
  
  #data frame para ir guardando solucion iteraciones
  DFSensiS <- data.frame(typosen =character() ,SumaMan = numeric(0), Min = numeric(0),PS= numeric(0),StD= numeric(0) ,stringsAsFactors = FALSE ) 

  for (i in 1:numMaxIter){
    PesosEffortlabel <- c("MineralAgr","ManureAgr","ScatterDwe","PointSou")
    PonderEfforRawVal <- c(20,20,20,20)    #Ponderacion Base (TODOS IGUAL IMPORTANCIA)
    PonderEffor <- PonderEfforRawVal/sum(PonderEfforRawVal)
    # primer parametro Concentracion agregation metric =>  #1:media ; 2:median;  3:3quantile; 4:max ; 5: Treshod (suma de lo que sobrepasa ese valor limite ; 6: suma de todas las concentraciones
    indexAgrM <- c(2,2.1)   #Segundo parametro, el umbral (solo es necesario si se escoge el tipo 5 de agregacion)
    signOb <- c(1,1)   #positivo si es minimizar y negativo si es maximizar (en este caso queremos minimizar ambos)
    genParam <- c(2,40,40)   #  nobject<-2 ; popSize=60 ; generations=40
    myConfig <- list(PonderEffor,indexAgrM,signOb,genParam)
    
    ## Lanza la optimizacion   
    myParetoPesosIg <-  MOO_GA_QL(theScen,MaxRateRed, myConfig) 
    
    #UN PARET0 concreto (en un fichero o en pantalla)
    Plot_Pareto(thedir,myParetoPesosIg,"F","F","median")  #the second parameter: T: create a jpeg file.  F: don't create a jpeg file. El tercer parametro, es el rescalado a 0-1. El ultimo parametro es la escala de las concentraciones
    
    #calculamos la mediana de las concentraciones de los puntos del pareto (COMO VALOR cercano a reducir a la mitad la contaminacion)
    MedianConc <- median(myParetoPesosIg$objectives[,2])
    
    # Buscamos la estrategia (el punto del pareto) mas cercano a ese valor de concentracion mediana
    myStg_PesosIg <- closestParetStrategByConcentration(theScen,myParetoPesosIg,MedianConc)
    MyEval_PesosIg <- EvalStg (theScen,myStg_PesosIg) 
    TotConcFromSubc(MyEval_PesosIg[[1]] , W="F")  #verifica que esa solucion de una concentracion proxima
    #para esa estrategia quiero un valor de reducciones totales para cada sector (empleo la suma como agregador)
    MyEffort_PesosIg <- NutsSectEffortFromSubc (MyEval_PesosIg)
    Reductions_bySectIg <- colSums(MyEffort_PesosIg[,3:(length(MyEffort_PesosIg)-1)] ) 
    
    # DFSensiBLS[nrow(DFSensiBLS)+1,] <- Reductions_bySectIg  #en lugar de rbind con esto no pierdo los nombres
    #   Reductions_bySectIter <- colSums(MyEffort_PesosIter[,3:(length(MyEffort_PesosIter)-1)] )
    DFSensiS <- rbind(DFSensiS, data.frame("typosen" = "BLS",t(Reductions_bySectIg)) )
  } 
  
  sensilits <- c("Man","Min","Ps","Std")
  
  for (tipo in 1:4){ 
    PonderEfforRawVal <- c(20,20,20,20)
    PonderEfforRawVal[tipo] <- 40
    
    PonderEffor <- PonderEfforRawVal/sum(PonderEfforRawVal)
    myConfig <- list(PonderEffor,indexAgrM,signOb,genParam)
    
    for (i in 1:numMaxIter){      ###### Modifico el peso de uno de los sectores
      myParetoPesosIter <-  MOO_GA_QL(theScen,MaxRateRed, myConfig) 
      Plot_Pareto(thedir,myParetoPesosIter,"F","F","median")  #the second parameter: T: create a jpeg file.  F: don't create a jpeg file. El tercer parametro, es el rescalado a 0-1. El ultimo parametro es la escala de las concentraciones
      MedianConc <- median(myParetoPesosIter$objectives[,2])      #calculamos la mediana de las concentraciones 
      
      # Buscamos la estrategia (el punto del pareto) mas cercano a ese valor de concentracion mediana
      myStg_PesosIter <- closestParetStrategByConcentration(theScen,myParetoPesosIter,MedianConc)
      MyEval_PesosIter <- EvalStg (theScen,myStg_PesosIter) 
      TotConcFromSubc(MyEval_PesosIter[[1]] , W="F")  #verifica que esa solucion de una concentracion proxima
      #para esa estrategia quiero un valor de reducciones totales para cada sector (empleo la suma como agregador)
      MyEffort_PesosIter <- NutsSectEffortFromSubc (MyEval_PesosIter)
      Reductions_bySectIter <- colSums(MyEffort_PesosIter[,3:(length(MyEffort_PesosIter)-1)] ) 
      
      #   Reductions_bySectIter<-data.frame("typosen" = sensilits[tipo],t(Reductions_bySectIter))  #le anado columna con el tipo
      
      DFSensiS <- rbind(DFSensiS, data.frame("typosen" = sensilits[tipo],t(Reductions_bySectIter)) )
      #DFSensiS[nrow(DFSensiS)+1,] <- Reductions_bySectIter  #en lugar de rbind con esto no pierdo los nombres
    }
  }
  
 # getwd()
  nombreCuenca <- basename(thedir) 
  namefile <-paste0("Sensitivity",nombreCuenca,".csv")
  write.csv(DFSensiS, file = paste0(thedir,"/",namefile) )  
  
  
}























# BLS <- BLScen
# EffortVal <- valEffort
# thePareto <- myPareto
# Funcion para encontrar la estrategia de un frente de pareto mas proxima a un valor de esfuerzo
closestParetStrategByEffort <- function(BLS,thePareto,EffortVal){ 
  
  #Busca la estrategia que este mas cerca (min) del valor de esfuerzo
  IndexClosestEffort <- which(abs(thePareto$objectives[,1]-EffortVal)==min(abs(thePareto$objectives[,1]-EffortVal))) 
  
  #DECODIFICAR LAS ESTRATEGIAS A FORMATO MATRICIAL
  # StgBLS <- DecodeStg (BLS, rep(0,length(thePareto$parameters[1,]))  )  #una estrategia en la que no se reduce nada
  StgMedian <- DecodeStg_QL (BLS,  thePareto$parameters[IndexClosestEffort,] )
  
  
  return(StgMedian)
}








# BLS <- BLScen
# myPto <- myParetoMedian
# A esta funcion se le manda el Pareto estimado con alguna de las metrica y el estima los paretos estimados 
# con las otras metricas
## Itero entre todas las estrategia del frente de pareto de una solucion y las evaluo de acuerdo a los otros objetivos
ParetoAllMetrics <- function(myPto,BLS){
  
  #volvemos a generarlas todas en base a esa estrategia
  # loop para cada estrategia, los quiero evaluar de acuerdo a las otras metricas
  df_plot<-NULL    #data frame donde guardo todas las evaluaciones
  for (index_st in 1:nrow(myPto$parameters)){
    #Decodifica
    # index_st<-1
    
    #Decodifica la estrategia
    My_strateg <- DecodeStg_QL (BLS, myPto$parameters[index_st,] ) 
    
    # Evalua la estrategia 
    stgStatus <- EvalStg (BLS,My_strateg)
    
    ##  OBJETIVO EFFORT TO REDUCE THE PRESURES
    weightBYlength <-"T"    #T: true, F: false
    temp <- TotEffortFromSubc(stgStatus) #Implementado con la version que lo calcula en base a las subcuencas Para TODAS LAS METRICAS
    Effort <- sum(temp[2:5])   #me devuelve el esfuerzo por tipo de presion, luego le pondremos la ponderacion. Por ahora todos igual, las sumo
    
    ## 2.2 OBJETIVO CONCENTRACIONES (varias posiblilidades de agrupacion)
    threshold<-20  # si no le pasamos el umbral, toma un valor por defecto
    weightBYlength <-"F"    #T: true, F: false
    stg_Concentr <- TotConcFromSubc (stgStatus[[1]] ,weightBYlength)  #tambien hay varias formas, empleamos la que lo hace en base a las subcuencas
    
    df_plot = rbind(df_plot, data.frame(t(c(Effort=Effort, stg_Concentr[c(2:4,6)])) ))
  }
  
  return(df_plot)
}







##  REDUCCIONES ES EQUIVALENTE A ESFUERZO (MIENTRAS NO SE PONDERE)
# Aplica diferentes tipos de metricas para devolver un valor para cada NUTS de la cuenca relativo al estado de las concentraciones
# Las metricas son mean, median, 3Q, max, treshold, sum. Existira la opcion de PONDERAR o no por el LONGITUD 
# Por ahora devuelve la SUMA como evaluacion
# Scen <- myStg_Evaluated
# W <- weightBYlength
NutsSectEffortFromSubc_OLD <- function(Scen,W="T"){
  
  #Cada tipo de presion lo proceso de una forma distinta. Min, Man, SD, PS  (por filas)
  
  myNewReductions <- as.data.frame(t(Scen[[2]])) 
  #  myNewReductions[1:10,]
  #hago el merge para que tengan areas y Nuts
  head(Scen[[1]])
  head(myNewReductions)
  myReductions <- merge(Scen[[1]],myNewReductions, by.x="HydroID",by.y=0 , all=TRUE)
  
  
  ####### 
  ## 2. SIN PONDERAR POR Area (aplicacion de las diferentes metricas de agrupamiento)
  #funcion para todos las metricas de agrupacion              #falta el treshold y la suma
  if(W=="F"){
    print(W)
    my.summary <- function(x, na.rm=TRUE){result <- c(n=as.integer(length(x)),
                                                      Mean=mean(x, na.rm=TRUE),     # SD=sd(x, na.rm=TRUE), 
                                                      Median=median(x), Q=quantile(x,0.75,na.rm=TRUE), 
                                                      Min=min(x), Max=max(x),Suma=sum(x)  )}
    
    # library(plyr)  #para usar ddply
    PressMinbyNUTS <- ddply(myReductions, c("Nuts2"), function(x) my.summary(x$Min))
    PressManbyNUTS <- ddply(myReductions, c("Nuts2"), function(x) my.summary(x$Man))
    PressSDWbyNUTS <- ddply(myReductions, c("Nuts2"), function(x) my.summary(x$SD))
    PressPSbyNUTS <- ddply(myReductions, c("Nuts2"), function(x) my.summary(x$PS))
    
  }  
  
  ####### 
  ## 3. PONDERANDO POR Area (aplicacion de las diferentes metricas de agrupamiento)
  #hay varias dificultades, para los valores ponderados por Areas
  #para las medianas y quantiles, si hay valores decimales, no los considera
  #en nuestro caso hay muchas Areas menores que 1 y introduce fuerte error, por ello multiplicamos
  # todas las Areas por 100 y entonces se reduce mucho el error
  # empleo la libreria library(Hmisc)
  # Y el concepto de maximo es solo el valor de multiplicar
  if(W=="T"){
    print(W)
    Min.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                        Mean=weighted.mean(x$Min, x$Area, na.rm=TRUE),
                                                        median= wtd.quantile(x$Min, 100*x$Area)[3] ,
                                                        Q=wtd.quantile(x$Min, 100*x$Area)[4] ,
                                                        Max=max(x$Min), Suma=sum(x$Min*x$Area )   )}
    
    PressMinbyNUTS <- ddply(myReductions, c("Nuts2"), function(x) Min.summaryW(x))
    
    Man.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                        Mean=weighted.mean(x$Man, x$Area, na.rm=TRUE),
                                                        median= wtd.quantile(x$Man, 100*x$Area)[3] ,
                                                        Q=wtd.quantile(x$Man, 100*x$Area)[4] ,
                                                        Max=max(x$Man), Suma=sum(x$Man*x$Area )   )}
    
    PressManbyNUTS <- ddply(myReductions, c("Nuts2"), function(x) Man.summaryW(x))
    
    SD.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                        Mean=weighted.mean(x$SD, x$Area, na.rm=TRUE),
                                                        median= wtd.quantile(x$SD, 100*x$Area)[3] ,
                                                        Q=wtd.quantile(x$SD, 100*x$Area)[4] ,
                                                        Max=max(x$SD), Suma=sum(x$SD*x$Area )   )}
    
    PressSDWbyNUTS <- ddply(myReductions, c("Nuts2"), function(x) SDW.summaryW(x))
    
    PS.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                       Mean=weighted.mean(x$PS, x$Area, na.rm=TRUE),
                                                       median= wtd.quantile(x$PS, 100*x$Area)[3] ,
                                                       Q=wtd.quantile(x$PS, 100*x$Area)[4] ,
                                                       Max=max(x$PS), Suma=sum(x$PS*x$Area )   )}
    
    PressPSbyNUTS <- ddply(myReductions, c("Nuts2"), function(x) PS.summaryW(x))
    
  } 
  
  ###############################################
  #por ahora para la reduccion total de presiones uso la SUMA, DEBERIA PONDERAR TAMBIEN
  ##################################################
  PresReductTotbyNUTS <- cbind(PressMinbyNUTS[,1:2],SumaMan=PressManbyNUTS$Suma,SumaMin=PressMinbyNUTS$Suma,SumaPS=PressPSbyNUTS$Suma,SumaSD=PressSDWbyNUTS$Suma)
  
  
  ## 4. Area EN CADA NUTS (que tiene tramos de catchment), lo anadimos como columna a el DF anterior  
  TotAreahByNUTS <-aggregate(myReductions$Area, by=list(myReductions$Nuts2), FUN=sum, na.rm=TRUE)  #calcuo del Area de cada NUT
  names(TotAreahByNUTS) <- c("Nuts2", "TotArea")   #nombre a las columnas
  PresReductTotbyNUTS_F<- merge(PresReductTotbyNUTS,TotAreahByNUTS , by=c("Nuts2") ) #los mezclo
  
  
  return(PresReductTotbyNUTS_F)
  
}


# Scen <- newSCEN
# W <- weightBYlength
TotEffortFromSubc_OLD <- function(Scen,W="T"){
  
  #Cada tipo de presion lo proceso de una forma distinta.  Man,Min, PS, SD,   (por filas)
  ptm <- proc.time()
  
  myNewReductions <- as.data.frame(t(Scen[[2]])) 
  #  myNewReductions[1:10,]
  #hago el merge para que tengan areas y Nuts
  myReductions <- merge(Scen[[1]],myNewReductions, by.x="HydroID",by.y=0 , all=TRUE)
  
  
  
  ####### 
  ## 2. SIN PONDERAR POR Area (aplicacion de las diferentes metricas de agrupamiento)
  #funcion para todos las metricas de agrupacion              #falta el treshold y la suma
  if(W=="F"){
    #   print(W)
    
    
    my.summary <- function(x, na.rm=TRUE){result <- c(n=as.integer(length(x)),
                                                      Mean=mean(x, na.rm=TRUE),     # SD=sd(x, na.rm=TRUE), 
                                                      Median=median(x, na.rm=TRUE), Q=quantile(x,0.75, na.rm=TRUE), 
                                                      Max=max(x, na.rm=TRUE),Suma=sum(x)  )}
    
    #solo lo tiene que aplicar una vez (no es por factores)
    PressMinbyNUTS <- my.summary( myReductions$Min)
    PressManbyNUTS <- my.summary( myReductions$Man)
    PressSDWbyNUTS <- my.summary( myReductions$SD)
    PressPSbyNUTS <- my.summary( myReductions$PS)
    
    
  }  
  
  ####### 
  ## 3. PONDERANDO POR Area (aplicacion de las diferentes metricas de agrupamiento)
  #hay varias dificultades, para los valores ponderados por Areas
  #para las medianas y quantiles, si hay valores decimales, no los considera
  #en nuestro caso hay muchas Areas menores que 1 y introduce fuerte error, por ello multiplicamos
  # todas las Areas por 100 y entonces se reduce mucho el error
  # empleo la libreria library(Hmisc)
  # Y el concepto de maximo es solo el valor de multiplicar
  if(W=="T"){
    #    print(W)
    
    
    Min.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                        Mean=weighted.mean(x$Min, x$Area, na.rm=TRUE),
                                                        median= wtd.quantile(x$Min, 100*x$Area)[3] ,
                                                        Q=wtd.quantile(x$Min, 100*x$Area)[4] ,
                                                        Max=max(x$Min, na.rm=TRUE), Suma=sum(x$Min*x$Area, na.rm=TRUE )   )}
    
    PressMinbyNUTS <- Min.summaryW(myReductions)
    
    Man.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                        Mean=weighted.mean(x$Man, x$Area, na.rm=TRUE),
                                                        median= wtd.quantile(x$Man, 100*x$Area)[3] ,
                                                        Q=wtd.quantile(x$Man, 100*x$Area)[4] ,
                                                        Max=max(x$Man), Suma=sum(x$Man*x$Area )   )}
    
    PressManbyNUTS <- Man.summaryW(myReductions)
    
    SD.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                        Mean=weighted.mean(x$SD, x$Area, na.rm=TRUE),
                                                        median= wtd.quantile(x$SD, 100*x$Area)[3] ,
                                                        Q=wtd.quantile(x$SD, 100*x$Area)[4] ,
                                                        Max=max(x$SD), Suma=sum(x$SD*x$Area )   )}
    
    PressSDWbyNUTS <- SDW.summaryW(myReductions)
    
    PS.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                       Mean=weighted.mean(x$PS, x$Area, na.rm=TRUE),
                                                       median= wtd.quantile(x$PS, 100*x$Area)[3] ,
                                                       Q=wtd.quantile(x$PS, 100*x$Area)[4] ,
                                                       Max=max(x$PS), Suma=sum(x$PS*x$Area )   )}
    
    PressPSbyNUTS <- PS.summaryW(myReductions)
    
    
    
  } 
  
  
  ###############################################
  #por ahora para la reduccion total de presiones uso la SUMA
  ##################################################
  PresReductTotbyNUTS <- c(PressMinbyNUTS[1],PressManbyNUTS[6],PressMinbyNUTS[6],PressPSbyNUTS[6],PressSDbyNUTS[6] )
  names(PresReductTotbyNUTS) <- c("n","SumaMan","SumaMin","SumaPS","SumaSD")
  
  
  ## 4. Total area en la cuenca
  TotArea <-sum(myReductions$Area)  #calculo de la area total
  PresReductTotbyNUTS <- c(PresReductTotbyNUTS,TotArea) #lo anado como ultimo elemento del vector
  names(PresReductTotbyNUTS)[length(PresReductTotbyNUTS)] <- c("TotArea")   #nombre 
  
  
  print(paste("evalEffort ",(proc.time() - ptm)[3] ))
  
  return(PresReductTotbyNUTS)
  
}




# L_BLS <- ListScen
# Mx_stg <- List_strateg
# Calcula EL ESFUERZO para aplicar la reduccion de presiones. 
# Relacionado con la cantidad de Nitratos que se dejan de introducir en el sistema
# Los agricolas: Mineral & Manure: son proporcionales al Area de cada subcuenca
# Los de PS
# Los de SD:
# LORO, ES SIMILAR AL ANTERIOR, SE PODRAN REUTILIZAR, MEZCLAR
Eval_PressRed_old <- function(L_BLS,Mx_stg){
  
  #construyo el vector de reducciones en el mismo orden que estan las columnas del DF
  
  strTemp2 <- colnames(L_BLS$ConcReduct)[3:length(colnames(L_BLS$ConcReduct))]  #elimino los dos primeros que son el HydroID y el year
  # strTemp3 <- matrix(unlist( strsplit(strTemp2, "_") ), ncol=2, byrow=TRUE)  [,1] # separo el nombre del nuts del id de la presion
  
  myvect <- unlist(Mx_stg)
  
  length(Mx_stg) #la dimension principal
  length(myvect)
  length(myvect)/length(Mx_stg)
  #modifico los nombres
  names(myvect) <- paste0 ( names(Mx_stg),"_", rep(1:length(Mx_stg), each =  length(myvect)/length(Mx_stg), len =length(myvect))  )
  
  #ya ordenados
  myvect2<-myvect[order(match(names(myvect), strTemp2))]
  
  #ya multiplico la matriz por el vector
  df_reductions  <- data.frame(mapply(`*`, L_BLS$ConcReduct[3:length(colnames(L_BLS$ConcReduct))],myvect2))  
  
  #tengo que sumar fila a fila
  return(rowSums(df_reductions))   #una reduccion de nitratos en cada subcuenca (cada una podria tener un coste diferente)
  #pero creo que interesa tener los valores sin poderar y se los puede ponderar luego
  
  
  #seguramente sea necesario una version multiplicando por Areas o similar ponderacion
  
}


