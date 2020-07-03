

################################################################################################
################################################################################################
############################   EVAL STRATEGY ######################################################
################## REDUCTION DEMANDAS DE AGUA BY SUBCATCHMENT ####################
#############################################################################################


# BLS <- BLScen
# EffortVal <- valEffort
# thePareto <- myPareto
# Funcion para encontrar la estrategia de un frente de pareto mas proxima a un valor de esfuerzo
closestParetStrategByEffort <- function(BLS,thePareto,EffortVal){ 
  
  #Busca la estrategia que este mas cerca (min) del valor de esfuerzo
  IndexClosestEffort <- which(abs(thePareto$objectives[,1]-EffortVal)==min(abs(thePareto$objectives[,1]-EffortVal))) 
  
  #DECODIFICAR LAS ESTRATEGIAS A FORMATO MATRICIAL
  # StgBLS <- DecodeStg (BLS, rep(0,length(thePareto$parameters[1,]))  )  #una estrategia en la que no se reduce nada
  StgMedian <- DecodeStg (BLS,  thePareto$parameters[IndexClosestEffort,] )
  
  
  return(StgMedian)
}



# L_BLS <- BLS 
# ThevectStg <- x
# Convierte una estrategia de formato vectorial a matricial (lo usa la rutina de optimzacion)
# si se elimina algun tipo de usos la rutina no funcioa bien
# el vector viene por filas 
DecodeStg <- function(L_BLS,ThevectStg){
  
 # The_N2 <- sort (as.character(unique(L_BLS$N2)))  #los N2 ordenados
  The_N2 <- sort(as.character(unique(BLScen[[6]]$N2))) #los N2 ordenados
  
  dimx <- length(The_N2)  #el numero de N2
  Mat_Stg <- matrix(ThevectStg, byrow = TRUE, ncol = dimx)
  rownames(Mat_Stg) <- c("Domest","Energ","Indust","Irrig","Livest")     #cada fila es un tipo de presion
  colnames(Mat_Stg) <- The_N2   #los ID de las N2 correctamente ordenados
  return(Mat_Stg)
}



# BLscen <- BLScen      la evaluacion de una estrategia siempre se hace en relacion con el BLS, es decir devuelve el nuevo escenario en relacion al BLS y a la estrategia
# strateg <- My_strateg    My_strateg2
# La evaluacion lo que hace es reducir las demandas en los porcentajes correspondientes
# Devuelve dos objetos:
#  1) DF con las nuevas DEMANDAS (despues de aplicar la estrategia de reducciones)
#  2) DF con las reducciones en las demandas 
EvalStg <- function(BLscen,strateg){


#### 1. Nuevas DEMANDAS acumuladas (routing) una vez aplicada la strategia de reduccion.

  BLscenCopy <- BLscen[1:5]  #hace una copia de la lista de las demandas acumuladas (no del fresW)
      # Multiplicar cada tipo de demandas por su reduccion correspondiente. Con eso tengo las nuevas demandas ya reducidas en varias columnas (la demanda correspondiente a cada NUTS), sumo por fila y tengo la demanda total en cada subcuenca
  listToReduce<-list()   #   BLscenCopy[[tipoWD]]
  for (tipoWD in 1:5){
   #  tipoWD <-1     #emplea cbind para anadir la columna suma porque es una matriz
    listToReduce[[(tipoWD)]]<- data.frame (cbind(BLscenCopy[[tipoWD]] , NewDemand=colSums( (t(BLscenCopy[[tipoWD]]))*(100 - strateg[(tipoWD),])/100  )))
    listToReduce[[(tipoWD)]]$rnames  <- rownames(listToReduce[[(tipoWD)]])
    listToReduce[[(tipoWD)]] <- listToReduce[[(tipoWD)]][,c("NewDemand","rnames" )]
    #los pasos anteriores los hago un poco obligado (porque si no es data frame no hace bien el merge posterior)
 }

  total <- suppressWarnings(Reduce(function(dtf1, dtf2) merge(dtf1,dtf2,by.x="rnames", by.y="rnames", all.x = TRUE), listToReduce ) )
  total <- merge(BLscen[[6]],total,by.x="Row.names"  ,by.y="rnames") #le anado el fresW y el area
  total <- total[,c(1,3,5,6,7,8,9,2,4)]    # recolocamos las columnas (para que queden como originalmente)
  names(total)[3:8]  <- c("domestWD","energWD", "industWD","irrWD","livesWD","Area")  #pongo los nombres correctos
 # [[1]]: DomesticWD, [[2]]: Energy [[3]] IndustrialWD, [[4]]:IrrigationWD , [[5]]: LivestockWD ,
  
  new_demands <- total  #Lo guarda como las nuevas demandas


#### 2. Calculo de la REDUCCION de demandas acumuladas (routing) una vez aplicada la strategia de reduccion
      #Generamos una estrategia de BLS (nada de reduccion), para calcular las demandas acumuladas en ese caso de no reduccion
      dimx <- length(unique(BLscen[[6]]$N2)) #el numero de columnas de la matriz (una por cada NUTS)
      BLS_strateg <- matrix( c( rep(0,dimx),     # domestWD   
                               rep(0,dimx),      # Energ
                               rep(0,dimx),       # Indust
                               rep(0,dimx),      # Irrig
                               rep(0,dimx)),      # Livest
                            byrow = TRUE, ncol = dimx)
      rownames(BLS_strateg) <- c("Domest","Energ","Indust","Irrig","Livest")  #cada fila es un tipo de presion
      colnames(BLS_strateg) <- unique(BLscen[[6]]$N2)

      BLscenCopy <- BLscen[1:5]  #hace una copia, pero solo de las demandas (no del fresW)
      # Tengo que multiplicar cada tipo de demandas por su reduccion correspondiente. Con eso tengo las nuevas demandas ya reducidas en varias columnas (la demanda correspondiente a cada NUTS), sumo por fila y tengo la demanda total en cada subcuenca
      listToReduce<-list()   #   BLscenCopy[[tipoWD]]
      for (tipoWD in 1:5){
        #  tipoWD <-1     #emplea cbind para anadir la columna suma porque es una matriz
        listToReduce[[(tipoWD)]]<- data.frame (cbind(BLscenCopy[[tipoWD]] , NewDemand=colSums( (t(BLscenCopy[[tipoWD]]))*(100 - BLS_strateg[(tipoWD),])/100) ) )
        listToReduce[[(tipoWD)]]$rnames  <- rownames(listToReduce[[(tipoWD)]])
        listToReduce[[(tipoWD)]] <- listToReduce[[(tipoWD)]][,c("NewDemand","rnames" )]
        #los pasos anteriores los hago un poco obligado (porque si no es data frame no hace bien el merge posterior)
      }
    
      BLSDemands <- suppressWarnings(Reduce(function(dtf1, dtf2) merge(dtf1,dtf2,by.x="rnames", by.y="rnames", all.x = TRUE), listToReduce ) )
      BLSDemands <- merge(BLscen[[6]],BLSDemands,by.x="Row.names"  ,by.y="rnames") #le anado el fresW y el area
      BLSDemands <- BLSDemands[,c(1,3,5,6,7,8,9,2,4)]   # recolocamos las columnas (para que queden como originalmente)
      names(BLSDemands)[3:8]  <- c("domestWD","energWD", "industWD","irrWD","livesWD","Area")   #pongo los nombres correctos
  
    # ESto serian las demandas en el BLS, si a eso le resto las demandas despues de aplicar la reduccion, Tengo el valor de las reducciones acumuladas
      BLSDemands[3:7] <- BLSDemands[3:7]- new_demands[3:7]
      ReductionsRout <- BLSDemands

      
#### 3. Calculo de la REDUCCION de demandas NO ACUMULADAS Lo que deja de extraerse. Esto es el ESFUERZO       
      #Tomo las demandas si ruteo, le aplico la reduccion correspondiente a la estrategia, hago la diferencia y el resultado es el esfuerzo, por sectores y regiones
      # Las dos resultados anteriores dan el resultado por subcuenca. Este lo da por regiones, no necesito mas para el esfuerzo
      #  strateg[2,3] <- 50
      Reductions<- NULL
      if(length(BLscen)==7)   #si el BLScenario aun no tiene [[7]], es decir, la Demanda no Routing, no evalua aun esta reduccion (le falta informacion para ello)
          Reductions <- strateg/100 * BLscen[[7]]   # y las reducciones queda como Null (quizas mejor NA)
    
      
  return( list(new_demands, Reductions,  ReductionsRout) )     #un escenario con las nuevas demandas, y las reducciones en las demandas
  
}





# BLscen <- BLScen      la evaluacion de una estrategia siempre se hace en relacion con el BLS, es decir devuelve el nuevo escenario en relacion al BLS y a la estrategia
# strateg <- My_strateg    My_strateg2
# La evaluacion lo que hace es reducir las demandas en los porcentajes correspondientes
# Devuelve dos objetos:
#  1) DF con las nuevas DEMANDAS (despues de aplicar la estrategia de reducciones)
#  2) DF con las reducciones en las demandas
# 
EvalStg_old <- function(BLscen,strateg){
  
  head(BLscen)
  #una primera cosa que tengo que hacer es reordenar las columnas del BLS, tienen que estar en el mismo orden de las estrategias
  BLscen <-  BLscen[, c(1,2,3,5,4,7,6,8,9 )]   #row.names fresW domestic energ inds  irrig   livest Area N2
  head(strateg)
  T_strg <- t(strateg)
  
  #verificar que son los mismos NUTS en ambas
#  BLscen[384,]
  BLscen[is.na(BLscen$N2), ]
  diferencias <- setdiff(as.character (unique(BLscen$N2)), row.names(T_strg) )
  if ( !identical(diferencias, character(0))  )    {warning("DISTINTA lista de NUTS") } #si la diferencia no es nula, algo falla
          
  new_demands <-BLscen[!(is.na(BLscen$N2)), ]
#  new_demands <- BLscen
  #Loop para multiplicar por nuts
  for (nu in unique(new_demands$N2) ){
      # nu<-101
      #  new_demands [new_demands $N2==nu,]  
      #  T_strg[rownames(T_strg)==nu, ]
      temp <- t(new_demands[new_demands$N2==nu,3:7]) * ((100 - T_strg[rownames(T_strg)==nu, ])/100)    #multiplicacion elemento a elemento
      new_demands[new_demands$N2==nu,3:7] <- t(temp)   #la traspongo para meterla en las celdas correspondientes del data frame 
  }
  
  #calcula tambien la diferencia entre Demandas del BLS y las Demandas despues de aplicada la estrategia de reduccion 
  stg_Reduct <-BLscen[!(is.na(BLscen$N2)), ]
#  stg_Reduct <- BLscen
  stg_Reduct[,3:7] <- stg_Reduct[,3:7]  - new_demands[,3:7] 
  
  
  return( list(new_demands,stg_Reduct) )     #un escenario con las nuevas demandas, y las reducciones en las demandas
}


# Funcion que calcula el WEI total de cada subcuenca, suma todas las demandas y hace la division entre demandas y fres
# TheScen <- Evaluated_BLScen[[1]]
# ListUsos <- ListWDemToWEI
Calcula_WEI <- function(TheScen,ListUsos){

  #aunque por definicion para calcular el WEI deberian estar todos los usos, yo implemento la opcion de no incluir algunos
  TodosUsos <- c("Domestic", "Energy", "Industrial", "Irrigation", "Livestock")
  indicesUsos <- match(ListUsos,TodosUsos) + 2  #le sumo dos unidades a los indices para la correspondencia con las columnas del DF
  
  #nueva columna con el WEI (suma demandas (seleccionadas) dividida con el agua disponible)
  TheScen$WEI <- rowSums( TheScen[,indicesUsos]) / TheScen$fresW
 
  #los agrego por NUTS2 (de acuedo a alguna metrica y alguna ponderacion, por ejemplo area del nuts)
  
  
  return(TheScen)     
}



# The_WEI_Scen <- BLS_WEI
# Funcion que agrega los WEI de cada subcuenca a nivel NUTS2
# Aplica diferentes tipos de metricas para devolver un valor para cada NUTS de la cuenca relativo al flujo de agua
# Las metricas son mean, median, 3Q, max, treshold, sum. Existira la opcion de PONDERAR o no por el area 
NutsWEIFromSubc <- function (The_WEI_Scen, W="F"){
  
  # Sin ponderar por area de la subcuenca
  if(W=="F"){
      my.summary <- function(x, na.rm=TRUE){result <- c(n=as.integer(length(x)),
                                                        Mean=mean(x, na.rm=TRUE),     # SD=sd(x, na.rm=TRUE), 
                                                        Median=median(x), Q=quantile(x,0.75), 
                                                        Max=max(x))}
      
      # library(plyr)  #para usar ddply
      WEIbyNUTS  <- ddply(The_WEI_Scen, c("N2"), function(x) my.summary(x$WEI))
    
  }
  
  # ponderando por area de la subcuenca
  if(W=="T"){
    
      my.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                         Mean=weighted.mean(x$WEI, x$Area, na.rm=TRUE),
                                                         median= wtd.quantile(x$WEI, 100*x$Area)[3] ,
                                                         Q=wtd.quantile(x$WEI, 100*x$Area)[4] ,
                                                         Max=max(x$WEI)   )}
      
      
      WEIbyNUTS <- ddply(The_WEI_Scen, c("N2"), function(x) my.summaryW(x))
  }
  
  ## Area EN CADA NUTS (que tiene tramos de catchment), lo anadimos como columna a el DF anterior  
  TotAreaByNUTS <-aggregate(The_WEI_Scen$Area, by=list(The_WEI_Scen$N2), FUN=sum, na.rm=TRUE)  #calcuo del Area de cada NUT
  names(TotAreaByNUTS) <- c("N2","TotArea")   #nombre a las columnas
  WEIbyNUTS <- merge(WEIbyNUTS,TotAreaByNUTS , by=c("N2") ) #los mezclo

  return(WEIbyNUTS)
}



TotWEIFromSubc <- function (The_WEI_Scen, W="F",thresh=1){

  # Sin ponderar por area de la subcuenca
  if(W=="F"){                                               #falta el treshold
    my.summary <- function(x, na.rm=TRUE){result <- c(n=as.integer(length(x)),
                                                      Mean=mean(x, na.rm=TRUE),     # SD=sd(x, na.rm=TRUE), 
                                                      Median=median(x), Q=quantile(x,0.75), 
                                                      Max=max(x),
                                                      Thr= sum(x[x>thresh])  )}
    
    WEI_tot  <- my.summary(The_WEI_Scen$WEI)
  }
  
  # ponderando por area de la subcuenca
  if(W=="T"){                                             #falta el treshold  
    my.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                       Mean=weighted.mean(x$WEI, x$Area, na.rm=TRUE),
                                                       median= wtd.quantile(x$WEI, 100*x$Area)[3] ,
                                                       Q=wtd.quantile(x$WEI, 100*x$Area)[4] ,
                                                       Max=max(x$WEI),
                                                       Thr= sum(x$WEI[x$WEI>thresh]*x$Area[x$WEI>thresh])   )}
    
    WEI_tot <- my.summaryW( The_WEI_Scen)
  }
  
  ## Area TOTAL (que tiene el BASIN)  
  TotArea <-sum(The_WEI_Scen$Area)  #calcuo del Area de cada NUT
  WEI_tot <- c(WEI_tot,TotArea) #lo anado como ultimo elemento del vector
  names(WEI_tot)[length(WEI_tot)] <- c("TotArea")   #nombre 
  
  return(WEI_tot)
}


# convierte los RATIOS de REDUCCION de una estrategia en valores (sea metro cubicos)
# BLS <- BLS
# StgtoPlot <- StgtoPlot
# esta funcion evalua la estrategia y recoge el esfuerzo [[2]] que ya no son ratios sino cantidad
stg_rates_to_values <- function(BLS , StgtoPlot){
  
  #Para dibujarlo en valores de cantidad de reduccion de agua extraida, tenemos que evaluar la estrategia
  MyEval_Scen <- EvalStg (BLS,StgtoPlot)  
  head(MyEval_Scen[[2]]) #  #devuelve [[1]]el nuevo escenario de demandas, y [[2]] La reduccion de demandas : diferencia de dichas demandas con relacion al BLS
  #el segundo nos da ya metros cubicos de reduccion de demanda. 
  # Falta agregarlos por nuts (ya tengo una funcion que los suma por nuts y tipo)
  PonderEffor <- "D"        #  D: Default (todos igual importancia) . En caso contrario pasa el array de pesos de ponderacion
  Mattemp <- ComputeEffort(MyEval_Scen,PonderEffor)  #Esfuerzo son las reducciones de demandas
  
 # Mattemp <- MyEffort_N2_sect[,c(3,4,5,6,7)]   #  ATENCION ESTOY REORDENANDOLOS, PARA EVITAR PROBLEMAS
#  rownames(Mattemp) <- MyEffort_N2_sect$N2  #asigno nombres de N2
#  colnames(Mattemp) <-  rownames(StgtoPlot)    #asigno nombres a los usos (en version bonita).
  
 # new_df <- Mattemp[ order(row.names(Mattemp)), ]  #reordeno de acuerdo al nombre de los N2
  
  
  return(Mattemp)
} 


# TheScen <-    Evaluated_BLScen
# ponder <-  PonderEffor
# El esfuerzo es simplemente la REDUCCION DE PRESIONES Nos interesa por sectores y NUTS
# Recibe el escenario evaluado (y el [[2]], son ya las reducciones por sectores y regiones, solo hace algo en el caso de la ponderacion)
# D: devuelve el esfuerzo por sectores y regiones (para figuras), medido en candidad de agua 
# Si se le pasa la matriz de ponderacion, pondera por ella y devulve un unico valor de esfuerzo (para el optimizador)
ComputeEffort <- function(TheScen, ponder="D" ){

  #Si todas las reducciones en las demandas requieren un coste (esfuerzo) igual
  if(ponder[1]=="D"){       # NO aplica ponderacion
    TheScen[[2]]  #el indice 2 son las reducciones de demandas
    Pressby <- TheScen[[2]]                     #en este caso devuelve el DF, para hacer figuras por sector y N2  
    #esto seria esfuerzo medido como cantidad de agua
  }
  
  if(ponder[1] !="D")          #aplica ponderacion
    Pressby <- sum( TheScen[[2]] * (ponder) ) #Este valor es para la optimizacion y debe ser un unico valor, asi que despues de ponderar, hace la suma total
        
  return(Pressby) 
}


# TheScen <-  Evaluated_BLScen
# ponder <- PonderEffor
# Calculo del esfuerzo de reduccion de una estrategia, por sectores y regiones
# El esfuerzo es simplemente la REDUCCION DE PRESIONES Nos interesa por sectores y NUTS
# Recibe un escenario evaluado y con la [[2]] diferencia de demandas calcula el esfuerzo de reduccion
N2EffortFromSubc_old <- function(TheScen, ponder="D" ){

   #Si todas las reducciones en las demandas requieren un coste (esfuerzo) igual
   if(ponder[1]=="D"){ 
     TheScen[[2]]  #el indice 2 son las reducciones de demandas
                 # las agrego por sectores y por N2
     Pressby <- aggregate(TheScen[[2]][,2:8], by=list(N2=TheScen[[2]]$N2), FUN=sum) #en este caso devuelve el DF, para hacer figuras por sector y N2  
      #esto seria esfuerzo medido como cantidad de agua
     
       #  PressIrrigbyNUTS <- aggregate(TheScen[[2]]$irrWD, by=list(Category=TheScen[[2]]$N2), FUN=sum)   
       #    names(PressIrrigbyNUTS) <- c("N2","Reductions") 
   }
  
   if(ponder[1] !="D"){ 
    
      PressbyNUTS_sect <- aggregate(TheScen[[2]][,2:8], by=list(N2=TheScen[[2]]$N2), FUN=sum)  
      rownames(PressbyNUTS_sect) <- PressbyNUTS_sect$N2
      PressbyNUTS_sect$N2 <- NULL
      temp <- t(PressbyNUTS_sect[,2:6]) * ponder     #multiplico y seria el esfuerzo ponderado 
      Pressby <- sum(temp)     #Este valor es para la optimizacion y debe ser un unico valor, asi que despues de ponderar, hace la suma total
     }
  
  return(Pressby) 
  
}



# TheScen <- MyEval_stg1
# Este calcula la suma total de lo extraido paara cada sector y N2
# En la siguiente funcion habria que anadirlo, ademas de otras agregaciones tambien la suma
#N2TotExtractionsFromSubc <- function(TheScen ){

 #   TheScen[[1]]  #el indice 1 son las demandas para ese escenario
              # las agrego por sectores y por N2
    
#    TotDemand <- aggregate(TheScen[[1]][,2:8], by=list(N2=TheScen[[1]]$N2), FUN=sum) #en este caso devuelve el DF, para hacer figuras por sector y N2  
    #esto seria esfuerzo medido como cantidad de agua
  
#  return(TotDemand) 
  
#}



  
# n_BLS <-   Evaluated_BLScen[[1]]   TheStg[[1]]   Eval_Scen[[1]]
# W <- "F": no pondero; "A", by area; "DA": By Drained Area
# Aplica diferentes tipos de metricas para devolver un valor para cada NUTS de la cuenca relativo al estado de las concentraciones
# Las metricas son mean, median, 3Q, max, treshold, sum. Existira la opcion de PONDERAR o no por el LONGITUD 
#### LORO FALTA ANADIR LA OPCION DE SUMA QUE ES MUY IMPORTANTE
NutsExtractionFromSubc <- function(n_BLS,W="F"){
  
  names(n_BLS)
  
  ####### 
  ## 1. SIN PONDERAR POR Area (aplicacion de las diferentes metricas de agrupamiento)
  #funcion para todos las metricas de agrupacion              #falta el treshold y la suma
  if(W=="F"){
    my.summary <- function(x, na.rm=TRUE){result <- c(n=as.integer(length(x)),
                                                      Mean=mean(x, na.rm=TRUE),     # SD=sd(x, na.rm=TRUE), 
                                                      Median=median(x), Q=quantile(x,0.75), 
                                                      Max=max(x))}
    
    # library(plyr)  #para usar ddply
    FresWbyNUTS  <- ddply(n_BLS, c("N2"), function(x) my.summary(x$fresW))
    DomesticWbyNUTS  <- ddply(n_BLS, c("N2"), function(x) my.summary(x$domestWD))
    IndustrialWbyNUTS  <- ddply(n_BLS, c("N2"), function(x) my.summary(x$industWD))
    EnergyWbyNUTS  <- ddply(n_BLS, c("N2"), function(x) my.summary(x$energWD))
    LivestockWbyNUTS  <- ddply(n_BLS, c("N2"), function(x) my.summary(x$livesWD))
    IrrigationWbyNUTS  <- ddply(n_BLS, c("N2"), function(x) my.summary(x$irrWD))
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
                                                       Mean=weighted.mean(x$fresW, x$Area, na.rm=TRUE),
                                                       median= wtd.quantile(x$fresW, 100*x$Area)[3] ,
                                                       Q=wtd.quantile(x$fresW, 100*x$Area)[4] ,
                                                       Max=max(x$fresW)   )}
    
    FresWbyNUTS <- ddply(n_BLS, c("N2"), function(x) my.summaryW(x))
    
    my.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                       Mean=weighted.mean(x$domestWD, x$Area, na.rm=TRUE),
                                                       median= wtd.quantile(x$domestWD, 100*x$Area)[3] ,
                                                       Q=wtd.quantile(x$domestWD, 100*x$Area)[4] ,
                                                       Max=max(x$domestWD)   )}
    
    DomesticWbyNUTS <- ddply(n_BLS, c("N2"), function(x) my.summaryW(x))

    my.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                       Mean=weighted.mean(x$industWD, x$Area, na.rm=TRUE),
                                                       median= wtd.quantile(x$industWD, 100*x$Area)[3] ,
                                                       Q=wtd.quantile(x$industWD, 100*x$Area)[4] ,
                                                       Max=max(x$industWD)   )}
    
    IndustrialWbyNUTS <- ddply(n_BLS, c("N2"), function(x) my.summaryW(x))
    
    my.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                       Mean=weighted.mean(x$energWD, x$Area, na.rm=TRUE),
                                                       median= wtd.quantile(x$energWD, 100*x$Area)[3] ,
                                                       Q=wtd.quantile(x$energWD, 100*x$Area)[4] ,
                                                       Max=max(x$energWD)   )}
    
    EnergyWbyNUTS <- ddply(n_BLS, c("N2"), function(x) my.summaryW(x))
    
    my.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                       Mean=weighted.mean(x$livesWD, x$Area, na.rm=TRUE),
                                                       median= wtd.quantile(x$livesWD, 100*x$Area)[3] ,
                                                       Q=wtd.quantile(x$livesWD, 100*x$Area)[4] ,
                                                       Max=max(x$livesWD)   )}
    
    LivestockWbyNUTS <- ddply(n_BLS, c("N2"), function(x) my.summaryW(x))
    
    my.summaryW <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                       Mean=weighted.mean(x$irrWD, x$Area, na.rm=TRUE),
                                                       median= wtd.quantile(x$irrWD, 100*x$Area)[3] ,
                                                       Q=wtd.quantile(x$irrWD, 100*x$Area)[4] ,
                                                       Max=max(x$irrWD)   )}
    
    IrrigationWbyNUTS <- ddply(n_BLS, c("N2"), function(x) my.summaryW(x))
    
  } 
  


  
  
  ## 4. Area EN CADA NUTS (que tiene tramos de catchment), lo anadimos como columna a el DF anterior  
  the_Nuts <- sort(as.character(unique(n_BLS$N2)))
  TotAreaByNUTS <- aggregate(n_BLS$Area, by=list(n_BLS$N2), FUN=sum, na.rm=TRUE)  #calcuo del Area de cada NUT
  names(TotAreaByNUTS) <- c("N2", "TotArea")   #nombre a las columnas
   
  FresWbyNUTS<- merge(FresWbyNUTS,TotAreaByNUTS , by=c("N2") ) #los mezclo con todos ellos
  DomesticWbyNUTS<- merge(DomesticWbyNUTS,TotAreaByNUTS , by=c("N2") )
  IndustrialWbyNUTS<- merge(IndustrialWbyNUTS,TotAreaByNUTS , by=c("N2") )
  EnergyWbyNUTS<- merge(EnergyWbyNUTS,TotAreaByNUTS , by=c("N2") )
  LivestockWbyNUTS<- merge(LivestockWbyNUTS,TotAreaByNUTS , by=c("N2") )
  IrrigationWbyNUTS<- merge(IrrigationWbyNUTS,TotAreaByNUTS , by=c("N2") )
  
    # MUCHA ATENCION TIENEN QUE ESTAR EN EL ORDEN CORRECTO
  
  return( list(FresWbyNUTS, DomesticWbyNUTS, EnergyWbyNUTS,IndustrialWbyNUTS, IrrigationWbyNUTS,  LivestockWbyNUTS))
  
}


# BLS <- BLScen   
# Concval <- MedianConc
# thePareto <- myParetoPesosIg
# Funcion para encontrar la estrategia de un frente de pareto mas proxima a un valor WEI
closestParetStrategByConcentration <- function(BLS,thePareto,Concval){ 
  
  #Busca la estrategia que este mas cerca (min) del valor de esfuerzo
  IndexClosest <- which(abs(thePareto$objectives[,2]-Concval)==min(abs(thePareto$objectives[,2]-Concval))) 
  
  #DECODIFICAR LAS ESTRATEGIAS A FORMATO MATRICIAL
  # StgBLS <- DecodeStg (BLS, rep(0,length(thePareto$parameters[1,]))  )  #una estrategia en la que no se reduce nada
  StgMedian <- DecodeStg (BLS, thePareto$parameters[IndexClosest[1],] )  #le anado el 1 por si hay mas de un punto a la misma distancia
  
  return(StgMedian)
}










































###############################################
## 9. Calcula WEI CATCHMENT
###############################################
# Para un Catchment calcula los WEI de cada uno de los consumos
# nameCat <- namecatch     #nombre de la cuenca principal
# CrSC <- C_SC_Rel   :  relacion entre catchment y subcatchment
# List_WaterInfo <- List_WQnI  #para cada subCatch, toda la info
# 4:r_fresW; 5:r_domestWD; 6:r_industWD; 7:r_energWD; 8:r_livesWD; 9:r_irrWD; 10: Area
Calcula_WEI_Catch <- function(nameCat,CrSC,List_WaterInfo){
  
  listofSubcatch_ID<- CrSC[CrSC$basin_name == nameCat ,]$cathment_hydroid   #ATENCION DEBERIA CONVERTIR A STRING
  #  length(listofSubcatch_ID)
  #List with the subseting of water (availability and uses) in the catchment
  # rm(List_C_W)
  #primer elemento de la nueva listafreswater
  df_C_W <- data.frame()  #DF con fresh y usos por subcatchment
  for(ind in 4:9){
    #cada indice es uno de los usos del agua, los meto en una lista
    # 4:r_fresW; 5:r_domestWD; 6:r_industWD; 7:r_energWD; 8:r_livesWD; 9:r_irrWD;
    df_C_W <- rbind(df_C_W,
                    t(List_WaterInfo[[ind]][names(List_WaterInfo[[ind]]) %in% listofSubcatch_ID])  )
  }
  
  #  length(t(List_WaterInfo[[ind]][names(List_WaterInfo[[ind]]) %in% listofSubcatch_ID]) )
  
  #si la traspongo, los nombres de las filas son los de cada subcatchment
  df_C_W2 <- as.data.frame(t(df_C_W)) #el traspose lo convertiria en matriz
  rownames(df_C_W2)<-  colnames(df_C_W) #le pongo los nombres de las filas a las columnas
  colnames(df_C_W2) <- c("fresW","domestWD","industWD","energWD","livesWD","irrWD")     #y a las columnas a mano
  #le anado los WEI (de la columna 7 en adelante)
  # class(df_C_W2)
  df_C_WEI <- df_C_W2  #data frame con el WEI por subcathment (lo inicializo a copia del anterior)
  # nrow(df_C_WEI)
  # head(df_C_WEI)
  # max(df_C_WEI$energWD)
  # boxplot(df_C_WEI$energWD)
  # boxplot(df_Catch_Inf$uses$fresW,df_Catch_Inf$uses$energWD, df_C_WEI$energWD)
  # length(df_Catch_Inf$uses$energWD)
  # length(df_C_WEI$energWD)
  
  #hago los nuevos calculos
  df_C_WEI$domestWD <- df_C_W2$domestWD/df_C_W2$fresW  # WEI DOMESTIC
  df_C_WEI$industWD <- df_C_W2$industWD/df_C_W2$fresW  # WEI Industrial
  df_C_WEI$energWD <- df_C_W2$energWD/df_C_W2$fresW  # WEI Energia
  df_C_WEI$livesWD <- df_C_W2$livesWD/df_C_W2$fresW  # WEI livestock
  df_C_WEI$irrWD <- df_C_W2$irrWD/df_C_W2$fresW  # WEI Irrigacion
  df_C_WEI$fresW<- NULL  #elimino esa columna
  #creo una nueva que sea el total
  df_C_WEI$Total<-(df_C_W2$domestWD + df_C_W2$industWD + df_C_W2$energWD + 
                     df_C_W2$livesWD + df_C_W2$irrWD) /df_C_W2$fresW  # WEI TODOS
  #  max(df_C_WEI$energWD)
  #  df_C_W2[which(df_C_WEI$energWD == max(df_C_WEI$energWD)), ]
  #  df_C_W2$WEI_Dom <- df_C_W2$domestWD/df_C_W2$fresW  # WEI DOMESTIC
  #  df_C_W2$WEI_Ind <- df_C_W2$industWD/df_C_W2$fresW  # WEI Industrial
  #  df_C_W2$WEI_Ener <- df_C_W2$energWD/df_C_W2$fresW  # WEI Energia
  #  df_C_W2$WEI_Liv <- df_C_W2$livesWD/df_C_W2$fresW  # WEI livestock
  #  df_C_W2$WEI_Irr <- df_C_W2$irrWD/df_C_W2$fresW  # WEI Irrigacion
  #  df_C_W2$WEI_All <- (df_C_W2$domestWD + df_C_W2$industWD + df_C_W2$energWD + 
  #                        df_C_W2$livesWD + df_C_W2$irrWD) /df_C_W2$fresW  # WEI TODOS
  #devuelve el DF
  result<-list("uses" = df_C_W2, "WEI" = df_C_WEI)
  return(result)
  
}

###############################################
## 9. Calcula WEI Catchment al aplicar reduccion en los consumos
###############################################
#  nameCat<-namecatch    #nombre de la cuenca
#  CrSC <- C_SC_Rel      #relacion entre id subcuenca y cuenca a la que pertenece
#  List_WaterInfo <- List_WQnI   #la informacion de cada subcuenca. Para cada subcatch cantidad en cada uso 
#  rateReduc <- RateReductionsPoint
#  Catch_Nuts2 <- df_C_N2
Calcula_WEI_Catch_Reduct <- function(nameCat,CrSC,List_WaterInfo,rateReduc,Catch_Nuts2){
  
  head(df_Catch_Inf$uses)  #aqui estarian los usos para ese subcatch
  head(df_Catch_Inf$WEI)  #aqui los WEI para el BLS
  
  #subseting de las subcuencas de la cuenca
  listofSubcatch_ID<- CrSC[CrSC$basin_name == nameCat ,]$cathment_hydroid   #ATENCION DEBERIA CONVERTIR A STRING
  
  dim(df_Catch_Inf$uses)
  # Hago un subseting de esos usos para los subcatchments en los que estoy (REDUNDANTE, YA QUE LOS ANTERIORES YA HAN PASADO POR SUBSETING) 
  MiCuencaUsos<-df_Catch_Inf$uses[rownames(df_Catch_Inf$uses) %in% listofSubcatch_ID,]
  dim(MiCuencaUsos)
  head(MiCuencaUsos)
  #Para cada fila, anado a que NUTS2 pertenece la subcuenca
  Catch_Nuts2
  
  #MEZCLO la info del catch con la de los N2 (UNA COLUMNA PARA CADA TIPO DE DEMANDA)
  merged_temp <- merge(MiCuencaUsos,Catch_Nuts2,  by.x = 0 , by.y ="SC")  #catch lo mezclo por nombre filas de el segundo DF
  dim(merged_temp)
  names(merged_temp)  #ya tengo un data fram en que cada columna es un uso y cada fila un subcatch
  head(merged_temp)
  names(merged_temp)[names(merged_temp)=="Row.names"] <- "SC"
  
  #la anterior ya la multiplico por los ratios de reduccion y calcula el nuevo WEI para cada subcatch
  #  rateReduc   
  df_N2rates_n<-data.frame(t(rateReduc))
  class(df_N2rates_n)
  #y le anado una columna igual al nombre fila (que es el NUTS2)
  df_N2rates_n$N2<-rownames(df_N2rates_n)
  dim(df_N2rates_n)
  # head(df_N2rates_n)
  # df_N2rates_n[merged_temp$N2, ]
  #  rownames(merged_temp)
  # df_N2rates_n[rownames(df_N2rates_n)]
  
  #   numtiposdemand<-5
  #   temp_WEI <- matrix(ncol=numtiposdemand, nrow=nrow(merged_temp) )
  #   for(nc in 1:numtiposdemand){  #la primera demanda es columna 4 (3+i), el primer ratio en la 2 (1+i)
  #     temp_WEI[,nc]  <-  merged_temp[,(3+nc)] * ((100-df_N2rates_n[merged_temp$N2,(nc)])/100)
  #   }
  #   temp_WEI <- data.frame(temp_WEI)
  #   colnames(temp_WEI)<-names(merged_temp)[4:(3+numtiposdemand)]
  #   rownames(temp_WEI)<-rownames(merged_temp)
  #lo que realmente quiero es sumar todas las demanda
  
  # merged_temp$total<- sum((merged_temp[,3] * ((100-df_N2rates_n[merged_temp$N2,(1)])/100) +
  #                       merged_temp[,4] * ((100-df_N2rates_n[merged_temp$N2,(2)])/100) +
  #                       merged_temp[,5] * ((100-df_N2rates_n[merged_temp$N2,(3)])/100) +
  #                       merged_temp[,6]* ((100-df_N2rates_n[merged_temp$N2,(4)])/100) +
  #                       merged_temp[,7]* ((100-df_N2rates_n[merged_temp$N2,(5)])/100)) / merged_temp[,2]) 
  # 
  merged_temp$total <- rowSums( merged_temp[,3:7] * ((100-df_N2rates_n[merged_temp$N2,(1:5)])/100)/ merged_temp[,2] )
  
  
  # head(temp_WEI)
  return(merged_temp)
}







#############################################
# 14. CalculaParetoSolReductions: Devuelve para cada solucion de Pareto, el ratio de reduccion por uso y Nuts2
#############################################
#  res <- resultados   #todo el frente de pareto (resultado de la optimizacion)
#  varname <- namesvar  #nombre de NUTS2
#  idP<-IdPareto     #indice del punto del frente de pareto del que queremos hacer el calculo
#  uso <-  usos   #nombre de los usos del agua considerados
CalculaParetoSolReductions <- function(res,varname,uso,idP ){
  #quizas lo primero seria eliminar el NUTS3 = 0
  numVar<-length(varname)  #es decir el numero de NUTS3
  #loop para los usos
  count<-0
  datatoplot<-NULL
  for(nu in uso){   #los pongo en forma matricial
    #subseting de los datos para hacer el histograma
    colIni <- numVar*count+1
    count<-count+1
    colFin <- numVar*count
    temp <- res$parameters[idP ,colIni:colFin]  #los valores para ese uso
    #ahora lo voy metiendo en la matriz
    datatoplot <- rbind(datatoplot,temp )
  }
  colnames(datatoplot)<-varname
  rownames(datatoplot)<-uso
  
  return(datatoplot) # DF con los ratios de reduccion de esa soluion. Columnas los NUTS2 y Filas cada uso
}
