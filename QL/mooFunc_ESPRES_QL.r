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
# Funciones para la optimizacion multi objetivo          #
# 
#
#                                       #
#                                         #
#                                         #
#                      #
################################################################################


# userVectW <- pairwiseeffort
# labelEffort <- PesosEffortlabel
###############################################  
# Funcion para calulo de los pesos a partir de la comparacion por pares
###############################################
ComputingWeihts <- function(userVectW,labelEffort){
  
  ##ATENCION AL TEMA DE LA ESCALA QUE ME PASA LUIGI UN 1 ES QUE USER CONSIDERA IGUAL PESO
  #### 2: indica mejor el de la derecha
  #### -2: indica mejor el de la izquierda
  
  ## create an artificial A matrix abc, example taken from Koczkodaj et al. (1997)
  
  numUses<- length(labelEffort)
  listuses<-labelEffort 
  mymatrix <- diag(numUses)
  colnames(mymatrix)<-listuses
  rownames(mymatrix)<-listuses
  
  countvectLuig <- 1
  for (i in 1:(numUses-1))  #relleno las no digagonales
    for (j in (i+1):numUses){
      #  print(paste(i, j,sep=" "))
      mymatrix[i,j] <- userVectW[countvectLuig] 
      mymatrix[j,i] <- 1/userVectW[countvectLuig]
      countvectLuig <- countvectLuig+1
    }
  
  resultado <- ahp(mymatrix)    ## compute the weights, Saaty's and Koczkodaj's inconsistencies
  return(resultado)
  
}





###############################################
## Versio AG del optimizador para QL
###############################################
# L_BLS <- BLScen
# MaxRateRed <- TheMaxReductions_QL
# Configlist <-  myConfig
MOO_GA_QL <- function(L_BLS, MaxRateRed, Configlist, verbose=TRUE) {
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  EJECUTANDO GA OPTIMIZADOR                ]")
  if (verbose) message("=======================================================")
  
  #una variable por cada tipo de presion y nuts
  #Consideramos 4 tipos de presiones en ese orden: MinN, ManN, SdN, PsN
  # Asi que el numero de variables sera 4 x Numero de NUTS de la cuenca
  numPresures <- 4
  the_Nuts <- sort(BLScen$TheNuts_EF)
  numvar <- numPresures*length(the_Nuts)

  lower <- 0
  liminf <- rep(lower,numvar) #el rango de las variables
  
 # unique(MaxRateRed$Nuts2)
  #verifico primero correspondencia entre los NUT2 del BLS y los que hay en la informacion de Maximos
  MaxReduct2 <- MaxRateRed[MaxRateRed$Nuts2 %in% the_Nuts, ]
  #como despues de cargarlos los ordene alfabeticamente en relacion a NUTS2 y a las presiones
  #puedo trasponerlos y tendre una correspondencia directa con los limites de maxima reduccion y lo codificado
  limsup <- as.numeric( t(MaxReduct2[,2:4])[3,] ) #traspongo y hago la fila 3 que es la de los valores

  
  #pongo un limite por cada tipo: Irrigation, ,"Livestook, Domestic","Industrial","Energy"","
  #  limsup <- rep(upper,numvar)       # c( l_mreduct[[1]]$x, l_mreduct[[2]]$x , l_mreduct[[3]]$x  )
  
  ## ATENCION CON LOS VALORES NA
  

  # opt_type<-1
  #Comparto todas las variables de esta funcion con la funcion my_funSWAT, y no se las paso (que no podria!!!)
  environment(myfun_MOO_GA) <- environment()  #le paso las variables de esta funcion a la otra
  
  results <- nsga2R(fn= myfun_MOO_GA, varNo=numvar, objDim=Configlist[[4]][1], lowerBounds=liminf, upperBounds=limsup,
                    popSize=Configlist[[4]][2], tourSize=2, generations=Configlist[[4]][3], cprob=0.9, 
                    XoverDistIdx=20, mprob=0.1,MuDistIdx=3)
  
}





myfun_MOO_GA <- function(x) {
  
  #GENERO UN X PARA TESTEAR
  #    PresRed_MinN <- rep(1,length(L_BLS$TheNuts_EF))    # Nitrates manure agricola 
  #    PresRed_ManN <- rep(2,length(L_BLS$TheNuts_EF))    # Nitrates mineral agricola
  #    PresRed_SdN <- rep(3,length(L_BLS$TheNuts_EF))     # Nitrates point sources (urban & industrial) 
  #    PresRed_PsN <- rep(4,length(L_BLS$TheNuts_EF))      # Nitrates scatter dweling 
  #    x <- c(PresRed_MinN,PresRed_ManN,PresRed_SdN,PresRed_PsN)
  
  #    names(PresRed_MinN) <-L_BLS$TheNuts_EF  #pongo el nombre de cada NUTS a cada reduccion de presiones
  #    names(PresRed_ManN) <-L_BLS$TheNuts_EF
  #    names(PresRed_SdN) <-L_BLS$TheNuts_EF
  #    names(PresRed_PsN) <-L_BLS$TheNuts_EF
  #    List_strateg <- list(PresRed_MinN,PresRed_ManN,PresRed_SdN,PresRed_PsN )  
  

  
  # STEP 1: nombre para los ficheros de resultados
  #a partir de la hora del sistema, genero un nombre para los ficheros de resultados
  nameoutput= format(Sys.time(), "%b%d%H%M%S")
  #nameoutput= (proc.time() - ptm)[3] 
  
  ####################################
  ### ESTRUCTURA DEL CROMOSOMA:   ####
  ##  PresRed_MinN:  1 gen por cada NUT: x[1:cutpoint]
  ##  PresRed_ManN:  1 gen por cada NUT: x[(cutpoint+1):(2*cutpoint)]
  ##  PresRed_SdN:   1 gen por cada NUT: x[(2*cutpoint+1):(3*cutpoint)]
  ##  PresRed_PsN:   1 gen por cada NUT: x[(3*cutpoint+1):(4*cutpoint)] 
  ##
  
  # 1. DECODIFICAMOS LA ESTRATEGIA DE REDUCCION
  #  cutpoint<-length(x)/numPresures  #consideramos para cada tipo de presion mismo numero de Nuts
  #  list_ReductionsDecod <- list( x[1:cutpoint], x[(cutpoint+1):(2*cutpoint)], x[(2*cutpoint+1):(3*cutpoint)], x[(3*cutpoint+1):(4*cutpoint)]  )     #separamos en cada tipo de presion
  #  names(list_ReductionsDecod[[1]]) <-L_BLS$TheNuts_EF      #les ponemos en nombre del NUTS para luego evaluarlas
  #  names(list_ReductionsDecod[[2]]) <-L_BLS$TheNuts_EF  
  #  names(list_ReductionsDecod[[3]]) <-L_BLS$TheNuts_EF  
  #  names(list_ReductionsDecod[[4]]) <-L_BLS$TheNuts_EF  
    
  #  x<-limsup
  
  # 1. DECODIFICAMOS LA ESTRATEGIA DE REDUCCION
    My_strateg <- DecodeStg_QL(L_BLS,x) 
   # dimx <- length(L_BLS$TheNuts_EF)
  #  My_strateg <- matrix(x, byrow = TRUE, ncol = dimx)
  #  rownames(My_strateg) <- c("Man","Min","PS","SDW")   #cada fila es un tipo de presion
  #  colnames(My_strateg) <- L_BLS$TheNuts_EF
    
    
  # 2. EVALUAMOS LA ESTRATEGIA ( son COMPLEMENTARIOS)
    ptm <- proc.time()
    optiStgEval <- EvalStg (L_BLS,My_strateg)   #Esta evaluacion hace todo el calculo matricial y ya devuelve presiones y concentraciones correspondientes a la estrategia
 #   print(paste("Process time Evaluate Strategy ",(proc.time() - ptm)  ))
    
    ## 2.1 OBJETIVO CONCENTRACIONES (varias posiblilidades de agrupacion)
      ptm <- proc.time()
      threshold <- Configlist[[2]][2]  #si se escoge la metrica de treshold, cargamos el valor, o se usa el de defecto
      weightBYlength<-"F"    #T: true, F: false
      stg_Concentr <- TotConcFromSubc (optiStgEval[[1]] ,weightBYlength,threshold)  #tambien hay varias formas, empleamos la que lo hace en base a las subcuencas
      #devuelve la agrupacion de varias metricas
      #   print(paste("Process time to evalue Concentration ",(proc.time() - ptm)[3]  ))
    
    
    ## 2.2 OBJETIVO EFFORT TO REDUCE THE PRESURES
      ptm <- proc.time()   
      # Solo en el MOO se pondera el esfuerzo. Asi, que hago aqui la ponderacion.
      EffortByNutsType <- NutsSectEffortFromSubc(optiStgEval)
      rownames(EffortByNutsType) <- EffortByNutsType$Nuts2 #preproceso el DF con esfuerzos
      EffortByNutsType <- t(EffortByNutsType[,3:6])
      Ponderacion <-  Configlist[[1]]     #Se pondera elemento a elemento
      PonderEffort <- sum(EffortByNutsType * Ponderacion) 
     
      
       

  #dependiendo de la metrica de agrupamiento el segundo objetivo: media, median, max
  #el primer objetivo sera la media de %ratios de reducion y el segundo la media del stg_Concentr$newconc
#  if ( Configlist[[2]][2] == 1 ) objtemp <- c(Effort, sum(stg_Concentr$newconc))  #suma de todos
  if ( Configlist[[2]][1] == 1 ) objtemp <- c(PonderEffort, stg_Concentr[2] )  #la media
  if ( Configlist[[2]][1] == 2 ) objtemp <- c(PonderEffort, stg_Concentr[3] )   #la mediana                                                      
  if ( Configlist[[2]][1] == 3 ) objtemp <- c(PonderEffort, stg_Concentr[4] )   #tercer quartil
  if ( Configlist[[2]][1] == 4 ) objtemp <- c(PonderEffort, stg_Concentr[5] )   #maximo (el peor valor de concentracion)
  if ( Configlist[[2]][1] == 5 ) objtemp <- c(PonderEffort, stg_Concentr[6] )   #Thr: suma de los valores que superan un umbral
  if ( Configlist[[2]][1] == 6 ) objtemp <- c(PonderEffort, stg_Concentr[7] ) 
      
  #  print(Configlis)
  # indexOb <-Configlist[[1]]  #los indices de los objetivos 
  signOb <- Configlist[[3]]   #positivo si es minimizar y negativo si es maximizar 
  #asi que ya podemos calcular el valor de los objetivos que se envia al optimzador
  obj <- c( objtemp[1]*signOb[1] , objtemp[2]*signOb[2] )  #los indices de los objetivos son 1 y 2 (por ahora)
  #el caso combinado es un poco diferente, pues es combinacion de 3 obj   (maximizar income-costWWTP)
  #  obj <- c( objtemp[indexOb[1]]*signOb[1] , objtemp[indexOb[2]]*signOb[2]+ (objtemp[indexOb[3]]*signOb[3]/10^6)  )
  
  print(obj)
  return(obj)
}


          # # El primer objetivo, lo agrupamos como suma ponderada
          # # Por eso no utilizo la tecnica de agrupamiento que indique el usuario:  Configlist[[2]][1]    Pero se podria hacer
          # # El Effort reduction  de la estrategia
          # DemanReductBySect <- eVal_Strategy[[7]]  #Devuelve el la reduccion demanda (volumen) por sectores y tramos 
          # weight<- Configlist[[1]]  #Dificultad de cada tipo de reduccion (agricola, livestock, urban)
          # Effort <- sum(DemanReductBySect[[1]])*weight[1] + 
          #   sum(DemanReductBySect[[2]])*weight[2] + 
          #   sum(DemanReductBySect[[3]])*weight[3]  #suma ponderada de la reduccion de las demandas (metros cubicos, pero ponderados)
          # 
          # # El WEIr de la estrategia
          # ST_WEIr <- eVal_Strategy[[6]]
          # #No esta claro que usar (max, mean, median, etc)