
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
## 11. Execute_1A: El optimizador
###############################################
# BLS <- BLScen   
# MaxReduct <- TheMaxReductions
# Configlist <- Configl  
MOO_GA <- function(BLS, MaxReduct, Configlist, verbose=TRUE) {
  
  if (verbose) message("=======================================================")
  if (verbose) message("[  EJECUTANDO OPTIMIZACION                ]")
  if (verbose) message("=======================================================")
  
  numPresures <- 5   #  domestWD   ;industWD  ;energWD  ;livesWD   ;irrWD
  the_Nuts <- sort(as.character(unique(BLScen[[6]]$N2)))
#  numvar <- numPresures*length(unique(BLS$N2))
  numvar <- numPresures*length(unique(BLScen[[6]]$N2)) 
  
  lower <- 0
  liminf <- rep(lower,numvar) #el rango minimo de las variables
  
  #verifico primero correspondencia entre los NUT2 del BLS y los que hay en la informacion de Maximos
  MaxReduct2 <- MaxReduct[MaxReduct$Nuts2 %in% the_Nuts, ]

  #como despues de cargarlos los ordene alfabeticamente en relacion a NUTS2 y a las presiones
  #puedo trasponerlos y tendre una correspondencia directa con los limites de maxima reduccion y lo codificado
  limsup <- as.numeric( (MaxReduct2[,2:4])[,3] ) #traspongo y hago la fila 3 que es la de los valores


  #Comparto todas las variables de esta funcion con la funcion my_funSWAT, y no se las paso (que no podria!!!)
  environment(myfun_MOO_GA1) <- environment()  #le paso las variables de esta funcion a la otra
  
  results <- nsga2R(fn = myfun_MOO_GA1, varNo=numvar, objDim=Configlist[[4]][1], lowerBounds=liminf, upperBounds=limsup,
                    popSize=Configlist[[4]][2], tourSize=2, generations=Configlist[[4]][3], cprob=0.9, 
                    XoverDistIdx=20, mprob=0.1,MuDistIdx=3)
  
  return(results)
}




myfun_MOO_GA1 <- function(x) {
  
  # STEP 1: nombre para los ficheros de resultados
  #a partir de la hora del sistema, genero un nombre para los ficheros de resultados
  nameoutput= format(Sys.time(), "%b%d%H%M%S")
  #nameoutput= (proc.time() - ptm)[3] 
  
 #  x<-limsup

  # 1. DECODIFICAMOS LA ESTRATEGIA DE REDUCCION
    My_strateg <- DecodeStg(BLS,x) # Decodifica la estrategia x. Solo Necesita el BLS para la dimension
  
  # 2. EVALUAMOS LA ESTRATEGIA ( criteri1: WEI . Criterio 2: Esfuerzos)
    opti_Scen <- EvalStg (BLScen,My_strateg)    #devuelve [[1]]el nuevo escenario de demandas, y [[2]]la diferencia de dichas demandas con relacion al BLS
    
      # 2.1 Calcula el WEI para cada subcuenca (para la Scenario al que se han aplicado reducciones)
      ListWDemToWEI <- c("Domestic", "Industrial", "Energy", "Livestock", "Irrigation")  #Obligatorio con todas las demandas
      BLS_WEI <- Calcula_WEI(opti_Scen[[1]],ListWDemToWEI) 
    
      # 2.2 Agregamos WEI para toda la cuenca  (diferentes metricas)
      weightBY <-"F"     #F: false (sin ponderar); Por ahora siempre asi
      threshold <- 1  # si no le pasamos el umbral, toma un valor por defecto (al calcular esa metrica)
      opti_Scen_WEI <- TotWEIFromSubc (BLS_WEI ,weightBY,threshold)  #atencion, si considermas el umbral tenemos que recibir el parametro
      
      # 2.3 Esfuerzos  (los consideramos con Ponderacion) (sera otro vector a recibir )
      PonderEffor <-  Configlist[[1]]     # c(.30 ,0.10 ,0.0 , 0.2, 0.20) # rep(0,nrow(My_strateg2))     
 #     names(PonderEffor) <- c("Domest","Energ","Indust","Irrig","Livest")                             
      opti_Scen_Effort <- ComputeEffort(opti_Scen,PonderEffor) #un valor ya final (ya que aplica la ponderacion por sectores)
      
      
  # 3. sELECCIONAMOS EL VALOR DEL OBJETIVO WEI DE ACUERDO A LA METRICA SELECCIONADA POR USUSARIO     
      #dependiendo de la metrica de agrupamiento el segundo objetivo: media, median, max
      if ( Configlist[[2]][1] == 1 ) objtemp <- c(opti_Scen_Effort, opti_Scen_WEI[2] )  #la media
      if ( Configlist[[2]][1] == 2 ) objtemp <- c(opti_Scen_Effort, opti_Scen_WEI[3] )   #la mediana                                                      
      if ( Configlist[[2]][1] == 3 ) objtemp <- c(opti_Scen_Effort, opti_Scen_WEI[4] )   #tercer quartil
      if ( Configlist[[2]][1] == 4 ) objtemp <- c(opti_Scen_Effort, opti_Scen_WEI[5] )   #maximo (el peor valor de concentracion)
      if ( Configlist[[2]][1] == 5 ) objtemp <- c(opti_Scen_Effort, opti_Scen_WEI[6] )   #suma de los valores que superan un umbral
      
      
  # 4. PROCESAMOS LOS OBJETIVOS (DE ACUERDO AL SIGNO)    
      #  print(Configlis)
      # indexOb <-Configlist[[1]]  #los indices de los objetivos 
      signOb <- Configlist[[3]]   #positivo si es minimizar y negativo si es maximizar 
      #asi que ya podemos calcular el valor de los objetivos que se envia al optimzador
      obj <- c( objtemp[1]*signOb[1] , objtemp[2]*signOb[2] )  #los indices de los objetivos son 1 y 2 (por ahora)
      #el caso combinado es un poco diferente, pues es combinacion de 3 obj   (maximizar income-costWWTP)
      #  obj <- c( objtemp[indexOb[1]]*signOb[1] , objtemp[indexOb[2]]*signOb[2]+ (objtemp[indexOb[3]]*signOb[3]/10^6)  )
      
      #    print(obj)
          
    return(obj)
}    
      
    
