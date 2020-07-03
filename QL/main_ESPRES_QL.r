################################################################################
# Author: Angel Udias    all complaints to: angel.udias-moinelo@ec.europa.eu  #
################################################################################
# Started: 21-Jan-2018 ->    ;                                                 #
# Updates: 10-August-2018                                                                    #
#                                                                              #
# ESPRES: Efficient Strategies for anthropogenic Pressure Reduction in European waterSheds                                                                             #
#              Main file                                                       #
#                                                                              #
# DESCRIPTION :                                                                #
# Afther choose a catchment, the program detect the NUTS contained             #
# Then iteratively it apply individual reduction for each NUTS and tipe of pressure in the 
# previously selected catchment. The water quality related type of pressure reduction considered are:
# CoeffMinN : Nitrates mineral agricola                                        #
# CoeffManN : Nitrates manure agricola                                         #
# CoeffSdN  : Nitrates scatter dweling                                         #
# CoeffPsN  : Nitrates point sources (urban & industrial)                      #
################################################################################

  ### clean
  memory.size()
  memory.size(TRUE)
  memory.limit()
  
  while(dev.cur()!=1) dev.off()
  rm(list=ls())
  .rs.restartR()
  
  memory.size()
  memory.size(TRUE)
  memory.limit()
  

  
#############################################################################
#############################################################################
### MENU SHINY:  SELECCION MODULO DE CANTIDAD QL                      #######
############################################################################
############################################################################
  
  
############################  
### 1. Pathway 
############################ 
  PATH_NAME <- "D:/HISTORICO/2018SEPTIEMBRE/ESPRES"   #edit if necessary
  SCRIPT_DIR 	<-paste0(PATH_NAME, "/QL/CODE/V4/")
  BASE_IN_DATA	<-paste0(PATH_NAME, "/QL/data/")
  BASE_PLOTS	<-paste0(PATH_NAME, "/QL/plots/")
  BASE_Shapes <- paste0(BASE_IN_DATA, "/Shape")
  
  BASE_OUT_DIR	<-paste0(PATH_NAME, "/QL/Results/")
   
############################    
### 2. LOADING SOURCE CODE
############################ 
  source(paste0(SCRIPT_DIR, "loadData_ESPRES_QL.r"))
  source(paste0(SCRIPT_DIR, "evalStg_EXPRES.r"))  
  source(paste0(SCRIPT_DIR, "functions_EXPRES.r"))
  source(paste0(SCRIPT_DIR, "mooFunc_ESPRES_QL.r"))
  source(paste0(SCRIPT_DIR, "plotFunc_ESPRES_QL.r"))
  source(paste0(SCRIPT_DIR, "plotMaps_ESPRES_QL.r"))
  
  
  
############################ 
### 3. LOADING LIBRARIES
############################
  Loading_libraries()     #### Es la misma funcion de ESPRES_QT  #####
  

  ## 5.2 LOADING SHAPE FILES FOR ALL THE MAPS 
  listTotalShapes <- Loading_MapShapes (BASE_Shapes,"Nuts2Vr2013.shp","basin.shp","catchment.shp")  #el path,nombre cuenca, el nombre del fichero de regiones, el nombre del fichero de cuencas, nombre fichero de subcuencas
  # [[1]]: NUS2 poligons
  # [[2]]: Catchment lines
  # [[3]]: Catchment poligons
  
  
  
#############################################################################
#############################################################################
### MENU SHINY:  SELECCION CUENCA                                   #######
############################################################################
############################################################################


############################  
### 4. SELECTION OF THE BASIN
############################     
  BasinName <-"Sava"       # "Danube"  Evrotas     Ebro   Adige     Danube   Sava 
  OUT_DIR <-  paste0(BASE_OUT_DIR,BasinName)
      #  Split <- strsplit(Path, "/")
      #  BasinName <-  Split[[1]][length(Split[[1]])]


##################################
### 5. CREATE BLS Scenario  (LOADING water QUALITY DATA for the BAsin)
##################################  
    ## 5.1. Load The Quality Status in each Hydro ID (subcatch)
    BLScen<-NULL  #para evitar que si no carga nada no veamos el error
    BLScen <- LoadDataCatch(BASE_IN_DATA,BasinName)  #devuelve una lista:  $DFConcentration ;$ConcReduct;  $TheNuts; $TheNuts_EF; 
    str(BLScen)  
    length(BLScen$DFConcentration$HydroID)
    nrow(BLScen$LoadReduct)
    # BLScen$DFConcentration   : concentraciones del BLS
    # BLScen$ConcReduct  : for each subcatchment the reduction de loas cuando se aplica reduccion presion 1% para cada nuts  

 
    ### 5.3 Subseting de la region de acuerdo a la cuenca que se este analizando
    #le tengo que pasar el nombre de la cuenca y los nombres de las regiones que intersectan la cuenca (las que optimizamos), tambien todos los hidro ID y el total de los shapes
    LstMyShapes <- Subsetingshapes (BasinName,BLScen$TheNuts,BLScen$DFConcentration$HydroID, listTotalShapes)
  #  LstMyShapes[[1]]

    
 
    
#############################################################################
#############################################################################
### MENU SHINY:  SUMMARY - General -   Subcatchments                 #######
############################################################################
############################################################################  

    # la siguiente funcion es copia de la de QT
    Map_SubCatch_new(BasinName,LstMyShapes)
    # Hay demasiadas subcuencas (son muy pequenitas), se tarda mucho en dibujarlo y no se ve nada
    
    
    
############################################################################
############################################################################
### MENU SHINY:  SUMMARY - General - NUTS2 Subcatchent               #######
############################################################################
############################################################################  
    
    ## Nueva version de la funcion para el mapa (que me gustaria que fuesen 2 mapas ,quizas con tab)      
    label <- TRUE
    Map_N2_SubCatch_Tab1(BasinName,LstMyShapes,label)      
    Map_N2_SubCatch_Tab2(BasinName,LstMyShapes,label)       
     
    ####  ATENCION CON ESTAS SUBCUENCAS SE TARDA MUCHO EN DIBUJAR ESTOS MAPAS ###
    
    
    
    
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Nitrates Concentration         #######
######################################################################################
######################################################################################   
    
    # Para estas pestanas tengo que evaluar el escenario base
    
    ############################  
    ###  EVALUATE ONE STRATEGY (the cero reductions: BLS)
    ############################   
    
    
    # Todo el codigo para los submenus del menu GENERAL, pueden ser copia de los
    # de la parte de cantidad (como mucho apuntando a los shape files de esta parte,
    # que antes o despues habra que unificar)
    
    # En cualquier caso, hace falta una BLS estrategia 
    
    
    ## 1 Genero una estrategia de reduccion de presiones NULA (que seria la del BLS)  
    #Defino una estategia, teniendo en cuenta los 4 tipos y los NUTS
    #porcentaje de reduccion y numero de NUTS (por ejemplo igual reduccion en todos los nuts)
    dimx <- length(BLScen$TheNuts_EF)  #el numero de columnas de la matriz (una por cada NUTS)
    BLSstg <- matrix( c( rep(0,dimx),     # Nitrates manure  agricola
                         rep(0,dimx),       # Nitrates mineral agricola
                         rep(0,dimx),       # Nitrates point sources (urban & industrial) 
                         rep(0,dimx)),      # Nitrates scatter dweling 
                      byrow = TRUE, ncol = dimx)
    rownames(BLSstg) <- c("Man","Min","PS","SD")   #cada fila es un tipo de presion
    colnames(BLSstg) <- BLScen$TheNuts_EF
    
    ## 2 EVALUA LA ESTRATEGIA NULA (del BLS), para disponer del valor de las concentraciones 
    #Cualquier escenario antes de hacer las figuras lo debo evaluar (incluso el BLS, porque empleo la columna de new conce)
    # Para el BLS, lo que hago es no aplicar reducciones Debe ser la evaluacion by subcuencas
    Evaluated_BLScen <- EvalStg (BLScen,BLSstg)  #LA EVALUO
    
    
    Evaluated_BLScen[[1]]   #data frame con las concentraciones originales y las nuevas
    Evaluated_BLScen[[2]][1:4,100:110]  # la matriz de reduccion de concentraciones (totales)
    str(Evaluated_BLScen)
    Evaluated_BLScen[[3]][1:4,1:10]  # la matriz de reduccion de LOAD (totales)

    # (Presion) Load reduction por sector  (si pongo 100 en la estrategia seria Eliminacion total del LoAD):
    rowSums(Evaluated_BLScen[[3]])
    
    # (Presion) Load reduction by sectors and Nuts
    NutsSectEffortFromSubc(Evaluated_BLScen)
    
    # (Presion) Load reduction by sectors
    TotEffortFromSubc(Evaluated_BLScen)
    
    
    
    
    ## 6.3 SUMARIO DE LAS AREAS DE LAS SUBCUENCAS DEL BLS    
 #   Summary_Scenario_Area(Evaluated_BLScen)     #le mando el escenario evaluado
    
    ptm <- proc.time()
    for (i in 1:500) EvalStg (BLScen,BLSstg)
    print (proc.time() - ptm)
      
    
    
    
    
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Nitrates Concentration -  Bar Plots      #######
######################################################################################
######################################################################################    
    
    # Barplot de las concentraciones
    # se puede ponderar de diferentes maneras, y el grafico se muestra de acuerdo a ello
    weightBY <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
    Stg_ConcbyNUT_AllMetric <- NutsConcFromSubc(Evaluated_BLScen[[1]] ,W="F") #agrego las concentraciones del escenario previamente evaluado
    BarPlot_Stg_Concentration(Stg_ConcbyNUT_AllMetric,weightBY)    #
    #La anterior ponderada por area o por drain area
  
    
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Nitrates Concentration -  Map by Nuts      #######
######################################################################################
######################################################################################     
    
    ### To Mapping the CONCENTRATION for the BLS strategy first I agregate by NUTS2
    weightBYlength <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
    ### LORO : tengo que retocar la siguiente funcion para que devuelva tambien la solucion con treshold
    newConcbyNUTW <- NutsConcFromSubc(Evaluated_BLScen[[1]] ,weightBYlength)   
    newConcbyNUTW
    newConcbyNUTW[,3:6] <- lapply(newConcbyNUTW[,3:6] , round, 2)  #reduzco numero de decimales para imprimir
    getwd()
    # write.csv(newConcbyNUTW, file = "ConctAreaSumary.csv")  #Para generar la tabla
    
    
    ## version SPPLOT
    #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
    NumClassInterv <- 8    #  Numero de categorias a la hora de dibujarlo
    paleta <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    BrBG    PuOr    YlGnBu  OrRd     Spectral
    estilo <-  "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"     "jenks"   no funciona: "bclust" 
    scale <- "linear"      # "linear"   "logarithmic"
    agregation <- "Mean"         # "Mean"     "Median"      "Q75"         "Max"  
  #  scaleRelation <- "Independent"   #  "Common"   "Independent"   #para el fresh Water no cambiaria,porque es solo una variable
    mytype<-"CONC"
    TheVariables <- Prepro_to_Map_N2_QL(newConcbyNUTW,scale,agregation,mytype)
    ## version SPPLOT   
    Map_var_NUTS_SPPLOT_QL (BasinName, LstMyShapes, TheVariables,NumClassInterv,paleta,estilo,scale,mytype )

    
    ## version BASE  (CESAR: ESTA NO SE ANADE A LA HERRAMIENTA, POR AHORA)
    AgrIndex <- 2     # 1:mean; 2: median; 3: 3Q; 4:Max; 5: Threshold    #tipo de agregacion de con la que se ha calculado las concentraciones 
    Map_var_NUTS_BASE_QL(BasinName,LstMyShapes,TheVariables,AgrIndex,NumClassInterv,paleta,estilo)
    
    
    
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Nitrates Concentration - Box Plots      #######
######################################################################################
######################################################################################     
    
    # Boxplot de las concentraciones de las subcuencas
    Boxplot_Stg_Concentration(BASE_PLOTS,Evaluated_BLScen,BasinName) # PLOTS ( que evaluar stg antes de hacer el plot)
    
    
    
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Nitrates Concentration - Map by Subcatchmets   #######
######################################################################################
######################################################################################    
    
    #lo que queremos es mapear una estrategia evaluada (usamos la que acabamos de evaluar)
    Evaluated_BLScen[[1]]$Concentration    #esa es la variable freswater

    #  NOTA: el estilo jenks tarda muchisimo para este mapa
    # NOTA 2: Los valores de concentracion son muy altos en algunos puntos, si lo dibuja sin escala logaritmica, la escala es solo de un color, porque el resto de los intervalos estan en una linea en la barra y no se aprecian
    #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
    NumClassInterv <- 9    #  Numero de categorias a la hora de dibujarlo
    paleta <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
    estilo <-  "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
    scale <- "linear"      # "linear"   "logarithmic"
    scaleRelation <- "Independent"   #  "Common"   "Independent"  #SOLO ES UN MAPA, NO ES NECESARIO
    valorMaxEscala <- 40  #   NULL    #todo lo que sea mayor a ese valor no lo dibuja
   
    #Para este mapa, cambiamos la funcion del codigo antiguo por esta nueva (que me esta gustando mas como queda)
    ListIndexUses <- c(3)  # index <- ListVar   # 3: concentracion en BLS; 13: Concent reduction; 14: New Concentration 
    Map_var_SubCatch_SPPLOT_QL (BasinName, LstMyShapes, Evaluated_BLScen[[1]], ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation,valorMaxEscala )
    

    head(Evaluated_BLScen[[1]])
    max(Evaluated_BLScen[[1]]$newconc)
    max(Evaluated_BLScen[[1]]$Concentration)
    
    
    
#######################################################################################
#######################################################################################
###     NUEVA FUNCIONALIDAD: MOSTRAR EL LOAD QUE SE APLICA POR CADA SECTOR ESPACIALMENTE  ******    
### MENU SHINY:  SUMMARY - APLIED LOAD - Map by Subcatchmets   #######
######################################################################################
###################################################################################### 
       # a partir del siguiente codigo se puede implementar la version que funciona.
       # la idea es que para hacer figuras con el load de N que se aplica en cada sector y subcuenca, 
       # lo que hago es aplicar una reduccion del 100%, y con eso tengo lo que que dejaria de aplicar
       # que a su vez es lo que se aplica en el BLS. Y luego he adaptado un poco la funcion de hacer el mapa por subcuenca
       # Faltaria hacer los boxplot, los mapas por nuts, de esos valores, e intentar integrarlo en la funcion base de plot (que ya hace las concentraciones)
       # y tambien se podria extender la funcionalidad a los resultados de la optimizacion
    
        # SI NO HAY REDUCCIONES (todo a cero) SERIA EL LOAD TOTAL, esfuerzo de reduccion nulo 
        dimx <- length(BLScen$TheNuts_EF)  #el numero de columnas de la matriz (una por cada NUTS)
        BLSstg <- matrix( c( rep(0,dimx),     # Nitrates manure  agricola
                             rep(0,dimx),       # Nitrates mineral agricola
                             rep(0,dimx),       # Nitrates point sources (urban & industrial) 
                             rep(0,dimx)),      # Nitrates scatter dweling 
                          byrow = TRUE, ncol = dimx)
        rownames(BLSstg) <- c("Man","Min","PS","SD")   #cada fila es un tipo de presion
        colnames(BLSstg) <- BLScen$TheNuts_EF
        
        ## 2 EVALUA LA ESTRATEGIA NULA (del BLS), para disponer del valor de las concentraciones 
        #Cualquier escenario antes de hacer las figuras lo debo evaluar (incluso el BLS, porque empleo la columna de new conce)
        # Para el BLS, lo que hago es no aplicar reducciones Debe ser la evaluacion by subcuencas
        Evaluated_BLScen <- EvalStg (BLScen,BLSstg)  #LA EVALUO
        
        head(Evaluated_BLScen[[3]][1:4,1:15] )
        
        # (Presion) Load reduction by sectors and Nuts
        NutsSectEffortFromSubc(Evaluated_BLScen)
        
        # (Presion) Load reduction by sectors
        TotEffortFromSubc(Evaluated_BLScen)
        
        ##### para dibujar el load del sector: MANURE.   Aplico 100% de reduccion a ese sector y esa reduccion seria lo que se aplica
        BLSstg <- matrix( c( rep(100,dimx),     # Nitrates manure  agricola
                             rep(100,dimx),       # Nitrates mineral agricola
                             rep(100,dimx),       # Nitrates point sources (urban & industrial) 
                             rep(100,dimx)),      # Nitrates scatter dweling 
                          byrow = TRUE, ncol = dimx)
        rownames(BLSstg) <- c("Man","Min","PS","SD")   #cada fila es un tipo de presion
        colnames(BLSstg) <- BLScen$TheNuts_EF
        Evaluated_Load <- EvalStg (BLScen,BLSstg) 
    
        head(Evaluated_Load[[3]][1:4,1:15] )  #son las 4 filas (manure, minera, PS, SD)
       
    
        names(Evaluated_Load[[3]][1:1,])
        Evaluated_BLScen[[1]]$HydroID
        Evaluated_BLScen[[1]]$Man <- Evaluated_Load[[3]][1:1,][match(Evaluated_BLScen[[1]]$HydroID, names(Evaluated_Load[[3]][1:1,])) ] 
        Evaluated_BLScen[[1]]$Min <- Evaluated_Load[[3]][2:2,][match(Evaluated_BLScen[[1]]$HydroID, names(Evaluated_Load[[3]][1:1,])) ] 
        Evaluated_BLScen[[1]]$PS <- Evaluated_Load[[3]][3:3,][match(Evaluated_BLScen[[1]]$HydroID, names(Evaluated_Load[[3]][1:1,])) ] 
        Evaluated_BLScen[[1]]$SD <- Evaluated_Load[[3]][4:4,][match(Evaluated_BLScen[[1]]$HydroID, names(Evaluated_Load[[3]][1:1,])) ] 
        names(Evaluated_BLScen[[1]])
        
        max(Evaluated_BLScen[[1]]$Man)
        boxplot(Evaluated_BLScen[[1]]$Man)
        #y esa nueva columna es la que quiero plotear 
        ListIndexUses <- c(15,16,17,18)  # index <- ListVar   # 3: concentracion en BLS; 13: Concent reduction; 14: New Concentration ; 15: LOAD  
        valorMaxEscala <- 200
        Map_var_SubCatch_SPPLOT_QL (BasinName, LstMyShapes, Evaluated_BLScen[[1]], ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation,valorMaxEscala )
        
    
        
        # (Presion) Load reduction by sectors and Nuts
        NutsSectEffortFromSubc(Evaluated_Man) 
    
    
    
    
    
    
######################################################################################
######################################################################################     
    
    
            ######################################
            ######################################
            ### MENU SHINY:  OPTIMIZATION  #######
            ######################################
            ######################################  
    
    
#############################################################################
#############################################################################
### MENU SHINY:  OPTIMIZATION - Maximun Pressure Reduction       #######
############################################################################
############################################################################

  thepath <- paste0(BASE_IN_DATA,"inputs/common/")

  TheMaxReductions_QL <- Loading_Max_PressRed_QL (thepath,BasinName)   #atencion que hay que arregarlos nombres del fichero

 
  
#############################################################################
#############################################################################
### MENU SHINY:  OPTIMIZATION - Sensitivity of the weight            #######
############################################################################
############################################################################ 
  
  # Funcion para realizar el analisis de sensibilidad a incrementos en el coste para reducir un tipo de presion (por sectores)  
  # Se le pasa el BLScen de la cuenca de la que se quiere analizar la sensibilidad
  # La funcion calcula (tarda bastante) y escribe un csv con el resultado
  # lo que haces es iterativamente (para cada tipo de presion) multiplicar por 2 el peso (Esfuerzo) que
  # el coste necesario para reducir una presion. Se ejecuta la optimizacion y se mira como es la distribucion por sectores
  # de los esfuerzos para conseguir el mismo nivel global de reduccion de presiones  
  # guarda el resultado en el diroctorio que le mandemos
 
  # la siguiente funcion lanza el analisis y solo la ejecuto yo una vez cada cada cuenca
  thepath <- paste0(BASE_IN_DATA,"inputs/",BasinName)
  numIter <- 100
  Analisis_Sensibilidad(thepath,BLScen,TheMaxReductions_QL,numIter)
  

  #######################################
    ####
    ##El siguiente codigo esta en el servidor de shiny y es lo que ve el usuario cuando hace el analisis de sensibilidad
    thepath <- paste0(BASE_IN_DATA,"/inputs/",BasinName,"/")
    SensiData <- read.csv( paste0(thepath,"Sensitivity",BasinName,".csv") )
  
    DFSensiBLS <- SensiData[SensiData$typosen=="BLS",2:5]
    DFSensiMan <- SensiData[SensiData$typosen=="Man",2:5]
    DFSensiMin <- SensiData[SensiData$typosen=="Min",2:5]
    DFSensiPs <- SensiData[SensiData$typosen=="Ps",2:5]
    DFSensiStd <- SensiData[SensiData$typosen=="Std",2:5]
    
#    colMeans(DFSensiBLS)
#    apply(DFSensiBLS,2,median) 

    Mattemp <- rbind (colMeans(DFSensiBLS),colMeans(DFSensiMan), 
                      colMeans(DFSensiMin),colMeans(DFSensiPs),colMeans(DFSensiStd) )
    rownames(Mattemp) <-  c("All == cost","Man=2 x cost","Min=2 x cost","Ps=2 x cost","Std=2 x cost") 
    colnames(Mattemp) <- c("Manure","Mineral","Point Sources","ScaterD")
    
    StgtoPlot2 <- (Mattemp)
    
    par(mfrow=c(1,1))
    barplot((as.matrix(StgtoPlot2))/1000000 ,
            names.arg = colnames(StgtoPlot2),las=1, beside=TRUE, 
            col=rainbow(nrow(StgtoPlot2)) , ylab=expression(paste("Nitrates load reduction 10"^"6"," Ton"^"3") ) ,
            main=paste0(" Sensitivity of the cost of reduction effort by sectors"),
            legend.text=TRUE, args.legend=list(x = "topright", cex = 1.05, bty = "n" ,inset=c(-0.05,-0.07) ),
            sub = " sectors  ")
    
    #Para entender la grafica"
    # los colores representan cada escenario con una configuracion del coste de los esfuerzos
    # El rojo es el escenario en el que los pesos para todos los sectores son iguales, es decir el coste (Esfuerzo) para reducir un tipo de presion u otro es el mismo
    # el amarillo-verdoso , es el escenario en el que reduccir el manure cuesta el doble que reducir el resto
    # y lo mismo los siguientes colores.
    # Por tanto las barras amarillo-verdosa es mas pequena que la roja en el grupo de manure,
    # quiere decir que al duplicarse el coste de reducir Manure, la solucion optima (que consigue reduccion de concentracion similar)
    # reduce menos nitratos procedentes de manure. Y las barras de ese mismo color en los otros grupos (sectores)
    # son mayores, porque lo que hace es reducir mas en dichos sectores (para conseguir la misma reduccion)
    # En el caso del incremento de coste de reducir Mineral, vemos que en el grupo de mineral se ve la disminucion
    # en la barra, indica que la disminucion de load en el sector mineral es menor, y se ven lijeros incrementos en otros sectores 
    # Hay que tener en cuenta que estos resultados son media de varias simulaciones que ademas tienen un componente aleatorio (el proceso de optimizacion)
    # por lo que las sumas se las reducciones en cada escenario no son exactamente el mismo valor.
    

    

      
############################################################################################
############################################################################################
### MENU SHINY:  OPTIMIZATION -  Setting  the pressure weights for each sector      #######
###########################################################################################
########################################################################################### 

  ## Desplegable: DIRECT   ##
  ###########################
  # 1. Ponderacion de Esfuerzos por SECTORES
    PesosEffortlabel <- c("MineralAgr","ManureAgr","ScatterDwe","PointSou")
    PonderEfforRawVal <- c(25,25,25,25)    #los valores que mete el usuario
    PonderEffor <- PonderEfforRawVal/sum(PonderEfforRawVal)
    
    #lo mostramos como Barplot 
    par(mar=c(4.5,7,2,1))
    barplot(as.table(PonderEffor), main="Relative Cost of Pressure Reduction", 
            horiz=TRUE, col="lightblue1",las=1, cex.axis = 1.3,cex.names=1.3,
            names.arg= PesosEffortlabel )
  
  
  # 2. Ponderacion de Esfuerzos por REGIONES
    PesosEffortRegionlabel <- sort(BLScen$TheNuts_EF) 
    PonderEffortRegion <- rep(100/length(PesosEffortRegionlabel),length(PesosEffortRegionlabel)  ) #si los considero de igual esfuerzo en todas las regiones
    PonderEfforReg <- PonderEffortRegion/sum(PonderEffortRegion)
    
    par(mar=c(4.5,6,2,1))
    barplot(as.table(PonderEfforReg), main="Relative Cost of Pressure Reduction by Regions", 
            horiz=TRUE, col="lightblue1",las=1, cex.axis = 1.3,cex.names=1.3,
            names.arg= PesosEffortRegionlabel )
    
  # 3. Compone la Matriz de ponderaciones en base a las dos anteriores.
    PonderMat <- PonderEffor %*% t(PonderEfforReg)
    colnames(PonderMat) <- sort(BLScen$TheNuts_EF) 
    rownames(PonderMat) <- PesosEffortlabel
    
    par(mar=c(4.5,6,2,1))
    PesosEffortlabel
    barplot(as.matrix(PonderMat), main="Relative Cost of Pressure Reduction by Sectors & Regions", 
            las=1, cex.axis = 1.3,cex.names=1.3,col= rainbow(length(PesosEffortlabel)),
            names.arg= PesosEffortRegionlabel ,beside = TRUE , ylab="% Relative Cost",
            legend.text=TRUE, args.legend=list(x = "topright", cex = 1.05, bty = "n")  )
    
  
  
  ## Desplegable: PAIR WISE ##
  ############################
  
  # Preparando la ponderacion por tipo de esfuerzo (pero no por region) a partir de la pair-wise comparison
  #A partir de las comparaciones por pares de la Interface, se calculan los pesos de la reduccion de presiones
  # para cada tipo de presion: domestWD, industWD, energWD, livesWD, irrWD
  # El numero de pares de comparaciones es = de N*(N-1)/2    Siendo N, el numero de categorias
  # Si tenemos 4 categorias "Man", "Min","PS","Sd", tendremos  4*3/2=6 pares de comparaciones
  # si tenemos 5 categorias "Domest","Energ","Indust","Irrig","Livest", tendremos 5*4/2 = 10
  PesosEffortlabel <- c("MineralAgr","ManureAgr","ScatterDwe","PointSou")  
  # Como compara todos con todos son 6 comparaciones:
  #   MineralAgr - ManureAgr  ; MineralAgr - ScatterDwe ; MineralAgr - PointSou  ;
  #   ManureAgr - ScatterDwe ; ManureAgr - PointSou ; 
  #   ScatterDwe - PointSou  ; 
  pairwiseeffort <- c(1, 2, 2, 2, 1, 1)  #Asi pues: esto es el resultado de todos los pairwise comparisons   
  Pesos <- ComputingWeihts(pairwiseeffort,PesosEffortlabel)  
  PonderEffor <- Pesos$weighting  #estos son los pesos que usaremos y haremos la grafica
  
  Pesos$Saaty   # Inconsistencia de acuerdo a la metrica de saaty
  
  #No ponemos botton de Inconsistency, yo diria que se verifica cada vez que completa la tabla o modifica un valor
  #pero tambien puede ser con valor
  
  if( Pesos$Saaty > 0.15) # of 0.10 or less is acceptable to continue the  AHP analysis. 
    print (paste( "Inconsistency ratio: ", Pesos$Saaty, " (Inconsistent matrix)",sep=" ") )
  
  
  #lo mostramos como Barplot 
  par(mar=c(4.5,7,2,1))
  barplot(as.table(PonderEffor), main="Relative Cost of Pressure Reduction", 
          horiz=TRUE, col="lightblue1",las=1, cex.axis = 1.3,cex.names=1.3,
          names.arg= PesosEffortlabel )
  
  
  
#############################################################################
#############################################################################
### MENU SHINY:  OPTIMIZATION -  Run  Optimization                 #######
############################################################################
############################################################################   
  
    ## Otros parametros para la optimizacion
    
    #otra funcion para el valor de los estados (sum, max, 3Q, mediana, media, treshold, ponderados o no por area)
    
    # Tengo que implementar una segunda consideracion por pesos, para cada NUTS
    # En este caso puede haber bastantes Nuts y serian demasiadas comparaciones: quizas usemos asignacion directa
    
    # primer parametro Concentracion agregation metric =>  #1:media ; 2:median;  3:3quantile; 4:max ; 5: Treshod (suma de lo que sobrepasa ese valor limite ; 6: suma de todas las concentraciones
    indexAgrM <- c(2,4.0)   #Segundo parametro, el umbral (solo es necesario si se escoge el tipo 5 de agregacion)
    
    signOb <- c(1,1)   #positivo si es minimizar y negativo si es maximizar (en este caso queremos minimizar ambos)
    genParam <- c(2,40,120)   #  nobject<-2 ; popSize=60 ; generations=40
    myConfig <- list(PonderMat,indexAgrM,signOb,genParam)
    
    ## Lanza la optimizacion   
    #OPTIMIZA (por el momento me olvido de la ponderacion)
    myPareto <- MOO_GA_QL(BLScen,TheMaxReductions_QL, myConfig) 
  
      #guardo el pareto en un fichero
      name <-paste(OUT_DIR,"Pareto_Evrotas_Th.rds",sep="/")
      saveRDS(myPareto, file = name)
     
      # Restore the object
      setwd(OUT_DIR)
      myParetomedia<-readRDS(file = "Pareto_Sava_Median.rds")   
      myParetomedia<-readRDS(file = "Pareto_Sava_Th5.rds") 
      myPareto <- myParetomedia
      
     myParetomedia <- myPareto
     myParetomedian <- myPareto
     myParetoQ3 <- myPareto
    
    ##################################
    #####  DIBUJANDO PARETOS INDIVIDUALES
    ###############################   
    #UN PARET0 concreto (en un fichero o en pantalla)
    Plot_Pareto(BASE_PLOTS,myPareto,"F","F","treshold")  #the second parameter: T: create a jpeg file.  F: don't create a jpeg file. El tercer parametro, es el rescalado a 0-1. El ultimo parametro es la escala de las concentraciones
    
     

     
     
    myP <- myPareto 
    
    myParetoMedia <- myPareto
    myParetoMedian <- myPareto
    myParetoSuma <- myPareto
    myParetoThr2 <- myPareto
    myParetoMax <- myPareto
    myPareto3Q <- myPareto
    
   
    
    #Verifico con una estrategia
    index <- 17
    stg_A <- myPareto$parameters[index,]
    stg_A_steg <- DecodeStg_QL (BLScen,stg_A)
    
   # stg_B_steg<-stg_A_steg
      #o defino una estrategia manualmente
      dimx <- length(BLScen$TheNuts_EF)  #el numero de columnas de la matriz (una por cada NUTS)
      BLSstg <- matrix( c( rep(10,dimx),     # Nitrates mineral agricola
                           rep(10,dimx),       # Nitrates manure agricola
                           rep(10,dimx),       # Nitrates point sources (urban & industrial) 
                           rep(10,dimx)),      # Nitrates scatter dweling 
                        byrow = TRUE, ncol = dimx)
      rownames(BLSstg) <- c("Man","Min","PS","SD")   #cada fila es un tipo de presion
      colnames(BLSstg) <- BLScen$TheNuts_EF
      stg_A_steg <-BLSstg

    #evaluacion de una estrategia 
    MyEval_stgA <- EvalStg (BLScen,stg_A_steg)  #eval le aplica las reducciones y nos devuelve la nueva concentracion y las reducciones
    MyEval_stgA[[1]]  #
    head(MyEval_stgA[[1]])  #es el data frame original a la que se anade reducciones totales y newconc
    
    dim(MyEval_stgA[[2]])  # Matriz de reducciones Totales por tipo de presion y subcuenca. Resultado de multiplicar matriz reducciones para !% por los porcentajes de reduccion
    dim(MyEval_stgA[[2]])
    MyEval_stgA[[2]][1:4,1:4]
    rowSums( MyEval_stgA[[2]])  # Suma por subcatchment del valor de reduccion de concentraciones por tipo de presion (atencion que no deberian poder sumarse)
    
    # Calculamos el esfuerzo ( EL CALCULO DE ESFUERZOS ASI CALCULADO SUMA LOS VALORES DE LAS CONCENTRACIONES, ESTO ESTA MAL)
    MyEffort_A <- NutsSectEffortFromSubc (MyEval_stgA)   #suma nitratos por sector y nut. No esta ponderado por el area
    # Calculamos el valor de la nueva concentracion
    ConcbyNUT_A <- NutsConcFromSubc (MyEval_stgA[[1]] , W="F" )
    ConcbyNUTTot_A <- TotConcFromSubc (MyEval_stgA[[1]] , W="F") 

    
    points( sum(MyEffort_A[3:6])*0.025 ,  ConcbyNUTTot_A[3], col="red"   )
    points( sum(MyEffort_A[3:6])*0.025 ,  median(ConcbyNUT_A$Median), col="red"   )
    
    
    name <- paste0("Stg_",stg_n1_index) 
    BarPlot_Stg_Effort(BLScen,stg_A_steg,name,rates="F") 
    
    
    #ANALIZAR SI LA REDUCCION DE PRESIONES DEBERIA SALIR DE LOS LOAD NO DE LAS CONCENTRACIONES
    # (Presion) Load reduction by sectors and Nuts
    NutsSectEffortFromSubc(MyEval_stgA)
    
    # (Presion) Load reduction by sectors
    TotEffortFromSubc(MyEval_stgA)

    
##############################################################################################
##############################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis          #######
#############################################################################################
############################################################################################# 
    
    # En esta parte de menu hace falta una BLS (ya la teniamos, pero repito el codigo)
    dimx <- length(BLScen$TheNuts_EF)  #el numero de columnas de la matriz (una por cada NUTS)
    BLSstg <- matrix( c( rep(10,dimx),     # Nitrates mineral agricola
                         rep(0,dimx),       # Nitrates manure agricola
                         rep(0,dimx),       # Nitrates scatter dweling 
                         rep(0,dimx)),      # Nitrates point sources (urban & industrial) 
                      byrow = TRUE, ncol = dimx)
    rownames(BLSstg) <- c("Man","Min","PS","SDW")   #cada fila es un tipo de presion
    colnames(BLSstg) <- BLScen$TheNuts_EF
    
    # Tambien hace falta un Pareto, del que se seleccionan un par de estrategias
    # Para todo este menu, siempre es necesrio tener un pareto
    myP <- myPareto 
    
    # Tambien hacen falta dos estrategias del frente de pareto (o la BLS y una del pareto)
    stg_n1_index <- 17  # BLS_strateg  # podria ser un indice  o la estrategia  BLS 
    stg_n2_index <- 2  # salio de una estrategia que mejora bastante en relacion con el BLS
    
    #extraigo la reduccion de presiones de esas dos estrategias del Pareto
    #  stg_n1 <- BLSstg
    stg_n1 <- myP$parameters[stg_n1_index,]
  #     stg_n1 <- c(72.5280639 ,6.3886658, 98.1918968, 99.8581251,  
   #                4.4037462, 6.4145990 , 99.0847713, 99.8216929,
    #               82.2737979,  1.0197124,  0.1511371, 99.1997143,
     #              8.0731878,  92.1701408, 80.5768745, 99.3559474)
       
    stg_n1_strateg <- DecodeStg_QL (BLScen,stg_n1)
    
    MyEval_stgA <- EvalStg (BLScen,stg_n1_strateg)
    MyEffort_A <- NutsSectEffortFromSubc (MyEval_stgA) 
    
    
    stg_n2 <- myP$parameters[stg_n2_index,]
      # stg_n2 <- c(25.74545056,23.23752164,12.18944436,10.20721822,12.85379076,12.70779061,11.25578568,12.92001566,12.09066195,3.27337747,7.48980363,
      #             2.12794821,8.26377705,15.56921971,17.72188694,14.54479082,12.09708854,12.37446998,10.03713830,7.02790565,7.24676957,19.12611697,
      #             11.05002042,11.13386662,7.45946358,10.35921613,9.02232814,23.29236562,20.05996612,11.28200725,1.04792454,17.68052834,0.39312227,
      #             14.55841440,16.05166586,10.18403268,17.91973075,13.10121509,1.57239449,2.97937139,3.25444919,0.28081568,6.96983361,10.10867277)
    Stg_n2_strateg <- DecodeStg_QL (BLScen,stg_n2)
    
    
    #ya tengo las dos estrategias para usar en las siguientes pestanas 
    stg_n1_strateg
    Stg_n2_strateg
    
    
   
##############################################################################################
##############################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - Effort Analysis  - BarPlot by Nuts          #######
#############################################################################################
############################################################################################# 
    
    stg_n1_strateg <- stg_A_steg 
    Stg_n2_strateg <- stg_B_steg
    name<-"3Q p22"
    name<-"3Q p6"
 
    # Nota: tener en cuenta que todos estos barplot se hancen sin ponderacion en las agregaciones
    
    ####### BOTON: Separate Strategies rates    ################
    par(mfrow=c(1,2))
    name <- paste0("Stg_",stg_n1_index)                              
    BarPlot_Stg_Effort(BLScen,stg_n1_strateg,name,rates="T")  #como porcentajes
    name <- paste0("Stg_",stg_n2_index)   
    BarPlot_Stg_Effort(BLScen,Stg_n2_strateg,name,rates="T")  #como porcentajes

    
    ####### BOTON: Separate Strategies volume    ################
    par(mfrow=c(1,2))
    name <- paste0("Stg_",stg_n1_index) 
    BarPlot_Stg_Effort(BLScen,stg_n1_strateg,name,rates="F")  #como metros cubicos totales
    name <-  paste0("Stg_",stg_n2_index)       
    BarPlot_Stg_Effort(BLScen,Stg_n2_strateg,name,rates="F")  #como metros cubicos totales
    
 
    # El barplot es mejorable (misma escala para ambos, las etiqueta no es load,... etc), no lo toco porque no se que empleareis 
    
    # check: DIFFERENCIA entre ambas estrategias
    # Calculo la diferencia restando (como es lineal no hay problema) y es valido tanto como % o como load
    stg_diff_rates <- Stg_n2_strateg - stg_n1_strateg
    
    ####### BOTON: Difference  ################
    name<-"Diference Rate"
    BarPlot_Stg_Effort(BLScen,stg_diff_rates,name,rates="T")
    name<-"Diference load"
    BarPlot_Stg_Effort(BLScen,stg_diff_rates,name,rates="F")  
    #tambien es mejorable (las etiquetas, no es load, es vol)
    
    
    
    ####### BOTON: Aggregate by Sector  ################ 
    #####  #####
    ##### #####  ATENCION REVISAR EL ORDEN EN QUE DEVUELVE LOS USOS N2EffortFromSubc
    ####   #####      Por tanto hago el calculo agregado de Nitratos (creo toneladas)
    MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
    MyEffort_stg1 <- NutsSectEffortFromSubc (MyEval_stg1)   #suma nitratos por sector y nut. No esta ponderado por el area
    Stg_n1_PressRed_bySect <- colSums(MyEffort_stg1[,3:(length(MyEffort_stg1)-1)] ) 

    MyEval_stg2 <- EvalStg (BLScen,Stg_n2_strateg) 
    MyEffort_stg2 <- NutsSectEffortFromSubc (MyEval_stg2)   #suma nitratos por sector y nut. No esta ponderado por el area
    Stg_n2_PressRed_bySect <- colSums(MyEffort_stg2[,3:(length(MyEffort_stg2)-1)] ) 
    
    #ATENCION: esta figura esta mal, al menos cuando se trate del evrotas con una sola region
    Mattemp <- rbind (Stg_n1_PressRed_bySect,Stg_n2_PressRed_bySect)
    rownames(Mattemp) <-  c("stg_A","stg_B") 
    
    StgtoPlot2 <- (Mattemp)
    
    par(mfrow=c(1,1))
    barplot((as.matrix(StgtoPlot2))/1000000 ,
            names.arg = colnames(StgtoPlot2),las=1, beside=TRUE, 
            col=rainbow(nrow(StgtoPlot2)) , ylab=expression(paste("Nitrates load reduction 10"^"6"," Ton"^"3") ) ,
            main=paste0(" Load reduction comparison"),
            legend.text=TRUE, args.legend=list(x = "topright", cex = 1.05, bty = "n" ,inset=c(-0.05,-0.07) )  )
    
   
    

##############################################################################################
##############################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - Effort Analysis  - Maps by Nuts          #######
#############################################################################################
#############################################################################################    
    
    
    #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
    NumClassInterv <- 8    #  Numero de categorias a la hora de dibujarlo
    paleta <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
    estilo <-  "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
    scale <- "linear"      # "linear"   "logarithmic"
    agregation <-   "No"  # "Mean"       "Mean"     "Median"      "Q75"         "Max"  
    scaleRelation <- "Common"   #  "Common"   "Independent"
    
    ####### BOTON: Separate Strategies rates    ################
    # Los PORCENTAJES de esfuerzos de reduccion por N2 y sectores
    # la estrategia 1   (fila de mapas superiores)
    stg_n1_strateg  #si lo quiero pintar en porcentajes no aplico transformaciones
    mytype<-"EF"
    TheVariables <- Prepro_to_Map_N2_QL(stg_n1_strateg,scale,agregation,mytype)
    ## version SPPLOT   
    Map_var_NUTS_SPPLOT_QL (BasinName, LstMyShapes, TheVariables,NumClassInterv,paleta,estilo,scale,mytype )
    
    # la estrategia 2  (fila de mapas inferiores)
    Stg_n2_strateg  #si lo quiero pintar en porcentajes no aplico transformaciones
    mytype<-"EF"
    TheVariables <- Prepro_to_Map_N2_QL(Stg_n2_strateg,scale,agregation,mytype)
    ## version SPPLOT   
    Map_var_NUTS_SPPLOT_QL (BasinName, LstMyShapes, TheVariables,NumClassInterv,paleta,estilo,scale,mytype )
    
    # lo anterior dibujara 4 mapas uno para cada tipo de uso
    # En la funcion de plotear mapa tengo que implementar aun en escala independiente
    
    
    ####### BOTON: Separate Strategies toneladas (cantitades)    ################  
   
    #la estrategia 1
    MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg)                     #evalua  (  para convertir los ratios en valores (sea m3 agua o toneladas de load) )
    MyEffort_stg1 <- NutsSectEffortFromSubc (MyEval_stg1)        #agrega por nuts
    mytype<-"EFT"
    TheVariables <- Prepro_to_Map_N2_QL(MyEffort_stg1,scale,agregation,mytype)
    mytype<-"EF"  
    Map_var_NUTS_SPPLOT_QL (BasinName, LstMyShapes, TheVariables,NumClassInterv,paleta,estilo,scale,mytype )
    
    #la estrategia 2
    MyEval_stg2 <- EvalStg (BLScen,Stg_n2_strateg)                     #evalua  (  para convertir los ratios en valores (sea m3 agua o toneladas de load) )
    MyEffort_stg2 <- NutsSectEffortFromSubc (MyEval_stg2)        #agrega por nuts
    mytype<-"EFT"
    TheVariables2 <- Prepro_to_Map_N2_QL(MyEffort_stg2,scale,agregation,mytype)
    mytype<-"EF"  
    Map_var_NUTS_SPPLOT_QL (BasinName, LstMyShapes, TheVariables2,NumClassInterv,paleta,estilo,scale,mytype )
    
    

    ####### BOTON: Difference    ################     
    #la diferencia en RATIOS la calculo restando (como es lineal no hay problema) y es valido tanto como % o como load
    stg_diff_rates <- Stg_n2_strateg - stg_n1_strateg   # fila de mapas superiores
    mytype<-"EF"
    TheVariables <- Prepro_to_Map_N2_QL(stg_diff_rates,scale,agregation,mytype)
    ## version SPPLOT  
    scale <- "linear"    #REVISAR HAY ALGUN NAN
    Map_var_NUTS_SPPLOT_QL (BasinName, LstMyShapes, TheVariables,NumClassInterv,paleta,estilo,scale,mytype )
    
     
    #la diferencia en CANTIDADES tambien la calculo restando
    Stg_diff_val <- MyEffort_stg2  #sobreescribo para tener el nombre de los nuts en el nuevo
    Stg_diff_val[,2:7] <- MyEffort_stg2[,2:7] - MyEffort_stg1[,2:7] 
    mytype<-"EFT"
    TheVariables2 <- Prepro_to_Map_N2_QL(Stg_diff_val,scale,agregation,mytype)
    mytype<-"EF"
    Map_var_NUTS_SPPLOT_QL (BasinName, LstMyShapes, TheVariables2,NumClassInterv,paleta,estilo,scale,mytype )
    
    
    
    
##############################################################################################
##############################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - Effort Analysis -   Flow Diagram          #######
#############################################################################################
#############################################################################################   
    
    
    #######################
    ### SANKEY de REDUCTIONS una estrategia
    ####################
    Plot_SANKEY_Reductions (BLScen , stg_n1_strateg)
    Plot_SANKEY_Reductions (BLScen , Stg_n2_strateg)
    
   
    
    #######################
    ### SANKEY de USES_and_REDUCTIONS una estrategia
    ########################
    # Lo primero que necesito son los usos: Cantidad de nitrato empleado en cada N2 y sector
    
    
    # Hay un problema conceptual que tengo que hablar con Bruna, 
    # reltaivo a la forma de calcular la concentracion que aporta cada sector,
    # ya que para el BLS la tengo toda junta
    ####################
#    Plot_SANKEY_Uses_Red (BLScen , stg_n1_strateg)
#    Plot_SANKEY_Uses_Red (BLScen , Stg_n2_strateg)
    
    
    # 
    # dimx <- length(BLScen$TheNuts_EF)  #el numero de columnas de la matriz (una por cada NUTS)
    # stgIter <- matrix( c( rep(100,dimx),     # Nitrates mineral agricola
    #                      rep(100,dimx),       # Nitrates manure agricola
    #                      rep(100,dimx),       # Nitrates scatter dweling 
    #                      rep(100,dimx)),      # Nitrates point sources (urban & industrial) 
    #                   byrow = TRUE, ncol = dimx)
    # rownames(stgIter) <- c("Man","Min","PS","SDW")   #cada fila es un tipo de presion
    # colnames(stgIter) <- BLScen$TheNuts_EF
    # 
    # ####### BOTON: Aggregate by Sector  ################ 
    # #####  #####
    # ##### #####  ATENCION REVISAR EL ORDEN EN QUE DEVUELVE LOS USOS N2EffortFromSubc
    # ####   #####      Por tanto hago el calculo agregado de Nitratos (creo toneladas)
    # Eval_ScenIter <- EvalStg (BLScen,stgIter) 
    # ConcIter <- NutsConcFromSubc(Eval_ScenIter[[1]],W="F")   #suma nitratos por sector y nut. No esta ponderado por el area
    # #Tomamos como referencia la columna del total: Sum
    # 
    # 
    # Stg_n1_PressRed_bySect <- colSums(MyEffort_stg1[,3:(length(MyEffort_stg1)-1)] ) 
    # 
    
    
    
    
    
  #####################################################################
  #####################################################################
  ### MENU SHINY:  OPTIMIZATION - Analysis - Concentration      #######
  #####################################################################
  #####################################################################  
    
    
    
    
    
    
    
#######################################################################################
#######################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - Concentration - BarPlot           #######
######################################################################################
######################################################################################  
    
    # Continuo utilizando las dos estrategias seleccionadas del frente de pareto
    stg_n1_strateg
    Stg_n2_strateg
    
    # Evaluo las dos estrategias
    MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
    MyEval_stg2 <- EvalStg (BLScen,Stg_n2_strateg) 
    
    # agregamos las nuevas concentraciones by Nuts (con varias metricas)
    ConcbyNUT_stg1<- NutsConcFromSubc (MyEval_stg1[[1]] , W="F" )
    ConcbyNUT_stg2<- NutsConcFromSubc (MyEval_stg2[[1]] , W="F" )
    
    ####### BOTON: Separate Strategies    ################  
    #vosotros lo haceis con plotly, asi que usais las dos DF anteriores: ConcbyNUT_stg1, ConcbyNUT_stg2
    
    #De forma individual quedarian asi (esto es para mi, para comparar con lo vuestro)
    BarPlot_Stg_Concentration(ConcbyNUT_stg1, "F")
    BarPlot_Stg_Concentration(ConcbyNUT_stg2, "F")
    # Nota: las concentraciones cambian realmente poco para muchas de las estrategias
    
    ### TABLAS con los data frames:(reduciendo numero de decimales:
    ConcbyNUT_stg1[,3:6] <- lapply(ConcbyNUT_stg1[,3:6] , round, 2)
    ConcbyNUT_stg2[,3:6] <- lapply(ConcbyNUT_stg2[,3:6] , round, 2)

    
    ####### BOTON: Difference    ################  
    # Calculo todas las diferencias
    Thedifference <- ConcbyNUT_stg2  # hago la copia de una de los df y luego modifico en un loop
    Thedifference[,c(3:6)] <- ConcbyNUT_stg1[,c(3:6)] - ConcbyNUT_stg2[,c(3:6)]  # Una estrategia de menos esfuerzo, reducira menos , con lo que tendra mayor concentracion  normamente dara mas valores positivos)
   
    BarPlot_Stg_Concentration(Thedifference,"F")  
    
    ### TABLAS con el data frames (reduciendo numero de decimales:
    Thedifference[,3:6] <- lapply(Thedifference[,3:6] , round, 2)
    
    
#######################################################################################
#######################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - Concentration - Map by Nuts    #######
######################################################################################
######################################################################################    
    
    # Evaluo las dos estrategias
    MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
    MyEval_stg2 <- EvalStg (BLScen,Stg_n2_strateg) 
    
    # agregamos las nuevas concentraciones by Nuts (con varias metricas)
    ConcbyNUT_stg1<- NutsConcFromSubc (MyEval_stg1[[1]] , W="F" )
    ConcbyNUT_stg2<- NutsConcFromSubc (MyEval_stg2[[1]] , W="F" )
    
    # write.csv(newConcbyNUTW, file = "ConctAreaSumary.csv")  #Para generar la tabla
    
    ####### BOTON: Separate Strategies    ################  
    
    ## version SPPLOT
    #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
    NumClassInterv <- 9    #  Numero de categorias a la hora de dibujarlo
    paleta <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
    estilo <-  "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
    scale <- "linear"      # "linear"   "logarithmic"
    agregation <- "Median"         # "Mean"     "Median"      "Q75"         "Max"  
    #  scaleRelation <- "Independent"   #  "Common"   "Independent"   #para el fresh Water no cambiaria,porque es solo una variable
    mytype<-"CONC"
    TheVariables1 <- Prepro_to_Map_N2_QL(ConcbyNUT_stg1,scale,agregation,mytype)
    TheVariables2 <- Prepro_to_Map_N2_QL(ConcbyNUT_stg2,scale,agregation,mytype)
    ## Dibuja los dos en  SPPLOT   
    Map_var_NUTS_SPPLOT_QL (BasinName, LstMyShapes, TheVariables1,NumClassInterv,paleta,estilo,scale,mytype )
    Map_var_NUTS_SPPLOT_QL (BasinName, LstMyShapes, TheVariables2,NumClassInterv,paleta,estilo,scale,mytype )
    
    #NOTA: por ahora lo hacemos asi. Tengo que estudiar la forma de que dibuje los dos en la misma escala

    ####### BOTON: Difference    ################  
    TheDifference <- TheVariables2   # la stg1 recibe menor reduccion de presiones, con lo que tendra mayor concentracion
    TheDifference[,3] <- TheVariables1[,3] - TheVariables2[,3]
    ## Dibuja los dos en  SPPLOT   
    Map_var_NUTS_SPPLOT_QL (BasinName, LstMyShapes, TheDifference,NumClassInterv,paleta,estilo,scale,mytype )
    
    
    
##############################################################################################
##############################################################################################
###   MENU SHINY:   OPTIMIZATION - Analysis - Concentration -   Box Plot              #######
#############################################################################################
#############################################################################################
    
    # Evaluo las dos estrategias
    MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
    MyEval_stg2 <- EvalStg (BLScen,Stg_n2_strateg) 
    
    # CESAR: Pongo la llamada a esa funcion que pinta el boxplot de una estrategia
    # tu los haces con plotly, asi que no me mato mucho para ponertelos aqui 
      Boxplot_Stg_Concentration(BASE_PLOTS,Evaluated_BLScen,BasinName) # PLOTS ( que evaluar stg antes de hacer el plot)
    
    ####### BOTON: Separate Strategies    ################
      # Proceso para dibujarlo con ggplot
      box_stg1 <- cbind ( MyEval_stg1[[1]][,c(6,14) ] , "stg14")
      colnames(box_stg1)[3]<- "stg"
      box_stg2 <- cbind (MyEval_stg2[[1]][,c(6,14) ] , "stg2")
      colnames(box_stg2)[3]<- "stg"
      box_total <- rbind( box_stg1, box_stg2)
    
      names(box_total)[2]
      ggplot(aes_string( y= names(box_total)[2] , x = "Nuts2", fill = "stg"), data = box_total) + geom_boxplot()   #seleccion por nombre
      # Muy poco util si no podemos desactivar outlier
     
      
      ########   BOTON:   Difference  ######
      TheDiference <- merge ( MyEval_stg1[[1]][c(1,6,14)], MyEval_stg2[[1]][c(1,6,14)] , by.x="HydroID", by.y="HydroID")  
     
      TheDiference$diff <- TheDiference$newconc.x - TheDiference$newconc.y
      
      boxplot(TheDiference$diff ~ TheDiference$Nuts2.x ,ylab="N", las=2,col=c("snow2"),
              outline=FALSE, main="Concentration difference" )
      #quizas se pueden anadir los puntos con un Jiter 
      points(factor(TheDiference$Nuts2.x), TheDiference$diff,col=3)  #arregal y rematar ,ordenarlos tambien
      
      
      # FOR THE REPORT
      # por lo general la diferencia de concentraciones es pequena comparado con el estado, asi que es dificil ver nada en el boxplot
      # Quizas lo mejor es un boxplot con la diferencia con el BLS, es decir de las reducciones en uno y el otro caso
      # Hago el boxplot de ambas reducciones
      TheReductions <- merge ( MyEval_stg1[[1]][c(1,6,13)], MyEval_stg2[[1]][c(1,6,13)] , by.x="HydroID", by.y="HydroID")  
      
   #   TheDiference$diff <- TheDiference$newconc.x - TheDiference$newconc.y
      
      
      boxplot(TheReductions[,c(3,5)],ylab="TN mg/L ", las=1,col=c("snow2"),
              outline=FALSE, names=c("3Q p22","3Q p6 "),main="Total Nitrogen concentration reduction " )
      
      #si hay mas de una region 
      boxplot(TheReductions[,c(3)] ~ TheReductions$Nuts2.x ,ylab="N", las=2,col=c("snow2"),
              outline=FALSE, main="Concentration reduction 3Q p22" )
      
      boxplot(TheReductions[,c(5)] ~ TheReductions$Nuts2.x ,ylab="N", las=2,col=c("snow2"),
              outline=FALSE, main="Concentration reduction 3Q p6" )
      
      name<-"3Q p22"
      name<-"3Q p6"
      
      
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Nitrates Concentration - Map by Subcatchmets   #######
######################################################################################
######################################################################################    
    
    #lo que queremos es mapear una estrategia evaluada (usamos la que acabamos de evaluar)
    Evaluated_BLScen[[1]]$Concentration    #esa es la variable freswater
    
    #  NOTA: el estilo jenks tarda muchisimo para este mapa
    # NOTA 2: Los valores de concentracion son muy altos en algunos puntos, si lo dibuja sin escala logaritmica, la escala es solo de un color, porque el resto de los intervalos estan en una linea en la barra y no se aprecian
    #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
    NumClassInterv <- 8    #  Numero de categorias a la hora de dibujarlo
    paleta <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
    estilo <-  "jenks"  #  "fisher"  "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
    scale <- "linear"      # "linear"   "logarithmic"
    scaleRelation <- "Independent"   #  "Common"   "Independent"  #SOLO ES UN MAPA, NO ES NECESARIO
    valorMaxEscala <- 4   # "Null"   #      
    
    ####### BOTON: Separate Strategies    ################
    ListIndexUses <- c(14)  # index <- ListVar   # 3: concentracion en BLS; 13: Concent reduction; 14: New Concentration 
    Map_var_SubCatch_SPPLOT_QL (BasinName, LstMyShapes, MyEval_stg1[[1]], ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation,valorMaxEscala )
    Map_var_SubCatch_SPPLOT_QL (BasinName, LstMyShapes, MyEval_stg2[[1]], ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation,valorMaxEscala )
    
    
    
    ########   BOTON:   Difference  ######
    TheDiference <- MyEval_stg2[[1]]  #asigno la que en teoria tiene mayor concentracion y luego le resto la otra
    TheDiference[,14] <- MyEval_stg1[[1]][,14] - MyEval_stg2[[1]][,14]
    ## la diferencia   
    Map_var_SubCatch_SPPLOT_QL (BasinName, LstMyShapes, TheDiference, ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation,valorMaxEscala )
    
    
    
    ######## BOTON: Concentration Reduction
    ListIndexUses <- c(13)  # index <- ListVar   # 3: concentracion en BLS; 13: Concent reduction; 14: New Concentration 
    Map_var_SubCatch_SPPLOT_QL (BasinName, LstMyShapes, MyEval_stg1[[1]], ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation,valorMaxEscala )
    Map_var_SubCatch_SPPLOT_QL (BasinName, LstMyShapes, MyEval_stg2[[1]], ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation,valorMaxEscala )
    
    
    
####################################################################################
#########################  FIN DE PROGRAMA PRINCIPAL      #########################
####################################################################################       
####################################################################################   
    
    
    
    
    
    
    
    
    ###########################################################################################
    ##### GENERACION DE ESTRATEGIAS ALEATORIAS Y COMPARACION CON ESTRATEGIAS EFICIENTES #######
    ################  CODIGO PARA HACER FIGURA 6 DEL ARTICULO  ################################
    ###########################################################################################
 
    
    ##  1. Seleccion de la cuenca 
      BasinName <-"Sava"    #     "Evrotas"  "Adige"     "Sava"   "Ebro"   
    
    
    ## 2. Genera un escenario BAse (si no lo habia generado antes, que es lo normal)
      BLScen<-NULL  #para evitar que si no carga nada no veamos el error
      BLScen <- LoadDataCatch(BASE_IN_DATA,BasinName)  #devuelve una lista:  $DFConcentration ;$ConcReduct;  $TheNuts; $TheNuts_EF; 
      # str(BLScen)  
      numNuts <- length(BLScen$TheNuts_EF)
    
    ## 3. Carga de los limites maximos de reduccion
      thepath <- paste0(BASE_IN_DATA,"inputs/common/")
      TheMaxReductions_QL <- Loading_Max_PressRed_QL (thepath,BasinName)   #atencion que hay que arregarlos nombres del fichero
    
    ## 4. Crea Matriz de Costes (Esfuerzo de cada reduccion). Por defecto ponderando todo igual  
      Equipeso <- 1
      Mat_Costes <- matrix( rep(Equipeso/1,(4*numNuts)) ,ncol=numNuts )
      colnames(Mat_Costes) <- sort(BLScen$TheNuts_EF)
      rownames(Mat_Costes) <- c("Man","Min","PS","SD")
      
    ## 5. Configuracion para ejecutar el optimizador
      indexAgrM <- c(2,5)   #Segundo parametro, el umbral (solo es necesario si se escoge el tipo 5 de agregacion)
      signOb <- c(1,1)   #positivo si es minimizar y negativo si es maximizar (en este caso queremos minimizar ambos)
      genParam <- c(2,50,300)   #  nobject<-2 ; popSize=60 ; generations=40
      # PonderEffor <- NULL  # la matriz de ponderacion sera lo que se actualizara en cada iteracion
      #  myConfig <- list(PonderEffor,indexAgrM,signOb,genParam)
      myConfig <- list(Mat_Costes,indexAgrM,signOb,genParam) #Matriz  equicoste, de ponderacion para las ejecuciones
    
    ## 6. Busca soluciones optimas  
      myParetoFig6 <- MOO_GA_QL(BLScen,TheMaxReductions_QL, myConfig) #Ejecuta la optimizacion
    
      myParetoFig6_Th <- myParetoFig6
    # ALTERNATIVAMENTE A LOS PASOS ANTERIORES SE PUEDE CARGAR UNA SOLUCION GUARDADA  
      # Restore the object
    #        setwd(OUT_DIR)
    #        myParetoFig6<-readRDS(file = "Pareto_Sava_Th5.rds")  

      
    ## 7. Dibuja el frente de Pareto  
      plot(myParetoFig6$objectives[,1],myParetoFig6$objectives[,2],cex.axis=0.9,
           xlim=c(0,1.05*max(myParetoFig6$objectives[,1])),col="blue",las=1,
           xlab="Effort",ylab="Concentration",main=BasinName,pch = 19 )
      
          #  plot(myParetoFig6$objectives[,1]/1000000,myParetoFig6$objectives[,2],ylim=c(0.015,0.04))
          #  plot(myParetoFig6$objectives[,1]/1000000,myParetoFig6$objectives[,2],col="blue")
          
          # 7.1 comprobacion: estraemos una estrategia y la evalua
            stg <-myParetoFig6$parameters[2,]
            myParetoFig6$objectives[2,]
            
            Random_strateg <- t(matrix( stg ,ncol=numNuts ))
            rownames(Random_strateg) <- sort(BLScen$TheNuts_EF)
            colnames(Random_strateg) <-  c("Man","Min","PS","SD")
            stg_deco <- DecodeStg_QL (BLScen,stg)  #Alternativamente lo siguiente la convierte tambien
            Eval_stg_deco <- EvalStg (BLScen,stg_deco)    
          #  PonderEffor <- myConfig[[1]]
          #  ComputeEffort(Eval_stg_deco,PonderEffor)
            
            NutsSectEffortFromSubc(Eval_stg_deco)
            
            # Calcula el WEI para cada subcuenca (para el escenario al que se han aplicado reducciones)
            ListWDemToWEI <- c("Man","Min","PS","SD")  #Obligatorio con todas las demandas
            stg_deco_WEI <- Calcula_WEI(Eval_stg_deco[[1]],ListWDemToWEI)  #el wei por subcuenca
            median(stg_deco_WEI$WEI )
    
    
    ## 5. Generacion de estrategias aleatoria  
    numStg <- 200   #numero de estrategias a generar y evaluar
    
    for (i in 1:numStg){
      # i=1
      Random_strateg <- matrix( runif(numNuts*4, 0, 50) ,ncol=numNuts ) #genera numeros aleatorios correspondientes al ratio de reduccion. El maximo deberia estar limitado por la maxima reduccion
      colnames(Random_strateg) <- sort(BLScen$TheNuts_EF)
      rownames(Random_strateg) <- c("Man","Min","PS","SD")
      
      Evalua_RS <- EvalStg (BLScen,Random_strateg)    #devuelve [[1]]el nuevo escenario de demandas, y [[2]]la diferencia de dichas demandas con relacion al BLS
      
      EffortByNutsTypes <- NutsSectEffortFromSubc(Evalua_RS)
      rownames(EffortByNutsTypes) <- EffortByNutsTypes$Nuts2 #preproceso el DF con esfuerzos
      EffortByNutsTypes <- t(EffortByNutsTypes[,3:6])
      PonderEffor <- myConfig[[1]]
      PonderEffort <- sum(EffortByNutsTypes * PonderEffor) #esfuerzo
      
      #  Calcula la CONCENTRACION para cada subcuenca (para la Scenario al que se han aplicado reducciones)
      threshold <- myConfig[[2]][2]  #si se escoge la metrica de treshold, cargamos el valor, o se usa el de defecto
      weightBYlength<-"F"    #T: true, F: false
      theCONC <- TotConcFromSubc (Evalua_RS[[1]] ,weightBYlength,threshold)  #tambien hay varias formas, empleamos la que lo hace en base a las subcuencas
    
      points( PonderEffort, theCONC[3], col="red" ,pch = 19) 
      
    }
    
    
    #Algunas Estrategias Uniformes
    numStg <- 50   #numero de estrategias a generar y evaluar
    for (i in 1:numStg){
      Random_strateg <- matrix( rep(i,(numNuts*4))  ,ncol=numNuts ) #genera numeros aleatorios correspondientes al ratio de reduccion. El maximo deberia estar limitado por la maxima reduccion
      colnames(Random_strateg) <- sort(BLScen$TheNuts_EF)
      rownames(Random_strateg) <-  c("Man","Min","PS","SD")
      
      Evalua_RS <- EvalStg (BLScen,Random_strateg)    #devuelve [[1]]el nuevo escenario de demandas, y [[2]]la diferencia de dichas demandas con relacion al BLS
      
      EffortByNutsTypes <- NutsSectEffortFromSubc(Evalua_RS)
      rownames(EffortByNutsTypes) <- EffortByNutsTypes$Nuts2 #preproceso el DF con esfuerzos
      EffortByNutsTypes <- t(EffortByNutsTypes[,3:6])
      PonderEffor <- myConfig[[1]]
      PonderEffort <- sum(EffortByNutsTypes * PonderEffor) #esfuerzo
      
      #  Calcula la CONCDENTRACION para cada subcuenca (para la Scenario al que se han aplicado reducciones)
      threshold <- myConfig[[2]][2]  #si se escoge la metrica de treshold, cargamos el valor, o se usa el de defecto
      weightBYlength<-"F"    #T: true, F: false
      theCONC <- TotConcFromSubc (Evalua_RS[[1]] ,weightBYlength,threshold)  #tambien hay varias formas, empleamos la que lo hace en base a las subcuencas
      
      points( PonderEffort, theCONC[3], col="green" ,pch = 19) 
      
    }
    
    
    legend("topright",inset=.02, legend=c("Pareto Stg", "Random Stg","Uniform Stg" ),
           col=c("blue", "red","green"),  cex=1.2,  pch=c(19,19,19),  box.lty=0)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #A partir de la estrategia de reducciones nulas (BLS), aplico alguna reduccion 
    Mtx_strateg <- BLSstg
    
    #AHORA MODIFICO LA ESTRATEGIA BASE PARA HACER ALGUNA PRUEBA
    Mtx_strateg[,]<-0    #modifico uno de los valores (Mineral en el primer Nut)
    Mtx_strateg[2,4]<-1    #primer indice tipo de presion, segundo indice el nuts
    

##################################
##### 7. CALCULO DE CONCENTRACIONES DESPUES DE APLICAR LA REDUCCION
##################################

  ## 7.1 EVALUACION DE LA CONCENTRACION  para cada subcuenca DESPUES DE APLICAR UNA ESTRATEGIA DE REDUCCION 
    newSCEN <- EvalStg (BLScen,Mtx_strateg)
    newSCEN[[1]]  # Es el BLScen al que se le ha anadido 
    #la columna con la redconc (reduccion de concentracion) y la newconc (nueva concentracion)
    newSCEN[[2]]  # La matriz de reducciones por tipo de presion y sucuenca

    newSCEN[[2]][1:4,100:110]
    
    #la diferencia entre esas dos sera la diferencia de concentracion total
    sum(newSCEN[[1]]$Concentration)
    sum(newSCEN[[1]]$newconc)

    
  ## 7.2 AGREGACION DE LO EVALUADO PARA CADA NUTS2 (diferentes metricas) (opcion ponderacion por LONGITUD)
    weightBYlength<-"F"     #F: false (sin ponderar); A: by area ; DA: by Drained Area
    newConcbyNUT <- NutsConcFromSubc(newSCEN[[1]] ,weightBYlength)   #Sin ponderar por longitud
    newConcbyNUT
    newConcbyNUT[,3:7] <- lapply(newConcbyNUT[,3:7] , round, 1)  #reduzco numero de decimales para imprimir
    weightBYlength<-"A"     #F: false (sin ponderar); A: by area ; DA: by Drained Area
    newConcbyNUTW <- NutsConcFromSubc(newSCEN[[1]] ,weightBYlength)   #Ponderando por longitud
    newConcbyNUTW
    newConcbyNUTW[,3:7] <- lapply(newConcbyNUTW[,3:7] , round, 1)  #reduzco numero de decimales para imprimir
    
  
  ## 7.3. EVALUACION/agregacion DE LA CONCENTRACION TOTAL DE LA CUENCA EN BASE A LA CONCENTRACION DE CADA NUTS 
    str(newConcbyNUTW)
    # STbyNUT : DF colums the agregation method (NutI) applied inside each NUT.
    # NutI: The number of colum of the INDICATOR of the previous agregation (inside the NUT) selected by user to this second agregation step. 3:mean; 4: median, 5: q75  
    # weightBYlength: indica si se quiere ponderar por area o no               
    ConcbyNUT <- newConcbyNUT  # status by NUT (en este caso empleo el calculo previo ponderado por area)
    NutI <- 7         # 3:mean; 4: median, 5: q75 , 6: max, 7:suma 
    weightBYlength <-"F"      #tipo de ponderacion
    STTotal_BasedInmean_W <- TotConcFromNuts(ConcbyNUT,NutI,weightBYlength) #lo ejecuta y devuelve un valor total para la cuenca (pero medido con varios indicadores)
    STTotal_BasedInmean_W
    #el valor de este para agregaciones como la suma, depende mucho de la agregacion de la primera agregacion.
    
    
  ## 7.4 EVALUACION DE LA CONCENTRACION TOTAL DE LA CUENCA EN BASE A LA CONCENTRACION TODAS las SUBCUENCAs 
    weightBYlength <-"F"    #T: true, F: false
    newSTbyNUTW_v2 <- TotConcFromSubc (newSCEN[[1]] ,weightBYlength)  
    newSTbyNUTW_v2
    threshold<-40  # si no le pasamos el umbral, toma un valor por defecto
    newSTbyNUTW_v3 <- TotConcFromSubc (newSCEN[[1]] ,weightBYlength,threshold)  
    newSTbyNUTW_v3
    ## ATENCION: hay bastantes subcuencas donde la concentracion toma valores muy altos
    # aunque son outlier, como los valores son muy altos, la suma de las concentraciones en esas
    # subcuencas es un valor muy alto.
    # Esto implica que no es una buena metrica para analizar el estado.
    # Pensarlo un poco mas, pero seguramente la unica metrica que no se ve afectada por estos
    # outliers tan exagerados sea la mediana y el Q75
    
    
    
  ##################################
  ##### 8. CALCULO DE ESFUERZOS
  ###############################
    
  ## EVALUACION DEL ESFUERZO para reducir presiones en cada subcuenca DESPUES DE APLICAR UNA ESTRATEGIA DE REDUCCION 
    newSCEN <- EvalStg (BLScen,Mtx_strateg)  # EJECUTADO PREVIAMENTE (computacionalmente pesado)
    newSCEN[[1]]    # no la uso ahora
    newSCEN[[2]]    #Necesitamos las reducciones de presion por Tipo (Min, Man, SD, PS) y subcatchment

  #Los esfuerzos en mineral y manure dependen del area
    newSCEN[[2]][1:4,1:20]
      
  ## 8.1 AGREGACION DE LO EVALUADO PARA CADA NUTS. Por ahora agregado como suma de cuencas en cada nuts
    weightBYlength<-"F"
    NutsSectEffortFromSubc(newSCEN,weightBYlength)
      
  ## 8.2 EVALUACION/agregacion DE LA CONCENTRACION TOTAL DE LA CUENCA EN BASE A LA CONCENTRACION DE CADA NUTS 
          #por ahora uso solo el siguiente, luego hago este, 
    
      
  ## 8.3 EVALUACION DEL ESFUERZO TOTAL DE LA APLICACION DE UNA ESTRATEGIA EN LA CUENCA EN BASE A LA CONCENTRACION DE CADA SUBCUENCA 
    TotEffortFromSubc(newSCEN)

      
      
  ##################################
  ##### 9. OPTIMIZACION MULTI OBJETIVO. BUSQUEDA ESTRATEGIAS EFICIENTES 
  ###############################
      
      ## 9.1  Ponderacion por tipo de esfuerzo (pero no por region)
      
      #otra funcion para el valor de los estados (sum, max, 3Q, mediana, media, treshold, ponderados o no por area)
      
      #ESTRATEGIAS PARETO EFICIENTES (aun sin ponderar)
      
      #A partir de las comparaciones por pares de la Interface, se calculan los pesos de la reduccion de presiones
      # para cada tipo de presion: Manure, Mineral, PS, ScaterD
      # El numero de pares de comparaciones es = de N*(N-1)/2    Siendo N, el numero de categorias
      # Si tenemos 4 categorias "Man", "Min","PS","Sd", tendremos  4*3/2=6 pares de comparaciones
      Effortlabel <- c("Man","Min","PS","SDW")  
      # Como compara todos con todos son 6 comparaciones:
      pairwiseeffort <- c(1,1,1,1,1,1)  #En ese orden  #   Man - Min ; Man - PS ; Man - Sd ; Min - PS ; Min - SD ; Ps - SD
      WeightUses <- ComputingWeihts(pairwiseeffort,Effortlabel)  
      WeightUses
      
      # Tengo que implementar una segunda consideracion por pesos, para cada NUTS
      # En este caso puede haber bastantes Nuts y serian demasiadas comparaciones: quizas usemos asignacion directa

      # primer parametro Concentracion agregation metric =>  #1:media ; 2:median;  3:3quantile; 4:max ; 5: Treshod (suma de lo que sobrepasa ese valor limite 
      indexAgrM <- c(2,2.1)   #Segundo parametro, el umbral (solo es necesario si se escoge el tipo 5 de agregacion)
  
      signOb <- c(1,1)   #positivo si es minimizar y negativo si es maximizar (en este caso queremos minimizar ambos)
      genParam <- c(2,16,40)   #  nobject<-2 ; popSize=60 ; generations=40
      Configl <- list(WeightUses,indexAgrM,signOb,genParam)
      
      
      #OPTIMIZA (por el momento me olvido de la ponderacion)
      myPareto  <- MOO_GA(BLScen, Configl) 
  #    myParetoW  <- MOO_GA(BLScen, Configl)
      
      myParetoMedia_1_1_1_1_1_1 <- myPareto  #4 ejecuciones
      
      myParetoMedian_1_1_1_1_1_1 <- myPareto
      
      myPareto3Q_1_1_1_1_1_1 <- myPareto
      
      myParetoThresh_1_1_1_1_1_1 <- myPareto
      
      myPareto3Q_2_3_2_1_1_1 <- myPareto
      
      myParetoMedia <- myPareto
      myParetoMedian <- myPareto
      myPareto3Q <- myPareto
      myParetoMax <-  myPareto
      myParetoThr <-  myPareto
      myParetoThr4 <-  myPareto
      
      saveRDS(myParetoMedia, "DAT_myParetoMedia2.rds")
      saveRDS(myParetoMedian, "DAT_myParetoMedian2.rds")
      saveRDS(myPareto3Q, "DAT_myPareto3Q3.rds")
      saveRDS(myParetoMax, "DAT_myParetoMax2.rds")
      saveRDS(myParetoThr, "myParetoThr.rds")
      
   #   mod2 <- readRDS("datosDCAST.rds")
   #   myPareto3Q  <- readRDS("DAT_myPareto3Q2.rds")  
      file <- paste0(OUT_DIR,"/paretos/","DAT_myPareto3Q.rds")
      myPareto3Q<-readRDS(file)

      
    ##################################
    ##### 10. DIBUJANDO PARETOS INDIVIDUALES
    ###############################   
      #UN PARET0 concreto (en un fichero o en pantalla)
      Plot_Pareto(OUT_DIR,myParetoMedia,"F","F","mean")  #the second parameter: T: create a jpeg file.  F: don't create a jpeg file. El tercer parametro, es el rescalado a 0-1. El ultimo parametro es la escala de las concentraciones
      Plot_Pareto(OUT_DIR,myParetoMedian,"F","F","median") 
      Plot_Pareto(OUT_DIR,myPareto3Q,"F","T","3Q")  #Cada una tiene su escala para las concentraciones: mean, median, max, 3Q
      Plot_Pareto(OUT_DIR,myParetoMax,"F","F","max")
      Plot_Pareto(OUT_DIR,myParetoThr4,"F","F","treshold")

      
 #     Plot_Pareto(OUT_DIR,myPareto3Q_1_1_1_1_1_1,"F","F","median")
      
    ##################################
    ##### 11. COMPARACION DE PARETOS   
    ##################################
      #Compara varias ejecuciones del algoritmo en las que se ha empleado la misma metrica de agregacion para la Concentracion  
      LosParetos <- list()   #creo una lista que contendra, todos los paretos a plotear 
      LosParetos[[1]] <- myParetoMedian   #pareto con pocas generaciones
      LosParetos[[2]] <- myPareto3Q  #mas generaciones. Ambos realizados con la agregacion de la media 
   #   Plot_ComparaParetos (LosParetos)  #si no se quiere no hace falta dar nombres a los paretos
      NamesPar <- c("myParetoMedian", "myPareto3Q")
      Plot_ComparaParetos (OUT_DIR,LosParetos,NamesPar,"F","3Q")   #le mandamos los paretos en forma de lista, asi no es problema cuantos se pueden comparar
      
      
    ##################################
    ##### 12. CALCULA LAS ESTRATEGIAS DE UN FRENTE DE PARETO DE ACUERDO A OTRAS METRICAS   
    ##################################  
      #Partimos de un Pareto, que ha sido calculado con cualquiera de las metricas de agregacion
      #   Para esas estrategias, calculamos CON TODAS LAS METRICAS la concentracion
      #1:media ; 2:median;  3:3quantile; 4:max ,  !!!!!!!QUIZAS SE PODRIA ANADIR EL PONDERADO Y EL SIN PONDERAR!!!!!
      DF_ParetMedia <- ParetoAllMetrics(myParetoMedia,BLScen)   #Le mando un pareto y me devuelve DF CON todas las estrategias evaluadas con todas las metricas
      DF_ParetMedian <- ParetoAllMetrics(myParetoMedian,BLScen)
      DF_Paret3Q <- ParetoAllMetrics(myPareto3Q,BLScen)
      DF_ParetMax <- ParetoAllMetrics(myParetoMax,BLScen)
 
      
    ##################################
    ##### 13. COMPARACION DE TODOS LOS PARETOS CON TODAS LAS METRICAS  (generados en 12)
    ##################################  
      #rutina muy especifica, sobre todo util para mis analisis, no para usuarios
      Plot_FullcompParetos(DF_ParetMedia,DF_ParetMedian,DF_Paret3Q,DF_ParetMax)

      
    ##################################
    ##### 14 BUSCAMOS LAS ESTRATEGIAS CON VALORES SIMILARES DE ESFUERZO EN CADA UNO DE LOS CASOS, parar luego dibujarlas 
    ##################################  
      valEffort <- 100000  ##Proponemos un valor de esfuerzo
      
      # Buscamos en el pareto la estrategia mas cercana a un valor de esfuerzo
      myStg_15M <- closestParetStrategByEffort(BLScen,myParetoMedian,valEffort)
      
      # Ploting Effor distribution of the Strategy (by regions and sectos)
      name <- "Strg 11 3Q"
      BarPlot_Stg_Effort(BLScen,myStg_15M,name,rates="T") #anadir que en lugar mediante porcentajes lo devuelva en valores absolutos
      
      
      ##############################  
    #### 15 PLOT DE UNA ESTRATEGIA
    ################################  
      # Antes de dibujar una estrategia la tenemos que evaluar
      myStg_15M_Evaluated <- EvalStg (BLScen,myStg_15M)  #La rutina que evalua necesita el BLS
      
      # Barplot concentration of the Strategy (under different agregation metrics)
      weightBYlength <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
      BarPlot_Stg_Concentration(myStg_15M_Evaluated,weightBYlength)  
      
      # Boxplot concentration of the strategy
      stgname <- "Strg"
      Boxplot_Stg_Concentration(BASE_PLOTS,myStg_15M_Evaluated,stgname) #la rutina evalua la estrategia, para
      
      
    ##################################
    ##### 16  Comparacion de las concentraciones de varias estrategias
    ################################## 
      #por ejemplo la estrategia no hacer nada (The BLScen) con la estrategia que acabamos de calculas y con otra de mucho mas esfuerzo
      LasEstrategias <- list()   #creo una lista que contendra, las estrategias
      
      LasEstrategias[[1]] <- DecodeStg_QL(BLScen, rep(0,length(myParetoMedian$parameters[1,]))  ) # Para cada Nuts y sector todo a ceros
        valEffort <- 100000  # Para ese valor de esfuerzo (busco la estrategia del pareto)
      LasEstrategias[[2]] <- closestParetStrategByEffort(BLScen,myParetoMedian,valEffort)
        valEffort <- 100000  # Lo mismo para este valor de esfuerzo
      LasEstrategias[[3]] <- closestParetStrategByEffort(BLScen,myParetoMedian,valEffort) 
      
      #BARPLOT una comparacion de esas 3 agregadas (pero podrian ser mas)
      Stgnames <- c("BLS","Strg 3","Strg 11")   #nombres de las estrategias a comparar
      typeAgr <- "median"   #Tipo de agregacion realizado a nivel de NUTS
      BarPlot_Stg_ConcentrComp(BLScen,LasEstrategias,Stgnames,typeAgr) #ahora le manda la lista con las estrategias sin evaluar y las evalua la propia rutina, por eso necesita el BLS
      
      #BOXPLOT
      Boxplot_Stg_ConcentrComp(BLScen,LasEstrategias,Stgnames)
      
      
      
    ##################################
    ##### 17  Comparacion estrategias de similar effort pero en diferentes paretos
    ##################################
      valEffort <- 1500000  ##Proponemos un valor de esfuerzo
      
      #Las guardaremos en una lista (como hicimos antes)
      EstasStrateg <- list()
      #Buscamos en el pareto la estrategia mas cercana a un valor de esfuerzo
      EstasStrateg[[1]] <- closestParetStrategByEffort(BLScen,myPareto,valEffort)
      EstasStrateg[[2]] <- closestParetStrategByEffort(BLScen,myParetoMedia,valEffort)
      
      Stgnames <- c("Stg_A","Stg_B")   #nombres de las estrategias a comparar
      typeAgr <- "median"   #Tipo de agregacion realizado a nivel de NUTS
      BarPlot_Stg_ConcentrComp(BLScen,EstasStrateg,Stgnames,typeAgr)
      typeAgr <- "3Q"   #lo mismo para el tercer cuartil
      BarPlot_Stg_ConcentrComp(BLScen,EstasStrateg,Stgnames,typeAgr)
      
      #BOXPLOT (con este no es necesario la metrica de agregacion)
      Boxplot_Stg_ConcentrComp(BLScen,EstasStrateg,Stgnames)
      
      #Esfuerzo de cada una de las estrategias
      name <- "Strg 3"
      BarPlot_Stg_Effort (BLScen,EstasStrateg[[1]],name,rates="T")
      name <- "Strg 11"
      BarPlot_Stg_Effort (BLScen,EstasStrateg[[2]],name,rates="T")
 

      
    ##############################################
    #### 18 DIBUJAMOS LOS MAPAS DE LAS CONCENTRACIONES
    ############################################
      ### 18.1 LOADING SHAPE FILES FOR THE MAPS 
        # le manda tambien el nombre de la cuenca a dibujar y hace entonces el subseting
        listTotalShapes <- Loading_MapShapes (BASE_Shapes,"Nuts2Vr2013.shp","basin.shp","catchment.shp")  #el path,nombre cuenca, el nombre del fichero de regiones, el nombre del fichero de cuencas, nombre fichero de subcuencas
        # [[1]]: NUS2 poligons
        # [[2]]: Catchment lines
        # [[3]]: Catchment poligons
      
      ### 18.2 Subseting de la region de acuerdo a la cuenca que se este analizando
        #le tengo que pasar el nombre de la cuenca y los nombres de las regiones que intersectan la cuenca (las que optimizamos), y el total de los shapes
        newSCEN <- EvalStg (BLScen,Mtx_strateg)  #necesitamos un escenario evaluado para hacerlo
        BasinName <-"Adige"
        LstMyShapes <- Subsetingshapes (BasinName,newSCEN[[1]]$TheNuts, listTotalShapes)
      
      ### 18.3 Mapping the CONCENTRATION for one strategy (previamente he hecho el subseting, podria hacerse desde esta funcion)
          #Selecciona una estrategia y la evalua antes de mandarla al mapa
          theParetoToMap <- myParetoMedian #para el pareto que este considerando
          valEffort <- 100000  #Buscamos la estrategia cercana a un esfuerzo
          Estrategia1 <- closestParetStrategByEffort(BLScen,theParetoToMap,valEffort)  #LA ESTRATEGIA 1  
          Scen_Estrategia1 <- EvalStg (BLScen,Estrategia1) 
          
        # 18.3.1 Primero necesito la concentracion agregada de la estrategia que quiero plotear
        weightBYlength<-"F"     #F: false (sin ponderar); A: by area ; DA: by Drained Area
        ### LORO : tengo que retocar la siguiente funcion para que devuelva tambien la solucion con treshold
        newConcbyNUTW <- NutsConcFromSubc(Scen_Estrategia1[[1]] ,weightBYlength)   
        newConcbyNUTW
        newConcbyNUTW[,3:6] <- lapply(newConcbyNUTW[,3:6] , round, 1)  #reduzco numero de decimales para imprimir
        
        # 18.3.2 Ahora lo mando a Map
        BasinName <-"Adige"
        ConcenbyNuts <- newConcbyNUTW 
        AgrIndex <- 2     # 1:mean; 2: median; 3: 3Q; 4:Max; 5: Threshold    #tipo de agregacion de con la que se ha calculado las concentraciones 
        NumClassInterv <- 8    #  Numero de categorias a la hora de dibujarlo
        paleta <-  "YlGnBu"    #  "YlGnBu"   RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral  "BuPu" 
        estilo <-  "jenks" #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
        Map_RegConc(BasinName,LstMyShapes,ConcenbyNuts,AgrIndex,NumClassInterv,paleta,estilo)
      
          
        
      ### 18.4. MAPPING COMPARING concentration of one strategy  wiht concentration in the  Base Line
        valEffort <- 100000  #Buscamos la estrategia cercana a un esfuerzo
        Estrategia1 <- closestParetStrategByEffort(BLScen,theParetoToMap,valEffort)  #LA ESTRATEGIA 1

        # 18.4.1 LLama a la funcion que hace las evaluaciones, la diferencia, y las pinta en el mapa. 
        #  En lugar de pasar la concentracion pasa la estrategia a comparar con la BLS
        AgrIndex <- 2     # 1:mean; 2: median; 3: 3Q; 4:Max; 5: Threshold    #tipo de agregacion de con la que se ha calculado las concentraciones 
        Map_RegConcComp(BasinName,LstMyShapes,BLScen,Estrategia1,AgrIndex,NumClassInterv,paleta,estilo)
        
        
        
    ############################################
    #### 19 MAPPING Effor distribution of the Strategy  by REGIONS and SECTORS
    ############################################
        
      ### 19.1. Como Barplot (funcion previamente probada) 
      theParetoToMap <- myParetoMedian #para el pareto que este considerando
      valEffort <- 100000  #Buscamos la estrategia cercana a un esfuerzo
      Estrategia1 <- closestParetStrategByEffort(BLScen,theParetoToMap,valEffort)  #LA ESTRATEGIA 1  
      name <- "Strg 2 median"
      BarPlot_Stg_Effort(BLScen,Estrategia1,name,rates="T")
        
         
      ### 19.2. Como MAPA 
      AgrIndex<-2
      Map_RegEffortRed(BasinName,LstMyShapes,BLScen,Estrategia1,NumClassInterv,paleta,estilo,rates="T")  #anadir que en lugar mediante porcentajes lo devuelva en valores absolutos


      ### 19.3. Como MAPA formato SPPLOT
      Map_RegEffortRed_SPPLOT(BasinName,LstMyShapes,BLScen,Estrategia1,NumClassInterv,paleta,estilo,rates="F")  
      
          
      
      
      
          
          
          
        
        ### 5. DIBUJANDO UN PERFIL CON LA CONCENTRACION ACTUAL EN CADA SUBCUENCA (lo hago pasando una estrategia nula)
        #Cualquier escenario antes de hacer las figuras lo debo evaluar (incluso el BLS, porque empleo la columna de new conce)
        # Para el BLS, lo que hago es no aplicar reducciones Debe ser la evaluacion by subcuencas
        newSCEN <- EvalStg (BLScen,Mtx_strateg) 
        
        
        
          
          
          # 18.5 Mapa de distribucion de presiones
          
          
          
          
      

      
      
      
      
      
      
      

      ## las subcuencas        
        #### TIENE EL PROBLEMA DE QUE TARDA MUCHO EN PINTARLA
        PathShape <- "D:/HISTORICO/2017SEPTIEMBRE/Globaqua/Shape/catchment" 
        Shapename <- "catchment.shp" 
      
          loadshapepath <- paste(PathShape,Shapename,sep="/")
          MyPoligons <- readShapePoly(loadshapepath)
    
          class(MyPoligons)
          plot(MyPoligons)
          plot(MyPoligons,xlim=c(3152100, 3852300), ylim=c(1920600,2438400))
      

        ########################
        ##### LAS REGIONES
        #####################
          # 18.2 Evalua una estrategia (concentracion por regiones)
          weightBYlength<-"T"    #T: true, F: false
          newConcbyNUTW <- NutsConcFromSubc(newSCEN[[1]] ,weightBYlength)   #Ponderando por longitud
          newConcbyNUTW
          newConcbyNUTW[,3:7] <- lapply(newConcbyNUTW , round, 1)  #reduzco numero de decimales para imprimir
          
          
          # 18.3 Load the shape
        PathShape <- "D:/HISTORICO/2017SEPTIEMBRE/Globaqua/Shape"
        Shapename <- "Nuts2Vr2013.shp"     
        loadshapepath<-paste(PathShape,Shapename,sep="/")
        MyPoligonsR <- readShapePoly(loadshapepath)
        class(MyPoligonsR)
        plot(MyPoligonsR)
        IDs <- sapply(slot(MyPoligonsR, "polygons"), function(x) slot(x, "ID"))
        summary(MyPoligonsR)
        plot(MyPoligonsR,xlim=c(3152100, 3852300), ylim=c(1920600,2438400))   # se puede dibujar solo parte del mapa
        summary(MyPoligonsR)
       # attributes(MyPoligonsR)
        attributes(MyPoligonsR@data)
        
        MyPoligonsR@data$NUTS_ID  #eS es el id de las regiones
        
        IDs <- sapply(slot(MyPoligonsR, "polygons"), function(x) slot(x, "ID"))
        
              

    
    
        
      
      
      
      
      #PINTO LOS PERFILES DE ALGUNA SOLUCION SELECCIONADA
      indexStSelected <- c(4)
      myPareto$objectives[indexStSelected,] #la estrategia seleccionada
      temp<-myPareto$parameters[indexStSelected,] 
      cutpoint<-length(temp)/4  #consideramos para cada tipo de presion mismo numero de Nuts
      lst_Reductions <- list( temp[1:cutpoint], temp[(cutpoint+1):(2*cutpoint)], temp[(2*cutpoint+1):(3*cutpoint)], temp[(3*cutpoint+1):(4*cutpoint)]  )     #separamos en cada tipo de presion
      names(lst_Reductions[[1]]) <-BLScen$TheNuts_EF      #les ponemos en nombre del NUTS para luego evaluarlas
      names(lst_Reductions[[2]]) <-BLScen$TheNuts_EF  
      names(lst_Reductions[[3]]) <-BLScen$TheNuts_EF  
      names(lst_Reductions[[4]]) <-BLScen$TheNuts_EF  
      
      
      newBLS <- EvalStg (BLScen,lst_Reductions) #calcula la concentracion de una solucion
      Two_Prof_Conc_Plots(OUT_DIR,BasinName,BLScen$DFConcentration,newBLS)
      
      
      
      #En la interface tambien hay que poner cuanto es el % maximo de cada tipo en cada NUTS que se puede reducir
      
      #otra funcion para dibujar sobre el mapa
      
      #sensibilidad
      
      # LOADING THE PRESURES REDUCTION BY NUTS & TYPE OF PRESSURES 
      
      
      # THE TWO OBJECTIVES: EFFORT REDUCTION   &  STATUS
      
      
      
      
      
    
    
                  
                  
  #perfil sencillo y comparativo con el BLS
  Single_Prof_Conc_Plots(OUT_DIR,BasinName,newBLS)
  Two_Prof_Conc_Plots(OUT_DIR,BasinName,BLScen$DFConcentration,newBLS)
  
  
  #EVALUA LA REDUCCION DE PRESSIONES (que como suma de N reducido multiplicado por un peso que representa la dificultad comparativa )
      #the strategy  
      PresRed_MinN <- rep(20,length(BLScen$TheNuts_EF))    # Nitrates mineral agricola
      PresRed_ManN <- rep(30,length(BLScen$TheNuts_EF))    # Nitrates manure agricola
      PresRed_SdN <- rep(10,length(BLScen$TheNuts_EF))     # Nitrates scatter dweling 
      PresRed_PsN <- rep(5,length(BLScen$TheNuts_EF))     # Nitrates point sources (urban & industrial) 
      
      names(PresRed_MinN) <-BLScen$TheNuts_EF  #pongo el nombre de cada NUTS a cada reduccion de presiones
      names(PresRed_ManN) <-BLScen$TheNuts_EF
      names(PresRed_SdN) <-BLScen$TheNuts_EF
      names(PresRed_PsN) <-BLScen$TheNuts_EF
      
      List_strateg <- list(PresRed_MinN,PresRed_ManN,PresRed_SdN,PresRed_PsN )
  
   #evalua the pressure reduction of the strategy    
   stg_pres_red <- sum( Eval_PressRed (BLScen,List_strateg) )
   #una reduccion de nitratos en cada subcuenca (cada una podria tener un coste diferente)
   #tambien podrian computarse como % de reduccion
  
 

 

 