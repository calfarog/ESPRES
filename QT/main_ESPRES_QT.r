#---
################################################################################
# Author: Angel Udias    all complaints to: angel.udias-moinelo@ec.europa.eu  #
################################################################################
# Started: 21-Jan-2018 ->    ;                                                 #
# Updates:                                                                     #
#                                                                              #
# ESPRES: Efficient Strategies for anthropogenic Pressure Reduction in European waterSheds                                                                             #
#              Main file 
# ## THE QUANTITY MODULES ##
# date: ", April 5, 2016"
# upgrade: July, 31, 2018"
#---

#############################################################################
#############################################################################
### MENU SHINY: INICIO DE LA APLICACION                             #######
############################################################################
############################################################################



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
### MENU SHINY:  SELECCION MODULO DE CANTIDAD QT                      #######
############################################################################
############################################################################



############################  
### 1. Pathway 
############################  
  PATH_NAME <- "D:/HISTORICO/2018SEPTIEMBRE/ESPRES/"       
  SCRIPT_DIR 	<- paste0(PATH_NAME, "/QT/CODE/V8/")
  BASE_IN_DATA <- paste0(PATH_NAME, "/QT/data/")   
  BASE_PLOTS	<-paste0(PATH_NAME, "/plots/")
  BASE_Shapes <- paste0(BASE_IN_DATA, "Shapes")

############################    
### 2. LOADING SOURCE CODE
############################  
  source(paste0(SCRIPT_DIR, "loadData_ESPRES_QT.r"))
  source(paste0(SCRIPT_DIR, "createScen_ESPRES_QT.r"))
  source(paste0(SCRIPT_DIR, "evalStg_ESPRES_QT.r"))
  source(paste0(SCRIPT_DIR, "mooFunc_ESPRES_QT.r"))
  source(paste0(SCRIPT_DIR, "plotFunc_ESPRES_QT.r"))
  source(paste0(SCRIPT_DIR, "plotMaps_ESPRES_QT.r"))

 # source(paste0(SCRIPT_DIR, "FuncionesGLOB.r"))
  
  
############################ 
### 3. LOADING LIBRARIES
############################ 
  Loading_libraries()
  
  
##################################
### 4. LOADING DATA  (water QUANTITY info)
##################################  

  ### 4.1 LOS NUTS2 Y LOS CATCHMENTS Y SU RELACION (INTERSECCION)
  ncname <- "WEI_optimisation_data"
  path_ncdf <- paste0(BASE_IN_DATA, "/NCDFData/")   
  List_WQnI <- Loading_WQTnInfo(path_ncdf,ncname)
  #  Nuts2ObjID,CatchmentID,intersec_fracc,r_FresW,r_DomestWD,r_IndustWD,r_EnergWD,r_LivesWD,r_IrrWD,c_area
        # Nuts2ObjID  <-List_WQnI[[1]]  #ATENCION que son MATRICES no Data Frames
        # head(Nuts2ObjID)
        # CatchmentID <-List_WQnI[[2]]    #from 2000001  to 92003782
        # intersec_fracc<- List_WQnI[[3]]
        # head(intersec_fracc)
        # r_fresW <-List_WQnI[[4]]
        # head(List_WQnI[[4]])
        # sum(List_WQnI[[4]])
        # class(r_domestWD)
        # length((r_fresW))
        # head(r_fresW)
        indexCol <-2
        indexCatchFW <- which(r_fresW[indexCol]!=0,arr.ind = T) 
        indexCatchDW <- which(r_domestWD[,indexCol]!=0,arr.ind = T)  #el resultado serian las filas de los catchmet sobre los que esa provincia tiene efecto
        indexCatchIW <- which(r_industWD[,indexCol]!=0,arr.ind = T) 
        
        for (indexrow in 5000:5500){
            indexColNUT <- which(r_fresW[indexrow,]!=0,arr.ind = T)
            print(paste0(indexrow," -  ",indexColNUT))  #el resultado son todas las provincias que influyen sobre un NUTS
        }      
        #Si sumo todas las columnas, tengo el total de agua que hay en cada subcuenca
        
        # r_domestWD <- List_WQnI[[5]]
        # head(List_WQnI[[5]])
        # r_industWD <- List_WQnI[[6]]
        # head(List_WQnI[[6]])
        # r_energWD  <- List_WQnI[[7]]
        # r_livesWD <- List_WQnI[[8]]
        # r_irrWD <- List_WQnI[[9]]
        # c_area  <- List_WQnI[[10]]
  
  #EL FICHERO ANTERIOR TIENE UN PROBLEMA, POR AHORA USAMOS EL SIGUIENTE
#  setwd("D:/HISTORICO/2018SEPTIEMBRE/ESPRES/QT/CODE/V6")
  # save(List_WQnI,file="List_WQnI.Rda")
#  load("List_WQnI.Rda")    #por ahora tengo que seguir usando esta fuente de datos
  
  
  ## 4.2 LOADING SHAPE FILES FOR ALL THE MAPS   (Cargando contornos de NUTS2 y SubCuencas para hacer mapas)
  NUTS2_shape <- "Nuts2/Nuts2Vr2013.shp"
  Catch_shape <- "catchment_polygons/catchment_polygons.shp"
  Basin_shape <- "basin/basin.shp"
  listTotalShapes <- Loading_MapShapes (BASE_Shapes,NUTS2_shape,Basin_shape,Catch_shape)  #el path,nombre cuenca, el nombre del fichero de regiones, el nombre del fichero de cuencas, nombre fichero de subcuencas
  # [[1]]: NUTS2 poligons
  # [[2]]: NUTS2 lines
  # [[3]]: Basin pligons      
  # [[4]]: Basin lines  
  # [[5]]: Catchment poligons
  # [[6]]: Catchment lines
  # [[7]]: Informacion  limites para cada pais (para el box)
  

  ### 4.3 Loading Catchment - Subcatchment pertenence (UN EXCEL CON ESTA INFORMACION)
  filename <- "basin_catchment_lookup.csv"
  C_SC_Rel <- Loading_C_SC_Rel(BASE_IN_DATA,filename)
  #  class(C_SC_Rel)
  #  head(C_SC_Rel)
  #  class(C_SC_Rel$basin_name)
  #  class(C_SC_Rel$cathment_hydroid)
  # unique(C_SC_Rel$basin_name)
  
  ### Loading Nuts ID Emiliano with the general Nuts ID names and long names
  filename <- "Nuts_correspond.csv"
  NUTS_Corr <- Loading_NUTS_Corr(BASE_IN_DATA,filename)


  
  
#############################################################################
#############################################################################
### MENU SHINY:  SELECCION CUENCA                                   #######
############################################################################
############################################################################
  
  
############################  
### 5. SELECTION OF THE BASIN
############################ 
  BasinName <-"Sava"       # "Danube"  Evrotas     Ebro   Adige     Danube   Sava
  
  
  
############################  
### 6. CREATE BLS (SUBSETING FOR THE BASIN SELECTED)
############################ 
  
  ### ATENCIONR: las dos siguientes lineas ya no se usan y con ello tampoco createScen_EXPRES_QT.r
        # # 6.2 lo primero sera detectar los NUTS de ese BASIN(cuenca)
        # BLS_SubCt_N2 <- NUTS2_BASIN(BasinName,List_WQnI,C_SC_Rel,Mode=1)  #los subcuencas en cada Nuts  (si esta en varios, lo asigna dentro del que tiene mas area )
        # The_N2area <- NUTS2_BASIN(BasinName,List_WQnI,C_SC_Rel,Mode=2)    # el area de cada Nuts2
        # # ATENCION que los N2 del anterior pueden no ser exactamente iguales que los del escenario (dependen del listado C_SC_Rel)
  
  
  # 6.2 BLS para la cuenca en estudio (nos da el fresW y cada uno de los usos en sus subcuencas y a que NUT pertenecen). tambien [[7]] la demanda sin ruteo por nuts y tipo de uso
  # lo hace como subseting de todo los datos para el Basin considerado
    BLScen <- GenerateBLS (BasinName,List_WQnI, C_SC_Rel,NUTS_Corr)  
    The_N2 <- sort(as.character(unique(BLScen[[6]]$N2)))  #para ordenar bien los tengo que convertir
    sum(BLScen[[6]]$fresW)   #esto seria la suma de todo el fresWater
    # Calcula cual es la demanda (NO ROUTING) de cada region para cada sector     # Guardo ese valor de DEMANDA NO ROUTING, como [[7]] elemento del BLScen
    BLScen[[7]] <- ComputeNotRoutDemand(BLScen)  #ya queda guardada en el BLS
  
 # write.csv(BLScen[[6]], file = "EbroDomesticRoutingDemand.csv")    
#  head(C_SC_Rel)
#  C_SC_Rel[ C_SC_Rel$basin_name=="Sava", ]  #los subcuencas del Sava
#  count(C_SC_Rel$basin_name=="Sava")
#  lista <- which(is.na(BLScen$N2))
#  unique(BLScenGood$N2[lista])
  

  
  
  ### 6.3 Subseting de la region de acuerdo a la cuenca que se este analizando
  #le tengo que pasar el nombre de la cuenca y los nombres de las regiones que intersectan la cuenca (las que optimizamos), y el total de los shapes
  LstMyShapes <- Subsetingshapes_QT (BasinName,The_N2,listTotalShapes,C_SC_Rel)
  #  LstMyShapes[[1]] : Poligonos de las Regiones NUTS2 
  #  LstMyShapes[[2]] :  Lineas de las regiones   NUTS2 
  #  LstMyShapes[[3]] : Poligonos del Basin
  #  LstMyShapes[[4]] : Lineas del Basin
  #  LstMyShapes[[5]] : Poligonos de las subcuencas
  #  LstMyShapes[[6]] : Lineas de las Subcuencas
  #  LstMyShapes[[7]] : Limites para los mapas de cada pais

  

#############################################################################
#############################################################################
### MENU SHINY:  SUMMARY - General - Subcatchments                  #######
############################################################################
############################################################################  
  
  Map_SubCatch_new(BasinName,LstMyShapes)
  
  #El resto de info que poneis en esa pestana, lo revisamos, pero creo que estan en la funcion de summary que lue
  

############################################################################
############################################################################
### MENU SHINY:  SUMMARY - General - NUTS2 Subcatchent               #######
############################################################################
############################################################################  
  
  ## Nueva version de la funcion para el mapa (que me gustaria que fuesen 2 mapas ,quizas con tab)      
  label <- TRUE
  Map_N2_SubCatch_Tab1(BasinName,LstMyShapes,label)      
  Map_N2_SubCatch_Tab2(BasinName,LstMyShapes,label)    
    
  
  ############################  
  ###  EVALUATE ONE STRATEGY (the cero reductions: BLS)
  ############################   
  #  BLScen <- readRDS(file = "Evalfresh.rds")
  
  ## 7.1 GENERO UNA ESTRATEGIA DE REDUCCION NULA
  dimx <- length(The_N2)  #el numero de columnas de la matriz (una por cada NUTS)
  My_strateg <- matrix( c( rep(0,dimx),     # domestWD   
                            rep(0,dimx),      # Energ
                            rep(0,dimx),       # Indust
                            rep(0,dimx),      # Irrig
                            rep(0,dimx)),      # Livest
                         byrow = TRUE, ncol = dimx)
  rownames(My_strateg) <- c("Domest","Energ","Indust","Irrig","Livest")  #cada fila es un tipo de presion
  colnames(My_strateg) <- The_N2
  
  ## 7.2 EVALUACION (Aplica dichas reducciones)(aunque en realidad es estrategia nula)
  # My_strateg[1,1]<- 10
  Evaluated_BLScen <- EvalStg (BLScen,My_strateg)  #devuelve [[1]]el nuevo escenario de demandas acumuladas, y [[2]]:La reduccion de demandas NO ACUMULADAS, para el calculo de esfuerzos  [[3]]la diferencia de dichas demandas con relacion al BLS
      head(BLScen)
      head(Evaluated_BLScen[[1]])      #  Nuevo escenario de demandas acumuladas
      head(Evaluated_BLScen[[2]])      #  Reduccion de demandas NO ACUMULADAS
      head(Evaluated_BLScen[[3]])

 
  #Comparo las demanda con routing y las sin routing (Para ello, empleao el scenario de 0 reducciones)
  aggregate(Evaluated_BLScen[[1]][,2:8], by=list(N2=Evaluated_BLScen[[1]]$N2), FUN=sum)  #con acumulado 
  BLScen[[7]]  #sin acumular    Esta se usa para esfuerzo y la anterior para el WEI
  
  
  ## 7.3 SUMARIO DE LAS AREAS DE LAS SUBCUENCAS DEL BLS     
  Summary_Scenario_Area_QT(Evaluated_BLScen)   
   
  ## Ejemplo de calculo del esfuerzo de reduccion
  ComputeEffort(Evaluated_BLScen,"D" )     #es exactamente lo mismo que Evaluated_BLScen[[2]]
  
       
        
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Water Availability - Map by Subcatchmets   #######
######################################################################################
######################################################################################    
    
  #lo que queremos es mapear una estrategia evaluada (usamos la que acabamos de evaluar)
  Evaluated_BLScen[[1]]$fresW   #esa es la variable freswater
  length(Evaluated_BLScen[[1]]$fresW)

  getwd()
#  Evaluated_BLScen[[1]] <- readRDS(file = "Evalfresh.rds")
  
  #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
  NumClassInterv <- 7    #  Numero de categorias a la hora de dibujarlo
  paleta <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
  estilo <- "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
  scale <- "logarithmic"      # "linear"   "logarithmic"
  scaleRelation <- "Independent"   #  "Common"   "Independent"
  
  #Para este mapa, cambiamos la funcion del codigo antiguo por esta nueva (que me esta gustando mas como queda)
  ListIndexUses <- c(1)  #el fresh water es el uso 1
  Map_var_SubCatch_SPPLOT (BasinName, LstMyShapes, Evaluated_BLScen[[1]], ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation )
  
  # For the report
  boxplot(Evaluated_BLScen[[1]]$fresW~Evaluated_BLScen[[1]]$N2,outline=FALSE, las=1 )   #,ylim=c(0,100000000)
  Fresh<-aggregate(Evaluated_BLScen[[1]][2], by=list(Category=Evaluated_BLScen[[1]]$N2), FUN=median)
  Fresh[,2]<-Fresh[,2]/1000000
  Fresh
  
     

#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Water Availability -  Box Plots      #######
######################################################################################
######################################################################################   
  
  # Boxplot del valor del fresh Water 
  namebox <- paste(BasinName," ", "BLScen"," strategy")
  # vosotros ya teneis una funcion que lo hace con plotly)  y ese es perfecto. En cualquier caso dejo la llamada a mi funcion que genera varios boxplot
  Boxplot_Stg_Extracciones(BASE_PLOTS,Evaluated_BLScen,namebox) # PLOTS ( que evaluar stg antes de hacer el plot)
  

  
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - cUMULATED Water Availability - Map by Nuts    #######
######################################################################################
######################################################################################

  # Primero necesito el valor de la variable agregada (FRESH WATER)
  weightBY <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
  ### LORO : Devuel una lista:([[1]] fresW, [[2]]domestic, [[3]]Ener, [[4]]Industria,   [[5]]Irri , [[6]]Livest)
  newExtraccNUT <- NutsExtractionFromSubc( Evaluated_BLScen[[1]] ,weightBY)
  
  # FRESH WATER  [[1]]
  newExtraccNUT[[1]] 
  
  #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
  NumClassInterv <- 6    #  Numero de categorias a la hora de dibujarlo
  paleta <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
  estilo <-  "kmeans"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
  scale <- "linear"      # "linear"   "logarithmic"
  agregation <- "Median"         # "Mean"     "Median"      "Q75"         "Max"  
  scaleRelation <- "Independent"   #  "Common"   "Independent"   #para el fresh Water no cambiaria,porque es solo una variable
  
  # Le mandamos los indices del uso o los usos a mapear, en este caso solo fresh water
  ListIndexUses <- c(1)     # 1: "Fresh Water", 2: "Domestic Water", 3: "Industrial Water", 4: "Energy Water", 5: "Livestock Water", 6: "Irrigation Water")
#     Map_var_NUTS_SPPLOT (BasinName, LstMyShapes, newExtraccNUT,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
  mytype<-"WA"      # WA: Water Availability; WU: Water Uses; WEI: Water Extraction Index; EF: Effort
  TheVariables <- Prepro_to_Map_N2(mytype,newExtraccNUT,scale,agregation)
  Map_var_NUTS_SPPLOT (BasinName, LstMyShapes,mytype, TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
  
  # este mapa se podria hacer tambien con la funcion   Map_RegWater_QT (aunque creo que me gusta mas este el otro recorta los NUTS con el contorno del Basin)
  

  
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Water Availability - BarPlot      #######
######################################################################################
######################################################################################
  
  #Primero el barplot (la funcion calcula el valor agregado de la variable). (FRESH WATER)
  indexofPresure <- 1 # 1: "Fresh Water", 2: "Domestic Water", 3: "Industrial Water", 4: "Energy Water", 5: "Livestock Water", 6: "Irrigation Water")
  weightBYlength <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
  BarPlot_Stg_Extracciones(Evaluated_BLScen,weightBYlength,indexofPresure)    # Hace un barplot para cada tipo: Devuel una lista: fresW, domestic, Industria, Ener, Livest, Irri

  #Para La tabla   necesito el valor de la variable agregada (FRESH WATER)
  weightBY <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
  ### LORO : Devuel una lista: ([[1]] fresW, [[2]]domestic, [[3]]Ener, [[4]]Industria,   [[5]]Irri , [[6]]Livest)
  newExtraccNUT <- NutsExtractionFromSubc( Evaluated_BLScen[[1]] ,weightBY)
  
  #  ### LORO : Devuelve una lista: [[1]] fresW, [[2]]domestic, [[3]]Industria, [[4]]Ener, [[5]]Livest, [[6]]Irri
  # Reduzco el numero de decimales
  newExtraccNUT[[indexofPresure]]  #
  newExtraccNUT[[indexofPresure]][,3:6] <- lapply(newExtraccNUT[[indexofPresure]][,3:6] , round, 1)  #reduzco numero de decimales para imprimir
  
  # getwd()
  # write.csv(newExtraccNUT, file = "ConctAreaSumary.csv")
  
             
      ## 7.4 SUMARIO DE LAS Extracciones DE LAS SUBCUENCAS (Y MAPAS) DEL BLS
     # namebox <- paste(BasinName," ", "BLScen"," strategy")
    #  Summary_Scenario_Water_QT(BASE_PLOTS,Evaluated_BLScen,namebox,BasinName,LstMyShapes) 
  
  

  
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Water Uses - Map by Subcatchmets   #######
######################################################################################
######################################################################################    
  
  #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
  NumClassInterv <- 7    #  Numero de categorias a la hora de dibujarlo
  paleta <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
  estilo <-  "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
  scale <- "logarithmic"      # "linear"   "logarithmic"
  scaleRelation <- "Common"   #  "Common"   "Independent"
  
  #Queremos mapear todos los otros usos 
  Evaluated_BLScen[[1]]
  
  #Si la escala es Independiente, lanza los usos seleccionados uno a uno 
  ListIndexUses <- c(2,3,4,5,6)     # 1: "Fresh Water", 2: "Domestic Water", 3:"Energy Water" , 4: "Industrial Water" , 5: "Irrigation Water" , 6:"Livestock Water" )
  Map_var_SubCatch_SPPLOT (BasinName, LstMyShapes, Evaluated_BLScen[[1]], ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation )
  # Tarda un poco en generarlo
  
  
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Water Uses - 	Box Plots      #######
######################################################################################
######################################################################################   
  
  # Boxplot del valor de los usos (sin considerar el fresh water)
  namebox <- paste(BasinName," ", "BLScen"," strategy")
  # vosotros ya teneis una funcion que lo hace con plotly)  y ese es perfecto. En cualquier caso dejo la llamada a mi funcion que genera varios boxplot
    Boxplot_Stg_Extracciones(BASE_PLOTS,Evaluated_BLScen,namebox) # PLOTS ( que evaluar stg antes de hacer el plot)
           
    #para los report
     boxplot(Evaluated_BLScen[[1]][,3:7],las=1,names = c("Domestic","Energy","Industrial","Irrigation","Livestock") ,
             col=c("snow2"),  outline = FALSE )
     Demands<-aggregate(Evaluated_BLScen[[1]][3:7], by=list(Category=Evaluated_BLScen[[1]]$N2), FUN=sum)
     Demands[,2:6]<-Demands[,2:6]/1000000
     Demands
     
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Water Uses - Map by Nuts    #######
######################################################################################
######################################################################################
 
  # Primero necesito el valor de la variable agregada (FRESH WATER)
  weightBY <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
  ### LORO : Devuel una lista: ([[1]] fresW, [[2]]domestic, [[3]]Ener, [[4]]Industria,   [[5]]Irri , [[6]]Livest)
  newExtraccNUT <- NutsExtractionFromSubc( Evaluated_BLScen[[1]] ,weightBY)
  
  # Ahora no es el fresWater [[1]], son todos los otros:
  # strWater<-c("Fresh Water", "Domestic Water", "Industrial Water", "Energy Water", "Livestock Water","Irrigation Water")
  # newExtraccNUT 
  
  #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
  NumClassInterv <- 8    #  Numero de categorias a la hora de dibujarlo
  paleta <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
  estilo <-  "kmeans"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
  scale <- "linear"      # "linear"   "logarithmic"
  agregation <- "Mean"         # "Mean"     "Median"      "Q75"         "Max"  
  scaleRelation <- "Independent"   #  "Common"   "Independent"
  
  mytype<-"WU"
  TheVariables <- Prepro_to_Map_N2(mytype,newExtraccNUT,scale,agregation)
  # Le mandamos los indices del uso o los usos a mapear,  e indicamos que metrica quiere
  ListIndexUses <- c(2,3,4,5,6)     # 1: "Fresh Water", 2: "Domestic Water", 3: "Industrial Water", 4: "Energy Water", 5: "Livestock Water", 6: "Irrigation Water")
  Map_var_NUTS_SPPLOT (BasinName, LstMyShapes, mytype,TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
  
  # este mapa se podria hacer tambien con la funcion   Map_RegWater_QT (aunque creo que me gusta mas este el otro recorta los NUTS con el contorno del Basin)
  
  
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - Water Uses - BarPlot      #######
######################################################################################
######################################################################################  
  
  #Vuestro desplegable es el que dara el indice del uso que quremos que se represente (seran todos mennos fresW. de; 2 a 8)
  #Primero el barplot (la funcion calcula el valor agregado de la variable). (FRESH WATER)
  indexofPresure <- 2 # 1: "Fresh Water", 2: "Domestic Water", 3: "Industrial Water", 4: "Energy Water", 5: "Livestock Water", 6: "Irrigation Water")
  weightBYlength <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
  BarPlot_Stg_Extracciones(Evaluated_BLScen,weightBYlength,indexofPresure)    # Hace un barplot para cada tipo: Devuel una lista: fresW, domestic, Industria, Ener, Livest, Irri
  
  #Para La tabla   necesito el valor de la variable agregada (FRESH WATER)
  weightBY <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
  ### LORO : Devuel una lista: ([[1]] fresW, [[2]]domestic, [[3]]Ener, [[4]]Industria,   [[5]]Irri , [[6]]Livest)
  newExtraccNUT <- NutsExtractionFromSubc( Evaluated_BLScen[[1]] ,weightBY)
  
  #  ### LORO : Devuelve una lista: [[1]] fresW, [[2]]domestic, [[3]]Industria, [[4]]Ener, [[5]]Livest, [[6]]Irri
  # Reduzco el numero de decimales
  newExtraccNUT[[indexofPresure]]  #
  newExtraccNUT[[indexofPresure]][,3:6] <- lapply(newExtraccNUT[[indexofPresure]][,3:6] , round, 1)  #reduzco numero de decimales para imprimir
  
  
  
  
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - WEI - Map by Subcatchmets     #######
######################################################################################
######################################################################################  
  
  ## Por definicion el WEI debe ser calculado con todas las extracciones
  # A mi me parece interesante la funcionalidad de dar el WEI sin algun tipo de demanda
  # lo cual nos permite ademas mantener una semejanda con el resto de pestanas 
  # En cualquier caso, la interface por defecto considera todos los usos seleccionados
  
  ## CALCULA EL WEI para cada subcuenca (para el BLS, o un Scenario al que se han aplicado reducciones)
  # la lista siguiente depende de lo que el usuario marque en vuestra interface
    # ListWDemToWEI <- c("Domestic", "Industrial", "Energy", "Livestock", "Irrigation")
  ListWDemToWEI <- c("Domestic", "Energy", "Industrial", "Irrigation", "Livestock")
  BLS_WEI <- Calcula_WEI(Evaluated_BLScen[[1]],ListWDemToWEI)   # Devuelve los WEI de cada subcuenca, considerando todas las demandas simultaneamente 

  #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
  NumClassInterv <- 9    #  Numero de categorias a la hora de dibujarlo
  paleta <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
  estilo <-  "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
  scale <- "linear"      # "linear"   "logarithmic"
  scaleRelation <- "Independent"   #  "Common"   "Independent"
  
  #Para que siga funcionando la funcion de los mapas, le pasamos el data frame que anade el WEI, 
  # indicando el indice de la columna del WEI (la 10)
  ListIndexUses <- c(9)     # 1: "Fresh Water", 2: "Domestic Water", 3: "Industrial Water", 4: "Energy Water", 5: "Livestock Water", 6: "Irrigation Water" , 9: WEI)
  Map_var_SubCatch_SPPLOT (BasinName, LstMyShapes, BLS_WEI, ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation )
  
  
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - WEI - 	Box Plots      #######
######################################################################################
######################################################################################  
  
  ## CALCULA EL WEI para cada subcuenca (para el BLS, o un Scenario al que se han aplicado reducciones)
  # la lista siguiente depende de lo que el usuario marque en vuestra interface
  # ListWDemToWEI <- c("Domestic", "Industrial", "Energy", "Livestock", "Irrigation")
  ListWDemToWEI <- c("Domestic", "Energy", "Industrial", "Irrigation", "Livestock")
  BLS_WEI <- Calcula_WEI(Evaluated_BLScen[[1]],ListWDemToWEI)   # Devuelve los WEI de cada subcuenca, considerando todas las demandas simultaneamente 
  
  # Boxplot de los WEI
  par(mfrow=c(1,2))
  boxplot(BLS_WEI$WEI~BLS_WEI$N2 ,ylab="WEI", las=2, col=c("snow2"),
          outline=TRUE, ylim=c(0,max(BLS_WEI$WEI)),main=" WEI" )
  
  boxplot(BLS_WEI$WEI~BLS_WEI$N2 ,ylab="WEI", las=2,col=c("snow2"),
          outline=FALSE, main="WEI Without ouliers" )
  
  
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - WEI - Map by Nuts    #######
######################################################################################
######################################################################################
  
  ## CALCULA EL WEI para cada subcuenca (para el BLS, o un Scenario al que se han aplicado reducciones)
  ListWDemToWEI <- c("Domestic", "Industrial", "Energy", "Livestock", "Irrigation")  #lista de los usos para calcular el WEI
  BLS_WEI <- Calcula_WEI(Evaluated_BLScen[[1]],ListWDemToWEI)   # Devuelve los WEI de cada subcuenca, considerando todas las demandas simultaneamente 
  
  ## AGREGACION del WEI PARA CADA NUTS2 (diferentes metricas)(y ponderando o sin poderar) 
  weightBY <-"F"     #F: false (sin ponderar); A: by area ; DA: by Drained Area ;L: longitud   (por definir si ponderar tiene sentido)
  BLS_WEI_By_N2 <- NutsWEIFromSubc (BLS_WEI,weightBY)
  
  
  #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
  NumClassInterv <- 6    #  Numero de categorias a la hora de dibujarlo
  paleta <-  "YlGnBu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
  estilo <-  "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
  scale <- "Median"      # "linear"   "logarithmic"
  agregation <- "Mean"         # "Mean"     "Median"      "Q75"         "Max"  
  scaleRelation <- "Independent"   #  "Common"   "Independent"
  
  # Le mandamos los indices del uso o los usos a mapear,  e indicamos que metrica quiere
  listWEI = list(BLS_WEI_By_N2) #en lugar de los usos, le paso el WEI con diferentes tipos de agregacion, y lo tengo que convertir a lista
  mytype<-"WEI"
  TheVariables <- Prepro_to_Map_N2(mytype,listWEI,scale,agregation)
  ListIndexUses <- c(1)    # 1: "WEI"   Con lo que su indice es directamente el 1
  Map_var_NUTS_SPPLOT (BasinName, LstMyShapes,mytype, TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
  
  
  
#######################################################################################
#######################################################################################
### MENU SHINY:  SUMMARY - WEI - BarPlot      #######
######################################################################################
######################################################################################    
 
  ## CALCULA EL WEI para cada subcuenca (para el BLS, o un Scenario al que se han aplicado reducciones)
  ListWDemToWEI <- c("Domestic", "Industrial", "Energy", "Livestock", "Irrigation")  #lista de los usos para calcular el WEI
  BLS_WEI <- Calcula_WEI(Evaluated_BLScen[[1]],ListWDemToWEI)   # Devuelve los WEI de cada subcuenca, considerando todas las demandas simultaneamente 

  ## AGREGACION del WEI PARA CADA NUTS2 (diferentes metricas)(y ponderando o sin poderar) 
  weightBY <-"F"     #F: false (sin ponderar); A: by area ; DA: by Drained Area ;L: longitud   (por definir si ponderar tiene sentido)
  BLS_WEI_By_N2 <- NutsWEIFromSubc (BLS_WEI,weightBY)
  BLS_WEI_By_N2
  BLS_WEI_By_N2[,3:7] <- lapply(BLS_WEI_By_N2[,3:7] , round, 3)  #reduzco numero de decimales para imprimir
  #con lo anterior ya teneis para la tabla
  
  #Barplot con los  WEI by Nuts (en lugar de un loop, le pasais desde el shiny el indice del tipo de agregacion)
  par(mfrow=c(5,1),mar=c(3,4,2,1))
  strgAgrrMetric <- c("count","mean","median","3Q","Max")  #un indice para cada estrategia de calculo
  for (index in 1:5){ 
    barplot(t(BLS_WEI_By_N2[,(1+index)]) , names.arg = BLS_WEI_By_N2$N2,las=3,  
            ylab=paste0(strgAgrrMetric[index], "  WEI by Nuts"),
            main=paste0(strgAgrrMetric[index],"  WEI" )      )
  }
  
  
  # LO SIGUIENTE ES LA AGREGACION PARA TODA LA CUENCA
  # Quizas se podria mezclar con lo anterior por NUTs y meter el valor total ( PARA PENSAR )
  
  ## AGREGACION TOTAL del WEI para la cuenca (diferentes metricas)
  weightBY <-"F"     #F: false (sin ponderar); A: by area ; DA: by Drained Area ;L: longitud   (por definir si ponderar tiene sentido)
  BLS_WEI_tot <- TotWEIFromSubc (BLS_WEI,weightBY)

  
  weightBY <-"F"     #F: false (sin ponderar); A: by area ; DA: by Drained Area ;L: longitud   (por definir si ponderar tiene sentido)
  threshold <- 4  # si no le pasamos el umbral, toma un valor por defecto (al calcular esa metrica)
  BLS_WEI_tot_th <- TotWEIFromSubc (BLS_WEI ,weightBY,threshold)  
  BLS_WEI_tot_th  
  
  

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
    
    # The limits of reduction of presures by Nuts and  Sectors
    BasinName <-"Ebro"    #     "Evrotas"  "Adige"     "Sava"   "Ebro"   
    TheMaxReductions <- Loading_Max_PressRed (BASE_IN_DATA,BasinName)   #atencion que hay que arregarlos nombres del fichero
                                                      #tambien hay que mandar el nombre de la cuenca y hacer subseting
   
      
#############################################################################
#############################################################################
### MENU SHINY:  OPTIMIZATION - Sensitivity of the weight            #######
############################################################################
############################################################################ 
    
    
    # Funcion para realizar el analisis de sensibilidad a incrementos en el coste para reducir un tipo de presion (por sectores)  
    # Se le pasa el BLScen de la cuenca de la que se quiere analizar la sensibilidad
    # La funcion calcula (tarda bastante) y escribe un csv con el resultado
    # lo que hace es iterativamente (para cada tipo de presion) multiplicar por 2 el peso (Esfuerzo) que
    # el coste necesario para reducir una presion. Se ejecuta la optimizacion y se mira como es la distribucion por sectores
    # de los esfuerzos para conseguir el mismo nivel global de reduccion de presiones  
    # guarda el resultado en el diroctorio que le mandemos
    
    # la siguiente funcion lanza el analisis y solo la ejecuto yo una vez cada  cuenca
    thepath <- paste0(BASE_IN_DATA,"/")
    
    numIter <- 100
    Analisis_Sensibilidad_QT(thepath,BLScen,TheMaxReductions,numIter)
    
    
    
    #######################################
    ####
    ##El siguiente codigo esta en el servidor de shiny y es lo que ve el usuario cuando hace el analisis de sensibilidad
    SensiData <- read.csv( paste0(thepath,"Sensitivity",BasinName,".csv") )
    
    DFSensiBLS <- SensiData[SensiData$typosen=="BLS",2:6]
    DFSensiDomest <- SensiData[SensiData$typosen=="Domest",2:6]
    DFSensiEnerg <- SensiData[SensiData$typosen=="Energ",2:6]
    DFSensiIndust <- SensiData[SensiData$typosen=="Indust",2:6]
    DFSensiIrrig <- SensiData[SensiData$typosen=="Irrig",2:6]
    DFSensiLivest <- SensiData[SensiData$typosen=="Livest",2:6]
    
    #    colMeans(DFSensiBLS)
    #    apply(DFSensiBLS,2,median) 
    
    Mattemp <- rbind (colMeans(DFSensiBLS),colMeans(DFSensiDomest), 
                      colMeans(DFSensiEnerg),colMeans(DFSensiIndust),colMeans(DFSensiIrrig),colMeans(DFSensiLivest) )
    rownames(Mattemp) <-  c("All == cost","Domest=2 x cost","Energ=2 x cost","Indust=2 x cost","Irrig=2 x cost","Livest=2 x cost") 
    colnames(Mattemp) <- c("Domest","Energ","Indust","Irrig","Livest")
    
    StgtoPlot2 <- (Mattemp)
    
    par(mfrow=c(1,1))
    barplot((as.matrix(StgtoPlot2))/1000000 ,
            names.arg = colnames(StgtoPlot2),las=1, beside=TRUE, 
            col=rainbow(nrow(StgtoPlot2)) , ylab=expression(paste("Water Extraction reduction 10"^"6"," Ton"^"3") ) ,
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
    
    ###########################
    ## Desplegable: DIRECT   ##
    ###########################
    # 1. Ponderacion de Esfuerzos por SECTORES
      PesosEffortlabel <- c("Domest","Energ","Indust","Irrig","Livest")
      PonderEfforRawVal <- c(20,20,20,20,20)    #los valores que mete el usuario
      PonderEffor <- PonderEfforRawVal/sum(PonderEfforRawVal)
      
         #lo mostramos como Barplot 
        par(mar=c(4.5,6,2,1))
        barplot(as.table(PonderEffor), main="Relative Cost of Pressure Reduction by Sectors", 
                horiz=TRUE, col="lightblue1",las=1, cex.axis = 1.3,cex.names=1.3,
                names.arg= PesosEffortlabel )
    
    # 2. Ponderacion de Esfuerzos por REGIONES
      PesosEffortRegionlabel <- colnames(BLScen[[7]])
      PonderEffortRegion <- rep(100/length(PesosEffortRegionlabel),length(PesosEffortRegionlabel)  ) #si los considero de igual esfuerzo en todas las regiones
      PonderEfforReg <- PonderEffortRegion/sum(PonderEffortRegion)
      
        par(mar=c(4.5,6,2,1))
        barplot(as.table(PonderEfforReg), main="Relative Cost of Pressure Reduction by Regions", 
                horiz=TRUE, col="lightblue1",las=1, cex.axis = 1.3,cex.names=1.3,
                names.arg= PesosEffortRegionlabel )
      
    # 3. Compone la Matriz de ponderaciones en base a las dos anteriores.
      PonderMat <- PonderEffor %*% t(PonderEfforReg)
      colnames(PonderMat) <- colnames(BLScen[[7]])
      rownames(PonderMat) <- PesosEffortlabel
      
          par(mar=c(4.5,6,2,1))
          PesosEffortlabel
          barplot(as.matrix(PonderMat), main="Relative Cost of Pressure Reduction by Sectors & Regions", 
                  las=1, cex.axis = 1.3,cex.names=1.3,col= rainbow(length(PesosEffortlabel)),
                  names.arg= PesosEffortRegionlabel ,beside = TRUE , ylab="% Relative Cost",
                  legend.text=TRUE, args.legend=list(x = "topright", cex = 1.05, bty = "n")  )


      
    ############################
    ## Desplegable: PAIR WISE ##
    ############################

    # 1. Ponderacion de Esfuerzos por SECTORES  #####
      # Preparando la ponderacion por tipo de esfuerzo (pero no por region) a partir de la pair-wise comparison
      #A partir de las comparaciones por pares de la Interface, se calculan los pesos de la reduccion de presiones
      # para cada tipo de presion: domestWD, industWD, energWD, livesWD, irrWD
      # El numero de pares de comparaciones es = de N*(N-1)/2    Siendo N, el numero de categorias
      # Si tenemos 4 categorias "Man", "Min","PS","Sd", tendremos  4*3/2=6 pares de comparaciones
      # si tenemos 5 categorias "Domest","Energ","Indust","Irrig","Livest", tendremos 5*4/2 = 10
      PesosEffortlabel <- c("Domest","Energ","Indust","Irrig","Livest")  
      # Como compara todos con todos son 6 comparaciones:
      #   Domest - Energ  ; Domest - Indust ; Domest - Irrig  ; Domest - Livest  ;
      #   Energ - Indust ; Energ - Irrig ; Energ - Livest 
      #   Indust - Irrig  ; Indust - Livest 
      #   Irrig - Livest 
      pairwiseeffort <- c(0.2, 2, 2, 1, 1, 1, 1, 2, 1, 2)  #Asi pues: esto es el resultado de todos los pairwise comparisons   
      Pesos <- ComputingWeihts(pairwiseeffort,PesosEffortlabel)  
      PonderEffor <- Pesos$weighting  #estos son los pesos que usaremos y haremos la grafica
      
      Pesos$Saaty   # Inconsistencia de acuerdo a la metrica de saaty
      
      #No ponemos botton de Inconsistency, yo diria que se verifica cada vez que completa la tabla o modifica un valor
      #pero tambien puede ser con valor
      if( Pesos$Saaty > 0.15) # of 0.10 or less is acceptable to continue the  AHP analysis. 
             print (paste( "Inconsistency ratio: ", Pesos$Saaty, " (Inconsistent matrix)",sep=" ") )
      
       #lo mostramos como Barplot 
       par(mar=c(4.5,6,2,1))
       barplot(as.table(PonderEffor), main="Relative Cost of Pressure Reduction", 
               horiz=TRUE, col="lightblue1",las=1, cex.axis = 1.3,cex.names=1.3,
               names.arg= PesosEffortlabel )
           
    # 2. Ponderacion de Esfuerzos por SECTORES  ####
       # las etiquedas de los sectores
       PesosEffortRegionlabel <- colnames(BLScen[[7]])
       # A partir del numero de regiones tenemos el numero de comparaciones:  n*(n-1)/2
       n <- length(BLScen[[7]])   
       pairwiseeffortReg <- runif(n*(n-1)/2, 0, 1)   #vector con todas las comparaciones. Esta matriz la devolvera la interface a partir de la matriz
       PesosRegions <- ComputingWeihts(pairwiseeffortReg,PesosEffortRegionlabel)  
       PonderEfforRegions <- PesosRegions$weighting  #estos son los pesos que usaremos y haremos la grafica
       PesosRegions$Saaty   # Inconsistencia de acuerdo a la metrica de saaty (muy alta porque los genere aleatoriamente)
              print (paste( "Inconsistency ratio: ", PesosRegions$Saaty, " (Inconsistent matrix)",sep=" ") )
          #lo mostramos como Barplot 
          par(mar=c(4.5,6,2,1))
          barplot(as.table(PonderEfforRegions), main="Relative Cost of Pressure Reduction", 
                  horiz=TRUE, col="lightblue1",las=1, cex.axis = 1.3,cex.names=1.3,
                  names.arg= PesosEffortRegionlabel )
              
       
       

#############################################################################
#############################################################################
### MENU SHINY:  OPTIMIZATION -  Run  Optimization                 #######
############################################################################
############################################################################   
      
    ## Otros parametros para la optimizacion
    
      #otra funcion para el valor de los estados (sum, max, 3Q, mediana, media, treshold, ponderados o no por area)
      
      # Tengo que implementar una segunda consideracion por pesos, para cada NUTS
      # En este caso puede haber bastantes Nuts y serian demasiadas comparaciones: quizas usemos asignacion directa
      
      # primer parametro Concentracion agregation metric =>  #1:media ; 2:median;  3:3quantile; 4:max ; 5: Treshod (suma de lo que sobrepasa ese valor limite 
      indexAgrM <- c(2,2.1)   #Segundo parametro, el umbral (solo es necesario si se escoge el tipo 5 de agregacion)
      
      signOb <- c(1,1)   #positivo si es minimizar y negativo si es maximizar (en este caso queremos minimizar ambos)
      genParam <- c(2,40,40)   #  nobject<-2 ; popSize=60 ; generations=40
      Configl <- list(PonderMat,indexAgrM,signOb,genParam)
      
    ## 9.3 Lanza la optimizacion   
      #OPTIMIZA (por el momento me olvido de la ponderacion)
      myPareto <- MOO_GA(BLScen,TheMaxReductions, Configl) 
      
    #  myPareto <-  My_ListSensiS
      
      myParetoMedia <- myPareto
      myParetoMedian <- myPareto
      myParetoQuantile <- myPareto
      
      
      
      myParetoMedia20x200 <- myPareto
      
      plot(myParetoMedia100x100$objectives,pch = 16)
      points(myParetoMedia40x100$objectives,pch = 16, col="blue")  #40x100
      points(myParetoMedia40x40$objectives, pch = 16, col="red")  #40x40
      points(myParetoMedia40x20$objectives, pch = 16, col="green")
      plot(myParetoMedia20x200$objectives, pch = 16, col="orange")

      
    ##################################
    ##### 10. DIBUJANDO PARETOS INDIVIDUALES
    ###############################   
      #UN PARET0 concreto (en un fichero o en pantalla)
      Plot_Pareto(BASE_PLOTS,myPareto,"F","F","mean")  #the second parameter: T: create a jpeg file.  F: don't create a jpeg file. El tercer parametro, es el rescalado a 0-1. El ultimo parametro es la escala de las concentraciones
      
  
     # el punto 3, (estrategia numero 3)
      index_of_stragegy <- 1
      stg_n1 <- myPareto$parameters[index_of_stragegy,]
  
      Stg_3_PressRed <- DecodeStg (BLScen,stg_n1)
      Stg_3_PressRed_bySect <- rowSums(Stg_3_PressRed)/dim(Stg_3_PressRed)[2]  #aproximacion a reduccion por sectores (en porcentajes)
  
      
      
      #Rutina para buscar (en el frente de pareto) la estrategia mas cercana de acuerdo al esfuerzo realizado
      valEffort <- 2000000000  #Buscamos la estrategia cercana a un esfuerzo
      Estrategia1 <- closestParetStrategByEffort(BLScen,myParetoMedia,valEffort)  #LA ESTRATEGIA 1  
      Scen_Estrategia1 <- EvalStg (BLScen,Estrategia1) 
      
  
      
  



##############################################################################################
##############################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - Effort Analysis  - BarPlot by Nuts          #######
#############################################################################################
#############################################################################################      
      
      # En muchas de las comparaciones se necesitara la Estrategia de la situacion base. Es decir no reducir nada
      # La genero en las siguientes lineas
      dimx <- length(The_N2)  #el numero de columnas de la matriz (una por cada NUTS)
      BLS_strateg <- matrix( c( rep(0,dimx),     # domestWD   
                               rep(0,dimx),      # industWD
                               rep(0,dimx),       # energWD
                               rep(0,dimx),      # livesWD
                               rep(0,dimx)),      # irrWD
                            byrow = TRUE, ncol = dimx)
      rownames(BLS_strateg) <- c("Domest","Energ","Indust","Irrig","Livest")  #cada fila es un tipo de presion
      colnames(BLS_strateg) <- The_N2
      
      # Para todo este menu, siempre es necesrio tener un pareto
      myP <- myPareto 

      # Tambien hacen falta dos estrategias del frente de pareto (o la BLS y una del pareto)
      stg_n1_index <- 1  # BLS_strateg  # podria ser un indice  o la estrategia  BLS 
      stg_n2_index <- 2  # salio de una estrategia que mejora bastante en relacion con el BLS
      
      #extraigo la reduccion de presiones de esas dos estrategias del Pareto
      # stg_n1 <- BLS_strateg
      stg_n1 <- myP$parameters[stg_n1_index,]
      stg_n1_strateg <- DecodeStg (BLScen,stg_n1)
        
      stg_n2 <- myP$parameters[stg_n2_index,]
      Stg_n2_strateg <- DecodeStg (BLScen,stg_n2)

      #ya tengo las dos estrategias para usar en esta pestana (y alguna de las siguientes)
      stg_n1_strateg
      Stg_n2_strateg
      
      
      # PRIMERO EVALUO LA ESTRATEGIA (se le manda siempre el BLScen y la estrategia) 
  #       Evaluated_Scen2 <- EvalStg (BLScen,My_strateg2)  #anado que devuelva un segundo elemento (que sera por sectores la reduccion de metros cubicos)
  #       head(Evaluated_Scen2[[1]]) #  #devuelve [[1]]el nuevo escenario de demandas, y [[2]]la diferencia de dichas demandas con relacion al BLS
      
      ###################          LOS BARPLOT        ####################

      ####### BOTON: Separate Strategies rates    ################
      par(mfrow=c(1,2))
      name <- "Stg_2"
      BarPlot_Stg_Effort_QT(BLScen,stg_n1_strateg,name,rates="T")  #como porcentajes
      name <- "Stg_9" 
      BarPlot_Stg_Effort_QT(BLScen,Stg_n2_strateg,name,rates="T")  #como porcentajes

      ####### BOTON: Separate Strategies volume    ################
      name <- "Stg_2"
      BarPlot_Stg_Effort_QT(BLScen,stg_n1_strateg,name,rates="F")  #como metros cubicos totales
      name <- "Stg_9"      
      BarPlot_Stg_Effort_QT(BLScen,Stg_n2_strateg,name,rates="F")  #como metros cubicos totales
      
      # El barplot es mejorable (misma escala para ambos, las etiqueta no es load,... etc), no lo toco porque no se que empleareis 
      
      # check: DIFFERENCIA entre ambas estrategias
      # Calculo la diferencia restando (como es lineal no hay problema) y es valido tanto como % o como load
      stg_diff_rates <- Stg_n2_strateg - stg_n1_strateg
      
      ####### BOTON: Difference  ################
      name<-"Diference Rate"
      BarPlot_Stg_Effort_QT(BLScen,stg_diff_rates,name,rates="T")
      name<-"Diference load"
      BarPlot_Stg_Effort_QT(BLScen,stg_diff_rates,name,rates="F")  
      #tambien es mejorable (las etiquetas, no es load, es vol)

      
      ####### BOTON: Aggregate by Sector  ################      
      #####  #####
      ##### #####  ATENCION REVISAR EL ORDEN EN QUE DEVUELVE LOS USOS N2EffortFromSubc
      ####   #####
      # tiene que ser en m3 o toneladas , no valen los porcenajes
      MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
        #el segundo nos da ya metros cubicos de reduccion de demanda. 
        # Falta agregarlos por nuts (ya tengo una funcion que los suma por nuts y tipo)
        PonderEffor <- "D"        #  D: Default (todos igual importancia) . En caso contrario pasa el array de pesos de ponderacion
        MyEffort_stg1 <- ComputeEffort(MyEval_stg1,PonderEffor)
        Stg_n1_PressRed_bySect <- t(MyEffort_stg1)   #seria para cuando el basin tiene solo 1 nuts
        if(length(MyEffort_stg1)>1)  Stg_n1_PressRed_bySect <- rowSums(MyEffort_stg1[,] )       #si el basin tiene varios nuts, suma todos ello
        Stg_n1_PressRed_bySect
        
        
      MyEval_stg2 <- EvalStg (BLScen,Stg_n2_strateg) 
        #el segundo nos da ya metros cubicos de reduccion de demanda. 
        # Falta agregarlos por nuts (ya tengo una funcion que los suma por nuts y tipo)
        PonderEffor <- "D"        #  D: Default (todos igual importancia) . En caso contrario pasa el array de pesos de ponderacion
        MyEffort_stg2 <- ComputeEffort(MyEval_stg2,PonderEffor)
        Stg_n2_PressRed_bySect <- t(MyEffort_stg2)   #seria para cuando el basin tiene solo 1 nuts
        if(length(MyEffort_stg2)>1) Stg_n2_PressRed_bySect <- rowSums(MyEffort_stg2[,] )  # reduccion por sectores 
        Stg_n2_PressRed_bySect
    

      Mattemp <- rbind (Stg_n1_PressRed_bySect[],Stg_n2_PressRed_bySect[])
      rownames(Mattemp) <-  c("stg_A","stg_B") 
      #################################
      StgtoPlot2 <- (Mattemp)
      
      par(mfrow=c(1,1))
      barplot((as.matrix(StgtoPlot2))/1000000 ,
              names.arg = colnames(StgtoPlot2),las=1, beside=TRUE, 
              col=rainbow(nrow(StgtoPlot2)) , ylab=expression(paste("Load reduction 10"^"6"," m"^"3") ) ,
              main=paste0(" Load reduction comparison"),
              legend.text=TRUE, args.legend=list(x = "topright", cex = 1.05, bty = "n" ,inset=c(-0.05,-0.07) )  )
      
      
        # # Ejemplo con agregacion por sectores ponderada    
        #   # Ponderado (meto solo un peso para cada sector, es decir vector) Future task (que sea matriz de sectores y N2)
        # PonderEffor <-  c(.20 ,0.25 ,0.10 , 0.35, 0.20) # rep(0,nrow(My_strateg2))     
        # names(PonderEffor) <- c("Domest","Energ","Indust","Irrig","Livest")                             
        # Effort_scen2 <- N2EffortFromSubc(Evaluated_Scen2,PonderEffor)  # El ponderado sera usado para optimizar y devuelve ya un valor total
        # # Para el optimizador, si emplea pesos, es mejor que ya vuelva agregado
        # #los pesos podria ser una matriz por sectores y N2, igual estructura que la estrategia
        # # por el momento considera que todos los N2 son iguales, y pone matriz solo de sectores
      
      


      
      
##############################################################################################
##############################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - Effort Analysis  - Maps by Nuts          #######
#############################################################################################
#############################################################################################     
      
      #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
      NumClassInterv <- 8    #  Numero de categorias a la hora de dibujarlo
      paleta <-  "Greens"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
      estilo <-  "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
      scale <- "linear"      # "linear"   "logarithmic"
      agregation <-   "No"  # "Mean"       "Mean"     "Median"      "Q75"         "Max"  
      scaleRelation <- "Common"   #  "Common"   "Independent"
  
      ####### BOTON: Separate Strategies rates    ################
        # Los PORCENTAJES de esfuerzos de reduccion por N2 y sectores
        # la estrategia 1   (fila de mapas superiores)
        stg_n1_strateg  #si lo quiero pintar en porcentajes no aplico transformaciones
        mytype<-"EF"
        TheVariables <- Prepro_to_Map_N2(mytype,t(stg_n1_strateg),scale,agregation) #preproceso para mapear todos los uso
        ListIndexUses <- c(1,2,3,4,5)  # Tipo sfuerzos:  1:"Domest", 2:"Energ",3:"Indust",4:"Irrig",5:"Livest"
        Map_var_NUTS_SPPLOT (BasinName, LstMyShapes, mytype,TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
      
        # la estrategia 2  (fila de mapas inferiores)
        Stg_n2_strateg  #si lo quiero pintar en porcentajes no aplico transformaciones
        mytype<-"EF"
        TheVariables <- Prepro_to_Map_N2(mytype,t(Stg_n2_strateg),scale,agregation) #preproceso para mapear todos los uso
        ListIndexUses <- c(1,2,3,4,5)  # Tipo sfuerzos:  1:"Domest", 2:"Energ",3:"Indust",4:"Irrig",5:"Livest"
        Map_var_NUTS_SPPLOT (BasinName, LstMyShapes,mytype, TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
        
      ####### BOTON: Separate Strategies volume    ################  
        #lo mismo que el anterior pero convirtiendo los ratios en cantidades totales
        stg_n1_strateg   #las estrategias que generamos previamente (para los barplot)
        Stg_n1_val <- stg_rates_to_values(BLScen , stg_n1_strateg)   #convierto los ratios en valores (sea m3 agua o toneladas de load) 
        Stg_n2_val <- stg_rates_to_values(BLScen , Stg_n2_strateg) 

        # la estrategia 1   (fila de mapas superiores)
        mytype<-"EF"
        TheVariables <- Prepro_to_Map_N2(mytype,t(Stg_n1_val),scale,agregation) #preproceso para mapear todos los uso
        ListIndexUses <- c(1,2,3,4,5)  # Tipo sfuerzos:  1:"Domest", 2:"Energ",3:"Indust",4:"Irrig",5:"Livest"
        Map_var_NUTS_SPPLOT (BasinName, LstMyShapes, mytype,TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
        
        # la estrategia 2  (fila de mapas inferiores)
        mytype<-"EF"
        TheVariables <- Prepro_to_Map_N2(mytype,t(Stg_n2_val),scale,agregation) #preproceso para mapear todos los uso
        ListIndexUses <- c(1,2,3,4,5)  # Tipo sfuerzos:  1:"Domest", 2:"Energ",3:"Indust",4:"Irrig",5:"Livest"
        Map_var_NUTS_SPPLOT (BasinName, LstMyShapes,mytype, TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
        
        
      ####### BOTON: Difference    ################     
          #la diferencia en RATIOS la calculo restando (como es lineal no hay problema) y es valido tanto como % o como load
        stg_diff_rates <- Stg_n2_strateg - stg_n1_strateg   # fila de mapas superiores
        mytype<-"EF"
        TheVariables <- Prepro_to_Map_N2(mytype,t(stg_diff_rates),scale,agregation) #preproceso para mapear todos los uso
        ListIndexUses <- c(1,2,3,4,5)  # Tipo sfuerzos:  1:"Domest", 2:"Energ",3:"Indust",4:"Irrig",5:"Livest"
        Map_var_NUTS_SPPLOT (BasinName, LstMyShapes, mytype,TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
        
        #la diferencia en CANTIDADES tambien la calculo restando
        Stg_diff_val <- Stg_n2_val - Stg_n1_val
        mytype<-"EF"
        TheVariables <- Prepro_to_Map_N2(mytype,t(Stg_diff_val),scale,agregation) #preproceso para mapear todos los uso
        ListIndexUses <- c(1,2,3,4,5)  # Tipo sfuerzos:  1:"Domest", 2:"Energ",3:"Indust",4:"Irrig",5:"Livest"
        Map_var_NUTS_SPPLOT (BasinName, LstMyShapes, mytype,TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
        
  
      
    # ##################################
    # #####  OTRO EJEMPLO DE VERIFICACION EN EL PLOTEADO DE ESFUERZOS, para ver un problema con las escalas
    # ###############################
    #     ## 8.1 propongo la estrategia de la que quiero calcular los esfuerzos
    #     dimx <- length(The_N2)  #el numero de columnas de la matriz (una por cada NUTS)
    #     My_strateg2 <- matrix( c( rep(1,dimx),      # domestWD  (los valores son porcentajes de reduccion de demanda de agua por sectores y N2)
    #                               rep(2,dimx),       # energWD 
    #                               rep(3,dimx),       # industWD
    #                               rep(6,dimx),       # irrWD 
    #                               rep(8,dimx)),      # livesWD
    #                            byrow = TRUE, ncol = dimx)
    #     rownames(My_strateg2) <- c("Domest","Energ","Indust","Irrig","Livest")   #cada fila es un tipo de presion
    #     colnames(My_strateg2) <- The_N2
    #     
    #     # PRIMERO EVALUO LA ESTRATEGIA (se le manda siempre el BLScen y la estrategia) 
    #     Evaluated_Scen2 <- EvalStg (BLScen,My_strateg2)  #anado que devuelva un segundo elemento (que sera por sectores la reduccion de metros cubicos)
    #     head(Evaluated_Scen2[[1]]) #  #devuelve [[1]]el nuevo escenario de demandas, y [[2]]la diferencia de dichas demandas con relacion al BLS
    #     
    #     #Dibuja barplot la distribucion de esfuerzos entre N2 y sectores
    #     name <- "mi estrategia"
    #     BarPlot_Stg_Effort_QT(BLScen,My_strateg2,name,rates="T")  #como porcentajes
    #     BarPlot_Stg_Effort_QT(BLScen,My_strateg2,name,rates="F")  #como metros cubicos totales
    #     
    #     
    #     NumClassInterv <- 8    #  Numero de categorias a la hora de dibujarlo
    #     paleta <-  "Greens"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
    #     estilo <-  "quantile"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
    #     scale <- "linear"      # "linear"   "logarithmic"
    #     agregation <-   "No"  # "Mean"       "Mean"     "Median"      "Q75"         "Max"  
    #     scaleRelation <- "Common"   #  "Common"   "Independent"
    #     
    #     ####### BOTON: Separate Strategies rates    ################
    #     # Los PORCENTAJES de esfuerzos de reduccion por N2 y sectores
    #     # la estrategia 1   (fila de mapas superiores)
    #     Evaluated_Scen2  #si lo quiero pintar en porcentajes no aplico transformaciones
    #     mytype<-"EF"
    #     TheVariables <- Prepro_to_Map_N2(mytype,t(My_strateg2),scale,agregation) #preproceso para mapear todos los uso
    #     ListIndexUses <- c(1,2,3,4,5)  # Tipo sfuerzos:  1:"Domest", 2:"Energ",3:"Indust",4:"Irrig",5:"Livest"
    #     Map_var_NUTS_SPPLOT (BasinName, LstMyShapes, mytype,TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
    #     
    #     #cambio algun valor (manualmente) para arreglar la escala
    #     My_strateg2 [1,1] <- 1.01
    #     My_strateg2 [2,1] <- 2.01
    #     My_strateg2 [3,1] <- 3.01
    #     My_strateg2 [4,1] <- 4.01
    #     My_strateg2 [5,1] <-5.01
    #   
    #     TheVariables <- Prepro_to_Map_N2(mytype,t(My_strateg2),scale,agregation) #preproceso para mapear todos los uso
    #     ListIndexUses <- c(1,2,3,4,5)  # Tipo sfuerzos:  1:"Domest", 2:"Energ",3:"Indust",4:"Irrig",5:"Livest"
    #     Map_var_NUTS_SPPLOT (BasinName, LstMyShapes, mytype,TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
    #     
      
      
       
        
##############################################################################################
##############################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - Effort Analysis -   Flow Diagram          #######
#############################################################################################
############################################################################################# 
      
        ###############
        ## VERIFICACION DE CALCULO DE WATER USES Y WATER USES REDUCTIONS CORRESPONDIENTES A UNA ESTRATEGIA
        ###############
        
        # con la Estrategia nula    
        BLS_strateg   
        # Si la evaluo     
        Evaluated_BLScen <- EvalStg (BLScen,BLS_strateg)  # [[1]] escenario de demandas, y [[2]]la diferencia de dichas demandas con relacion al BLS
        Evaluated_BLScen[[2]]  #todo nulo porque no hay reduccion de demandas
        # La siguiente funcion calcula la demanda agregada por N2
    #    UsesbyN2_sect_BLScenStg <- N2TotExtractionsFromSubc(Evaluated_BLScen)     
        # Y esta funcion hace el mismo calculo para las reducciones
        BLS_Redcuctions <- stg_rates_to_values(BLScen , BLS_strateg)
        # si hago el calculo de las reducciones       
        aggregate(Evaluated_BLScen[[2]][,2:8], by=list(N2=Evaluated_BLScen[[2]]$N2), FUN=sum)  
        
        #repito toda la verificacion con una estrategia
        stg_n1_strateg
        # La evaluo tambien     
        Evaluated_stg1 <- EvalStg (BLScen,stg_n1_strateg)  # [[1]] escenario de demandas, y [[2]]la diferencia de dichas demandas con relacion al BLS
        Evaluated_stg1[[2]]  #todo nulo porque no hay reduccion de demandas
        # La siguiente funcion calcula la demanda agregada por N2
   #     UsesbyN2_sect_stg1 <- N2TotExtractionsFromSubc(Evaluated_stg1)     
        # Y esta funcion hace el mismo calculo para las reducciones
        stg1_Redcuctions <- stg_rates_to_values(BLScen , stg_n1_strateg)
        # si hago el calculo de las reducciones yo mismo, da lo mismo:       
        aggregate(Evaluated_stg1[[2]][,2:8], by=list(N2=Evaluated_stg1[[2]]$N2), FUN=sum)  
        
        # Para una estrategia [[1]] new demandas  + [[2]] reduccion demandas  =  [[1]] del BLS es decir demandas iniciales
        # Comprobacion.
        # Para la estrategia, el calculo de [[1]] sale de
 #       UsesbyN2_sect_stg1 <- N2TotExtractionsFromSubc(Evaluated_stg1)
        # Para la estrategia, el calculo de [[2]] sale de
        # stg1_Reductions <- stg_rates_to_values(BLScen , stg_n1_strateg)
        # # proceso un poco la primera y las sumo
        # temp_Uses_stg1 <- UsesbyN2_sect_stg1[,c(1,3,4,5,6,7)]
        # rownames(temp_Uses_stg1) <- temp_Uses_stg1$N2
        # temp_Uses_stg1$N2 <-NULL
        # temp_Uses_stg1 <- temp_Uses_stg1[ order(row.names(temp_Uses_stg1)), ]  #reordeno de acuerdo al nombre de los N2
        # 
        # stg1_Reductions+temp_Uses_stg1
        # # Comparando esa suma con el BLS se verifica que es igual
        # UsesbyN2_sect_BLScenStg
        # 
        # #Asi que la comprobacion ha sido correcta (resuelto un error)
        # 
        # ## quiero comprobar tambien los ratios entre lo reducido y lo usado
        # stg1_Reductions/temp_Uses_stg1 *100
        # 
        # colSums(stg1_Reductions)/colSums(temp_Uses_stg1) *100
        
        
        #######################
        ### SANKEY de REDUCTIONS una estrategia
        ####################
        Plot_SANKEY_Reductions (BLScen , stg_n1_strateg)
        Plot_SANKEY_Reductions (BLScen , Stg_n2_strateg)
        
        
        #######################
        ### SANKEY de USES_and_REDUCTIONS una estrategia
        ####################
        Plot_SANKEY_Uses_Red (BLScen , stg_n1_strateg)
        Plot_SANKEY_Uses_Red (BLScen , Stg_n2_strateg)
        
        
        
        
        
        
        
        # ###############
        # #####  compara dos estrategias   NO ME GUSTA EL RESULTADO
        # ##############
        # #repito proceso stg 1
        # #los nodos son: los nuts y los usos
        # nombreN2 <- rownames(Stg_n1_val)
        # nombreUsos <- colnames(Stg_n1_val)
        # 
        # nodes = data.frame("name" =  c(nombreN2,nombreUsos) )
        # nodesindex <- nodes  
        # nodesindex$ID <- seq.int(nrow(nodesindex))-1
        # 
        # tempSankey <- Stg_n1_val
        # tempSankey$N2<-rownames(tempSankey)
        # links1 <- melt(tempSankey, id=c("N2"))
        # #  nodesindex$ID[match(links$source,nodesindex$name)]
        # links1$source <- nodesindex$ID[match(links1$N2,nodesindex$name)]
        # links1$target <- nodesindex$ID[match(links1$variable,nodesindex$name)]
        # 
        # 
        # #  Proceso la segund DF para la stg 2
        # tempSankey2 <- Stg_n2_val  
        # #renombro las columnas y la filas para distinguir stg 1 y 2
        # colnames(tempSankey2)[1:length(tempSankey2)] <- paste0( "stg2_",colnames(tempSankey2)[1:length(tempSankey2)] )
        # 
        # # nombres nodos y links
        # nombreN2 <- rownames(Stg_n1_val)
        # nombreUsos2 <- c( colnames(Stg_n1_val), colnames(tempSankey2) )
        # 
        # # Para comparar con la otra estrategia , tengo que anadir los nodos y arcos de la segunda
        # nodes2 = data.frame("name" =  c(nombreN2,nombreUsos2) )  #anado los sectores para la segunda stg
        # nodesindex2 <- nodes2  
        # nodesindex2$ID2 <- seq.int(nrow(nodesindex2))-1
        # 
        # 
        # #links para el segundo nivel (sectores stg1 a sectores stg2)
        # Stg_n1_PressRed_bySect <- colSums(Stg_n1_val)   # reduccion por sectores (no la necesito)
        # Stg_n2_PressRed_bySect <- colSums(Stg_n2_val)  # reduccion por sectores 
        # 
        # links2 <- data.frame( sourcetemp =  colnames(Stg_n1_val)   ,
        #                       targettemp =  colnames(tempSankey2) ,value= Stg_n2_PressRed_bySect  )
        # 
        # #poniendolo en forma de indices
        # links2$source <- nodesindex2$ID[match(links2$sourcetemp,nodesindex2$name)]
        # links2$target <- nodesindex2$ID[match(links2$targettemp,nodesindex2$name)]
        # 
        # 
        # links <- rbind ( links1[,c("source","target","value")]  , links2[,c("source","target","value")] )
        # 
        # 
        # 
        # #### siguiente nivel
        # sankeyNetwork(Links = links, Nodes = nodes2,
        #               Source = "source", Target = "target",
        #               Value = "value", NodeID = "name",
        #               fontSize= 12, nodeWidth = 30)
        # 
        # 
        
    
    
    
    
#######################################################################################
#######################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - Water Uses - BarPlot      #######
######################################################################################
######################################################################################  
            # COMPROBACION: cuando aplicamos el 100% de la reduccion de presiones, las extracciones son nulas
            # My_strateg2 <- matrix( c( rep(100,dimx),      # domestWD  (los valores son porcentajes de reduccion de demanda de agua por sectores y N2)
            #                           rep(100,dimx),       # energWD
            #                           rep(100,dimx),       # industWD
            #                           rep(100,dimx),       # irrWD
            #                           rep(100,dimx)),      # livesWD
            #                        byrow = TRUE, ncol = dimx)
            # rownames(My_strateg2) <- c("Domest","Energ","Indust","Irrig","Livest")   #cada fila es un tipo de presion
            # colnames(My_strateg2) <- The_N2
            # 
            # MyEval_stgTot <- EvalStg (BLScen,My_strateg2) 
            # tot <- NutsExtractionFromSubc( MyEval_stgTot[[1]] ,"F")
 
                   
    ##################################################
    ## ATENCION QUE ESTO ES CON DEMANDA ACUMULADA   ##
    ##################################################
        
    # Continuo utilizando las dos estrategias seleccionadas del frente de pareto
    stg_n1_strateg
    Stg_n2_strateg
    
    # Evaluo las dos estrategias
    MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
    MyEval_stg2 <- EvalStg (BLScen,Stg_n2_strateg)   
    
    # Agregamos las nuevas demandas ACUMULADAS by Nuts    # Ahora no es el fresWater [[1]], son todos los otros:
    weightBY <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
    ExtraccNUT_stg1 <- NutsExtractionFromSubc( MyEval_stg1[[1]] ,weightBY) # ([[1]] fresW, [[2]]domestic, [[3]]Ener, [[4]]Industria,   [[5]]Irri , [[6]]Livest)
    ExtraccNUT_stg2 <- NutsExtractionFromSubc( MyEval_stg2[[1]] ,weightBY) 
 
    ####### BOTON: Separate Strategies    ################  
    
    #Vuestro desplegable es el que dara el indice del uso que quremos que se represente (seran todos mennos fresW. de; 2 a 8)
    #Primero el barplot (la funcion calcula el valor agregado de la variable). (FRESH WATER)
    indexofPresure <- 2 # 1: "Fresh Water", 2: "Domestic Water", 3: "Industrial Water", 4: "Energy Water", 5: "Livestock Water", 6: "Irrigation Water")
    weightBYlength <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
    BarPlot_Stg_Extracciones(MyEval_stg1,weightBYlength,indexofPresure)    # Hace un barplot para cada tipo: Devuel una lista: fresW, domestic, Industria, Ener, Livest, Irri
    BarPlot_Stg_Extracciones(MyEval_stg2,weightBYlength,indexofPresure)    # y para cada estrategia
    
    # Yo lo dejo asi, vosotros lo haceis con plotly. Lo chulo seria meter las dos estrategia juntas 
    # es decir para cada nuts y tipo de metrica, dos barras en lugar de una (una barra por cada strategia)

    ####### BOTON: Difference    ################  
      #Las calculo todas
    Thedifference <- ExtraccNUT_stg2  # hago la copia de una de las listas y luego modifico en un loop
    for (indexofPresure in 2:6){   # 1: "Fresh Water", 2: "Domestic Water", 3: "Industrial Water", 4: "Energy Water", 5: "Livestock Water", 6: "Irrigation Water")
      Thedifference[[indexofPresure]][,c(3:6)] <- ExtraccNUT_stg1[[indexofPresure]][,c(3:6)] - ExtraccNUT_stg2[[indexofPresure]][,c(3:6)]  # Una estrategia de menos esfuerzo usa mas agua (y normamente dara mas valores positivos)
    }
    
    agregation <- "Median"  
    indexofPresure <- 2    # 1: "Fresh Water", 2: "Domestic Water", 3: "Industrial Water", 4: "Energy Water", 5: "Livestock Water", 6: "Irrigation Water")
    BarPlot_Stg_Extracciones(Thedifference,weightBYlength,indexofPresure) 
   
    strWater<-c("Fresh Water", "Domestic Water","Energy Water", "Industrial Water","Irrigation Water",  "Livestock Water")
    par(mfrow=c(4,1),mar=c(4.1,4.1,2.1,2.1) )
    
    strgAgrrMetric <- c("mean","median","3Q","Max")  #un indice para cada estrategia de calculo
    for (index in 1:4){
      barplot(t(Thedifference[[indexofPresure]][,(2+index)]) , names.arg = Thedifference[[indexofPresure]]$N2,las=3,  
              ylab=paste0(strgAgrrMetric[index], " Water by Nuts"),
              main=paste0(strgAgrrMetric[index]," ",strWater[indexofPresure] )  ,cex.main=0.9    )
    }
   
      
    
    #Para Las tablas   necesito el valor de la variable agregada (ya calculado)

    #  ### LORO : Devuelve una lista: [[1]] fresW, [[2]]domestic, [[3]]Ener, [[4]]Industria,[[5]]Irri, [[6]]Livest, 
    # Reduzco el numero de decimales
    ExtraccNUT_stg1[[indexofPresure]]  #
    ExtraccNUT_stg1[[indexofPresure]][,3:6] <- lapply(ExtraccNUT_stg1[[indexofPresure]][,3:6] , round, 1)  #reduzco numero de decimales para imprimir
    ExtraccNUT_stg1

    ExtraccNUT_stg2[[indexofPresure]][,3:6] <- lapply(ExtraccNUT_stg2[[indexofPresure]][,3:6] , round, 1)  #reduzco numero de decimales para imprimir
    ExtraccNUT_stg2
    
    
    
#######################################################################################
#######################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - Water Uses - Map by Nuts    #######
######################################################################################
######################################################################################
    
    # En la pestana anterior ya calcule los agregados y las diferencias 

    #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
    NumClassInterv <- 7    #  Numero de categorias a la hora de dibujarlo
    paleta <-  "Greens"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
    estilo <-  "jenks"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
    scale <- "linear"      # "linear"   "logarithmic"
    agregation <- "Mean"         # "Mean"     "Median"      "Q75"         "Max"  
    scaleRelation <- "Common"   #  "Common"   "Independent"
        
    ####### BOTON: Separate Strategies    ################
    
    mytype<-"WU"
    TheVariables_stg1 <- Prepro_to_Map_N2(mytype,ExtraccNUT_stg1,scale,agregation)
    # Le mandamos los indices del uso o los usos a mapear,  e indicamos que metrica quiere
    ListIndexUses <- c(2,3,4,5,6)     # 1: "Fresh Water", 2: "Domestic Water", 3:"Energy Water" , 4:"Industrial Water" , 5:"Irrigation Water", 6:"Livestock Water" )
    Map_var_NUTS_SPPLOT (BasinName, LstMyShapes, mytype,TheVariables_stg1,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
    
    TheVariables_stg2 <- Prepro_to_Map_N2(mytype,ExtraccNUT_stg2,scale,agregation)
    # Le mandamos los indices del uso o los usos a mapear,  e indicamos que metrica quiere
    ListIndexUses <- c(2,3,4,5,6)     # 1: "Fresh Water", 2: "Domestic Water", 3:"Energy Water" , 4:"Industrial Water" , 5:"Irrigation Water", 6:"Livestock Water" )
    Map_var_NUTS_SPPLOT (BasinName, LstMyShapes, mytype,TheVariables_stg2,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
    

    #No se como de facil es para vosotros eliminar lo que sobra en cada mapa (encima y debajo y encajarlo como yo querria) ????
    
    ####### BOTON: Difference    ################
    # la diferencia ya la habia calculado para los barplot
    TheVariables_diff <- Prepro_to_Map_N2(mytype,Thedifference,scale,agregation)  
    # Le mandamos los indices del uso o los usos a mapear,  e indicamos que metrica quiere
    ListIndexUses <- c(2,3,4,5,6)     # 1: "Fresh Water", 2: "Domestic Water", 3:"Energy Water" , 4:"Industrial Water" , 5:"Irrigation Water", 6:"Livestock Water" )
    Map_var_NUTS_SPPLOT (BasinName, LstMyShapes, mytype,TheVariables_diff,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
    
    
           
##############################################################################################
##############################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - Water Uses - 	Box Plots            #######
#############################################################################################
#############################################################################################       
    
    # Ya habia evaluado las estrategias (repito el proceso)
    MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
    MyEval_stg2 <- EvalStg (BLScen,Stg_n2_strateg) 
    
    #  En vuestra interface se selecciona un indice de uso del agua  
    #  2: "Domestic Water", 3:"Energy Water" , 4:"Industrial Water" , 5:"Irrigation Water", 6:"Livestock Water" )
    indexUse <- 3   #lo recibe de shiny
    box_stg1 <- cbind ( MyEval_stg1[[1]][,c(9,(indexUse+1)) ] , "stg1")
    colnames(box_stg1)[3]<- "stg"
    box_stg2 <-  cbind (MyEval_stg2[[1]][,c(9,(indexUse+1)) ] , "stg2")
    colnames(box_stg2)[3]<- "stg"
    box_total <- rbind( box_stg1, box_stg2)

    names(box_total)[2]
   # ggplot(aes( y= energWD , x = N2, fill = stg), data = box_total) + geom_boxplot()
    ggplot(aes_string( y= names(box_total)[2] , x = "N2", fill = "stg"), data = box_total) + geom_boxplot()   #seleccion por nombre
    
    #en ggplot eliminar todos los outliers no es tan facil. Podria tener mas sentido reescalar...
    # pesar en ello
    #
    
 
    
    
    

##############################################################################################
##############################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - WEI - BARPLOT	              #######
#############################################################################################
############################################################################################# 
      
    # Continuo utilizando las dos estrategias seleccionadas del frente de pareto
    stg_n1_strateg
    Stg_n2_strateg
    
    # Evaluo las dos estrategias
    MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
    MyEval_stg2 <- EvalStg (BLScen,Stg_n2_strateg)
    
    ## CALCULA EL WEI para cada subcuenca (para el BLS, o un Scenario al que se han aplicado reducciones)
    ListWDemToWEI <-  c("Domestic", "Energy", "Industrial", "Irrigation", "Livestock")  #lista de los usos para calcular el WEI
    stg1_WEI <- Calcula_WEI(MyEval_stg1[[1]],ListWDemToWEI)   # Devuelve los WEI de cada subcuenca, considerando todas las demandas simultaneamente 
    stg2_WEI <- Calcula_WEI(MyEval_stg2[[1]],ListWDemToWEI) 
    
    ## AGREGACION del WEI PARA CADA NUTS2 (diferentes metricas)(y ponderando o sin poderar) 
    weightBY <-"F"     #F: false (sin ponderar); A: by area ; DA: by Drained Area ;L: longitud   (por definir si ponderar tiene sentido)
    stg1_WEI_By_N2 <- NutsWEIFromSubc (stg1_WEI,weightBY)
    stg2_WEI_By_N2 <- NutsWEIFromSubc (stg2_WEI,weightBY)
    
    ####### BOTON: Separate Strategies    ################
    
      #pongo este codigo (que es solo para una serie).
      # pero vosotros usais plotly, y el usuario selecciona la metrica
 
    agregationIndex <- 2         # 1: "Mean"   2:"Median"  3:"Q75"   4:"Max"  
    #selecciono la columna en ambos
    uno <- stg1_WEI_By_N2[,c(1,(2+agregationIndex))]
    dos <- stg2_WEI_By_N2[,c(1,(2+agregationIndex))]
    tres<- merge(uno,dos, by.x = "N2", by.y = "N2") 
    rownames(tres) <- tres[,1]
    tres <- tres[,-1]
    colnames(tres) <- c("stgA", "StgB")
    cuatro<-t(tres)
    
    par(mfrow=c(1,1))
    barplot((as.matrix(cuatro)) ,
            names.arg = colnames(cuatro),las=1, beside=TRUE, 
            col=rainbow(nrow(cuatro)) , ylab=expression(paste("WEI") ) ,
            main=paste0(" WEI comparison"),
            legend.text=TRUE, args.legend=list(x = "topright", cex = 1.05, bty = "n" ,inset=c(-0.00,-0.07) )  )
    
    
    ####### BOTON: Difference    ################
     #lo mismo vosotros lo haceis con plotly y seleccionas un tipo de agregador, mi codigo es solo un ejemplo
    diferencia <- stg1_WEI_By_N2  # lo hago como pocia, para tener la estructura comun
    diferencia[,3:6] <- stg1_WEI_By_N2[,3:6] - stg2_WEI_By_N2[,3:6] 
    diferencia 

    par(mfrow=c(4,1),mar=c(4.1,4.1,2.1,2.1) )
    
    strgAgrrMetric <- c("mean","median","3Q","Max")  #un indice para cada estrategia de calculo
    for (index in 1:4){
      barplot(t(diferencia[,(2+index)]) , names.arg = diferencia$N2,las=3,  
              ylab=paste0(strgAgrrMetric[index], " Water by Nuts"),
              main=paste0(strgAgrrMetric[index]," ",strWater[indexofPresure] )  ,cex.main=0.9    )
    }
    

  
##############################################################################################
##############################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - WEI - 	Maps by Nuts            #######
#############################################################################################
############################################################################################# 
    
    # Continuo utilizando las dos estrategias seleccionadas del frente de pareto
    stg_n1_strateg
    Stg_n2_strateg
    
    # Evaluo las dos estrategias
    MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
    MyEval_stg2 <- EvalStg (BLScen,Stg_n2_strateg)
    
    ## CALCULA EL WEI para cada subcuenca (para el BLS, o un Scenario al que se han aplicado reducciones)
    ListWDemToWEI <-  c("Domestic", "Energy", "Industrial", "Irrigation", "Livestock")  #lista de los usos para calcular el WEI
    stg1_WEI <- Calcula_WEI(MyEval_stg1[[1]],ListWDemToWEI)   # Devuelve los WEI de cada subcuenca, considerando todas las demandas simultaneamente 
    stg2_WEI <- Calcula_WEI(MyEval_stg2[[1]],ListWDemToWEI) 
    
    ## AGREGACION del WEI PARA CADA NUTS2 (diferentes metricas)(y ponderando o sin poderar) 
    weightBY <-"F"     #F: false (sin ponderar); A: by area ; DA: by Drained Area ;L: longitud   (por definir si ponderar tiene sentido)
    stg1_WEI_By_N2 <- NutsWEIFromSubc (stg1_WEI,weightBY)
    stg2_WEI_By_N2 <- NutsWEIFromSubc (stg2_WEI,weightBY)
    
    
    ####### BOTON: Separate Strategies    ################
    
    #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
    NumClassInterv <- 8    #  Numero de categorias a la hora de dibujarlo
    paleta <-  "Greens"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
    estilo <-  "quantile"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
    scale <- "linear"      # "linear"   "logarithmic"
    agregation <- "Median"         # "Mean"     "Median"      "Q75"         "Max"  
    scaleRelation <- "Independent"   #  "Common"   "Independent"
    
    # Le mandamos los indices del uso o los usos a mapear,  e indicamos que metrica quiere
    liststg1_WEI_By_N2 = list(stg1_WEI_By_N2) #en lugar de los usos, le paso el WEI con diferentes tipos de agregacion, y lo tengo que convertir a lista
    mytype<-"WEI"
    TheVariables <- Prepro_to_Map_N2(mytype,liststg1_WEI_By_N2,scale,agregation)
    ListIndexUses <- c(1)    # 1: "WEI"   Con lo que su indice es directamente el 1
    Map_var_NUTS_SPPLOT (BasinName, LstMyShapes,mytype, TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
    
    liststg2_WEI_By_N2 = list(stg2_WEI_By_N2) #en lugar de los usos, le paso el WEI con diferentes tipos de agregacion, y lo tengo que convertir a lista
    mytype<-"WEI"
    TheVariables <- Prepro_to_Map_N2(mytype,liststg2_WEI_By_N2,scale,agregation)
    ListIndexUses <- c(1)    # 1: "WEI"   Con lo que su indice es directamente el 1
    Map_var_NUTS_SPPLOT (BasinName, LstMyShapes,mytype, TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
    
    ####### BOTON: Difference    ################
    diferencia <- stg1_WEI_By_N2  # lo hago como pocia, para tener la estructura comun
    diferencia[,3:6] <- stg1_WEI_By_N2[,3:6] - stg2_WEI_By_N2[,3:6] 
    liststdiferencia = list(diferencia) #en lugar de los usos, le paso el WEI con diferentes tipos de agregacion, y lo tengo que convertir a lista
    mytype<-"WEI"
    TheVariables <- Prepro_to_Map_N2(mytype,liststdiferencia,scale,agregation)
    ListIndexUses <- c(1)    # 1: "WEI"   Con lo que su indice es directamente el 1
    estilo <-  "pretty"
    paleta <-  "BuPu" 
    NumClassInterv <- 8    #atencion parece que el maximo de colores en esta paleta son 8 o 12, dependiendo
    Map_var_NUTS_SPPLOT (BasinName, LstMyShapes,mytype, TheVariables,ListIndexUses,NumClassInterv,paleta,estilo,scale,agregation,scaleRelation )
    
    
   
    
##############################################################################################
##############################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - WEI -   Box Plot          #######
#############################################################################################
#############################################################################################
    
    # Continuo utilizando las dos estrategias seleccionadas del frente de pareto
    stg_n1_strateg
    Stg_n2_strateg
    
    # Evaluo las dos estrategias
    MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
    MyEval_stg2 <- EvalStg (BLScen,Stg_n2_strateg)   
    
    # Calcula el WEI para ambas estrategias
    ListWDemToWEI <- c("Domestic", "Energy", "Industrial", "Irrigation", "Livestock")
    stg1_WEI <- Calcula_WEI(MyEval_stg1[[1]],ListWDemToWEI)   # Devuelve los WEI de cada subcuenca, considerando todas las demandas simultaneamente 
    stg2_WEI <- Calcula_WEI(MyEval_stg2[[1]],ListWDemToWEI)   # Devuelve los WEI de cada subcuenca, considerando todas las demandas simultaneamente 
    
    ####### BOTON: Separate Strategies    ################
    par(mfrow=c(1,2))
    boxplot(stg1_WEI$WEI~stg1_WEI$N2 ,ylab="WEI", las=2, col=c("snow2"),
            outline=TRUE, ylim=c(0,max(stg1_WEI$WEI)),main=" WEI stg1" )
    boxplot(stg2_WEI$WEI~stg2_WEI$N2 ,ylab="WEI", las=2, col=c("snow2"),
            outline=TRUE, ylim=c(0,max(stg2_WEI$WEI)),main=" WEI stg2" )
    
    boxplot(stg1_WEI$WEI~stg1_WEI$N2 ,ylab="WEI", las=2,col=c("snow2"),
            outline=FALSE, main="WEI Without ouliers" )
    
    # quizas mejor con ggplot
    
    Thebox_data <- cbind ( stg1_WEI[,c(9,10)] , stg2_WEI[,c(10)])
    colnames(Thebox_data) <- c("stgA","stgB")
    
    
    box_stg1 <- cbind ( stg1_WEI[,c(9,10) ] , "stg1")
    colnames(box_stg1)[3]<- "stg"
    box_stg2 <- cbind (stg1_WEI[,c(9,10) ] , "stg2")
    colnames(box_stg2)[3]<- "stg"
    box_total <- rbind( box_stg1, box_stg2)
    
    names(box_total)[2]
    # ggplot(aes( y= energWD , x = N2, fill = stg), data = box_total) + geom_boxplot()
    ggplot(aes_string( y= names(box_total)[2] , x = "N2", fill = "stg"), data = box_total) + geom_boxplot()   #seleccion por nombre
    
    
    
    ########   BOTON:   Difference  ######
    TheDiference <- merge ( stg1_WEI[c(1,9,10)], stg2_WEI[c(1,9,10)] , by.x="Row.names", by.y="Row.names")  
    TheDiference$diff <- TheDiference$WEI.x - TheDiference$WEI.y
    
    boxplot(TheDiference$diff ~ TheDiference$N2.x ,ylab="WEI", las=2,col=c("snow2"),
            outline=FALSE, main="WEI difference" )
    #quizas se pueden anadir los puntos con un Jiter 
    points(factor(TheDiference$N2.x), TheDiference$diff,col=3)  #arregal y rematar ,ordenarlos tambien
  
    
    
##############################################################################################
##############################################################################################
### MENU SHINY:  OPTIMIZATION - Analysis - WEI -   Maps by Subcatchment          #######
#############################################################################################
############################################################################################# 
    
    # Continuo utilizando las dos estrategias seleccionadas del frente de pareto
    stg_n1_strateg
    Stg_n2_strateg
    
    # Evaluo las dos estrategias
    MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
    MyEval_stg2 <- EvalStg (BLScen,Stg_n2_strateg)   
    
    # Calcula el WEI para ambas estrategias
    ListWDemToWEI <- c("Domestic", "Energy", "Industrial", "Irrigation", "Livestock")
    stg1_WEI <- Calcula_WEI(MyEval_stg1[[1]],ListWDemToWEI)   # Devuelve los WEI de cada subcuenca, considerando todas las demandas simultaneamente 
    stg2_WEI <- Calcula_WEI(MyEval_stg2[[1]],ListWDemToWEI)   # Devuelve los WEI de cada subcuenca, considerando todas las demandas simultaneamente 
    
    
    ####### BOTON: Separate Strategies    ################
    
    #Parametros que se le pasan a las funciones que hacen mapas en las que las variables van en colores (debo anadir la opcion B/W)
    NumClassInterv <- 7    #  Numero de categorias a la hora de dibujarlo
    paleta <-  "BuPu"    #   "Greys"  "Blues" "Greens"   BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
    estilo <-  "kmeans"  #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
    scale <- "linear"      # "linear"   "logarithmic"
    scaleRelation <- "Independent"   #  "Common"   "Independent"
    
    #Para que siga funcionando la funcion de los mapas, le pasamos el data frame que anade el WEI, 
    # indicando el indice de la columna del WEI (la 10)
    ListIndexUses <- c(9)     # 1: "Fresh Water", 2: "Domestic Water", 3: "Industrial Water", 4: "Energy Water", 5: "Livestock Water", 6: "Irrigation Water" , 9: WEI)
    Map_var_SubCatch_SPPLOT (BasinName, LstMyShapes, stg1_WEI, ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation )
    Map_var_SubCatch_SPPLOT (BasinName, LstMyShapes, stg2_WEI, ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation )
    
    
    #tengo que ver como meter ambos en la misma escala 
    
    
    ####### BOTON: Diference    ################
      #nuevo data frame con la diferencia (aunque las filas esten desordenadas)  
      stg_Diff_WEI <- within(merge(stg1_WEI,stg2_WEI,by="Row.names"), {
          fresW <-  fresW.x
          domestWD <- domestWD.x
          energWD <- energWD.x
          industWD <- industWD.x
          irrWD <- irrWD.x
          livesWD <- livesWD.x
          Area <- Area.x
          N2 <- N2.x
          WEI <-  WEI.x - WEI.y  
       })[,c("Row.names","fresW","domestWD","energWD","industWD","irrWD",
             'livesWD',"Area","N2","WEI")]
      
    #solo me interesa la diferencia del WEI, calculo todas, para que este en la misma columan y respete el nombre
    #en el futuro intentar otra forma para los nombres de las variables a poner
    
      #como el DF tiene menos columnas (le indico el indice 1, cambia la correspondencia)
      ListIndexUses <- c(9)     # 1: "Fresh Water", 2: "Domestic Water", 3: "Industrial Water", 4: "Energy Water", 5: "Livestock Water", 6: "Irrigation Water" , 9: WEI)
      Map_var_SubCatch_SPPLOT (BasinName, LstMyShapes, stg_Diff_WEI, ListIndexUses, NumClassInterv,paleta,estilo,scale,scaleRelation )

      
      
####################################################################################
#########################  FIN DE PROGRAMA PRINCIPAL      #########################
####################################################################################       
####################################################################################   
      
      
      
      
      
      
      
###########################################################################################
##### GENERACION DE ESTRATEGIAS ALEATORIAS Y COMPARACION CON ESTRATEGIAS EFICIENTES #######
###########################################################################################
      
     ##  1. Seleccion de la cuenca 
      BasinName <-"Ebro"    #     "Evrotas"  "Adige"     "Sava"   "Ebro"   
      
     ## 2. Genera un escenario BAse (si no lo habia generado antes, que es lo normal)
      BLScen <- GenerateBLS (BasinName,List_WQnI, C_SC_Rel,NUTS_Corr)  
      The_N2 <- sort(as.character(unique(BLScen[[6]]$N2)))  #para ordenar bien los tengo que convertir
      sum(BLScen[[6]]$fresW)   #esto seria la suma de todo el fresWater
      # Calcula cual es la demanda (NO ROUTING) de cada region para cada sector     # Guardo ese valor de DEMANDA NO ROUTING, como [[7]] elemento del BLScen
      BLScen[[7]] <- ComputeNotRoutDemand(BLScen)  #ya queda guardada en el BLS

     ## 3. Carga de los limites maximos de reduccion
      TheMaxReductions <- Loading_Max_PressRed (BASE_IN_DATA,BasinName) 

     ## 4. Crea Matriz de Costes (Esfuerzo de cada reduccion). Por defecto ponderando todo igual  
      Equipeso <- 1
      dimy <- length(The_N2)
      Mat_Costes <- matrix( rep(Equipeso/100,(5*dimy)) ,ncol=dimy )
      colnames(Mat_Costes) <- The_N2
      rownames(Mat_Costes) <- c("Domest","Energ","Indust","Irrig","Livest")
    
    ## 5. Configuracion para ejecutar el optimizador
      indexAgrM <- c(2,2.1)   #Segundo parametro, el umbral (solo es necesario si se escoge el tipo 5 de agregacion)
      signOb <- c(1,1)   #positivo si es minimizar y negativo si es maximizar (en este caso queremos minimizar ambos)
      genParam <- c(2,50,100)   #  nobject<-2 ; popSize=60 ; generations=40
      # PonderEffor <- NULL  # la matriz de ponderacion sera lo que se actualizara en cada iteracion
      #  myConfig <- list(PonderEffor,indexAgrM,signOb,genParam)
      myConfig <- list(Mat_Costes,indexAgrM,signOb,genParam) #Matriz  equicoste, de ponderacion para las ejecuciones
      
    ## 6. Busca soluciones optimas  
      myParetoMedian <- MOO_GA(BLScen,TheMaxReductions, myConfig) #Ejecuta la optimizacion
      
    ## 7. Dibuja el frente de Pareto  
      plot(myParetoMedian$objectives[,1]/1000000,myParetoMedian$objectives[,2],
               xlim=c(0,max(myParetoMedian$objectives[,1]/1000000)),col="blue",
               xlab="Effort",ylab="WEI",main=BasinName,pch = 19)
          
        #  plot(myParetoMedian$objectives[,1]/1000000,myParetoMedian$objectives[,2],ylim=c(0.015,0.04))
        #  plot(myParetoMedian$objectives[,1]/1000000,myParetoMedian$objectives[,2],col="blue")
        
          # 6.1 comprobacion: estraemos una estrategia y la evalua
          stg <-myParetoMedian$parameters[1,]
          myParetoMedian$objectives[1,]
          
          Random_strateg <- t(matrix( stg ,ncol=dimy ))
          colnames(Random_strateg) <- The_N2
          rownames(Random_strateg) <- c("Domest","Energ","Indust","Irrig","Livest")
          stg_deco <- DecodeStg (BLScen,stg)  #Alternativamente lo siguiente la convierte tambien
          Eval_stg_deco <- EvalStg (BLScen,std_deco) 
          PonderEffor <- myConfig[[1]]
          ComputeEffort(Eval_stg_deco,PonderEffor)
          # Calcula el WEI para cada subcuenca (para la Scenario al que se han aplicado reducciones)
          ListWDemToWEI <- c("Domestic", "Industrial", "Energy", "Livestock", "Irrigation")  #Obligatorio con todas las demandas
          stg_deco_WEI <- Calcula_WEI(Eval_stg_deco[[1]],ListWDemToWEI)  #el wei por subcuenca
          median(stg_deco_WEI$WEI )
          
          
     ## 5. Generacion de estrategias aleatoria  
      numStg <- 100   #numero de estrategias a generar y evaluar

      for (i in 1:numStg){
        Random_strateg <- matrix( runif(dimy*5, 0.0, 50) ,ncol=dimy ) #genera numeros aleatorios correspondientes al ratio de reduccion. El maximo deberia estar limitado por la maxima reduccion
        colnames(Random_strateg) <- The_N2
        rownames(Random_strateg) <- c("Domest","Energ","Indust","Irrig","Livest")
        
        Evalua_RS <- EvalStg (BLScen,Random_strateg)    #devuelve [[1]]el nuevo escenario de demandas, y [[2]]la diferencia de dichas demandas con relacion al BLS
       
        PonderEffor <- myConfig[[1]]
        TheEffort <- ComputeEffort(Evalua_RS,PonderEffor)/1000000  #esfuerzo
         # Effort <- sum(Evaluat_RS[[2]])/1000000   #esto daria el mismo resultado
        
        #  Calcula el WEI para cada subcuenca (para la Scenario al que se han aplicado reducciones)
        ListWDemToWEI <- c("Domestic", "Industrial", "Energy", "Livestock", "Irrigation")  #Obligatorio con todas las demandas
        BLS_WEI <- Calcula_WEI(Evalua_RS[[1]],ListWDemToWEI)  #el wei por subcuenca
        theWEI <- median(BLS_WEI$WEI)  #agregacion a media
        
            # 2.2 Agregamos WEI para toda la cuenca  (diferentes metricas)
          #  weightBY <-"F"     #F: false (sin ponderar); Por ahora siempre asi
          #  threshold <- 4  # si no le pasamos el umbral, toma un valor por defecto (al calcular esa metrica)
          #  Stg_TotWEI <- TotWEIFromSubc (BLS_WEI ,weightBY,threshold)  #atencion, si considermas el umbral tenemos que recibir el parametro
        
        points( TheEffort, theWEI, col="red" ,pch = 19) 
        
      }

      
      #Algunas Estrategias Uniformes
      numStg <- 50   #numero de estrategias a generar y evaluar
      for (i in 1:numStg){
        Random_strateg <- matrix( rep(i,(dimy*5))  ,ncol=dimy ) #genera numeros aleatorios correspondientes al ratio de reduccion. El maximo deberia estar limitado por la maxima reduccion
        colnames(Random_strateg) <- The_N2
        rownames(Random_strateg) <- c("Domest","Energ","Indust","Irrig","Livest")
        
        Evalua_RS <- EvalStg (BLScen,Random_strateg)    #devuelve [[1]]el nuevo escenario de demandas, y [[2]]la diferencia de dichas demandas con relacion al BLS
        
        PonderEffor <- myConfig[[1]]
        TheEffort <- ComputeEffort(Evalua_RS,PonderEffor)/1000000  #esfuerzo
        # Effort <- sum(Evaluat_RS[[2]])/1000000   #esto daria el mismo resultado
        
        #  Calcula el WEI para cada subcuenca (para la Scenario al que se han aplicado reducciones)
        ListWDemToWEI <- c("Domestic", "Industrial", "Energy", "Livestock", "Irrigation")  #Obligatorio con todas las demandas
        BLS_WEI <- Calcula_WEI(Evalua_RS[[1]],ListWDemToWEI)  #el wei por subcuenca
        theWEI <- median(BLS_WEI$WEI)  #agregacion a media
        
        # 2.2 Agregamos WEI para toda la cuenca  (diferentes metricas)
        #  weightBY <-"F"     #F: false (sin ponderar); Por ahora siempre asi
        #  threshold <- 4  # si no le pasamos el umbral, toma un valor por defecto (al calcular esa metrica)
        #  Stg_TotWEI <- TotWEIFromSubc (BLS_WEI ,weightBY,threshold)  #atencion, si considermas el umbral tenemos que recibir el parametro
        
        points( TheEffort, theWEI, col="green" ,pch = 19) 
        
      }
      
      
      legend("topright",inset=.02, legend=c("Pareto Stg", "Random Stg","Uniform Stg" ),
             col=c("red", "blue","green"),  cex=1.0,  pch=c(19,19,19),  box.lty=0)
  

      
      
      
      
      
      
    
      
      
      

      
      mat1 <- matrix ( seq(6),ncol=2)
      mat2 <- matrix ( 2+seq(6),ncol=2)   
      mat3 <- matrix ( 5+seq(6),ncol=2) 
      myList <- list(mat1,mat2,mat3)
      
      lapply(myList,mean)
      sapply(myList, quantile) 
      myList[[1]]+myList[[2]]
      Reduce(`+`, myList)
      
      Reduce(`+`, myList[1:3])  #se puede seleccionar solo algunos elementos de la lista para la operacion
      
      Reduce(`*`, myList[1:3])
      Reduce(mean, myList[1:3])
      
      mat1+mat2
      
      apply(simplify2array(myList), 1:2, mean)
      apply(simplify2array(myList), 1:2, sd)  # Element-wise mean over list of matrices
      
      indexAgrM <- c(2,2.1)   #Segundo parametro, el umbral (solo es necesario si se escoge el tipo 5 de agregacion)
      signOb <- c(1,1)   #positivo si es minimizar y negativo si es maximizar (en este caso queremos minimizar ambos)
      genParam <- c(2,40,100)   #  nobject<-2 ; popSize=60 ; generations=40
      # PonderEffor <- NULL  # la matriz de ponderacion sera lo que se actualizara en cada iteracion
      #  myConfig <- list(PonderEffor,indexAgrM,signOb,genParam)
      myConfig <- list(Mat_Costes,indexAgrM,signOb,genParam) #Matriz  equicoste, de ponderacion para las ejecuciones
      myParetoMedian2 <-  MOO_GA(theScen,MaxRateRed, myConfig) #Ejecuta la optimizacion
      
      
      plot(myParetoMedian2$objectives[,1]/1000000,myParetoMedian2$objectives[,2],ylim=c(0.015,0.04))
      plot(myParetoMedian2$objectives[,1]/1000000,myParetoMedian2$objectives[,2],col="blue")
      
      
      My_strateg <- DecodeStg(BLS,x) # Decodifica la estrategia x. Solo Necesita el BLS para la dimension
      baseStg<-My_strateg
      
      for(iter in 1:50){
        My_strateg <- baseStg * runif(1, 0.1,5)
     #   My_strateg  <- matrix(runif(20,  0.1,34), ncol=4)
        
        # My_strateg  <- matrix( c(16.310877972, 49.9969579, 39.3225099,  2.879720020,
        #                        43.68036326, 41.03281173, 45.53873700, 4.853836806, 
        #                        42.8037602, 49.903321,  4.420157189,  12.51275484,
        #                        47.51973699, 18.2977368, 46.1988944,  4.5228100, 
        #                        47.663757, 47.1892090, 38.8544014,   24.0380979), ncol=4)
        #                        
        # 2. EVALUAMOS LA ESTRATEGIA ( criteri1: WEI . Criterio 2: Esfuerzos)
        EvaluatedS <- EvalStg (BLScen,My_strateg)    #devuelve [[1]]el nuevo escenario de demandas, y [[2]]la diferencia de dichas demandas con relacion al BLS
        
        # 2.1 Calcula el WEI para cada subcuenca (para la Scenario al que se han aplicado reducciones)
        ListWDemToWEI <- c("Domestic", "Industrial", "Energy", "Livestock", "Irrigation")  #Obligatorio con todas las demandas
        BLS_WEI <- Calcula_WEI(EvaluatedS[[1]],ListWDemToWEI) 
        median(BLS_WEI$WEI )
        
        # 2.2 Agregamos WEI para toda la cuenca  (diferentes metricas)
        weightBY <-"F"     #F: false (sin ponderar); Por ahora siempre asi
        threshold <- 4  # si no le pasamos el umbral, toma un valor por defecto (al calcular esa metrica)
        opti_Scen_WEI <- TotWEIFromSubc (BLS_WEI ,weightBY,threshold)  #atencion, si considermas el umbral tenemos que recibir el parametro
        
        # 2.3 Esfuerzos  (los consideramos con Ponderacion) (sera otro vector a recibir )
        PonderEffor <- c("D")  # Configlist[[1]]     # c(.30 ,0.10 ,0.0 , 0.2, 0.20) # rep(0,nrow(My_strateg2))     
        #     names(PonderEffor) <- c("Domest","Energ","Indust","Irrig","Livest")                             
        Effort <- ComputeEffort(EvaluatedS,myConfig[1]) /1000000 # el valor del esfuerzo
        print(Effort)
        Effort <- sum(EvaluatedS[[2]])/1000000
        
        points( Effort, opti_Scen_WEI[3], col="red" ,pch = 19)
      }
      
      #############################################################
      ####### FIN DE GENERACION DE ESTRATEGIAS ALEATORIAS #########
      #############################################################
      
      
      
      
      
      
  
  
  
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
      
      
      
      
      
  
  
  
  
  
  
  
# 2. Cargando contornos de NUTS2 y SubCuencas para hacer mapas
PATH_MAPS <- "DATA/"  
PATH_MAPS <- paste(PATH_NAME,PATH_MAPS,sep="")
NUTS2_polig<-"nuts2esrilaea_polygons/nuts2esrilaea_polygons.shp"
Catch_polig<-"catchment_polygons/catchment_polygons.shp"

  InfoMaps<-Loading_MapsInfo(PATH_MAPS,NUTS2_polig,Catch_polig )  #tarda bastante 
  InfoMaps[[1]]   # informacion  poligonos NUTS2
  InfoMaps[[2]]   # informacion  lineas NUTS2 
  class(InfoMaps[[1]])  #poligonos
  class(InfoMaps[[2]])
  InfoMaps[[3]]   # informacion  pol.cuencas 
  InfoMaps[[4]]   # informacion  cuencas 
  InfoMaps[[5]]   # informacion  DF_lim_map  (limites para cada pais) 

  
  
  InfoMaps <- listTotalShapes   #por si tengo que hacer pruebas hasta que tenga nueva version
  
#class(InfoMaps[[1]] )
# 3. PLOTING NUTS2
  # 3.1 Ploting 1 NUTS2 seleccionada por objectID
  listofNUTS2_ID<-11   #la provincia con objectid 1
  Plot_NUTS2(InfoMaps[[1]],listofNUTS2_ID)

  # 3.2 Ploting varios NUTS2 seleccionada por ID
  listofNUTS2_ID<-c(10,11)   #la provincia con objectid 1
  Plot_NUTS2(InfoMaps[[1]],listofNUTS2_ID)


# 4. PLOTING SubCatchtment
  # 4.1 Ploting 1 Catchtment seleccionada por hydroid
  listofCH_ID<-"2000001"   #la provincia con objectid 1
  Plot_Catch(InfoMaps[[3]],listofCH_ID)

  # 4.2 Ploting varios Catchtment seleccionada por hydroid
  listofCH_ID<-c(2000001,2000002,2000003,2000005,2000011)   #la provincia con objectid 1
  Plot_Catch(InfoMaps[[3]],listofCH_ID)


  
  
  
# 5. Loading Water Quantity Info
  BASE_IN_DATA <- "data/QT/"   
  BASE_IN_DATA <- paste(PATH_NAME,BASE_IN_DATA,sep="")
 # BASE_IN_DATA= "D:/DISCO_F/2016ENERO/GLOBAQUA/DATA/NCDFData/"
  ncname <- "WEI_optimisation_data"
  List_WQnI <- Loading_WQnInfo(BASE_IN_DATA,ncname)
#  Nuts2ObjID,CatchmentID,intersec_fracc,r_FresW,r_DomestWD,r_IndustWD,r_EnergWD,r_LivesWD,r_IrrWD,c_area
  Nuts2ObjID  <-List_WQnI[[1]]     #ATENCION que son MATRICES no Data Frames
  head(Nuts2ObjID)
  CatchmentID <-List_WQnI[[2]]    #from 2000001  to 92003782
  intersec_fracc<- List_WQnI[[3]]
  head(intersec_fracc)
  r_fresW <-List_WQnI[[4]]
  head(r_fresW)
  class(r_fresW)
  length((r_fresW))
  
  #DIFERENCIA ENTRE r_fresW Y intersec_fracc PARA UN NUTS
  indexNUT <- 228    #seleccionamos un NUTS2
  
  dim(r_fresW)
  length( which(r_fresW[,indexNUT]==1) )
  #separo parte comun y parte diferente
  index_Inter <- which(intersec_fracc[,indexNUT]==1)   # subcathment que estan contenidos totalmente en ese NUT
  index_Inter2 <- which(intersec_fracc[,indexNUT]>0)  #subcathment contenidos total o parcialmente en ese NUT
  index_rfresh <- which(r_fresW[,indexNUT]>0)     #se refiere a los subcathcment sobre los que influye ese NUT
  length(index_Inter)
  length(index_rfresh)
  index_Inter %in% index_rfresh
  notinInter <- index_rfresh[!(index_rfresh %in%  index_Inter)]  #subcuencas influenciadas pero no contenidas en ese NUT
  length(index_rfresh[!(index_rfresh %in%  index_Inter)] )
  intersect(index_Inter,index_rfresh)
  setdiff(index_Inter,index_rfresh)
  
  
  ListSubCathc<-names(index_Inter)  #lista de nombres de las subcuencas que intersecan con NUT 228
  ListnameNuts2<- as.character(Nuts2ObjID[indexNUT] )   
  #los dibujo
  Plot_NUTS2(InfoMaps[[1]],ListnameNuts2)  #se lo puedo pasar como nombre o como id, pero CUIDADO, QUE NO APUNTAN A LO MISMO
  Plot_SubCatch(InfoMaps[[3]],ListSubCathc)
  Plot_NUTS2_SubCatch(InfoMaps[[1]],ListnameNuts2,InfoMaps[[3]],ListSubCathc)
  ListSubCathc2<-names(index_Inter2)
  Plot_NUTS2_SubCatch(InfoMaps[[1]],ListnameNuts2,InfoMaps[[3]],ListSubCathc2)  #modificar ese plot
  
  ListSubCathc2 <- names(index_rfresh)
  Plot_SubCatch(InfoMaps[[3]],ListSubCathc2)

  ListSubCathc2 <- names(notinInter)
  Plot_SubCatch(InfoMaps[[3]],ListSubCathc2)
  
  
  
  
  
  
  
  
  
  
  
  
  


  

# 6. Loading Catchment - Subcatchment pertenence (UN EXCEL CON ESTA INFORMACION)
  PATH_C_SC <- "DATA/Catch_Rel_SubC/"
  PATH_C_SC <- paste(PATH_NAME,PATH_C_SC,sep="")
  filename <- "basin_catchment_lookup.csv"
  C_SC_Rel<-Loading_C_SC_Rel(PATH_C_SC,filename)
#  class(C_SC_Rel)
#  head(C_SC_Rel)
#  class(C_SC_Rel$basin_name)
#  class(C_SC_Rel$cathment_hydroid)
#########################################

  
  
  
  
  
  
  

 #######     #INTERSECAR LAS REGIONES y SUBCATCHMENT Y PINTAR TODO

# 7. Understanding the INTERSECCTION variable,intersec_fracc[catch,Nuts2]
  # Fraction of catchment area intersected by NUTS2. 0:nothing; 1: all.
  
  # 7.1 En que NUTS2 esta una SubCatchmen (cuando esta solo en uno). Por ejemplo el Subcatchment que esta en la primera posicion
  ListSubCathc<-as.character(CatchmentID[1])  #lista de nombres
  intersec_fracc[1,] #lista todas las subcuencas, valor a 0 las que no intersecta con NUTS2=1
  which(intersec_fracc[1,]!=0) #devuelve el nombre de la columna y el indice de la columna
  ListnameNuts2<-names(which(intersec_fracc[1,]!=0))  #lista de nombres
  intersec_fracc[1,220:240]  #es decir para Catchment=1, interseca con region 238 en columna 229
  intersec_fracc[1,229]  #por indice de la columna
  intersec_fracc[1,"238"]  #accediendo por nombre de la columna
  intersec_fracc[1,c("238")] #o equvalente por nombre de la columna 
  intersec_fracc[1,ListnameNuts2] #lo mismo
  intersec_fracc[ListSubCathc,ListnameNuts2]   #accediendo con los nombres de catch and nuts
  #los dibujo
  Plot_NUTS2(InfoMaps[[1]],ListnameNuts2)  #se lo puedo pasar como nombre o como id, pero CUIDADO, QUE NO APUNTAN A LO MISMO
  Plot_SubCatch(InfoMaps[[3]],ListSubCathc)
  Plot_NUTS2_SubCatch(InfoMaps[[1]],ListnameNuts2,InfoMaps[[3]],ListSubCathc)

  # 7.2 En que NUTS2 estan contenidos un conjunto de subcuencas (NADA FACIL DE HACER EL CRUCE)
  ListSubCathc <- as.character(CatchmentID[1:2])  #varios subcatchment, por ejemplo los dos primeros
#  class(ListSubCathc)
#  class(intersec_fracc)
#  which(intersec_fracc[1:2,]==1)
#  which(intersec_fracc[ListSubCathc[1],]==1)
 # which(intersec_fracc[ListSubCathc[2],]==1) 
#  which(intersec_fracc[CatchmentID %in% ListSubCathc,]==1)  #
#  nameNUTS<-names(which(intersec_fracc[ListSubCathc,]==1)) 
  tempRows <- intersec_fracc[CatchmentID %in% ListSubCathc, ,drop=FALSE]  #hago un subset con solo las filas que estan en esas cuencas
  which(colSums(tempRows)!=0)  # !=0 si queremos incluir tambien los que estan en dos NUTS
  ListnameNuts <-  names(which(colSums(tempRows)!=0)  ) #otro subset con solo las columnas en las que hay valores a 1
   #DIBUJO los N2 y los subcatchment 
  Plot_NUTS2(InfoMaps[[1]],ListnameNuts)
  Plot_SubCatch(InfoMaps[[3]],ListSubCathc)
  Plot_NUTS2_SubCatch(InfoMaps[[1]],ListnameNuts,InfoMaps[[3]],ListSubCathc)


  # 7.3 SUBCatchmen que interseca con varios NUTS2 (esta en varios NUTS2)
  # Buscamos un catchment que interseque con varias provincias
  for (inCach in 1:50){  #lo buscamos entre los primeros 50 catchments
    index<-which(intersec_fracc[inCach,]!=0,arr.ind = T) 
    print(paste(inCach,index,sep=" "  ) )
  }
  #por ejemplo para el SUBcathment que esta en el indice 38
  ListSubCathc<-as.character(CatchmentID[38])
  which(intersec_fracc[38,]!=0)
  ListnameNuts2<-names(which(intersec_fracc[38,]!=0))
  #los dibujo
  Plot_NUTS2(InfoMaps[[1]],ListnameNuts2)  #se lo puedo pasar como nombre o como id, pero CUIDADO, QUE NO APUNTAN A LO MISMO
  Plot_SubCatch(InfoMaps[[3]],ListSubCathc)
  Plot_NUTS2_SubCatch(InfoMaps[[1]],ListnameNuts2,InfoMaps[[3]],ListSubCathc)

  # 7.4 SUBCatchmet contenidos en un NUTS2 (por ejemplo NUTS2 en la segunda columna)
  #recordar que el NUTS valor 0 es particular
  ListnameNuts2<-as.character(Nuts2ObjID[229]) #ese es el nombre del NUTS ya como caracter
  class(ListnameNuts2) #es character
  # Lo siguiente seria los catchment que estan TOTAL o PARCIALMENTE en le NUTS (condicion !=0)
  which(intersec_fracc[,ListnameNuts2]!=0)  #nombres de subcuencas y debajo posicion de las que intersecan EL NUTS  de la posicion 229 (provincia)
  ListSubCathc<-names(which(intersec_fracc[,ListnameNuts2]!=0)) #todos los catchment que intersecan con ese NUTS
  #DIBUJO
  Plot_NUTS2(InfoMaps[[1]],ListnameNuts2)  #se lo puedo pasar como nombre o como id, pero CUIDADO, QUE NO APUNTAN A LO MISMO
  Plot_SubCatch(InfoMaps[[3]],ListSubCathc)
  Plot_NUTS2_SubCatch(InfoMaps[[1]],ListnameNuts2,InfoMaps[[3]],ListSubCathc)

  # Lo siguiente seria los catchment que estan TOTALMENTE Dentro del NUTS   (condicion ==1)
  ListSubCathc<-names(which(intersec_fracc[,ListnameNuts2]==1)) #todos los catchment que intersecan EL NUTS  de la posicion 229
  #DIBUJO
  Plot_NUTS2(InfoMaps[[1]],ListnameNuts2)  #se lo puedo pasar como nombre o como id, pero CUIDADO, QUE NO APUNTAN A LO MISMO
  Plot_SubCatch(InfoMaps[[3]],ListSubCathc)
  Plot_NUTS2_SubCatch(InfoMaps[[1]],ListnameNuts2,InfoMaps[[3]],ListSubCathc)


  # 8. Understanding the routed_Freshwate    r_fresW[catch]     # valor del routing agua disponible
  head(r_fresW)
  head(intersec_fracc)
  #esta cantidad debe ser diferente en cada subcatchment. Plot fresW para todos los cathch del NUTS2
  ListnameNuts2<-as.character(Nuts2ObjID[229]) #ese es el nombre del NUTS ya como caracter
  ListSubCathc<-names(which(intersec_fracc[,ListnameNuts2]==1)) # las cuencas que estan en esa region
  Plot_SubCatch(InfoMaps[[3]],ListSubCathc,nombre="NUTS 229")  #esto no dibuja el flow con diferente color
  nombreV<-"Fresh Water"
  Plot_SubCatch_Var(InfoMaps[[3]],ListSubCathc,nombre="NUTS 229",r_fresW,nombrevar=nombreV)


      #ATENCION DEBERIA CONVERTIR A STRING EL NUMERO DEL SUBCATCHMENT (??????)

  #####################################################
  ######### WORKING WIHT WHOLE CATCHMENT ####################
  ######################################################  

  # 9. Ploting All de subcatch of a CATCHMENT
  BasinsList<-unique(C_SC_Rel$basin_name)
  BasinsList
  
  # 9.1 El Danubio (lo dibuja)
  nameCatch<-"Danube"
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,nameCatch) 
    
  # 9.2 El Ebro (lo dibuja)
  nameCatch<-"Po"
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,nameCatch)  #sin ninguna variable
  #Dibujando alguna variable en color (por ejemplo el freswater)
  nombreV<-"Fresh Water"
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,nameCatch,r_fresW,nombrevar=nombreV)

  ###################################
  # 10. INTERSECCION ENTRE CATCHMENT Y NUTS2
  # A partir del catchment, saco la lista de subcathch y de esta todos los NUTS2
  # 10.1 Ejemplo 1:    ###
  BasinsList<-unique(C_SC_Rel$basin_name)
  # "Ter"   "Ebro"  "Loire" "Danube"   "Rhine" "Tajo" "Duero"  "Jõcar"  "Po"  "Guadalquivir"  "Adige"
  namecatch <-"Ebro"
   # La siguiente funcion tiene dos modos para devolver relacion entre un Catchment y los N2 en los que esta
  # Mode: 1-> DF relacion SC-N2;  2-> Ratio AreaSubcatch in each N2.   Por defecto devuelve la lista
  rel1_N2_cth<-NUTS2_in_Catch(namecatch,CatchmentID,C_SC_Rel,intersec_fracc,Mode=1)
  rel2_N2_cth<-NUTS2_in_Catch(namecatch,CatchmentID,C_SC_Rel,intersec_fracc,Mode=2)
  rel1_N2_cth
  ListSubCatCh<-unique(rel1_N2_cth$SC) # solo los nombres de los Subcatchment
  length(ListSubCatCh)
  rel2_N2_cth  #con ratios para cada N2
  ListnameNuts<-names(rel2_N2_cth)  # solo los nombres de los N2
  length(ListnameNuts)
  
  #DIBUJO los N2 y los subcatchment 
  Plot_NUTS2(InfoMaps[[1]],ListnameNuts)
  Plot_SubCatch(InfoMaps[[3]],ListSubCatCh)
  Plot_NUTS2_SubCatch(InfoMaps[[1]],ListnameNuts,InfoMaps[[3]],ListSubCatCh,N2Contor=InfoMaps[[2]])
  
  
 # Plot_NUTS2(InfoMaps[[1]],220)

  ###################################
# 11. Mapeo distintos usos del agua ########
  
  # 11.1 Seleccion de la Cuenca (BUSCO UNA CUENCA PEQUENA COMO EJEMPLO)
  BasinsList<-unique(C_SC_Rel$basin_name)
   # "Ter"   "Ebro"  "Loire" "Danube"   "Rhine"  "Duero"  "Jõcar"  "Po"  "Guadalquivir"  "Adige"
  namecatch <-"Po"
  
  # 11.2 Mapa de las subcuencas de la cuenca
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch)

  # 11.3 Mapa de Fresh Water por subcuencas de la cuenca
  nombreV<-"Fresh Water"
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,r_fresW,nombreV)

  # 11.4 Mapas de Algunos Usos del agua
  nombreV<-"Domestic Water"
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,r_domestWD,nombreV)
  nombreV<-"Industrial Water"
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,r_industWD,nombreV)
  nombreV<-"Energy Water"
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,r_energWD,nombreV)
  nombreV<-"Livestook Water"
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,r_livesWD,nombreV)
  nombreV<-"Irrigation Water"
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,r_irrWD,nombreV)

  
  ###################################################################################
  ###################################################################################
  ####################################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  ###################################################################################
  ###################################################################################
  ####################################################################################
  
  
  
  
  
  
  #############################
## 12. Calculo de WEI ########
  # "Ter"   "Ebro"  "Loire" "Danube"   "Rhine"  "Duero"  "Jõcar"  "Po"  "Guadalquivir"  "Adige"
  namecatch <-"Ebro" 
  # Informacion contenida en List_WQnI
  # 4:r_fresW; 5:r_domestWD; 6:r_industWD; 7:r_energWD; 8:r_livesWD; 9:r_irrWD; 10: Area
  df_Catch_Inf<-Calcula_WEI_Catch(namecatch,C_SC_Rel,List_WQnI) 
  #devuelve una lista con dos DF una de usos y otra con los wEI: uses, WEI

  # 12.1 Mapa de las usos del agua (igual a lo que hacia en 11.4) ######
  #para una de las variables
  nombreV<-"fresW"
  valvar<-df_Catch_Inf$uses$fresW
  names(valvar)<-rownames(df_Catch_Inf$uses)  #el nombre de cada subcathment para el plot
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,valvar,nombreV,logFormat=TRUE) #habria que quitar el logaritmo
  
  # 12.2 un loop para TODOS LOS USOS  ####
  for (idmap in colnames(df_Catch_Inf$uses)){
    #  idmap<-"fresW"
    nombreV<-idmap
    valvar<-df_Catch_Inf$uses[,idmap]  
    names(valvar)<-rownames(df_Catch_Inf$uses) #nombres subcatchments
    Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,valvar,nombreV,logFormat=TRUE) 
  }
  
  max(df_Catch_Inf$uses$fresW)
  max(df_Catch_Inf$uses$domestWD)
  max(df_Catch_Inf$uses$industWD)
  max(df_Catch_Inf$uses$energWD )
  max(df_Catch_Inf$uses$livesWD)
  max(df_Catch_Inf$uses$irrWD)
  boxplot(df_Catch_Inf$uses$fresW,df_Catch_Inf$uses$domestWD,df_Catch_Inf$uses$energWD )
  boxplot(df_Catch_Inf$uses$energWD/df_Catch_Inf$uses$fresW )
  
  # 12.3 Loop para TODOS LOS WEI  ####
  for (idmap in colnames(df_Catch_Inf$WEI)){
  #  idmap<-"Total"
    nombreV<-idmap
    valvar<-df_Catch_Inf$WEI[,idmap]  
    names(valvar)<-rownames(df_Catch_Inf$WEI) #nombres subcatchments
    Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,valvar,nombreV,logFormat=FALSE) 
  }

max(df_Catch_Inf$WEI$domestWD)
max(df_Catch_Inf$WEI$industWD)
max(df_Catch_Inf$WEI$energWD )
max(df_Catch_Inf$WEI$livesWD)
max(df_Catch_Inf$WEI$irrWD)
max(df_Catch_Inf$WEI$Total)



  #en el loira hay un ERROR con EL AGUA DE IRRIGACION
#  idmap<-"Total"
#  sum(is.na( valvar ))
#  sum( valvar == 0) 
  
  #########################
  # 13. ANALISIS ITERATIVO DE REDUCCION DE LOS CONSUMOS  ###
  # "Ter"   "Ebro"  "Loire" "Danube"   "Rhine"  "Duero"  "Jõcar"  "Po"  "Guadalquivir"  "Adige" 
  namecatch <-"Loire" 
  
  # WEI vs Reduccion de la demanda

  # 13.1 Los valores para esa Cuenca (devuelve dos DF, usos y WEI)
  df_Catch_Inf <- Calcula_WEI_Catch(namecatch,C_SC_Rel,List_WQnI) 

  # 13.3 Seleccion el tipo de demanda
  demanType <- "Domestic"
  
  
  # 13.6 La reduccion Iterativa de ese tipo de demanda en todas las NUTS2
  df_iteration<- data.frame()
  for (rf in seq(0.01,0.995, 1/15)){
    print(rf)
    #En cada Iteracion cambio el ratio de reduccion, lo aplico y calculo wl WEI 
    wei_temp <- df_Catch_Inf$uses$domestWD *(1-rf) /df_Catch_Inf$uses$fresW
  #  mean(wei_temp)
  #  median(wei_temp)
    df_iteration<-rbind(df_iteration,data.frame(rf,mean(wei_temp),median(wei_temp),max(wei_temp) ))
  }
  
  # 14.7 Plot de las figuras
  yrange <- range(df_iteration$max.wei_temp,df_iteration$mean.wei_temp)
  plot(df_iteration$rf, df_iteration$mean.wei_temp,ylim=yrange, xlab=" % Reduction",ylab="WEI",
       pch=20,col="black",main="fresW"  )
  points(df_iteration$rf, df_iteration$median.wei_temp ,pch=20,col="blue")
  points(df_iteration$rf, df_iteration$max.wei_temp ,pch=20,col="red")
  
  legend("topright",c("mean","median","max"),pch=20,col =c("black","blue","red")
         ,box.col = "white") 
  
  
  
  #########################################################
# 15. Optimizacion Multi-Objetivo ######
  #########################################################
   # 1:abatement rate; 2:Domestic; 3:Industrial; 4:Energy; 5:Livestook; 6:Irrigation ; 7: all (2 to 6)
  listuses<-c("","Domestic","Industrial","Energy","Livestook","Irrigation")
  indexOb <-c(1,2,3,4,5,6)   #El primero deberia ser siempre freshWater, todos los otros son los usos que pongamos
  #Los objetivos son: %av rate reduction  , WEI av.
  indexAgrM <- c(1,2) #agregation metric =>  1:mean; 2:median; 3:max (para los dos objetivos anteriores)
  signOb <- c(1,1)   #positivo si es minimizar y negativo si es maximizar 
  genParam <- c(2,80,1000)   #  nobject<-2 ; popSize=60 ; generations=40
  Configlist <- list(indexOb,indexAgrM,signOb,genParam)

  # 15.1 para una cuenca #####
  # "Ter"   "Ebro"  "Loire" "Danube"   "Rhine"  "Duero"  "Jõcar"  "Po"  "Guadalquivir"  "Adige"
  namecatch <-"Po"  #seleccinamos la cuenca
  df_Catch_Inf<-Calcula_WEI_Catch(namecatch,C_SC_Rel,List_WQnI) #valores para esa Cuenca
  #Nuts2 que atraviesa esa cuenca
    df_C_N2 <- NUTS2_in_Catch(namecatch,CatchmentID,C_SC_Rel,intersec_fracc,Mode=1)

  #Ejecuta OPTIMIZACION
  resultados <- MOO_GA(df_Catch_Inf$uses,df_C_N2,Configlist)

  
  # resultadosEqWeight <-  resultados 
  # resultadosDiffWeight <-  resultados   
  #########################################################
  # 16. VISUALIZACION SOLUCIONES ######
  #########################################################
    ## 16.1
  #Ejecuta Visualizacion de Resultados
  MCA_Visualization(namecatch, resultados,df_C_N2)


  ## CARGAMOS INFORMACION PARA OTRAS FIGURAS   #necesitamos saber que nuts queremos pintar
  rel2_N2_cth<-NUTS2_in_Catch(namecatch,CatchmentID,C_SC_Rel,intersec_fracc,Mode=2)
  rel2_N2_cth  #con ratios para cada N2
  ListnameNuts<-names(rel2_N2_cth)  # Realmente lo podriamos sacar del names(valvarNUTS)
  

  ## 16.2 DIBUJA EL PARETO CON UN NUMERO IDENTIFICACION
  Plot_Pareto(resultados)
  
  ## 16.3 GENERA UN FICHERO CSV  CON LAS SOLUCIONES
  PATH_results <- "D:/DISCO_F/2016JUNIO/GLOBAQUA/PAPER/Resultados/"  #directorio destino
  namesvar<-unique(df_C_N2$N2)  #los nombres de los nuts3
  usos<-listuses[c(2,3,4,5,6)]
  namefile <- c(namecatch,usos)
  namefile <- paste0(namefile, collapse = "_")
  Crea_csv_Pareto(PATH_results,resultados,namesvar, usos, namefile)

  
  ## 16.4 Seleccionamos uno de los puntos del pareto, y GENERA DIAGRAMA de BARRAS para cada uso
  IdPareto<- 14   #seleccionamos un punto del pareto
  Plot_Histo(resultados,namesvar,usos,IdPareto )
  #la version reducida, solo en algunos NUTS2
  namesvarRed<-factor(namesvar[c(1,2,3)])
  Plot_Histo(resultados,namesvar,usos,IdPareto,namesvarRed )
  
  #MAPA DEL BLS #########
  # Informacion contenida en List_WQnI
  # 4:r_fresW; 5:r_domestWD; 6:r_industWD; 7:r_energWD; 8:r_livesWD; 9:r_irrWD; 10: Area
  df_WEI_BLS <- Calcula_WEI_Catch(namecatch,C_SC_Rel,List_WQnI) #el WEI del BLS 
  head(df_WEI_BLS)
  
  ## WEI total de BLS  
  idmap<-"Total"
  nombreV<-idmap
  valvar1<-df_WEI_BLS$WEI[,idmap]  
  boxplot(valvar1)
  names(valvar1)<-rownames(df_WEI_BLS$WEI) #nombres subcatchments
  BR<-c(0.01, 0.015, 0.03, 0.05, 0.1, 0.5, 1, 2, 4, 6 ) 
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,valvar1,nombreV,logFormat=FALSE,BR) 
  namefile<-"BLS_PO.csv"
  namefile <- paste0(PATH_results, namefile )
  write.table(valvar1, file = namefile, sep = ",", col.names = NA,   qmethod = "double")
  
  
  ## 16.6 MAPA CON NUEVO VALOR DE LOS WEI para reduccion en los usos
  #Calcula un DF con ratios reduccion por USO y NUTS2
  RateReductionsPoint <- CalculaParetoSolReductions(resultados,namesvar,usos,IdPareto )

  #MAPA DE ESA SOLUCCION DE PARETO CON REDUCCION DE USOS
  #Nuts2 que atraviesa esa cuenca
  df_C_N2 <- NUTS2_in_Catch(namecatch,CatchmentID,C_SC_Rel,intersec_fracc,Mode=1)
  head(df_C_N2) #relacion subcatch y N2
  #calculamos para cada subcatchment su WEI (total, el de todos los usos) con la estrategia del punto 12
  df_WEI_BLS_Punto <- Calcula_WEI_Catch_Reduct(namecatch,C_SC_Rel,List_WQnI,RateReductionsPoint,df_C_N2)
  head(df_WEI_BLS_Punto)
  sum(df_WEI_BLS_Punto$total)
  
  ## pinto el WEI de una solucion Pareto 
  nombreV<-"total"
  valvar<-df_WEI_BLS_Punto$total  
  names(valvar)<-df_WEI_BLS_Punto$SC 
  #y la ordeno, chapuza, tengo
  valvar2<-valvar[names(valvar1)]
  boxplot(valvar2)
  BR<-c(0.01, 0.015, 0.03, 0.05, 0.1, 0.5, 1, 2, 4, 6 ) 
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,valvar2,nombreV,logFormat=FALSE,BR)
  
  namefile<-"Diff_Weigth_PointB.csv"
  namefile <- paste0(PATH_results, namefile )
  write.table(valvar2, file = namefile, sep = ",", col.names = NA,   qmethod = "double")
  
  
  
 dim( df_Catch_Inf$uses)
  head(df_Catch_Inf$uses)
  
  namefile<-"usos.csv"
  namefile <- paste0(PATH_results, namefile )
  write.table(df_Catch_Inf$uses, file = namefile, sep = ",", col.names = NA,   qmethod = "double")
  
  

 
sum(valvar1)  
sum(valvar)

<-head(valvar[match(valvar1,valvar)])
head(valvar1)

  merge(valvar1, valvar, by.x = 0, by.y =0)

  
  head(df_WEI_BLS$WEI)
  dim(df_WEI_BLS$WEI)
  head(df_WEI_BLS_Loira_P1)
  dim(df_WEI_BLS_Loira_P1)
  
  kk<-merge(df_WEI_BLS$WEI,df_WEI_BLS_Loira_P1, by.x = 0, by.y ="SC")
  head(kk)
  
head(valvar1)
head(valvar)
class(valvar)

valvar["32002050"]
valvar1["32002050"]
valvar1==valvar

match(valvar,valvar1)

dfkk<-data.frame(valvar1,valvar)
head(dfkk)


  ## 16.5 Seleccionamos uno de los puntos del pareto y hacemos mapas con ratios de aplicacion
  #selecciona un punto del pareto
  IdPareto<- 12      #seleccionamos un punto del pareto
  valvarNUTS <- DF_parallel[IdPareto,1:dim(resultados$parameters)[2]]  #dejo solo los ratios en cada NUTS2
  tipoconsumo<-"Domestic"
  nombrevar<-paste(tipoconsumo," Optimization.","Point number:",IdPareto,sep=" ")
  #Mapa con la intensidad de reduccion de consumo en cada NUTS2
  Plot_NUTS2_var(InfoMaps[[1]],ListnameNuts,valvarNUTS,InfoMaps[[2]],nombrevar)  #le mandamos tambien los contornos InfoMaps[[2]]
    
  

  
  
  #  Loop para TODOS LOS WEI  ####
  for (idmap in colnames(df_Catch_Inf$WEI)){
    #  idmap<-"Total"
    nombreV<-idmap
    valvar<-df_Catch_Inf$WEI[,idmap]  
    names(valvar)<-rownames(df_Catch_Inf$WEI) #nombres subcatchments
    Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,valvar,nombreV,logFormat=FALSE) 
  }
  

  
  
  
  
  
  valvarNUTS <- DF_parallel[,15:21]  #dejo solo los ratios en cada NUTS2
  colnames(valvarNUTS)<-c(as.character(namesvar))
  valvarNUTS<-valvarNUTS[IdPareto,]
  tipoconsumo<-"Domestic"
  tipoconsumo<-"Industrial"
  tipoconsumo<-"Irrigation"
  nombrevar<-paste(tipoconsumo," Optimization.","Point number:",IdPareto,sep=" ")
  #Mapa con la intensidad de reduccion de consumo en cada NUTS2
  Plot_NUTS2_var(InfoMaps[[1]],ListnameNuts,valvarNUTS,InfoMaps[[2]],nombrevar)  #le mandamos tambien los contornos InfoMaps[[2]]
  
  


  
  
  
  
  
  nombreV<-"fresW"
  valvar<-df_Catch_Inf$uses$fresW
  names(valvar)<-rownames(df_Catch_Inf$uses)  #el nombre de cada subcathment para el plot
  Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,valvar,nombreV,logFormat=TRUE)
  
  
  
  
  
  
  
  
  
  
  
  
  #modficacion para que dibuje varios mapas al mismo tiempo
  #incluso substraccion de Fresh wate menos alguna
  
  ###############################################
  # 12. Fresh Water menos algun uso del agua en una CUENCA ########
  
  namecatch <-"Ter"
  
  
  listofSubcatch_Segura_ID<-C_SC_Rel[C_SC_Rel$basin_name == namecatch ,]$cathment_hydroid   #ATENCION DEBERIA CONVERTIR A STRING
  
  tempFW<-r_fresW[names(r_fresW) %in% listofSubcatch_Segura_ID]
  tempDW<-r_domestWD[names(r_domestWD) %in% listofSubcatch_Segura_ID]
  tempIdW<-r_industWD[names(r_industWD) %in% listofSubcatch_Segura_ID]
  tempLW<-r_livesWD[names(r_livesWD) %in% listofSubcatch_Segura_ID]
  tempIW<-r_irrWD[names(r_irrWD) %in% listofSubcatch_Segura_ID]
  
  #Water Exploitation Index
  WEI_ter_DW<-tempDW/tempFW
  WEI_ter_IdW<-tempIdW/tempFW
  WEI_ter_LW<-tempLW/tempFW
  WEI_ter_IW<-tempIW/tempFW
  #Water Exploitation Index histograms
  par(mfrow=c(2,2),mar=c(2,2,1,0)) 
  hist(WEI_ter_DW)
  hist(WEI_ter_IdW)
  hist(WEI_ter_LW)
  hist(WEI_ter_IW)
  
  funcion_visualizar{
    
    boxplot(WEI_ter)
    hist(WEI_ter)
    nombreV<-"WEI_WD"
    Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,WEI_ter,nombreV,logFormat=FALSE) #habria que quitar el logaritmo
    
    nombreV<-"Fresh - Domest - Livest Water"
    ValVar<-tempFW-tempDW-tempIdW-tempLW-tempIW
    
    par(mfrow=c(1,2)) 
    boxplot(tempFW,tempDW,tempIdW,tempLW,tempIW,ValVar)
    
    tempFW-tempDW-tempIdW-tempLW-tempIW
    tempFW[1]-tempDW[1]-tempIdW[1]-tempLW[1]-tempIW[1]
    #Puedo hacer un plot de estos:
    nombreV<-"Fresh Water"
    Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,tempFW,nombreV)
    
    par(mfrow=c(2,2)) 
    hist((tempFW))  
    hist(log10(tempFW))  
    boxplot((tempFW))
    boxplot(log10(tempFW))
    
    
    nombreV<-"Fresh - Domest - Livest Water"
    ValVar<-tempFW-tempDW-tempLW
    class(ValVar)
    Plot_Catch(InfoMaps[[3]],C_SC_Rel,namecatch,ValVar,nombreV)
    par(mfrow=c(2,2)) 
    hist((ValVar))  
    hist(log10(ValVar))  
    boxplot((ValVar))
    boxplot(log10(ValVar))
    
    hist(tempFW, col=rgb(0.1,0.1,0.1,0.5))
    hist(ValVar, col=rgb(0.8,0.8,0.8,0.5), add=T)
    box()
    x_min<- min(log10(tempFW),log10(ValVar)) 
    x_max<- max(log10(tempFW),log10(ValVar))
    hist(log10(tempFW), xlim=c(x_min,x_max),col=rgb(0.1,0.1,0.1,0.5))
    hist(log10(ValVar),xlim=c(x_min,x_max), col=rgb(0.8,0.8,0.8,0.5), add=T)
    box()
    
    
    
  }
  
  
  
  
  
  
  
# 13. Los NUTS2 de esa cuenca
  namecatch <-"Loire"
  ListSubCathcSegur<- as.character( C_SC_Rel[C_SC_Rel$basin_name == namecatch ,]$cathment_hydroid )
  # Lista de todos los Nuts2 que contienen esos Subcatch
  tempRows <- intersec_fracc[CatchmentID %in% ListSubCathcSegur, ,drop=FALSE]  #hago un subset con solo las filas que estan en esas cuencas
  which(colSums(tempRows)!=0)  # !=0 si quiero tambien los que estan en dos 
  ListnameNuts <-  names(which(colSums(tempRows)!=0)  ) #estas son las NUTS2 que contienen esos subcatchment
  #DIBUJO los N2 y los subcatchment 
  Plot_NUTS2(InfoMaps[[1]],ListnameNuts)
  Plot_SubCatch(InfoMaps[[3]],ListSubCathcSegur)
  Plot_NUTS2_SubCatch(InfoMaps[[1]],ListnameNuts,InfoMaps[[3]],ListSubCathcSegur)




















  #Sin embargo por NUTS2 tiene tambien informacion de celdas aguas abajo. 
  #Hagamos un ejemplo con un NUTS2 con pocos subcatchment
  for (inNUTS in 1:length(Nuts2ObjID)){
    print(paste(inNUTS,sep=" : "  ) )
    index<-which(intersec_fracc[,inNUTS]==1,arr.ind = T)   #accede por indice, no por nombre
    temp<-paste(length(index),sep=" ")
    print(paste(temp,sep=" : "  ) )  
  } #por ejemplo la provincia con Nuts2ObjID =206  tiene solo "4" subcatchment
  
  which(intersec_fracc[,206]==1)  #nombres y numero de fila de los subcatchment del NUTS2 de la columna 12
  
  #los dibujamos
  ListnameNuts2 <- as.character(Nuts2ObjID[206])
  nameCathc<-names(which(intersec_fracc[,206]==1)) #todos los catchment que intersecan EL NUTS de la posicion 206
  #los dibujo
  Plot_NUTS2(InfoMaps[[1]],ListnameNuts2)  #se lo puedo pasar como nombre o como id, pero CUIDADO, QUE NO APUNTAN A LO MISMO
  Plot_Catch(InfoMaps[[3]],nameCathc)
  Plot_NUTS_Catch(InfoMaps[[1]],ListnameNuts2,InfoMaps[[3]],nameCathc)


# A partir de ese NUTS2 y la informacion de r_Domestic, voy a buscar todos los catchmets de una cuenca
  #en la matriz de freswater, si miro la fila de alguno de esos subCatchments, aparecera todas las provincias con influencia
  nameCathc[1]  #el primer subcathmen de la provincia
  moreNUTs2 <- which(r_fresW[nameCathc[2],]!=0) #otras provinc


#por ejemplo para el cathment que esta en el indice 38
nameCathc<-as.character(CatchmentID[1])
which(intersec_fracc[1,]!=0)  #esta linea y la siguiente son equivalentes
which(intersec_fracc[nameCathc,]!=0)
ListnameNuts2<-names(which(intersec_fracc[1,]!=0))

which(r_fresW[1,]!=0)  #Por Catchmet tengo informacion en las mismas celdas (catchment, NUTS) que en intersec_fracc

#NUEVO INTENTO, BUSCO UN CATCHMENT QUE TENGA ROUTING EN MUCHAS PROVINCIAS ()
for (inCach in 1:length(CatchmentID)){
  print(paste(inCach,sep=" : "  ) )
  index<-which(r_fresW[inCach,]!=0,arr.ind = T)   #accede por indice, no por nombre
  temp<-paste(length(index),sep=" ")
  print(paste(temp,sep=" : "  ) )  
} 

# 7813

nameCathc<-as.character(CatchmentID[7813])
which(r_fresW[1,]!=0)  #esta linea y la siguiente son equivalentes
which(r_fresW[nameCathc,]!=0)  #las provincias que tienen relacion con ese catchmet
nameNUTS<-names(which(r_fresW[nameCathc,]!=0))
  
  





  

######### FILTRANDO POR CATCHMENT (FILAS)
#por ejemplo el Routed_Domestic para un determinado catchment
r_Domestic[4,]
which(r_Domestic[4,]!=0,arr.ind = T)  #posiciones con valor no nulo para el catchment de la 4 fila
#si lo repito para unos cuantos catch, ya encuentro alguno con varios nuts
for (inCach in 1:100){
  index<-which(r_Domestic[inCach,]!=0,arr.ind = T) 
  print(paste(inCach,index,sep=" "  ) )
}

######### FILTRANDO POR NUTS (COLUMNAS)
#por ejemplo el Routed_Domestic para un determinado catchment
r_Domestic[,4]  #demasiadas lineas para encontrar asi 
which(r_Domestic[,4]!=0,arr.ind = T)  #posiciones con valor no nulo para ese NUTS
#si lo repito para unos cuantos catch, ya encuentro alguno con varios nuts
for (inNUTS in 1:100){
  print(paste(inNUTS,sep=" : "  ) )
  index<-which(r_Domestic[,inNUTS]!=0,arr.ind = T) 
  temp<-paste(index,sep=" ")
  print(paste(temp,sep=" : "  ) )  
}











  
  #NUTS2 (PROVINCIAS)
  

    
  LisProv<- which(intersec_fracc[65,]!=0,arr.ind = T)  #Para la cuenca 65 en que provincias esta
  pol.Pais<-pol.paises[pol.paises$objectid %in% LisProv,]
  dim(pol.Pais)
  nombre<- "Provincias"
  plot(xlim, ylim, type='n', xlab='', ylab='', main=nombre)
  plot(pol.Pais, add=TRUE, col='white')
  
  
  #CUENCAS
  #una sola cuenca
  head(pol.cuencas$hydroid)
  pol.Cuenc<-pol.cuencas[pol.cuencas$hydroid == "2000001",  ]
  xlim<-pol.Cuenc@bbox[1,]
  ylim<-pol.Cuenc@bbox[2,]
  nombre<- paste("cuenca: ","2000001", sep="" )
  plot(xlim, ylim, type='n', xlab='', ylab='', main=nombre)
  plot(pol.Cuenc, add=TRUE, col='white')
  
  #varias cuencas seleccionadas con una condicion relativa a su ID
  head(pol.cuencas$hydroid)
  ListaCuencas<-which(pol.cuencas$hydroid < 2000005,arr.ind = T)  #devuelve la posicion
  pol.Cuenc<-pol.cuencas[ListaCuencas,  ]
  summary(pol.Cuenc)
  pol.Cuenc@bbox
  pol.Cuenc@bbox[1,1]
  xlim<-pol.Cuenc@bbox[1,]
  ylim<-pol.Cuenc@bbox[2,]
  #ya los imprimo
  plot(xlim, ylim, type='n', xlab='', ylab='', main=nombre)
  plot(pol.Cuenc, add=TRUE, col='white')
  
  #varias cuencas relaccionadas con una provincia determinada
  
  



#  pol.Cuenc <- pol.cuencas
  class(pol.cuencas)   
  names(pol.cuencas)


  dim(pol.Cuenc)  

  plot(xlim, ylim, type='n', xlab='', ylab='', main=nombre)
  plot(kk, add=TRUE, col='white')
  plot(Pais, add=TRUE, col='red')

  pol.Cuenc<-pol.cuencas[pol.cuencas$hydroid %in% ListaCuencas ]
 
#  dim(pol.Cuenc)  



  
}




pol.Pais <- pol.paises
pol.Pais<-pol.paises[pol.paises$objectid=="1",]

class(paises)
Pais<-paises[paises$objectid=="100",]
dim(paises)


# Defino el layout
m = t(matrix(c(1:2)))
nf <- layout(m, widths=c(100, 15)) # la altura, en proporciones
xlim<-c(2552500,5200000)
ylim<- c(1557500,4587500)
nombre<- "Provincias"
plot(xlim, ylim, type='n', xlab='', ylab='', main=nombre)
plot(pol.Pais, add=TRUE, col='white')
plot(Pais, add=TRUE, col='red')
#legend('topleft', legend=c(paste('<', Cortes_concen[1]), paste(Cortes_concen[1], '-', Cortes_concen[2]), paste('>', Cortes_concen[2]))
 #      , col=c('lightgray', 'yellow', 'red'), bty='n', pch=15, cex=1.6, title='NO3 concentration')
box()

par(mar=c(0, 0, 0, 0))
plot.new()
tempTable<-table(thepoint2$DiscretConc)
for (i in 1:3) {
  tTable<-paste( names(tempTable)[i],as.vector(tempTable)[i],sep=": ")
  mtext(tTable, side = 3, line = (-6-i), adj = -0.1, cex = 0.9)
}


###################
####### CUENCAS
#####################

pol.Cuenc <- pol.cuencas
pol.Cuenc2<-pol.Cuenc[pol.Cuenc$hydroid=="31293"] #no entiendo porque con este solo una dim

class(cuencas)
dim(cuencas)
Cuenca<-cuencas[cuencas$hydroid=="31293"]  #no entiendo porque esta tiene solo una dimension






