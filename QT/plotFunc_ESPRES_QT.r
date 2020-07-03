

#Sumario de Areas y  para cada nuts
## Existe la misma funcion para el modulo de calidad, y el otro es mas completo
# Intentar unificarlos en uno solo (debende de los nombres de los NUTS)
# TheScen <- Evaluated_BLScen
Summary_Scenario_Area_QT <- function(TheScen){
  
  datforBox <- TheScen[[1]]
  colnames(datforBox)[which(colnames(datforBox)=="N2")] <-"Nuts2"      #para no hacer muchos cambios renombro una columna
  
  par(mfrow=c(1,2))
  boxplot(datforBox$Area~datforBox$Nuts2 ,ylab="Area", las=2,
          outline=TRUE, ylim=c(0,max(datforBox$Area)),main=" Area" )
  
  boxplot(datforBox$Area~datforBox$Nuts2 ,ylab="Area", las=2,
          outline=FALSE, main="Area Without ouliers" )
  

  
 # count(datforBox, "Nuts2")   #number of subcatchment by NUTS2
  
  
  # SUMMARY OF Areas by Nuts  
  Area.summary <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                      Mean=mean(x$Area, na.rm=TRUE),
                                                      median= quantile(x$Area)[3] ,
                                                      Q=quantile(x$Area)[4] ,
                                                      Max=max(x$Area), Sum=sum(x$Area) )}
  
  AreaSumary <- ddply(datforBox, c("Nuts2"), function(x) Area.summary(x))
  getwd()
  #write.csv(AreaSumary, file = "AreaSumary.csv")
  
  #Barplot con las  Areas by Nuts
  par(mfrow=c(5,1))
  strgAgrrMetric <- c("count","mean","median","3Q","Max")  #un indice para cada estrategia de calculo
  for (index in 1:5){
    barplot(t(AreaSumary[,(1+index)]) , names.arg = AreaSumary$Nuts2,las=3,  
            ylab=paste0(strgAgrrMetric[index], "  Area by Nuts"),
            main=paste0(strgAgrrMetric[index],"  Area" )      )
  }
  

  
}






# TheStg <- Eval_Scen
# COMPARA la concentracion de una solucion para diversas metricas de agrupacion
BarPlot_Stg_Extracciones <- function(TheStg,thePonder,indexExtr){     
  
  # recibe y manda uno de las posibles indicadores de ponderacion  #F: false (sin ponderar); A: by area ; DA: by Drained Area
  Stg_ExtraccNUT_AllMetric <- NutsExtractionFromSubc(TheStg[[1]] ,thePonder)
  
  
  ## TODOS LOS BARPLOT. FreshW,  "domestWD"  "industWD"  "energWD"   "livesWD"   "irrWD" 
  strWater<-c("Fresh Water", "Domestic Water","Energy Water", "Industrial Water","Irrigation Water",  "Livestock Water")
#  for (indexExtr in 1:6){
    #Stg_ExtraccNUT_AllMetric[[2]]
    
      ##Barplot para esa estrategia determinada (con varias metricas de agrupamiento)
      par(mfrow=c(4,1),mar=c(4.1,4.1,2.1,2.1) )
      
      strgAgrrMetric <- c("mean","median","3Q","Max")  #un indice para cada estrategia de calculo
      for (index in 1:4){
        barplot(t(Stg_ExtraccNUT_AllMetric[[indexExtr]][,(2+index)]) , names.arg = Stg_ExtraccNUT_AllMetric[[indexExtr]]$N2,las=3,  
                ylab=paste0(strgAgrrMetric[index], " Water by Nuts"),
                main=paste0(strgAgrrMetric[index]," ",strWater[indexExtr] )  ,cex.main=0.9    )
      }
  # }
}         



# pathplot <- BASE_PLOTS   
# TheScen <- Evaluated_BLScen
# strgnames <- namebox
# Boxplot con las concentraciones relativas a varias area
Boxplot_Stg_Extracciones <- function(pathplot,TheScen,strgnames){
  
  datforBox <- TheScen[[1]]   #divido todos los valores de agua por 1 millon
  datforBox[,2:7]<-datforBox[,2:7]/1000000
  
  par(mfrow=c(2,2))
  boxplot(datforBox$fresW~datforBox$N2 ,ylab= expression ( Fresh ~ Water ~ 10^{6} ), las=2,
          outline=TRUE, ylim=c(0,max(datforBox$fresW)),main="Fresh Water" )
  
  boxplot(datforBox$fresW~datforBox$N2 ,ylab=expression ( Fresh ~ Water ~ 10^{6} ), las=2,
          outline=FALSE, main="F.W. Without ouliers" )
  
  #ancho del box proporcionales al area
  temp2<-aggregate(datforBox$Area, by=list(Category=datforBox$N2), FUN=sum)  #calcula area para cada nuts
  proportionArea=temp2$x/sum(temp2$x)
  boxplot(datforBox$fresW~datforBox$N2 ,ylab= expression ( Fresh ~ Water ~ 10^{6} ), las=2,
          outline=FALSE, main="F.W.  Proportional to Area",width=proportionArea )
  

  #   library(ggplot2)
  # Basic violin plot
  #  p <- ggplot(datforBox, aes(x=N2 , y=log(newconc) )) + 
  #    geom_violin()
  #  p
  # Rotate the violin plot
  # p + coord_flip()
  # Set trim argument to FALSE
  theviolin <- ggplot(datforBox, aes(x=N2, y=log(fresW) )) + 
    geom_violin(trim=FALSE)
  
  
  boxplot(datforBox$domestWD~datforBox$N2 ,ylab= expression ( Domestic ~ Water ~ 10^{6} ), las=2,
          outline=TRUE, ylim=c(0,max(datforBox$domestWD)),main="Domestic Water" )
  
  boxplot(datforBox$industWD~datforBox$N2 ,ylab= expression ( Industrial ~ Water ~ 10^{6} ), las=2,
          outline=TRUE, ylim=c(0,max(datforBox$industWD)),main="Industrial Water" )
  
  boxplot(datforBox$energWD~datforBox$N2 ,ylab= expression ( Energy ~ Water ~ 10^{6} ), las=2,
          outline=TRUE, ylim=c(0,max(datforBox$energWD)),main="Energy Water" )
  
  boxplot(datforBox$livesWD~datforBox$N2 ,ylab= expression ( Livestock ~ Water ~ 10^{6} ), las=2,
          outline=TRUE, ylim=c(0,max(datforBox$livesWD)),main="livestock Water" )
  
  boxplot(datforBox$irrWD~datforBox$N2 ,ylab= expression ( Irrigation ~ Water ~ 10^{6} ), las=2,
          outline=TRUE, ylim=c(0,max(datforBox$irrWD)),main="Irrigation Water" )
  
  #  ggplot(datforBox, aes(x = N2, y = newconc, fill = N2)) +       #escala logaritmica
  #    geom_violin() + scale_y_log10() +
  #    stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 3, color = "midnightblue") +
  #    stat_summary(fun.y = "median", geom = "point", shape = 2, size = 3, color = "red")
  
  
  # log(400)
  return(theviolin)
  
}



    
# BLS <- BLScen
# StgtoPlot <- stg_n1_strateg
# rates <- "F"  #Para mostrar los rates en porcentajes o en valor de esfuerzo (contaminante reduccido)
# namemain <- name
# Barplot con la reduccion de esfuerzos aplicado por sectores y region
# LORO: ANADIR QUE HAGA JPEG, y que reciba un nombre
BarPlot_Stg_Effort_QT <- function(BLS,StgtoPlot,namemain,rates="F"){     
  
  #Si quiere verse el esfuerzo como rates (no tengo que hacer ninguna evaluacion), dibuja directamente
  
  if(rates=="T"){
    barplot((as.matrix(StgtoPlot)) ,
            names.arg = colnames(StgtoPlot),las=3, beside=TRUE, 
            col=rainbow(nrow(StgtoPlot)) , ylab="% Reduction",main=namemain,
            legend.text=TRUE, args.legend=list(x ='topright', cex = 1.05, bty = "n",inset=c(-0.05,-0.07))  )
  }
  
  if(rates=="F"){
    StgtoPlot2 <- stg_rates_to_values(BLS , StgtoPlot)
  #  StgtoPlot2 <- (MatrVal)
        # #Para dibujarlo en valores de cantidad de reduccion de agua extraida, tenemos que evaluar la estrategia
        # MyEval_Scen <- EvalStg (BLS,StgtoPlot)  
        # head(MyEval_Scen[[2]]) #  #devuelve [[1]]el nuevo escenario de demandas, y [[2]]la diferencia de dichas demandas con relacion al BLS
        # #el segundo nos da ya metros cubicos de reduccion de demanda. 
        # # Falta agregarlos por nuts (ya tengo una funcion que los suma por nuts y tipo)
        # PonderEffor <- "D"        #  D: Default (todos igual importancia) . En caso contrario pasa el array de pesos de ponderacion
        # MyEffort_N2_sect <- N2EffortFromSubc(MyEval_Scen,PonderEffor)
        #  
        # 
        # Mattemp <- MyEffort_N2_sect[,c(3,5,4,7,6)]   #  ATENCION ESTOY REORDENANDOLOS, PARA EVITAR PROBLEMAS
        # rownames(Mattemp) <- MyEffort_N2_sect$N2  #asigno nombres de N2
        # colnames(Mattemp) <-  rownames(StgtoPlot)    #asigno nombres a los usos (en version bonita).
        # #################################

    barplot((as.matrix(StgtoPlot2))/1000000 ,
            names.arg = colnames(StgtoPlot2),las=3, beside=TRUE, 
            col=rainbow(nrow(StgtoPlot2)) , ylab=expression(paste("Reduction 10"^"6"," m"^"3") ) ,
            main=paste0(namemain," Reduction of load"),
            legend.text=TRUE, args.legend=list(x = "topright", cex = 1.05, bty = "n" ,inset=c(-0.05,-0.07) )  )
  }
  
}




########
## FUNTION TO RESCALE TO 0 - 1 a serie of values.   Funcion comun para el modulo de CANTIDAD Y CALIDAD
########
range01 <- function(x){(x-min(x))/(max(x)-min(x))}





#############################################
#  pLOT_pARETO                                      Funcion comun para el modulo   CANTIDAD Y CALIDAD
#############################################
# path <- BASE_PLOTS
# Res <- myParetoMedia
# scaled <- "T"    Para mostrar el pareto rescalado (by default) o en sus unidades originales
# tipo <-  "mean"  indica el tipo de agregacion empleado para la concentracion (media, mediana, 3Q, sum, max)
Plot_Pareto <- function(path,Res,outjpeg="F",scaled="T",tipo){
  
  ### Dependiendo del tipo de agregacion para el calculo de las concentraciones: mean, max, 3Q
  switch(tipo, 
         "mean"={ ylablabel ="Mean Concentration"
         },
         "median"={  ylablabel ="Median Concentration"
         },
         "3Q"={  ylablabel ="3Q Concentration"
         },
         "max"={  ylablabel ="Max Concentration"
         },
         "treshold"={  ylablabel ="Treshold Concentration"
         },
         { ylablabel ="Error"
         }
  )
  
  # Res$objectives[,1] <- Res$objectives[,1]/1000000  #convert the reduction to Million cubic meter/year
  if(scaled=="T"){
    Res$objectives[,1] <-range01(Res$objectives[,1])   #the pressures Reducction Effort
    Res$objectives[,2] <-range01(Res$objectives[,2])   #the concentration 
  }
  
  
  #UN PRIMER PARETO CON NUMEROS EN CADA PUNTO
  if(outjpeg=="T") jpeg(file = paste(path, "pareto.jpeg",sep="/") ,width=8, height=8, units="in",res = 200 )
  par(cex=0.4)
  
  par(mfrow=c(1,1),mar=c(6.1,4.1,2.1,2.1),cex.axis=1.1)
  plot(Res$objectives, xlab=" Ponderate Pressures Reduction Effort",  ylab= ylablabel,
       main=paste0("Eficient Strategies acording to ", ylablabel),  pch=19,col="black")
  etiqueta<-row(Res$objectives)[,1] #Etiqueta con el indice de la solucion
  text(Res$objectives[,1], Res$objectives[,2], labels= etiqueta, cex= 1,pos=4) 
  
  if(outjpeg=="T") dev.off()  # fin de la primera figura
  
  #UN SEGUNDO PARETO SIN NUMEROS EN CADA PUNTO Y MAS GRANDE
  # par(mfrow=c(1,1),mar=c(5.1,5.2,1.1,1.1),cex.axis=1.2,cex.lab=1.5)
  #  plot(Res$objectives,cex=1.1, xlab=expression(' "Effort (" ') ,  ylab= "Water Body Stress Indicator",
  #      main="",  pch=19,col="black",las=1)
  
}





#######################
### SANKEY de REDUCTIONS de una estrategia
####################
# BLS <- BLScen  
# thestg <- stg_n1_strateg
Plot_SANKEY_Reductions <- function(BLS, thestg){
  
  
  ##########################
  # CANTIDADES REDUCIDAS DE LAS DEMANDAS
  ##########################    
  # # tiene que ser en m3 o toneladas , no valen los porcenajes
  # MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
  # #el segundo nos da ya metros cubicos de reduccion de demanda. 
  # # Falta agregarlos por nuts (ya tengo una funcion que los suma por nuts y tipo)
  # PonderEffor <- "D"        #  D: Default (todos igual importancia) . En caso contrario pasa el array de pesos de ponderacion
  # MyEffort_stg1 <- N2EffortFromSubc(MyEval_stg1,PonderEffor)  #Esto son las REDUCCIONES DE DEMANDAS
  # Stg_n1_PressRed_bySect <- colSums(MyEffort_stg1[,2:length(MyEffort_stg1)] )  # reduccion por sectores 
  # Stg_n1_PressRed_bySect
  
  
  # La siguiente funcion ya hace el calculo (ordenando adecuadamente el tipo de usos). Hace lo mismo que lineas anteriores pero bien
  reductions_by_N2_sect <- stg_rates_to_values(BLS, thestg)   #convierto los ratios en valores (sea m3 agua o toneladas de load) 
  
  #Sankey de Los N2 a las REDUCCION  en los usos
  
  #   tempSankey <- MyEffort_stg1   #renombro los nobres de las columnas y la filas para distinguir stg 1 y 2
  #    paste0( "stg1_",colnames(tempSankey) )
  
  #los nodos son: los nuts y los usos
  nombreN2 <- rownames(reductions_by_N2_sect)
  nombreUsos <- colnames(reductions_by_N2_sect)
  
  nodes = data.frame("name" =  c(nombreN2,nombreUsos) )
  nodesindex <- nodes  
  nodesindex$ID <- seq.int(nrow(nodesindex))-1
  
  #  Creo una matriz de todos los N2 a todos los Usos
  tempSankey <- reductions_by_N2_sect  
  
  tempSankey$N2<-rownames(tempSankey)
  links <- melt(tempSankey, id=c("N2"))
  
  #  nodesindex$ID[match(links$source,nodesindex$name)]
  links$source <- nodesindex$ID[match(links$N2,nodesindex$name)]
  links$target <- nodesindex$ID[match(links$variable,nodesindex$name)]
  
  sankeyNetwork(Links = links, Nodes = nodes,
                Source = "source", Target = "target",
                Value = "value", NodeID = "name",
                fontSize= 12, nodeWidth = 30)
  
}



#######################
### SANKEY de USES and_REDUCTIONS una estrategia
####################
# BLS <- BLScen  
# thestg <- Stg_n2_strateg
Plot_SANKEY_Uses_Red <- function(BLS, thestg){
  
  # LOS USOS: serian las demandas sin acumular en la BLS, sin aplicar reducciones.
  # No necesito evaluar el escenario, estan en  BLS[[7]]
  
  temp_Uses <- t(BLS[[7]])  #solo lo traspongo para que las columnas sean los usos

  
  #los nodos son: los nuts y los usos y el nodo central de reducciones
  nombreN2 <- rownames(temp_Uses)
  nombreUsos <- colnames(temp_Uses)
  
  nodes = data.frame("name" =  c(nombreN2,nombreUsos,"reductions") )
  nodesindex <- nodes  
  nodesindex$ID <- seq.int(nrow(nodesindex))-1
  
  tempSankey <- data.frame(temp_Uses)
  class(tempSankey)
  tempSankey$N2 <- rownames(tempSankey)
  links1 <- melt(tempSankey, id=c("N2"))
  #  nodesindex$ID[match(links$source,nodesindex$name)]
  links1$source <- nodesindex$ID[match(links1$N2,nodesindex$name)]
  links1$target <- nodesindex$ID[match(links1$variable,nodesindex$name)]
  
  ###  Las REDUCCIONES de usos se calculan evaluando el BLS con, la estrategia. La siguiente funcion lo hace y ya devuelve las cantidades
  stg1_Reductions <- t(stg_rates_to_values(BLS, thestg))
#  colnames(stg1_Reductions) <- nombreUsos  #apano para los nombres
  
  #proceso para los links que van de N2 a reductions (suma por filas)
  reduction_N2  <- rowSums(stg1_Reductions)
  links2 <- data.frame( sourcetemp =  names(reduction_N2)   ,
                        targettemp =  "reductions" ,value= reduction_N2 )
  
  #poniendolo en forma de indices
  links2$source <- nodesindex$ID[match(links2$sourcetemp,nodesindex$name)]
  links2$target <- nodesindex$ID[match(links2$targettemp,nodesindex$name)]
  
  # proceso para los links que van de  reductions a sectores (suma por columnas)
  reduction_Sect  <- colSums(stg1_Reductions)
  links3 <- data.frame(  sourcetemp =  "reductions",
                         targettemp =  names(reduction_Sect) ,value= reduction_Sect )
  
  #poniendolo en forma de indices
  links3$source <- nodesindex$ID[match(links3$sourcetemp,nodesindex$name)]
  links3$target <- nodesindex$ID[match(links3$targettemp,nodesindex$name)]
  
  links <- rbind ( links1[,c("source","target","value")]  , links2[,c("source","target","value")], links3[,c("source","target","value")] )
  
  #### la grafica
  sankeyNetwork(Links = links, Nodes = nodes,
                Source = "source", Target = "target",
                Value = "value", NodeID = "name",
                fontSize= 12, nodeWidth = 30)
  
}






















################################################
### LA FUNCION SUMMARY    YA NO SE USA
#Sumario de las presiones relativas a las extracciones de agua
#  directory <-  BASE_PLOTS     
#  Eval_Scen <- Evaluated_BLScen
#  name <- namebox
#  catchname <- BasinName
#  LstShapes <-  LstMyShapes       # LstMyShapes[1]
Summary_Scenario_Water_QT <- function(directory,Eval_Scen,name,catchname,LstShapes){
  # Boxplot de las concentraciones 
  Boxplot_Stg_Extracciones(directory,Eval_Scen,name) # PLOTS ( que evaluar stg antes de hacer el plot)
  
  # Barplot de las concentraciones
  # se podria ponderar de diferentes maneras, y el grafico se muestra de acuerdo a ello
  weightBYlength <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
  BarPlot_Stg_Extracciones(Eval_Scen,weightBYlength)    # Hace un barplot para cada tipo: Devuel una lista: fresW, domestic, Industria, Ener, Livest, Irri
  #La anterior ponderada por area o por drain area
  
  ### 18.3 Mapping the CONCENTRATION for one strategy (previamente he hecho el subseting, podria hacerse desde esta funcion)
  
  # 18.3.1 Primero necesito la concentracion agregada de la estrategia que quiero plotear
  weightBY <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
  ### LORO : Devuel una lista: [[1]] fresW, [[2]]domestic, [[3]]Industria, [[4]]Ener, [[5]]Livest, [[6]]Irri
  newExtraccNUT <- NutsExtractionFromSubc(Eval_Scen[[1]] ,weightBY)
  
  # para el fres y 
  strWater<-c("Fresh Water", "Domestic Water", "Industrial Water", "Energy Water", "Livestock Water","Irrigation Water")
  
  ###################################################################
  # PARA CADA TIPO DE PRESION SE PUEDE GENERAR UNA TABLA  ##############################
  indexofPresure <- 1
  newExtraccNUT[[indexofPresure]]  #
  newExtraccNUT[[indexofPresure]][,3:6] <- lapply(newExtraccNUT[[indexofPresure]][,3:6] , round, 1)  #reduzco numero de decimales para imprimir
  
  getwd()
  # write.csv(newExtraccNUT, file = "ConctAreaSumary.csv")
  
  
  # 18.3.2 Ahora lo mando a Map, y genera un monton de mapas, uno por cada uso y por cada metrica
  #  catchName <-"Adige"
  NumClassInterv <- 8    #  Numero de categorias a la hora de dibujarlo
  paleta <-  "YlGnBu"    #   "BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
  estilo <-  "jenks" #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
  #Un loop de mapas con cada tipo de extraccon: FW, Domestic, Industr
  for (PresIndes in 1:6){
    # un mapa con cada metrica de agregacion
    for (AgrIndex in 1:4){                 # AgrIndex <- 1     # 1:mean; 2: median; 3: 3Q; 4:Max; 5: Threshold    #tipo de agregacion de con la que se ha calculado las concentraciones 
      Map_RegWater_QT(catchname,LstShapes,PresIndes,newExtraccNUT[[PresIndes]],AgrIndex,NumClassInterv,paleta,estilo)
    }
  }
  #  LstShapes[[1]]
  
}






#############################################
# 12. MCA_Visualization.  Funcion para visualizar los resultados de la optimizacion
#############################################
# nameCat <- namecatch 
# Res <- resultados
# Catch_Nuts2 <- df_C_N2 #los Nuts2 y Subcatch que atraviesa esa cuenca
MCA_Visualization <- function(nameCat,Res,Catch_Nuts2){
  
  #Dependiendo del numero de variable, habra mas o menos figuras
  numvar<-length(unique(df_C_N2$N2))  #los NUTS2 que atraviesa esa cuenca
  namesvar<-unique(df_C_N2$N2)
  nrow<-floor(numvar/4)
  
  par(mfrow=c(nrow,3))
  
  col.rainbow <- rainbow(nrow(Res$parameters))
  #  col.topo <- topo.colors(length(Res$parameters)/2)
  #  col.terrain <- terrain.colors(length(Res$parameters)/2)
  #Enfrento de dos en dos sucecisvas las variables (no todas con todas)
  for (nr in seq(1, (numvar-1), 1) ) {
    for (nr2 in seq((nr+1), numvar, 1) ) {
      print(paste(nr,nr2,sep=," ")) 
      plot(Res$parameters[,c(nr,nr2)], xlab=paste("NUTS:",namesvar[nr],sep=" "), ylab=paste("NUTS:",namesvar[nr2],sep=" ")
           , main=paste(nameCat," parameter space",sep="") ,pch=19,col=col.rainbow )
      
    }}
  #LORO: hacerlo con un      scatterplotMatrix
  
  #El tipo de consumo
  # 2:Domestic; 3:Industrial; 4:Energy; 5:Livestook; 6:Irrigation ; 7: all (2 to 6)
  if ( Configlist[[1]][2] == 2 ) text_cons <- "Domestic"
  if ( Configlist[[1]][2] == 3 ) text_cons <- "Industrial"
  if ( Configlist[[1]][2] == 4 ) text_cons <- "Energy"
  if ( Configlist[[1]][2] == 5 ) text_cons <- "Livestook"
  if ( Configlist[[1]][2] == 6 ) text_cons <- "Irrigation"
  if ( Configlist[[1]][2] == 7 ) text_cons <- "All"
  
  
  if ( Configlist[[2]][2] == 1 ) ytexto <- paste("Avg. WEI",text_cons,sep=" ")
  if ( Configlist[[2]][2] == 2 ) ytexto <- paste("Median WEI",text_cons,sep=" ")
  if ( Configlist[[2]][2] == 3 ) ytexto <- paste("Max. WEI",text_cons,sep=" ")
  
  par(mfrow=c(1,1))
  plot(Res$objectives, xlab="Avg.Rate", ylab= ytexto, main="Objective space",
       pch=19,col=col.rainbow )
  etiqueta<-row(Res$objectives)[,1] #Etiqueta con el indice de la solucion
  text(Res$objectives[,1], Res$objectives[,2], labels= etiqueta, cex= 1,pos=4)   
  #pos=2 es a la izquierda del punto, pos puede ser 1,2,3,4
  
  
  #anado a cada punto, cual es el valor de sus parametros
  # namepoints<-paste(resultados$parameters)
  plot(Res$objectives, xlab="Avg.Rate", ylab= ytexto, main="Objective space",
       pch=19,col=col.rainbow )
  etiqueta<-paste(round(Res$parameters[,1], digits=1),round(Res$parameters[,2], digits=1),sep="  ") 
  text(Res$objectives[,1], Res$objectives[,2], labels= etiqueta, cex= 0.5,pos=3)   
  #pos=2 es a la izquierda del punto, pos puede ser 1,2,3,4
  
  
  #Para cuando hay muchas variables, interesante un combplot 
  par(mfrow=c(1,2))
  library(lattice)
  # parallel(~iris[1:4] | Species, iris) 
  
  
  #Creo un DF con la 
  DF_parallel<-cbind (Res$parameters,Res$objectives ) 
  colnames(DF_parallel)<-c(as.character(namesvar),"Rate","WEI")
  parallelplot(~DF_parallel )
  
}






#############################################
# 15. DIBUJA HISTOGRAMA CON LOS % REDUCCION EN CADA NUTS PARA CADA TIPO DE CONSUMO
#############################################
#  res <- resultados
#  varname <- namesvar  
#  varnameR <- namesvarRed   
#  idP<-IdPareto
#  uso <-  usos
Plot_Histo <- function(res,varname,uso,idP,varnameR=NULL ){
  #primero pone en forma de DF las reducciones para esa solucion
  datatoplot <- CalculaParetoSolReductions(res,varname,uso,idP )
  head(datatoplot)
  #si hay valor en variables reducidas, filtor por solo algunos NUTS2 (para que se vea mejor)
  if(!is.null(varnameR)) datatoplot<- datatoplot[,  colnames(datatoplot) %in% varnameR]
  
  #el barplot global
  par(mfrow=c(1,1),mar=c(5.3,4.2,1.1,1.1),cex.axis=1.4,cex.lab=1.5)
  # par(mar=c(6.1,4.1,2.1,2.1),cex.axis=1.0)
  barplot(datatoplot, col=rainbow(length(rownames(datatoplot))),
          ylim=c(0, 60),las=2,ylab = "Effort rate by NUTS2 and Use",
          # main=paste(" Effort rate for Pareto sol: ",idP,sep=""),
          width=2, beside=TRUE)
  legend("topright", inset=c(0.01,0), fill=rainbow(length(rownames(datatoplot))), 
         legend=rownames(datatoplot),cex=1.3)
  
}



#############################################
# 15. Crea_csv_Pareto
#############################################
Crea_csv_Pareto <- function(  path,res,varname, uses,filename ){
  
  DF_parallel<-cbind (res$parameters,res$objectives ) 
  #   dim(DF_parallel)
  #la dimension de este nos indica el numero de usos considerados
  colnames(DF_parallel) <-  c(rep(as.character(varname),length(uses)),"Rate","WEI")  
  DF_parallel<-round(DF_parallel, digits = 2)
  print(DF_parallel)
  write.table(DF_parallel, file = paste(path,filename,".csv",sep=""), sep = ",", col.names = NA,   qmethod = "double")
  
}





Iter_MCA_Visualization <- function(nameCat,Res,Catch_Nuts2){
  
  par(mfrow=c(1,1))
  
  plot(df_iteration$rf*100, df_iteration$mean.wei_temp, xlab=" % Reduction",ylab="WEI",
       pch=20,col="black"  )
  # points(df_iteration$rf, df_iteration$median.wei_temp ,pch=20,col="blue")
  #  points(df_iteration$rf, df_iteration$max.wei_temp ,pch=20,col="red")
  points(resultados$objectives,col="red",pch=20)
  legend("topright",c("iterative","optimal"),pch=20,col =c("black","red")
         ,box.col = "white") 
}



##############################################
### Summary_Scenario_WEI   YA NO LO USAMOS
############################################

#Sumario de las presiones relativas a las extracciones de agua
#  directory <-  BASE_PLOTS     
#  WEI_Scen <- BLS_WEI
#  name <- namebox
#  catchname <- BasinName
#  LstShapes <-  LstMyShapes       # LstMyShapes[1]
Summary_Scenario_WEI <- function(directory,WEI_Scen,name,catchname,LstShapes){
  
  # Boxplot de los WEI
  par(mfrow=c(1,2))
  boxplot(WEI_Scen$WEI~WEI_Scen$N2 ,ylab="WEI", las=2,
          outline=TRUE, ylim=c(0,max(WEI_Scen$WEI)),main=" WEI" )
  
  boxplot(WEI_Scen$WEI~WEI_Scen$N2 ,ylab="WEI", las=2,
          outline=FALSE, main="WEI Without ouliers" )
  
  
  
  # SUMMARY OF WEI by Nuts  
  WEI.summary <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                     Mean=mean(x$WEI, na.rm=TRUE),
                                                     median= quantile(x$WEI)[3] ,
                                                     Q=quantile(x$WEI)[4] ,
                                                     Max=max(x$WEI), Sum=sum(x$WEI) )}
  
  WEISumary <- ddply(WEI_Scen, c("N2"), function(x) WEI.summary(x))
  getwd()
  #write.csv(WEISumary, file = "WEISumary")
  
  #Barplot con los  WEI by Nuts
  par(mfrow=c(5,1),mar=c(3,4,2,1))
  strgAgrrMetric <- c("count","mean","median","3Q","Max")  #un indice para cada estrategia de calculo
  for (index in 1:5){
    barplot(t(WEISumary[,(1+index)]) , names.arg = WEISumary$N2,las=3,  
            ylab=paste0(strgAgrrMetric[index], "  WEI by Nuts"),
            main=paste0(strgAgrrMetric[index],"  WEI" )      )
  }
  
  # Los mapas con el Base map (me gusta menos que la otra alternativa), por el momento lo dejo comentado
  # NumClassInterv <- 8    #  Numero de categorias a la hora de dibujarlo
  # paleta <-  "YlGnBu"    #   "BuPu"  RdBu    PiYG    RYG    PuOr    YlGnBu  OrRd     Spectral
  # estilo <-  "jenks" #    "fixed"   "sd"  "equal"    "pretty"    "quantile"    "kmeans"    "hclust"    "bclust"    "jenks"
  # #Un loop de mapas con cada tipo de extraccon: FW, Domestic, Industr
  # PresIndes <- 7  #empleo el indice 7 para el WEI
  #   # un mapa con cada metrica de agregacion
  #   for (AgrIndex in 1:4){                 # AgrIndex <- 1     # 1:mean; 2: median; 3: 3Q; 4:Max; 5: Threshold    #tipo de agregacion de con la que se ha calculado las concentraciones 
  #     Map_RegWater_QT(catchname,LstShapes,PresIndes,WEISumary,AgrIndex,NumClassInterv,paleta,estilo)
  #   }
  
  #Directamente hace 4 mapas del WEI, cada una agregada por NUTS2, de acuerdo a una metrica (mean, media, Q75, max, sum)
  Map_WEI_QT_SPPLOT(catchname,LstShapes,WEISumary,NumClassInterv,paleta,estilo)
  
}





