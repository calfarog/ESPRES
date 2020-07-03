################################################################################
# Author: Angel Udias     all complaints to: angel.udias-moinelo@ec.europa.eu  #
################################################################################
# Started: 21-Jan-2018 ->    ;                                                 #
# Updates:                                                                     #
#                                                                              #
# ESPRES: Efficient Strategies for anthropogenic Pressure Reduction in European waterSheds                                                                             #
#                  PLOTING functions file                                      #
#                                                                              #
# DESCRIPTION :                                                                #
# Funciones para dibujar los resultados          #
# 
#
#                                       #
#                                         #
#                                         #
#                      #
################################################################################


#########################################################################
#########################################################################
######################  CONCENTRATION  #####################################
####################################################################
######################################################################


# TheStg <- Thedifference    #recibe la concentracion agregada por nuts para diferentes metricas
# thePonder <- "F"
# COMPARA la concentracion de una solucion para diversas metricas de agrupacion
BarPlot_Stg_Concentration <- function(TheStg,thePonder){     
  
  Stg_ConcbyNUT_AllMetric <- TheStg
  
  ##Barplot para esa estrategia determinada (con varias metricas de agrupamiento)
  par(mfrow=c(4,1),mar=c(4.1,4.1,2.1,2.1) )
  
  strgAgrrMetric <- c("mean","median","3Q","Max")  #un indice para cada estrategia de calculo
  for (index in 1:4){
    barplot(t(Stg_ConcbyNUT_AllMetric[,(2+index)]) , names.arg = Stg_ConcbyNUT_AllMetric$Nuts2,las=3,  
            ylab=paste0(strgAgrrMetric[index], " Concentration by Nuts"),
            main=paste0(strgAgrrMetric[index],"  strategy" )  ,cex.main=0.9    )
  }
  
}


# NOTA : problemas con el violing en mi ordenador, tema ggplot ####3 
# pathplot <- BASE_PLOTS   
# TheScen <- Evaluated_BLScen
# strgnames <- BasinName
# Boxplot con las concentraciones relativas a varias area
Boxplot_Stg_Concentration <- function(pathplot,TheScen,strgnames){
  
  datforBox <- TheScen[[1]]
  datforBox <- datforBox[complete.cases(datforBox),  ]    #elimino los NA
  
  par(mfrow=c(2,2))
  boxplot(datforBox$newconc~datforBox$Nuts2 ,ylab="Concentration", las=2,
          outline=TRUE, ylim=c(0,max(datforBox$newconc)),main=" Boxplot strategy" )
  
  boxplot(datforBox$newconc~datforBox$Nuts2 ,ylab="Concentration", las=2,
          outline=FALSE, main="Without ouliers" )
  
  #angcho del box proporcionales a la longitud
  temp2<-aggregate(datforBox$LengthKm, by=list(Category=datforBox$Nuts2), FUN=sum)  #calcula area para cada nuts
  proportionLength=temp2$x/sum(temp2$x)
  boxplot(datforBox$newconc~datforBox$Nuts2 ,ylab="Concentration", las=2,
          outline=FALSE, main="Width Proportional to Length",width=proportionLength )
  
  #box proporcionales al area
  temp2<-aggregate(datforBox$Area, by=list(Category=datforBox$Nuts2), FUN=sum)  #calcula area para cada nuts
  proportionLength=temp2$x/sum(temp2$x)
  boxplot(datforBox$newconc~datforBox$Nuts2 ,ylab="Concentration", las=2,
          outline=FALSE, main="Width proportional to Area",width=proportionLength )
  
  #box proporcionales al drain Area
  temp2<-aggregate(datforBox$DrainedArea, by=list(Category=datforBox$Nuts2), FUN=sum)  #calcula area para cada nuts
  proportionLength=temp2$x/sum(temp2$x)
  boxplot(datforBox$newconc~datforBox$Nuts2 ,ylab="Concentration", las=2,
          outline=FALSE, main="Width proportional to Drain Area",width=proportionLength )
  
  #   library(ggplot2)
  # Basic violin plot
  #  p <- ggplot(datforBox, aes(x=Nuts2 , y=log(newconc) )) + 
  #    geom_violin()
  #  p
  # Rotate the violin plot
  # p + coord_flip()
  # Set trim argument to FALSE
  #    theviolin <- ggplot(datforBox, aes(x=Nuts2, y=log(newconc) )) + 
  #      geom_violin(trim=FALSE)
  
  
  #  ggplot(datforBox, aes(x = Nuts2, y = newconc, fill = Nuts2)) +       #escala logaritmica
  #    geom_violin() + scale_y_log10() +
  #    stat_summary(fun.y = "mean", geom = "point", shape = 8, size = 3, color = "midnightblue") +
  #    stat_summary(fun.y = "median", geom = "point", shape = 2, size = 3, color = "red")
  
  
  # log(400)
  
  
  #   return(theviolin)
  
  
}



# BLS <-  BLScen
# list_Stg <- LasEstrategias
# strgnames <- Stgnames
# typeAgr <- typeAgr
# BARPLOT COMPARANDO la CONCENTRACION de varias soluciones para la misma metricas de agrupacion
# compara para todas las metricas
# LORO, SI ANADO MAS METRICAS TENDRE QUE ANADIRLAS EN ESTA FUNCION
BarPlot_Stg_ConcentrComp <- function(BLS,list_Stg,strgnames,typeAgr){     
  
  numstg <-length(list_Stg)
  
  # EVALUA Y AGREGA LAS CONCENTRACIONES DE CADA ESTRATEGIA
  StgStatus_ConcbyNUT <-list()  #ire metiendo los df esta lista
  for(nc in 1: numstg){
    StgStatus <- EvalStg (BLS,list_Stg[[nc]])    #EVALUA LAS ESTRATEGIAS 
    weightBYlength<-"F"    #T: true, F: false
    StgStatus_ConcbyNUT[[nc]]<- NutsConcFromSubc(StgStatus[[1]] ,weightBYlength)   #AGREGACION DE ACUERDO A TODAS LAS METRICAS
  }
  # INDEX de la columna:
  # 3: media
  # 4: mediana
  # 5: 3Q
  # 6: max
  if ( typeAgr == "mean" ) indagr <- 3   #la media
  if ( typeAgr == "median" ) indagr <- 4   #la mediana                                                      
  if ( typeAgr == "3Q" ) indagr <- 5   #la media  #tercer quartil
  if ( typeAgr == "max" ) indagr <- 6   #la max
  
  DF_ConcComp <- StgStatus_ConcbyNUT[[1]] [,c(1,2,7,indagr)] #CARGO LA PRIMERA ESTRATEGIA de acuerdo a la metrica de agregacion
  for (index in 2:numstg){
    DF_ConcComp <- cbind(DF_ConcComp, StgStatus_ConcbyNUT[[index]][,c(indagr)] )   #el resto de acuerdo a la misma metrica
  }  #Ya tengo una matriz con todas las estrategias de acuerdo a la metrica seleccionada
  
  names(DF_ConcComp) <- paste0( c("Nuts2","n","TotArea", strgnames))
  
  rownames(DF_ConcComp)<-DF_ConcComp[,1]  #le pongo nombre a las filas 
  
  barplot((as.matrix(DF_ConcComp[,(4:(3+numstg))])) ,
          names.arg = colnames(DF_ConcComp[,(4:(3+numstg))]),las=3, beside=TRUE, 
          col=rainbow(nrow(DF_ConcComp)) , 
          ylab=paste0(typeAgr,"Concentration"),main=paste0(typeAgr,"  Agregation Metric" ),
          legend.text=TRUE, args.legend=list(x = "topright", cex = 0.7, bty = "n")  )
  
  barplot((as.matrix(t(DF_ConcComp[,4:(3+numstg)]))) ,
          names.arg = DF_ConcComp[,1],las=3, beside=TRUE, 
          col=rainbow(length(DF_ConcComp[,4:(3+numstg)])) ,
          ylab=paste0(typeAgr,"Concentration"),main=paste0(typeAgr,"  Agregation Metric" ),
          legend.text=TRUE, args.legend=list(x = "topright", cex = 0.7, bty = "n")  )
}




# BLS <- BLScen
# list_Stg <- LasEstrategias
# strgnames <- Stgnames
# BOXPLOT COMPARANDO la CONCENTRACION de varias soluciones 
Boxplot_Stg_ConcentrComp <- function(BLS,list_Stg,strgnames){     
  
  numstg <-length(list_Stg)
  
  # EVALUA LAS CONCENTRACIONES DE CADA ESTRATEGIA Y LA METO EN UNA MATRIZ
  StgStatus <- matrix(ncol=length(strgnames), nrow=nrow(BLS$DFConcentration))  #guardare los resultados en una matriz de esa dimension
  for(nc in 1: numstg){
    # nc<-1
    temp <- EvalStg (BLS,list_Stg[[nc]])    #EVALUA LAS ESTRATEGIAS 
    #  length( temp[[1]]$DFConcentration$newconc)
    #guardo solo la nueva concentracion para esa estrategia
    StgStatus[,nc] <-  temp[[1]]$DFConcentration$newconc 
  }
  
  #la convierto en data frame, le doy nombres a las columna y hago los boxplot
  colnames(StgStatus) <- strgnames
  StgStatus <- data.frame(StgStatus)
  #Le anado la info del nuts al que pertenecen (pego la columna)
  StgStatus <-  cbind( temp[[1]]$DFConcentration[,c(1,6)]  ,StgStatus)
  
  #hago el melt para ponerlo en forma fila
  ColStgStatus <- melt(StgStatus,id.vars =c("HydroID","Nuts2") )
  
  p1 <- ggplot(data = ColStgStatus, aes(x=Nuts2, y=value)) + 
    geom_boxplot(aes(fill=variable))
  p1 + facet_wrap( ~ Nuts2, scales="free")
  print(p1)
  
  p2 <- ggplot(data = ColStgStatus, aes(x=variable, y=value)) + 
    geom_boxplot(aes(fill=Nuts2))
  p2 + facet_wrap( ~ variable, scales="free")
  print(p2)
  
  p3 <- ggplot(data = ColStgStatus, aes(x = Nuts2 , y = value)) + 
    geom_boxplot(aes(fill =variable ), width = 0.8) + theme_bw() +
    coord_cartesian(ylim = c(0, 100))
  print(p3)
  
  #Hago un subseting, eliminando los valores de concentracion mayores de 200, para que la grafia salga mejor
  #ATENCION QUE ESTO HACE CAMBIAR LOS BIGOTES
  DF_totalSub <- ColStgStatus[(ColStgStatus$value<30 & ColStgStatus$value>-0.01) ,]
  
  p4 <- ggplot(data = DF_totalSub, aes(x=Nuts2, y=value)) + 
    geom_boxplot(aes(fill=variable))
  p4 + facet_wrap( ~ Nuts2, scales="free")
  print(p4)
  
  p5 <- ggplot(data = DF_totalSub, aes(x=variable, y=value)) + 
    geom_boxplot(aes(fill=Nuts2))
  p5 + facet_wrap( ~ variable, scales="free")
  print(p5)
  
  #### SON MUCHOS GRAFICOS, EN SU MOMENTO SE SELECCIONARA CUAL SE DEBE MOSTRAR Y CUAL NO
  
}




#########################################################################
#########################################################################
######################  EFFORT  #####################################
####################################################################
######################################################################


# BLS <- BLScen   
# StgtoPlot <- stg_n1_strateg
# rates <- "F"  #Para mostrar los rates en porcentajes o en valor de esfuerzo (contaminante reduccido)
# namemain <- name
# Barplot con la reduccion de esfuerzos aplicado por sectores y region
# LORO: ANADIR QUE HAGA JPEG, y que reciba un nombre
BarPlot_Stg_Effort <- function(BLS,StgtoPlot,namemain,rates="F"){     
  
  #Si quiere verse el esfuerzo como rates (no tengo que hacer ninguna evaluacion), dibuja directamente
  
  if(rates=="T"){
    par(mar=c(5.1,4.1,4.1,3.1)) 
    barplot((as.matrix(StgtoPlot)) ,
            names.arg = colnames(StgtoPlot),las=3, beside=TRUE, 
            col=rainbow(nrow(StgtoPlot)) , ylab="% Reduction",main=namemain,
            legend.text=TRUE, args.legend=list(x ='topright', cex = 0.95, bty = "n" ,inset=c(-0.2,-0.02))  )
  }
  

  
  if(rates=="F"){
    #Para dibujarlo en valores de contaminante, tenemos que evaluar la estrategia
    myStg_Evaluated <- EvalStg (BLS,StgtoPlot) 
    str(myStg_Evaluated[[2]]) #el segundo elemento de la lista de evaluacion son las reducciones todales de conentracion por cada tipo en cada subcuenca
    head( myStg_Evaluated[[2]][1:4,1:5])  
    
    #tengo que agregarlo por cada NUTS
    Mattemp <- NutsSectEffortFromSubc (myStg_Evaluated)  
    rownames(Mattemp) <- Mattemp$Nuts2
    Mattemp <- Mattemp[,3:6]   ## LORO ATENCION A MODIFICAR LA EVALUACION, POR EL ORDEN DEL TIPO DE REDUCCIONES
    colnames(Mattemp)<- rownames(StgtoPlot)   #############################   ###################
    #################################
    StgtoPlot2 <- t(Mattemp)
    
    par(mar=c(5.1,4.1,4.1,3.1))   
    
    barplot((as.matrix(StgtoPlot2)) ,
            names.arg = colnames(StgtoPlot2),las=3, beside=TRUE, 
            col=rainbow(nrow(StgtoPlot2)) , ylab="Reduction (T/Year)",main=paste0(namemain," Reduction of load"),
            legend.text=TRUE, args.legend=list(x = "topright", cex = 0.95, bty = "n" ,inset=c(-0.2,-0.02) )  )
  }
  
}




#######################
### SANKEY de REDUCTIONS de una estrategia
#################### 
# BLS <- BLScen         
# thestg <- stg_n1_strateg
Plot_SANKEY_Reductions <- function(BLS, thestg){

  ##########################
  # CANTIDADES REDUCIDAS DE LOS NITRATOS (EQUIVALENTE AL ESFUERZO)
  ##########################    
  # # tiene que ser en m3 o toneladas , no valen los porcenajes
  # MyEval_stg1 <- EvalStg (BLScen,stg_n1_strateg) 
  # #el segundo nos da ya metros cubicos de reduccion de demanda. 
  # # Falta agregarlos por nuts (ya tengo una funcion que los suma por nuts y tipo)
  # PonderEffor <- "D"        #  D: Default (todos igual importancia) . En caso contrario pasa el array de pesos de ponderacion
  # MyEffort_stg1 <- N2EffortFromSubc(MyEval_stg1,PonderEffor)  #Esto son las REDUCCIONES DE DEMANDAS
  # Stg_n1_PressRed_bySect <- colSums(MyEffort_stg1[,2:length(MyEffort_stg1)] )  # reduccion por sectores 
  # Stg_n1_PressRed_bySect

  MyEval_stg <- EvalStg (BLS,thestg)                     #evalua  (  para convertir los ratios en valores (sea m3 agua o toneladas de load) )
  MyEffort_stg <- NutsSectEffortFromSubc(MyEval_stg) #la suma de los esfuerzos es lo mismo que las reducciones
  rownames(MyEffort_stg) <- MyEffort_stg$Nuts2
  MyEffort_stg[,c(1,2,7)]<-NULL
  colnames(MyEffort_stg) <- c("Man","Min","PS","SD")
  
  reductions_by_N2_sect <- MyEffort_stg  
  
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
  
  # Colour links
  links$type <-  sub(' .*', '', nodes[links$target + 1, 'name'])

  sankeyNetwork(Links = links, Nodes = nodes,
                Source = "source", Target = "target",
                Value = "value", NodeID = "name",
                LinkGroup = 'type',
                fontSize= 12, nodeWidth = 30)

}



#######################
### SANKEY de USES and_REDUCTIONS una estrategia
###   LOS USOS .  ATENCION NO VEO LA FORMA DE CALCULAR LAS CONCENTRACIONES QUE GENERA CADA SECTOR
###   CANTIDADES REDUCIDAS DE LOS NITRATOS (EQUIVALENTE AL ESFUERZO)
####################
# BLS <- BLScen  
# thestg <- stg_n1_strateg
Plot_SANKEY_Uses_Red <- function(BLS, thestg){
  
  # Evalua la estrategia     
  Evaluated_stg <- EvalStg(BLS,thestg)  # [[1]] Las concentraciones (los usos), y [[2]] las reducciones: la diferencia de dichas demandas con relacion al BLS
  weightBYlength <- "F"    #F: false (sin ponderar); A: by area ; DA: by Drained Area
  ### LORO : tengo que retocar la siguiente funcion para que devuelva tambien la solucion con treshold
  newConcbyNUTW <- NutsConcFromSubc(Evaluated_stg[[1]] ,weightBYlength) 
  
  Usesby_N2_sect <- newConcbyNUTW   #los usos son equivalentes a las concentraciones (empleamos solo por N2) 
  # proceso un poco la primera y las sumo
  temp_Uses <- Usesby_N2_sect[,c(1,3,4,5,6,7)]
  rownames(temp_Uses) <- temp_Uses$N2
  temp_Uses$N2 <-NULL
  temp_Uses <- temp_Uses[ order(row.names(temp_Uses)), ]  #reordeno de acuerdo al nombre de los N2
  
  #los nodos son: los nuts y los usos y el nodo central de reducciones
  nombreN2 <- rownames(temp_Uses)
  nombreUsos <- colnames(temp_Uses)
  
  nodes = data.frame("name" =  c(nombreN2,nombreUsos,"reductions") )
  nodesindex <- nodes  
  nodesindex$ID <- seq.int(nrow(nodesindex))-1
  
  tempSankey <- temp_Uses
  tempSankey$N2<-rownames(tempSankey)
  links1 <- melt(tempSankey, id=c("N2"))
  #  nodesindex$ID[match(links$source,nodesindex$name)]
  links1$source <- nodesindex$ID[match(links1$N2,nodesindex$name)]
  links1$target <- nodesindex$ID[match(links1$variable,nodesindex$name)]
  
  ###  las REDUCCIONES de usos (ESFUERZO)
  stg1_Reductions <- stg_rates_to_values(BLS, thestg)
  colnames(stg1_Reductions) <- nombreUsos  #apano para los noMbres
  
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







#Sumario de Areas y Drained Areas para cada nuts
# TheScen <- Evaluated_BLScen
Summary_Scenario_Area <- function(TheScen){
  
    datforBox <- TheScen[[1]]

    par(mfrow=c(2,2))
    boxplot(datforBox$Area~datforBox$Nuts2 ,ylab="Area", las=2,
            outline=TRUE, ylim=c(0,max(datforBox$Area)),main=" Area" )
    
    boxplot(datforBox$Area~datforBox$Nuts2 ,ylab="Area", las=2,
            outline=FALSE, main="Area Without ouliers" )
    
    boxplot(datforBox$DrainedArea~datforBox$Nuts2 ,ylab="Drained Area", las=2,
            outline=TRUE, ylim=c(0,max(datforBox$DrainedArea)),main="Drained Area" )
    
    boxplot(datforBox$DrainedArea~datforBox$Nuts2 ,ylab="Drained Area", las=2,
            outline=FALSE, main="Drained Area" )
    
    boxplot(datforBox$LengthKm~datforBox$Nuts2 ,ylab="Length Km", las=2,
            outline=TRUE, ylim=c(0,max(datforBox$LengthKm)),main="Length Km")
    
    boxplot(datforBox$LengthKm~datforBox$Nuts2 ,ylab="Length Km", las=2,
            outline=FALSE, main="Length Km" )
    
    boxplot(datforBox$PopEq~datforBox$Nuts2 ,ylab="Population Equivalent",las=2,
            outline=TRUE, ylim=c(0,max(datforBox$PopEq)),main="Population Equivalent" )
    
    boxplot(datforBox$PopEq~datforBox$Nuts2 ,ylab="Population Equivalent",las=2,
            outline=FALSE, main="Population Equivalent" )
    
    count(datforBox, "Nuts2")   #number of subcatchment by NUTS2
    
    
    # SUMMARY OF Areas by Nuts  
    Area.summary <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                        Mean=mean(x$Area, na.rm=TRUE),
                                                        median= quantile(x$Area)[3] ,
                                                        Q=quantile(x$Area)[4] ,
                                                        Max=max(x$Area), Sum=sum(x$Area) )}
    
    AreaSumary <- ddply(datforBox, c("Nuts2"), function(x) Area.summary(x))
    #write.csv(AreaSumary, file = "AreaSumary.csv")
    
    #Barplot con las  Areas by Nuts
    par(mfrow=c(5,1))
    strgAgrrMetric <- c("count","mean","median","3Q","Max")  #un indice para cada estrategia de calculo
    for (index in 1:5){
      barplot(t(AreaSumary[,(1+index)]) , names.arg = AreaSumary$Nuts2,las=3,  
              ylab=paste0(strgAgrrMetric[index], "  Area by Nuts"),
              main=paste0(strgAgrrMetric[index],"  Area" )      )
    }
    
    # SUMMARY OF Drained Areas by Nuts
    DrainedArea.summary <- function(x, na.rm=TRUE){result <- c(n=as.integer(dim(x)[1]),
                                                               Mean=mean(x$DrainedArea, na.rm=TRUE),
                                                               median= quantile(x$DrainedArea)[3] ,
                                                               Q=quantile(x$DrainedArea)[4] ,
                                                               Max=max(x$DrainedArea), Sum=sum(x$DrainedArea) )}
    
    DrainedAreaSumary <- ddply(datforBox, c("Nuts2"), function(x) DrainedArea.summary(x))
    
    getwd()
    #write.csv(DrainedAreaSumary, file = "SumaryDrainedAreaSumary.csv")
    
    #BArplot con las Drained Areas by Nuts
    par(mfrow=c(5,1))
    strgAgrrMetric <- c("count","mean","median","3Q","Max")  #un indice para cada estrategia de calculo
    for (index in 1:5){
      barplot(t(DrainedAreaSumary[,(1+index)]) , names.arg = DrainedAreaSumary$Nuts2,las=3,  
              ylab=paste0(strgAgrrMetric[index], " Drain Area by Nuts"),
              main=paste0(strgAgrrMetric[index]," Drain Area" )      )
    }
  
}














########
## FUNTION TO RESCALE TO 0 - 1 a serie of values
########
range01 <- function(x){(x-min(x))/(max(x)-min(x))}






#############################################
#  pLOT_pARETO 
#############################################
# nameCat <- namecatch 
# Res <- myParetoMedia
# Catch_Nuts2 <- df_C_N2 #los Nuts2 y Subcatch que atraviesa esa cuenca
# path <- OUT_DIR
# scaled <- Para mostrar el pareto rescalado (by default) o en sus unidades originales
# tipo <-   indica el tipo de agregacion empleado para la concentracion (media, mediana, 3Q, sum, max)
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




######
#####  LORO: ANADIR UNA VERSION QUE RENORMALICE LAS ESCALAS A 0 Y 1
#Dibuja varios frentes de Pareto en la misma figura
# names  <- NamesPar    #los nombres de los paretos, si no tiene nombres los genera automaticamente
# TheParList <- LosParetos
Plot_ComparaParetos <- function(path,TheParList,names=NULL,outjpeg="F",tipo){
  
  #sI NO HAY NOMBRE
  if( is.null(names) ) { names <- paste0("pareto ",seq(1,length(TheParList))) } 
  
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
         { ylablabel ="Error"
         }
  )
  
  
  #busco los minimos y maximos
  listToReduce <- list()  #ire metiendo en esta lista
  for(nc in 1:length(TheParList)){
    # print( myCRITERlist[[nc]][myCRITERlist[[nc]]$adm0_name == country,3:4] )
    listToReduce[[nc]] <- TheParList[[nc]]$objectives[,1]  #el esfuerzo
  }
  Effortmin <- min( unlist(listToReduce))
  Effortmax <- max( unlist(listToReduce))
  listToReduce <- list()  #ire metiendo en esta lista
  for(nc in 1:length(TheParList)){
    # print( myCRITERlist[[nc]][myCRITERlist[[nc]]$adm0_name == country,3:4] )
    listToReduce[[nc]] <- TheParList[[nc]]$objectives[,2]  #la concentracion
  }
  Concmin <- min( unlist(listToReduce))
  concmax <- max( unlist(listToReduce))
  
  #dibuja ya los paretos
  if(outjpeg=="T") jpeg(file = paste(path, "comparaPareto.jpeg",sep="/") ,width=8, height=8, units="in",res = 200 )
  par(cex=0.4)
  par(mfrow=c(1,1),mar=c(4.1,4.1,1.1,2.1),cex.axis=1.1)
  #el primero es el plot el resto son en un loop  
  plot(TheParList[[1]]$objectives[,1] ,TheParList[[1]]$objectives[,2] , xlab="Effort Pressures Reduction ", 
       ylab= ylablabel, 
       xlim=c(Effortmin,Effortmax),ylim=c(Concmin,concmax ), pch=19,col=4)
  for (i in 2:length(TheParList) ){
    points(TheParList[[i]]$objectives[,1] ,TheParList[[i]]$objectives[,2] ,
           xlim=c(Effortmin,Effortmax),ylim=c(Concmin,concmax ), pch=19,col=(3+i))
  }  
  legend("topright",  legend=names, bty = "n" , pch=16, col=4:(4+length(TheParList)) )
  
  if(outjpeg=="T") dev.off()  # fin de la primera figura
  
  
}



#Dibujamos los de paretos optenidos con cada tipo de agregacion para comparar
#esta rutina es muy concreta, solo la usare para mi
#  DF_PMedia <- DF_ParetMedia
#  DF_PMedian <- DF_ParetMedian
#  DF_P3Q <- DF_Paret3Q
#  DF_PMax <-  DF_ParetMax
Plot_FullcompParetos <- function(DF_PMedia,DF_PMedian,DF_P3Q,DF_PMax){
  par(cex=0.4)
  par(mfrow=c(2,2),mar=c(4.1,4.1,1.1,2.1),cex.axis=1.1)
  xmin <- min( DF_PMedia$Effort,DF_PMedian$Effort,DF_P3Q$Effort,DF_PMax$Effort )
  xmax <- max( DF_PMedia$Effort,DF_PMedian$Effort,DF_P3Q$Effort,DF_PMax$Effort )
  for (i in 2:5){
    ymin <- min( DF_PMedia[,i],DF_PMedian[,i],DF_P3Q[,i],DF_PMax[,i] )
    ymax <- max( DF_PMedia[,i],DF_PMedian[,i],DF_P3Q[,i],DF_PMax[,i] )  
    plot(DF_PMedia$Effort,DF_PMedia[,i], xlab="Effort Pressres Reduction ", 
         ylab= paste0( colnames(DF_PMedia)[i]  ,"  concentration"), xlim=c(xmin,xmax),ylim=c(ymin,ymax ), pch=19,col="black")
    points(DF_PMedian$Effort,DF_PMedian[,i],col="red" )
    points(DF_P3Q$Effort,DF_P3Q[,i],col="blue" )
    points(DF_PMax$Effort,DF_PMax[,i],col="green" )
  }
  #faltaria poner la leyend diciendo como se genero cada color black: media, red: median, blue: Q75, Green:Max
  
}



























# pathplot <- OUT_DIR
# nameCuenca <- catchName
# TheBLSConc <- ListScen$DFConcentration
## Dibuja un perfil del valor de las concentraciones
Single_Prof_Conc_Plots_OLD <- function(pathplot,nameCuenca,TheBLSConc){
  
  nf <- layout(matrix(c(1,2), 1, 2, byrow = TRUE),widths=c(5,1))
  layout.show(nf)
  
  plot(TheBLSConc[,3],xaxt = "n",ylab="Concentration",
       xlab="HydroID",las=1,main=paste0(nameCuenca," BLS Nitrates concentration"),pch=20,col=1,cex=0.2)
  axis(1, at=1:length(TheBLSConc$HydroID), labels=TheBLSConc$HydroID,las=2,cex.axis=0.2)
  
  
  boxplot(TheBLSConc[,3],ylab="Concentration",las=2)
  # se puede aladir una linea con el valor medio o con el valor mediano ( o un boxplot a la derecha)
  
}




# pathplot <- OUT_DIR
# nameCuenca <- catchName
# TheBLSConc <- ListScen$DFConcentration
# TheSecondSConc <- newConcentr
#dibuja un perfil del valor de las concentraciones
Two_Prof_Conc_Plots <- function(pathplot,nameCuenca,TheBLSConc,TheSecondSConc ){
  
  
  
  d<-data.frame(x=rnorm(1500),f1=rep(seq(1:20),75),f2=rep(letters[1:3],500))
  # first factor has 20+ levels
  d$f1<-factor(d$f1)
  # second factor a,b,c
  d$f2<-factor(d$f2)
  
  boxplot(x~f2*f1,data=d,col=c("red","blue","green"),frame.plot=TRUE,axes=FALSE)
  
  # y axis is numeric and works fine
  yts=pretty(d$x,n=5)
  axis(2,yts)
  
  # I know this doesn't work; what I'd like is to spread the factors out 
  # so the each group of three(a,b,c) is labeled correctly
  axis(1,at=seq(1:20))
  
  # Use the legend to handle the f2 factor labels
  legend(1, max(d$x), c("a", "b","c"),fill = c("red", "blue","green"))
  
  
  ggplot(data = d, aes(x = f1, y = x)) + 
    geom_boxplot(aes(fill = f2), width = 0.8) + theme_bw()
  
  
  
  
  
  
  
  
  nf <- layout(matrix(c(1,2), 1, 2, byrow = TRUE),widths=c(5,1))
  layout.show(nf)
  
  plot(TheBLSConc[,3],xaxt = "n",ylab="Concentration",
       xlab="HydroID",las=1,main=paste0(nameCuenca," BLS Nitrates concentration"),pch=20,col=1)
  points(TheSecondSConc[,3],col="red",cex=0.9,pch =4)
  legend("topright", c("BLS", "New"), col = c(1, "red"),
         pch = c(20, 4),  bty = "n") 
  axis(1, at=1:30, labels=TheBLSConc$HydroID,las=2,cex.axis=0.7)
  
  
  boxplot(TheBLSConc[,3],TheSecondSConc[,3],ylab="Concentration",las=2,
          names = c("BLS","New") )
  # se puede aladir una linea con el valor medio o con el valor mediano ( o un boxplot a la derecha)
  
}






