

###############################################
## NUTS2 por los que pasa una CUENCA (BASIN)
###############################################
# Esta funcion tiene dos posibilidades:
# 1) Devolver un DF listando Subcatch a subcathc a que Nuts2 pertenecen
# 2) Devolver la lista de los Nuts2
# nameCatch <- BasinName
# CrSC <- C_SC_Rel
# L_WQnI <- List_WQnI
# ATENCION CONSIDERO !=0, Es decir que pueden estar en dos NUTS2
# Mode: 1) DF relacione SC-N2  2) Ratio AreaSubcatch in each N2.   Por defecto devuelve la lista
NUTS2_BASIN <- function(nameCatch,L_WQnI,CrSC,Mode=2){
  
  Ct_ID <- L_WQnI[[2]]
  Intersecc <- L_WQnI[[3]]
  
  ListSubCathc <- as.character( CrSC[CrSC$basin_name == nameCatch ,]$cathment_hydroid )
  
  if (Mode==1){
    #DF con el listado entre cada Subcatchment y el NUTS2 en el que esta
    df_SC_N2 <- data.frame(SC = character(), N2 = character() )
    for (sc in ListSubCathc){
      # sc<-42001441
      nameCathc<-as.character(sc)
      Listnametemp<-names(which(Intersecc[nameCathc,]!=0))
      if(length(Listnametemp)>1){   # Si un subcatch esta en varios N2, busca en el que hay mas area
        indiceMaxArea <- which.max( Intersecc[nameCathc, Listnametemp[seq(1:length(Listnametemp))] ] )
        #  print( Intersecc[nameCathc, Listnametemp[seq(1:length(Listnametemp))] ])
        df_SC_N2<- rbind(df_SC_N2, data.frame(SC=nameCathc , N2=Listnametemp[indiceMaxArea]) )
      }else{
        #si el subcatch esta solo en un NUTS, lo mete tambien
        df_SC_N2<- rbind(df_SC_N2, data.frame(SC=nameCathc , N2=Listnametemp) )
      } #fin del else
    }  #fin del  for
    
    result<-df_SC_N2
  } #fin del If de Mode==1
  if (Mode==2){
    # head(ListSubCathc)
    # Lista de todos los Nuts2 que contienen esos Subcatch
    tempMatrix <- Intersecc[Ct_ID %in% ListSubCathc, ,drop=FALSE]  #hago un subset con solo las filas que estan en esas cuencas
    # which(colSums(tempMatrix)!=0)  # !=0 si quiero tambien los que estan en dos
    #atencion lo anterior devuelve el nombre del N2, no el indice de la columna
    ListnameNuts2 <-  names(which(colSums(tempMatrix)!=0)  )
    if(length(ListnameNuts2) == 1){
      ratioSubCinN2<-round(sum(tempMatrix[,ListnameNuts2])/length(ListSubCathc), digits = 4) *100
    }else{
      ratioSubCinN2<-round(colSums(tempMatrix[,ListnameNuts2])/length(ListSubCathc), digits = 4) *100  
    }
    
    
    result<-ratioSubCinN2
  }  #fin del if Mode==2
  
  #le anadimos la columna
  
  #ATENCION. Futura mejora, cuando haya un SC en dos N2, seleccionare en el que tenga mas area
  return(result)
}


# nrow(df_SC_N2)
# unique(df_SC_N2$SC)
# tb <-table(df_SC_N2$SC,df_SC_N2$N2)
# rowsum ( (table(df_SC_N2$SC,df_SC_N2$N2)) )
# 
# 
# tbl <- structure(c(466L, 686L, 337L, 686L), 
#                  .Dim = 4L,
#                  .Dimnames = structure(list( c("f-f", "f-m", "m-f", "m-m")),
#                                        .Names = ""), class = "table")
# 
# attributes(tb)$class <- "matrix" 
# 
# tb2<- ( as.data.frame.matrix(tb)[,2:5] )
# class(tb2)
# rowSums(tb2)>1
# which ( rowSums(tb2)>1  )




# nameCat <- BasinName
# L_WQnI <- List_WQnI
# CrSC <-  C_SC_Rel
# N2Corr  <- NUTS_Corr
# 4:r_fresW; 5:r_domestWD; 6:r_industWD; 7:r_energWD; 8:r_livesWD; 9:r_irrWD; 10: Area
###############################################
##  Subseting de la info que se corresponde unicamente con la cuenca en estudio
###############################################
GenerateBLS <- function (nameCat,L_WQnI, CrSC,N2Corr) {
  
      # HAGO SUBSETING (las subcuencas y Nuts de ese BASIN) DE CADA MATRIZ DE DATOS
      #los hidro Id de las subcuencas del Basin seleccionado  
      listofSubcatch_ID <- CrSC[CrSC$basin_name == nameCat ,]$cathment_hydroid
      N2Corr <- N2Corr[tolower(N2Corr$BASIN) == tolower(nameCat),]

      # para saber las regiones de la cuenca seleccionadas uso la funcion
      # que devuelve la relacion subBasin con NUts (de acuerdo indices Emiliano), para disponer de dicha relacion
      the_SubCt_N2 <- NUTS2_BASIN(nameCat,L_WQnI,CrSC,Mode=1)
      listofNuts_ID <- unique(the_SubCt_N2$N2) 

    # La seleccion de filas y columnas es por nombre (Los nombres son numeros, tener cuidado con esto)
    tempFresW <- L_WQnI[[4]][ as.character(listofSubcatch_ID) ]   #el agua disponible en cada subcuenca (ya sumado lo que aporta cada NUTS)
    tempDomestWD <- L_WQnI[[5]][ as.character(listofSubcatch_ID) , as.character(listofNuts_ID) ]   #la demanda domestica
    tempEnergWD <- L_WQnI[[6]][ as.character(listofSubcatch_ID) , as.character(listofNuts_ID) ]   #la demanda Energ
    tempIndustWD <- L_WQnI[[7]][ as.character(listofSubcatch_ID) , as.character(listofNuts_ID) ]   #la demanda Industry
    tempIrrigWD <- L_WQnI[[8]][ as.character(listofSubcatch_ID) , as.character(listofNuts_ID) ]   #la demanda Irrigation
    tempLivestWD <- L_WQnI[[9]][ as.character(listofSubcatch_ID) , as.character(listofNuts_ID) ]   #la demanda Livestock
    temparea <- L_WQnI[[10]][ as.character(listofSubcatch_ID) ]      #el area de cada subcuenca
    # Cargo todos los de dos dimensiones en una lista
    RoutingWList <- list(tempDomestWD,tempEnergWD,tempIndustWD,tempIrrigWD,tempLivestWD)  #no mete aun el fresW, ni el area
  
      # Prepara correspondencia entre ID antiguos y nuevos para las provincias
      #llama a la funcion que devuelve la relacion subBasin con NUts (de acuerdo indices Emiliano), para disponer de dicha relacion
      the_SubCt_N2 <- NUTS2_BASIN(nameCat,L_WQnI,CrSC,Mode=1) 
      # nrow(the_SubCt_N2)     #hay algun N2 de emiliano sin equivalente en la tabla N2Corr
      # length(unique(the_SubCt_N2$N2))  #de los ID de Emiliano
      the_SubCt_N2$N2name <- N2Corr[ match(the_SubCt_N2$N2,N2Corr$EMILIANO),"NUTS_ID"]
      the_SubCt_N2$N2name <- factor(the_SubCt_N2$N2name)  #elimina niveles no existentes en ese subseting
    
   #   head(the_SubCt_N2)
  #    unique(the_SubCt_N2$N2name)
   #   the_SubCt_N2[the_SubCt_N2$N2name=="HR04",]
      
  # Cada matriz es un tipo de demanda.
  # Las filas son subcuencas. Anadimos ultima columna  con la region en la que esta esa subcuenca
  # Las otras columnas: cada columna es la influencia de cada region en todas las sucuencas (subcuencas) aguas abajo de dicha region

  # Lo anterior lo hago para todos los elementos de la lista dentro de un loop
  for (i in 1:length(RoutingWList)){
  #  i<-6
    #Remplazo y ordeno los nombres de las columnas (las regiones)
    if (length(listofNuts_ID)>1) {  #solo funciona cuando tenemos mas de 1 nuts, para Evrotas no se hace
      newnames <- N2Corr$NUTS_ID[match(colnames(RoutingWList[[i]]),N2Corr$EMILIANO )]
      colnames(RoutingWList[[i]]) <- as.character(newnames)
      #Reordeno columnas de acuerdo a los nombres
      RoutingWList[[i]] <- RoutingWList[[i]][ , order(colnames(RoutingWList[[i]]))]
    }
          #  newnames <- N2Corr$NUTS_ID[which(N2Corr$EMILIANO ==listofNuts_ID) ]          #si la cuenca tiene un solo Nut (Evrotas), tengo que usar otra sentencia
  }    # [[1]]: DomesticWD, [[2]]: Energy [[3]] IndustrialWD, [[4]]:IrrigationWD , [[5]]: LivestockWD ,
  

     fresW_N2<- merge(tempFresW,the_SubCt_N2, by.x=0,by.y="SC",all=TRUE)  #le anado el N2
     #remplaza la columna N2name con N2
     fresW_N2$N2 <- NULL
     names(fresW_N2)[3]<-"N2" 
     fresW_N2_Ar<- merge( temparea,fresW_N2, by.x=0,by.y="Row.names",all=TRUE)    #anado tambien el Area
     colnames(fresW_N2_Ar)[2:3] <- c("area", "fresW")
     
    # Incluyo fresh water y are como un elemento mas de la lista
     RoutingWList[[6]] <- fresW_N2_Ar

      #######
      #ATENCION, faltara resolver cuando algun NUTS es NA, por ejemplo Andorra
      #ATENCION, porque cuando aplico reduccion demanda en una region, por ruteo dicha reduccion se arrastra aguas abajo
   
  return(ListaWaterRouting=RoutingWList)    
}  
  
  

# BLScenBasin <- BLScen
# #############################
#  Para la cuenca tenemos las demanda acumulada para cada Region y sectores. 
#  Esta funcion calcula cual es la demanda NO ACUMULADA, es decir la que no considera nada de aguas arriba
#  La principal utilidad es para calcular los esfuerzos de reduccion de presiones (demandas)
# ################################
ComputeNotRoutDemand<- function (BLScenBasin){
  
  BLScenBasin[[1]]  #Ejemplo demanda domenstica demanda:  # [[1]]: DomesticWD, [[2]]: Energy [[3]] IndustrialWD, [[4]]:IrrigationWD , [[5]]: LivestockWD ,
  TheN2 <- sort(unique(BLScenBasin[[6]]$N2))            # para cada nuts
  
  # Hago un loop en el que itero para ambos elementos, para anular todas las demandas acumuladas, menos la de una region y sector
  # de esa forma puedo calcula las demanda sin rutear. Para ello, todas las demandas a 100 (reduccion total), menos la correspondiente del loop
      
      # 1. Genero la matriz de reducciones de demandas. Todo a 100: Eliminaria todas las demandas 
      dimx <- length(TheN2)  #el numero de columnas de la matriz (una por cada NUTS)
      FullReductStg <- matrix( c( rep(100,dimx),     # domestWD   
                               rep(100,dimx),      # Energ
                               rep(100,dimx),       # Indust
                               rep(100,dimx),      # Irrig
                               rep(100,dimx)),      # Livest
                            byrow = TRUE, ncol = dimx)
      rownames(FullReductStg) <- c("Domest","Energ","Indust","Irrig","Livest")  #cada fila es un tipo de presion
      colnames(FullReductStg) <- TheN2
      
      #  2. Creo matriz vacia donde meter las demandas sin routing
      NoRoutDemands <- data.frame(matrix( nrow = 5, ncol = length(TheN2)))  #5 filas de los sectores, una columna para cada region
      colnames(NoRoutDemands) <- (TheN2)
      rownames(NoRoutDemands) <- c("Domest","Energ","Indust","Irrig","Livest")
        
      #  3. Loop en el que se van calculando las demandas sin acumular por sector y region 
      for(use in 1:5){
        for (reg in as.character(TheN2)){        # [filas, columnas]
          #  use<-1
          # reg<-"AL01"
            FullReductStg[use,reg] <-0   #Pongo a 0 la reduccion correspondiente. La unica demanda no reducida es esa
            EvalScenDemand <- EvalStg (BLScenBasin,FullReductStg)  
            NoRoutDemands[use,reg] <- sum(EvalScenDemand[[1]][EvalScenDemand[[1]]$N2==reg,(2+use)])  # Solo lo de la propia region, para descartar lo que rutea esa propia demanda
            FullReductStg[use,reg] <- 100     #lo vuelvo a poner a 100, para la siguiente iteracion
        }
      }
      
      return(NoRoutDemands)  #devuelve las demandas por region y sector (SIN RUTEO)
}






GenerateBLS_old <- function (nameCat,L_WQnI, CrSC,N2Corr) {
  # length((r_fresW))
  # head(r_fresW)
  indexCol <-2
  indexCatchFW <- which(r_fresW[,indexCol]!=0,arr.ind = T)  #el resultado serian las filas de los catchmet sobre los que esa provincia tiene efecto
  indexCatchDW <- which(r_domestWD[,indexCol]!=0,arr.ind = T) 
  indexCatchIW <- which(r_industWD[,indexCol]!=0,arr.ind = T) 
  
  for (indexrow in 5000:5500){
    indexColNUT <- which(r_fresW[indexrow,]!=0,arr.ind = T)
    print(paste0(indexrow," -  ",indexColNUT))  #el resultado son todas las provincias que influyen sobre un NUTS
  }      
  
  
  
  #    Area <- L_WQnI[[10]]
  
  #los hidro Id de las subcuencas del Basin seleccionado  
  listofSubcatch_ID <- CrSC[CrSC$basin_name == nameCat ,]$cathment_hydroid   #ATENCION DEBERIA CONVERTIR A STRING
  #  length(listofSubcatch_ID)
  #List with the subseting of water (availability and uses) in the catchment
  # rm(List_C_W)
  #primer elemento de la nueva listafreswater
  df_C_W <- data.frame()  #DF con fresh y usos por subcatchment
  for(ind in 4:10){
    # ind<-4
    #cada indice es uno de los usos del agua, los meto en una lista
    # 4:r_fresW; 5:r_domestWD; 6:r_industWD; 7:r_energWD; 8:r_livesWD; 9:r_irrWD;  10: anado tambien el area
    df_C_W <- rbind(df_C_W,
                    t(L_WQnI[[ind]][names(L_WQnI[[ind]]) %in% listofSubcatch_ID])  )
  }
  
#  rownames(L_WQnI[[4]])
#  names(L_WQnI[[5]])
  
  #  length(t(List_WaterInfo[[ind]][names(List_WaterInfo[[ind]]) %in% listofSubcatch_ID]) )
  
  #si la traspongo, los nombres de las filas son los de cada subcatchment
  df_C_W2 <- as.data.frame(t(df_C_W)) #el traspose lo convertiria en matriz
  rownames(df_C_W2)<-  colnames(df_C_W) #le pongo los nombres de las filas a las columnas
  colnames(df_C_W2) <- c("fresW","domestWD","industWD","energWD","livesWD","irrWD","Area")     #y a las columnas a mano
  
  #ANADO COLUMNA CON EL NUT , PERO REMPLAZANDO LAS ID DE EMILIANO POR NOMBRES   

  #llama a la funcion que devuelve la relacion subBasin con NUts (de acuerdo indices Emiliano), para disponer de dicha relacion
  the_SubCt_N2 <- NUTS2_BASIN(nameCat,L_WQnI,CrSC,Mode=1) 
  # nrow(the_SubCt_N2)     #hay algun N2 de emiliano sin equivalente en la tabla N2Corr
  # length(unique(the_SubCt_N2$N2))  #de los ID de Emiliano
  the_SubCt_N2$N2name <- N2Corr[ match(the_SubCt_N2$N2,N2Corr$EMILIANO),"NUTS_ID"]
  the_SubCt_N2$N2name<-factor(the_SubCt_N2$N2name)  #elimina niveles no existentes en ese subseting

  # remplazo los NA por "UNKNOWN"    #cuando hay alguna subcuenca que no esta en la seleccion de NUTS
  # Get levels and add "UNKNOWN"
  levels <- levels(the_SubCt_N2$N2name)
  levels[length(levels) + 1] <- "UNKNOWN"
  # refactor Species to include "None" as a factor level
  # and replace NA with "None"
  the_SubCt_N2$N2name <- factor(the_SubCt_N2$N2name, levels = levels)
  the_SubCt_N2$N2name[is.na( the_SubCt_N2$N2name)] <- "UNKNOWN"
  head(the_SubCt_N2)
  nrow(the_SubCt_N2)
  #remplaza la columna N2name con N2
  the_SubCt_N2$N2 <- NULL
  names(the_SubCt_N2)[2]<-"N2"
  
  the_SubCt_N2$N2<-factor(the_SubCt_N2$N2)  #ya elimino el factor unknow
  # FINALMENTE, lo mezclo con el data frame del BLS
  df_result<- merge(df_C_W2,the_SubCt_N2, by.x=0,by.y="SC",all=TRUE)

  return(df_result)
  
  
        #### PROCEDIMIENTO ANTIGUO PARA CAMBIAR LOS N2 ID DE EMILIANO. eL PROBLEMA ES QUE DEJABA filas de subcatchment con NA y se perdia el orden a la hora de mapearlo
        # Remplazo el ID Emiliano por el ID deseado para cada nUTS2
        #    TempMerg <- merge(the_SubCt_N2,N2Corr, by.x="N2",by.y="EMILIANO")      #,all=TRUE 
        #    TempMerg$N2 <-TempMerg$NUTS_ID
        #    TempMerg <- TempMerg[,1:2]
        #    TempMerg$N2 <- factor(TempMerg$N2)  #elimina todos los factores sin datos
        #    # lo mezclo con el data frame del BLS
        #    df_result<- merge(df_C_W2,TempMerg, by.x=0,by.y="SC",all=TRUE)
        
        
        #le anado los WEI (de la columna 7 en adelante)
        # class(df_C_W2)
        #  df_C_WEI <- df_C_W2  #data frame con el WEI por subcathment (lo inicializo a copia del anterior)
        # nrow(df_C_WEI)
        # head(df_C_WEI)
        # max(df_C_WEI$energWD)
        # boxplot(df_C_WEI$energWD)
        # boxplot(df_Catch_Inf$uses$fresW,df_Catch_Inf$uses$energWD, df_C_WEI$energWD)
        # length(df_Catch_Inf$uses$energWD)
        # length(df_C_WEI$energWD)
  
  
}

