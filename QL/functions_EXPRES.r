################################################################################
# Author: Angel Udias     all complaints to: angel.udias-moinelo@ec.europa.eu  #
################################################################################
# Started: 12-Mar-2018 ->    ;                                                 #
# Updates:                                                                     #
#                                                                              #
# ESPRES: Efficient Strategies for anthropogenic Pressure Reduction in European waterSheds                                                                             #
#        other functions needed for the tool                           #
#                                                                              #
# DESCRIPTION :                                                                #
# Funciones para hacer operaciones intermedias   #
# 
#
#                                       #
#                                         #
#                                         #
#                      #
################################################################################






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


