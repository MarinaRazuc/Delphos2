source("AG.R")
source("extras.R")
require("caret")
#require("GA")
#require("doParallel")
#require("iterators")
#require("foreach")
require("memoise")
		 
primera_fase=function(archivo, metodo, interna, trials, clase_propiedad, alpha, pm, popSize, tourSize, pxo, pMut, eliteSize, nroGens, stallGens, umbral, valInterna){
	
	columnas=ncol(interna)
	if(pm>(columnas-1)){
		str1=iconv("Error, la cantidad máxima de descriptores a elegir es mayor a la cantidad de descriptores disponibles.", from="UTF-8", to="UTF-8")
		gmessage(str1, icon="error")
		return
	}
	m=proc.time() 
	soluciones=data.frame()
	soluciones=matrix(data=NA, nrow=trials, ncol=(columnas-1))
	#ver despues como usar las medias algogenet@summary --> matriz de medias, min, max, etc de cada iteracion
	# print("TRIALS")
	# print(trials)
	iterwrap=c(1:trials) #trials x parametros
	for(i in iterwrap){
		set.seed(i) 
		print("Primera Fase")
		
		dataframe1=partir(interna, valInterna, i)
	
		#correr GA con entrenamiento (dataframe1$it) y testeo (dataframe$et)
		convergencia<<-FALSE
		algogenet=algoritmo_genetico_2(archivo, metodo, dataframe1$it, dataframe1$et, clase_propiedad, alpha, pm, popSize,  tourSize, pxo, pMut, eliteSize, nroGens, stallGens, umbral) #... etc
		#print("-------ALGOGENET------")
		#print(algogenet)
		
		soluciones[i,]=algogenet$individuo
		#soluciones[i,]=algogenet@solution[1,]
		
	}
	print(proc.time()-m)
	soluciones

}