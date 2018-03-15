source("AG.R")

llamada=function(metodo, entrenamiento, testeo, clase_propiedad, pm, popSize, nroGens, stallGens, stallThres){
	ag=proc.time()
	individuo=algoritmo_genetico_2(metodo, entrenamiento, testeo, clase_propiedad, alpha=0.6, pm, popSize, tourSize=3, pxo=0.6, pmut=0.2, eliteSize=5, nroGens, stallGens, stallThres)
#	individuo=algoritmo_genetico(metodo, entrenamiento, testeo, clase_propiedad, alpha=0.6, pm, popSize, tourSize=3, pxo=0.6, pMut=0.2, eliteSize=5, nroGens, stallGens, stallThres)

	print(proc.time()-ag)
	
	individuo
}
