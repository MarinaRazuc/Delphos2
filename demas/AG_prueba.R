source("AG.R")





algoritmo_genetico=function(){ 
	
	
	tam=15
	pop_ini=generar_poblacion_inicial(tam, 30, 10 )
	agr=agregacion(pop_ini)
	
ga(type = "binary", 
	   fitness=fitness(strindividuo, metodo, entrenamiento, testeo, alpha, pm),
	   nBits=pm,
	   population = gaControl(type)$population,
	   selection = gabin_tourSelection#(object, k = 3, ...), ver como hay que llamar a esto 
	   crossover = gaControl(type)$crossover, 
	   mutation = gaControl(type)$mutation,
	   popSize = popSize, 
	   pcrossover = pxo, 
	   pmutation = pMut, 
	   elitism = eliteSize, 
	   updatePop = FALSE,
	   postFitness = NULL,
	   maxiter = nroGens,
	   run = stallGens,
	   maxFitness = Inf,
	   names = NULL,
	   suggestions = NULL, 
	   optim = FALSE,
	   optimArgs = NULL,
	   keepBest = FALSE,
	   parallel = FALSE,
	   monitor = if(interactive()) 
				   { if(is.RStudio()) gaMonitor else gaMonitor2 } 
				 else FALSE,
	   seed = NULL)


}


agregacion=function(){
	
}