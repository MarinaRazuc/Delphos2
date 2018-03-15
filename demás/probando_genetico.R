global1=0
global2=2
global3=c()


asigno=function(x,y,z){
	global1<<-x
	global2<<-y
	global3<<-z

	print(global1)
	print(global2)
	print(global3)
	
}


llamado=function(x,y,z){
	print("Antes de la llamada")
	print(global1)
	print(global2)
	print(global3)
	asigno(x,y,z)
	print("DESPUES DE LA LLAMADA")
	print(global1)
	print(global2)
	print(global3)
}





genetico=function(df){
	resus=ga(
		type="binary",
		nBits=5, 
		fitness=fitness2,
	#	population = gabin_Population,
	# 	selection = gabin_tourSelection,
	#  	crossover = gaControl(type)$crossover, 
	#   mutation = gaControl(type)$mutation,
		popSize = 20, 
	#	pcrossover = pxo, 
	#	pmutation = pMut, 
		elitism = 3, 
	#   updatePop = FALSE,
	#   postFitness = NULL,
		maxiter =30,
	#	run =stallGens,
	#   maxFitness = Inf,
	#   names = NULL,
	#   suggestions = NULL, 
	#   optim = FALSE,
	#   optimArgs = NULL,
	#   keepBest = FALSE,
	#   parallel = FALSE,
	#  	monitor = if(interactive()){ 
	#				if(is.RStudio()) gaMonitor else gaMonitor2 
	#			}else FALSE,
		monitor=FALSE,
	#   seed = NULL
	
	)
}




