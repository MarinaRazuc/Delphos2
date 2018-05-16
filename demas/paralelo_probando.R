fitness <- function(x, pause = 0.1){
	Sys.sleep(pause)
	x*runif(1)
}


algo_gen=function(){

	resultados<-ga(
			type = "binary", #"real-valued", 
		   fitness=fitness, 
		   #min<-0, max<-10,
		   nBits=1,
		   population = gaControl("binary")$population,
		   selection = gaControl("binary")$selection,
		   crossover = gaControl("binary")$crossover, 
		   mutation = gaControl("binary")$mutation,
		   popSize = 50, 
		   pcrossover = 0.8, 
		   pmutation = 0.1, 
		   elitism = base::max(1, round(50*0.05)), 
		   updatePop = FALSE,
		   postFitness = NULL,
		   maxiter = 100,
		   run = maxiter,
		   maxFitness = Inf,
		   names = NULL,
		   suggestions = NULL, 
		   optim = FALSE,
#		   optimArgs = list(method = "L-BFGS-B", 
#							poptim = 0.05,
#							pressel = 0.5,
#							control = list(fnscale = -1, maxit = 100)),
#		   keepBest = FALSE,
		   parallel = TRUE,
#		   monitor = if(interactive()) 
#					   { if(is.RStudio()) gaMonitor else gaMonitor2 } 
#					 else FALSE,
		   seed = NULL
	
	)
	
	resultados

}