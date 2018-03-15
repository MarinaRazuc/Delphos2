source("construir_modelo.R")
source("extras.R")
require("memoise")

#vars globales 
metodoG=0
entrenamientoG=data.frame()
testeoG=data.frame()
alphaG=1
pmG=0
funcionFitnessG="num" # num de numerico, nom de nominal
contador=0
#I=0
maxConver=0
conver=0
mejor=0
diferencia=	0.000001
umbralFitness=0.000001
nroIter=0
popTam=0
PromFit=0
PromFitViejo<<-0
convergencia=FALSE
#F1
cardinalidad<-function( individuo ){  #F1 en la tesis
	largo<-length(individuo)
	iters<-c(1:largo)
	total<-0

	for(i in iters){
		if( individuo[i]!=0 )
			total<-total+1
	}
	total
}


#asumo que metodo es un número que obtengo mediante la interfaz gráfica
#precision_2(metodo, entrenamiento, testeo, individuo)
precision_2<-function(metodo, datasetE, datasetT, individuo){  #preparación de F2 en la tesis
	bandera=FALSE

	#filtro los datos de entrenamiento y de testeo segun el individuo (descriptores elegidos)
	datosEfiltro<-filtrar(datasetE, individuo)
	datosTfiltro<-filtrar(datasetT,individuo)
	
	#creo el modelo teniendo en cuenta metodo, datosEfiltro y otros parámetros correspondientes
	#(ver si me los pasan por parámetro o cómo los obtengo)
	t <- proc.time() 
	
	modelo=tryCatch(construir_modelo(metodo, datosEfiltro), #... significa parámetros oooo los puedo tener como vars globales
	error=function(e){
						print("ERROR")
						print(e)
						bandera<<-TRUE
					}
	)
	
	tiempo=proc.time()-t
	
	if(bandera==FALSE){
		if(funcionFitnessG=="num"){
		#	p <- proc.time()
			resu_F2<-F2(modelo, datosTfiltro)
			
			#print(proc.time()-p)
		}else{
		#	p <- proc.time()
			resu_F2<-F2_clasif(modelo, datosTfiltro)
		#	print(proc.time()-p)
		}
	}else {
		#print("BANDERA ES TRUE")
		resu_F2=1000  #numero grande porque marca el error
	}
	resu_F2
 #	write(eval$string, file=paste0('Resultados_',N))
 	#scan(file = 'Resultado_LR.txt', what = 'char', sep = "\n")
}




F2_clasif=function(modelo, datosTfiltro){
	#evaluate me brinda información sobre el modelo
	eval=evaluate_Weka_classifier(modelo, newdata = datosTfiltro)
	#busco la posición del renglón donde están los casos correctamente clasificados
	 
	pos=regexpr('Correctly', eval$string)
	#obtengo un substring entre posición+56 (lo que ocupa "Correctly classified cases y el nro") y posición+72 (espacios en blanco)
	keep=substring(eval$string, pos+56, pos+60) #ver 
	#elimino espacios en blanco para quedarme con el número
	valor=gsub(" ", "", keep)
	#convierto el char a int 
	#valorint=round(strtoi(valor))
	valorint=as.double(valor)
	resultado=valorint/100
	
	resultado
}


#F2(modelo, testeoFiltrado)
#Estima la precisión de un método de predicción cuando un dado conjunto de descriptores es usado.
#mean square error of prediction - error medio cuadrado de la predicción
F2=function(P, testeoFiltrado){
		
	m2=nrow(testeoFiltrado)#cantidad de filas de testeoFiltrado
	frac=1/m2
	suma=0
	ncols=ncol(testeoFiltrado)
	
	#library(doParallel)
	#registerDoParallel(cores=2)
	
	t <- proc.time() 
	for (i in 1:m2){
		yi=testeoFiltrado[i,ncols] #valor de la propiedad para el compuesto i
		fin=ncols-1
		if(fin!=1)
			xi=testeoFiltrado[ i, 1:fin] #valores de los descriptores para el compuesto i
		else{#fin es 1
			if(dim(testeoFiltrado)[2]!=1){
				xi1=testeoFiltrado[i, 1:2]
				xi=xi1[1]
			}else{ #tiene solo una columna
				xi=data.frame(testeoFiltrado)
				names(xi)=c(names(testeoFiltrado))
			}
		}
		#aca siempre son numericos
		if(metodoG==1){ #por ahora  speedglm
			#ypredict=predict(P, newdata=xi, type="response") #type link o response
			ypredict=predict(P, newdata=xi, fitted=FALSE) #fitted->TRUE o FALSE
		}else{#alguno de weka
			ypredict=predict(P, newdata=xi, type="class")  #------------------------------VER
		}
		diferencia=yi-ypredict
		cuad=diferencia^2 #ver si funciona
		suma=suma+cuad
	}
	print(proc.time()-t)
	
	resultado=frac*suma

	resultado
}



fitness1=function(individuo, ...){
	resultado<-fitness_real(individuo, metodoG, entrenamientoG, testeoG, alphaG, pmG)

	PromFit<<-PromFit+resultado
		
	resultado
}

mfitness=memoise(fitness1)


fitness_real<-function(individuo, metodo, entrenamiento, testeo, alpha, pm){ #... parametros -{}?
	print("Fitness")
	efe1=cardinalidad(individuo)
	if(efe1==0){
		FAG=0.000001 #probando
	}else{
		efe2<-precision_2(metodo, entrenamiento, testeo, individuo)
		
		#FAG=aF2+(1-a)F2F1/pm
		frac=efe1/pm
		FAG=alpha*efe2+(1-alpha)*efe2*frac
		
		FAG=1/FAG
	}
	
	# print(efe1)
	# print(efe2)
	FAG 
}




# generar población de manera aleatoria
# nro_desc es cantidad de descriptores
# generar popSize individuos aleatorios, de tamaño nro_desc, con max cantidad de locis seteados a 1 
# genero una población que tiene popSize cantidad de individuos.
# cada individuo tiene nro_desc cantidad de cromosomas
# cada individuo tiene max cantidad de cromosomas seteados a 1
generar_poblacion_inicial=function(nro_desc, popSize, maximo ){ 
	
#	print("Generar población")
	pop=matrix(nrow=popSize, ncol=nro_desc)

	
	for (i in 1:popSize){
		
		ind=runif(nro_desc, 0, 1)
		ind=round(ind)
		
		if(cardinalidad(ind) > maximo){
			ind=ajustar(ind, maximo)
		}
		
		if(nro_desc==maximo){
			if(cardinalidad(ind)>(nro_desc/2)){
				
				ind=ajustar(ind, maximo)		#MM
			}
		}
		#print(ind)
		#ind[nro_desc+1]=1 #para tomar la propiedad
		pop[i,]=ind
		#print("Nuevo individuo")
	} 
	#print(proc.time()-gp)
	#print("Ya genere la poblacion.")
#	write("poblacion inicial", file="poblaciones.csv", append=TRUE)
#	write.table(pop, file="poblaciones.csv", append=TRUE)
	pop
}




ajustar=function(ind, maximo){
	#se podrian hacer muchos if, viendo si la cardinalidad es multiplo de x de max, y de ahi decidir cuantos bits poner a 0
	#print(ind)
	
	while(cardinalidad(ind) > maximo){
		i=runif(1, 1, length(ind))
		j=runif(1, 1, length(ind))
		k=runif(1, 1, length(ind))
		l=runif(1, 1, length(ind))
		m=runif(1, 1, length(ind))
		ind[round(i)]=0
		if(cardinalidad(ind)>1){
			ind[round(j)]=0
			if(cardinalidad(ind)>1){
				ind[round(k)]=0
				if(cardinalidad(ind)>1){
					ind[round(l)]=0
					if(cardinalidad(ind)>1){
						ind[round(m)]=0
					}
				}
			}
		}
		#print(cardinalidad(ind))
		#print(max)
	}
	ind
}




fitness2=function(x){
	resul=cardinalidad(x)/length(x)
	resul
}


#PERMUTACION (para ga)
#Return a list with two elements:
#children: a matrix of dimension 2 times the number of decision variables 
# 			containing the generated offsprings;
#fitness: a vector of length 2 containing the fitness values for the offsprings. 
#			A value NA is returned if an offspring is different (which is usually the case) from the two parents.
# permutacion= function(objeto, parents){ #parents: A two-rows matrix of values indexing the parents from the current population.
	# padre1=objeto@population[parents[1], ]
	# padre2=objeto@population[parents[2], ]
	# largo=length(padre1)
	# punto=round(runif(1,1,largo-2)) #punto de cruce
	# h1p1= padre1[1:punto] #hijo 1 parte 1
	# h1p2= padre2[(punto+1):largo]
	# h2p1=padre2[1:punto]
	# h2p2=padre1[(punto+1):largo]
	
	# hijo1=c(h1p1, h1p2)
	# hijo2=c(h2p1, h2p2)
	
	# if(cardinalidad(hijo1)>pmG)
		# hijo1=ajustar(hijo1, pmG)
	# if(cardinalidad(hijo2)>pmG)
		# hijo2=ajustar(hijo2, pmG)
	
	# children=matrix(NA, 2, largo)
	# children[1,]=hijo1
	# children[2,]=hijo2
	
	# resultado=list()
	# resultado$children=children
	# resultado$fitness=c(NA, NA)
	# resultado
# }

#permutación2
permutacion2=function(padre1, padre2){
	largo=length(padre1) #el largo es el mismo para ambos
	#print("LARGO PADRES----------------")
	#print(largo)
	punto=round(runif(1,1,largo-2)) #punto de cruce
	#print("PUNTO------------------------")
	#print(punto)
	h1p1= padre1[1:punto] #hijo 1 parte 1
	h1p2= padre2[(punto+1):largo]
	h2p1=padre2[1:punto]
	h2p2=padre1[(punto+1):largo]
	
	hijo1=c(h1p1, h1p2)
	hijo2=c(h2p1, h2p2)
	
	if(cardinalidad(hijo1)>pmG)
		hijo1=ajustar(hijo1, pmG)
	if(cardinalidad(hijo2)>pmG)
		hijo2=ajustar(hijo2, pmG)
	
	children=matrix(NA, 2, largo)
		
	resultado=list()
	resultado$hijo1=hijo1
	resultado$hijo2=hijo2
	resultado

}

#MUTACION----(para ga)----------------
# mutacion=function(objeto, parent){
	# individuo=objeto@population[parent, ]
	# largo=cardinalidad(individuo)
	# # print("-----> MUTACIÓN:   cardinalidad individuo:")
	# # print(largo)
	
	# if(largo>0){
		# rnd=round(runif(1,1,largo))
		# valor=individuo[rnd]
		
		# valor=1-valor
		# individuo[rnd]=valor
		# if(cardinalidad(individuo)>pmG)
			# individuo=ajustar(individuo, pmG)
	# }
	# individuo
# }

#Mutación 2
mutacion2=function(individuo){
	largo=cardinalidad(individuo)
	
	if(largo>0){
		rnd=round(runif(1,1,largo))
		valor=individuo[rnd]
		
		valor=1-valor
		individuo[rnd]=valor
		if(cardinalidad(individuo)>pmG)
			individuo=ajustar(individuo, pmG)
	}
	individuo
}

#ALGORITMO GENÉTICO
# algoritmo_genetico<-function(metodo, entrenamiento, testeo, clase_propiedad, alpha, pm, popSize, tourSize, pxo, pMut, eliteSize, nroGens, stallGens, stallThres){
	
	# PromFit<<-0 #VER SI ESTO ES ASI
	# nroIter<<-0
	# popTam<<-popSize
	# numcols=ncol(entrenamiento) #entrenamiento y testeo tienen el mismo nro de cols
	# if(pm==0 || pm>numcols){
		# pm<-numcols-1
	# }
	 
	# conver<<-0
	# mejor<<-0
	# maxConver<<-nroGens *0.2 #el 20% de la max cant de generaciones
	# # print("MAXCONVER------------------")
	# # print(maxConver)
	# entrenamientoG<<-entrenamiento
	# testeoG<<-testeo
	# alphaG<<-alpha
	# pmG<<-pm
	# metodoG<<-metodo
	# funcionFitnessG<<-clase_propiedad
	# umbralFitness<<-stallThres
	# pobl=generar_poblacion_inicial(numcols-1,  popSize, pm) #ver si esta bien ese -1
	# # individuo=Pop[i,],
	# # defaultControl=gaControl()
	# # gaControl("binary", list(selection="gabin_tourSelection"))
	# resus<-ga( 
		# type="binary", nBits=numcols-1, #nBits: a value specifying the number of bits to be used in binary encoded optimizations.
		# # fitness=fitness1,
		# fitness=mfitness,
		# # population = gabin_Population,
	   # selection = gabin_tourSelection,k=tourSize,#(object, k = 3, ...), ver como hay que llamar a esto 
	  # crossover = permutacion, #gaControl("binary")$crossover, 
	   # mutation = mutacion, 
	   # popSize = popSize, 
	   # pcrossover = pxo, 
	   # pmutation = pMut, 
	   # elitism = eliteSize, 
	  # # updatePop = TRUE,
	  # # postFitness = NULL,
	   # maxiter =nroGens,
	   # run =stallGens,
	  # # maxFitness = Inf,
	  # # names = NULL,
	   # suggestions = pobl, 
	   # optim = TRUE,
	   # optimArgs = list( method = "L-BFGS-B", 
						 # poptim = 0.05,
						 # pressel = 0.5,
						 # control = list(fnscale = -1, maxit = 100)
						# ),
	  # # keepBest = FALSE,
	 # # parallel =TRUE,
	   # # monitor = if(interactive()) 
				   # # { if(is.RStudio()) gaMonitor else gaMonitor2 } 
				 # # else FALSE,
	# monitor=FALSE,
	  # # seed = NULL
	   # )

	# # forget(mfitness) #limpiar cache?
	# # gaControl(defaultControl)
	
	# resus

# }

#promedio de los valores de un vector
calcular_promedio=function(valores){
	
	largo=length(valores)
	suma=0
	for(i in 1:largo){
		suma=suma+valores[i]
	}
	resu=suma/largo
	
	resu
	
}

#tengo que devolver una lista conteniendo al individuo y al indice
determinar_mejor_individuo=function(fit_vals){
	#mientras mas pequeño el valor de fitness mejor
	largo=length(fit_vals)
	mejor=0 #ver
	indice=1
	for(i in 1:largo){
		elem=fit_vals[i]
		
		if(elem>mejor){
			
			indice=i
			mejor=elem
		}
	}
	
	resultado=list()
	resultado$individuo=poblacion_actual[indice, ]
	resultado$indice=indice
	
	resultado
}

#diferencia en bits de dos individuos
comparar_individuos=function(individuoA, individuoB){
	largo=length(individuoA)
	cant=0
	for(i in 1:largo){
		if(individuoA[i]!=individuoB[i]){
			cant=cant+1
		}
	}

	cant
}

#devuelvo un indice
realizar_torneo=function(torneo, fit_vals){
#En torneo tengo tourSize indices
	indice=1
	mejor=0 #ver
	largo=length(torneo)
	
	for(i in 1:largo){
		valor=fit_vals[torneo[i]]
		
		if(valor>mejor){
			mejor=valor
			indice=torneo[i]
		}
	}

	indice
}

iguales=function(indiv1, indiv2){
	ig=FALSE
	largo=length(indiv1)
	bandera=FALSE
	i=1
	while(i<=largo && !bandera){
		if(indiv1[i]!=indiv2[i]){
			bandera=TRUE
		}
		i=i+1
	}
	 bandera
}

#genera el pool de apareamiento mediante torneos
generar_pool=function(tourSize, fit_vals, poblacion_actual, pxo){
	k=1
	parejas=ceiling(nrow(poblacion_actual)/2)
	
	#pool=matrix(0, parejas*2, ncol(poblacion_actual))
	pool=c() #q sea solo los indices
	
	torneo=c()
	p=c()
	for(i in 1:parejas){
		igls=FALSE
		while(!igls){
			for(h in 1:2){
				bandera=FALSE
				while(!bandera){ #segun pxo
					for(j in 1:tourSize){
						torneo[j]=round(runif(1,1,nrow(poblacion_actual)))
					} 
					p[h]=realizar_torneo(torneo, fit_vals)
					bandera=dado(pxo) #si bandera es TRUE, queda el individuo elegido, si es FALSE, se repite la elección
				}
			}
			igls=iguales(poblacion_actual[p[1], ], poblacion_actual[p[2], ])
		}
				
		pool[k]=p[1]
		k=k+1
		pool[k]=p[2]
		k=k+1
	}
	
	pool
}

#p es valor entre 0 y 1
dado=function(p){
	s=runif(1,0,1)
	if(s<=p){
		resu=TRUE
	}else{
		resu=FALSE
	}
	resu
}

#determina si indiviudo pertenece a poblacion
pertenece=function(poblacion, individuo){
	resultados=apply(poblacion, 1, function(x) all(x == individuo))
	largo=length(resultados)
	
	i=1
	pertenece=FALSE
	while(i<=largo && !pertenece){
		pertenece=resultados[i]
		i=i+1
	}
	
	pertenece
}

#algoritmo genético casero
algoritmo_genetico_2=function(archivo, metodo, entrenamiento, testeo, clase_propiedad, alpha, pm, popSize, tourSize, pxo, pmut, eliteSize, nroGens, stallGens, stallThres){	
	empeora<<-0
	PromFit<<-0 
	nroIter<<-0
	popTam<<-popSize
	numcols=ncol(entrenamiento) #entrenamiento y testeo tienen el mismo nro de cols
	filas=nrow(entrenamiento)
	if(pm==0 || pm>numcols){
		pm<-numcols-1 #debería ser proporción del tamaño del individuo
	}
	# <3 
	
	entrenamientoG<<-entrenamiento
	testeoG<<-testeo
	alphaG<<-alpha
	pmG<<-pm
	metodoG<<-metodo
	funcionFitnessG<<-clase_propiedad
	umbralFitness<<-stallThres
	poblacion_actual<<-generar_poblacion_inicial(numcols-1,  popSize, pm) #ver si esta bien ese -1
	fit_vals=c()
	
	PromFit<<-0
	PromFitAnterior<<-0
	bandera=FALSE
	
	
	nueva_poblacion=matrix(0, popSize, numcols-1) #no considero propiedad
		
	i=1
	while(i<=nroGens && !bandera){
		#tomar poblacion
		#calcular fitness de cada uno
		
		for(j in 1:popSize){
			individuo=poblacion_actual[j,]
			fit_vals[j]=mfitness(individuo)
		}
		
		PromFitAnterior<<-PromFit
		PromFit<<-calcular_promedio(fit_vals)		
		
		
		if((abs(PromFitAnterior-PromFit)<=umbralFitness)||(PromFit<PromFitAnterior)){
			#estanco o empeora
			empeora<<-empeora+1
		}else{
			if(PromFitAnterior<PromFit){
				#mejora significativa
				empeora<<-0
			}
		}
		
		if(empeora==stallGens){
			resultado=determinar_mejor_individuo(fit_vals)
			bandera=TRUE
			str2=paste(paste0("No hubo mejora significativa del fitness en ", stallGens), "generaciones.")
			write.table(poblacion_actual, archivo, append=TRUE)
		}
		
		if(!bandera){
			#elegir los mejores eliteSize individuos y ponerlos en la nueva población
			valores=fit_vals
			for(k in 1:eliteSize){
				mejor=determinar_mejor_individuo(valores)
				nueva_poblacion[k,]=mejor$individuo
				valores[mejor$indice]=0
				
			}
				
			#tomar de a tourSize individuos y hacer un torneo, el que gane pasa a formar parte del pool de apareamiento
			pool=generar_pool(tourSize, fit_vals, poblacion_actual, pxo)
		
			#tengo pool con parejas en orden
			#npop=proc.time()
			m=1
			k=eliteSize+1 #la siguiente posición libre en nueva_población luego de haber acomodado la elite
			
			while(k<=popSize){
				p1=poblacion_actual[pool[m],]
				#print(length(p1))
				p2=poblacion_actual[pool[m+1],]
				#print(length(p2))
				m=m+2
				hijos=permutacion2(p1,p2)
				h1=hijos$hijo1
				h2=hijos$hijo2
				mutar=dado(pmut)
				if(mutar){
					h1=mutacion2(h1)
				}
				mutar=dado(pmut)
				if(mutar){
					h2=mutacion2(h2)
				}
				while(pertenece(nueva_poblacion, h1)){
					h1=mutacion2(h1)
				}
				nueva_poblacion[k, ]=h1
				k=k+1
				if(k<=popSize){
					while(pertenece(nueva_poblacion, h2)){
						h2=mutacion2(h2)
					}
					nueva_poblacion[k,]=h2
				}
				k=k+1
			}
			#print(proc.time()-npop)
			#print("Poblacion generada")
		}
		i=i+1
	}#FIN WHILE
	
	if(!bandera){ #se ejecutaron todas las generaciones
		resultado=determinar_mejor_individuo(fit_vals)$individuo #????????
		#devuelvo el mejor individuo de la poblacion vieja
		#que seria el primero de la nueva ya que fue elegido por elitismo
		
		
		write.table(nueva_poblacion, archivo, append=TRUE)
	}
	
	forget(mfitness) 
	#print(resultado)
	resultado
		
}




