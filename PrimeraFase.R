source("AG.R")
source("extras.R")
require("caret")
require("memoise")

PROBANDO<<-0

		 
primera_fase=function(archivo, metodo, interna, trials, clase_propiedad, alpha, pm, popSize, tourSize, pxo, pMut, eliteSize, nroGens, stallGens, umbral, valInterna){
	columnas=ncol(interna)
	if(pm>(columnas-1)){
		str1=iconv("Error, the maximum number of descriptors must be less or equal than the total number of descriptors.", from="UTF-8", to="UTF-8")
		gmessage(str1, icon="error")
		return
	}
	m=proc.time() 
	soluciones=data.frame()
	soluciones=matrix(data=NA, nrow=trials, ncol=(columnas-1))
	iterwrap=c(1:trials) #trials x parametros
	for(i in iterwrap){
		set.seed(i) 
		print("Primera Fase")
		pr=proc.time()
		dataframe1=partir(interna, valInterna, i)
		print(proc.time()-pr)
		algogenet=algoritmo_genetico_2(metodo, dataframe1$it, dataframe1$et, clase_propiedad, alpha, pm, popSize,  tourSize, pxo, pMut, eliteSize, nroGens, stallGens, umbral)	
		soluciones[i,]=algogenet$individuo	
	}
	
	calcular_maes(archivo, metodo, interna, soluciones, clase_propiedad)	
	print(proc.time()-m)
	soluciones
}

calcular_maes=function(archivo, metodo, interna, soluciones, clase_propiedad){
	print("Calculating errors...")
	
	cant=nrow(soluciones)
	num=length(soluciones[1,])-1
	maes_primero=data.frame()
	
	for(i in 1:cant){
		datos=filtrar(interna, soluciones[i,])
		if(metodo==1){ #speedglm
			for(j in 1:10){
				partes=partir(datos, 0.75, j)
				train=partes$it
				test=partes$et
				
				modelo=construir_modelo(1, train)
				
				m2=nrow(test)
				ncols=ncol(test)
				suma=0
				for(k in 1:m2){
					yi=test[i,ncols] #valor de la propiedad para el compuesto i
					fin=ncols-1
					if(fin!=1)
						xi=test[ i, 1:fin] #valores de los descriptores para el compuesto i
					else{#fin es 1
						if(dim(test)[2]!=1){
							xi1=test[i, 1:2]
							xi=xi1[1]
						}else{ #tiene solo una columna
							xi=data.frame(test)
							names(xi)=c(names(test))
						}
					}
					ypredict=predict(modelo, newdata=xi, fitted=FALSE) #fitted->TRUE o FALSE
					
					diferencia=yi-ypredict
					cuad=abs(diferencia)
					suma=suma+cuad
				}
				mae=suma/m2
				maes_primero[i,j]=mae
			}
		}else{ #metodo de weka
			modelo=construir_modelo(metodo, datos)
			for(j in 1:10){
				eval1=evaluate_Weka_classifier(object=modelo, numFolds=10, seed=j)
				if(clase_propiedad=="num"){
					mae=as.numeric(eval1$details[2])
				}else{
					mae=as.numeric(eval1$details[5])
				}
				maes_primero[i,j]=mae
			}
		}
	}
	
	save(maes_primero, file=archivo) 
}