require("RWeka")
require("speedglm")
ene=0
#se pasó del uso de un switch al uso de if anidados por el tipo de resultado que me daba el sapply.. 
#capaz que se puede volver para atrás y ver cómo usar el resultado de sapply
construir_modelo=function(metodo, datos){
						
	
	if(metodo==1){
		B=proc.time()
		resultado=regresionLineal4(datos)
		print(proc.time()-B)
	}else{
		if(metodo==2){
			resultado=regresionNoLineal(datos)
		}else{
			if(metodo==3){
				#B=proc.time()
				resultado=arbolDeRegresion2(datos)
				#resultado=arbolDeRegresion(datos)
				#print(proc.time()-B)
			}else{
				if(metodo==4){
					pep=proc.time()	
					resultado=kVecinosMasCercanos(datos)
					print(proc.time()-pep)
				}else{
					if(metodo==5){
						A=proc.time()
						resultado=arbolDeDecision(datos)
						print(proc.time()-A)
					}else{
						if(metodo==6){ #knn 
							resultado=kVecinosStacking(datos)
						}else{
							if(metodo==7){
								resultado=arbolDeDecision2(datos)
							}else{
								if(metodo==8){
									B=proc.time()
									resultado=randomForest(datos)
									#resultado=randomForestStacking(datos)
									print(proc.time()-B)
								}else{
									if(metodo==9){
										B=proc.time()
										resultado=randomCommittee(datos)
										print(proc.time()-B)
									}else{
										if(metodo==10){
											B=proc.time()
											resultado=multilayer(datos)
											print(proc.time()-B)
										}else{
											str1=iconv("No se eligió un método disponible", from="UTF-8", to="UTF-8")
											print(str1)
											return
										}										
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	resultado

}



randomForestStacking=function(datos){
	print("Random Forest Stacking")
	modelo=Stacking(formula=V1~., data=datos, 
					control=Weka_control(
								M="weka.classifiers.trees.RandomTree",
								X=10,
								S=2,
								B="weka.classifiers.trees.RandomForest", #B="weka.classifiers.lazy.IBk",
								num_slots=2
							)
					)
	modelo
}

multilayer=function(datos){
	print("Multilayer Perceptron")
	MP=make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
	modelo=MP(formula=V1~., data=datos)
	modelo
}

randomCommittee=function(datos){
	print("Random Committee")
	RC=make_Weka_classifier("weka/classifiers/meta/RandomCommittee")
	modelo=RC(formula=V1~., data=datos)
	modelo
}

randomForest=function(datos){
	print("Random Forest")
	RF=make_Weka_classifier("weka/classifiers/trees/RandomForest")
	modelo=RF(formula=V1~., data=datos)
	modelo
}

arbolDeDecision2=function(datos){
	str1=iconv("Árbol de Decisión", from="UTF-8", to="UTF-8")
	print(str1)

	pep=proc.time()
	modelo=Stacking(formula=V1~., data=datos, 
					control=Weka_control(
								M="weka.classifiers.trees.J48",
								X=10,
								S=2,
								B="weka.classifiers.trees.RandomTree", #B="weka.classifiers.lazy.IBk",
								num_slots=2
							)
					)
					
	print(proc.time()-pep)
	
	modelo
	
}

arbolDeDecision=function(datos){
	str1=iconv("Árbol de Decisión", from="UTF-8", to="UTF-8")
	print(str1)
	modelo=J48(formula=V1~., data=datos, control=Weka_control(M=10)) #mínimo de instancias por hoja, VER!
	
	modelo
}


kVecinosStacking=function(datos){

	str1=iconv("k Vecinos Más Cercanos", from="UTF-8", to="UTF-8")
	print(str1)
	pep=proc.time()
	modelo=Stacking(formula=V1~., data=datos, 
					control=Weka_control(
								M="weka.classifiers.lazy.IBk",
								X=10,
								S=2,
								B="weka.classifiers.trees.RandomTree", #B="weka.classifiers.lazy.IBk",
								num_slots=2
							)
					)
					
	print(proc.time()-pep)
	
	#evalAR=evaluate_Weka_classifier(modelo, numfolds=10, seed=2)
	#print(evalAR)
	
	modelo

}


kVecinosMasCercanos=function(datos){
	str1=iconv("k Vecinos Más Cercanos", from="UTF-8", to="UTF-8")
	print(str1)

	# largo=dim(datos)[2]
	# elemento=datos[1,largo]
	# print("ELEMENTO")
	# print(elemento)
	# print("CLASE")
	# print(class(elemento))
	
	
	modelo=IBk(formula=V1~., data=datos, control=Weka_control(K=3, F=TRUE, A="weka.core.neighboursearch.KDTree")) #VER SI EL K (CANT DE VECINOS) ME LO PASAN POR PARAMETRO O COMO LO DECIDO.
	
	#evalKNN=evaluate_Weka_classifier(modelo, numfolds=10, seed=2)
	#print(evalKNN)
	#write(evalKNN, file=paste0("evaluacion_", ene))
	ene<<-ene+1
	
	modelo
}

arbolDeRegresion=function(datos){
	str1=iconv("Árbol de Regresión", from="UTF-8", to="UTF-8")
	print(str1)


	modelo=M5P(V1~., datos, control=Weka_control(R=TRUE, N=TRUE))
	
	#evalAR=evaluate_Weka_classifier(modelo, numfolds=10, seed=2)
	#print(evalAR)
	#write(evalAR, file=paste0("evaluacion_", ene))
	ene<<-ene+1
	
	modelo
}

arbolDeRegresion2=function(datos){

	str1=iconv("Árbol de Regresión", from="UTF-8", to="UTF-8")
	print(str1)
	modelo=Stacking(formula=V1~., data=datos, 
					control=Weka_control(
								M="weka.classifiers.trees.M5P",
								X=10,
								S=2,
								B="weka.classifiers.trees.RandomTree", 
								num_slots=2
							)
					)
					
					
	#evalAR=evaluate_Weka_classifier(modelo, numfolds=10, seed=2)
	#print(evalAR)
	
	modelo
}



regresionNoLineal=function(datos){
	#Stacking() #con GaussianProcesses y MultilayerPerceptron
	#M-->full name of meta classifier
	#X-->nro de folds en el cross validation
	#S-->seed
	#B-->classifier specification
	str1=iconv("Regresión No Lineal", from="UTF-8", to="UTF-8")
	print(str1)
	
	nnn=proc.time()
	
	modelo=Stacking(
					formula=V1~., 
					data=datos,
					control=Weka_control(
								M="weka.classifiers.functions.MultilayerPerceptron",
								X=10,
								S=2,
								B="weka.classifiers.functions.GaussianProcesses", 
								num_slots=2
							)
	
			)
		
	
	print(proc.time()-nnn)
	
	#evalNLR=evaluate_Weka_classifier(modelo, numfolds=10, seed=2)
	
	#print(evalNLR)
	#write(evalNLR, file=paste0("evaluacion_", ene))
	ene<<-ene+1
	modelo
}


regresionLineal=function(datos){
	str1=iconv("Regresión Lineal", from="UTF-8", to="UTF-8")
	print(str1)

	LR=lm("V1~.", datos)
	#LR=LinearRegression(fmla, data=datos)
	
	#print(class(LR))
	resus=summary(LR)
	#dump("resus", file="sumaris.txt", append=TRUE)
	#print(summary(LR))
	#print(proc.time()-rl)
	LR
	
}


regresionLineal2=function(datos){
	str1=iconv("Regresión Lineal", from="UTF-8", to="UTF-8")
	print(str1)
	
	# print("dimension de los datos: ")
	# print(dim(datos))
	# print("valor")
	# print(datos[1,1])
	# print(names(datos)[1])
	
	fmla=as.formula("V1~.")
	LR=LinearRegression(fmla, data=datos, control=Weka_control(S=2, R="1.0e-6", minimal=TRUE), options=c(model=TRUE, instances=TRUE)) #
	#evalLR=evaluate_Weka_classifier(LR, numfolds=10, seed=2)
	#print(evalLR)
	#write(evalLR, file=paste0("evaluacion_", ene))
	#ene<<-ene+1
#	print(class(LR))
#	print(LR)
	LR

}

regresionLineal4=function(datos){
	str1=iconv("Regresión Lineal", from="UTF-8", to="UTF-8")
	print(str1)
	fmla=as.formula("V1~.")
	modelo=speedglm(fmla,datos, x=FALSE, y=FALSE, method="Cholesky") #maxit=25, k=2 ,  method=c('eigen','Cholesky','qr')
	
	modelo
}

trasponer=function(datos){
	cols=ncol(datos)
	prop=datos[,cols]
	valores=datos[, 1:(cols-1)]
	dfr=data.frame(prop, valores)
	dfr
}

regresionLineal3=function(datos){
	str1=iconv("Regresión Lineal", from="UTF-8", to="UTF-8")
	print(str1)
	modelo=Stacking(formula=V1~., data=datos, 
					control=Weka_control(
								M="weka.classifiers.functions.GaussianProcesses",
								X=10,
								S=2,
								B="weka.classifiers.functions.LinearRegression", 
								num_slots=2
							)
					)
	modelo
}

regresionLineal5=function(datos){
	str1=iconv("Regresión Lineal", from="UTF-8", to="UTF-8")
	print(str1)
	modelo=Stacking(formula=V1~., data=datos, 
					control=Weka_control(
								M="weka.classifiers.trees.RandomTree",
								X=10,
								S=2,
								B="weka.classifiers.functions.LinearRegression", 
								num_slots=2
							)
					)
					
	
	modelo
}


regresionLinealSpeedlm=function(datos){
	str1=iconv("Regresión Lineal", from="UTF-8", to="UTF-8")
	print(str1)
	fmla=as.formula("V1~.")
	
	modelo=speedlm("V1~.", datos, method='Cholesky', fitted = FALSE)
	
	modelo
}

