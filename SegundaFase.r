source("extras.R")
metodoSF<<-"RF"

obtener_archivo_entrada=function(){
	win1=gwindow(title="Cargar archivo...", visible=FALSE, width=300, height=100, parent=c(500,230))
	group2=ggroup(horizontal=FALSE, container=win1, spacing=15)
	group1=ggroup(horizontal=TRUE, container=group2, spacing=15)
	text1=gedit("Ingrese archivo", container=group1, width=30)
	button1=gbutton("Browse...", container=group1, 
					handler=function(h,...){
						file1=gfile(type="open")
						if(length(file1)>0){
							if(is.na(file1)){
								svalue(text1)="Ingrese archivo"
							}else{
								svalue(text1)=file1
							}
						}
						print(file1)
					}, width=15)	
	button2=gbutton("OK", container=group2, handler=function(h, ...){
												bandera<<-FALSE
												archivo_ent=svalue(text1)
												tryCatch(load(archivo_ent), 
														error=function(e){
															bandera<<-TRUE
														}
												)
												if(!bandera){
													ventana_fase_dos(archivo_ent)
													dispose(win1)
												}else{
													msg=iconv("El archivo seleccionado no es válido.", from="UTF-8", to="UTF-8")
													gmessage(msg, icon="error")
												}
											
												
											}, width=5)  
					
	visible(win1)=TRUE
}


ventana_fase_dos=function(archivo){
	win1=gwindow(title="SecondPhase", visible=FALSE, width=300, height=100, parent=c(425,200))
	frame1=gframe(container=win1, text="Second Phase Settings", horizontal=FALSE, spacing=15, pos=0)
	
	group200=ggroup(horizontal=TRUE, container=frame1, spacing=15)#experiment i of n 
	lay0=glayout(container=group200)
	ggroup(container=frame1)
	
	glabel(" ", container=frame1)
	group0=ggroup(horizontal=TRUE, container=frame1, spacing=5) #max cant subconjs
	
	stringS=iconv("  Máxima cantidad de subconjuntos:", from="UTF-8", to="UTF-8")
	labelC=glabel(stringS, container=group0)
	editS=gedit("5", container=group0, width=4)
	glabel(" ", container=frame1)
	
	group1=ggroup(horizontal = FALSE, container=frame1, spacing=10) #seleccionar metodo
	group11=ggroup(horizontal=TRUE, container=group1, spacing=5)
	str2="  Seleccione método para la segunda fase: "
	str2=iconv(str2, from="UTF-8", to="UTF-8")
	label1=glabel(str2 , container=group11)
	radio1 = gradio(c("RandomForest","RandomCommittee"), container=group1, 
				handler=function(h,...){
							valor=svalue(radio1)
							#print(valor)
							if(valor=="RandomCommittee"){
								 metodoSF<<-"RC"
							}else{
								if(valor=="RandomForest"){
									metodoSF<<-"RF"  
								}else{
									metodosSF<<-"RP"
								}
							}
					})
	group4=ggroup(container=frame1, spacing=15)
	glabel("  ", container=group4)
	group2=ggroup(horizontal=TRUE, container=frame1, spacing=15)#guardar como
	label2=glabel(" Guardar como... ", container=group2)
	texto2=gedit("SecondPhase.RData", container=group2)
	boton2=gbutton("Browse", container=group2,
					handler=function(h,...){
						file4=gfile("Guardar como...", type="save")
						if(is.na(file4)){
							svalue(texto2)="SecondPhase.RData"
						}else{
							svalue(texto2)=file4
						}
						print(file4)
					})
	group5=ggroup(container=frame1, spacing=15)
	glabel("   ", container=group5)
	group3=ggroup(horizontal=TRUE, container=frame1, spacing=15)
	lay3=glayout(container=group3)
	boton3=gbutton("OK",
					handler=function(h,...){
						valor=svalue(radio1)
						if(valor=="RandomCommittee"){
							 metodoSF<<-"RC"
							 
						}else{
							if(valor=="RandomForest"){
								metodoSF<<-"RF"
							} 
						}
						if(svalue(editS)==0){
							gmessage("La cantidad de subconjuntos debe ser mayor a 0", icon=error)
						}else{
							segunda_fase(archivo, metodoSF, svalue(texto2), svalue(editS))#, svalue(editI), svalue(editT))	
							dispose(win1)
						}
					} )
	lay3[1:3, 15:20]=boton3
	
	glabel("  ", container=frame1)
	
	visible(win1)=TRUE

}

segunda_fase=function(archivo, metodoSF, salida, maxCant){ 
	print("Segunda Fase")
	grafico=data.frame()
	auxiliar=data.frame()
	corr_coefs=c()
	maes_segundo=c()
	correctos=c()
	load(archivo)
	individuos=matrix(0,nrow(resultados), ncol(resultados))

	cols=ncol(externa)
	clase=class(externa[,cols])
	iteras=nrow(resultados)
	niveles=length(levels(externa[1,cols]))
	ROCareaS=matrix(0,iteras,niveles)
	confusion=matrix(0, iteras*niveles, niveles)
	matts=c()
	
	g=1
	param=1
	filac=1
	for(i in 1:iteras){ #por cada subconj de la primera fase
		ult=g+10
		colu=1
		while(colu<11){
			grafico[g,1]=i
			grafico[g,2]=maes_primero[i,colu]
			grafico[g,3]="P"
			colu=colu+1
			g=g+1
		}
		individuo=resultados[i,]
		IF=filtrar(interna, individuo)
		EF=filtrar(externa, individuo) 

		modelo=tryCatch(construirModelo(IF ,metodoSF), 
					error=function(e){
						gmessage("Error al calcular el modelo. ", icon="error")
						stop()
					}
				)
				
		if(clase=="numeric"){
			evalF2=evaluate_Weka_classifier(modelo, newdata=EF)
			corcoef=evalF2$details[1]
			mae=evalF2$details[2]
			maes_segundo[i]=mae
			corr_coefs[i]=corcoef
			grafico[g,1]=i
			grafico[g, 2]=mae
			grafico[g,3]="S"
			g=g+1
			
			print("---------------------")
			str1=iconv("Coeficiente de Correlación", from="UTF-8", to="UTF-8")
			print(str1)
			print(corcoef)
			print("---------------------")
			print("Mean Absolute Error: ")
			print(mae)
			print("---------------------")
		}else{ 
			evalF2=evaluate_Weka_classifier(modelo, newdata=EF, class=TRUE)
			correctos[i]=evalF2$details[1]
			mae=maes_segundo[i]=evalF2$details[5]
			matriz=evalF2$confusionMatrix
			for(h in 1:nrow(matriz)){
				for(k in 1:ncol(matriz)){
					confusion[filac, k]=matriz[h,k]
				}
				filac=filac+1
			}	
			noms=row.names(matriz)
			confusion=as.data.frame(confusion)
			names(confusion)=noms
			
			rocarea=c()
			#print(evalF2$detailsClass)
			for(j in 1:niveles){
				ROCareaS[i,j]=evalF2$detailsClass[j,6]
				rocarea[j]=evalF2$detailsClass[j,6]
			}
			print("---------------------")
			print("Porcentaje de casos clasificados correctamente: ")
			print(correctos)
			print("---------------------")
			print("Mean Absolute Error")
			print(mae)
			print("---------------------")
			str2=iconv("Matriz de Confusión", from="UTF-8", to="UTF-8")
			print(str2)
			print(matriz)
			print("---------------------")
			print("ROC Area")
			print(rocarea)
			print("---------------------")
		}
		auxiliar[param, 1]=i
		auxiliar[param, 2]=mae
		param=param+1
		if(i!=1){ #no es el primero
			 tryCatch(dev.off(), 
						 error=function(e){
											str1=iconv("No hay gráficos activos.", from="UTF-8", to="UTF-8")
											print(str1)
										 }
						 )
		}else{
			names(grafico)=c("Subconjunto", "MAE", "Fase")
			names(auxiliar)=c("Subconjunto", "MAE")
		}
		elems=buscar_mayor_y_menor(grafico, auxiliar)
		mayor=elems$mayor
		menor=elems$menor
		x11(width=80, height=50, title="MAE")
		boxplot(MAE~Subconjunto,  data=grafico, boxwex = 0.25, xlab = "Subconjunto",ylab = "MAE", col="lightblue", xlim=c(0, iteras+1), ylim=c(menor,mayor+0.02))
		par(new=TRUE)
		plot(auxiliar, axes=FALSE, col="red", type="p", xlim=c(0, iteras+1), ylim=c(menor,mayor+0.02), main="MAE - Primera y Segunda Fase")
		legend(x=iteras+0.1, y=mayor-0.02, legend="Fase Dos", col="red", text.width=0.3, pch="o")
	}
	completo=rbind(interna, externa)
	
	if(clase=="numeric"){
		ordenados=ordenarR(resultados, maes_segundo, corcoef)
		maes_segundo=ordenados$maes_ord
		resultados=ordenados$individuos
		corcoef=ordenados$n_corcoef
		
	}else{
		matts=calcular_matt(nrow(individuos), confusion)
		print("maes_Segundo")
		print(maes_segundo)
		ordenados=ordenarC(resultados, matts, maes_segundo, ROCareaS, correctos)
		resultados=ordenados$individuos
		maes_segundo=ordenados$maes_ord
		matts=ordenados$matts_ord
		ROCareaS=ordenados$rocas
		correctos=ordenados$correctos
	}
	print("iteras")
	print(iteras)
	print("maxCant")
	print(maxCant)
	
	if(iteras < maxCant){
		print("Devolver menos subconjuntos-----------------------------------------------------")
		#tengo mas subconjs de los que me pidieron
		resultados=resultados[1:maxCant, ]
		maes_segundo=maes_segundo[1:maxCant, ]
		if(clase=="numeric"){
			corcoef=corcoef[1:maxCant]
		}else{
			matts=matts[1:maxCant]
			ROCareaS=ROCareaS[1:maxCant, ]
			correctos=correctos[1:maxCant]
		}
	}
	
	save(resultados, corr_coefs, maes_segundo, confusion, ROCareaS,correctos, completo, grafico, matts, file=salida)
}

ordenarR=function(individuos, maes_segundo, corcoef){
	
	nuevos_individuos=matrix(0, nrow(ind), ncol(ind))
	cant=nrow(ind)
	indices=c(1:cant)
	indis_ords=c(1:cant)
	maes_ords=matrix(0, nrow(ind), 2)
	nuevo_corcoef=c(1:cant)
	
	for(i in 1:cant){
		idc=buscar_mejor(indices, corcoef)
		nuevos_individuos[i,]=ind[idc, ]
		maes_ords[i,1]=i
		maes_ords[i,2]=maes[idc]
		nuevo_corcoef[i]=corcoef[idc]
		indices[idc]=-1
	}
		
	ordenados=list()
	ordenados$individuos=nuevos_individuos
	ordenados$maes_ord=maes_ords
	ordenados$n_corcoef=nuevo_corcoef
		
	ordenados
}


calcular_matt=function(largo, confusion){
	cantCaract=length(names(confusion))
	fila=1
	matts=c(1:largo)
	
	for(i in 1:largo){ #por cada individuo
		FNT=FPT=TNT=TPT=0 #valores totales
		for(j in 1:cantCaract){ #por cada clase -> una tabla
			FN=FP=TN=TP=0
			TP=TP+confusion[fila+j-1, j] #TP es solo pos (j,j)
			for(k in 1:cantCaract){ #FN es fila j sin pos (j,j)
				if(k!=j){
					FN=FN+confusion[fila+j-1, k]
				}
			}
			for(k in 1:cantCaract){ #FP es columna j sin pos (j,j)
				if(k!=j){
					FP=FP+confusion[fila+k-1, j]
				}
			}
			for(k in 1:cantCaract){
				for(h in fila:(fila+cantCaract-1)){ #TN es toda la matriz sin pos j
					if(k!=j && h!=(j+cantCaract)){
						TN=TN+confusion[h,k]
					}
				}
			}
			FNT=FNT+FN
			FPT=FPT+FP
			TNT=TNT+TN
			TPT=TPT+TP
		}
		fila=fila+cantCaract
		matts[i]=cuenta(FNT, FPT, TNT, TPT)
	}
	matts
}

cuenta=function(FN, FP, TN, TP){
	nominador=TP*TN - FP*FN
	denominador=sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
	resultado=nominador/denominador
	
	resultado
}

ordenarC=function(ind, matts, maes, RA, correctos){
	nuevos_individuos=matrix(0, nrow(ind), ncol(ind))
	cant=nrow(ind)
	indices=c(1:cant)
	indis_ords=c(1:cant)
	maes_ords=matrix(0, nrow(ind), 2)
	matt_ord=c(1:cant)
	nuevo_RA=matrix(0, cant, 3)
	nuevo_correctos=c(1:cant)
	
	for(i in 1:cant){
		idc=buscar_mejor(indices, matts)
		nuevos_individuos[i,]=ind[idc, ]
		maes_ords[i,1]=i
		maes_ords[i,2]=maes[idc]
		matt_ord[i]=matts[idc]
		nuevo_RA[i, ]=RA[idc,]
		nuevo_correctos[i]=correctos[idc]
		indices[idc]=-1
	}
	
	print("maes_ords")
	print(maes_ords)
		
	ordenados=list()
	ordenados$individuos=nuevos_individuos
	ordenados$maes_ord=maes_ords
	ordenados$matts_ords=matt_ord
	ordenados$rocas=nuevo_RA
	ordenados$correctos=nuevo_correctos
	
	ordenados
}


buscar_mejor=function(indices, matt){
	largo=length(indices)
	mejor=0
	idc=0
	
	for(i in 1:largo){
		ind=indices[i]
		if(ind!=-1){
			if(matt[ind]>mejor){
				mejor=matt[ind]
				idc=i
			}
		}
	}
	
	idc
}



buscar_mayor_y_menor=function(grafico, auxiliar){
	mayor=grafico[1,2]
	menor=mayor
	largo=dim(grafico)[1]
	for(i in 2:largo){
		elem=grafico[i,2]
		if(elem>mayor)
			mayor=elem
		if(elem<menor)
			menor=elem
	}
	largo=dim(auxiliar)[1]
	for(i in 1:largo){
		elem=auxiliar[i, 2]
		if(elem>mayor)
			mayor=elem
		if(elem<menor)
			menor=elem
	}
	elems=list()
	elems$mayor=mayor
	elems$menor=menor
	elems
}

#construirModelo(datos)
construirModelo=function(datos, metodo){
	largo=dim(datos)[2]
	ultimo=names(datos)[largo]
	elemu=datos[1,largo]
	names(datos)[1]="V0"
	fmla=as.formula(paste(ultimo,"~."))
	
	if(metodo=="RF"){ #randomforest
		print("Random Forest")
		RF=make_Weka_classifier("weka/classifiers/trees/RandomForest")
		modelo=RF(formula=fmla, data=datos)
	}else{
		if(metodo=="RC"){ #randomcommittee
			print("Random Committee")
			RC=make_Weka_classifier("weka/classifiers/meta/RandomCommittee")
			modelo=RC(formula=fmla, data=datos)
		}else{#no se usa
			if(metodo=="RP"){ #reptree
				print("REPTree")
				RP=make_Weka_classifier("weka/classifiers/trees/REPTree")
				modelo=RP(formula=fmla, data=datos)
			}
		}				
	}
	modelo
}
