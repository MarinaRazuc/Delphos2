
source("extras.R")
metodoSF="RF"

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
											if(svalue(text1)!="Ingrese archivo"){
												archivo_ent=svalue(text1)
												ventana_fase_dos(archivo_ent)
												dispose(win1)
											}else{
												gmessage("Ingrese un archivo para ejecutar la segunda fase.", icon="error")
											}
												
											}, width=5)  
					
	visible(win1)=TRUE
}


ventana_fase_dos=function(archivo){
	win1=gwindow(title="SecondPhase", visible=FALSE, width=300, height=100, parent=c(425,200))
	frame1=gframe(container=win1, text="Second Phase Settings", horizontal=FALSE, spacing=15, pos=0)
	
	group200=ggroup(horizontal=TRUE, container=frame1, spacing=15)#experiment i of n 
	stri=paste0(paste(paste0("Experiment ", expActual), "of "), nroExp)
	glabel("                                ", container=group200)
	glabel("                                ", container=group200)
	label0=glabel(stri, container=group200, width=10)
	ggroup(container=frame1)
	ggroup(container=frame1)
	
	#group0=ggroup(horizontal =FALSE, container=frame1, spacing=15) #label
	group1=ggroup(horizontal = FALSE, container=frame1, spacing=15) #seleccionar metodo
	#glabel("  ", container=group1)
	str2="Seleccione método para la segunda fase: "
	str2=iconv(str2, from="UTF-8", to="UTF-8")
	label1=glabel(str2 , container=group1)
	str3="(Se utilizará junto con Stacking de Weka)"
	str3=iconv(str3, from="UTF-8", to="UTF-8")
	label3=glabel(str3, container=group1)
	radio1 = gradio(c("RandomCommittee","RandomForest"), container=group1, 
				handler=function(h,...){
							valor=svalue(radio1)
							#print(valor)
							if(valor=="RandomCommittee"){
								 metodoSF<<-"RC"
							}else{
								 metodoSF<<-"RF"  
							}
					})
	group4=ggroup(container=frame1, spacing=15)
	glabel("  ", container=group4)
	group2=ggroup(horizontal=TRUE, container=frame1, spacing=15)#guardar como
	label2=glabel(" Guardar como... ", container=group2)
	texto2=gedit("SecondPhase.csv", container=group2)
	boton2=gbutton("Browse", container=group2,
					handler=function(h,...){
						file4=gfile("Guardar como...", type="save")
						if(is.na(file4)){
							svalue(texto2)="SecondPhase.csv"
						}else{
							svalue(texto2)=file4
						}
						print(file4)
					})
	group5=ggroup(container=frame1, spacing=15)
	glabel("   ", container=group5)
	group3=ggroup(horizontal=TRUE, container=frame1, spacing=15)
	glabel("                                         ", container=group3)
	boton3=gbutton("OK", container=group3,
					handler=function(h,...){
						valor=svalue(radio1)
						if(valor=="RandomCommittee"){
							 metodoSF<<-"RC"
							 
						}else{
							 metodoSF<<-"RF"
							  
						}
						segunda_fase(archivo, metodoSF, svalue(texto2))	
						dispose(win1)
					} )
	glabel("              ", container=group3)
													
	
	visible(win1)=TRUE

}

#segunda_fase(archivo_a_leer, folds_validacion)
segunda_fase=function(archivo, metodoSF, salida){ 
	#metodoSF RC o RF	
	print("Segunda Fase")
	
	scan3=scan(archivo, what="numeric")
#	str1=paste("poblaciones_", archivo)
	print("LLAMAR A PROCESAR")
	procesado=procesar(scan3, salida)
			
	soluciones=procesado$individuos
	interna=procesado$interna
	externa=procesado$externa
	
	#write.table(externa, "EXTERNA.csv", append=TRUE)
	
	cols=ncol(externa)
	clase=class(externa[,cols])
	iteras=nrow(soluciones)
	resultados=c(1:iteras)
	for(i in 1:iteras){
		individuo=soluciones[i,]
		IF=filtrar(interna, individuo)
		
		EF=filtrar(externa, individuo) 
		
		modelo=construirModelo(IF ,metodoSF)
		write(individuo, salida, append=TRUE)
		
		if(clase=="numeric"){
			evalF2=evaluate_Weka_classifier(modelo, newdata=EF)
			corcoef=evalF2$details[1]
			mae=evalF2$details[2]
			print("---------------------")
			str1=iconv("Coeficiente de Correlación", from="UTF-8", to="UTF-8")
			print(str1)
			print(corcoef)
			print("---------------------")
			print("Mean Absolute Error: ")
			print(mae)
			print("---------------------")
			write(str1, salida, append=TRUE)
			write(corcoef, salida, append=TRUE)
			write("Mean Absolute Error: ", salida, append=TRUE)
			write(mae, salida, append=TRUE)
			
			resultados[i]=as.numeric(corcoef)	
		}else{ 
			evalF2=evaluate_Weka_classifier(modelo, newdata=EF, class=TRUE)
			correctos=evalF2$details[1]
			mae=evalF2$details[5]
			matriz=evalF2$confusionMatrix
			rocarea=c()
			for(j in 1:3){
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
			str1=paste0("Porcentaje de Casos clasificados correctamente: ", correctos)
			write(str1, salida, append=TRUE)
			
			str1=paste0("Mean Absolute Error: ", mae)
			write(str1, salida, append=TRUE)
			
			write(str2, salida ,append=TRUE)
			write(matriz, salida ,append=TRUE)
			write("ROC Area: ", salida ,append=TRUE)
			write(rocarea, salida ,append=TRUE)
			
			resultados[i]=correctos/100
		}
					
		
	}
	
	write("---", salida, append=TRUE)
	completo=rbind(interna, externa)
	write.table(completo, salida, append=TRUE) 
}

#construirModelo(datos)
construirModelo=function(datos, metodo){
	# print("dim de datos")
	# print(dim(datos))
	largo=dim(datos)[2]
	# print(largo)
	ultimo=names(datos)[largo]
	# print("ultimo nombre")
	# print(ultimo)
	elemu=datos[1,largo]
	# print("ultimo elemento")
	# print(elemu)
	names(datos)[1]="V0"
	fmla=as.formula(paste(ultimo,"~."))
	
	
	if(metodo=="RF"){ #randomforest
		modelo=Stacking(formula=fmla, data=datos, 
						control=Weka_control(
									M="weka.classifiers.meta.RandomCommittee",
									X=10,
									S=2,
									B="weka.classifiers.trees.RandomForest", 
									num_slots=2
								)
						)
	}else{
		if(metodo=="RC"){ #randomcommittee
			modelo=Stacking(formula=fmla, data=datos, 
							control=Weka_control(
										M="weka.classifiers.trees.REPTree",
										X=10, 
										S=2,
										B="weka.classifiers.meta.RandomCommittee", 
										num_slots=2
									)
							)
		}else{
			if(metodo=="RP"){ #reptree
				modelo=Stacking(formula=fmla, data=datos, 
							control=Weka_control(
										M="weka.classifiers.trees.RandomCommittee",
										X=10, 
										S=2,
										B="weka.classifiers.meta.REPTree", 
										num_slots=2
									)
							)
			}
		}				
	}
	modelo
}

#ordenar(soluciones, valores_soluciones)
ordenar=function(sols, resus){

	cols=ncol(sols)
	filas=nrow(sols)
	ordenados=matrix(0,filas,cols )
	claves=matrix(0, filas, 2)
	
	indice_mayor=0
	
	for(j in 1:filas){
		mayor=0
		#me quedo con el mayor
		for(i in 1:filas){
			if(resus[i]>=0 && resus[i]>mayor){
				mayor=resus[i]
				indice_mayor=i
			}
		}
		resus[indice_mayor]=-1
		claves[j,1]=indice_mayor
		claves[j,2]=mayor
		ordenados[j,]=sols[indice_mayor, ]
	}
	
	
	resultado=list()
	resultado$individuos_ordenados=ordenados
	resultado$valores_ordenados=claves
	
	resultado
	

}