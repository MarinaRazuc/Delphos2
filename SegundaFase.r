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
	lay0=glayout(container=group200)
	stri=paste0(paste(paste0("Experiment ", expActual), "of "), nroExp)
	label0=glabel(stri, width=10)
	lay0[1:3, 25:30]=label0
	ggroup(container=frame1)
	
	#ggroup(container=frame1)
	glabel(" ", container=frame1)
	group0=ggroup(horizontal=TRUE, container=frame1, spacing=5) #max cant subconjs
	
	stringS=iconv("  Máxima cantidad de subconjuntos:", from="UTF-8", to="UTF-8")
	labelC=glabel(stringS, container=group0)
	editS=gedit("5", container=group0, width=4)
	glabel(" ", container=frame1)
	
	# grupoT=ggroup(container=frame1, horizontal=TRUE, spacing=5)
	# labelT=glabel("  Modelos a construir por subconjunto: ", container=grupoT)
	# editT=gedit("2", container=grupoT, width=4)
	# glabel(" ", container=frame1)
	
	# group9=ggroup(container=frame1, horizontal=TRUE, spacing=5)
	# labelI=glabel("  Cantidad de intentos por subconjunto:", container=group9)
	# editI=gedit("4", container=group9, width=4)
	# glabel(" ", container=frame1)
	
	group1=ggroup(horizontal = FALSE, container=frame1, spacing=10) #seleccionar metodo
	#glabel("  ", container=group1)
	group11=ggroup(horizontal=TRUE, container=group1, spacing=5)
	str2="  Seleccione método para la segunda fase: "
	str2=iconv(str2, from="UTF-8", to="UTF-8")
	label1=glabel(str2 , container=group11)
	# str3="(Se utilizará junto con Stacking de Weka) "
	# str3=iconv(str3, from="UTF-8", to="UTF-8")
	# label3=glabel(str3, container=group11)
	radio1 = gradio(c("RandomCommittee","RandomForest", "REPTree"), container=group1, 
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
	lay3=glayout(container=group3)
	boton3=gbutton("OK",
					handler=function(h,...){
						valor=svalue(radio1)
						if(valor=="RandomCommittee"){
							 metodoSF<<-"RC"
							 
						}else{
							if(valor=="RandomForest"){
								metodoSF<<-"RF"
							}else{
								metodoSF<<-"RP"
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
	scan3=scan(archivo, what="numeric")

	procesado=procesar(scan3, salida)
			
	soluciones=procesado$individuos
	interna=procesado$interna
	externa=procesado$externa
	maes_primera=procesado$maes
	
	print("dim maes")
	print(dim(maes_primera))
	
	cols=ncol(externa)
	clase=class(externa[,cols])
		
	iteras=nrow(soluciones)
	resultados=c(1:iteras)
	
	if(iteras>maxCant)
		iteras=maxCant
	
	g=1
	for(i in 1:iteras){
		
		#cargo maes en grafico
		#pri=(i-1)*10+1
		#ult=(i-1)*10+10
		ult=g+10
		colu=1
		while(colu<11){
			grafico[g,1]=i
			grafico[g,2]=maes_primera[i,colu]
			grafico[g,3]="P"
			colu=colu+1
			g=g+1
		}
		
		individuo=soluciones[i,]
		IF=filtrar(interna, individuo)
		EF=filtrar(externa, individuo) 
		write("---", salida, append=TRUE)
		write(individuo, salida, append=TRUE)
		
		modelo=construirModelo(IF ,metodoSF)
		
		
		if(clase=="numeric"){
			evalF2=evaluate_Weka_classifier(modelo, newdata=EF)
			corcoef=evalF2$details[1]
			mae=evalF2$details[2]
			grafico[g,1]=i
			grafico[g, 2]=mae
			grafico[g,3]="S"
			g=g+1
			# print("grafico")
			# print(grafico)
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
			write.table(matriz, salida ,append=TRUE)
			write("ROC Area: ", salida ,append=TRUE)
			write(rocarea, salida ,append=TRUE)
			
			resultados[i]=correctos/100
		}
			
		
		# grafico[i,1]=i
		# grafico[i,2]=mae
		if(i!=1){ #no es el primero
			 tryCatch(dev.off(), 
						 error=function(e){
											str1=iconv("No hay gráficos activos.", from="UTF-8", to="UTF-8")
											print(str1)
										 }
						 )
		}else{
			 names(grafico)=c("Subconjunto", "MAE", "Fase")
		}
		dev.flush()
		x11(width=55, height=50, title="Segunda Fase", xpos=18, ypos=15)
		print(boxplot(MAE~Subconjunto,  data=grafico,boxwex = 0.25, main = "MAEs de cada subconjunto, primera y segunda fase", xlab = "Subconjunto",ylab = "MAE", col="lightblue"))
		# x11(width=200, height=100, title="Segunda Fase");print(ggplot(grafico, aes(x=individuo, y=MAE))+ geom_point(aes(colour=MAE), size=4))
	}
	
	x11(width=55, height=50, title="Segunda Fase", xpos=702, ypos=15)
	print(ggplot(grafico, aes(x=Subconjunto, y=MAE)) + geom_point(aes(colour=Fase), size=3))

	write("---", salida, append=TRUE)
	completo=rbind(interna, externa)
	write.table(completo, salida, append=TRUE)
	
	write("---", salida, append=TRUE)
	write.table(grafico, salida, append=TRUE)
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
		RF=make_Weka_classifier("weka/classifiers/trees/RandomForest")
		modelo=RF(formula=fmla, data=datos)
	}else{
		if(metodo=="RC"){ #randomcommittee
			RC=make_Weka_classifier("weka/classifiers/meta/RandomCommittee")
			modelo=RC(formula=fmla, data=datos)
		}else{
			if(metodo=="RP"){ #reptree
				RP=make_Weka_classifier("weka/classifiers/trees/REPTree")
				modelo=RP(formula=fmla, data=datos)
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