source("extras.R")


mostrar_resultados=function(archivo){
	load(archivo)
	individuos=resultados
	cant_desc=length(individuos[1,]) #-1?
	nombres_desc=names(completo)
	valores=completo
	coefis=corr_coefs
	maes=maes_segundo
	porcentaje=correctos
	rocarea=ROCareaS
	todos_maes=grafico 
	this_confu=confusion
	
	# print("MATRIZ")
	# print(confusion)
	
	largo=dim(valores)[2]
	elem=valores[1,largo]
	clase=class(elem)
	print("maes")
	print(maes)
	if(clase=="numeric"){
		mostrar_numericos(individuos, nombres_desc, coefis, maes, valores, todos_maes)
	}else{
		mostrar_nominales(individuos, nombres_desc, porcentaje, maes, rocarea, valores, todos_maes, this_confu)
	}
	
}

mostrar_numericos=function(individuos, nombres_desc, coefs, maes, valores, todos){

	cant=dim(individuos)[1] #filas
	cols=dim(individuos)[2]
	seleccionados=filtrar_nombres(nombres_desc, individuos)
	
	names(valores)=nombres_desc
	maxj=length(maes)
	
	for(j in 1:maxj){
		h=as.numeric(maes[j])
		maes[j]=signif(h, digits=6)
	}
	
	win1=gwindow(title="Resultados", visible=FALSE, width=500, height=200, parent=c(200,50))
	grupomayor=ggroup(horizontal=FALSE, spacing=7, container=win1, heigth=100, width=200)
	glabel(" ", container=grupomayor)
	grupo0=ggroup(horizontal = TRUE, spacing = 10,   container = grupomayor)
	
	etiq=paste(paste(paste0(" Individuos seleccionados: ( ", cols), "descriptores"),")")
	glabel(etiq, container=grupo0)
	
	lay=glayout(container=grupomayor)
	grupo1=ggroup(horizontal = FALSE, spacing = 5, width=500, height=200, use.scrollwindow = TRUE)
	names(individuos)=nombres_desc
	glabel(" ", container=grupo1)
	
	for(i in 1:cant){
		str1=paste0(" ", i)
		str1=paste(str1," ")
		for (j in 1:cols){
			str1=paste0(str1,individuos[i,j])
		}
		str1=paste(str1, " ")
		glabel(str1, container=grupo1)
	}
	lay[1:15, 1:100]=grupo1
	
	grupo4=ggroup(horizontal=TRUE, spacing=10, container=grupomayor)
	glabel(" Descriptores elegidos por individuo: ", container=grupo4)
	
	lay2=glayout(container=grupomayor)
	grupo2=ggroup(horizontal=FALSE, spacing=5, width=500, height=200, use.scrollwindow = TRUE)
	glabel(" ", container=grupo2)
	
	num=length(individuos[1,])
	indices="	   "
	for(i in 1:num){
		indices=paste(paste0(indices, i), "		 ")
	}
	#gtemp=ggroup(container=grupo2, horizontal=TRUE)
	#glabel(indices, container=gtemp)
	
	card=c()
	for(i in 1:cant){
		str2=paste0(" ", i)
		str2=paste(str2," ")
		
		maxcols=card[i]=cardinalidad(individuos[i,])
		
		gtemp=ggroup(container=grupo2, horizontal=TRUE)
		for(j in 1:num){
			str2=paste(str2, paste("", paste0(j, paste(")", paste(seleccionados[i,j], "  ")))))
			
		}
		str2=paste(str2, "  ")
		#print(str2)
		glabel(str2, container=gtemp)
	}
	lay2[1:15, 1:100]=grupo2
	
	grupo5=ggroup(container=grupomayor, horizontal=FALSE, spacing=5)
	glabel(" ", container=grupo5)
	grupo3=ggroup(container=grupo5, horizontal=TRUE, spacing=20) #botones
	glabel(" ", container=grupo3)
	
	lay3=glayout(container=grupo3)
	
	botonmae=gbutton(" Ver MAE ",
					handler=function(h,...){
						ventana_mae(todos, maes)
					}, width=15)
	botoncoef=gbutton(" Ver Coef.Corr. ", handler=function(h, ...){
											ventana_coef(coefs)
										})
	botoncard=gbutton(" Ver Cardinalidad ", handler=function(h, ...){
											ventana_card(card)
										})
	 
	
	botonFilt=gbutton("  Filtrar  ",  
					handler=function(h,...){
						ventana_filtrado(individuos, valores)
					}, width=15)
	botonSal=gbutton("  Salir  ",  
					handler=function(h,...){
						dispose(win1)
					}, width=15)
	lay3[1:3, 1:5]=botonmae
	lay3[1:3, 8:13]=botoncoef
	lay3[1:3, 16:21]=botoncard
	
	lay3[1:3, 24:29]=botonFilt
	lay3[1:3, 70:75]=botonSal
	glabel(" ", container=grupo5)
	
	visible(win1)=TRUE	
}

#
mostrar_nominales=function(individuos, nombres_desc, porcentaje, maes, rocarea, valores, todos, confusion){
	cant=dim(individuos)[1] #filas
	cols=dim(individuos)[2]
	seleccionados=filtrar_nombres(nombres_desc, individuos)
	names(valores)=nombres_desc
	
	maxj=length(maes)
	for(j in 1:maxj){
		h=as.numeric(maes[j])
		maes[j]=signif(h, digits=6)
	}
	
	win1=gwindow(title="Resultados", visible=FALSE, width=500, height=200, parent=c(200,50))
	grupomayor=ggroup(horizontal=FALSE, spacing=7, container=win1, heigth=100, width=200)
	glabel(" ", container=grupomayor)
	grupo0=ggroup(horizontal = TRUE, spacing = 10,   container = grupomayor)
	
	etiq=paste(paste(paste0(" Individuos seleccionados: ( ", cols), "descriptores"),")")
	glabel(etiq, container=grupo0)
		
	lay=glayout(container=grupomayor)
	grupo1=ggroup(horizontal = FALSE, spacing = 5, width=500, height=200, use.scrollwindow = TRUE)
	names(individuos)=nombres_desc
	glabel(" ", container=grupo1)
	
	for(i in 1:cant){
		str1=paste0(" ", i)
		str1=paste(str1," ")
		for (j in 1:cols){
			str1=paste0(str1,individuos[i,j])
		}
		str1=paste(str1, " ")
		glabel(str1, container=grupo1)
	}
	lay[1:15, 1:100]=grupo1
		
	grupo4=ggroup(horizontal=TRUE, spacing=10, container=grupomayor)
	glabel(" Descriptores elegidos por individuo: ", container=grupo4)
	
	lay2=glayout(container=grupomayor)
	grupo2=ggroup(horizontal=FALSE, spacing=5, width=500, height=200, use.scrollwindow = TRUE)
	glabel(" ", container=grupo2)
	#+++++++++++++++++++++++++
	num=length(individuos[1,])
	card=c()
	for(i in 1:cant){
		str2=paste0(" ", i)
		str2=paste(str2," ")
		
		maxcols=card[i]=cardinalidad(individuos[i,])
		gtemp=ggroup(container=grupo2, horizontal=TRUE)
		for(j in 1:num){
			str2=paste(str2, paste("", paste0(j, paste(")", paste(seleccionados[i,j], "  ")))))
		}
		str2=paste(str2, " ")
		glabel(str2, container=gtemp)
	}
	lay2[1:15, 1:100]=grupo2
		
	grupo5=ggroup(container=grupomayor, horizontal=FALSE, spacing=5)
	glabel(" ", container=grupo5)
	grupo3=ggroup(container=grupo5, horizontal=TRUE, spacing=20) #botones
	glabel(" ", container=grupo3)
	
	lay3=glayout(container=grupo3)
	
	botonmae=gbutton(" Ver MAE ",
					handler=function(h,...){
						ventana_mae(todos, maes)
					})
	botonrocarea=gbutton(" Ver ROC Area ", handler=function(h, ...){
											ventana_rocarea(rocarea)
										})
	str9=iconv(" Ver Coef. Matthews ", from="UTF-8", to="UTF-8")
	botonconfusion=gbutton(str9, handler=function(h,...){
											ventana_matts(matts, cant)
											 #ventana_confusion(confusion, cant)
										 })
	botoncasos=gbutton(" Ver Porcentaje Correctos ", handler=function(h, ...){
											ventana_casos(porcentaje)
										})									
	botoncard=gbutton(" Ver Cardinalidad ", handler=function(h, ...){
											ventana_card(card)
										})
	 
	botonFilt=gbutton("  Filtrar  ",  
					handler=function(h,...){
						ventana_filtrado(individuos, valores)
					})
	botonSal=gbutton("  Salir  ",  
					handler=function(h,...){
						dispose(win1)
					})
	lay3[1:3, 1:5]=botonmae
	lay3[1:3, 8:13]=botonrocarea
	lay3[1:3, 16:21]=botonconfusion
	lay3[1:3, 24:29]=botoncasos
	lay3[1:3, 32:37]=botoncard
	lay3[1:3, 40:45]=botonFilt
	lay3[1:3, 60:65]=botonSal
	glabel(" ", container=grupo5)
	visible(win1)=TRUE	
}

ventana_card=function(cardis){
	datos=data.frame()
	cant=length(cardis)
	for(i in 1:cant){
		datos[i,1]=i
		datos[i,2]=cardis[i]
	}
	names(datos)=c("Subconjuntos", "Cardinalidad")
	x11(width=2000, height=1000, title="Cardinalidad de los subconjuntos");
	#print(plot(datos, colour="purple"))
	print(ggplot(datos, aes(x=Subconjuntos, y=Cardinalidad))   + geom_point(aes(colour=Cardinalidad), size=4, color="purple"))
}
ventana_matts=function(matts, cant){
	datos=data.frame()
	for(i in 1:cant){
		datos[i, 1]=i
		datos[i, 2]=matts[i]
	}
	
	names(datos)=c("Subconjuntos", "Coef.Matthews")
	x11(width=2000, height=1000, title="Coeficiente de Matthews de los subconjuntos");
	print(ggplot(datos, aes(x=Subconjuntos, y=Coef.Matthews))   + geom_point(aes(colour=Coef.Matthews), size=4, color="purple"))
}

ventana_confusion=function(confusion, cant){
	str1=iconv("Matrices de Confusión", from="UTF-8", to="UTF-8")
	win1=gwindow(title=str1, visible=FALSE, width=500, height=200, parent=c(200,50))
	grupo1=grupomayor=ggroup(horizontal=FALSE, spacing=7, container=win1, heigth=100, width=200, use.scrollwindow = TRUE)
	lay1=glayout(container=grupo1)
	
	print("matriz de confusion")
	print(confusion)
	print("cant")
	print(cant)
	nombres=names(confusion)
	print("nombres")
	print(nombres)
	cantnoms=length(nombres)
	strTitulo="      "
	for(i in 1:cantnoms){
		strTitulo=paste(paste(strTitulo, nombres[i]), " ")
	}
	
	fila=1
	ubif=2
	for (i in 1:cant){ #para armar todas las tablas
		ind=iconv(paste0("Individuo nº ", i), from="UTF-8", to="UTF-8")
		lay1[ubif, 1]=glabel(ind)
		ubif=ubif+1
		lay1[ubif, 1]=glabel(strTitulo)
		ubif=ubif+1
		for(j in 1:cantnoms){
			temp1=paste(nombres[j]," ")
			for(k in 1:cantnoms){
				temp1=paste(paste(temp1,confusion[fila, k])," ")
			}
			lay1[ubif,1]=temp1
			ubif=ubif+1
			fila=fila+1
		}
		lay1[ubif, 1]=glabel(" ----------------- ")
		ubif=ubif+1
	}
	#matriz=glabel(confusion)
	#lay1[3, 1]=matriz
	
	visible(win1)=TRUE

}
#
ventana_coef=function(coefis){
	nuevo=data.frame()
	cant=length(coefis)
	for(i in 1:cant){
		nuevo[i,1]=i
		nuevo[i,2]=coefis[i]
	}
	
	str1=iconv("Coeficientes de Correlación", from="UTF-8", to="UTF-8")
	names(nuevo)=c("subconjuntos", "Coef_Corr")
	
	x11(width=2000, height=1000, title=str1);
	print(ggplot(nuevo, aes(x=subconjuntos, y=Coef_Corr))   + geom_point(aes(colour=Coef_Corr), size=4))
	 
}

ventana_mae=function(todos, maes){
	nuevomae=data.frame()
	cant=length(maes)
	for(i in 1:cant){
		nuevomae[i,1]=i
		nuevomae[i,2]=maes[i]
	}
	names(todos)=c("subconjunto", "MAE")
	names(nuevomae)=c("subconjunto", "MAE")
	x11(width=80, height=50, title="MAE")
	valores=buscarMayorMenor(todos, maes)
	mayor=valores$mayor
	menor=valores$menor
	
	boxplot(MAE~subconjunto,  data=todos, boxwex = 0.25, xlab = "Subconjunto",ylab = "MAE", col="lightblue", xlim=c(0, length(nuevomae[,1])+1), ylim=c(menor,mayor+0.02))
	par(new=TRUE)
	plot(nuevomae, axes=FALSE, col="red", type="p", xlim=c(0, length(nuevomae[,1])+1), ylim=c(menor,mayor+0.02), main="MAE - Primera y Segunda Fase")
	legend(x=length(nuevomae[,1])+0.5, y=mayor-0.02, legend="Fase Dos", col="red", text.width=0.4, pch="o")
}

buscarMayorMenor=function(pri, seg){
	mayor=0
	menor=1000
	largo=length(pri[,2])
	for(i in 1:largo){
		elem=pri[i,2]
		if(elem>mayor){
			mayor=elem
		}
		if(elem<menor){
			menor=elem
		}
	}
	
	largo=length(seg)
	for(j in 1:largo){
		elem=seg[j]
		if(elem>mayor)
			mayor=elem
		if(elem<menor)
			menor=elem
	}
	
	valores=list()
	valores$mayor=mayor
	valores$menor=menor
	
	valores
}


ventana_rocarea=function(rocarea){
	nuevo=data.frame()
	dims=dim(rocarea)
	filas=dims[1]
	cols=dims[2]
	maxI=filas*cols 
	i=1
	k=1
	while(k <= maxI){
		for(j in 1:cols){
			nuevo[k, 1]=i
			nuevo[k, 2]=rocarea[i,j]
			k=k+1
		}
		i=i+1
	}
	
	# print(rocarea)
	print(nuevo)
	names(nuevo)=c("subconjuntos", "ROC_Area")
	x11(width=2000, height=1000, title="ROC Area");
	print(ggplot(nuevo, aes(x=subconjuntos, y=ROC_Area))   + geom_point(aes(colour=subconjuntos), size=4))
	
	#x11(width=2000, height=1000, title="ROC Area");
	#print(ggplot(nuevo, aes(x=ROC_Area, y=subconjuntos))   + geom_point(aes(colour=subconjuntos), size=4))

}

ventana_casos=function(porc){
	nuevo=data.frame()
	porc=round(as.numeric(porc))
	largo=length(porc)
	for(i in 1:largo){
		nuevo[i,1]=i
		nuevo[i,2]=porc[i]
	}
	names(nuevo)=c("subconjuntos", "Porc_Casos_Correctos")
	print(nuevo)
	x11(width=2000, height=1000, title="Porcentaje de casos correctamente clasificados");
	print(ggplot(nuevo, aes(x=subconjuntos, y=Porc_Casos_Correctos))   + geom_point(aes(colour=Porc_Casos_Correctos), size=4))
}

#
ventana_filtrado=function(individuos, valores){
	win1=gwindow(title = "Filtrar", visible=FALSE, width=300, height=120, parent=c(575,150))
	grupomayor=ggroup(horizontal=FALSE, spacing=10, container=win1)
	grupo1=ggroup(horizontal=FALSE, spaacing=10, container=grupomayor)
	glabel(" ", container=grupo1)
	lay1=glayout(container=grupo1)
	label1=glabel("   Seleccionar subconjunto: ")
	cant=dim(individuos)[1]
	print(cant)
	combobox1=gcombobox(c(1:cant), selected=1, editable=FALSE)
	lay1[1:3, 2:12]=label1
	lay1[1:3, 14:19]=combobox1
	grupo2=ggroup(horizontal=TRUE, spacing=10, container=grupomayor)
	#lay2=glayout(container=grupo2)
	glabel(" ", container=grupo2)
	label2=glabel(" Guardar como: (archivo csv)", container=grupo2)
	edit2=gedit("Datos_filtrados.csv", container=grupo2)
	boton2=gbutton("Browse", container=grupo2,
					handler=function(h,...){
						file4=gfile("Guardar como...", type="save")
						if(length(file4)!=0){
							if(is.na(file4)){
								svalue(edit2)="Datos_filtrados.csv"
							}else{
								svalue(edit2)=file4
							}
							print(file4)
						}
					})	
	glabel(" ", container=grupo2)
	
	grupo3=ggroup(horizontal=FALSE, container=grupomayor)
	glabel(" ",container=grupo3)
	lay3=glayout(container=grupo3)
	botonok=gbutton(" OK ", handler=function(h,...){
						filtrar2(valores, individuos[as.numeric(svalue(combobox1)), ], svalue(edit2))
						gmessage("Filtrado finalizado.", icon="info")
					})
	botonCan=gbutton(" Cancelar ",
					handler=function(h,...){
						dispose(win1)
					})
	lay3[1:3, 8:13]=botonok
	lay3[1:3, 30:35]=botonCan
	glabel(" ", container=grupo3)
	
	visible(win1)=TRUE
	
}


filtrar2=function(valores, individuo, archivo){
	# print("individuo")
	# print(individuo)
	largo<-length(individuo)
	iters<-c(1:largo)
	cols<-c()
	for(i in iters){
		if(individuo[i]!=0){
			cols=c(cols,i)
		}
	}
	newdataset<-valores[,cols]
	write.table(newdataset, archivo)
}