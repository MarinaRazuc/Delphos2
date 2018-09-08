
#obtener_nombres()
obtener_nombres=function(individuo, nombres){
	largo=length(individuo)
	resus=c()
	j=1
	for(i in 1:largo){
		if(individuo[i]==1){
				resus[j]=nombres[i]
				
				j=j+1
		}
	}
	
	resus
}


#tomo los descriptores del dataset según me indique el indicador (valga la redundancia)
#El indicador es de longitud igual a las columnas del dataset-1.
#Por cada 1 en el indicador, se toma la columna del dataset que corresponda
filtrar<-function( dataset, indicador ){
	largo<-length(indicador)
	iters<-c(1:largo)
	cols<-c()
	for(i in iters){
		#print(indicador[i])
		if(indicador[i]!=0){
			cols=c(cols,i)
		}
	}

	cols<-c(cols, largo+1)  #para que tome la columna correspondiente a la propiedad
	newdataset<-dataset[,cols]
	newdataset
}#FIN filtrar

#
#
#
calcular_error=function(modelo, datos){
	
	m2=nrow(datos)#cantidad de filas de datos
	frac=1/m2
	suma=0
	ncols=ncol(datos)
	#t <- proc.time() 
	for (i in 1:m2){
		#print("i es: ")
		#print(i)
		yi=datos[i,ncols] #valor de la propiedad para el compuesto i
		fin=ncols-1
		if(fin!=1)
			xi=datos[ i, 1:fin] #valores de los descriptores para el compuesto i
		else{#fin es 1
			if(dim(datos)[2]!=1){
				xi1=datos[i, 1:2]
				xi=xi1[1]
			}else{ #tiene solo una columna
				xi=data.frame(datos)
				names(xi)=c(names(datos))
			}
		}
		ypredict=predict(modelo, newdata=xi)   
		diferencia=yi-ypredict
		cuad=diferencia^2 #ver si funciona
		suma=suma+cuad
	}
	resultado=frac*suma
	resultado
}

#
#
#
imprimir_error=function(modelo, datos){#funcion para mi
	filas=nrow(datos)
	cols=ncol(datos)
	colsmenosuno=cols-1
	eme=proc.time()
	for(i in 1:filas){
		yi=datos[i, cols]
		xi=datos[i,1:colsmenosuno]
		ypredict=predict(modelo, newdata=xi, fitted=FALSE) #fitted->TRUE o FALSE
		error=abs(yi-unname(pred))
		print(error)
	}
	
	print(proc.time()-eme)
}

#partir
partir=function(datos, porcentaje, seed){# datos a ser partidos segun porcentaje porcentaje y semilla seed
	set.seed(seed) 
	indices=createDataPartition(datos$V1, p=porcentaje, list=FALSE) 
	externa_o_test=datos[-indices, ]
	interna_o_train=datos[indices, ]
	
	resultado=list()
	resultado$et=externa_o_test
	resultado$it=interna_o_train
	
	resultado

}
#
#generar_data_frame(valores_descriptores, valores_propiedad, nombres_descriptores, nombres_compuestos)
#
generar_data_frame=function(val, prop, nomD){#data frames
	win1=gwindow(visible=FALSE, title="Espere...", height=100, width=250, parent=c(550, 150))
	grupo1=ggroup(container=win1, horizontal=FALSE, spacing=10)
	glabel(" ", container=grupo1)
	glabel("  Loading data...  ", container=grupo1)
	glabel(" ", container=grupo1)
	barra=gprogressbar(container=grupo1)
	visible(win1)=TRUE
	
	if(length(nomD)>0){
		names(val)=nomD[,1]
	}
	svalue(barra)<-20

	val=filtrado_columnas(val) #filtrado data frame: elimina columnas constantes
	svalue(barra)<-50
	
	
	dataframe1=cbind(val,prop) #new data frame
	svalue(barra)<-60
	
	if(length(nomD)==0){
		svalue(barra)<-70
		ultimo=dim(dataframe1)[2] #el nro de la ultima columna
		svalue(barra)<-80
		nombreUlt=names(dataframe1)[ultimo-1]  #el nombre del ultimo descriptor (ultimo-1 porque ultimo es la propiedad, que se llama V1)
		svalue(barra)<-90
		names(dataframe1)[1]=paste0(nombreUlt, "1")##le agrego un 1 para diferenciarlo del ultimo descriptor, y asi tmp se llama V1 como la prop
		
	}
	svalue(barra)<-100
	dispose(win1)
	dataframe1
}
#
#filtrado_columnas
#
filtrado_columnas=function(A){
	
	ncols=ncol(A)
	nfils=nrow(A)
	columnas=c()  #para guardar las columnas que luego debo eliminar
	k=1
	#j=cols
	j=1

	while(j<=ncols){
		referencia=A[1,j] #fila 1 columna j
		contador=1
		distintos=FALSE
		i=2
		
		while(i<=nfils){
			elem=A[i,j]
			if(elem==0){
				contador=contador+1
			}
			if(elem!=referencia){
				distintos=TRUE
			}
			i=i+1
		}
		if((!distintos) ){ #debo eliminar la columna || (contador>=(.85*nfils))
			columnas[k]=j
			k=k+1
		}
		j=j+1
	}
	
	nuevoA=A
	
	h=k-1
	while(h>0){
		colnula=columnas[h]
		nuevoA=nuevoA[, -colnula]
		h=h-1
	}
	
	nuevoA
}


#
#
#acomodar(matriz_de_0s_y_1s)
acomodar=function(datos){
	filas=nrow(datos)
	cols=ncol(datos)
	
	banderai=FALSE
	banderaj=FALSE
	i=1
	j=1
	
	while(i<=filas && !banderai){
		while(j<=cols && !banderaj){
			elem=datos[i,j]
			if(elem!=0){
				banderaj=TRUE
				j=1
			}else{
				j=j+1
			}
		}
		if(banderaj){ #encontre un 1, puede haber mas individuos
			banderaj=FALSE
			i=i+1
		}else{ #no hay mas individuos
			banderai=TRUE
		}
	}
	
	if(banderai){
		k=filas
		while(k>=i){	
			datos=datos[-k,]
			k=k-1
		}
	}
	
	datos
}


#determina el nombre de los descriptores seleccionados segun los individuos
filtrar_nombres=function(nombres_desc, individuos){
	nombres=data.frame()
	largo=dim(individuos)[2]
	cant=dim(individuos)[1]
	
	for(i in 1:cant){
		for(j in 1:largo){
			if(individuos[i,j]==1){
				nombres[i,j]=nombres_desc[j]
			}else{
				nombres[i,j]=" "
			}	
		}
	}
	
	nombres
}




generar_particiones=function(ndfr4, pex, pin){
		set.seed(2) 
		indices4=createDataPartition(ndfr4$V1, p=pex, list=FALSE) #p por parametro (validacion externa)
		externa=ndfr4[-indices4, ]
		interna=ndfr4[indices4, ]
		
		indices2=createDataPartition(interna$V1, p=pin, list=FALSE) # p por parametro (validacion interna)
		entrenamiento=interna[indices2, ]
		testeo=interna[-indices2, ]
	
		resultados=list()
		
		resultados$externa=externa
		resultados$interna=interna
		resultados$entrenamiento=entrenamiento
		resultados$testeo=testeo
		
		resultados
}	





generar_pop=function(nro, cant, maximo){
	m=proc.time()
	p1=generar_poblacion_inicial(nro, cant, maximo)
	print(proc.time()-m)
	
	p1
}