library("gridExtra")
library("grid")

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

	#print(cols)
	
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
		#predict(modelo, newdata)
		#print("Xi"); print(xi)
		ypredict=predict(modelo, newdata=xi)   
		#print(ypredict)
		diferencia=yi-ypredict
		cuad=diferencia^2 #ver si funciona
		suma=suma+cuad
	}
	#print(proc.time()-t)
	#print("----------------------------------")

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

#		pred=predict(modelo, xi)

		#speedglm
		#aparente// response mas rapido q link
		#pred=predict(modelo, newdata=xi, type="response") #type link o response
		#speedlm
		ypredict=predict(modelo, newdata=xi, fitted=FALSE) #fitted->TRUE o FALSE
		#weka
		#pred=predict(modelo, newdata=xi)
		#pred=predict(modelo, newdata=xi, type="probability") #type->class o probability #punto de demora 

		error=abs(yi-unname(pred))
		#print(pred)
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
	
	if(length(nomD)>0){
		names(val)=nomD[,1]
	}
	val=filtrado_columnas(val) #filtrado data frame: elimina columnas constantes
	dataframe1=cbind(val,prop) #new data frame
	if(length(nomD)==0){
		ultimo=dim(dataframe1)[2] #el nro de la ultima columna
		nombreUlt=names(dataframe1)[ultimo-1]  #el nombre del ultimo descriptor (ultimo-1 porque ultimo es la propiedad, que se llama V1)
		names(dataframe1)[1]=paste0(nombreUlt, "1")##le agrego un 1 para diferenciarlo del ultimo descriptor, y asi tmp se llama V1 como la prop
	}
	#print("Generé dataframe")
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
			#print(elem)
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
		#print(colnula)
		nuevoA=nuevoA[, -colnula]
		#nuevoA[,colnula]=NULL
		h=h-1
	}
	
	nuevoA
}



####
###
## procesar archivo de la primera fase
#procesar(archivo_leido, archivo_de_salida)
procesar=function(scan2, salida){ #salida para escribir las poblaciones alli
	print("Procesando archivo...")
	nombres=c()
	nombres_desc=c()
	
	largo=length(scan2)#todos los caracteres digamos
	bandera=FALSE
	i=1
	while(i<largo && !bandera){
		elem=scan2[i]
		pos=grep("V", elem)
		# print("I")
		# print(i)
		if(length(pos)!=0){ #es un nombre V*
			nombres=c(nombres, elem)
		}else{#llegue a las poblaciones
			bandera=TRUE
		}
		i=i+1
	}
	
	#aca debería leer las poblaciones

	banderaSEP=FALSE
	poblacion=data.frame()
	colus=length(nombres)
	fils=1
	while(!banderaSEP){
		#arranca una nueva poblacion
		cols=1
		
		while(cols<=colus){
			# print("fila, columna")
			# print(fils)
			
			elem=as.numeric(scan2[i])
			# print("elemento")
			# print(elem)
			poblacion[fils, cols]=elem
			cols=cols+1
			i=i+1
		}#finalice un individuo
		elem=scan2[i] #puede ser numeracion o -
		#print("elemento nuevo")
		#print(elem)
		pos=grep("-", elem)
		#print(pos)
		if(length(pos)!=0){ #es el separador
			
			banderaSEP=TRUE
			
			#banderaPOP=TRUE
			write.table(poblacion, salida, append=TRUE)
		}else{
			pos=grep("V", elem)
			
			if(length(pos)!=0){ #comienza una nueva poblacion
				write.table(poblacion, salida, append=TRUE)
				write("---", salida, append=TRUE)
				#banderaPOP=TRUE
				#debo saltear nombres y primera numeracion
				i=i+colus+1
				fils=1
				cols=1
			}else{#comienza un nuevo individuo
				fils=fils+1
				i=i+1#salteo numeracion
				cols=1
			}
		}
		#}
	}
	i=i+colus+2
	
	individuos=matrix(0, 20, length(nombres))#ver
	if(bandera){#ahora busco los individuos
		j=1
		k=1
		limitador=2 
		bandera=FALSE
		#i=i+1 #descarto el primer 1 que es de numeración
		while(i<largo && !bandera){
			elem=as.numeric(scan2[i])
			# print("I")
			# print(i)
			if(is.na(elem)){ #llegamos a algo q no es numero
				# print("ISNAELEM")
				bandera=TRUE
			}else{
				# print("limitador")
				# print(limitador)
				pos=grep(limitador, elem)
				# print("pos")
				# print(pos)
				if(length(pos)==0){#sigo en el individuo 
					individuos[j,k]=elem
					k=k+1
				}else{ #termine con el individuo
					j=j+1
					k=1
					limitador=limitador+1
				}
			}
			i=i+1
		}
		
		if(bandera){
			bandera=FALSE
			cant=length(individuos[1,])+1 #+1 por la propiedad
			h=1
			# print("cant")
			# print(cant)
			# print("h")
			# print(h)
			# print(h<=cant)
			while(h<=cant){
				# print("H")
				# print(h)
				nombres_desc=c(nombres_desc, scan2[i])
				i=i+1
				h=h+1
			}
			#valores=matrix(0,500,length(nombres)) #ver
			#valores=matrix()
			valores=data.frame()
			valor_propiedad=c()
			i=i+1 #para saltear la numeracion
			#print("i es:")
			
			h=1
			j=1
			k=1
			bandera=FALSE
			nominal=FALSE
			while(i<largo && !bandera){
				while(h<=cant){
					# print("j, k")
					# print(j) 
					# print("I")
					# print(i)
					
					if(h==cant){
						elem=as.numeric(scan2[i])
						if(is.na(elem)){
							elem=scan2[i]
							nominal=TRUE
						}
						valor_propiedad=c(valor_propiedad, elem)
					}else{
						elem=as.numeric(scan2[i])
						valores[j,k]=elem
					}
					h=h+1
					k=k+1
					i=i+1
				}
				elem=scan2[i]
				pos=grep("-", elem)
				if(length(pos)!=0){ #termine con interna
					bandera=TRUE
				}else{
					j=j+1
					h=1
					k=1
					i=i+1 #para saltear la numeracion
				}
			}
			if(nominal)
				valor_propiedad=as.factor(valor_propiedad)
			valores=cbind(valores, valor_propiedad)
			###########################
			if(bandera){#debo leer externa
				i=i+cant+2 #salteo nombres, separadores y primera numeracion
				
				externa=data.frame()
				valor_prop=c()
				j=1
				k=1
				h=1
				bandera=FALSE
				
				while(i<=largo && !bandera){
					# print("I")
					# print(i)
					while(h<=cant){
						elem=as.numeric(scan2[i])
						if(h==cant){
							if(is.na(elem)){
								elem=scan2[i]
							}
							valor_prop=c(valor_prop, elem)
						}else{
							externa[j,k]=elem
						}
						k=k+1
						i=i+1
						h=h+1
					}
					elem=scan2[i]
					pos=grep("-", elem)
					if(length(pos)!=0){
						bandera=TRUE
					}else{
						h=1
						k=1
						i=i+1
						j=j+1
					}
				}
				if(nominal)
					valor_prop=as.factor(valor_prop)
				externa=cbind(externa, valor_prop)
			}
		}
	}
	
	individuos=acomodar(individuos)
	names(valores)=nombres_desc
	names(externa)=nombres_desc
		
	
	resultados=list()
	resultados$nombres=nombres #no se si es necesario
	resultados$individuos=individuos
	resultados$nombres_desc=nombres_desc
	resultados$interna=valores
	resultados$externa=externa
	resultados
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

#
#
#quiero devolver una matriz con los individuos que encuentro
buscar_individuos=function(scan1){
	i=1
	largo=0
	bandera=FALSE
	individuos=data.frame()
	I_inicial=0
	
	while(!bandera){
		elem=scan1[i]
		pos=grep("V", elem)
		if(length(pos)!=0){ #es un nombre V*
			largo=largo+1
			i=i+1
		}else{#llegue a las poblaciones
			bandera=TRUE
		}
	}
	
	# print("ELEM")
	# print(scan1[i])
	
	fin=FALSE
	popus=0
	if(bandera){
		#tengo que saltear las poblaciones
		bandera=FALSE
		while(!fin){
			popus=popus+1
			while(!bandera){
				i=i+largo+1 #creo
				elem=scan1[i]
						
				pos=grep("-",elem)
				if(length(pos)!=0){ #se termino la poblacion
					
					bandera=TRUE
				}
			}
			if(bandera){
				elem=scan1[i+1]
				# print("ELEM")
				# print(elem)
				pos=grep("V",elem)
				
				if(length(pos)!=0){#debo saltear la poblacion
					#print("debo saltear la poblacion")
					bandera=FALSE
					i=i+largo+1
					
				}else{#encontre el primer individuo
					#print("encontre el primer individuo")
					i=i+1
					fin=TRUE
				}
			}
		}
		I_inicial=i
		#busco el primer individuo
			
		for(k in 1:popus){
			for(j in 1:largo){
				individuos[k,j]=as.numeric(scan1[i])
				i=i+1
			}
			#saltear hasta la marca ---
			saltee=FALSE
			while(!saltee){
				elem=scan1[i]
				pos=grep("-",elem)
				if(length(pos)!=0){ #la encontre
					saltee=TRUE
					i=i+1
				}else{
					i=i+1
				}
			}
		}
	}
	resultado=list()
	resultado$largo=largo
	resultado$indis=individuos
	resultado$I=I_inicial
	resultado
}

#quiero devolver un vector con los nombres de los descriptores
nombres_descriptores=function(scan1, cant, ipobl){
	nombres=c()
	i=ipobl #parado en el primer individuo
	j=1
	porcentaje=c()
	bandera=FALSE
	mae=c()
	coefi=c()
	rocarea=data.frame()
	r=1
	p=1
	
	elem=scan1[i+cant]
	pos=grep("P", elem)
	if(length(pos)!=0){ #es Porcentaje
		clase="nom"
	}else{
		clase="num"
	}
	
	while(!bandera){ #hasta que no saltee todos los individuos
		p=1
		i=i+cant #me paro en coef o porc
		
		
		if(clase=="nom"){
			#print("clase es nom")
			elem=as.numeric(scan1[i])
			#print(elem)
			#print(is.na(elem))
			while(is.na(as.numeric(scan1[i]))){
				i=i+1
			}
			porcentaje[j]=scan1[i]
			i=i+1
			while(is.na(as.numeric(scan1[i]))){
				i=i+1
			}
			mae[j]=scan1[i]
			i=i+1
			while(length(grep("ROC", scan1[i]))==0){
				i=i+1
			}
			i=i+2
			elem=scan1[i]
			while(length(grep("-", elem))==0){
				rocarea[r,p]=elem
				p=p+1
				i=i+1
				elem=scan1[i]
			}
			r=r+1
		}else{#numerico
			#print("ES NUMERICO")
			#print("elem num")
			#print(scan1[i])
			while(is.na(as.numeric(scan1[i]))){
				i=i+1
			}
			#print(scan1[i])
			coefi[j]=scan1[i]
			i=i+1
			#print("siguiente")
			#print(scan1[i])
			while(is.na(as.numeric(scan1[i]))){
				i=i+1
			}
			#print(scan1[i])
			mae[j]=scan1[i]
			i=i+1
		}
		i=i+1
		#print(scan1[i])
		if(is.na(as.numeric(scan1[i]))){#llegue a los nombres
			#♣print("llegue a los nombres")
			bandera=TRUE
		}else{
			j=j+1
		}
	}
	
	#a juntar nombres
	for(h in 1:cant){
		elem=scan1[i]
		nombres=c(nombres,elem)
		i=i+1
	}
	#a juntar los valores
	valores=data.frame()
	#estoy parada en el 1 de la numeracion
	e=1
	largo=length(scan1)
	#print(cant)
	#print(scan1[i])
	i=i+2
	
	while(i<=largo){
		for(d in 1:cant){
			valores[e,d]=as.numeric(scan1[i])
			i=i+1
		}
		
		i=i+2
		e=e+1
	}
	
	
	resultados=list()
	resultados$porcentaje=porcentaje
	resultados$mae=mae
	resultados$coefi=coefi
	resultados$rocarea=rocarea
	resultados$nombres=nombres
	resultados$valores=valores
	
	resultados
}

##
##
##
mostrar_resultados=function(archivo){

	scan1=scan(archivo, what="numeric")
	#procesado=procesar_SF(scan1)
	proceso1=buscar_individuos(scan1)
	individuos=proceso1$indis
	cant_desc=proceso1$largo
	ipobl=proceso1$I
	
	proceso2=nombres_descriptores(scan1, cant_desc, ipobl)
	nombres_desc=proceso2$nombres
	valores=proceso2$valores

	coefis=proceso2$coefi
	maes=proceso2$mae
	porcentaje=	proceso2$porcentaje
	rocarea=proceso2$rocarea
	
	largo=dim(valores)[2]
	elem=valores[,largo]
	clase=class(elem)
	
	if(clase=="numeric"){
		mostrar_numericos(individuos, nombres_desc, coefis, maes)
	}else{
		mostrar_nominales(porcentaje, maes, rocarea)
	}
	
	
}

mostrar_numericos=function(individuos, nombres_desc, coefs, maes){

	cant=dim(individuos)[1] #filas
	cols=dim(individuos)[2]
	seleccionados=filtrar_nombres(nombres_desc, individuos)
	nombres(valores)=nombres_desc
	
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
	
	card=c()
	for(i in 1:cant){
		str2=paste0(" ", i)
		str2=paste(str2," ")
		
		maxcols=card[i]=cardinalidad(individuos[i,])
		gtemp=ggroup(container=grupo2, horizontal=TRUE)
		for(j in 1:maxcols){
			str2=paste(str2, seleccionados[i,j])
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
						ventana_mae(maes)
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
	lay3[1:3, 80:85]=botonSal
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

ventana_mae=function(maes){
	nuevomae=data.frame()
	cant=length(maes)
	for(i in 1:cant){
		nuevomae[i,1]=i
		nuevomae[i,2]=maes[i]
	}
	names(nuevomae)=c("subconjuntos", "MAE")
	
	x11(width=2000, height=1000, title="MAE");
	#print(plot(nuevomae))
	print(ggplot(nuevomae, aes(x=subconjuntos, y=MAE))   + geom_point(aes(colour=MAE), size=4))
	
	#geom_line(aes(colour=MAE, group=MAE))
	
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
	# print(nuevo)
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
	cant=length(individuos)
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
						if(is.na(file4)){
							svalue(edit2)="Datos_filtrados.csv"
						}else{
							svalue(edit2)=file4
						}
						print(file4)
					})	
	glabel(" ", container=grupo2)
	
	grupo3=ggroup(horizontal=FALSE, container=grupomayor)
	glabel(" ",container=grupo3)
	lay3=glayout(container=grupo3)
	botonok=gbutton(" OK ", handler=function(h,...){
						filtrar2(valores, individuos[as.numeric(svalue(combobox1))], svalue(edit2))
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

#
mostrar_nominales=function(individuos, nombres_desc, valores, porcentaje, maes, rocarea){
	cant=dim(individuos)[1] #filas
	cols=dim(individuos)[2]
	seleccionados=filtrar_nombres(nombres_desc, individuos)
	nombres(valores)=nombres_desc
	
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
	
	card=c()
	for(i in 1:cant){
		str2=paste0(" ", i)
		str2=paste(str2," ")
		
		maxcols=card[i]=cardinalidad(individuos[i,])
		gtemp=ggroup(container=grupo2, horizontal=TRUE)
		for(j in 1:maxcols){
			str2=paste(str2, seleccionados[i,j])
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
						ventana_mae(maes)
					}, width=15)
	botonrocarea=gbutton(" Ver ROC Area ", handler=function(h, ...){
											ventana_rocarea(rocarea)
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
					}, width=15)
	botonSal=gbutton("  Salir  ",  
					handler=function(h,...){
						dispose(win1)
					}, width=15)
	lay3[1:3, 1:5]=botonmae
	lay3[1:3, 8:13]=botonrocarea
	lay3[1:3, 16:21]=botoncasos
	lay3[1:3, 24:29]=botoncard
	lay[1:3, 32:37]=botonFilt
	lay3[1:3, 70:75]=botonSal
	glabel(" ", container=grupo5)
	
	visible(win1)=TRUE	
	
	
	#VER LO DE LAS MATRICES DE CONFUSION
	#por cada individuo
		#obtener_matriz_confusion(i)
		#hacer grafico con ella

}


lasegunda=function(soluciones, externa){ 
	
	cols=ncol(externa)
	clase=class(externa[,cols])
	
	
	iteras=nrow(soluciones)
	print("iteras")
	print(iteras)
	resultados=c(1:iteras)
	print("dim externa")
	print(dim(externa))
	for(i in 1:iteras){
		individuo=soluciones[i,]
		EF=filtrar(externa, individuo)
		modelo=construirModelo(EF)
		evalF2=evaluate_Weka_classifier(modelo, numfolds=nfolds, seed=2)
		#print(evalF2)
		
		if(clase=="nom"){
			pos=regexpr('Correctly', evalF2$string)
			#obtengo un substring entre posición+56 (lo que ocupa "Correctly classified cases y el nro") y posición+72 (espacios en blanco)
			keep=substring(evalF2$string, pos+56, pos+60) #ver 
			#elimino espacios en blanco para quedarme con el número
			valor=gsub(" ", "", keep)
			#convierto el char a int 
			#valorint=round(strtoi(valor))
			valorint=as.double(valor)
			resultado=valorint/100
		}else{ #no tengo porcentaje de correctamente clasificados... que uso??
			#una opcion podria ser el coeficiente de correlacion
			#Coeficiente de correlacion escalado o
			#1 - MAE
			
			#COEFICIENTE
			pos=regexpr('Correlation', evalF2$string)
			keep=substring(evalF2$string, pos+40, pos+46) #ver si 46 o menos
			valor=gsub(" ", "", keep)
			valorint=as.double(valor)
			resultado=(valorint+1)/2
		}
		print("Resultado")
		print(resultado)
		
		resultados[i]=resultado
		
	}
	
	ordenados=ordenar(soluciones, resultados)
	
	ordenados 

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


grafiquitos=function(datos){
	#win=gwindow(title = "Graficos", visible=FALSE, width=190, height=200, parent=c(575,230), toolkit="RGtk2")
	#grupo1=ggroup(horizontal = FALSE, container=win, spacing=2)
	#dev.flush()
	#dev.off()
	x11()
	ggplot(datos, aes(x=generaciones, y=fitness)) +  geom_line(aes(colour=vals, group=vals)) + geom_point(aes(colour=vals), size=3)
	
	#add(win, graf)
	#visible(win)=TRUE
	
	#ggplot(datos, aes(x=generaciones, y=fitness)) +  geom_line(aes(colour=valores, group=valores)) + geom_point(aes(colour=valores), size=3)
}	





