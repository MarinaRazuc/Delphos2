
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
imprimir_error=function(modelo, datos){
	
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


#
#
#
procesar_SF=function(scaneo){ #probablemente esto cambie porque voy a necesitar guardar mas info
	nombres=c()
	nombres_desc=c()
	
	largo=length(scaneo)
	bandera=FALSE
	i=1
	while(i<=largo && !bandera){
		elem=scaneo[i]
		pos=grep("V", elem)
		if(length(pos)!=0){#algo con V
			nombres=c(nombres, elem)
		}else{ #no hay mas V
			bandera=TRUE
		}
		i=i+1
	}
	
	individuos=data.frame()
	if(bandera){ #busco individuos
		j=1
		k=1
		limitador=2 
		bandera=FALSE
		while(i<=largo && !bandera){
			elem=as.numeric(scaneo[i])
			if(is.na(elem)){ #llegamos a algo q no es numero
				bandera=TRUE
			}else{
				pos=grep(limitador, elem)
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
		
		vals_finales=c()
		if(bandera){ #saltear V1 y V2 de valores
			i=i+3 #para saltear vs y numeracion (CREO)
			cant=nrow(individuos)
			k=1
			while(k<=cant && i<=largo){
				elem=scaneo[i]
				vals_finales=c(vals_finales, elem)
				k=k+1
				i=i+3 #por las numeraciones
			}
			#tengo todos los valores
			i=i-1
			#tengo a i posicionado en el primer nombre
			cant=ncol(individuos)+1
			k=1
			
			while(k<=cant && i<=largo){
				elem=scaneo[i]
				nombres_desc=c(nombres_desc, elem)
				k=k+1
				i=i+1
			}
			
			valores=data.frame()
			i=i+1 #para saltear la numeracion
			h=1
			j=1
			k=1
			while(i<=largo){
				while(h<=cant){
					valores[j,k]=as.numeric(scaneo[i])
					h=h+1
					k=k+1
					i=i+1
				}
				j=j+1
				h=1
				k=1
				i=i+1 #para saltear la numeracion
			}
		}
	}
	
	
	resultados=list()
	resultados$nombres=nombres #no se si es necesario
	resultados$individuos=individuos
	resultados$nombres_desc=nombres_desc
	resultados$valores=valores
	resultados$vals_finales=vals_finales
	
	resultados
	
}
####
###
##
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


##
##
##
mostrar_resultados=function(archivo){

	#cambiar totalmente porque ahora los SecondPhase_xxxx.csv guardan otra info

	print("mostrar resultados")
	print(archivo)
	
	scan1=scan(archivo, what="numeric")
	procesado=procesar_SF(scan1)
	
	individuos=procesado$individuos
	matriz=procesado$valores
	ndescriptores=procesado$nombres_desc
	aptitudes=procesado$vals_finales
	
	win1=gwindow(title="Resultados", visible=FALSE, width=200,  height=200, parent=c(200,200))
	group1=ggroup(container=win1, spacing=15, horizontal=FALSE) #contenedor principal
	group2=ggroup(container=group1, spacing=15, horizontal=FALSE)
	
	text1=gtext(individuos, container=group2, editable=FALSE)
	
	visible(win1)=TRUE
	

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





