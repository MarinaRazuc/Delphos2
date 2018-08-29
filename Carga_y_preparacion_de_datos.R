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
		if((!distintos) ){ #debo eliminar la columna|| (contador>=(.85*nfils))
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

