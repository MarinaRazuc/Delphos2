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


preparar_datos=function(seed, trials){
	nombres=read.csv("Nombres_dfr4.csv", sep=" ", header=FALSE)
	propiedad=read.csv("prop_dfr4.csv", sep=" ", header=FALSE)
	
	if(class(propiedad[1])=="character")
		clase_propiedad="nom"
	else
		clase_propiedad="num"
		
	dfr4=read.csv("dfr4.csv", header=FALSE, sep=" ")
	names(dfr4)=nombres[,1]
	
	#FALTA COMPARAR NROS DE FILAS Y COLS Y DAR ERROR CUANDO CORRESPONDA
	
	fdfr4=filtrado_columnas(dfr4) #filtrado data frame: elimina columnas constantes
	ndfr4=cbind(fdfr4,propiedad) #new data frame
	
	set.seed(seed) #por parametro
	indices4=createDataPartition(ndfr4$V1, p=0.75, list=FALSE) #p por parametro (validacion externa)
	#V1 porque se supone que tengo el nombre de los descriptores, pero NO el de la propiedad, que queda como V1
	externa=ndfr4[-indices4, ]
	interna=ndfr4[indices4, ]
	
	primera_fase(interna, trials, clase_propiedad) #FALTAN MUCHOS MUCHOS PARAMETROS
	segunda_fase()

}