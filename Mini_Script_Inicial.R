require("caret")
#require("GA")
source("Carga_y_preparacion_de_datos.R")

generar_data_frame_mini=function(){
	nombres=read.csv("nombres descriptores.csv", sep=";", header=FALSE, stringsAsFactors=FALSE)
	propiedad=read.csv("propiedad logPliver.csv", sep=";", header=FALSE, dec=",", stringsAsFactors=FALSE)
	dataframe1=read.csv("valores descriptores.csv", header=FALSE, sep=";", dec=",", stringsAsFactors=FALSE)
	names(dataframe1)=nombres[,1]

	dataframe1=filtrado_columnas(dataframe1) #filtrado data frame: elimina columnas constantes
	dataframe2=cbind(dataframe1,propiedad) #new data frame

	dataframe2
}
##set.seed(seed) #por parametro
##indices4=createDataPartition(dataframe2$V1, p=0.75, list=FALSE) #p por parametro (validacion externa)
#V1 porque se supone que tengo el nombre de los descriptores, pero NO el de la propiedad, que queda como V1
##externa=dataframe2[-indices4, ]
##interna=dataframe2[indices4, ]

