require("caret")
#require("GA")
source("Carga_y_preparacion_de_datos.R")

	generar_data_frame_C=function(){
	nombres=read.csv("nombres descriptores.csv", sep=";", header=FALSE)
	propiedad=read.csv("valores_propiedad_logPliver_CLASIFICACION.csv", sep=";", header=FALSE)
	dataframe1=read.csv("valores descriptores.csv", header=FALSE, sep=";", stringsAsFactors=FALSE, dec=",")  #
	names(dataframe1)=nombres[,1]

	dataframe1=filtrado_columnas(dataframe1) #filtrado data frame: elimina columnas constantes
	dataframe2=cbind(dataframe1,propiedad) #new data frame

	dataframe2
}