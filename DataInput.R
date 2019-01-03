source("GeneralSetting.R")
source("extras.R")


descriptores<<-data.frame()
propiedad<<-data.frame()
nombresD<<-data.frame()

#data_input
data_input=function(cod){
	win2=gwindow(title = "DataInput", visible=FALSE, width=200,  height=70., parent=c(200,200)) #visible=FALSE por ahi
	group_1=ggroup(horizontal = FALSE, container=win2) #el contenedor principal
	glabel(" ", container=group_1)
	frame1=gframe(container=group_1, text="Data", horizontal=FALSE, spacing=10, pos=0.5)
	lay1=glayout(container=frame1)
	
	group_2=ggroup(horizontal=FALSE) #para la carga de datos
	group_3=ggroup(horizontal=TRUE, container=group_1) #para los botones
	group_4=ggroup(horizontal=FALSE, container=group_2)
	group_5=ggroup(horizontal=TRUE, container=group_4) # matrix of data
	group_6=ggroup(horizontal=TRUE, container=group_4) # select file + browse
	group_7=ggroup(horizontal=TRUE, container=group_4) # check files with csv
	
	check4=gcheckboxgroup(c("CSV file with separator semicolon"), container=group_7, checked=TRUE)
	##label4in=glabel(" ", container=group_7, width=5)
	#label4=glabel("(*) Matrix of values of descriptors ", container = group_4, width=70)
	label4=glabel(" Matrix of data ", container = group_5, width=70,  pos=0.5)
	
	text4=gedit("Select File", container=group_6, font.attr=list(style="bold"), width=30)
	##label4in=glabel(" ", container=group_6, width=5)
	button4=gbutton("Browse...", container=group_6, 
					handler=function(h,...){
						file4=gfile()
						if(length(file4)>0){
							if(is.na(file4)){
								svalue(text4)="Select File"
							}else{
								svalue(text4)=file4
							}
						}
						print(file4)
					}, width=15)
					
	lay1[2,1:10]=group_2
	grupo00=ggroup(spacing=10, horizontal=FALSE)

	lay1[4:5, 1:10]=grupo00
	button8=gbutton("OK", container=group_3, width=5, 
					handler=function(h,...){
						#carga_y_control(win2, svalue(text4), svalue(text5), svalue(text6), svalue(check4), svalue(check5), svalue(check6), cod)
						carga_y_control(win2, svalue(text4), svalue(check4), cod)
					})
	button9=gbutton("Cancel", container=group_3, width=5, 
					handler=function(h,...){
						dispose(win2)
					})
	visible(win2)=TRUE
}


#carga_y_control
#carga_y_control=function(win2, A1, A2, A3, c1, c2, c3, cod){
carga_y_control=function(win2, A1, c1, cod){ 
	bandera<<-FALSE
	print(A1)
	
	if(A1!="Select File" && A1!=""){
		if(length(c1)!=0){
			sepa=";"
			deci=","
		}else{
			sepa=","
			deci="."
		}
		print("separador")
		print(sepa)
		print("decimal")
		print(deci)
		
		datos<<-tryCatch(read.csv(A1, sep=sepa, header=TRUE, stringsAsFactors=FALSE, dec=deci), 
						error=function(e){
							#print("Error en la lectura del archivo")
							print(e)
							bandera<<-TRUE
							gmessage("Error, enter file with the data.", icon="error")
						}			
					)
		print("dimensiones luego de leer")
		print(dim(datos))
		if(!bandera){
			clase=class(datos[1,ncol(datos)]) 
		    nombresD <<- names(datos)[1:(ncol(datos)-1)]
			propiedad <<- datos[,ncol(datos)]
			descriptores <<- datos[,1:(ncol(datos)-1)]
			print("------")
			print(length(nombresD))
			print(length(propiedad))
			print(dim(descriptores))
		}
	}else{
			gmessage("Error, enter file with the data.", icon="error")
			bandera<<-TRUE
	}
	
	if(!bandera){
		dims=c()
		print("dimensiones")
		print(dim(datos))
		dims[1]=nrow(datos) #filas descriptores
		dims[2]=ncol(datos)-1 #columnas descriptores
		dims[3]=nrow(datos) #dim(propiedad)[1]
		dims[4]=1 #dim(propiedad)[2] #1 o 0
		
		dims[5]=length(names(datos))-1 #dim(nombresD)[1]
		dims[6]=1 #dim(nombresD)[2] #1 o 0
		
		
		cartel_data_loaded(win2, dims, cod, datos)
		
	}
}
	
#cartel_data_loaded
cartel_data_loaded=function(win2, dimensiones, codigo, datos){
		
	window1=gwindow(title = "DataLoaded", visible=FALSE, width=200,  height=200, parent=c(575,230))
	group_1=ggroup(horizontal = FALSE, container=window1) #ppal
	
	frame1=gframe(container=group_1, text="Data Loaded", horizontal=FALSE, spacing=10, pos=1)
	group_2=ggroup(container=group_1, horizontal = TRUE)
	
	str11="Matrix of values of descriptors"
	str12=paste(paste0(paste(paste0("Size:( ", dimensiones[1]), "x "), dimensiones[2]), ")")
	str13="Vector of experimental values"
	str14=paste(paste0(paste(paste0("Size: ( ", dimensiones[3]), "x "),dimensiones[4] ), ")")
	str15="Vector of descriptors names"
	str16=paste(paste0(paste(paste0("Size: ( ", dimensiones[5]), "x "), dimensiones[6]), ")")

	 
	label0=glabel(" ", container=frame1)
	label1=glabel(str11, container=frame1)
	label2=glabel(str12, container=frame1)
	label3=glabel(" ", container=frame1)
	label4=glabel(str13, container=frame1)
	label5=glabel(str14, container=frame1)
	label6=glabel(" ", container=frame1)
	label7=glabel(str15, container=frame1)
	label8=glabel(str16, container=frame1)
	label6=glabel(" ", container=frame1)

	button1=gbutton("OK", container=group_2, handler=function(h, ...){
														dataframe1=generar_data_frame(descriptores,propiedad,nombresD)
														#dataframe1=controlar(datos)
														dispose(win2)
														dispose(window1)
														seteo(dataframe1, codigo)
										
													}, width=15)
	button2=gbutton("Cancel", container=group_2, handler=function(h, ...){
															
																dispose(window1)
														}, width=15)

	visible(window1)=TRUE
}



