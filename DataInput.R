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
	#glabel(" ", container=frame1)
	lay1=glayout(container=frame1)
	
	group_2=ggroup(horizontal=FALSE) #para la carga de datos
	group_3=ggroup(horizontal=TRUE, container=group_1) #para los botones
	group_4=ggroup(horizontal=TRUE, container=group_2)
	group_5=ggroup(horizontal=TRUE, container=group_2)
	group_6=ggroup(horizontal=TRUE, container=group_2)

	check4=gcheckboxgroup(c("CSV file with separator semicolon"), container=group_4, checked=TRUE)
	label4in=glabel(" ", container=group_4, width=5)
	label4=glabel("(*) Matrix of values of descriptors ", container = group_4, width=70)
	text4=gedit("Ingrese archivo", container=group_4, font.attr=list(style="bold"), width=70)
	label4in=glabel(" ", container=group_4, width=5)
	button4=gbutton("Browse...", container=group_4, 
					handler=function(h,...){
						file4=gfile()
						if(length(file4)>0){
							if(is.na(file4)){
								svalue(text4)="Ingrese archivo"
							}else{
								svalue(text4)=file4
							}
						}
						print(file4)
					}, width=15)
	 

	check5=gcheckboxgroup(c("CSV file with separator semicolon"), container=group_5, checked=TRUE)
	label4in=glabel(" ", container=group_5, width=5)
	label4=glabel("(*) Vector of experimental values  ", container = group_5, width=70)
	text5=gedit("Ingrese archivo", container=group_5, font.attr=list(style="bold"), width=70)
	label4in=glabel(" ", container=group_5, width=5)
	button5=gbutton("Browse...", container=group_5, 
					handler=function(h,...){
						file5=gfile()
						if(length(file5)>0){
							if(is.na(file5)){
								svalue(text5)="Ingrese archivo"
							}else{
								svalue(text5)=file5
							}
						}
						print(file5)
					}, width=15)
					
	check6=gcheckboxgroup(c("CSV file with separator semicolon"), container=group_6, checked=TRUE)
	label4in=glabel(" ", container=group_6, width=5)
	label4=glabel("    Vector of descriptors names      ", container = group_6, width=70)
	text6=gedit("Ingrese archivo", container=group_6, font.attr=list(style="bold"), width=70)
	label4in=glabel(" ", container=group_6, width=5)
	button6=gbutton("Browse...", container=group_6, 
					handler=function(h,...){
						file6=gfile()
						if(length(file6)>0){
							if(is.na(file6)){
								svalue(text6)="Ingrese archivo"
							}else{
								svalue(text6)=file6
							}
						}
						print(file6)
					}, width=15)
					
	
					

	lay1[2,1:10]=group_2
	#lab=glabel(container=group_2)
	lay1[3,10]=glabel("(*)Required")
	#lay1[4,1]=glabel("")
	glabel(container=group_3)	
	grupo00=ggroup(spacing=10, horizontal=FALSE)
	
	strs2=c("',' coma decimal","'.' punto decimal")
	radio431 = gradio(strs2, container=grupo00, 
				handler=function(h,...){
						valor=svalue(radio431)
						print(valor)
						if(valor==strs2[1]){
							 dec<<-","
							 print(dec)
						}else{
							 dec<<-"."
							 print(dec)
						}
					})
	lay1[4:6, 1:10]=grupo00
	button8=gbutton("OK", container=group_3, width=5, 
					handler=function(h,...){
						if(svalue(radio431)==strs2[1]){
							dec<<-","
						}else{
							dec<<-"."
						}
						carga_y_control(win2, svalue(text4), svalue(text5), svalue(text6), svalue(check4), svalue(check5), svalue(check6), cod, dec)
					})
	button9=gbutton("Cancel", container=group_3, width=5, 
					handler=function(h,...){
						dispose(win2)
					})
	visible(win2)=TRUE
}


#carga_y_control
carga_y_control=function(win2, A1, A2, A3, c1, c2, c3, cod, deci){ 
	bandera<<-FALSE
	print(A1)
	print(A2)
	print(A3)
	
	if(A2!="Ingrese archivo" && A2!=""){
		if(length(c2!=0)){
			sep=";"
		}else{
			sep=" "
		}
		
		propi=tryCatch(read.csv(A2, sep=sep, header=FALSE, stringsAsFactors=FALSE, dec=","), 
						error=function(e){
							print("Error en la lectura del archivo")
							print(e)
							bandera<<-TRUE
							gmessage("Error, ingrese archivo con los valores correspondientes a la propiedad.", icon="error")
						}
					)
		if(!bandera){
			clase=class(propi[1,1])
			
			if(clase=="numeric"){
				propiedad<<-tryCatch(read.csv(A2, sep, header=FALSE, dec=deci, stringsAsFactors=FALSE), 
								error=function(e){
									print("Error en la lectura del archivo")
									print(e)
									bandera<<-TRUE
									gmessage("Error, ingrese archivo con los valores correspondientes a la propiedad.", icon="error")
								}
							) 
			}else{
				if(clase=="character"){
					propiedad<<-tryCatch(read.csv(A2, sep, header=FALSE), 
								error=function(e){
									print("Error en la lectura del archivo")
									print(e)
									bandera<<-TRUE
									gmessage("Error, ingrese archivo con los valores correspondientes a la propiedad.", icon="error")
								}
							)
				}
			}
			if(!bandera){
				if(A1!="Ingrese archivo" && A1!=""){
					if(length(c1)!=0){
						sep=";"
					}else{
						sep=","
					} 
					descriptores<<-tryCatch(read.csv(file=A1, sep=sep, dec=deci, header=FALSE, stringsAsFactors=FALSE), 
										error=function(e){
											print("Error en la lectura del archivo")
											print(e)
											gmessage("Error, ingrese archivo con los valores correspondientes a los descriptores.", icon="error")
											bandera<<-TRUE
										}
								)
					if(!bandera){
						if(A3!="Ingrese archivo" && A3!=""){
							if(length(c3)!=0){
								sep=";"
							}else{
								sep=" "
							}
							nombresD<<-tryCatch(read.csv(A3, sep, header=FALSE), 
											error=function(e){
												print(e)
												nombresD<<-data.frame()
											}
										)
						}
					}else{
						nombresD<<-data.frame()
					}
				}
			}else{
				gmessage("Error, ingrese archivo con los valores correspondientes a la propiedad.", icon="error")
				bandera<<-TRUE
			}
		}
	}else{
			gmessage("Error, ingrese archivo con los valores correspondientes a la propiedad.", icon="error")
			bandera<<-TRUE
	}

	#	dataframes: descriptores, propiedad, nombresD y nombresP
	# 	nombresD y nombresP tal vez no estén, no son necesarios

	if(!bandera){
		dims=c()
		
		dims[1]=dim(descriptores)[1] #filas
		dims[2]=dim(descriptores)[2] #columnas
		dims[3]=dim(propiedad)[1]
		dims[4]=dim(propiedad)[2] #1 o 0
		
		dims[5]=dim(nombresD)[1]
		dims[6]=dim(nombresD)[2] #1 o 0
		
		if(dims[1] != dims[3]){
			gmessage("ERROR, la cantidad de valores para la propiedad no coincide con la cantidad de observaciones correspondientes a los descriptores", icon="error")
			bandera=TRUE
		}else{ #todo ok
			if(dims[5]!=0){ #hay algo en el df nombresD
				if(dims[2]!=dims[5]){
					bandera=TRUE
					gmessage("Error, la cantidad de nombres de descriptores no coincide con la cantidad de descriptores disponibles.", icon="error")
				}else{
					if(dims[6]!=1){
						gmessage("Error en el formato del vector de nombres de descriptores.", icon="error")
						bandera=TRUE
					}
				}
			}
			if(!bandera){
				cartel_data_loaded(win2, dims, cod)
			}	
		}
	}
}
	
#cartel_data_loaded
cartel_data_loaded=function(win2, dimensiones, codigo){
		
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
														dispose(win2)
														dispose(window1)
														seteo(dataframe1, codigo)
										
													}, width=15)
	button2=gbutton("Cancel", container=group_2, handler=function(h, ...){
															
																dispose(window1)
														}, width=15)

	visible(window1)=TRUE
}



