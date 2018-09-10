require("gWidgets")
#require("genalg")
require("RWeka")
require("gWidgetsRGtk2")
require("RGtk2")
require("cairoDevice")
require("caret")
require("gWidgets2RGtk2")
require("gWidgets2")

source("DataInput.R")
source("SegundaFase.r")
source("AG.R")
source("Mostrar_Resultados.R")

archivo=NULL

iniciar=function(){
	win=gwindow(title = "DELPHOS.R", visible=FALSE, width=190, height=200, parent=c(575,150), toolkit="RGtk2")
	group=ggroup(horizontal = FALSE, container=win, spacing=2)
	glabel("  ", container=group)
	frame1=gframe(container=group, text="Experiment Design", horizontal=FALSE, spacing=5, pos=0, expand=TRUE)
	lay1=glayout(container=frame1)
	font(frame1)<-list(weight="bold", style="oblique", size=20)
	label1=glabel("             ")
	lay1[1,1:5]=label1
	group2=ggroup(horizontal = FALSE, spacing=15)
	lay2=glayout(container=group2)
	
	obj_button_1=gbutton("First Phase", width=10, 
						handler=function(h, ...){
							primeraFase()
							
						})
	lay2[1,4:11]=obj_button_1
	objb2=gbutton("Second Phase", handler=(function(h,...){
																segundaFase()
															}), width=10)
	lay2[2,4:11]=objb2
	objb3=gbutton("First and Second Phase", handler=(function(h,...){
																primerasegunda()
																
															}), width=10)
	lay2[3,4:11]=objb3
	lay2[4:5, 1:5]=glabel("       ")
	lay1[3, 1:5]=group2
	
	group3=ggroup(container=group, spacing=15)
	lay3=glayout(container=group3)
	
	objla2=glabel("Results")
	lay3[2,4:10]=objla2
	objsep2=gseparator()
	lay3[3,3:11]=objsep2
	objb3=gbutton("View saved selections", handler=function(h,...){
																	ver_resultados()
																}, width=10)
	lay3[4,4:10]=objb3
	lay3[5, 1]=glabel(" ")
	visible(win)=TRUE
}
#----------------------------------------------------------------


#solamente la primera fase
primeraFase=function(){
	data_input("P") #P primera fase
}

#solamente la segunda fase
segundaFase=function(){ #S segunda fase
	obtener_archivo_entrada()
}

#primera y segunda fase en secuencia
primerasegunda=function(){ #PS primera y segunda fase
	data_input("PS")
}


#cargar_archivo
ver_resultados=function(){
	win=gwindow(title="View Results", visible=FALSE, width=200,  height=70, parent=c(200,200))
	
	group2=ggroup(container=win, spacing=15, horizontal=FALSE)
	group1=ggroup(container=group2, spacing=10, horizontal=TRUE)
	#glabel("  ", container=group1)
	label1=glabel(" Load File... (Second Phase file)", container=group1)
	text1=gedit("Load file", container=group1, width=20)
	button5=gbutton("Browse...", container=group1, 
					handler=function(h,...){
						file5=gfile(type="open")
						if(length(file5)>0){
							if(is.na(file5)){
								svalue(text1)="Load file"
							}else{
								svalue(text1)=file5
							}
						}
						print(file5)
					}, width=15)
	#glabel("  ",container=group1)
	
	lay3=glayout(container=group2)
	#glabel("            ", container=group3)
	buttonok=gbutton("OK",handler=function(h,...){
												bandera<<-FALSE
												tryCatch(load(svalue(text1)), 
														error=function(e){
																	print(e)
																	str1=iconv("The selected file is not valid.", from="UTF-8", to="UTF-8")
																	gmessage(str1, icon="error")
																	bandera<<-TRUE
														}
												)
												if(!bandera){
													mostrar_resultados(svalue(text1))
													dispose(win)
												}
											})
	#glabel("                                                                    ", container=group3)
	
	lay3[1, 16:22]=buttonok
	
	visible(win)=TRUE
}








