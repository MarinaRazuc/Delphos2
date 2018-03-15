source("extras.R")
source("AG.R")
source("SegundaFase.r")
source("PrimeraFase.R")
require("gWidgets2")
require("gWidgets2RGtk2")
require("gWidgetsRGtk2")

metodo="Regression Trees"
valInterna=0.75
valExterna=0.25
seed=1
trials=3
maxSelectVars=0
alpha=0.6
popSize=100
eliteSize=10
tourSize=6
pxo=0.6
pmut=0.2
nroGens=100
stallGens=10
umbral=0.000001
archivoUno="FirstPhase.csv"
dataframe0=NULL
salida=NULL
INTERNA=NULL
EXTERNA=NULL
clase="num"


seteo=function(datos, codigo){
	#print("SETEO")
	dataframe0<<-datos
	win1=gwindow(title="FirstPhase",  visible=FALSE, width=300, height=400, parent=c(470,50))
	group_1=ggroup(horizontal = FALSE, container=win1) #principal
	
	frame1=gframe(container=group_1, text="First Phase Settings", horizontal=FALSE, spacing=10, pos=0)
	group_11=ggroup(horizontal=FALSE, container=frame1)
	
	group_2=ggroup(horizontal=TRUE, container=group_11) #label experiment i of n
	print(expActual)
	print(nroExp)
	str1=paste0(paste(paste0("Experiment ", expActual), "of "), nroExp)
	label3=glabel("                             ", container=group_2)
	label2=glabel("                             ", container=group_2)
	label1=glabel(str1, container=group_2, width=5)
	
	group_3=ggroup(horizontal=FALSE, container=group_11) #general settings
	frame2=gframe(text="General Settings", pos=0, container=group_3, horizontal=FALSE , spacing=5)
	group_31=ggroup(horizontal=TRUE, container=frame2)
	label4=glabel("  Percentage of Internal Validation     ", container=group_31)
	obj_gedit1=gedit(c(75), container = group_31, width=5, coerce.with =as.numeric, 
				handler=function(h, ...){
					# valInterna<<-svalue(obj_gedit1)/100
					# if(valInterna<=0 || valInterna >=100){
						# gmessage("Error, el porcentaje de validación interna debe estar entre 1 y 99", icon="error") #VER
					# }
				} )

	group_32=ggroup(horizontal=TRUE,container=frame2)
	label5=glabel("  Percentage of External Validation     ", container=group_32)
	obj_gedit2=gedit(c(25), container = group_32, width=5, coerce.with =as.numeric,
				handler=function(h, ...){
					#valExterna <<-svalue(obj_gedit2)/100
				} )

	group_33=ggroup(horizontal=TRUE,container=frame2)
	label6=glabel("  Seed                                                      ", container=group_33)
	obj_gedit3=gedit(c(1), container=group_33, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					#seed<<-svalue(obj_gedit3)
				} )
	
	group_4=ggroup(horizontal=FALSE, container=group_11) #wrapper configuration
	group_41=ggroup(container=group_4, horizontal=TRUE)
	label41=glabel("  Trials                                   ", container=group_41)
	obj_gedit41=gedit(c(3), container=group_41, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					#trials<<-svalue(obj_gedit41)
				} )
	
	group_42=ggroup(container=group_4, horizontal=TRUE)
	label42=glabel("  Alpha Value                       ", container=group_42)
	obj_gedit42=gedit(c(0.6), container=group_42, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					#alpha<<-svalue(obj_gedit42)
				} )

	group_44=ggroup(container=group_4, horizontal=TRUE) #maximum selected  vars
	label44=glabel("  Maximum Selected Vars  ", container=group_44)
	obj_gedit44=gedit(c(0), container=group_44, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					#maxSelectVars<<-svalue(obj_gedit44)
				} )
	
	 
	group_43=ggroup(container=group_4, horizontal=TRUE, width=500)
	#glabel("grupo 43", container=group_43)
	group_431=ggroup(container=group_43, horizontal=FALSE, width=250)
	#glabel("grupo 431", container=group_431)
	#glabel("grupo 432", container=group_432)
	
	label431=glabel("Function:", container=group_431)
	radio431 = gradio(c("Regression","Classification"), container=group_431, 
				handler=function(h,...){
						valor=svalue(radio431)
						print(valor)
						if(valor=="Regression"){
							 metodo<<-svalue(combobox4321)
							 print(metodo)
						}else{
							 metodo<<-svalue(combobox4322)
							 print(metodo)
						}
					})
					
	group_432=ggroup(container=group_43, horizontal=FALSE, anchor=250, value=5)
	layout1=glayout(homogeneus=FALSE, spacing=10, container=group_432)
	regresion=c("Linear Regression", "Non-Linear Regression", "Regression Trees", "k-Nearest Neighbours")
	clasificacion=c("Decision Trees", "k-Nearest Neighbours")
	label432=glabel(" ")
	combobox4321=gcombobox(regresion, selected=3, editable=FALSE)
	combobox4322=gcombobox(clasificacion, selected=1, editable=FALSE)
#	add(layout1, combobox4321, fill = 'x', anchor = 100)
#	add(layout1, combobox4322, fill = 'x', anchor = 100)
	layout1[1,1]<-label432
	layout1[2,1:20]<-combobox4321
	layout1[3,1:20]<-combobox4322
	
	
	group_45=ggroup(container=group_4,horizontal=TRUE) #ga settings
	frame45=gframe(container=group_45, horizontal=FALSE, text="GA Settings", spacing=5, pos=0)
	group_451=ggroup(container=frame45,horizontal=TRUE)
	label451=glabel("  Pop Size                 ", container=group_451)
	obj_gedit451=gedit(c(100), container=group_451, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					#popSize<<-svalue(obj_gedit451)
				} )

	group_452=ggroup(container=frame45,horizontal=TRUE)
	label452=glabel("  Elite Size                 ", container=group_452)
	obj_gedit452=gedit(c(10), container=group_452, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					#eliteSize<<-svalue(obj_gedit452)
				} )
	
	group_453=ggroup(container=frame45,horizontal=TRUE) 
	label453=glabel("  Tournament Size  ", container=group_453)
	obj_gedit453=gedit(c(6), container=group_453, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
				#	tourSize<<-svalue(obj_gedit453)
				} )

	group_454=ggroup(container=frame45,horizontal=TRUE) 
	label454=glabel("  PXO                        ", container=group_454)
	obj_gedit454=gedit(c(0.6), container=group_454, width=5, coerce.with=as.numeric,
				handler=function(h, ...){
					pxo<<-svalue(obj_gedit454)
				} )

	group_455=ggroup(container=frame45,horizontal=TRUE) 
	label455=glabel("  PMut                      ", container=group_455)
	obj_gedit455=gedit(c(0.2), container=group_455, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					pmut<<-svalue(obj_gedit455)
				} )

	group_456=ggroup(container=frame45,horizontal=TRUE) 
	label456=glabel("-----Stopping Criteria-----", container=group_456)
	
	group_457=ggroup(container=frame45,horizontal=TRUE) 
	label457=glabel("  #Gens                     ", container=group_457)
	obj_gedit457=gedit(c(100), container=group_457, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					nroGens<<-svalue(obj_gedit457)
				} )

	group_458=ggroup(container=frame45,horizontal=TRUE) 
	label458=glabel("  Stall Gens               ", container=group_458)
	obj_gedit458=gedit(c(10), container=group_458, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					stallGens<<-svalue(obj_gedit458)
				} )
		
	group_500=ggroup(container=frame45, horizontal=TRUE)
	label500=glabel("  Stall Threshold      ", container=group_500)
	obj_gedit500=gedit(c(0.00001), container=group_500, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					umbral<<-svalue(obj_gedit500)
				} )
	
	group_A=ggroup(container=frame45, horizontal=TRUE)
	labelA=glabel(" Guardar resultados como... (archivo csv) ", container=group_A)
	textA=gedit(archivoUno, container=group_A, font.attr=list(style="bold"), width=20)
	buttonA=gbutton("Browse...", container=group_A, 
					handler=function(h,...){
						
						file5=gfile("Guardar como...", type="save")
						if(is.na(file5)){
							svalue(textA)="Ingrese archivo"
						}else{
							svalue(textA)=file5
						}
						print(file5)
						archivoUno<<-file5
						
					}, width=15)
	
	group_5=ggroup(horizontal=TRUE, container=group_11) #boton ok
	label51=glabel("                                  ", container=group_5)
	button5=gbutton("OK", container=group_5, 
					handler=function(h,...){
						bandera=TRUE
						valInterna<<-svalue(obj_gedit1)/100
						if(valInterna<=0 || valInterna >=1){
							str1=iconv("Error, el porcentaje de validación interna debe estar entre 1 y 99.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") #VER
							bandera=FALSE
						}
						valExterna <<-svalue(obj_gedit2)/100
						if(valExterna<=0 || valExterna >=1){
							str1=iconv("Error, el porcentaje de validación externa debe estar entre 1 y 99.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") #VER
							bandera=FALSE
						}
						seed<<-svalue(obj_gedit3)
						if(seed<=0){
							str1=iconv("Error, la semilla (seed) debe ser un número positivo.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") 
							bandera=FALSE
						}
						trials<<-svalue(obj_gedit41)
						if(trials<=0){
							str1=iconv("Error, la cantidad de pruebas (trials) debe ser un número positivo.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") 
							bandera=FALSE
						}
						alpha<<-svalue(obj_gedit42)
						if(alpha<=0 || alpha>1){
							gmessage("Error, alpha debe ser un valor entre 0 y 1", icon="error")
							bandera=FALSE
						}
						maxSelectVars<<-svalue(obj_gedit44)
						if(maxSelectVars<0 || maxSelectVars>dim(dataframe0)[2]){
							str1=iconv("Error, la cantidad máxima de variables seleccionadas (maximum selected vars) debe estar entre 0 y la cantidad máxima de descriptores.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") 
							bandera=FALSE
						}
						popSize<<-svalue(obj_gedit451)
						if(popSize<=0){
							str1=iconv("Error, el tamaño de la población debe ser un número positivo.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") 
							bandera=FALSE
						}
						eliteSize<<-svalue(obj_gedit452)
						if(eliteSize>popSize){
							str1=iconv("Error, el tamaño de la elite debe ser menor que el tamaño de la población.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error")
							bandera=FALSE
						}
						tourSize<<-svalue(obj_gedit453)
						if(tourSize<=0){
							str1=iconv("Error, el tamaño del torneo debe ser un número positivo.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") 
							bandera=FALSE
						}else{
							if(tourSize>popSize){
								str1=iconv("Error, el tamaño del torneo no puede superar el tamaño de la población", from="UTF-8", to="UTF-8")
								gmessage(str1, icon="error")
								bandera=TRUE
							}
						}
						pxo<<-svalue(obj_gedit454)
						if(pxo<0 || pxo>1){
							str1=iconv("Error, la probabilidad de permutación (pxo) debe estar entre 0 y 1.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error")
							bandera=FALSE
						}
						pmut<<-svalue(obj_gedit455)
						if(pmut<0 || pmut>1){
							str1=iconv("Error, la probabilidad de mutación (pmut) debe estar entre 0 y 1.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error")
							bandera=FALSE
						}
						nroGens<<-svalue(obj_gedit457)
						if(nroGens<=0){
							str1=iconv("Error, el número de generaciones (nroGens) debe ser positivo.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error")
							bandera=FALSE
						}
						stallGens<<-svalue(obj_gedit458)
						if(stallGens<0){
							gmessage("Error, 'stallGens' debe ser mayor a 0.", icon="error")
							bandera=FALSE
						}
						umbral<<-svalue(obj_gedit500)
						
						valor=svalue(radio431)
						if(valor=="Regression"){
							 metodo<<-svalue(combobox4321)
							 print(metodo)
						}else{
							 metodo<<-svalue(combobox4322)
							 print(metodo)
						}
						
						archivoUno<<-svalue(textA)
						
						if(bandera){
							#print("IFBANDERA")
							dispose(win1)							
							comenzar_calculo(archivoUno, codigo)
						}
					
					}, width=15)
					
	visible(win1)=TRUE

}


comenzar_calculo=function(archivo, cod){
	#print("Llamar primera fase")
	#print(archivo)
	#print(dim(dataframe0))
	partes=partir(dataframe0, 1-valExterna, seed) #ver valInterna y valExterna

	INTERNA<<-partes$it
	EXTERNA<<-partes$et
	
	largo=length(partes$it[1,])
	#print(largo)
	
	if(class(partes$it[,largo])=="numeric"){
		clase<<-"num"
	}else{
		clase<<-"nom"
	}

	 
	if(metodo=="Linear Regression"){
		nromet=1
	}else{
		if(metodo=="Non-Linear Regression"){
			nromet=2
		}else{
			if(metodo=="Regression Trees"){
				nromet=3
			}else{
				if(metodo=="k-Nearest Neighbours"){
					nromet=4
				}else{
					if(metodo=="Decision Trees"){
						nromet=5
					}
				}	
			}
		}
	}
	
	
	resultados=primera_fase(archivo, nromet, partes$it, trials, clase, alpha, maxSelectVars, popSize, tourSize, pxo, pmut, eliteSize, nroGens, stallGens,umbral, valInterna)

	write("---", archivo, append=TRUE)
	write.table(resultados, archivo, append=TRUE)#ver
	write("---", archivo, append=TRUE)
	#write.table(dataframe0, archivo, append=TRUE)
	write.table(partes$it, archivo, append=TRUE)
	write("---", archivo, append=TRUE)
	write.table(partes$et, archivo, append=TRUE)
	write("---", archivo, append=TRUE)
	
	
	
	if(cod=="PS"){
		ventana_fase_dos(archivo)
	}
}




