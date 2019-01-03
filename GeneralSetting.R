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
archivoUno="FirstPhase.RData"
dataframe0=NULL
salida=NULL
INTERNA=NULL
EXTERNA=NULL
clase="num"


seteo=function(datos, codigo){
	dataframe0<<-datos
	win1=gwindow(title="FirstPhase",  visible=FALSE, width=300, height=400, parent=c(470,50))
	group_1=ggroup(horizontal = FALSE, container=win1) #principal
	glabel("  ", container=group_1)
	frame1=gframe(container=group_1, text="First Phase Settings", horizontal=FALSE, spacing=10, pos=0)
	group_11=ggroup(horizontal=FALSE, container=frame1)
	
	group_2=ggroup(horizontal=TRUE, container=group_11) #label experiment i of n
	label2=glabel("                             ", container=group_2)
	
	group_3=ggroup(horizontal=FALSE, container=group_11) #general settings
	frame2=gframe(text="General Settings", pos=0, container=group_3, horizontal=FALSE , spacing=5)
	label3=glabel("                             ", container=group_3)
	group_31=ggroup(horizontal=TRUE, container=frame2)
	label4=glabel("  Percentage of Internal Validation     ", container=group_31)
	obj_gedit1=gedit(c(75), container = group_31, width=5, coerce.with =as.numeric, 
				handler=function(h, ...){
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
	frame4=gframe(text="Wrapper Configuration", pos=0, container=group_4, horizontal=FALSE , spacing=5)
	group_41=ggroup(container=frame4, horizontal=TRUE)
	

	label41=glabel("  Trials (#subsets)                ", container=group_41)
	obj_gedit41=gedit(c(10), container=group_41, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					#trials<<-svalue(obj_gedit41)
				} )
	
	group_42=ggroup(container=frame4, horizontal=TRUE)
	label42=glabel("  Alpha Value                       ", container=group_42)
	obj_gedit42=gedit(c(0.6), container=group_42, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					#alpha<<-svalue(obj_gedit42)
				} )

	group_44=ggroup(container=frame4, horizontal=TRUE) #maximum selected  vars
	label44=glabel("  Maximum Selected Vars  ", container=group_44)
	obj_gedit44=gedit(c(10), container=group_44, width=5, coerce.with=as.numeric, 
				handler=function(h, ...){
					#maxSelectVars<<-svalue(obj_gedit44)
				} )
	 
	group_43=ggroup(container=frame4, horizontal=TRUE, width=500)
	group_431=ggroup(container=group_43, horizontal=FALSE, width=250)
	
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
	regresion=c("Linear Regression", "Multilayer Perceptron", "k-Nearest Neighbours", "Random Forest", "Random Committee", "Regression Tree")
	clasificacion=c("Multilayer Perceptron", "Random Forest", "Random Committee", "k-Nearest Neighbours", "Decision Trees")
	label432=glabel(" ")
	combobox4321=gcombobox(regresion, selected=4, editable=FALSE)
	combobox4322=gcombobox(clasificacion, selected=2, editable=FALSE)
#	add(layout1, combobox4321, fill = 'x', anchor = 100)
#	add(layout1, combobox4322, fill = 'x', anchor = 100)
	layout1[1,1]<-label432
	layout1[2,1:20]<-combobox4321
	layout1[3,1:20]<-combobox4322
	
	
	group_45=ggroup(container=frame4,horizontal=TRUE) #ga settings
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
	labelA=glabel(" Save results as... (.RData file) ", container=group_A)
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
	label51=glabel("                                                  ", container=group_5)
	button5=gbutton("OK", container=group_5, 
					handler=function(h,...){
						bandera=TRUE
						valInterna<<-svalue(obj_gedit1)/100
						if(valInterna<=0 || valInterna >=1){
							str1=iconv("Error, the percentage of internal validation must be between 1 and 99.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") #VER
							bandera=FALSE
						}
						valExterna <<-svalue(obj_gedit2)/100
						if(valExterna<=0 || valExterna >=1){
							str1=iconv("Error, the percentage of external validation must be between 1 and 99.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") #VER
							bandera=FALSE
						}
						seed<<-svalue(obj_gedit3)
						if(seed<=0){
							str1=iconv("Error, the seed must be a positive number.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") 
							bandera=FALSE
						}
						trials<<-svalue(obj_gedit41)
						if(trials<=0){
							str1=iconv("Error, the number of tests (trials) must be a positive number.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") 
							bandera=FALSE
						}
						alpha<<-svalue(obj_gedit42)
						if(alpha<=0 || alpha>1){
							gmessage("Error, alpha must be a value between 0 and 1.", icon="error")
							bandera=FALSE
						}
						maxSelectVars<<-svalue(obj_gedit44)
						if(maxSelectVars<0 || maxSelectVars>ncol(dataframe0)){
							str1=iconv("Error, the maximum number of selected variables must be between 0 and the maximum number of descriptors.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") 
							bandera=FALSE
						}
						popSize<<-svalue(obj_gedit451)
						if(popSize<=0){
							str1=iconv("Error, the size of the population must be a positive number.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") 
							bandera=FALSE
						}
						eliteSize<<-svalue(obj_gedit452)
						if(eliteSize>popSize){
							str1=iconv("Error, the size of the elite must be smaller than the size of the population.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error")
							bandera=FALSE
						}
						tourSize<<-svalue(obj_gedit453)
						if(tourSize<=1){
							str1=iconv("Error, the size of the tournament must be greater than 1.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error") 
							bandera=FALSE
						}else{
							if(tourSize>popSize){
								str1=iconv("Error, the size of the tournament can not exceed the size of the population.", from="UTF-8", to="UTF-8")
								gmessage(str1, icon="error")
								bandera=TRUE
							}
						}
						pxo<<-svalue(obj_gedit454)
						if(pxo<0 || pxo>1){
							str1=iconv("Error, the probability of permutation (pxo) must be between 0 and 1.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error")
							bandera=FALSE
						}
						pmut<<-svalue(obj_gedit455)
						if(pmut<0 || pmut>1){
							str1=iconv("Error, the mutation probability (pmut) must be between 0 and 1.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error")
							bandera=FALSE
						}
						nroGens<<-svalue(obj_gedit457)
						if(nroGens<=0){
							str1=iconv("Error, the number of generations (nroGens) must be positive.", from="UTF-8", to="UTF-8")
							gmessage(str1, icon="error")
							bandera=FALSE
						}
						stallGens<<-svalue(obj_gedit458)
						if(stallGens<=0){
							gmessage("Error, 'stallGens' must be greater than 0.", icon="error")
							bandera=FALSE
						}
						umbral<<-svalue(obj_gedit500)
						
						valor=svalue(radio431)
						if(valor=="Regression"){
							 metodo<<-svalue(combobox4321)
						}else{
							 metodo<<-svalue(combobox4322)
						}
						
						archivoUno<<-svalue(textA)
						
						if(bandera){
							dispose(win1)							
							comenzar_calculo(archivoUno, codigo)
						}
					
					}, width=15)
					
	visible(win1)=TRUE

}


comenzar_calculo=function(archivo, cod){
	print("comienza el cálculo")
	write.csv(dataframe0, file="datosss_1.csv")
	partes=partir(dataframe0, 1-valExterna, seed) #ver valInterna y valExterna	
	largo=length(partes$it[1,])
	
	if(class(partes$it[,largo])=="numeric"){
		clase<<-"num"
	}else{
		clase<<-"nom"
	}
	print("CLASE :")
	print(clase)
	
	nromet=8
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
					}else{
						if(metodo=="Random Forest"){
							nromet==8
						}else{
							if(metodo=="Random Committee"){
								nromet=9
							}else{
								if(metodo=="Multilayer Perceptron"){
									nromet=10
								}
							}
						}
					}
				}	
			}
		}
	}
	
	resultados=primera_fase(archivo, nromet, partes$it, trials, clase, alpha, maxSelectVars, popSize, tourSize, pxo, pmut, eliteSize, nroGens, stallGens,umbral, valInterna)
	interna=partes$it
	externa=partes$et
	load(archivo)
	save(maes_primero, resultados, externa, interna, file=archivo) 
	
	if(cod=="PS"){
		ventana_fase_dos(archivo)
	}
}




