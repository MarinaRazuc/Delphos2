win3=gwindow(title = "PROBANDO", visible=TRUE, width=200, heigth=100) #visible=FALSE por ahi
group_1=ggroup(horizontal = TRUE, container=win3, width=200)
text1=gtext("Ingrese archivin", container=group_1, font.attr=list(style="bold"), width=100)
button1=gbutton("Seleccionar Archivo", container=group_1, 
				handler=function(h,...){
							file1=gfile()
							svalue(text1)=file1
							print(file1)
						})