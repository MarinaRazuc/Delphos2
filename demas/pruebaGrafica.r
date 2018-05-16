	cargaremos=function(){
	win1=gwindow(title="FirstPhase",  visible=FALSE, width=300, height=400)
	regresion=c("Linear Regression", "Non-Linear Regression", "Regression Trees", "k-Nearest Neighbours")
	clasificacion=c("Decision Trees", "k-Nearest Neighbours")
	group_432=ggroup(container=win1, horizontal=FALSE, width=250, height=30,visible=TRUE)

	#label432=glabel(" ", container=group_432)
	combobox4321=gcombobox(regresion, selected=3, editable=FALSE, container=group_432 , width=10, height=5, visible=TRUE, expand=TRUE)
	combobox4322=gcombobox(clasificacion, selected=1, editable=FALSE, container=group_432, width=10, height=5)
	visible(win1)=TRUE
	}