#-------------------------SPLIT DATA---------------------------------------------------
dividir_dataset <- function()
{
#	setwd("C:/Users/Jime-Lio/Desktop/AD_Clasificacion");
	porc_train = 0.75;

	data <- read.csv("M9_HIA_classification.csv", header=TRUE) #HIA
		
	#split data
	library(caret)
	set.seed(2);
	indexes <- createDataPartition(data$HIA, p = porc_train, list = FALSE) #divide el dataset de manera estratificada HIA
	test = data[-indexes,]
	assign("test", data[-indexes,], envir = .GlobalEnv);
	train = data[indexes,]
	assign("train", data[indexes,], envir = .GlobalEnv);

		
	#guardar train y test en archivo csv para correr en weka
		
	name_file_train = paste0(path_train,i,".csv");
	name_file_test = paste0(path_test,i,".csv");
		
	write.csv(train, file = name_file_train);
	write.csv(test, file = name_file_test);
		
	
}