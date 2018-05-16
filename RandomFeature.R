leer_resultados_clasif <- function(pfile,cadena,palabra){
  library(stringr);
  library(cwhmisc);
  f = readLines(pfile);
  mline = grep(cadena,f,value=TRUE);
  mline = grep(cadena,f,value=TRUE);
  if (palabra=="Instances"){
    A<-cpos(mline,palabra, start=1) #position in string
    comienzo = A + 19;
    fin = A + 21;
  }else{
    A<-cpos(mline,palabra, start=1) #position in string
    comienzo = A + 29;
    fin = A + 35;
  }
  resultado<-substr(mline, start=comienzo, stop=fin);
  res = as.numeric(resultado[2]);
  return(res);
}

Random_Feature <- function(metodo,repeticiones,cant_descriptores, tipo)
{
# metodo=0 para linear regression
# metodo=1 para neural network
# metodo=2 para random forest
# metodo=3 para random committe

cant_repeticiones = repeticiones;
cant_descriptores_al_azar = cant_descriptores;

#archivos de entrada

# values_file = './Input/values_Modulo.csv'; #archivo csv con los valores de los descriptores m?s valores de la propiedad
# names_file = './Input/names_Modulo.xlsx'; #archivo excel (columna) con los nombres de los descriptores m?s el nombre de la propiedad
# property_values_file = './Input/property_values_Modulo.xlsx';
# 
# values_file_EV = './Input/EV_values_Modulo.csv'; #archivo csv con los valores de los descriptores m?s valores de la propiedad
# names_file_EV = './Input/EV_names_Modulo.xlsx'; #archivo excel (columna) con los nombres de los descriptores m?s el nombre de la propiedad
# property_values_file_EV = './Input/EV_property_values_Modulo.xlsx';

#path_file = "./Input/Train_BACE1.csv";
path_file = "./Input/train_modulo.csv";
datos = read.csv(path_file, header=TRUE);

#path_file_EV = "./Input/EV_BACE1.csv";
path_file_EV = "./Input/EV_modulo.csv";
datos_EV = read.csv(path_file_EV, header=TRUE);
cant_datos_EV = nrow(datos_EV);

if (tipo == "C"){
  accuracy = vector();
}else{
  coeficientes = vector();
}
property_name = names(datos)[ncol(datos)];

#comienzo con las repeticiones
for (i in 1:cant_repeticiones){
  print(paste0("Corrida ", i, collapse=""));
  set.seed(i);
  nros_azar = c(sample(col(datos)-1, cant_descriptores_al_azar),ncol(datos));
  nuevos_datos = datos[,nros_azar];
  nuevos_datos_EV =datos_EV[,nros_azar];

  output_file_weka = paste0("./Output_TXT/salida_RFS",property_name,"_",i,".txt", collapse="");
  
  #creo los archivos csv y arff
  pathcsv = paste0("./CSV_files/train_RFS_",property_name, "_",i,".csv", collapse="");
  patharff = paste0("./ARFF_files/train_RFS_",property_name,"_",i,".arff", collapse="");
  write.csv(nuevos_datos, file = pathcsv,row.names=FALSE);
  write.arff(nuevos_datos, file = patharff);
  
  pathcsv_EV = paste0("./CSV_files/EV_RFS_",property_name,"_",i,".csv", collapse="");
  patharff_EV = paste0("./ARFF_files/EV_RFS_",property_name,"_",i,".arff", collapse="");
  write.csv(nuevos_datos_EV, file = pathcsv_EV,row.names=FALSE);
  write.arff(nuevos_datos_EV, file = patharff_EV);
  
  #corro Weka
  if (metodo==0){#Linear Regression
    method = "LR";
    linea_LR  = paste0("java -Xmx1024M -cp ./weka.jar weka.classifiers.functions.LinearRegression -S 0 -R 1.0E-8 -num-decimal-places 4 -t ",patharff," -T ",patharff_EV," > ",output_file_weka, collapse="");
    shell(linea_LR);
  }else{
    if (metodo ==1){
      method = "NN";
      linea_NN  = paste0("java -Xmx1024M -cp ./weka.jar weka.classifiers.functions.MultilayerPerceptron -L 0.3 -M 0.2 -N 500 -V 0 -S 0 -E 20 -H a  -t ",patharff," -T ",patharff_EV," > ",output_file_weka, collapse="");
      shell(linea_NN);
    }else{
      if (metodo ==2){
        method = "RF";
        linea_RF  = paste0("java -Xmx1024M -cp ./weka.jar weka.classifiers.trees.RandomForest -P 100 -I 100 -num-slots 1 -K 0 -M 1.0 -V 0.001 -S 1 -t ",patharff," -T ",patharff_EV," > ",output_file_weka, collapse="");
        shell(linea_RF);      
      }else{
        if (metodo ==3){
          method = "RC";
          linea_RC  = paste0("java -Xmx1024M -cp ./weka.jar weka.classifiers.meta.RandomCommittee -S 1 -num-slots 1 -I 10 -t ",patharff," -T ",patharff_EV, " > ",output_file_weka, collapse="");
          shell(linea_RC);
        }else{
          print("Ingresar un método válido");
        }
      }
    }
  }

  
  if (tipo == "C"){
    percentage = leer_resultados_clasif(output_file_weka,"Correctly Classified Instances","Instances")*100/cant_datos_EV;
    accuracy = c(accuracy,percentage);
  }else{
    coef_corr = leer_resultados_clasif(output_file_weka,"Correlation coefficient","coefficient");
    coeficientes = c(coeficientes,coef_corr);
  }
  
}   

if (tipo == "C"){
  path_result_file=paste0("./Results Accuracy/ACC_",property_name,"_RandForest",method,".xlsx",collapse="");
  write.xlsx(accuracy,file=path_result_file);
}else{
  path_result_file=paste0("./Results Correlation Coefficient/CC_",property_name,"_RandForest",method,".xlsx",collapse="");
  write.xlsx(coeficientes,file=path_result_file);
}
}