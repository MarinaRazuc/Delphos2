particiones=function(ndfr4,pex, pin, N){ #N numero para generar nombre de archivo
	
	

	for(i in 1:3){
			set.seed(i) 
			indices4=createDataPartition(ndfr4$V1, p=pex, list=FALSE) #p por parametro (validacion externa)
			externa=ndfr4[-indices4, ]
			interna=ndfr4[indices4, ]
			
			write("Externa ", file=paste(paste0('Externa_e_Interna_',N+i), ".txt"))
			write(indices4, file=paste(paste0('Externa_e_Interna_',N+i), ".txt"), append=TRUE)
			write("Interna ", file=paste(paste0('Externa_e_Interna_',N+i), ".txt"), append=TRUE)
			write(-indices4, file=paste(paste0('Externa_e_Interna_',N+i), ".txt"), append=TRUE)
			

			indices2=createDataPartition(interna$V1, p=pin, list=FALSE) # p por parametro (validacion interna)
			entrenamiento=interna[indices2, ]
			testeo=interna[-indices2, ]
			
			write("Entrenamiento ", file=paste(paste0('Testea_y_Entrena_',N+i), ".txt"))
			write(indices2, file=paste(paste0('Testea_y_Entrena_',N+i), ".txt"), append=TRUE)
			write("Testeo ", file=paste(paste0('Testea_y_Entrena_',N+i), ".txt"), append=TRUE)
			write(-indices2, file=paste(paste0('Testea_y_Entrena_',N+i), ".txt"), append=TRUE)
			
			
			#correr GA con entrenamiento y testeo
	}
}

