GA1 = ga(type = "real-valued",
         fitness = fitness, min = 0, max = 1,
         popSize = 50, maxiter = 100, monitor = FALSE,
         seed = 12345)
		 
GA2 = ga(type = "real-valued", 
         fitness = fitness, min = 0, max = 1,
         popSize = 50, maxiter = 100, monitor = FALSE,
         seed = 12345, parallel = TRUE)
		 
GA3 = ga(type = "real-valued", 
         fitness = fitness, min = 0, max = 1,
         popSize = 50, maxiter = 100, monitor = FALSE,
		 seed = 12345, parallel = 2)
		 
GA4 = ga(type = "real-valued", 
         fitness = fitness, min = 0, max = 1,
         popSize = 50, maxiter = 100, monitor = FALSE,
         seed = 12345, parallel = "snow")