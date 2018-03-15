library(GA)
fitness <- function(x)
{
  Sys.sleep(0.01)
  x*runif(1)
}

library(rbenchmark)
out <- benchmark(GA1 = ga(type = "real-valued", 
                          fitness = fitness, min = 0, max = 1,
                          popSize = 50, maxiter = 100, monitor = FALSE,
                          seed = 12345),
                 GA2 = ga(type = "real-valued", 
                          fitness = fitness, min = 0, max = 1,
                          popSize = 50, maxiter = 100, monitor = FALSE,
                          seed = 12345, parallel = TRUE),
                 GA3 = ga(type = "real-valued", 
                          fitness = fitness, min = 0, max = 1,
                          popSize = 50, maxiter = 100, monitor = FALSE,
                          seed = 12345, parallel = 2),
                 GA4 = ga(type = "real-valued", 
                          fitness = fitness, min = 0, max = 1,
                          popSize = 50, maxiter = 100, monitor = FALSE,
                          seed = 12345, parallel = "snow"),
                 columns = c("test", "replications", "elapsed", "relative"),
                 order = "test", 
                 replications = 10)
#out$average <- with(out, average <- elapsed/replications)
#out[,c(1:3,5,4)]