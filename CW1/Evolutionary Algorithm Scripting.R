library(data.table)
fit <- data.frame()
j = 0
maxIter = 100000
results <- data.frame(method=character(),popSize=integer(),decayRate=numeric(),target=character(),solution=character(),iterations=numeric(),fitness=integer(),loopNum=integer(),stringsAsFactors = FALSE)
source("EvolutionaryAlgorithm.R")

## Scripting runs to test hypothesis

# Question 1. Is it true that crossover is faster than no crossover?
set.seed(99) #set the seed for reproducibility
for (i in 1:5)
{
  j <<- j + 1
  results[nrow(results)+1,] <- crossover()
  print(tail(results,1))
  
  j <<- j + 1
  results[nrow(results)+1,] <- noCrossover()
  print(tail(results,1)) 
}

# Question 1b. If the problem was more difficult do you think the answer to the hypothesis would be the same?
phrases = c("Methinks", "Methinks it", "Methinks it is like", "Methinks it is like a weasel")
for (k in 1:3){
  for (i in 1:length(phrases))
  {
    j <<- j + 1
    results[nrow(results)+1,] <- crossover(phrase=phrases[i])
    print(tail(results,1))
    
    j <<- j + 1
    results[nrow(results)+1,] <- noCrossover(phrase=phrases[i])
    print(tail(results,1))
  }
}


# Question 2. Does using a higher mutation rate affect the result? Why?
min = 5 # range of decay rates = 1/min average decays -> min average decays
for (i in c(.2,.4,.6,.8,1,2,3,4,5))
{
  j <<- j + 1
  results[nrow(results)+1,] <- crossover(decayRate=(i/(L)))
  print(tail(results,1))
  
  j <<- j + 1
  results[nrow(results)+1,] <- noCrossover(decayRate=(i/(L)))
  print(tail(results,1))
}

# Question 2b. How does the best of the GAs compare to the speed of the mutation hill-climber
for (i in 1:5)
{
  j <<- j + 1
  results[nrow(results)+1,] <- hillClimber()
  print(tail(results,1))
}