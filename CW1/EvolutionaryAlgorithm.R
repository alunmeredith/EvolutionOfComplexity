# Assignment 1 from the evolution of complexity course (not assessed)
# http://sussed.soton.ac.uk/tag.895ea393565329bb.render.userLayoutRootNode.uP?uP_root=root&uP_sparam=activeTab&activeTab=u11l1s168&uP_tparam=frm&frm=frame

# Fitness function = number of matching characters
# Mutation rate = 1/L (L = # characters in target string)
# Selection process = Tournament size 2

# TARGET()
# converts the target string into integers and saves its length
target <- function(phrase)
{
  targetString <<- phrase # <<- saves variable globally
  # as.integer and charToRaw cast the string into different datatypes. 
  targetInt <<- as.integer(charToRaw(targetString))
  L <<- nchar(targetString)
}

# INITIAL()
# Creates N random individuals (columns), saved globally
initial <- function(N=500) {
  # runif generates numbers from a uniform density 
  # (rnorm,rbinom,rpois also also   generate numbers)
  # sapply applies a function over a vector and simplifies the data type of the result
  population <<- sapply(1:N,  function(x) floor(runif(L,32,127)))
}

# FITNESS()
# Compares individual to target and evaluates number of matching characters
fitness <- function(population) {
  mpopulation <- as.matrix(population)
  # apply, applies a function over the rows or columns of a matrix (2nd argument)
  apply(mpopulation, 2, function(x) sum((x - targetInt) == 0))
}

# MUTATION()
# Function that mutates randomly an average of 1 character
# Mutation is applied to an individual
mutation <- function(population, decayRate=1/L) {
  mpopulation <- as.matrix(population)
  apply(mpopulation, 2, function(individual)
    sapply(individual, function(x)
    ifelse(runif(1,0,1) < decayRate, x <- floor(runif(1,32,127)), x <- x)))
}

# INDIVIDUAL()
# Function to randomly pick n individuals from the population
# Returns a list of two elements, the individual and their index within the population
individual <- function(population, n=1) {
  x <- floor(runif(n, 1, ncol(population)+1)) # r uses 1 as its base index
  return(list(population[,x], x))
}

### HILL CLIMBER
hillClimber <- function(popSize=500, decayRate=(1/L), phrase="Methinks it is like a weasel")
{
  target(phrase)
  initial(1)
  itterationsHC = 0
  method = "hillClimber"
  # Loops until phrase is found
  while( fitness(population) != L) 
  {
    # Average of 1 mutation per cycle
    new <- mutation(population, decayRate) 
    if (fitness(new) > fitness(population)) {
      population <<- new
    }
    itterationsHC = itterationsHC + 1
    # Saves fitness every 1000 cycles
    if (itterationsHC %% 1000*popSize == 0) if(exists("j")) { 
      fit[itterationsHC/1000, j] <<- max(fitness(population))
    }
    # Break clause if too many cycles
    if (itterationsHC >= maxIter) break
  }
  # Print summary as solution found
  return(c(
    method,
    popSize,
    decayRate*L,
    phrase,
    rawToChar(as.raw(population[,which.max(fitness(population))])),
    itterationsHC,
    max(fitness(population)),
    j))
}

### Genetic Algorithm without crossover
# Note that crossover algorithm only mutates one individual per itteration
# Hillclimber mutates P (500) times per itteration
noCrossover <- function(popSize=500, decayRate=(1/L), phrase="Methinks it is like a weasel")
{
  # Convert phrase into numerical, initialise population, set itterations to 0. 
  target(phrase)
  initial(popSize)
  itterationsGA = 0
  method = "noCrossover"
  # Loops until phrase is found
  while( max(fitness(population)) != L) 
  {
    # Host a tournament to select individual to mutate
    parents <- individual(population, 2)
    child <- mutation(
      parents[[1]][,which.max(fitness(parents[[1]]))], decayRate)
    # Host a tournament to select an individual to be replaced
    tournament <- individual(population, 2)
    n <- tournament[[2]][which.min(fitness(tournament[[1]]))]
    population[,n] <<- child
    # Increase number of itterations
    itterationsGA = itterationsGA + 1
    # Saves fitness every 1000 cycles
    if (itterationsGA %% 1000*popSize == 0) if(exists("j")) {  
      fit[itterationsGA/1000, j] <<- max(fitness(population))
    }
    # Breaks after too many cycles
    if (itterationsGA >= maxIter) break
  }
  # Print summary as solution found
  return(c(
    method,
    popSize,
    decayRate*L,
    phrase,
    rawToChar(as.raw(population[,which.max(fitness(population))])),
    itterationsGA,
    max(fitness(population)),
    j))
}

### Genetic Algorithm with crossover
# Note that crossover algorithm only mutates one individual per itteration
# Hillclimber mutates 500 individuals per itteration
crossover <- function(popSize=500, decayRate=(1/L), phrase="Methinks it is like a weasel") 
{
  target(phrase)
  initial(popSize)
  itterationsGAC = 0
  method = "crossover"
  while( max(fitness(population)) != L) # Loops until phrase is found
  {
    # Host a tournament to select individual to mutate
    parentsTourn <- individual(population, 4)
    parent <- matrix(nrow = L, ncol = 2)
    parent[,1] <- parentsTourn[[1]][,which.max(fitness(parentsTourn[[1]][,1:2]))]
    parent[,2] <- parentsTourn[[1]][,which.max(fitness(parentsTourn[[1]][,3:4]))+2]
    # Crossover child and mutate
    crossover <- sample(c(1,2), L, replace = T)
    child = 0
    for (i in 1:L) child[i] = parent[i, crossover[i]]
    # Host a tournament to select an individual to be replaced
    tournament <- individual(population, 2)
    n <- tournament[[2]][which.min(fitness(tournament[[1]]))]
    population[,n] <<- mutation(child, decayRate)
    # Increase itterations
    itterationsGAC = itterationsGAC + 1
    # Saves fitness every 1000 cycles
    if (itterationsGAC %% 1000*popSize == 0) if(exists("j")) {  
      fit[itterationsGAC/1000, j] <<- max(fitness(population))
    # Breaks after too many cycles
    if (itterationsGAC >= maxIter) break
    }
  }
  # Print summary as solution found
  return(c(
    method,
    popSize,
    decayRate*L,
    phrase,
    rawToChar(as.raw(population[,which.max(fitness(population))])),
    itterationsGAC,
    max(fitness(population)),
    j))
}