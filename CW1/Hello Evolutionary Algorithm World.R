# Assignment 1 from the evolution of complexity course (not assessed)
# http://sussed.soton.ac.uk/tag.895ea393565329bb.render.userLayoutRootNode.uP?uP_root=root&uP_sparam=activeTab&activeTab=u11l1s168&uP_tparam=frm&frm=frame

# Fitness function = number of matching characters
# Mutation rate = 1/L (L = # characters in target string)
# Selection process = Tournament size 2

target <- function(phrase)
{
  targetString <<- phrase
  targetInt <<- as.integer(charToRaw(targetString))
  L <<- nchar(targetString)
}

# INITIAL()
# Creates N random individuals (columns), saved globally
initial <- function(N=500) {
  population <<- sapply(1:N,  function(x) floor(runif(L,32,127)))
}

# FITNESS()
# Compares individual to target and evaluates number of matching characters
fitness <- function(population) {
  mpopulation <- as.matrix(population)
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
hillClimber <- function(popSize=500, decayRate=1/L, phrase="Methinks it is like a weasel")
{
  target(phrase)
  initial(1)
  itterationsHC = 0
  while( fitness(population) != L) # Loops until phrase is found
  {
    new <- mutation(population, decayRate) # Average of 1 mutation per cycle
    if (fitness(new) > fitness(population)) {
      population <<- new
    }
    itterationsHC = itterationsHC + 1
    if (itterationsHC %% 1000*popSize == 0) if(exists("j")) { # Saves fitness every 1000 cycles
      fit[itterationsHC/1000, j] <<- max(fitness(population))
    }
    if (itterationsHC > 100000) return(fitness(population)) # Break clause if too many cycles
  }
  print(j)
  return(itterationsHC)
}

### Genetic Algorithm without crossover
# Note that crossover algorithm only mutates one individual per itteration
# Hillclimber mutates P (500) times per itteration
noCrossover <- function(popSize=500, decayRate=1/L, phrase="Methinks it is like a weasel")
{
  target(phrase)
  initial(popSize)
  itterationsGA = 0
  while( max(fitness(population)) != L) # Loops until phrase is found
  {
    # Host a tournament to select individual to mutate
    parents <- individual(population, 2)
    child <- mutation(
      parents[[1]][,which.max(fitness(parents[[1]]))], decayRate)
  
    # Host a tournament to select an individual to be replaced
    tournament <- individual(population, 2)
    n <- tournament[[2]][which.min(fitness(tournament[[1]]))]
    population[,n] <- child
    
    itterationsGA = itterationsGA + 1
    if (itterationsGA %% 1000*popSize == 0) if(exists("j")) {  # Saves fitness every 1000 cycles
      fit[itterationsGA/1000, j] <<- max(fitness(population))
    }
    if (itterationsGA > 100000) return(itterationsGA)  # Breaks after too many cycles
  }
  return(itterationsGA)
}

### Genetic Algorithm with crossover
# Note that crossover algorithm only mutates one individual per itteration
# Hillclimber mutates 500 individuals per itteration
crossover <- function(popSize=500, decayRate=1/L, phrase="Methinks it is like a weasel") 
{
  target(phrase)
  initial(popSize)
  itterationsGAC = 0
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
    population[,n] <- mutation(child, decayRate)
    
    itterationsGAC = itterationsGAC + 1
    if (itterationsGAC %% 1000*popSize == 0) if(exists("j")) {  # Saves fitness every 1000 cycles
      fit[itterationsGAC/1000, j] <<- max(fitness(population))
    if (itterationsGAC > 100000) return(itterationsGAC) # Breaks after too many cycles
    }
  }
  return(itterationsGAC)
}

