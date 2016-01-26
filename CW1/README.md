# Coursework 1: "Methinks it is like a weasel"
This coursework lays down the foundations of evolutionary algorithms and the foundational comparison between a hill climber. The phrase "Methinks it is like a weasel" is searched for through a hillclimber and natural selection algorithms with and without crossover. This coursework wasn't marked so the code may be more rough than other work. The main function of this work is to introduce the algorithmic foundations of a evolutionary algoritmh: initialising the population, selection, fitness and variation algorithms. 

Evolutionary algorithm uses a steady state population, tournament selection, uniform crossover, and unary fitness where applicable. 

## Primary Scripts
 * EvolutionaryAlgorithm.R
    * Defines the primary functions to be used within the evolutionary algorithm: Target(), Initial(), Fitness(), Mutation(), Individual(). 
    * Uses the above functions to implement a hillclimber and evolutionary algorithms with/without crossover parametised by the population size, decay rate and target phrase. 
 * Evolutionary Algorithm Scripting.R
   * This script runs through the functions implemented in EvolutionaryAlgorithm.R for a variety of parameters in order to answer the questions:
     1. Is it true that crossover is faster than no crossover
     2. If the problem was more difficult do you think the answer to the hypothesis would be the same?
     3. Does using a higher mutation rate affect the result? Why?
     4. How does the best GAs compare to the speed of the mutation hill-climber
 * EvolutionaryAlgorithmAnalysis.R
    * Takes the outputs of Evolutionary Algorithm Scripting.R to produce plots to answer each of the questions. Text analysis of these results are included in this script through comments. 
