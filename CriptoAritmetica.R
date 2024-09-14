install.packages('dplyr')
library('dplyr')
#Set of the characters of the words without repetition 
words <- 'SENDMORY'
#Length of the chromosomes
individual_size <- nchar(words)
#Population size 
population_size <- 100
#Number of generations
generation_interactions <- 50
#Words of the problems
letters <- c('SEND','MORE','MONEY')
#Set of characters sorted
words_sorted = paste(sort(unlist(strsplit(words, ""))), collapse = "")
#Set seed
set.seed(12)

#Generating Random Population
generate_random_population <- function(){
  population <- list()
  for(i in seq(1,population_size,1)){
    individual <- sample(seq(0,9,1),size = individual_size)
    population <- append(population,list(individual))
  }
  return (population)
}


#Fitness Function f(["SEND + "MORE] - "MONEY")
fitness <- function(individual){
  arcial_fitness <- c() 
  j <- 1
  for( i in letters){
    valor <- c()
    soma <- 0
    for(ii in unlist(strsplit(i, ""))){
      char_index <- match(ii,unlist(strsplit(words_sorted, "")))
      soma <- soma + individual[char_index]
    }
    parcial_fitness[j] <- soma
    j <- j +1
  }
  return ( parcial_fitness[1] + parcial_fitness[2] - parcial_fitness[3] )
}

#Mutation - Swaping two index 
random_mutation <- function(individual_index,mutation_rate){
    if(sample(seq(1:10),1) <= mutation_rate){
      index_to_swap = sample(seq(1,8,1),2)
      flag <- population[[individual_index]][index_to_swap[1]]
      population[[individual_index]][index_to_swap[1]] <- population[[individual_index]][index_to_swap[2]]
      population[[individual_index]][index_to_swap[2]] <- flag
    }
      return (population[[individual_index]])
}

cyclic_crossover <- function(){
  initial_index <- sample(seq(1:8),1)
}


# Check diff between set A and set B
find_diff <- function(individual_a,individual_b){
  repeated <- c()
  for(x in individual_b){
    if(!x %in% individual_a ){
      repeated <- append(repeated,match(x,individual_b))
      if(length(repeated) == 2){
        break
      }
    }
  }
  return (repeated)
}

# Will find a cycle of length <= 8
find_cycle <- function(individual_a,individual_b){
  index_to_swap <- sample(seq(1:8),1)
  indexes <- c()
  indexes <- append(indexes,index_to_swap)
  count <- 1
  while(TRUE){
    swap_flag1 <- individual_a[index_to_swap]
    value_to_find <- individual_b[index_to_swap]
    index_to_swap <- match(value_to_find,individual_a)
    
    if(is.na(index_to_swap)){
      indexes_available <- find_diff(individual_b,individual_a)
      index_to_swap <- indexes_available[count]
      count = count + 1
    }
    if(indexes[1] == index_to_swap){
      break
    }
    indexes <- append(indexes,index_to_swap)
  }
  return (indexes)
}

#Croossover Cycling - Will return two children between index a and b
make_children <- function(index_ind_a,index_ind_b,population){
  #Crossover Rate - 60%
  if(sample(seq(1:10),1) <= 6){
    individual_a <- population[[index_ind_a]]
    individual_b <- population[[index_ind_b]]
    cycle <- find_cycle(individual_a,individual_b)
    for(i in cycle){
      flag <- individual_a[i]
      individual_a[i] <- individual_b[i]
      individual_b[i] <- flag
    } 
    return (list(individual_a,individual_b))
  }
}


tournament_with_three <- function(population){
  candidates <- sample(seq(1,population_size,1),3)
  candidates_fitness <- fitness_pop[sample(seq(1,population_size,1),3)]
  print(candidates)
  print(candidates_fitness)
  return (candidates[which.max(candidates_fitness)])
}


roulette <- sum(fitness_pop)
ap <- fitness_pop/roulette
sample()
population <- generate_random_population()
fitness_pop <- unlist(lapply(population, fitness))

#Parent 1 == 
#population[[1]] <- c(3,2,4,0,7,5,8,6)
#Parent 2 == 
#population[[2]] <- c(1,8,3,9,0,7,4,2)
#initial_index = 3


population[[1]] <- c(7,5,1,4,3,6,8,2)
population[[2]] <- c(3,4,8,7,5,2,6,1)

population_fitness <- c()
