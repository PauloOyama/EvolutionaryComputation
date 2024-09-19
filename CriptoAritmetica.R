install.packages('dplyr')
library('dplyr')
#Set seed
set.seed(12)

# -------------------------------------------------------------------------#
#                               FUNCTIONS                                  #
# -------------------------------------------------------------------------#

# ------------------------#
#       POPULATION        #
# ------------------------#
#Generating Random Population
generate_random_population <- function(size){
  population <- list()
  for(i in seq(1,size,1)){
    individual <- sample(seq(0,9,1),size = individual_size)
    population <- append(population,list(individual))
  }
  return (population)
}

# ------------------------#
#       FITNESS           #
# ------------------------#
#Fitness Function f(["SEND + "MORE] - "MONEY")
fitness <- function(individual){
  parcial_fitness <- c() 
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

# ------------------------#
#       MUTATION          #
# ------------------------#
#Mutation - Swaping two index 
random_mutation <- function(individual,mutation_rate){
    if(sample(seq(1:10),1) <= mutation_rate){
      index_to_swap = sample(seq(1,8,1),2)
      flag <- individual[index_to_swap[1]]
      individual[index_to_swap[1]] <- individual[index_to_swap[2]]
      individual[index_to_swap[2]] <- flag
    }
      return (individual)
}

# ----------------------------------#
#       CROSSOVER - CYCLIC          #
# ----------------------------------#
# Check diff between set A and set B and return theirs index
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
cyclic_crossover <- function(a,b,TX_CROSS){
  #Crossover Rate - 60%
  if(sample(seq(1:10),1) <= TX_CROSS){
    individual_a <- a
    individual_b <- b
    cycle <- find_cycle(individual_a,individual_b)
    for(i in cycle){
      flag <- individual_a[i]
      individual_a[i] <- individual_b[i]
      individual_b[i] <- flag
    } 
    return (list(individual_a,individual_b))
  }
}

# ----------------------------------#
#       CROSSOVER - PMX             #
# ----------------------------------#
#Clean Cycles in OffSpring
repared_offsprint <- function (a,indexes_slice,mapping_swap){
  
  parent <- a
  preffix <- a[1:indexes_slice[1]-1]
  suffix  <- a[(indexes_slice[2]):length(a)] 
  for(x in preffix){
    if(x %in% mapping_swap[[1]]){
      index <- match(x,mapping_swap[[1]])
      preffix[match(x,preffix)] <- mapping_swap[[2]][index] 
    }
  }
  
  for(x in suffix){
    if(x %in% mapping_swap[[1]]){
      index <- match(x,mapping_swap[[1]])
      suffix[match(x,suffix)] <- mapping_swap[[2]][index] 
    }
  }
  
  return (list(preffix,suffix))
}

#Map OffSpring to be used in individual
map_offspring <- function(slice_a,slice_b){
  checkers <- c()
  for(x in slice_a){
    if(x %in% slice_b){
      checkers <- append(checkers,x)
    }
  }
  
  for( i in checkers){
    index_a <- match(i,slice_a)
    index_b <- match(i,slice_b)
    slice_a[index_a] <- slice_a[index_b]
    slice_a <- slice_a[-index_b]
    slice_b <- slice_b[-index_b]
  }
  
  return (list(c(slice_a,slice_b),c(slice_b,slice_a)))
}

pmx <- function(a,b,TX_CROSS){
  parent_one <- a
  parent_two <- b
  if(sample(seq(1,10),1) <= TX_CROSS){
    indexes_slice <- sort(sample(seq(1,length(parent_one),1),2))
    
    slice_a = a[indexes_slice[1]:(indexes_slice[2]-1)]
    slice_b = b[indexes_slice[1]:(indexes_slice[2]-1)]
    
    mapping_swap <- map_offspring(slice_a,slice_b)
    
    parts_parent_one <- repared_offsprint(parent_one,indexes_slice, mapping_swap)
    parts_parent_two <- repared_offsprint(parent_two,indexes_slice, mapping_swap)
    
    parent_one <- c(parts_parent_one[[1]],slice_b,parts_parent_one[[2]])
    parent_two <- c(parts_parent_two[[1]],slice_a,parts_parent_two[[2]])
    return (list(parent_one,parent_two))
  }
}



# ----------------------------------#
#       PARENTS  - TOURNMENT        #
# ----------------------------------#
# Tournament will return the index of the best candidate
tournament_with_three <- function(population_size){
  candidates <- sample(seq(1,population_size,1),3)
  candidates_fitness <- fitness_pop[candidates]
  return (candidates[which.max(candidates_fitness)])
}

# ----------------------------------#
#       PARENTS  - ROULETTE         #
# ----------------------------------#
#Roulette will return an index of the candidate
roulette <- function(population){
  fitness_pop <- unlist(lapply(population,fitness))
  fitness_pop <- unlist(lapply(fitness_pop, function(x) x + abs(fitness_pop[which.min(fitness_pop)])))
  roulette <- sum(fitness_pop)
  probability <- fitness_pop/roulette
  return (sample(seq(1,length(population),1),1,prob = probability))
}

# ----------------------------------#
#       REINSERTION - ORDER         #
# ----------------------------------#
#Take Parents and Childrens Together and take the bests
ordened_reinsertion <- function(population, childrens,population_size){
  all_population <- append(population,childrens)
  all_fitness <- unlist(lapply(all_population,fitness))
  best_fitness <- order(all_fitness,decreasing = TRUE)[1:population_size]
  return (all_population[sample(best_fitness)])
}

# ----------------------------------#
#       REINSERTION - ELITISMO      #
# ----------------------------------#
#Take the [TX_Elitism] (%)  better parents and the [1 - TX_Elitism] better childrens
elitism_reinsertion <- function(population, childrens,TX_Elitism){
  parents_fitness <- unlist(lapply(population,fitness))
  best_parents <- order(parents_fitness,decreasing = TRUE)[1:(population_size*TX_Elitism)]
  new_parents <- population[sample(best_parents)]
  
  complement_TX <- 1 - TX_Elitism
  childrens_fitness <- unlist(lapply(childrens,fitness))
  best_childrens <- order(childrens_fitness,decreasing = TRUE)[1:(population_size*complement_TX)]
  new_childrens <- childrens[sample(best_childrens)]
  return (c(new_parents,new_childrens))
}

# -------------------------------------------------------------------------#
#                               MAIN                                       #
# -------------------------------------------------------------------------#

# ----------------------------------#
#           PARAMENTERS             #
# ----------------------------------#
#Set of the characters of the words without repetition 
words <- 'SENDMORY'
#Length of the chromosomes
individual_size <- nchar(words)
#Population size 
population_size <- 50
#Number of generations
generation_interactions <- 50
#Words of the problems
letters <- c('SEND','MORE','MONEY')
#Set of characters sorted
words_sorted = paste(sort(unlist(strsplit(words, ""))), collapse = "")
#Mutation Rate
MT <- 1 #10%
#CrossOver Rate
TX_CROSS <- 6 #60%

# ----------------------------------#
#         Genetic Algorithm         #
# ----------------------------------#

#Step 1 Generate Initial Population
population <- generate_random_population(population_size)

#Step 2  - Avaliate Individual Performance
fitness_population <- unlist(lapply(population,fitness))

best_individual_count <- 0
best_individual_fitness <- 0
best_individual_SD <- 0
best_individual_MEAN <- 0
best_individual <- rep(0,8)
for( h in seq(1,generation_interactions,1)){

  #Step 3 - Stop Condition
  if(best_individual_count > 5) break
  if(best_individual_fitness == 30 && best_individual_MEAN == 30 && best_individual_SD == 0) break
  
  childrens <- c()
  for( x in seq(1,population_size,1)){
    #step 4 - Select parents 
    #Tournement
    #parents = c(tournament_with_three(population_size),tournament_with_three(population_size))
    #Roulete
    parents = c(roulette(population),roulette(population))
    
    #Step 5 - CrossOver
    #Cyclic
    children <- cyclic_crossover(population[[parents[1]]],population[[parents[2]]],TX_CROSS)
    #PMX
    #children <- pmx(population[[parents[1]]],population[[parents[2]]],TX_CROSS)
    #Doens't have childrens
    if(is.null(children)){
      next
    }
    
    #Step 6 - Mutation
    children[[1]] = random_mutation(children[[1]],MT)
    children[[2]] = random_mutation(children[[2]],MT)
    
    childrens <- append(childrens,children)
  }

  
  #Step 7 - Fitness Children
  childrens_fitness <- unlist(lapply(childrens, fitness))
  
  #Step 8 - Reinsertion
  #population <- ordened_reinsertion(population,childrens,population_size)
  population <- elitism_reinsertion(population,childrens,0.2)
  
  #METRICS - Save Metrics
  new_fitness <- unlist(lapply(population,fitness))
  index_better <- which.max(new_fitness)
  best_individual_fitness <- new_fitness[index_better]
  best_individual_SD  <- sd(new_fitness)
  best_individual_MEAN <- mean(new_fitness)
  
  #Basic logs
  cat('Generation =',h , ' With Fitness =', best_individual_fitness)
  cat(' Mean=',best_individual_MEAN,' Sd=',best_individual_SD,'\n')

  
  #Stop Condition By Individual Performance
  if (sum(best_individual == population[[index_better]]) != 8){
    best_individual <- population[[index_better]]
  }else{
    best_individual_count = best_individual_count + 1
  }
  
  cat('BEST =',best_individual,'\n')
}


