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
    print(i)
    individual <- sample(seq(1,8,1),size = individual_size)
    population <- append(population,list(individual))
  }
  return (population)
}

generate_random_population()

#Fitness Function f(["SEND + "MORE] - "MONEY")
fitness <- function(individual_index){
  arcial_fitness <- c() 
  j <- 1
  for( i in letters){
    valor <- c()
    soma <- 0
    for(ii in unlist(strsplit(i, ""))){
      char_index <- match(ii,unlist(strsplit(words_sorted, "")))
      print(char_index)
      soma <- soma + population[[individual_index]][char_index]
    }
    parcial_fitness[j] <- soma
    j <- j +1
  }
  return ( parcial_fitness[1] + parcial_fitness[2] - parcial_fitness[3] )
}

population_fitness <- c()
population_fitness <- append(population_fitness,4)
population_fitness
