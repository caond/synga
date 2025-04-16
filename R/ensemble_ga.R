#' Generate an Ensemble using Genetic Algorithm
#'
#' This function uses a genetic algorithm to create a synthetic dataset by combining
#' two input datasets (`df1` and `df2`) based on a specified distance metric. It employs
#' crossover, mutation, and selection processes to evolve a population of candidate solutions.
#' The final ensemble is selected based on the fitness of the individuals in the population.
#'
#' @param data_model A data model that includes conversion functions and dataset handling.
#' @param df1 A data frame, the first source dataset.
#' @param df2 A data frame, the second source dataset.
#' @param distance A string specifying the distance metric to use. Default is "auc".
#'
#' @return A data frame representing the best synthetic dataset generated through the genetic algorithm.
#'
#' @details
#' The algorithm operates in multiple generations, each consisting of the following steps:
#' \itemize{
#'   \item Fitness evaluation of the population.
#'   \item Selection of the top half of the population.
#'   \item Crossover and mutation to create new individuals for the next generation.
#'   \item Elitism: The best individual is preserved across generations.
#' }
#' The fitness of each individual is determined by a distance function (such as AUC).
#' The final ensemble is derived from the individual with the best fitness score.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- ensemble_ga(data_model, df1, df2, distance = 'auc')
#' head(result)
#' }
#'
#' @import parallel
#' @import doParallel
#' @import foreach
#' @export
#'

ensemble_ga <- function(data_model, df1, df2, n_core) {
  distance<-'frobenius'
  GENERATIONS=100
  POPULATION_SIZE <- 100
  EARLY_STOP_GENERATION<-10
  MUTATION_RATE <- 0.1
  N<-NROW(df1)
  TARGET<-0
  if (distance=='auc') TARGET<- 0.5


  selected_distance_function <- distance_functions[[distance]]

  # convert into number so the distance model (eg. logistic regression can be run)
  df_n<-to_number(data_model,data_model$data_converted)
  df1_n<-to_number(data_model,df1)
  df2_n<-to_number(data_model,df2)

  # we also keep the actual row id from  two synthetic data
  df1<-df1%>%mutate(id_internal_key=row_number(),source_synthetic=1)
  df2<-df2%>%mutate(id_internal_key=row_number()+N,source_synthetic=2)

  # Compute distances between df and df1, df2
  df_df1 <- selected_distance_function(df_n,df1_n)
  df_df2 <- selected_distance_function(df_n,df2_n)
  # Compute weights based on distances
  weights <- 1 / (EPS + c(df_df1, df_df2))
  weights <- weights / sum(weights)
  w1 <- weights[1]
  w2 <- weights[2]

  # Generate initial population (random samples from df1 and df2)
  initialize_population <- function(){
    population <- list()
    for (i in 1: (POPULATION_SIZE-1)) {
      # Randomly select rows from df1 and df2
      choices <- sample(c(1, 2), size = N, replace = TRUE, prob = c(w1, w2))
      df3 <- rbind(df1[choices == 1, c('id_internal_key','source_synthetic') ],df2[choices == 2, c('id_internal_key','source_synthetic') ])
      population[[i]] <- df3[sample(1:N, N), ]  # Shuffle rows to mix data

    }
    if (df_df1<df_df2)
    {
      population[[POPULATION_SIZE]]<-df1[,c('id_internal_key','source_synthetic')]
    }
    else{
      population[[POPULATION_SIZE]]<-df2[,c('id_internal_key','source_synthetic')]
    }


    return(population)
  }

  # Crossover function: combining two `df3` candidates
  crossover <- function(parent1, parent2) {
    crossover_point <- sample(1:N, 1)  # Random split point

    # First part: Take from parent1
    child <- parent1[1:crossover_point, ]

    # Second part: Take unique rows from parent2
    remaining_rows <- parent2[(crossover_point + 1):N, ]
    remaining_rows <- remaining_rows[!(remaining_rows$id_internal_key %in% child$id_internal_key), ]  # Remove duplicates
    child <- rbind(child, remaining_rows)

    # How many more rows do we need?
    missing_count <- N - nrow(child)

    # If still missing rows, randomly sample from the full dataset (without replacement if possible)
    if (missing_count > 0) {

      # Combine both parents and remove the rows already used
      available_rows <- rbind(parent1, parent2)

      # Remove duplicate rows based on the 'id_internal_key' column (keeping only one of each id_internal_key)
      available_rows <- available_rows[!duplicated(available_rows$id_internal_key), ]
      # Remove already used rows
      available_rows <- available_rows[!(available_rows$id_internal_key %in% child$id_internal_key), ]
      extra_rows <- available_rows[sample(1:nrow(available_rows), missing_count, replace = FALSE), ]  # No duplicates
      child <- rbind(child, extra_rows)
    }

    return(child[complete.cases(child),])

  }


  # Mutation function: randomly swap a row from df1 with df2
  mutation <- function(df3) {
    if (runif(1) < MUTATION_RATE) {
      row_index <- sample(1:N, 1)
      # Swap with a random row from the dataset (df1 or df2) which not exists in df3

      # Identify rows from df1 and df2 that are not in df3
      available_rows <- rbind(df1, df2)
      available_rows <- available_rows[!(available_rows$id_internal_key %in% df3$id_internal_key), c('id_internal_key', 'source_synthetic')]  # Remove rows already in df3
      df3[row_index, ] <- available_rows[sample(1:NROW(available_rows), 1), c('id_internal_key', 'source_synthetic')]
    }
    return(df3)
  }

  # df_i= an individual population
  goodness_of_fit<-function(df_i,df1_n,df2_n){

    df_i_n<-rbind(
          df1_n[df_i[df_i$source_synthetic==1,'id_internal_key'],],
          df2_n[df_i[df_i$source_synthetic==2,'id_internal_key']-N,]
        )
    selected_distance_function(df_n,df_i_n)
  }


  population <- initialize_population()
  best_fitness <- Inf  # Initialize with a large value
  no_improvement_count <- 0  # Counter for consecutive generations with no improvement
  best_fit_idx<- 1

  for (gen in 1:GENERATIONS) {
    # Evaluate fitness of the population
    if (n_core>1)
    {
      fitness_scores <- foreach(individual = population, .combine = c) %dopar% {
        goodness_of_fit(individual, df1_n = df1_n, df2_n = df2_n)
      }
    }else{
      fitness_scores <- numeric(length(population))  # Initialize a numeric vector to store the results
      for (i in seq_along(population)) {
         fitness_scores[i] <- goodness_of_fit(population[[i]], df1_n = df1_n, df2_n = df2_n)
      }
    }
    best_fit_idx <- which.min(abs(fitness_scores - TARGET))
    current_best_fitness <- fitness_scores[best_fit_idx]

    # Print the best fitness score of the generation
    cat("\nGeneration:", gen, "Best Fit:", current_best_fitness, "\n")

    # Check if there is an improvement
    if (current_best_fitness < best_fitness) {
      best_fitness <- current_best_fitness
      no_improvement_count <- 0  # Reset counter
    } else {
      no_improvement_count <- no_improvement_count + 1
    }

    # Stop if no improvement for 10 generations
    if (no_improvement_count >= EARLY_STOP_GENERATION) {
      cat("Stopping early: No improvement for 10 generations.\n")
      break
    }

    # Selection: Choose the best half of the population
    top_k <- floor(POPULATION_SIZE / 2)  # Ensure integer value
    selected_indices <- order(fitness_scores, decreasing = FALSE)[1:top_k]
    selected_population <- population[selected_indices]

    # Elitism: Preserve the best individual
    next_population <- list()
    next_population[[1]] <- population[[best_fit_idx]]  # Carry over best solution

    # Create the next generation using crossover and mutation
    while (length(next_population) < POPULATION_SIZE) {
      # Randomly select two parents
      parents <- sample(selected_population, 2, replace = TRUE)
      parent1 <- parents[[1]]
      parent2 <- parents[[2]]

      # Crossover & Mutation
      child1 <- mutation(crossover(parent1, parent2))
      child2 <- mutation(crossover(parent2, parent1))

      # Add children to the new population
      next_population <- c(next_population, list(child1, child2))
    }

    # Ensure the new population size is exactly POPULATION_SIZE
    population <- next_population[1:POPULATION_SIZE]
  }

  # Final best individual after all generations
  df_i <- population[[best_fit_idx]]

  best_df_ensemble<-rbind(
    df1[df_i[df_i$source_synthetic==1,'id_internal_key'],],
    df2[df_i[df_i$source_synthetic==2,'id_internal_key']-N,]
  )

  return(best_df_ensemble[, setdiff(colnames(best_df_ensemble), c('id_internal_key', 'source_synthetic'))])
}
