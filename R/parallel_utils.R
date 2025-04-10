# Environment to store the shared cluster
.parallel_env <- new.env(parent = emptyenv())

# Create and register the cluster (called once)
start_parallel_cluster <- function(workers = parallel::detectCores() - 1) {
  if (is.null(.parallel_env$cl)) {
    cl <- parallel::makeCluster(workers)
    doParallel::registerDoParallel(cl)
    .parallel_env$cl <- cl
  }
  invisible(.parallel_env$cl)
}

# Get the existing cluster (used by all internal functions)
get_parallel_cluster <- function() {
  if (is.null(.parallel_env$cl)) stop("Parallel cluster not initialized. Call start_parallel_cluster() first.")
  .parallel_env$cl
}

# Stop and clean up
stop_parallel_cluster <- function() {
  if (!is.null(.parallel_env$cl)) {
    parallel::stopCluster(.parallel_env$cl)
    .parallel_env$cl <- NULL
  }
}
