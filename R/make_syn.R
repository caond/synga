#' Generate Synthetic Data Using Copula and Decomposition Methods
#'
#' This function generates synthetic data from an input data frame using one of several methods:
#' copula-based modeling (`cop`), matrix decomposition (`decomp`), or a genetic algorithm ensemble of both (`ga`).
#' It can optionally apply NA logical rules to maintain structural consistency in the generated dataset.
#'
#' @param df A data.frame containing the original dataset to be modeled.
#' @param data_type A named vector or list indicating the type of each column (e.g., "numeric", "factor", "integer").
#' @param method The synthesis method to use. One of `"ga"`, `"cop"`, or `"decomp"`. Default is `"ga"`.
#' @param n_samples Number of synthetic samples to generate. If `NA`, the same number of rows as `df` is used.
#' @param na.rm Logical. Whether to remove rows with `NA` values before training the model. Default is `FALSE`.
#' @param na.rules Logical. Whether to apply NA propagation rules (e.g., if A is NA, then B must also be NA). Default is `TRUE`.
#' @param n_core Number of CPU cores to use for parallelization. Default is `1` (no parallelization).
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{data_model}}{The model object trained on the original data.}
#'   \item{\code{syn}}{The generated synthetic dataset.}
#'   \item{\code{compare_dist()}}{A function to compare the distributions of real vs synthetic data.}
#'   \item{\code{benchmark_cor()}}{A function to benchmark the correlation structure of real vs synthetic data.}
#' }
#'
#' @examples
#' \dontrun{
#' syn_obj <- make_syn(df, data_type, method = "cop", n_samples = 1000)
#' head(syn_obj$syn)
#' syn_obj$compare_dist()
#' }
#'
#' @export

make_syn<-function(df, data_type,method='ga',n_samples=NA, na.rm=FALSE, na.rules=TRUE, n_core=1)
{
  if (!(method %in% c('ga','cop','decomp'))){
      stop("\nMethod shoud be 'ga' or 'cop' or 'decomp'")
  }

  if (n_core>1)
  {
    cat("Initialise cores...", end = "\n")
    start_parallel_cluster(workers = n_core)  # Only needs to run once
    cl <- get_parallel_cluster()
  }


  cat("Creating model...", end = "\n")
  data_model <- create_model(df, data_type,na.rm,na.rules,n_core)
  cat("Generating synthetic data...", end = "\n")
  if (method=='ga')
  {

      syn_cop<-generate_syn_copula(data_model,n_samples)
      syn_decomp<-generate_syn_decomp(data_model,n_samples)
      syn<-ensemble_ga(data_model,syn_cop,syn_decomp,n_core)
  }

  if (method=='cop')
  {
    syn<-generate_syn_copula(data_model,n_samples)
  }

  if (method=='decomp')
  {
    syn<-generate_syn_decomp(data_model,n_samples)
  }
  if (na.rules) syn<-apply_na_logical_rules(syn,data_model$rules)


  if (n_core>1)
  {
    cat("Stop parallel core", end = "\n")
    stop_parallel_cluster()
  }
  cat("Done!", end = "\n")
  cat("Your synthetic data return as $syn", end = "\n")
  cat("Run $compare_dist() or $benchmar_cor() to validate the synthetic data", end = "\n")
  return(
    list(
      data_model=data_model,
      syn=syn,
      compare_dist=function(){
        compare_dist(data_model,syn)
      },
      benchmark_cor=function(){
        benchmark_cor(data_model,syn)
      }

    )
  )
}
