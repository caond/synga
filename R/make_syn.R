make_syn<-function(df, data_type,method='ga',n_samples=NA, na.rm=FALSE,n_core=1)
{
  if (!(method %in% c('ga','cop','decomp'))){
      stop("\nMethod shoud be 'ga' or 'cop' or 'decomp'")
  }

  if (n_core>1)
  {
    cat("Initialise cores...", end = "")
    start_parallel_cluster(workers = n_core)  # Only needs to run once
    cl <- get_parallel_cluster()
  }


  cat("Creating model...", end = "")
  data_model <- create_model(df, data_type,na.rm,n_core)
  cat("Generating synthetic data...", end = "")
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
    syn<-generate_syn_copula(data_model,n_samples)
  }

  syn<-apply_logical_rules_to_data(syn,data_model$rules)


  if (n_core>1)
  {
    stop_parallel_cluster()
  }
  cat("Done!", end = "")
  return(
    list(
      data_model=data_model,
      syn=syn
    )
  )
}
