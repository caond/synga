# Example script for using the synga package
library(synga)
library(dplyr)
# Load example data
data(ed)

# Step 1: Create the model
ed_model <- create_model(ed, data_type)

# Step 2: Generate synthetic data
syn_cop <- generate_syn_copula(ed_model)
syn_decomp <- generate_syn_decomp(ed_model)

# Step 3: Generate ensemble model
syn_en <-  ensemble_ga(ed_model,syn_cop,syn_decomp,100,'frobenius')
syn_en_auc <-  ensemble_ga(ed_model,syn_cop,syn_decomp,100,'auc')

# Step 4: Compare synthetic data with original data
compare_dist(ed_model,syn_cop)
compare_dist(ed_model,syn_decomp)
compare_dist(ed_model,syn_en)
compare_dist(ed_model,syn_en_auc)


# Step 5: Compare synthetic data with original data
benchmark_cor(ed_model,syn_cop)
benchmark_cor(ed_model,syn_decomp)
benchmark_cor(ed_model,syn_en)
benchmark_cor(ed_model,syn_en_auc)

