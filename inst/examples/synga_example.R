# Example script for using the synga package
library(synga)
# Load example data
data(ed)
data(data_type)


# Step 1: Create the model
ed_syn <- make_syn(ed, data_type,na.rm=FALSE, n_core = 5)
# Step 2: Check synthetic data
print(ed_syn$syn)
ed_syn$compare_dist()
ed_syn$bechmark_cor()
