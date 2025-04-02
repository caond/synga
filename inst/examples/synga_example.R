# Example script for using the synga package
library(synga)
library(dplyr)
# Load example data
data(ed)


sm<-read.csv('c:/Users/he117862/Downloads/stomach_cancer_shared.csv')%>% select(-recurrence_date_of_onset)

sm_data_type<-list(person_ID='key',
                   sex='factor',
                   aboriginal_status='factor',
                   age='integer',
                   country_of_birth='factor',
                   person_last_known_postcode='factor',
                   diagnosis_postcode='factor',
                   diagnosis_year='integer',
                   tumour_site_code='factor',
                   grade='factor',
                   detection_method='factor',
                   age_at_death='integer',
                   cause_of_death_code='factor',
                   cause_of_death_due_to_tumour='factor',
                   cause_of_death_cancer_related='factor',
                   recurrence_date_of_onset='date')
#sm<- sm %>% mutate(age_at_death=if_else(is.na(age_at_death),0,age_at_death))
#sm<- sm%>% dplyr::select(cause_of_death_code,diagnosis_year, age_at_death)
#sm_data_type<-list(diagnosis_year='integer',
 #                  age_at_death='integer',
  #                 cause_of_death_code='factor')

# Step 1: Create the model

sm_model <- create_model(sm, sm_data_type,na.rm = TRUE)

# Step 2: Generate synthetic data
syn_cop <- generate_syn_copula(sm_model) # or syn_cop <- generate_syn_copula(sm_model,2000)
syn_decomp <- generate_syn_decomp(sm_model) # syn_decomp <- generate_syn_decomp(sm_mode,2000)
#summary(syn_decomp$country_of_birth)
#summary(sm_model$data_converted$country_of_birth)
# Step 3: Generate ensemble model
syn_en <-  ensemble_ga(sm_model,syn_cop,syn_decomp,100,'frobenius')
syn_en_auc <-  ensemble_ga(sm_model,syn_cop,syn_decomp,100,'auc')

# Step 4: Compare synthetic data with original data
compare_dist(sm_model,syn_cop)
compare_dist(sm_model,syn_decomp)
compare_dist(sm_model,syn_en)
compare_dist(sm_model,syn_en_auc)


# Step 5: Compare synthetic data with original data
benchmark_cor(sm_model,syn_cop)
benchmark_cor(sm_model,syn_decomp)
benchmark_cor(sm_model,syn_en)
benchmark_cor(sm_model,syn_en_auc)

