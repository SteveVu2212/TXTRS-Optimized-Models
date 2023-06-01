rm(list = ls())


#Loading required libraries
library("readxl")
library(tidyverse)
library(zoo)
library(profvis)
library(data.table)
library(openxlsx)
library(Rcpp)



#Get actuarial and financial functions
sourceCpp("./Rcpp_functions.cpp")
source("utility_functions.R")

#Get model inputs and assumptions
source("TxTRS_model_inputs.R")


#Get benefit data and model
source("TxTRS_R_BModel revised.R")
# benefit_data_05 <- get_benefit_data(retire_refund_ratio = 0.2)
# final_data <- benefit_data_05$final_tab
# benefit_data_05$nc_agg


#Get workforce data (do this periodically only)
source("TxTRS_workforce.R")
#Run the get_wf_data function to produce the wf_data.rds only when the separation assumptions, mortality assumptions, retire/refund assumption, or any assumptions that affect the workforce projection are updated. 
# get_wf_data()


#Get liability model
source("TxTRS_liability_model.R")
wf_data <- readRDS("wf_data.rds")


#Get funding model
source("TxTRS_funding_model.R")






#################################################################################################
################################## TESTING ######################################################
# funding_baseline <- get_funding_data()
# funding_recession <- get_funding_data(return_scen = "recur_recession")
# 
# 
# baseline <- get_liability_data()
# 
# lower_dr_new_scen <- get_liability_data(dr_new = 0.05)
# 
# lower_dr_all <- get_liability_data(dr_current = 0.05, dr_new = 0.05)
# 
# cb_baseline <- get_liability_data(db_new_ratio = 0)
# 
# cb_complex <- get_liability_data(ee_cont_cb = 0.05,
#                                  er_credit_cb = 0.12, 
#                                  dr_current = 0.065,
#                                  dr_new = 0.05,
#                                  cola_current_retire = 0.02,
#                                  cola_current_retire_one = COLA_current_retire_one,
#                                  one_time_cola = F,
#                                  cola_current_active = 0.01,
#                                  cola_new_active = 0.025,
#                                  icr_smooth_period = 3,
#                                  icr_floor = 0.03,
#                                  icr_cap = 0.09,
#                                  icr_upside_share = 0.7,
#                                  icr_return_vol = 0.14,
#                                  acr = 0.05,  
#                                  # retire_refund_ratio = 0.7,
#                                  
#                                  db_new_ratio = 0.4)
# 
# write.csv(cb_baseline, "cb_baseline.csv")
# 
# 
# benefit_data <- get_benefit_data()

