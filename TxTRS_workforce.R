#################################################################
##                       Workforce Model                       ##
#################################################################
# source("TxTRS_R_BModel revised.R")
# library(data.table)


get_wf_data <- function(ee_cont_cb = CB_EE_paycredit,
                        er_credit_cb = CB_ER_paycredit,
                        dr_current = dr_current_,
                        dr_new = dr_new_,
                        cola_current_retire = COLA_current_retire,
                        cola_current_retire_one = COLA_current_retire_one,
                        one_time_cola = one_time_cola_,
                        cola_current_active = COLA_current_active,
                        cola_new_active = COLA_new_active,
                        icr_smooth_period = icr_smooth_period_,
                        icr_floor = icr_floor_,
                        icr_cap = icr_cap_,
                        icr_upside_share = icr_upside_share_,
                        icr_return_vol = icr_return_vol_,
                        return_scen = return_scen_,
                        model_return = model_return_,
                        acr = ACR,
                        retire_refund_ratio = retire_refund_ratio_,
                        cal_factor = cal_factor_
                        ) {

#Get benefit data
benefit_data <- get_benefit_data(ee_cont_cb = ee_cont_cb, 
                                 er_credit_cb = er_credit_cb, 
                                 dr_current = dr_current, 
                                 dr_new = dr_new, 
                                 cola_current_retire = cola_current_retire, 
                                 cola_current_retire_one = cola_current_retire_one,
                                 one_time_cola = one_time_cola, 
                                 cola_current_active = cola_current_active, 
                                 cola_new_active = cola_new_active, 
                                 icr_smooth_period = icr_smooth_period, 
                                 icr_floor = icr_floor, 
                                 icr_cap = icr_cap, 
                                 icr_upside_share = icr_upside_share, 
                                 icr_return_vol = icr_return_vol, 
                                 return_scen = return_scen,
                                 model_return = model_return,
                                 acr = acr,
                                 retire_refund_ratio = retire_refund_ratio,
                                 cal_factor = cal_factor)

#Initialize empty workforce projection arrays
ea <- SalaryEntry$entry_age
age <- Age
year <- YearStart:(YearStart + ModelPeriod)   #test now, fix this later
term_year <- year
retire_year <- year


active_dim <- c(length(ea), length(age), length(year))
active_dim_names <- list(ea, age, year)

term_dim <- c(length(ea), length(age), length(year), length(term_year))
term_dim_names <- list(ea, age, year, term_year)

retire_dim <- c(length(ea), length(age), length(year), length(term_year), length(retire_year))
retire_dim_names <- list(ea, age, year, term_year, retire_year)

wf_active <- array(0, dim = active_dim, dimnames = active_dim_names)
wf_term <- array(0, dim = term_dim, dimnames = term_dim_names)
wf_refund <- wf_term
wf_retire <- array(0, dim = retire_dim, dimnames = retire_dim_names)


#Initial active population
active_int_df <- expand_grid(ea, age) %>%
  left_join(SalaryHeadCountData, by = c("ea" = "entry_age", "age" = "Age")) %>%
  replace(is.na(.), 0) %>%
  select(ea, age, Count)

active_int_matrix <- xtabs(Count ~ ea + age, active_int_df)

wf_active[,,1] <- active_int_matrix

# active_int_pop <- c(2000, 8000, 8000, 7000, 8000, 9000, 8000, 7000, 6000, 5000)
# 
# active_int_ea <- data.frame(ea = SalaryEntry$entry_age, age = SalaryEntry$entry_age, n.active = active_int_pop)
# 
# active_int <- expand_grid(ea, age) %>%
#   left_join(active_int_ea) %>%
#   replace(is.na(.), 0) %>%
#   pivot_wider(names_from = age, values_from = n.active) %>%
#   select(-1)
# 
# wf_active[,,1] <- as.matrix(active_int)

#Position matrix to add new hires
position_matrix <- expand_grid(ea, age) %>% 
  mutate(new = ifelse(ea == age, 1, 0)) 

position_matrix <- xtabs(new ~ ea + age, position_matrix)

##Create probability array

#Mortality probability array (4 dimensions)
mort_df_term <- expand_grid(ea, age, year, term_year) %>% 
  left_join(MortalityTable, by = c("ea" = "entry_age", "age" = "Age", "year" = "Years", "term_year")) %>% 
  mutate(mort = ifelse(is.na(mort), 0, mort))

mort_array_term <- xtabs(mort ~ ea + age + year + term_year, mort_df_term)

#Separation probability array (3 dimensions): 
sep_df <- expand_grid(ea, age, year) %>% 
  mutate(ey = year - (age - ea)) %>% 
  left_join(SeparationRates, by = c("ea" = "entry_age", "age" = "Age", "ey" = "EntryYear")) %>% 
  select(ea, age, year, SepRate) %>% 
  mutate(SepRate = ifelse(is.na(SepRate), 0, SepRate))

sep_array <- xtabs(SepRate ~ ea + age + year, sep_df)

#Refund and retirement probability arrays
#Determine the optimal retirement age
optimal_retire <- benefit_data$final_tab %>% 
  rename(term_age = Age) %>% 
  select(EntryYear, entry_age, term_age, YOS, retire_age, ben_decision) %>% 
  mutate(refund = case_when(ben_decision == "refund" ~ 1,     #use case_when instead of ifselse to handle NA values better
                            ben_decision == "mix" ~ 1 - retire_refund_ratio,
                            TRUE ~ 0),
         retire = case_when(ben_decision == "retire" ~ 1,
                            ben_decision == "mix" ~ 1,
                            TRUE ~ 0),
         refund_age = term_age)

#Retire probability array (4 dimensions)
retire_df <- expand_grid(ea, age, year, term_year) %>% 
  mutate(entry_year = year - (age - ea),
         term_age = age - (year - term_year),
         YOS = term_age - ea) %>% 
  filter(year - term_year >= 0, YOS >= 0) %>% 
  left_join(optimal_retire, by = c("ea" = "entry_age",
                                   "age" = "retire_age",
                                   "entry_year" = "EntryYear",
                                   "term_age",
                                   "YOS")) %>% 
  mutate(retire = ifelse(is.na(retire), 0, retire))

retire_array <- xtabs(retire ~ ea + age + year + term_year, retire_df) 

#Refund probability array (4 dimensions). Note that employees get refunds in the same year they get terminated. 
refund_df <- expand_grid(ea, age, year, term_year) %>% 
  mutate(entry_year = year - (age - ea),
         term_age = age - (year - term_year),
         YOS = term_age - ea) %>% 
  filter(year - term_year >= 0, YOS >= 0) %>% 
  left_join(optimal_retire, by = c("ea" = "entry_age",
                                   "age" = "refund_age",
                                   "entry_year" = "EntryYear",
                                   "term_age",
                                   "YOS")) %>% 
  mutate(refund = ifelse(is.na(refund), 0, refund))

refund_array <- xtabs(refund ~ ea + age + year + term_year, refund_df)

#Transition matrix to shift the population to the right by 1 age after 1 year
TM <-  diag(length(age) + 1)[-1, -(length(age) + 1)] 


#Workforce projection
for (i in 2:length(year)) {
  active2term <- wf_active[,,i-1] * sep_array[,,i-1]   #calculate the # of newly terminated actives. 2-dimensional array
  
  wf_active[,,i] <- (wf_active[,,i-1] - active2term) %*% TM  #deduct terminated members from the active workforce and shift the wf_active matrix to the right by one year
  
  new_entrants <- add_new_entrants(g = pop_growth_, ne_dist = SalaryEntry$entrant_dist, wf1 = wf_active[,,i-1],
                                   wf2 = wf_active[,,i], ea = ea, age = age, position_matrix = position_matrix)  #new entrants matrix to be added to the active workforce

  wf_active[,,i] = wf_active[,,i] + new_entrants  #add new entrants
  
  term2death <- wf_term[,,i-1,] * mort_array_term[,,i-1,] #3-dimensional array
  
  wf_term[,,i,] <- apply(wf_term[,,i-1,] - term2death, 3, function(x) x %*% TM) %>% array(term_dim[-3]) 
  
  wf_term[,,i,i] <- active2term %*% TM   #add newly terminated members the term population
  
  term2refund <- wf_term[,,i,i] * refund_array[,,i,i]  #calculate the # of newly refunded members. 2-dimensional array
  
  wf_term[,,i,i] <- wf_term[,,i,i] - term2refund
  
  wf_refund[,,i,i] <- term2refund
  
  term2retire <- wf_term[,,i,] * retire_array[,,i,]  #calculate the # of newly retired members. 3-dimensional array
  
  wf_term[,,i,] <- wf_term[,,i,] - term2retire
  
  retire2death <- apply(wf_retire[,,i-1,,], 4, function(x) x * mort_array_term[,,i-1,]) %>% array(retire_dim[-3])   #4-dimensional array
  
  wf_retire[,,i,,] <- apply(wf_retire[,,i-1,,] - retire2death, c(3,4), function(x) x %*% TM) %>% array(retire_dim[-3])
  
  wf_retire[,,i,,i] <- term2retire
  
}


#####Convert the multidimensional arrays into data frames 
wf_active_df <- data.frame(expand.grid(ea = ea, age = age, year = year), n.active = as.vector(wf_active)) %>% filter(age >= ea)

wf_term_df <- data.frame(expand.grid(ea = ea, age = age, year = year, term_year = term_year), n.term = as.vector(wf_term)) %>% 
  filter(age >= ea, year >= term_year)

wf_refund_df <- data.frame(expand.grid(ea = ea, age = age, year = year, term_year = term_year), n.refund = as.vector(wf_refund)) %>% 
  filter(age >= ea, year >= term_year)

#Since the wf_retire array is too big to handle using the above method, we need to split it into smaller parts for processing
wf_retire_list <- list()  #empty list to save retire workforce data in the for loop

for (i in seq_along(SalaryEntry$entry_age)) {
  wf_retire_name <- paste0("wf_retire_", SalaryEntry$entry_age[i])
  assign(wf_retire_name, wf_retire[i,,,,])
  wf_retire_i <- data.table(CJ(retire_year, term_year, year, age), n.retire = as.vector(get(wf_retire_name)))[n.retire > 0,] %>% 
    mutate(ea = SalaryEntry$entry_age[i])
  assign(wf_retire_name, wf_retire_i)   #do this to save memory space
  wf_retire_list <- append(wf_retire_list, list(get(wf_retire_name)))
}

#Combine all retire data frames from the retire list into one retire data frame 
wf_retire_df <- rbindlist(wf_retire_list) %>% 
  select(ea, age, year, term_year, retire_year, n.retire)


wf_data <- list(wf_active_df = wf_active_df,
                wf_term_df = wf_term_df,
                wf_refund_df = wf_refund_df,
                wf_retire_df = wf_retire_df)


saveRDS(wf_data, "wf_data.rds")

}
#################################################################
##                        Liability Model                        ##
#################################################################

# benefit_data <- get_benefit_data()
# 
# get_funding_data <- function(benefit_data = benefit_data, 
#                              db_new_ratio = 1, 
#                              cb_new_ratio = 0) {
# 
# #Plan design ratio for new hires:
# # DB_member_ratio <- 1
# # CB_member_ratio <- 0
# 
# 
# #Join wf active table with FinalData table to calculate the overall payroll, normal costs, PVFB, and PVFS each year
# wf_active_df_final <- wf_active_df %>% 
#   mutate(entry_year = year - (age - ea)) %>% 
#   left_join(FinalData, by = c("ea" = "entry_age", "age" = "Age", "year" = "Years", "entry_year" = "EntryYear")) %>% 
#   select(ea, age, year, entry_year, n.active, normal_cost_DB, normal_cost_CB, Salary, PVFB_DB, PVFB_CB, PVFNC_DB, PVFNC_CB, PVFS) %>% 
#   replace(is.na(.), 0) %>% 
#   # filter(n.active > 0) %>% 
#   #allocate members to plan designs based on entry year
#   mutate(n.active_DB_legacy = ifelse(entry_year <= YearStart, n.active, 0),
#          n.active_DB_new = ifelse(entry_year <= YearStart, 0, n.active * db_new_ratio),
#          n.active_CB_new = ifelse(entry_year <= YearStart, 0, n.active * cb_new_ratio)) %>% 
#   group_by(year) %>% 
#   summarise(
#     #Payroll
#     payroll_DB_legacy_est = sum(Salary * n.active_DB_legacy),
#     payroll_DB_new_est = sum(Salary * n.active_DB_new),
#     payroll_CB_new_est = sum(Salary * n.active_CB_new),
#     payroll_est = sum(Salary * n.active),
#     #Normal cost rates
#     nc_rate_DB_legacy_est = ifelse(payroll_DB_legacy_est == 0, 0, sum(normal_cost_DB * Salary * n.active_DB_legacy) / sum(Salary * n.active_DB_legacy)),
#     nc_rate_DB_new_est = ifelse(payroll_DB_new_est == 0, 0, sum(normal_cost_DB * Salary * n.active_DB_new) / sum(Salary * n.active_DB_new)),
#     nc_rate_CB_new_est = ifelse(payroll_CB_new_est == 0, 0, sum(normal_cost_CB * Salary * n.active_CB_new) / sum(Salary * n.active_CB_new)),
#     #Present value of future benefits
#     PVFB_DB_legacy_est = sum(PVFB_DB * n.active_DB_legacy),
#     PVFB_DB_new_est = sum(PVFB_DB * n.active_DB_new),
#     PVFB_CB_new_est = sum(PVFB_CB * n.active_CB_new),
#     #Present value of future normal costs
#     PVFNC_DB_legacy_est = sum(PVFNC_DB * n.active_DB_legacy),
#     PVFNC_DB_new_est = sum(PVFNC_DB * n.active_DB_new),
#     PVFNC_CB_new_est = sum(PVFNC_CB * n.active_CB_new),
#     #Count of active members
#     n.active = sum(n.active)
#     ) %>% 
#   ungroup() %>% 
#   mutate(nc_rate_est = (nc_rate_DB_legacy_est * payroll_DB_legacy_est + nc_rate_DB_new_est * payroll_DB_new_est + nc_rate_CB_new_est * payroll_CB_new_est) / payroll_est,
#          AAL_active_DB_legacy_est = PVFB_DB_legacy_est - PVFNC_DB_legacy_est,
#          AAL_active_DB_new_est = PVFB_DB_new_est - PVFNC_DB_new_est,
#          AAL_active_CB_new_est = PVFB_CB_new_est - PVFNC_CB_new_est) %>% 
#   replace(is.na(.), 0) 
#   
#   
# #Calculate PVFB for term vested members
# #The calculation of the PVFB for term vested CB members is more complicated than that for DB members because actual investment returns and hence actual ICR can affect the cash balance after termination. 
# wf_term_df_final <- wf_term_df %>% 
#   filter(n.term > 0) %>% 
#   mutate(entry_year = year - (age - ea)) %>% 
#   #join FinalData to get DBWealth (the present value of benefits at termination)
#   left_join(FinalData, by = c("ea" = "entry_age", "term_year" = "Years", "entry_year" = "EntryYear")) %>% 
#   select(ea, age, year, term_year, entry_year, retire_age, n.term, DBWealth) %>% 
#   # left_join(AnnFactorData, by = c("ea" = "entry_age", "age" = "Age", "year" = "Years", "term_year", "entry_year" = "EntryYear")) %>% 
#   #join BenefitsTable to get the actual cash balance at current age, the surv_DR at current age, and the surv_ICR at current age
#   left_join(BenefitsTable, by = c("ea" = "entry_age", "age" = "retire_age", "year" = "Years", "term_year", "entry_year" = "EntryYear")) %>% 
#   select(ea, age, year, term_year, entry_year, retire_age, n.term, DBWealth, CBBalance_final, surv_DR, surv_ICR) %>%
#   #rename to clarify variables' meanings
#   rename(CBBalance_current = CBBalance_final, 
#          surv_DR_current = surv_DR,
#          surv_ICR_current = surv_ICR) %>%
#   #join AnnFactorData to obtain various actuarial factors for the CB calculation
#   left_join(AnnFactorData, by = c("ea" = "entry_age", "retire_age" = "Age", "term_year", "entry_year" = "EntryYear")) %>% 
#   select(ea, age, year, term_year, entry_year, retire_age, n.term, DBWealth, CBBalance_current, surv_DR_current, surv_DR, 
#          surv_ICR_current, surv_ICR, AnnuityFactor_ACR, AnnuityFactor_DR) %>% 
#   #rename to clarify variables' meanings
#   rename(surv_DR_retire = surv_DR,
#          surv_ICR_retire = surv_ICR) %>% 
#   
#   mutate(
#     #PVFB_term_DB = First DB benefit * annuity factor at retirement * surv_DR at retirement / surv_DR at current time
#     #Note that DBWealth (PV at termination) = First DB benefit * annuity factor at retirement * surv_DR at retirement
#     PVFB_term_DB = DBWealth / surv_DR_current,
#     
#     #First, the actual CBBalance at current time needs to be projected to retirement age using expected ICR: 
#     #CBBalance_retire = CBBalance_current / (surv_ICR_retire/surv_ICR_current)
#     #Then convert the CBBalance_retire into retirement annuity using the ACR
#     #Annuity_CB = CBBalance_retire / AnnuityFactor_ACR
#     #Finally, discount the CB annuity back to current time to get the PVFB_term_CB
#     #PVFB_term_CB = Annuity_CB * AnnuityFactor_DR * surv_DR_retire / surv_DR_current
#     PVFB_term_CB = CBBalance_current / (surv_ICR_retire/surv_ICR_current) / AnnuityFactor_ACR * AnnuityFactor_DR * surv_DR_retire / surv_DR_current,
#     n.term_DB_legacy = ifelse(entry_year <= YearStart, n.term, 0),
#     n.term_DB_new = ifelse(entry_year <= YearStart, 0, n.term * db_new_ratio),
#     n.term_CB_new = ifelse(entry_year <= YearStart, 0, n.term * cb_new_ratio)
#   ) %>% 
#   group_by(year) %>% 
#   summarise(AAL_term_DB_legacy_est = sum(PVFB_term_DB * n.term_DB_legacy),
#             AAL_term_DB_new_est = sum(PVFB_term_DB * n.term_DB_new),
#             AAL_term_CB_new_est = sum(PVFB_term_CB * n.term_CB_new)) %>% 
#   ungroup()
# 
# 
# #Join wf refund table with benefit table to calculate the overall refunds each year
# wf_refund_df_final <- wf_refund_df %>% 
#   filter(n.refund > 0) %>% 
#   mutate(entry_year = year - (age - ea)) %>% 
#   left_join(BenefitsTable, by = c("ea" = "entry_age", "age" = "retire_age", "year" = "Years", "term_year", "entry_year" = "EntryYear")) %>% 
#   select(ea, age, year, term_year, entry_year, n.refund, DBEEBalance, CBBalance) %>% 
#   #allocate members to plan designs based on entry year
#   mutate(n.refund_DB_legacy = ifelse(entry_year <= YearStart, n.refund, 0),
#          n.refund_DB_new = ifelse(entry_year <= YearStart, 0, n.refund * db_new_ratio),
#          n.refund_CB_new = ifelse(entry_year <= YearStart, 0, n.refund * cb_new_ratio)) %>% 
#   group_by(year) %>% 
#   summarise(refund_DB_legacy_est = sum(DBEEBalance * n.refund_DB_legacy),
#             refund_DB_new_est = sum(DBEEBalance * n.refund_DB_new),
#             refund_CB_new_est = sum(CBBalance * n.refund_CB_new)) %>% 
#   ungroup()
# 
# 
# 
# #Join wf retire table with benefit table to calculate the overall retirement benefits each year
# wf_retire_df_final <- wf_retire_df %>% 
#   mutate(entry_year = year - (age - ea)) %>%    
#   left_join(BenefitsTable, by = c("ea" = "entry_age", "entry_year" = "EntryYear", "term_year", "retire_year" = "Years")) %>% 
#   select(ea, age, year, term_year, retire_year, entry_year, n.retire, DB_Benefit, CB_Benefit, COLA) %>% 
#   left_join(AnnFactorData %>% select(-COLA), by = c("ea" = "entry_age", "entry_year" = "EntryYear", "term_year", "year" = "Years")) %>% 
#   select(ea, age, year, term_year, retire_year, entry_year, n.retire, DB_Benefit, CB_Benefit, COLA, AnnuityFactor_DR) %>% 
#   rename(base_DB_benefit = DB_Benefit,
#          base_CB_benefit = CB_Benefit) %>% 
#   #let's ignore COLA for now 
#   #allocate members to plan designs based on entry year
#   mutate(
#     DB_benefit_final = base_DB_benefit * (1 + COLA)^(year - retire_year),
#     CB_benefit_final = base_CB_benefit * (1 + COLA)^(year - retire_year),
#     n.retire_DB_legacy = ifelse(entry_year <= YearStart, n.retire, 0),
#     n.retire_DB_new = ifelse(entry_year <= YearStart, 0, n.retire * db_new_ratio),
#     n.retire_CB_new = ifelse(entry_year <= YearStart, 0, n.retire * cb_new_ratio),
#     #We use "AnnuityFactor_DR - 1" below because the PVFB for retirees excludes the first payment (i.e. the first payment has already been delivered when the PVFB is calculated)
#     PVFB_retire_DB = DB_benefit_final * (AnnuityFactor_DR - 1),
#     PVFB_retire_CB = CB_benefit_final * (AnnuityFactor_DR - 1)
#     ) %>% 
#   group_by(year) %>% 
#   summarise(retire_ben_DB_legacy_est = sum(DB_benefit_final * n.retire_DB_legacy),
#             retire_ben_DB_new_est = sum(DB_benefit_final * n.retire_DB_new),
#             retire_ben_CB_new_est = sum(CB_benefit_final * n.retire_CB_new),
#             AAL_retire_DB_legacy_est = sum(PVFB_retire_DB * n.retire_DB_legacy),
#             AAL_retire_DB_new_est = sum(PVFB_retire_DB * n.retire_DB_new),
#             AAL_retire_CB_new_est = sum(PVFB_retire_CB * n.retire_CB_new)) %>% 
#   ungroup()
# 
# 
# 
# #Project benefit payments for current retirees
# retire_current_int <- RetireeDistribution %>% 
#   select(age, n.retire_ratio:total_ben_ratio) %>% 
#   mutate(n.retire_current = n.retire_ratio * retiree_pop_current,
#          total_ben_current = total_ben_ratio * ben_payment_current,
#          avg_ben_current = total_ben_current / n.retire_current,
#          Years = YearStart)
# 
# 
# wf_retire_current <- AnnFactorData_retire %>% 
#   left_join(retire_current_int, by = c("Age" = "age", "Years")) %>% 
#   select(base_age:AnnuityFactor_DR_retire, n.retire_current, avg_ben_current, total_ben_current) %>% 
#   group_by(base_age) %>% 
#   mutate(n.retire_current = recur_grow(n.retire_current, -mort),
#          avg_ben_current = recur_grow2(avg_ben_current, cola),
#          total_ben_current = n.retire_current * avg_ben_current,
#          #We use "AnnuityFactor_DR - 1" below because the PVFB for retirees excludes the first payment (i.e. the first payment has already been delivered when the PVFB is calculated)
#          PVFB_retire_current = avg_ben_current * (AnnuityFactor_DR_retire - 1)) %>% 
#   filter(!is.na(n.retire_current)) %>% 
#   ungroup()
#   
# wf_retire_current_final <- wf_retire_current %>% 
#   group_by(Years) %>% 
#   summarise(retire_ben_current_est = sum(total_ben_current),
#             AAL_retire_current_est = sum(n.retire_current * PVFB_retire_current)) %>% 
#   ungroup() %>% 
#   rename(year = Years)
# 
# 
# 
# #####Mini funding model
# funding_df <- wf_active_df_final %>% 
#   left_join(wf_term_df_final) %>% 
#   left_join(wf_refund_df_final) %>% 
#   left_join(wf_retire_df_final) %>% 
#   left_join(wf_retire_current_final) %>% 
#   replace(is.na(.), 0) %>% 
#   mutate(AAL_legacy_est = AAL_active_DB_legacy_est + AAL_term_DB_legacy_est + AAL_retire_DB_legacy_est + AAL_retire_current_est,
#          AAL_new_est = AAL_active_DB_new_est + AAL_active_CB_new_est + 
#            AAL_term_DB_new_est + AAL_term_CB_new_est + 
#            AAL_retire_DB_new_est + AAL_retire_CB_new_est,
#          AAL_est = AAL_legacy_est + AAL_new_est,  
#          tot_ben_refund_legacy_est = refund_DB_legacy_est + retire_ben_DB_legacy_est + retire_ben_current_est,
#          tot_ben_refund_new_est = refund_DB_new_est + refund_CB_new_est + retire_ben_DB_new_est + retire_ben_CB_new_est,
#          tot_ben_refund_est = tot_ben_refund_legacy_est + tot_ben_refund_new_est)
# 
# #Calculate liability gain/loss if any and project AAL using the roll forward method
# funding_df$liability_gain_loss_est <- 0
# funding_df$AAL_roll <- 0
# 
# for (i in 1:nrow(funding_df)) {
#   if (i == 1) {
#     funding_df$liability_gain_loss_est[i] <- 0
#     funding_df$AAL_roll[i] <- funding_df$AAL_est[i]
#   } else {
#     funding_df$liability_gain_loss_est[i] <- round(funding_df$AAL_est[i] - (funding_df$AAL_est[i-1]*(1 + ARR) + funding_df$payroll_est[i-1] * funding_df$nc_rate_est[i-1] - funding_df$tot_ben_refund_est[i]), digits = 1)
#     funding_df$AAL_roll[i] <- funding_df$AAL_roll[i-1]*(1 + ARR) + funding_df$payroll_est[i-1] * funding_df$nc_rate_est[i-1] - funding_df$tot_ben_refund_est[i] + funding_df$liability_gain_loss_est[i]
#   }
# }
# 
# #Check liability gain/loss
# #If the liability gain/loss isn't 0 under the perfect condition (experience = assumption), something must be wrong.
# funding_df$liability_gain_loss_est 
# 
# 
# }
# 
# 
# 
# write.csv(funding_df, "funding_df_TexsTRS_CB2.csv")


###################################################  TESTING   ###########################################


# retire_current_int_test <- retire_current_int %>% 
#   mutate(age_group = cut(age, c(seq(55, 100, 5),121), right = F)) %>% 
#   group_by(age_group) %>% 
#   summarise(n.retire_current = sum(n.retire_current),
#             total_ben_current = sum(total_ben_current)) %>% 
#   ungroup() %>% 
#   mutate(n.retire_current_ratio = n.retire_current / sum(n.retire_current),
#          total_ben_current_ratio = total_ben_current / sum(total_ben_current),
#          monthly_benefit = total_ben_current / n.retire_current / 12)
# 
# write.csv(retire_current_int_test, "retire_current_int_test.csv")
# wf_active_test <- wf_active_df %>% 
#   group_by(year, ea) %>% 
#   summarise(n.active = sum(n.active))
# 
# 
# ggplot(wf_active_test, aes(x = ea, y = n.active, col = year, group = year)) +
#   geom_line() + 
#   facet_wrap(~year)


# 
# wf_retire_test <- wf_retire_df %>% 
#   mutate(entry_year = year - (age - ea)) %>%    
#   left_join(BenefitsTable, by = c("ea" = "entry_age", "entry_year" = "EntryYear", "term_year", "retire_year" = "Years")) %>% 
#   select(ea, age, year, term_year, retire_year, entry_year, n.retire, DB_Benefit, CB_Benefit) %>% 
#   rename(base_DB_benefit = DB_Benefit,
#          base_CB_benefit = CB_Benefit) %>% #let's ignore COLA for now 
#   #allocate members to plan designs based on entry year
#   mutate(n.retire_DB = ifelse(entry_year < YearStart, n.retire, n.retire * db_new_ratio),
#          n.retire_CB = ifelse(entry_year < YearStart, 0, n.retire * cb_new_ratio)) %>% 
#   group_by(year, age) %>% 
#   summarise(n.retire = sum(n.retire),
#             total_ben = sum(base_DB_benefit * n.retire_DB),
#             avg_ben = sum(base_DB_benefit * n.retire_DB)/sum(n.retire_DB)) %>% 
#   ungroup() %>% 
#   group_by(year) %>% 
#   mutate(n.retire_ratio = n.retire / sum(n.retire),
#          total_ben_ratio = total_ben / sum(total_ben),
#          avg_ben_ratio = avg_ben / mean(avg_ben)) %>% 
#   ungroup()
#   
# ggplot(wf_retire_test %>% filter(year > 2100), aes(x = age, y = n.retire_ratio, col = year, group = year)) +
#   geom_line() +
#   facet_wrap(~year)
# 
# ggplot(wf_retire_test, aes(x = age, y = avg_ben, col = year, group = year)) +
#   geom_line() +
#   facet_wrap(~year)
# 
# ggplot(wf_retire_test, aes(x = age, y = avg_ben_ratio, col = year, group = year)) +
#   geom_line() +
#   facet_wrap(~year)
# 
# ggplot(wf_retire_test %>% filter(year > 2100), aes(x = age, y = total_ben_ratio, col = year, group = year)) +
#   geom_line() +
#   facet_wrap(~year)
# 
# wf_retire_test2 <- wf_retire_test %>% 
#   filter(year == YearStart + 100)
# 
# write.csv(wf_retire_test2, "wf_retire_test2.csv")

