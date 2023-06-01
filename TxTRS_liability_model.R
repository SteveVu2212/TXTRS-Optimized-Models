#################################################################
##                       Liability Model                       ##
#################################################################

# benefit_data <- get_benefit_data()

get_liability_data <- function(
    ee_cont_cb = CB_EE_paycredit,
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
    cal_factor = cal_factor_,
    
    db_new_ratio = db_new_ratio_
  ) {
  
  
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
  
  
  
  #Plan design ratio for new hires:
  cb_new_ratio = 1 - db_new_ratio
  
  
  #Join wf active table with FinalData table to calculate the overall payroll, normal costs, PVFB, and PVFS each year
  wf_active_df_final <- wf_data$wf_active_df %>% 
    filter(year <= YearStart + ModelPeriod) %>% 
    mutate(entry_year = year - (age - ea)) %>% 
    left_join(benefit_data$final_tab, by = c("ea" = "entry_age", "age" = "Age", "year" = "Years", "entry_year" = "EntryYear")) %>% 
    select(ea, age, year, entry_year, n.active, normal_cost_DB, normal_cost_CB, Salary, PVFB_DB, PVFB_CB, PVFNC_DB, PVFNC_CB, PVFS) %>% 
    replace(is.na(.), 0) %>% 
    # filter(n.active > 0) %>% 
    #allocate members to plan designs based on entry year
    mutate(n.active_DB_legacy = ifelse(entry_year <= YearStart, n.active, 0),
           n.active_DB_new = ifelse(entry_year <= YearStart, 0, n.active * db_new_ratio),
           n.active_CB_new = ifelse(entry_year <= YearStart, 0, n.active * cb_new_ratio)) %>% 
    group_by(year) %>% 
    summarise(
      #Payroll
      payroll_DB_legacy_est = sum(Salary * n.active_DB_legacy),
      payroll_DB_new_est = sum(Salary * n.active_DB_new),
      payroll_CB_new_est = sum(Salary * n.active_CB_new),
      payroll_est = sum(Salary * n.active),
      #Normal cost rates
      nc_rate_DB_legacy_est = ifelse(payroll_DB_legacy_est == 0, 0, sum(normal_cost_DB * Salary * n.active_DB_legacy) / sum(Salary * n.active_DB_legacy)),
      nc_rate_DB_new_est = ifelse(payroll_DB_new_est == 0, 0, sum(normal_cost_DB * Salary * n.active_DB_new) / sum(Salary * n.active_DB_new)),
      nc_rate_CB_new_est = ifelse(payroll_CB_new_est == 0, 0, sum(normal_cost_CB * Salary * n.active_CB_new) / sum(Salary * n.active_CB_new)),
      #Present value of future benefits
      PVFB_DB_legacy_est = sum(PVFB_DB * n.active_DB_legacy),
      PVFB_DB_new_est = sum(PVFB_DB * n.active_DB_new),
      PVFB_CB_new_est = sum(PVFB_CB * n.active_CB_new),
      #Present value of future normal costs
      PVFNC_DB_legacy_est = sum(PVFNC_DB * n.active_DB_legacy),
      PVFNC_DB_new_est = sum(PVFNC_DB * n.active_DB_new),
      PVFNC_CB_new_est = sum(PVFNC_CB * n.active_CB_new),
      #Count of active members
      n.active = sum(n.active)
    ) %>% 
    ungroup() %>% 
    mutate(nc_rate_est = (nc_rate_DB_legacy_est * payroll_DB_legacy_est + nc_rate_DB_new_est * payroll_DB_new_est + nc_rate_CB_new_est * payroll_CB_new_est) / payroll_est,
           AAL_active_DB_legacy_est = PVFB_DB_legacy_est - PVFNC_DB_legacy_est,
           AAL_active_DB_new_est = PVFB_DB_new_est - PVFNC_DB_new_est,
           AAL_active_CB_new_est = PVFB_CB_new_est - PVFNC_CB_new_est) %>% 
    replace(is.na(.), 0) 
  
  
  #Calculate PVFB for term vested members
  #The calculation of the PVFB for term vested CB members is more complicated than that for DB members because actual investment returns and hence actual ICR can affect the cash balance after termination. 
  wf_term_df_final <- wf_data$wf_term_df %>% 
    filter(year <= YearStart + ModelPeriod) %>% 
    filter(n.term > 0) %>% 
    mutate(entry_year = year - (age - ea)) %>% 
    #join FinalData to get PV_DB_Benefit (the present value of benefits at termination)
    left_join(benefit_data$final_tab, by = c("ea" = "entry_age", "term_year" = "Years", "entry_year" = "EntryYear")) %>% 
    select(ea, age, year, term_year, entry_year, retire_age, n.term, PV_DB_Benefit) %>% 
    # left_join(AnnFactorData, by = c("ea" = "entry_age", "age" = "Age", "year" = "Years", "term_year", "entry_year" = "EntryYear")) %>% 
    #join BenefitsTable to get the actual cash balance at current age, the surv_DR at current age, and the surv_ICR at current age
    left_join(benefit_data$ben_tab %>% select(-PV_DB_Benefit), by = c("ea" = "entry_age", "age" = "retire_age", "year" = "Years", "term_year", "entry_year" = "EntryYear")) %>% 
    select(ea, age, year, term_year, entry_year, retire_age, n.term, PV_DB_Benefit, CBBalance_final, surv_DR, surv_ICR) %>%
    #rename to clarify variables' meanings
    rename(CBBalance_current = CBBalance_final, 
           surv_DR_current = surv_DR,
           surv_ICR_current = surv_ICR) %>%
    #join AnnFactorData to obtain various actuarial factors for the CB calculation
    left_join(benefit_data$ann_factor_tab, by = c("ea" = "entry_age", "retire_age" = "Age", "term_year", "entry_year" = "EntryYear")) %>% 
    select(ea, age, year, term_year, entry_year, retire_age, n.term, PV_DB_Benefit, CBBalance_current, surv_DR_current, surv_DR, 
           surv_ICR_current, surv_ICR, AnnuityFactor_ACR, AnnuityFactor_DR) %>% 
    #rename to clarify variables' meanings
    rename(surv_DR_retire = surv_DR,
           surv_ICR_retire = surv_ICR) %>% 
    
    mutate(
      #PVFB_term_DB = First DB benefit * annuity factor at retirement * surv_DR at retirement / surv_DR at current time
      #Note that PV_DB_Benefit (PV at termination) = First DB benefit * annuity factor at retirement * surv_DR at retirement
      PVFB_term_DB = PV_DB_Benefit / surv_DR_current,
      
      #First, the actual CBBalance at current time needs to be projected to retirement age using expected ICR: 
      #CBBalance_retire = CBBalance_current / (surv_ICR_retire/surv_ICR_current)
      #Then convert the CBBalance_retire into retirement annuity using the ACR
      #Annuity_CB = CBBalance_retire / AnnuityFactor_ACR
      #Finally, discount the CB annuity back to current time to get the PVFB_term_CB
      #PVFB_term_CB = Annuity_CB * AnnuityFactor_DR * surv_DR_retire / surv_DR_current
      PVFB_term_CB = CBBalance_current / (surv_ICR_retire/surv_ICR_current) / AnnuityFactor_ACR * AnnuityFactor_DR * surv_DR_retire / surv_DR_current,
      n.term_DB_legacy = ifelse(entry_year <= YearStart, n.term, 0),
      n.term_DB_new = ifelse(entry_year <= YearStart, 0, n.term * db_new_ratio),
      n.term_CB_new = ifelse(entry_year <= YearStart, 0, n.term * cb_new_ratio)
    ) %>% 
    group_by(year) %>% 
    summarise(AAL_term_DB_legacy_est = sum(PVFB_term_DB * n.term_DB_legacy),
              AAL_term_DB_new_est = sum(PVFB_term_DB * n.term_DB_new),
              AAL_term_CB_new_est = sum(PVFB_term_CB * n.term_CB_new)) %>% 
    ungroup()
  
  
  #Join wf refund table with benefit table to calculate the overall refunds each year
  wf_refund_df_final <- wf_data$wf_refund_df %>% 
    filter(year <= YearStart + ModelPeriod) %>% 
    filter(n.refund > 0) %>% 
    mutate(entry_year = year - (age - ea)) %>% 
    left_join(benefit_data$ben_tab, by = c("ea" = "entry_age", "age" = "retire_age", "year" = "Years", "term_year", "entry_year" = "EntryYear")) %>% 
    select(ea, age, year, term_year, entry_year, n.refund, DBEEBalance, CBBalance) %>% 
    #allocate members to plan designs based on entry year
    mutate(n.refund_DB_legacy = ifelse(entry_year <= YearStart, n.refund, 0),
           n.refund_DB_new = ifelse(entry_year <= YearStart, 0, n.refund * db_new_ratio),
           n.refund_CB_new = ifelse(entry_year <= YearStart, 0, n.refund * cb_new_ratio)) %>% 
    group_by(year) %>% 
    summarise(refund_DB_legacy_est = sum(DBEEBalance * n.refund_DB_legacy),
              refund_DB_new_est = sum(DBEEBalance * n.refund_DB_new),
              refund_CB_new_est = sum(CBBalance * n.refund_CB_new)) %>% 
    ungroup()
  
  
  
  #Join wf retire table with benefit table to calculate the overall retirement benefits each year
  wf_retire_df_final <- wf_data$wf_retire_df %>% 
    filter(year <= YearStart + ModelPeriod) %>% 
    mutate(entry_year = year - (age - ea)) %>%    
    left_join(benefit_data$ben_tab, by = c("ea" = "entry_age", "entry_year" = "EntryYear", "term_year", "retire_year" = "Years")) %>% 
    select(ea, age, year, term_year, retire_year, entry_year, n.retire, DB_Benefit, CB_Benefit, COLA) %>% 
    left_join(benefit_data$ann_factor_tab %>% select(-COLA), by = c("ea" = "entry_age", "entry_year" = "EntryYear", "term_year", "year" = "Years")) %>% 
    select(ea, age, year, term_year, retire_year, entry_year, n.retire, DB_Benefit, CB_Benefit, COLA, AnnuityFactor_DR) %>% 
    rename(base_DB_benefit = DB_Benefit,
           base_CB_benefit = CB_Benefit) %>% 
    #let's ignore COLA for now 
    #allocate members to plan designs based on entry year
    mutate(
      DB_benefit_final = base_DB_benefit * (1 + COLA)^(year - retire_year),
      CB_benefit_final = base_CB_benefit * (1 + COLA)^(year - retire_year),
      n.retire_DB_legacy = ifelse(entry_year <= YearStart, n.retire, 0),
      n.retire_DB_new = ifelse(entry_year <= YearStart, 0, n.retire * db_new_ratio),
      n.retire_CB_new = ifelse(entry_year <= YearStart, 0, n.retire * cb_new_ratio),
      #We use "AnnuityFactor_DR - 1" below because the PVFB for retirees excludes the first payment (i.e. the first payment has already been delivered when the PVFB is calculated)
      PVFB_retire_DB = DB_benefit_final * (AnnuityFactor_DR - 1),
      PVFB_retire_CB = CB_benefit_final * (AnnuityFactor_DR - 1)
    ) %>% 
    group_by(year) %>% 
    summarise(retire_ben_DB_legacy_est = sum(DB_benefit_final * n.retire_DB_legacy),
              retire_ben_DB_new_est = sum(DB_benefit_final * n.retire_DB_new),
              retire_ben_CB_new_est = sum(CB_benefit_final * n.retire_CB_new),
              AAL_retire_DB_legacy_est = sum(PVFB_retire_DB * n.retire_DB_legacy),
              AAL_retire_DB_new_est = sum(PVFB_retire_DB * n.retire_DB_new),
              AAL_retire_CB_new_est = sum(PVFB_retire_CB * n.retire_CB_new)) %>% 
    ungroup()
  
  
  
  #Project benefit payments for current retirees
  retire_current_int <- RetireeDistribution %>% 
    select(age, n.retire_ratio:total_ben_ratio) %>% 
    mutate(n.retire_current = n.retire_ratio * retiree_pop_current,
           total_ben_current = total_ben_ratio * ben_payment_current,
           avg_ben_current = total_ben_current / n.retire_current,
           Years = YearStart)
  
  
  wf_retire_current <- benefit_data$ann_factor_retire_tab %>% 
    filter(Years <= YearStart + ModelPeriod) %>% 
    left_join(retire_current_int, by = c("Age" = "age", "Years")) %>% 
    select(base_age:AnnuityFactor_DR_retire, n.retire_current, avg_ben_current, total_ben_current) %>% 
    group_by(base_age) %>% 
    mutate(n.retire_current = recur_grow(n.retire_current, -mort),
           avg_ben_current = recur_grow2(avg_ben_current, cola),
           total_ben_current = n.retire_current * avg_ben_current,
           #We use "AnnuityFactor_DR - 1" below because the PVFB for retirees excludes the first payment (i.e. the first payment has already been delivered when the PVFB is calculated)
           PVFB_retire_current = avg_ben_current * (AnnuityFactor_DR_retire - 1)) %>% 
    filter(!is.na(n.retire_current)) %>% 
    ungroup()
  
  wf_retire_current_final <- wf_retire_current %>% 
    group_by(Years) %>% 
    summarise(retire_ben_current_est = sum(total_ben_current),
              AAL_retire_current_est = sum(n.retire_current * PVFB_retire_current)) %>% 
    ungroup() %>% 
    rename(year = Years)
  
  
  #Project benefit payments for current term vested members
  #Note that we use the original "dr_current_" in calculating the benefit payments so that any discount rate adjustment can work
  retire_ben_term <- PMT(r = dr_current_, nper = amo_period_term, pv = PVFB_term_current, g = payroll_growth_)
  
  year <- YearStart:(YearStart + ModelPeriod)
  amo_years_term <- (YearStart + 1):(YearStart + amo_period_term)
  retire_ben_term_est <- double(length = length(year))
  retire_ben_term_est[which(year %in% amo_years_term)] <- recur_grow3(retire_ben_term, payroll_growth_, amo_period_term)
  
  wf_term_current <- data.frame(year, retire_ben_term_est) %>% 
    mutate(AAL_term_current_est = roll_pv(rate = dr_current, g = payroll_growth_, nper = amo_period_term, pmt_vec = retire_ben_term_est))
  
  
  
  #####Funding model - liability side
  funding_df <- wf_active_df_final %>% 
    left_join(wf_term_df_final) %>% 
    left_join(wf_refund_df_final) %>% 
    left_join(wf_retire_df_final) %>% 
    left_join(wf_retire_current_final) %>% 
    left_join(wf_term_current) %>% 
    replace(is.na(.), 0) %>% 
    mutate(AAL_legacy_est = AAL_active_DB_legacy_est + AAL_term_DB_legacy_est + AAL_retire_DB_legacy_est + AAL_retire_current_est + AAL_term_current_est,
           AAL_new_est = AAL_active_DB_new_est + AAL_active_CB_new_est + 
             AAL_term_DB_new_est + AAL_term_CB_new_est + 
             AAL_retire_DB_new_est + AAL_retire_CB_new_est,
           AAL_est = AAL_legacy_est + AAL_new_est,  
           tot_ben_refund_legacy_est = refund_DB_legacy_est + retire_ben_DB_legacy_est + retire_ben_current_est + retire_ben_term_est,
           tot_ben_refund_new_est = refund_DB_new_est + refund_CB_new_est + retire_ben_DB_new_est + retire_ben_CB_new_est,
           tot_ben_refund_est = tot_ben_refund_legacy_est + tot_ben_refund_new_est)
  
  #Calculate liability gain/loss if any and project AAL using the roll forward method
  funding_df$liability_gain_loss_legacy_est <- 0
  funding_df$liability_gain_loss_new_est <- 0
  funding_df$liability_gain_loss_est <- 0
  
  funding_df$AAL_legacy_roll <- 0
  funding_df$AAL_new_roll <- 0
  funding_df$AAL_roll <- 0
  
  
  for (i in 1:nrow(funding_df)) {
    if (i == 1) {
      funding_df$liability_gain_loss_legacy_est[i] <- 0
      funding_df$liability_gain_loss_new_est[i] <- 0
      
      funding_df$AAL_legacy_roll[i] <- funding_df$AAL_legacy_est[i]
      funding_df$AAL_new_roll[i] <- funding_df$AAL_new_est[i]
      
    } else {
      
      funding_df$liability_gain_loss_legacy_est[i] <- round(funding_df$AAL_legacy_est[i] - (funding_df$AAL_legacy_est[i-1] * (1 + dr_current) + funding_df$payroll_DB_legacy_est[i-1] * funding_df$nc_rate_DB_legacy_est[i-1] - funding_df$tot_ben_refund_legacy_est[i]), digits = 1)
      funding_df$liability_gain_loss_new_est[i] <- round(funding_df$AAL_new_est[i] - (funding_df$AAL_new_est[i-1] * (1 + dr_new) + funding_df$payroll_DB_new_est[i-1] * funding_df$nc_rate_DB_new_est[i-1] + funding_df$payroll_CB_new_est[i-1] * funding_df$nc_rate_CB_new_est[i-1] - funding_df$tot_ben_refund_new_est[i]), digits = 1)
      
      funding_df$AAL_legacy_roll[i] <- funding_df$AAL_legacy_roll[i-1] * (1 + dr_current) + funding_df$payroll_DB_legacy_est[i-1] * funding_df$nc_rate_DB_legacy_est[i-1] - funding_df$tot_ben_refund_legacy_est[i] + funding_df$liability_gain_loss_legacy_est[i]
      funding_df$AAL_new_roll[i] <- funding_df$AAL_new_roll[i-1] * (1 + dr_new) + funding_df$payroll_DB_new_est[i-1] * funding_df$nc_rate_DB_new_est[i-1] + funding_df$payroll_CB_new_est[i-1] * funding_df$nc_rate_CB_new_est[i-1] - funding_df$tot_ben_refund_new_est[i] + funding_df$liability_gain_loss_new_est[i]
      
    }
  }
  
  funding_df$liability_gain_loss_est <- funding_df$liability_gain_loss_legacy_est + funding_df$liability_gain_loss_new_est
  funding_df$AAL_roll <- funding_df$AAL_legacy_roll + funding_df$AAL_new_roll
  
  #Check liability gain/loss
  #If the liability gain/loss isn't 0 under the perfect condition (experience = assumption), something must be wrong.
  # funding_df$liability_gain_loss_est 
  
  return(funding_df)
  
}

