####################################
# TxTRS Normal Cost/Benefit Model #
####################################

# rm(list = ls())
# library("readxl")
# library(tidyverse)
# library(zoo)
# library(profvis)
# source("utility_functions.R")
#### Start the Timing
#profvis({






#Assigning numeric inputs
# model_inputs <- read_excel(FileName, sheet = 'Main')
# 
# for(i in 1:nrow(model_inputs)){
#   if(!is.na(model_inputs[i,2])){
#     assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
#   }
# }

#Assigning character inputs
# character_inputs <- read_excel(FileName, sheet = "Character Inputs")
# 
# for(i in 1:nrow(character_inputs)){
#   if(!is.na(character_inputs[i,2])){
#     assign(as.character(character_inputs[i,2]),as.character(character_inputs[i,3]))
#   }
# }




#Convert salary and headcount matrices into long-form format
SalaryMatrix_long <- SalaryMatrix %>% 
  pivot_longer(cols = -1, names_to = "YOS", values_to = "Salary")
HeadCountMatrix_long <- HeadCountMatrix %>% 
  pivot_longer(cols = -1, names_to = "YOS", values_to = "Count")

#Calculate salary cumulative growth 
SalaryGrowthYOS <- SalaryGrowthYOS %>% 
  mutate(sal_cum_growth = cumprod(1 + lag(salary_increase, default = 0)))

#Joining salary data, headcount data, and estimate entry salaries for existing employees
SalaryHeadCountData <- SalaryMatrix_long %>%
  left_join(HeadCountMatrix_long) %>% 
  mutate(YOS = as.numeric(YOS),
         CurrentYear = YearStart,
         entry_age = Age - YOS,
         EntryYear = CurrentYear - YOS) %>% 
  filter(Salary > 0, entry_age >= 18) %>% 
  left_join(SalaryGrowthYOS) %>% 
  #estimate the salary at entry-age
  mutate(entry_salary = Salary / sal_cum_growth) %>% 
  select(EntryYear, entry_age, Age, YOS, Count, entry_salary)


#Calculate entrant count distribution 
SalaryEntry <- SalaryEntry %>% 
  mutate(entrant_dist = Count/sum(Count)) %>% 
  select(-Count, -`22298`)



#TEST CB NORMAL COST
# NormalYOSI <- 1
# CB_vesting <- 1
# ACR <- ARR
# ICR <- ARR

################
# Main rule: Retirement Eligibility
################

#Grandfathered criteria
#To be grandfathered, one must have met at least one of the following conditions on or before 2005:
#1. at least 50 years old
#2. Age plus YOS >= 70
#3. YOS >= 25
IsGrandfathered <- function(Age, YOS, EntryYear, EntryAge){
  YOS_2005 <- pmin(YOS, 2005 - EntryYear)
  Age_2005 <- pmin(Age, 2005 - EntryYear + EntryAge)
  Check <- ifelse(EntryYear <= 2005 & 
                    (Age_2005 >= 50 |
                       Age_2005 + YOS_2005 >= 70 |
                       YOS_2005 >= 25), TRUE, FALSE)
  return(Check)
}

#Determine retirement eligibility
IsRetirementEligible_Normal <- function(Age, YOS, EntryYear){
  Check <- ifelse((Age >= NormalRetAgeI & YOS >= NormalYOSI) |
                    (EntryYear <= 2007 & YOS + Age >= NormalRetRule & YOS >= NormalYOSI) |
                    (EntryYear > 2007 & EntryYear <= 2009 & YOS + Age >= NormalRetRule & Age >= NormalRetRuleAgeI & YOS >= NormalYOSI) |   #for those hired after 2007 and vested as of 2014
                    (EntryYear > 2009 & YOS + Age >= NormalRetRule & Age >= NormalRetRuleAgeII & YOS >= NormalYOSI), TRUE, FALSE)          #for those not vested as of 2014
  return(Check)
}

IsRetirementEligible_Early <- function(Age, YOS, EntryYear){
  Check <- ifelse((Age >= ReduceRetAge & YOS >= NormalYOSI) |
                    (YOS >= EarlyRetYOS) |
                    (EntryYear > 2007 & YOS + Age >= NormalRetRule & YOS >= NormalYOSI), TRUE, FALSE)
  return(Check)
}


IsRetirementEligible <- function(Age, YOS, EntryYear){
  Check <- ifelse(IsRetirementEligible_Normal(Age, YOS, EntryYear) == T | IsRetirementEligible_Early(Age, YOS, EntryYear) == T, TRUE, FALSE)
  return(Check)
}


#Determine retirement type
RetirementType <- function(Age, YOS, EntryYear){
  Check = ifelse(IsRetirementEligible_Normal(Age, YOS, EntryYear) == T, "Normal",
                 ifelse(IsRetirementEligible_Early(Age, YOS, EntryYear) == T, "Early", "None"))
  
  return(Check)
}

#Determine separation type
SeparationType <- function(Age, YOS, EntryYear){
  Check <- ifelse(IsRetirementEligible(Age, YOS, EntryYear) == T, "retire",
                  ifelse(YOS >= 5, "term vested", "term non-vested"))
  
  return(Check)
}

# can_retire = IsRetirementEligible(Age = retire_age, YOS = YOS, EntryYear = EntryYear)
# lookup table for retirement type
memo_retirement_type <- CJ(Age, YOS, EntryYear)
memo_retirement_type[, can_retire := IsRetirementEligible(Age, YOS, EntryYear)]


##Transform base mortality rates and mortality improvement rates
#Add age 19 and 18 to the mortality improvement tables (with the same values as age 20)
# MaleMP_20 <- as.numeric(as.vector(MaleMP[1,]))[-1]
# MaleMP_18 <- c(18, MaleMP_20)
# MaleMP_19 <- c(19, MaleMP_20)
# MaleMP <- rbind(MaleMP, MaleMP_18, MaleMP_19) %>% arrange(Age)
# 
# FemaleMP_20 <- as.numeric(as.vector(FemaleMP[1,]))[-1]
# FemaleMP_18 <- c(18, FemaleMP_20)
# FemaleMP_19 <- c(19, FemaleMP_20)
# FemaleMP <- rbind(FemaleMP, FemaleMP_18, FemaleMP_19) %>% arrange(Age)

#Turn MP tables into long format and impute values for years after the max year in the MP tables
mp_start_time <- Sys.time()

MaleMP <- MaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_male") %>% 
  mutate(Years = as.numeric(Years))

MaleMP_ultimate <- MaleMP %>%        #ultimate rates = rates for the last year in the MP table
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_male = MP_male) %>% 
  select(-Years)

MaleMP_final <- expand_grid(Age, Years = 1951:MaxYear) %>% 
  left_join(MaleMP, by = c("Age", "Years")) %>% 
  left_join(MaleMP_ultimate, by = "Age") %>% 
  mutate(
    # MP_final_male = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male)   
    #Since the plan assumes "immediate convergence" of MP rates, the "ultimate rates" are used for all years
    MP_final_male = MP_ultimate_male) %>% 
  group_by(Age) %>% 
  mutate(MPcumprod_male_raw = cumprod(1 - MP_final_male),
         MPcumprod_male_adj = MPcumprod_male_raw / MPcumprod_male_raw[Years == 2014]) %>%   #Adjust the mort improvement rates for the 2014 base year
  ungroup()


FemaleMP <- FemaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_female") %>% 
  mutate(Years = as.numeric(Years))

FemaleMP_ultimate <- FemaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_female = MP_female) %>% 
  select(-Years)

FemaleMP_final <- expand_grid(Age, Years = 1951:MaxYear) %>% 
  left_join(FemaleMP, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_ultimate, by = "Age") %>% 
  mutate(
    # MP_final_female = ifelse(Years > max(FemaleMP$Years), MP_ultimate_female, MP_female)
    #Since the plan assumes "immediate convergence" of MP rates, the "ultimate rates" are used for all years
    MP_final_female = MP_ultimate_female
  ) %>%
  group_by(Age) %>% 
  mutate(MPcumprod_female_raw = cumprod(1 - MP_final_female),
         MPcumprod_female_adj = MPcumprod_female_raw / MPcumprod_female_raw[Years == 2014]) %>% 
  ungroup()

##Mortality calculations
#Mortality initial setup
# MortalityTable_int <- expand_grid(EntryYear, Age, Years, YOS)
# 
# MortalityTable_int <- MortalityTable_int %>% 
#   mutate(term_year = EntryYear + YOS,
#          entry_age = Age - (Years - EntryYear)) %>% 
#   #Years here is the same as retirement/refund years
#   filter(term_year <= Years,
#          entry_age %in% SalaryEntry$entry_age) %>% 
#   arrange(EntryYear, entry_age, YOS, Age)


MortalityTable_int <- expand_grid(EntryYear, entry_age = SalaryEntry$entry_age, Age, YOS) %>% 
  mutate(term_year = EntryYear + YOS,
         Years = EntryYear + Age - entry_age) %>% 
  filter(term_year <= Years) %>% 
  arrange(EntryYear, entry_age, YOS, Age)


#Join base mortality table with mortality improvement table and calculate the final mortality rates
MortalityTable <- MortalityTable_int %>% 
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(MaleMP_final, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_final, by = c("Age", "Years")) %>% 
  
  #MPcumprod is the cumulative product of (1 - MP rates), starting from 2011. We use it later so make life easy and calculate now
  mutate(
    RetirementCond = IsRetirementEligible(Age, YOS, EntryYear),
    mort_male = ifelse(RetirementCond == T, RP_2014_ann_employee_male, 
                       RP_2014_employee_male * ScaleMultiple) * MPcumprod_male_adj, 
    mort_female = ifelse(RetirementCond == T, RP_2014_ann_employee_female, 
                         RP_2014_employee_female * ScaleMultiple) * MPcumprod_female_adj,
    mort = (mort_male + mort_female)/2)

#filter out the necessary variables
MortalityTable <- MortalityTable %>% 
  select(EntryYear, term_year, Years, entry_age, Age, YOS, mort) %>%
  mutate(
    is_before_year_start = ifelse(EntryYear <= YearStart, 1, 0),
    is_after_CB_vesting = ifelse(YOS >= CB_vesting, 1, 0)
  ) %>%
  group_by(EntryYear, entry_age, YOS) %>%
  mutate(
    surv = cumprod(1 - lag(mort, default = 0)),
    min_age = min(Age),
    is_min_age = ifelse(Age == min_age, 1, 0)
  ) %>%
  ungroup()

# PV_CB_Benefit = ifelse(YOS >= CB_vesting, CB_Benefit * AnnFactorAdj_DR, CBBalance)


#Create a second mortality table for current retirees
MortalityTable_retire <- expand_grid(Age = Age[Age >= 40], Years = Years[Years >= YearStart]) %>% 
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(MaleMP_final, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_final, by = c("Age", "Years")) %>% 
  mutate(base_age = Age - (Years - YearStart),
         mort_male = RP_2014_ann_employee_male * MPcumprod_male_adj,
         mort_female = RP_2014_ann_employee_female * MPcumprod_female_adj,
         mort = (mort_male + mort_female)/2) %>% 
  select(base_age, Age, Years, mort) %>% 
  filter(base_age >= 40) %>% 
  arrange(base_age)

mp_end_time <- Sys.time()

######################
######################

#Separation Rates (only apply to active members)
#Because separation rates are subject to retirement eligibility, which depends on entry years (tiers), the separation table needs to take entry years into account
sep_start_time <- Sys.time()

SeparationRates <- expand_grid(EntryYear, Age, YOS) %>% 
  mutate(entry_age = Age - YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(EntryYear, entry_age, Age) %>% 
  #Calculate the years from normal retirement
  group_by(EntryYear, entry_age) %>% 
  mutate(normal_retire = ifelse(RetirementType(Age, YOS, EntryYear) == "Normal", Age, 0),
         first_retire = min(normal_retire[normal_retire != 0]),
         YearsFromNR = pmax(first_retire - Age, 0)) %>% 
  left_join(TerminationRateBefore10, by = "YOS") %>% 
  left_join(TerminationRateAfter10, by = "YearsFromNR") %>% 
  left_join(RetirementRates, by = "Age") %>% 
  replace(is.na(.), 0) %>%    
  
  #If retirement eligible, use the retirement rates, or else check if YOS < 10 and use the "before 10" term rates, or else use the "after 10" term rates.
  mutate(retirement_type = RetirementType(Age, YOS, EntryYear),
         SepRateMale = ifelse(retirement_type == "Normal", NormalMale,
                              ifelse(retirement_type == "Early", ReducedMale,
                                     ifelse(YOS < 10, TermBefore10Male, TermAfter10Male))),
         
         SepRateFemale = ifelse(retirement_type == "Normal", NormalFemale,
                                ifelse(retirement_type == "Early", ReducedFemale,
                                       ifelse(YOS < 10, TermBefore10Female, TermAfter10Female))),
         
         SepRate = ((SepRateMale + SepRateFemale)/2),
         
         RemainingProb = cumprod(1 - lag(SepRate, default = 0)),
         SepProb = lag(RemainingProb, default = 1) - RemainingProb) %>% 
  ungroup()

#Filter out unecessary values
SeparationRates <- SeparationRates %>% 
  select(EntryYear, entry_age, Age, YOS, SepRate, RemainingProb, SepProb)

sep_end_time <- Sys.time()

ReducedGFT_long <- ReducedGFT %>% 
  pivot_longer(cols = -1, names_to = "Age", values_to = "RedGFT") %>% 
  mutate(Age = as.numeric(Age))

ReducedGFT_mod <- expand_grid(YOS, Age = 55:60) %>% 
  left_join(ReducedGFT_long) %>% 
  group_by(Age) %>% 
  mutate(RedGFT2 = ifelse(YOS > 30, max(RedGFT, na.rm = T), RedGFT)) %>% 
  select(YOS, Age, RedGFT2) %>% 
  ungroup()

# expand_grid(EntryYear, entry_age = SalaryEntry$entry_age, Age, YOS)
ReducedFactor_ <- expand_grid(EntryYear, entry_age = SalaryEntry$entry_age, Age, YOS) %>% 
  # semi_join(SalaryData, by = c("EntryYear", "entry_age")) %>% 
  filter(entry_age + YOS <= Age) %>% 
  left_join(ReducedGFT_mod) %>% 
  left_join(ReducedOthers) %>% 
  mutate(RetType = RetirementType(Age, YOS, EntryYear),
         RF = ifelse(RetType == "Normal", 1,
                     ifelse(RetType == "Early", 
                            ifelse(EntryYear <= 2007 & YOS >= 30, 1 - 0.02*(50 - Age),
                                   ifelse(IsGrandfathered(Age, YOS, EntryYear, entry_age) == T & YOS >= 20, RedGFT2,
                                          ifelse(EntryYear > 2007 & EntryYear <= 2009 & (YOS + Age >= NormalRetRule | YOS >= 30), 1 - 0.05*(60 - Age),
                                                 ifelse(EntryYear > 2009 & (YOS + Age >= NormalRetRule | YOS >= 30), 1 - 0.05*(62 - Age), RedOthers)))),
                            0))) %>% 
  rename(retire_age = Age)

ReducedFactor_mort <- ReducedFactor_ %>%
  left_join(MortalityTable, by=c('EntryYear', 'entry_age', 'retire_age'='Age', 'YOS')) %>%
  left_join(memo_retirement_type, by = c('EntryYear', 'YOS','retire_age' = 'Age'))


##----------------------------------------------------------------

##################################################################
##                    Benefit Model Function                    ##
##################################################################

# ee_cont_cb = CB_EE_paycredit
# er_credit_cb = CB_ER_paycredit
# dr_current = dr_current_
# dr_new = dr_new_
# cola_current_retire = COLA_current_retire
# cola_current_retire_one = COLA_current_retire_one
# one_time_cola = one_time_cola_
# cola_current_active = COLA_current_active
# cola_new_active = COLA_new_active
# icr_smooth_period = icr_smooth_period_
# icr_floor = icr_floor_
# icr_cap = icr_cap_
# icr_upside_share = icr_upside_share_
# icr_return_vol = icr_return_vol_
# return_scen = return_scen_
# model_return = model_return_
# acr = ACR
# retire_refund_ratio = retire_refund_ratio_
# cal_factor = cal_factor_
# ReducedFactor_input = ReducedFactor_mort

get_benefit_data <- function(
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
    ReducedFactor_input = ReducedFactor_mort
) {
  
  icr_start_time <- Sys.time()
  #Calculate the expected ICR
  set.seed(1234)
  
  geometric_return = dr_new
  sd_return = icr_return_vol
  smooth_period = icr_smooth_period
  floor = icr_floor
  cap = icr_cap
  upside_share = icr_upside_share
  
  ICR <- expected_icr_rcpp(geometric_return = dr_new, sd_return = icr_return_vol, smooth_period = icr_smooth_period,
                      floor = icr_floor, cap = icr_cap, upside_share = icr_upside_share)
  
  icr_mid_time_1 <- Sys.time()
  
  # ICR <- expected_icr(geometric_return = dr_new, sd_return = icr_return_vol, smooth_period = icr_smooth_period,
  #                          floor = icr_floor, cap = icr_cap, upside_share = icr_upside_share)
  # icr_mid_time_2 <- Sys.time()
  
  #Set return values for "model" and "assumption" scenarios
  return_scenarios$model <- model_return
  return_scenarios$assumption <- dr_current
  
  
  #Actual ICRs for CB projection
  actual_icr_table <- data.frame(Years, ICR) %>% 
    left_join(return_scenarios %>% select(year, any_of(return_scen)), by = c("Years" = "year")) %>% 
    rename(inv_return = return_scen) %>% 
    mutate(inv_return = ifelse(Years <= YearStart, icr_floor, 
                               ifelse(is.na(inv_return), dr_current, inv_return)),
           actual_ICR = rollapply(inv_return, width = icr_smooth_period, align = "right",
                                  FUN = smooth_return, fill = icr_floor, floor = icr_floor, cap = icr_cap, upside_share = icr_upside_share)
           ) %>% 
    #test the model with icr different from assumption
    # mutate(actual_ICR = ifelse(Years == 2027, 0.20,
    #                            ifelse(Years == 2040, 0.01,
    #                                   ifelse(Years == 2050, 0.3,
    #                                          ifelse(Years == 2065, -0.12, ICR))))) %>%
    
    # mutate(actual_ICR = ICR) %>% 
    
    select(-ICR, -inv_return)
  
  icr_end_time <- Sys.time()  
  
  ##Salary Projection
  #Create a long-form table of EntryYear, Age, YOS and merge with salary data
  
  sal_start_time <- Sys.time()
  
  SalaryData <- expand_grid(EntryYear, entry_age = SalaryEntry$entry_age, YOS) %>% 
    mutate(Age = entry_age + YOS,
           Years = EntryYear + YOS) %>% 
    filter(Age <= MaxAge) %>% 
    arrange(EntryYear, entry_age, YOS) %>% 
    select(EntryYear, entry_age, Age, YOS, Years) %>% 
    left_join(SalaryEntry, by = "entry_age") %>% 
    left_join(SalaryGrowthYOS, by = "YOS") %>% 
    left_join(SalaryHeadCountData %>% select(EntryYear, entry_age, entry_salary), by = c("EntryYear", "entry_age")) %>% 
    #add actual ICR
    left_join(actual_icr_table) %>%
    group_by(EntryYear, entry_age) %>% 
    mutate(GFT = IsGrandfathered(Age, YOS, EntryYear, entry_age),
           FinAvgSalaryYears = ifelse(GFT == T, FinAvgSalaryYears_gft, FinAvgSalaryYears_current),
           #The entry salaries from SalaryHeadCountData are used to project salaries for existing members. 
           #The start_sal from the SalaryEntry table is used to project salaries for new members
           #Note that projected salaries for new hires are based on the fixed starting salaries from the YearStart. Therefore, payroll growth needs to be incorporated. 
           Salary = ifelse(EntryYear <= max(SalaryHeadCountData$EntryYear), entry_salary * sal_cum_growth,
                           start_sal * sal_cum_growth * (1 + payroll_growth_)^(EntryYear - YearStart)),
           # FinalAvgSalary = rollmean(lag(Salary), k = FinAvgSalaryYears, fill = NA, align = "right"),
           FinalAvgSalary = baseR.rollmean(Salary, FinAvgSalaryYears),
           DBEEContAmount = DB_EE_cont * Salary,
           DBEEBalance = cumFV(Interest, DBEEContAmount),
           CBEEContAmount = ee_cont_cb * Salary,
           CBERContAmount = er_credit_cb * Salary,
           #use cumFV2 for cash balance projection since the interest (actual_ICR) is a vector
           # CBEEBalance = cumFV(ICR, CBEEContAmount),
           # CBERBalance = cumFV(ICR, CBERContAmount),
           CBEEBalance = cumFV2(actual_ICR, CBEEContAmount),
           CBERBalance = cumFV2(actual_ICR, CBERContAmount),
           CBBalance = CBEEBalance + ifelse(YOS >= CB_vesting, CBERBalance, 0)) %>% 
    ungroup() %>% 
    filter(!is.na(Salary))
  
  sal_end_time <- Sys.time()
  
  #Reduced Factor to account for early retirement
  #Convert Reduced Factor table for the grandfathered into long form and impute the rates with YOS > 30
  
  rf_start_time <- Sys.time()
  
  ReducedFactor <- ReducedFactor_input %>%
    semi_join(SalaryData, by = c("EntryYear", "entry_age"))
  
  rf_end_time <- Sys.time()
    
  #Survival Probability and Annuity Factor for inactive members
  
  ann_start_time <- Sys.time()
  
  # AnnFactorData <- MortalityTable %>% # I should reduce the size of Mort Table by using the same operations on Salary Data
  #   semi_join(SalaryData, by = c("EntryYear", "entry_age")) %>%
  #   left_join(actual_icr_table)
  
  AnnFactorData <- ReducedFactor %>%
    left_join(actual_icr_table) %>%
    rename(Age = retire_age)

  setDT(AnnFactorData)
  ann_mid_time_1 <- Sys.time()
  
  # Rprof("./profiling_outputs/AnnFactorData_profile.out", interval = 0.1, append=FALSE)
  
  AnnFactorData <- AnnFactorData[,{
      DR = is_before_year_start * dr_current + (1 - is_before_year_start) * dr_new
      cum_DR = cumprod(1 + lag(DR, default = 0))
      surv_DR = surv/cum_DR
      surv_ICR = surv/(1 + ICR)^(Age - min_age)
      actual_ICR = (1 - is_min_age)*actual_ICR
      cumprod_actual_ICR = cumprod(1 + actual_ICR)
      surv_actual_ICR = surv/cumprod_actual_ICR
      surv_ACR = surv/(1 + acr)^(Age - min_age)
      COLA = is_before_year_start * cola_current_active + (1 - is_before_year_start) * cola_new_active
      surv_DR_COLA = surv_DR * (1 + COLA)^(Age - min_age)
      surv_ACR_COLA = surv_ACR * (1 + COLA)^(Age - min_age)
      AnnuityFactor_DR = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA
      AnnuityFactor_ACR = rev(cumsum(rev(surv_ACR_COLA)))/surv_ACR_COLA
      .(term_year, Years, Age, mort, actual_ICR, is_after_CB_vesting, can_retire,
        DR, cum_DR, surv, surv_DR, surv_ICR, cumprod_actual_ICR, surv_actual_ICR,
        surv_ACR, COLA, surv_DR_COLA, surv_ACR_COLA, AnnuityFactor_DR, AnnuityFactor_ACR,
        RedGFT2, RedOthers, RetType, RF, is_before_year_start, min_age, is_min_age)
  }, by = .(EntryYear, entry_age, YOS)]
  
  # Rprof(NULL)
  # summaryRprof("./profiling_outputs/AnnFactorData_profile.out")
  
  ann_mid_time_2 <- Sys.time()
  
  #Survival Probability and Annuity Factor for current retirees
  # COLA_current_retire <- 0.02
  # one_time_cola <- "Yes"
  # COLA_current_retire_one <- 0.01
  
  AnnFactorData_retire <- MortalityTable_retire %>% 
    group_by(base_age) %>% 
    mutate(surv = cumprod(1 - lag(mort, default = 0)),
           DR = dr_current,
           cum_DR = cumprod(1 + lag(DR, default = 0)),
           surv_DR = surv/cum_DR,
           cola_type = ifelse(one_time_cola == T, "one_time", "normal"),
           cola = ifelse(Years < YearStart + 2, 0,
                         ifelse(cola_type == "one_time", ifelse(Years == YearStart + 2, cola_current_retire_one, 0), 
                                cola_current_retire)),
           # surv_DR_COLA_retire = surv_DR * (1 + COLA_current_retire)^(Age - min(Age)),
           AnnuityFactor_DR_retire = annfactor(surv_DR_vec = surv_DR, cola_vec = cola, one_time_cola = one_time_cola)) %>% 
    ungroup()

  ann_end_time <- Sys.time()  
  
  #Benefits and Present Value
  
  benefit_start_time <- Sys.time()
  
  BenefitsTable <- AnnFactorData %>% 
    mutate(term_age = entry_age + YOS) %>% 
    rename(retire_age = Age) %>% 
    #remove actual_ICRs from SalaryData as these ICRs correspond to the term_year instead of Years (use actual_ICRs from AnnFactorData instead)
    left_join(SalaryData %>% select(-actual_ICR), by = c("EntryYear", "term_year" = "Years", "entry_age", "term_age" = "Age", "YOS")) %>%
    # left_join(ReducedFactor, by = c("EntryYear", "entry_age", "retire_age", "YOS"))  %>%
    # left_join(memo_retirement_type, by = c('EntryYear', 'YOS','retire_age' = 'Age'))
  
  
  setDT(BenefitsTable)
  
  benefit_mid_time_1 <- Sys.time()
  
  BenefitsTable <- BenefitsTable[, {
    AnnFactorAdj_DR = AnnuityFactor_DR * surv_DR
    DB_Benefit = RF * BenMult * FinalAvgSalary * YOS * cal_factor
    DB_Benefit = ifelse(RF == 1, pmax(DB_Benefit, 150 * 12), DB_Benefit)
    PV_DB_Benefit = DB_Benefit * AnnFactorAdj_DR
    CBBalance_final = CBBalance / surv_actual_ICR
    CB_Benefit = CBBalance_final / AnnuityFactor_ACR
    PV_CB_Benefit = is_after_CB_vesting * CB_Benefit * AnnFactorAdj_DR + (1 - is_after_CB_vesting) * CBBalance
    .(term_year, Years, retire_age, mort, actual_ICR, 
      DR, cum_DR, surv, surv_DR, surv_ICR, cumprod_actual_ICR, surv_actual_ICR, 
      surv_ACR, COLA, surv_DR_COLA, surv_ACR_COLA, AnnuityFactor_DR, AnnuityFactor_ACR, 
      term_age, start_sal, entrant_dist, salary_increase, sal_cum_growth, entry_salary, 
      GFT, FinAvgSalaryYears, Salary, FinalAvgSalary, DBEEContAmount, DBEEBalance, 
      CBEEContAmount, CBERContAmount, CBEEBalance, CBERBalance, CBBalance, RedGFT2, 
      RedOthers, RetType, RF, can_retire, AnnFactorAdj_DR, DB_Benefit, PV_DB_Benefit, 
      CBBalance_final, CB_Benefit, PV_CB_Benefit)
  }, by = .(EntryYear, entry_age, YOS) ]
  
  benefit_mid_time_2 <- Sys.time()
  
  
  #Employees are assumed to retire at the earliest age of retirement eligibility
  retire_age_tab <- BenefitsTable %>% 
    group_by(EntryYear, entry_age, term_age) %>% 
    summarise(retire_age = n() - sum(can_retire) + min(retire_age)) %>% 
    ungroup() %>% 
    mutate(retire_age = ifelse(retire_age == max(Age) + 1, term_age, retire_age))
    
  
  BenefitsTable_retire <- BenefitsTable %>% 
    semi_join(retire_age_tab) %>% 
    select(EntryYear, entry_age, term_age, retire_age, PV_DB_Benefit, PV_CB_Benefit, 
           surv_ICR, AnnuityFactor_ACR, AnnFactorAdj_DR) %>% 
    mutate(PV_DB_Benefit = ifelse(is.na(PV_DB_Benefit), 0, PV_DB_Benefit))

  benefit_end_time <- Sys.time()  
  
  # Benefit Accrual & Normal Cost #
  #### Real Pension Wealth = Pension Wealth adjusted for inflation
  #### Actuarial PV of Pension Wealth = Pension Wealth discounted back to entry age, multiplied by separation probability
  #Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
  #####################################

  finalData_start_time <- Sys.time()  
  
  FinalData <- SalaryData %>% 
    left_join(BenefitsTable_retire, by= c("EntryYear", "entry_age", "Age" = "term_age")) %>% 
    left_join(SeparationRates, by = c("EntryYear", "entry_age", "Age", "YOS")) %>%
    #need group_by for the PVFB function
    group_by(EntryYear, entry_age) %>%                            
    mutate(
      DR = ifelse(EntryYear <= YearStart, dr_current, dr_new),
      sep_type = SeparationType(Age, YOS, EntryYear),
      #Term vested DB members are assumed to choose between a refund and deferred benefits based on a retire/refund ratio. This ratio can change for calibration purposes.
      # DBWealth = pmax(DBEEBalance, PV_DB_Benefit),         
      DBWealth = ifelse(sep_type == "retire", PV_DB_Benefit, 
                        ifelse(sep_type == "term vested", retire_refund_ratio * PV_DB_Benefit + (1 - retire_refund_ratio) * DBEEBalance,
                               DBEEBalance)),
      ben_decision = ifelse(YOS == 0, NA, ifelse(sep_type == "retire", "retire",
                                                 ifelse(sep_type == "term vested", "mix", "refund"))),
      #CB members mimic DB members' behavior. This is to simplify the workforce projection done later.
      # CBWealth = ifelse(DBWealth == DBEEBalance, CBBalance, PV_CB_Benefit),   
      
      CBWealth = ifelse(sep_type == "retire", PV_CB_Benefit, 
                        ifelse(sep_type == "term vested", retire_refund_ratio * PV_CB_Benefit + (1 - retire_refund_ratio) * CBBalance,
                               CBBalance)),
      
      Real_DBWealth = DBWealth/(1 + assum_infl_)^YOS,
      Real_CBWealth = CBWealth/(1 + assum_infl_)^YOS) %>%
    ungroup()
  
  finalData_mid_time_1 <- Sys.time()
  
  FinalData <- FinalData %>%
    group_by(EntryYear, entry_age) %>% 
    mutate(
      #Calculate present value of future benefits (PVFB) for DB members
      PVFB_DB = opt_PVFB_rcpp(SepRate, DR, DBWealth),
      
      #Calculate present value of future salaries (PVFS)
      PVFS = opt_PVFS_rcpp(RemainingProb, DR, Salary),
    ) %>%
    ungroup()
  
  finalData_mid_time_2 <- Sys.time()
  
  FinalData <- FinalData %>%
    group_by(EntryYear, entry_age) %>% 
    mutate(
      #Calculate present value of future benefits (PVFB) for CB members
      # PVFB_CB = PVFB_CB(ee_bal_vec = CBEEBalance, er_bal_vec = CBERBalance,
      #                   ee_cont_vec = CBEEContAmount, er_cont_vec = CBERContAmount,
      #                   icr = ICR, yos_vec = YOS, vesting_period = CB_vesting,
      #                   surv_icr_vec = surv_ICR, annuity_acr_vec = AnnuityFactor_ACR, annuity_adj_vec = AnnFactorAdj_DR,
      #                   sep_type_vec = sep_type, sep_rate_vec = SepRate, interest_vec = DR, retire_refund_ratio = retire_refund_ratio),

      PVFB_CB = get_pvfb_cb_vec(ee_bal_vec = CBEEBalance, ee_cont_vec = CBEEContAmount,
                                 er_bal_vec = CBERBalance, er_cont_vec = CBERContAmount,
                                 vesting_period = CB_vesting, retire_refund_ratio = retire_refund_ratio,
                                 yos_vec = YOS, surv_icr_vec = surv_ICR,
                                 annuity_acr_vec = AnnuityFactor_ACR, annuity_adj_vec = AnnFactorAdj_DR,
                                 sep_type_vec = sep_type, sep_rate_vec = SepRate,
                                 interest_vec = DR, icr = ICR),
      
      #Calculate entry-age normal cost rate by dividing the PVFB by the PVFS at entry age
      normal_cost_DB = PVFB_DB[YOS == 0] / PVFS[YOS == 0],
      normal_cost_CB = PVFB_CB[YOS == 0] / PVFS[YOS == 0],
      
      #Calculate present value of future normal costs (PVFNC)
      PVFNC_DB = PVFS * normal_cost_DB,
      PVFNC_CB = PVFS * normal_cost_CB
    ) %>% 
    ungroup()
  
  finalData_end_time <- Sys.time()

  
  # example_final_data <- FinalData %>%
  #   filter(EntryYear == 2025, entry_age == 30) %>%
  #   mutate(
  #     PVFB_CB_rcpp =  get_pvfb_cb_vec(ee_bal_vec = CBEEBalance, ee_cont_vec = CBEEContAmount,
  #                                    er_bal_vec = CBERBalance, er_cont_vec = CBERContAmount,
  #                                    vesting_period = CB_vesting, retire_refund_ratio = retire_refund_ratio,
  #                                    yos_vec = YOS, surv_icr_vec = surv_ICR,
  #                                    annuity_acr_vec = AnnuityFactor_ACR, annuity_adj_vec = AnnFactorAdj_DR,
  #                                    sep_type_vec = sep_type, sep_rate_vec = SepRate,
  #                                    interest_vec = DR, icr = ICR),
  #     
  #     PVFB_CB_r = PVFB_CB(ee_bal_vec = CBEEBalance, er_bal_vec = CBERBalance,
  #                       ee_cont_vec = CBEEContAmount, er_cont_vec = CBERContAmount,
  #                       icr = ICR, yos_vec = YOS, vesting_period = CB_vesting,
  #                       surv_icr_vec = surv_ICR, annuity_acr_vec = AnnuityFactor_ACR, annuity_adj_vec = AnnFactorAdj_DR,
  #                       sep_type_vec = sep_type, sep_rate_vec = SepRate, interest_vec = DR, retire_refund_ratio = retire_refund_ratio)
  #   )
  
  #Calculate normal cost rate for each entry age in each entry year
  
  nc_start_time <- Sys.time()
  
  NormalCost <- FinalData %>% 
    filter(YOS == 0) %>% 
    select(EntryYear, entry_age, normal_cost_DB, normal_cost_CB)
  
  #Calculate the aggregate normal cost for current year (for testing purposes)
  NC_aggregate <- NormalCost %>% 
    left_join(SalaryHeadCountData, by = c("EntryYear", "entry_age")) %>%
    left_join(SalaryData %>% select(EntryYear, entry_age, Age, Salary), by = c("EntryYear", "entry_age", "Age")) %>% 
    filter(!is.na(Count)) %>% 
    summarise(normal_cost_aggregate_DB = sum(normal_cost_DB * Salary * Count) / sum(Salary * Count),
              normal_cost_aggregate_CB = sum(normal_cost_CB * Salary * Count) / sum(Salary * Count))

  nc_end_time <- Sys.time()  
  
  output <- list(ann_factor_tab = AnnFactorData,
                 ann_factor_retire_tab = AnnFactorData_retire,
                 ben_tab = BenefitsTable,
                 ben_retire_tab = BenefitsTable_retire,
                 final_tab = FinalData,
                 nc_tab = NormalCost,
                 nc_agg = NC_aggregate)
  
  return(output)

}


##############################################################################################################
################## TESTING ###################################################################################
# 






