##################################################################
##                  Model Inputs & Assumptions                  ##
##################################################################


#1. Actuarial and economic assumptions:
dr_current_ <- 0.07   #discount rate for current members
dr_new_ <- 0.07   #discount rate for new members
assum_infl_ <- 0.023   #inflation assumption
payroll_growth_ <- 0.029   #payroll growth assumption
pop_growth_ <- 0   #plan's active population growth assumption
inf_rate <- 0.023  #inflation rate assumption

#Mortality assumptions
ScaleMultiple <- 0.9



#2. Benefit assumptions:
#Defined benefit assumptions:
ReduceRetAge <- 55
EarlyRetYOS <- 30
NormalRetAgeI <- 65
NormalYOSI <- 5
NormalRetRule <- 80
NormalRetRuleAgeI <- 60
NormalRetRuleAgeII <- 62

FinAvgSalaryYears_current <- 5   #final average salary period
FinAvgSalaryYears_gft <- 3   #final average salary period for grandfathered members

Interest <- 0.02   #interest on DB employee balance
BenMult <- 0.023   #DB benefit multiplier


#Cost of living adjustment (COLA) assumptions:
COLA_current_active <- 0   #COLA for current active members
COLA_new_active <- 0   #COLA for new active members (note that this applies to the cash balance plan too if new members go to the CB plan)
COLA_current_retire <- 0   #COLA for current retirees
COLA_current_retire_one <- 0   #One-time COLA for current retirees
one_time_cola_ <- F   #One-time COLA or not? True means yes, False means no

#Cash balance benefit assumptions
CB_EE_paycredit <- 0.06   #Cash balance employee contribution rate
CB_ER_paycredit <- 0.09   #Cash balance employer pay credit
CB_vesting <- 5

icr_smooth_period_ <- 5   #Smoothing period for the interest crediting rate (ICR)   
icr_floor_ <- 0.04   #Minimum ICR granted
icr_cap_ <- 0.07   #Maximum ICR granted
icr_upside_share_ <- 0.5   #Upside sharing ratio of the ICR
icr_return_vol_ <- 0.12   #Assumed annual volatility of the investment returns to be used for the ICR calculation

ACR <- 0.04  #Annuity Conversion Rate: interest rate to convert the cash balance into retirement annuity


#3. Funding assumptions
DB_EE_cont <- 0.08

funding_policy_ <- "statutory"



#Amortization policy
amo_pay_growth_ <- 0.029
amo_period_current_ <- 30
amo_period_new_ <- 30
amo_method_ <- "level %"


#4. Investment assumptions
return_scen_ <- "assumption"
model_return_ <- 0.07


#5. Plan design assumptions
db_new_ratio_ <- 1   #% of new hires that will join the DB plan. 


#6. Model assumptions 
ModelPeriod <- 30    #Projection period (typically 30 years)
MinAge <- 20          #Age of the typical youngest member
MaxAge <- 120         #Max age from mortality assumptions
YearStart <- 2022     #Year of the latest val report
MinYear <- 1980       #No hard rule about this. Should get back to about 40 years from now.   
MaxYear <- YearStart + ModelPeriod + MaxAge - MinAge

EntryYear <- MinYear:(YearStart + ModelPeriod)  
Years <- MinYear:MaxYear
Age <- MinAge:MaxAge
YOS <- 0:70
RetirementAge <- Age

retiree_pop_current <- 475952
ben_payment_current <- 13260988197
retire_refund_ratio_ <- 0.3

cal_factor_ <- 12.09 / 11.85431   #This is to adjust the DB benefits to result in a PVFB that matches (or mostly matches) the plan's number. Update this when a new valuation report is released.
nc_cal_ <- 12.09 / 12.059083      #This is to further calibrate the normal cost in the funding model. Update this when a new valuation report is released.

# $9,443,955,800 is the PVFB for current term vested members, and $4,971,195,222  is the remaining gap between the val's AAL and the model's AAL. Update this when a new valuation report is released.
PVFB_term_current <- 9443955800 + 4971195222 
amo_period_term <- 50

#7. Import key data tables
FileName <- 'TxTRS_BM_Inputs.xlsx'

SurvivalRates <- read_excel(FileName, sheet = 'Mortality Rates')
MaleMP <- read_excel(FileName, sheet = 'MP-2018_Male') 
FemaleMP <- read_excel(FileName, sheet = 'MP-2018_Female')

SalaryGrowthYOS <- read_excel(FileName, sheet = "Salary Growth YOS")
SalaryMatrix <- read_excel(FileName, sheet = "Salary Matrix") 
HeadCountMatrix <- read_excel(FileName, sheet = "Head Count Matrix")
SalaryEntry <- read_excel(FileName, sheet = "Entrant Profile")

TerminationRateAfter10 <- read_excel(FileName, sheet = 'Termination Rates after 10')
TerminationRateBefore10 <- read_excel(FileName, sheet = 'Termination Rates before 10')
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')

ReducedGFT <- read_excel(FileName, sheet = "Reduced GFT")
ReducedOthers <- read_excel(FileName, sheet = "Reduced Others")

RetireeDistribution <- read_excel(FileName, sheet = "Retiree Distribution")

funding_data <- read_excel(FileName, sheet = "Funding Data")
return_scenarios <- read_excel(FileName, sheet = "Return Scenarios")


