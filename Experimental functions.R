#get salary
# sal_cum_growth <- SalaryGrowthYOS$sal_cum_growth[with(SalaryGrowthYOS, YOS %in% 2:5)]

get_sal <- function(ey, ea, age, start_year, payroll_growth, sal_entry_tab, sal_count_tab, sal_growth_tab) {
  yos <- age - ea
  sal_cum_growth <- sal_growth_tab$sal_cum_growth[sal_growth_tab$YOS == yos]
  entry_sal <- sal_count_tab$entry_salary[with(sal_count_tab, EntryYear == ey & entry_age == ea)]
  start_sal <- sal_entry_tab$start_sal[sal_entry_tab$entry_age == ea]
  
  if (ey <= max(sal_count_tab$EntryYear)) {
    sal <- entry_sal * sal_cum_growth
  } else {
    sal <- start_sal * sal_cum_growth * (1 + payroll_growth)^(ey - start_year)
  }
    
  return(sal)
}


# test <- wf_active_df_final %>% 
#   filter(n.active > 0) %>% 
#   rowwise() %>%
#   mutate(sal_test = list(get_sal(ey = entry_year, ea = ea, age = age, start_year = YearStart,
#                              payroll_growth = payroll_growth, sal_entry_tab = SalaryEntry, sal_count_tab = SalaryHeadCountData,
#                              sal_growth_tab = SalaryGrowthYOS))) %>%
#   ungroup()
# 
# 
# 
# get_sal(ey = 1985, ea = 20, age = 57, start_year = YearStart,
#         payroll_growth = payroll_growth, sal_entry_tab = SalaryEntry, sal_count_tab = SalaryHeadCountData,
#         sal_growth_tab = SalaryGrowthYOS)



# get_sal2 <- function(ey, ea, age, start_year, payroll_growth, sal_entry_tab, sal_count_tab, sal_growth_tab) {
#   sal <- double(length = length(ey))
#   yos <- double(length = length(ey))
#   for (i in 1:length(ey)) {
#     yos[i] <- age[i] - ea[i]
#     sal_cum_growth <- sal_growth_tab$sal_cum_growth[sal_growth_tab$YOS == yos[i]]
#     entry_sal <- sal_count_tab$entry_salary[with(sal_count_tab, EntryYear == ey[i] & entry_age == ea[i])]
#     start_sal <- sal_entry_tab$start_sal[sal_entry_tab$entry_age == ea[i]]
# 
#     if (ey[i] <= max(sal_count_tab$EntryYear)) {
#       sal[i] <- entry_sal * sal_cum_growth
#     } else {
#       sal[i] <- start_sal * sal_cum_growth * (1 + payroll_growth)^(ey[i] - start_year)
#     }
# 
#   }
# 
#   return(sal)
# }
# 
# 
# test <- wf_active_df_final %>% 
#   filter(n.active > 0) %>% 
#   mutate(sal_test2 = get_sal2(ey = entry_year, ea = ea, age = age, start_year = YearStart,
#                               payroll_growth = payroll_growth, sal_entry_tab = SalaryEntry, sal_count_tab = SalaryHeadCountData,
#                               sal_growth_tab = SalaryGrowthYOS))



#get salary vector
get_sal_vec <- function(ey, ea, yos_range, start_year, payroll_growth, sal_entry_tab, sal_count_tab, sal_growth_tab) {
  sal_cum_growth_vec <- sal_growth_tab$sal_cum_growth[sal_growth_tab$YOS %in% yos_range]
  entry_sal <- sal_count_tab$entry_salary[with(sal_count_tab, EntryYear == ey & entry_age == ea)]
  start_sal <- sal_entry_tab$start_sal[sal_entry_tab$entry_age == ea]
  
  if (ey <= max(sal_count_tab$EntryYear)) {
          sal_vec <- entry_sal * sal_cum_growth_vec
        } else {
          sal_vec <- start_sal * sal_cum_growth_vec * (1 + payroll_growth)^(ey - start_year)
        }

  
  return(sal_vec)
}


#get final average salary vector
get_fas_vec <- function(ey, ea, yos_range, start_year, payroll_growth, sal_entry_tab, sal_count_tab, sal_growth_tab) {
  age_range <- ea + yos_range
  #grandfather status
  gft <- IsGrandfathered(Age = age_range, YOS = yos_range, EntryYear = ey, EntryAge = ea)
  fas_period <- ifelse(gft == T, FinAvgSalaryYears_gft, FinAvgSalaryYears_current)

  sal_vec <- get_sal_vec(ey, ea, yos_range, start_year, payroll_growth, sal_entry_tab, sal_count_tab, sal_growth_tab)
  fas_vec <- baseR.rollmean(data = sal_vec, window_vec = fas_period)
  return(fas_vec)
}


# get_fas_vec(ey = 1985, ea = 30, yos_range = YOS,
#             start_year = YearStart, payroll_growth = payroll_growth, sal_entry_tab = SalaryEntry,
#             sal_count_tab = SalaryHeadCountData, sal_growth_tab = SalaryGrowthYOS)


#get DB balance vector
get_db_ee_bal_vec <- function(ee_cont, interest, ey, ea, yos_range, start_year, payroll_growth, sal_entry_tab, sal_count_tab, sal_growth_tab) {
  sal_vec <- get_sal_vec(ey, ea, yos_range, start_year, payroll_growth, sal_entry_tab, sal_count_tab, sal_growth_tab)
  db_ee_cont <- sal_vec * ee_cont
  db_ee_bal_vec <- cumFV(interest = interest, cashflow = db_ee_cont)
  return(db_ee_bal_vec)
}


# get_db_ee_bal_vec(ee_cont = DB_EE_cont, interest = Interest, ey = 1985, ea = 20, yos_range = YOS,
#                   start_year = YearStart, payroll_growth = payroll_growth, sal_entry_tab = SalaryEntry,
#                   sal_count_tab = SalaryHeadCountData, sal_growth_tab = SalaryGrowthYOS)






#get grandfather status
get_gft <- function(age, yos, ey, ea) {
  yos_2005 <- min(yos, 2005 - ey)
  age_2005 <- min(age, 2005 - ey + ea)
  if (ey <= 2005 & (age_2005 >= 50 | age_2005 + yos_2005 >= 70 | yos_2005 >= 25)) {
    check <- T
  } else {
    check <- F
  }
  return(check)
}


#get retirement eligibility - normal
get_eligible_norm <- function(retire_age, yos, ey) {
  if ((retire_age >= 65 & yos >= 5) | (ey <= 2007 & yos + retire_age >= 80 & yos >= 5) | (ey > 2007 & ey <= 2009 & yos + retire_age >= 80 & retire_age >= 60 & yos >= 5) | (ey > 2009 & yos + retire_age >= 80 & retire_age >= 62 & yos >= 5)) {
    check <- T
  } else {
    check <- F
  }
  return(check)
}

#get retirement eligibility - early 
get_eligible_early <- function(retire_age, yos, ey) {
  if ((retire_age >= 55 & yos >= 5) | (yos >= 30) | (ey > 2007 & yos + retire_age >= 80 & yos >= 5)) {
    check <- T
  } else {
    check <- F
  }
  return(check)
}

#get retirement eligibility
get_eligible <- function(retire_age, yos, ey) {
  if (get_eligible_norm(retire_age, yos, ey) == T | get_eligible_early(retire_age, yos, ey) == T) {
    check <- T
  } else {
    check <- F
  }
  return(check)
}


#get retirement type
get_retire_type <- function(retire_age, yos, ey) {
  if (get_eligible_norm(retire_age, yos, ey) == T) {
    check <- "Normal"
  } else if (get_eligible_early(retire_age, yos, ey) == T){
    check <- "Early"
  } else {
    check <- "None"
  }
  return(check)
}



#get reduced factor
get_reduced_fact <- function(ey, ea, yos, retire_age) {
  ret_type <- get_retire_type(retire_age, yos, ey)
  
  if (ret_type == "Normal") {
    reduced_fact <- 1
  } else if (ret_type == "Early") {
    if (ey <= 2007  & yos >= 30) {
      reduced_fact <- 1 - 0.02*(50 - retire_age)
    } else if (get_gft(age = retire_age, yos = yos, ey = ey, ea = ea) == T & yos >= 20) {
      reduced_fact <- ReducedGFT_mod$RedGFT2[with(ReducedGFT_mod, YOS == yos & Age == retire_age)]
    } else if (ey > 2007 & ey <= 2009 & (yos + retire_age >= NormalRetRule | yos >= 30)) {
      reduced_fact <- 1 - 0.05*(60 - retire_age)
    } else if (ey > 2009 & (yos + retire_age >= NormalRetRule | yos >= 30)) {
      reduced_fact <- 1 - 0.05*(62 - retire_age)
    } else {
      reduced_fact <- ReducedOthers$RedOthers[ReducedOthers$Age == retire_age]
    }
  } else {
    reduced_fact <- 0
  }
}


test <- ReducedFactor %>% rowwise() %>% 
  mutate(RF_test = get_reduced_fact(ey = EntryYear, ea = entry_age, yos = YOS, retire_age = retire_age))
  


sum(test$RF - test$RF_test)



#get survival vector
get_surv_vec <- function(mort_vec) {
  surv_vec <- cumprod(1 - lag(mort_vec, default = 0))
  return(surv_vec)
}


#get discount vector
get_disc_vec <- function(dr, range) {
  ini_annuity <- rep(1, range)
  time <- seq(0, (range-1))
  disc_vec <- ini_annuity / (1 + dr) ^ time
  return(disc_vec)
}

#get survival discounted vector
get_surv_disc_vec <- function(mort_vec, dr) {
  surv_vec <- get_surv_vec(mort_vec)
  disc_vec <- get_disc_vec(dr, range = length(mort_vec))
  surv_disc_vec <- surv_vec * disc_vec
  return(surv_disc_vec)
}




