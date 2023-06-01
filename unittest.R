
set.seed(1234)

geometric_return = dr_new_
sd_return = icr_return_vol_
smooth_period = icr_smooth_period_
floor = icr_floor_
cap = icr_cap_
upside_share = icr_upside_share_
n_periods = 30
n_simulations = 10000

# Test ICR function
mean_return <- est_arith_return(geometric_return, sd_return)
#simulate returns with the arithmetic avg return and standard deviation
simulated_returns <- matrix(rnorm(n_simulations * n_periods, mean = mean_return, sd = sd_return), 
                            nrow = n_periods, ncol = n_simulations)
#add "initial returns" so that the smooth returns for the first few years can be calculated. 
#For this model, the initial returns are set equal to the floor.
initial_returns <- matrix(floor, nrow = smooth_period - 1, ncol = n_simulations)
returns_matrix <- rbind(initial_returns, simulated_returns)

smooth_return_cpp <- rollapply_Cpp(returns_matrix, width=smooth_period, floor=floor, 
                                   cap=cap, upside_share=upside_share)

avg_interest_rates_cpp <- get_avg_return_Cpp(smooth_return_cpp)
exp_interest_rate_cpp <- median(avg_interest_rates_cpp)


smooth_returns <- rollapply(returns_matrix, width = smooth_period, align = "right", FUN = smooth_return, 
                            floor = floor, cap = cap, upside_share = upside_share)
#get the median of the average smooth returns
avg_interest_rates <- apply(smooth_returns, 2, FUN = geo_return)
exp_interest_rate <- median(avg_interest_rates)






