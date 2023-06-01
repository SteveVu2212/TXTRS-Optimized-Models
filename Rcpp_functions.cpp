#include <Rcpp.h>
#include <string>
using namespace Rcpp;

/////////////////// ICR ///////////////////////

// [[Rcpp::export]]
double prod_Cpp(NumericVector x) {
  double result = 1.0;
  int n = x.size();
  for(int i = 0; i < n; i++){
    result *= (1.0 + x[i]);
  }
  return result;
}

// [[Rcpp::export]]
double geo_return_Cpp(NumericVector returns){
  int n = returns.size();
  double prod_result = prod_Cpp(returns);
  double geo_return_ = std::pow(prod_result, 1.0/n) - 1.0;
  return geo_return_;
}

// [[Rcpp::export]]
double smooth_return_Cpp(NumericVector returns, double floor, double cap, double upside_share){
  double geo_return = geo_return_Cpp(returns);
  double result = std::min(std::max(floor, floor + upside_share * (geo_return - floor)), cap);
  return result;
}

// [[Rcpp::export]]
NumericMatrix rollapply_Cpp(NumericMatrix return_mat, int width, double floor, double cap, double upside_share){
  int num_row = return_mat.nrow();
  int num_col = return_mat.ncol();
  int new_row = num_row - width + 1;

  NumericMatrix result(new_row, num_col);
  for(int i = 0; i < num_col; i++){
    NumericVector col = return_mat(_, i);
    for(int j = 0; j < new_row; j++){
      NumericVector returns = col[seq(j,j+width-1)];
      result(j,i) = smooth_return_Cpp(returns, floor, cap, upside_share);
    }
  }

  return result;
}

// [[Rcpp::export]]
NumericVector get_avg_return_Cpp(NumericMatrix return_mat){
  int n = return_mat.ncol();
  NumericVector avg_return(n);
  for(int i = 0; i < n; i++){
    NumericVector col = return_mat(_, i);
    avg_return[i] = geo_return_Cpp(col);
  }
  return avg_return;
}

/////////////////////// PVFB_DB, PVFS ////////////////////////////

// [[Rcpp::export]]
double get_npv_rcpp(double rate, NumericVector cashflows){
  int n = cashflows.size();
  NumericVector discount_factors(n);
  for(int i = 0; i < n; i++){
    discount_factors[i] = 1 / std::pow(1+rate, i+1);
  }
  double net_present_value = sum(cashflows * discount_factors);
  return(net_present_value);
}


// [[Rcpp::export]]
NumericVector lag_rcpp(NumericVector x, int k, double default_value) {
  int n = x.size();
  if (k >= n) {
    return rep(default_value, n);
  } else {
    NumericVector result(n);
    result[seq(k, n-1)] = NumericVector(x[seq(0, n-k-1)]);
    result[seq(0, k-1)] = NumericVector(k, default_value);
    return result;
  }
}

// [[Rcpp::export]]
NumericVector cumprodC(NumericVector x) {
  int n = x.size();
  NumericVector out(n);
  
  out[0] = x[0];
  for(int i = 1; i < n; ++i) {
    out[i]  = out[i - 1] * x[i];
  }
  return out;
}


// [[Rcpp::export]]
NumericVector opt_PVFB_rcpp(NumericVector sep_rate_vec, NumericVector interest, NumericVector value_vec) {
  int n = value_vec.size();
  NumericVector PVFB(n);
  for (int i = 0; i < n; i++) {
    NumericVector sep_rate_ = sep_rate_vec[seq(i, n-1)];
    // Rcpp::Rcout << "sep_rate_: " << sep_rate_ << std::endl;
    
    NumericVector sep_prob = cumprodC(1 - lag_rcpp(sep_rate_, 2, 0)) * lag_rcpp(sep_rate_, 1, 0);
    // Rcpp::Rcout << "sep_prob: " << sep_prob << std::endl;
    
    NumericVector value = value_vec[seq(i, n-1)];
    // Rcpp::Rcout << "value: " << value << std::endl;
    
    NumericVector value_adjusted = value * sep_prob;
    // Rcpp::Rcout << "value_adjusted: " << value_adjusted << std::endl;
    
    if (value_adjusted.size() > 1){
      PVFB[i] = get_npv_rcpp(interest[i], value_adjusted[seq(1, value_adjusted.size() - 1)]);
    } else{
      PVFB[i] = get_npv_rcpp(interest[i], value_adjusted);
    }
    
    // Rcpp::Rcout << "PVFB[" << i << "]: " << PVFB[i] << std::endl;
    
  }
  return PVFB;
}


// [[Rcpp::export]]
NumericVector opt_PVFS_rcpp(NumericVector remaining_prob_vec, NumericVector interest, NumericVector sal_vec){
  int n = sal_vec.size();
  NumericVector PVFS(n);
  for (int i = 0; i < n; i++){
    NumericVector remaining_prob_og = remaining_prob_vec[seq(i, n-1)];
    NumericVector remaining_prob = remaining_prob_og / remaining_prob_og[0];
    NumericVector sal = sal_vec[seq(i, n-1)];
    NumericVector sal_adjusted = sal * remaining_prob;
    PVFS[i] = get_npv_rcpp(interest[i], sal_adjusted);
  }
  return PVFS;
}

//////////////////////// PVFB_CB //////////////////////////////

// [[Rcpp::export]]
NumericVector get_cum_fv(double interest, NumericVector cashflow, double first_value){
  int n = cashflow.size();
  NumericVector cum_value(n);
  cum_value[0] = first_value;
  for(int i = 1; i < n; i++){
    cum_value[i] = cum_value[i-1]*(1+interest) + cashflow[i-1];
  }
  return cum_value;
}

// [[Rcpp::export]]
NumericVector get_CBBalance(double icr, NumericVector YOS, double vesting_period,
                            NumericVector ee_contr, double ee_bal,
                            NumericVector er_contr, double er_bal){
  int n = ee_contr.size();
  NumericVector CBBalance(n);
  NumericVector CB_EE_Balance = get_cum_fv(icr, ee_contr, ee_bal);
  NumericVector CB_ER_Balance = get_cum_fv(icr, er_contr, er_bal);
  for(int i = 0; i < n; i++){
    if (YOS[i] >= vesting_period){
      CBBalance[i] = CB_EE_Balance[i] + CB_ER_Balance[i];
    } else {
      CBBalance[i] = CB_EE_Balance[i];
    }
  }
  return CBBalance;
}

// [[Rcpp::export]]
NumericVector get_pvcb_benefit(NumericVector surv_icr, NumericVector annuity_acr,
                               NumericVector annuity_adj, NumericVector CBBalance){
  int n = surv_icr.size();
  NumericVector pvcb_benefit(n);
  for(int i = 0; i < n; i++){
    pvcb_benefit[i] = CBBalance[i] / surv_icr[i] / annuity_acr[i] * annuity_adj[i];
  }
  return pvcb_benefit;
}

// [[Rcpp::export]]
NumericVector get_CBWealth(Rcpp::StringVector sep_type, NumericVector pvcb_benefit,
                          NumericVector CBBalance, double retire_refund_ratio){
  int n = sep_type.size();
  NumericVector CBWealth(n);
  for(int i = 0; i < n; i++){
    if(sep_type[i] == "retire") {
      CBWealth[i] = pvcb_benefit[i];
    } else if (sep_type[i] == std::string("term vested")) {
      CBWealth[i] = retire_refund_ratio*pvcb_benefit[i] + (1 - retire_refund_ratio)*CBBalance[i];
    } else {
      CBWealth[i] = CBBalance[i];
    }
  }
  return CBWealth;
}

// [[Rcpp::export]]
double get_pvfb_cb(double interest, NumericVector sep_rate, NumericVector CBWealth){
  int n = CBWealth.size();
  double PVFB;
  NumericVector sep_prob = cumprodC(1 - lag_rcpp(sep_rate, 2, 0)) * lag_rcpp(sep_rate, 1, 0);
  NumericVector CBWealth_adj = CBWealth * sep_prob;
  if(n > 1){
    PVFB = get_npv_rcpp(interest, CBWealth_adj[seq(1, n-1)]);
  } else {
    PVFB = get_npv_rcpp(interest, CBWealth_adj);
  }
  return PVFB;
}

// [[Rcpp::export]]
NumericVector get_pvfb_cb_vec(NumericVector ee_bal_vec, NumericVector ee_cont_vec,
                              NumericVector er_bal_vec, NumericVector er_cont_vec,
                              double vesting_period, double retire_refund_ratio,
                              NumericVector yos_vec, NumericVector surv_icr_vec,
                              NumericVector annuity_acr_vec, NumericVector annuity_adj_vec,
                              Rcpp::StringVector sep_type_vec, NumericVector sep_rate_vec,
                              NumericVector interest_vec, double icr) {
  int n = ee_bal_vec.size();
  NumericVector PVFB(n);
  for(int i = 0; i < n; i++){
    NumericVector CBBalance = get_CBBalance(icr, yos_vec[seq(i,n-1)], vesting_period,
                                            ee_cont_vec[seq(i,n-1)], ee_bal_vec[i],
                                            er_cont_vec[seq(i,n-1)], er_bal_vec[i]);
    
    NumericVector PV_CB_Benefit = get_pvcb_benefit(surv_icr_vec[seq(i,n-1)], annuity_acr_vec[seq(i,n-1)],
                                                   annuity_adj_vec[seq(i,n-1)], CBBalance);
    
    NumericVector CBWealth = get_CBWealth(Rcpp::wrap(sep_type_vec[seq(i,n-1)]), PV_CB_Benefit,
                                          CBBalance, retire_refund_ratio);
    
    PVFB[i] = get_pvfb_cb(interest_vec[i], sep_rate_vec[seq(i,n-1)], CBWealth);
  }
  
  return PVFB;
}
