*SECTION 2
clear
cd C:\Users\lavia\OneDrive\Desktop\PhD\Monetary2\PS_12
use wrds_data
xtset gvkey fyear
*a)Estimate a Q-model of investment using variants of equation 16 in lecture 11
sort gvkey fyear
gen aveq=((prcc_c*csho)+dt)/capital
drop if missing(aveq)
*drop outliers in Q
winsor2 aveq, cuts (0 95)
label var aveq "Average-Q"
label var irate "Investment Rate"
label var cf_r "Cashflow rate"
reghdfe irate_w L.aveq_w, absorb(gvkey)
*now estimate the augmented q model with cf_r_w
reghdfe irate_w L.aveq_w L.cf_r_w, absorb(gvkey)
*b)Estimate the augmented Q-model using the within estimator (LSDV). Always include common year effect as well. Then estimate the model by GMM using xtabond2 in Stata
reghdfe irate_w L.aveq_w L.cf_r_w, absorb(gvkey sic#fyear)
eststo q_gmm: xtabond2 irate_w L.aveq_w L.cf_r_w i.fyear, gmm(irate_w, lag(2 4)) iv(i.fyear) noleveleq robust
*c-d-e)Try both GMM difference estimator and the system estimator and discuss the relative merits. Try both one step and two step estimator
*difference GMM two step
eststo q_gmm_d2: xtabond2 irate_w L.aveq_w L.cf_r_w i.fyear, gmm(irate_w, lag(2 4)) iv(i.fyear) noleveleq two robust
*difference GMM two step without correction
eststo q_gmm_d2no: xtabond2 irate_w L.aveq_w L.cf_r_w i.fyear, gmm(irate_w, lag(2 4))iv(i.fyear) noleveleq two 
*system GMM one step
eststo q_sgmm_1: xtabond2 irate_w L.aveq_w L.cf_r_w i.fyear, gmm(irate_w, lag(2 4)) iv(i.fyear) robust
*system GMM two step 
eststo q_sgmm_2: xtabond2 irate_w L.aveq_w L.cf_r_w i.fyear, gmm(irate_w, lag(2 4)) iv(i.fyear)  two robust
*system GMM two step without correction 
eststo q_sgmm_2no: xtabond2 irate_w L.aveq_w L.cf_r_w i.fyear, gmm(irate_w, lag(2 4)) gmm(aveq_w cf_r_w, lag(3 4)) iv(i.fyear) two
esttab q_gmm q_gmm_d2 q_sgmm_1 q_sgmm_2 using "$Tab.txt", replace label ///
 nobaselevels star (* .10 ** .05 *** .01) b(3) ar2(3) se nonumbers ///
	booktabs alignment(cc)
*e)Divide the sample in two types of firms based on wether emp is lower or greater than 250. Re estimate Q model allowing the coefficients to be different for small and large firms
*I now create dummy for small and large firms
gen small_firm=1 if emp<0.25
replace small_firm=0 if emp>=0.25
gen large_firm=1-small_firm
*I now create variable for both small and large firms
gen large_cf_r=L.large_firm*L.cf_r_w
gen small_cf_r=L.small_firm*L.cf_r_w
*I now estimate the Q model 
eststo qs: reghdfe irate_w L.aveq_w L.cf_r_w c.L.aveq_w#L.small_firm L.small_cf_r, absorb(gvkey fyear)
*GMM difference one step
eststo qs_gmm_1: xtabond2 irate_w L.aveq_w L.cf_r_w c.L.aveq_w#L.small_firm L.small_cf_r i.fyear, gmm(aveq_w cf_r_w, lag(2 4)) iv(i.fyear) noleveleq robust
*GMM difference two step 
eststo qs_gmm_2: xtabond2 irate_w L.aveq_w L.cf_r_w c.L.aveq_w#L.small_firm L.small_cf_r i.fyear, gmm(aveq_w cf_r_w, lag(2 4)) iv(i.fyear) noleveleq two robust
*system GMM one step
eststo qss_gmm_1: xtabond2 irate_w L.aveq_w L.cf_r_w c.L.aveq_w#L.small_firm L.small_cf_r i.fyear, gmm(aveq_w cf_r_w, lag(2 4)) iv(i.fyear) robust
*system GMM two step
eststo qss_gmm_2: xtabond2 irate_w L.aveq_w L.cf_r_w c.L.aveq_w#L.small_firm L.small_cf_r i.fyear, gmm(aveq_w cf_r_w, lag(2 4))  iv(i.fyear) two robust
esttab qs qs_gmm_1 qs_gmm_2 qss_gmm_1 qss_gmm_2 using "$Tab.txt", replace label ///
 nobaselevels star (* .10 ** .05 *** .01) b(3) ar2(3) se nonumbers ///
	booktabs alignment(cc)
*f)Estimate the Euler equations for capital in the parametrization of the lecture notes
*I assume perfect competition, thus pi is operating profit
gen beta=0.95
gen delta=0.05
gen cst=beta*cf_r
gen irate2=L.irate_w*L.irate_w
gen cst_oi=L.cst*L.oi_r_w
gen cst_irate2=L.cst*L.irate2
gen cst_ic=(1-delta)*L.cst*L.irate
*GMM difference one step
eststo pc_gmm1:xtabond2 irate cst_oi cst_irate2 cst_ic i.fyear, gmm(oi_r_w irate, lag(2 4)) iv(i.fyear) noleveleq robust
*GMM difference two step 
eststo pc_gmm2:xtabond2 irate cst_oi cst_irate2 cst_ic i.fyear, gmm(oi_r_w irate, lag(2 4)) iv(i.fyear) noleveleq two robust
*system GMM one step
eststo pc_gmm_s1:xtabond2 irate cst_oi cst_irate2 cst_ic i.fyear, gmm(oi_r_w irate, lag(2 4)) iv(i.fyear) robust
*system GMM two step
eststo pc_gmm_s2:xtabond2 irate cst_oi cst_irate2 cst_ic i.fyear, gmm(oi_r_w irate, lag(2 4)) iv(i.fyear) two robust
esttab pc_gmm1 pc_gmm2 pc_gmm_s1 pc_gmm_s2 using "$Tab.txt", replace label ///
 nobaselevels star (* .10 ** .05 *** .01) b(3) ar2(3) se nonumbers ///
	booktabs alignment(cc) 
*I assume imperfect competition, thus pi is equal to (alpha/mu)*(Y/K)
*I assume markup in USA (1.3 Hall 2018)
gen mu=1.3
gen pie=(L.capshare*L.output_cap_r)/mu
gen cst_pi=L.cst*pie
*GMM difference one step
eststo ic_gmm1:xtabond2 irate cst_pi cst_irate2 cst_ic i.fyear, gmm(oi_r_w irate, lag(2 4)) iv(i.fyear) noleveleq robust
*GMM difference two step 
eststo ic_gmm2:xtabond2 irate cst_pi cst_irate2 cst_ic i.fyear, gmm(oi_r_w irate, lag(2 4)) iv(i.fyear) noleveleq two robust
*system GMM one step
eststo ic_gmm_s1:xtabond2 irate cst_pi cst_irate2 cst_ic i.fyear, gmm(oi_r_w irate, lag(2 4)) iv(i.fyear) robust
*system GMM two step
eststo ic_gmm_s2:xtabond2 irate cst_pi cst_irate2 cst_ic i.fyear, gmm(oi_r_w irate, lag(2 4)) iv(i.fyear) two robust
esttab ic_gmm1 ic_gmm2 ic_gmm_s1 ic_gmm_s2 using "$Tab.txt", replace label ///
 nobaselevels star (* .10 ** .05 *** .01) b(3) ar2(3) se nonumbers ///
	booktabs alignment(cc) 
save wrds_data_2, replace 
