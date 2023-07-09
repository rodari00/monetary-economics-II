********************************************************
**** Empirical Exercise ********************************
**** Monetary II -Part 2     ***************************
**** Federico Rodari ***********************************
********************************************************



* Filepath (change accordingly)
global filepath1 "C:/Users/feder/Dropbox/Github/monetary-economics-II/ps1"



**************************************
* Analysis
**************************************
clear all 
cls

*** Load dataset from part 1 

import delimited  "$filepath1/data/data_winsorized.csv",  clear

xtset gvkey year

********************************************************************************
***   Point (a)  ***************************************************************
********************************************************************************
* Estimate a Q-model of investment using variants of equation 16 in lecture 11

sort gvkey year
* Define Q
gen aveq=((prcc_c*csho)+dt)/capital
drop if missing(aveq)
* Drop outliers in Q using winsorization
winsor2 aveq, cuts (0 95)
label var aveq "Average-Q"
label var irate "Investment Rate"
label var cashflowrate "Cashflow rate"

************ Estimation
eststo clear
* Estimate the Q model
reghdfe irate_w aveq_w, absorb(gvkey)
eststo Q
estadd local fixed "Firm"

* Estimate the augmented q model with cashflowrate_w
reghdfe irate_w aveq_w L.cashflowrate_w, absorb(gvkey)
eststo Qaug
estadd local fixed "Firm"

* Print tables
#delimit ;
esttab  using "$filepath1/tables/a-Q-model.tex" , 
title (Q-model of investment) label 	
nobaselevels ar2 mtitles(Q-model Augmented-Q-Model) 
s(fixed N, label("FE"))
nodepvar replace booktabs;
#delimit cr

********************************************************************************
***   Point (b)  ***************************************************************
********************************************************************************
* Estimate the augmented Q-model using the within estimator (LSDV).Then estimate the model by GMM using xtabond2 in Stata (allowing for common year effects)

eststo clear

reghdfe irate_w aveq_w L.cashflowrate_w, absorb(gvkey sic#year)

eststo LSDV
estadd local fixed "Firm, Year"

xtabond2 irate_w aveq_w L.cashflowrate_w i.year, gmm(irate_w, lag(2 4)) iv(i.year) noleveleq robust

eststo GMMxtabond2
estadd local fixed "Firm, Year"

* Print tables
#delimit ;
esttab  using "$filepath1/tables/b-Q-model-LSDV-GMM.tex" , 
title (LSDV and GMM Estimation) label 	
nobaselevels ar2 mtitles(LSDV GMM) keep(aveq_w L.cashflowrate_w _cons) 
s(fixed N, label("FE"))
nodepvar replace booktabs;
#delimit cr


********************************************************************************
***   Point (c,d) **************************************************************
********************************************************************************
* Try both GMM difference estimator and the system estimator and discuss the relative merits. Try both one step and two step estimator

********************************************************************************
*                                  Estimation      

eststo clear
* Difference GMM one step (without Windmeijer’s finite-sample correction)
eststo d_gmm_one: xtabond2 irate_w aveq_w L.cashflowrate_w i.year, gmm(irate_w, lag(2 4)) iv(i.year) noleveleq 

* System GMM one step (with Windmeijer’s finite-sample correction)
eststo sgmm_one: xtabond2 irate_w aveq_w L.cashflowrate_w i.year, gmm(irate_w, lag(2 4)) iv(i.year) robust

* Difference GMM two step (with Windmeijer’s finite-sample correction)
eststo d_gmm_two: xtabond2 irate_w aveq_w L.cashflowrate_w i.year, gmm(irate_w, lag(2 4)) iv(i.year) noleveleq two robust

* Difference GMM two step (without Windmeijer’s finite-sample correction)
eststo d_gmm_two_nocorr: xtabond2 irate_w aveq_w L.cashflowrate_w i.year, gmm(irate_w, lag(2 4)) iv(i.year) noleveleq two 

* System GMM two step (with Windmeijer’s finite-sample correction)
eststo sgmm_two: xtabond2 irate_w aveq_w L.cashflowrate_w i.year, gmm(irate_w, lag(2 4)) iv(i.year) two robust

*System GMM two step (without Windmeijer’s finite-sample correction)
eststo sgmm_nocorr: xtabond2 irate_w aveq_w L.cashflowrate_w i.year, gmm(irate_w, lag(2 4)) gmm(aveq_w cashflowrate_w, lag(3 4)) iv(i.year) two

#delimit ;
esttab  using "$filepath1/tables/c-Q-model-GMM.tex" , 
title (Difference v. System GMM) label 	
nobaselevels ar2 mtitles(d-GMM-1 s-GMM-1 d-GMM-2-Wind d-GMM-2 s-GMM-2-Wind s-GMM-2) keep(aveq_w L.cashflowrate_w _cons) 
s(sargan sarganp hansen hansenp ar1 ar1p ar2 ar2p  N)
nodepvar replace booktabs;
#delimit cr



	
	
********************************************************************************
***   Point (e)  ***************************************************************
********************************************************************************
* Divide the sample in two types of firms based on wether emp is lower or greater than 250. Re estimate Q model allowing the coefficients to be different for small and large firms. (Emp is in thousands)

* Dummy for small and large firms
gen small_firm=1 if emp<0.25
replace small_firm=0 if emp>=0.25
gen large_firm=1-small_firm

* Interaction for large/small firms and cashflowrate_w
gen large_cfr=L.large_firm*L.cashflowrate_w
gen small_cfr=L.small_firm*L.cashflowrate_w

********************************************************************************
*                                  Estimation      

eststo clear

* Estimation of Q model 
eststo qs: reghdfe irate_w c.L.cashflowrate_w##i.L.small_firm c.L.aveq_w##i.L.small_firm , absorb(gvkey year)


* Difference GMM one step (without Windmeijer’s finite-sample correction)
eststo d_gmm_one_nocorr: xtabond2 irate_w c.L.cashflowrate_w##i.L.small_firm c.L.aveq_w##i.L.small_firm i.year, gmm(irate_w, lag(2 4)) iv(i.year) noleveleq  

* System GMM one step (without Windmeijer’s finite-sample correction)
eststo sgmm_one_nocorr: xtabond2 irate_w c.L.cashflowrate_w##i.L.small_firm c.L.aveq_w##i.L.small_firm i.year, gmm(irate_w, lag(2 4)) iv(i.year) 

* Difference GMM two step (with Windmeijer’s finite-sample correction)
eststo d_gmm_two: xtabond2 irate_w c.L.cashflowrate_w##i.L.small_firm c.L.aveq_w##i.L.small_firm i.year, gmm(irate_w, lag(2 4)) iv(i.year) noleveleq two robust

* Difference GMM two step (without Windmeijer’s finite-sample correction)
eststo d_gmm_two_nocorr: xtabond2 irate_w c.L.cashflowrate_w##i.L.small_firm c.L.aveq_w##i.L.small_firm i.year, gmm(irate_w, lag(2 4)) iv(i.year) noleveleq two 

* System GMM two step (with Windmeijer’s finite-sample correction)
eststo sgmm_two: xtabond2 irate_w c.L.cashflowrate_w##i.L.small_firm c.L.aveq_w##i.L.small_firm i.year, gmm(irate_w, lag(2 4)) iv(i.year) two robust

*system GMM two step (without Windmeijer’s finite-sample correction)
eststo sgmm_two_nocorr: xtabond2 irate_w c.L.cashflowrate_w##i.L.small_firm c.L.aveq_w##i.L.small_firm i.year, gmm(irate_w, lag(2 4)) gmm(aveq_w cashflowrate_w, lag(3 4)) iv(i.year) two

#delimit ;
esttab  using "$filepath1/tables/e-Q-model-GMM.tex" , 
title (Regression Results ) label 	
nobaselevels ar2 mtitles( qs d-GMM-1 s-GMM-1 d-GMM-2-Wind d-GMM-2 s-GMM-2-Wind s-GMM-2) 
s(sargan sarganp hansen hansenp ar1 ar1p ar2 ar2p  N)
nodepvar replace booktabs;
#delimit cr


********************************************************************************
***   Point (f)  ***************************************************************
********************************************************************************
* Estimate the Euler equations for capital in the parametrization of the lecture notes

* With perfect competition, pi is operating profit
gen beta=0.95
gen delta=0.05
gen cst=beta*cashflowrate
gen irate2=L.irate_w*L.irate_w
gen cst_oi=L.cst*L.oibdprate_w
gen cst_irate2=L.cst*L.irate2
gen cst_ic=(1-delta)*L.cst*L.irate

eststo clear
*GMM difference one step
eststo pc_gmm1:xtabond2 irate cst_oi cst_irate2 cst_ic i.year, gmm(oibdprate_w irate, lag(2 4)) iv(i.year) noleveleq robust
*GMM difference two step 
eststo pc_gmm2:xtabond2 irate cst_oi cst_irate2 cst_ic i.year, gmm(oibdprate_w irate, lag(2 4)) iv(i.year) noleveleq two robust
*system GMM one step
eststo pc_gmm_s1:xtabond2 irate cst_oi cst_irate2 cst_ic i.year, gmm(oibdprate_w irate, lag(2 4)) iv(i.year) robust
*system GMM two step
eststo pc_gmm_s2:xtabond2 irate cst_oi cst_irate2 cst_ic i.year, gmm(oibdprate_w irate, lag(2 4)) iv(i.year) two robust

#delimit ;
esttab  using "$filepath1/tables/f-Euler-GMM-perfect.tex" , 
title (Euler Equation GMM (Perfect Competition)) label 	
nobaselevels ar2 mtitles(d-GMM-1 d-GMM-2 s-GMM-1 s-GMM-2) 
s(N)
nodepvar replace booktabs;
#delimit cr

	
	
	
*With mperfect competition, thus pi is equal to (alpha/mu)*(Y/K)
*I assume markup in USA (1.3 Hall 2018)

gen mu=1.3
gen pie=(L.capshare*L.output_cap_r)/mu
gen cst_pi=L.cst*pie


eststo clear
*GMM difference one step
eststo pc_gmm1:xtabond2 irate cst_pi cst_irate2 cst_ic i.year, gmm(oibdprate_w irate, lag(2 4)) iv(i.year) noleveleq robust
*GMM difference two step 
eststo pc_gmm2:xtabond2 irate cst_pi cst_irate2 cst_ic i.year, gmm(oibdprate_w irate, lag(2 4)) iv(i.year) noleveleq two robust
*system GMM one step
eststo pc_gmm_s1:xtabond2 irate cst_pi cst_irate2 cst_ic i.year, gmm(oibdprate_w irate, lag(2 4)) iv(i.year) robust
*system GMM two step
eststo pc_gmm_s2:xtabond2 irate cst_pi cst_irate2 cst_ic i.year, gmm(oibdprate_w irate, lag(2 4)) iv(i.year) two robust

#delimit ;
esttab  using "$filepath1/tables/f-Euler-GMM-imperfect.tex" , 
title (Euler Equation GMM (Imperfect Competition)) label 	
nobaselevels ar2 mtitles(d-GMM-1 d-GMM-2 s-GMM-1 s-GMM-2) 
s(N)
nodepvar replace booktabs;
#delimit cr

	
	
