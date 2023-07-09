********************************************************
**** Empirical Exercise ********************************
**** Monetary II - Compustat ***************************
**** Prof. Fabio Schiantarelli *************************
**** Vaibhav Ojha **************************************
********************************************************


* The folder structure should be as follows:- 
* On the Desktop there should be a folder called Compustat 
* Within Compustat there should be sub-folders called "Data", "Results", and "Code"
* All the data files should be stored in the sub-folder "Data"

* Basically we can put the filepath here that corresponds to the laptop or the computer on which we decide to run this code. *
global filepath1 "/Users/ojhav/Desktop/Compustat/"

 

clear 
cls

log using "$filepath1/Code/Compustat_Vaibhav_log.smcl", replace

***** NBER CES Data ************************************

use "$filepath1/Data/nberces5818v1_s1987.dta", clear
compress
summarize

* This allows us to setup the data in time series format *
tsset sic year

* We generate the labor share in value added and use other lagged values of this metric uptil 10 time periods *
* And average them out to get "labshare" for every value of "sic". Then when we are done with the use of this *
* "labor" variable we drop it and move forward with our analysis. *********************************************

gen lab_s = pay/vadd
bysort sic: gen lab_s_final = (lab_s + L1.lab_s + L2.lab_s + L3.lab_s + L4.lab_s + L5.lab_s + L6.lab_s + L7.lab_s + L8.lab_s + L9.lab_s)/10
drop lab_s

* Now we have our labour share variable "lab_s_final" ready for the analysis later on *
* For readability we give a label to this newly defined variable below ****************

label var lab_s_final "Labour share in value added ten years MA"

* Using the labour share expression that we have calculated above we compute the capital share equation as follows *
gen cap_s_final = 1 - lab_s_final
* Basically this means that we have two inputs in our production function - labour and capital *

* Again for the purpose of readability we give this variable a label ***************************
label var cap_s_final "Capital share, as the residual of the implied labour share"

* Now we create a variable that capturs the ratio of value added to gross output (shipments) *
gen value_to_output = vadd/vship 
* Now we want to take a 10 year MA of this variable as given below ***************************
bysort sic: gen value_add_ma10 = (value_to_output + L1.value_to_output + L2.value_to_output + L3.value_to_output + L4.value_to_output + L5.value_to_output + L6.value_to_output + L7.value_to_output + L8.value_to_output + L9.value_to_output)/10
* Now that our use is done we drop the "value_to_output" variable ****************************
drop value_to_output

* Again for the purpose of readability I assign a label to this newly created variable *******
label var value_add_ma10 "Value added to gross output - shipments - ten years MA"

* Now we only keep the variables that we are going to use ahead in our analysis **************
keep sic year lab_s_final cap_s_final value_add_ma10 piship piinv

* We save this transformed data subset to use ahead ******************************************
save "$filepath1/Data/nber_ces_transform.dta", replace 


**********************************************************************************************
** 1.2 Clean, Merge Compustat data ***********************************************************
**********************************************************************************************

clear

* sections 1.2 (a) to (g) are done on the WRDS website. ************************
* sub-part (h) entails keeping only US-based firms. ****************************
* we load the Compustat dataset here. ******************************************

use "$filepath1/Data/comp_fundamentals_annual.dta", clear

* We only keep observations if they correspond to the USA **********************
keep if fic == "USA"

* sub-part (i) ***************************************************************** 
* Additionally we want to keep only the final versions of the statements *******
keep if final == "Y"
compress

* sub-part (j) *****************************************************************
* Here we drop financial companies and regulated utilities *********************

* Here we destring the company key to be able to use it to conduct operations **
destring gvkey, replace

* This will essentially take care of any duplications in the values for ********
* industry format and currency *************************************************

drop if (indfmt == "FS")
drop if (curncd == "CAD")
keep if (fyear != .)
drop if (gvkey == 66552 & fyear>=2010 & fyear<=2018 & fyr == 6)
* Now we set the data as panel by using the gvkey and the year variable ********
tsset gvkey fyear 
* The first variable here, gvkey will serve as the panel identifier whereas ****
* The second variable here, year will serve as the year identifier *************

* We destring the sic variable to use its values to trim the data a bit ********
destring sic, replace
* We drop the observations if either sic is in the range (4900,5000) or (6000,7000)
drop if (sic>=4900 & sic<5000) | (sic>=6000 & sic<7000)

* sub-part (k) ********************************************************************
* Now we trim the data further if the values for acquisitions is extremely large *
gen aqc_per = aqc/at
* We drop the observations here if the acquisitions percent is more than 5 percent *
drop if aqc_per > 0.05
* We are done with the use of aqc_per so we drop it here ***************************
drop aqc_per

* sub-part (l) *********************************************************************
* Shifting the book value of capital forward by one year ***************************
gen iter_ppent = L1.ppent
* We drop the earlier ppent variable as it is not the variable of interest anymore *
drop ppent
* We rename iter_ppent as ppent because now this variable will take the role of ppent *
rename iter_ppent ppent
* Therefore, we have moved the book value of capital forward by one year **************
label var ppent "Property, Plant and Equipment - Total (Net)"
* We assign the label of the earlier ppent variable to the new ppent variable *********

* Now we trim again but this time we want to keep observations or data only if * 
* they are non-missing and positive ********************************************

* Here we remove missing values ************************************************
keep if (at!=. & ppent!=. & emp!=. & capxv!=. & sale!=.)
* Here we remove non-positive observations *************************************
keep if (at>0 & ppent>0 & emp>0 & capxv>0 & sale>0)

* sub-part (n) *****************************************************************
* Here we keep only if firms exist for greater than or equal to 2 years ********
* so we start by creating a counter variable for every company (firm) count ****
* the number of observations which would correspond to the length of time in ***
* years for which the company was alive. Then when we have that we can drop ****
* observations if the number of observations or the number of years for which **
* the company was alive was less than 2 ****************************************

* conm is the variable that captures company name so it serves as a demarcation*
* for a company ****************************************************************
bysort conm: gen obs = _N
drop if obs < 2
* We label our newly created variable over here ********************************
label var obs "Number of observations"

* sub-part (o) *****************************************************************
* We are going to merge the Compustat and NBER CES data now that we have made **
* desirable updates to both of these datasets. In the Compustat dataset we *****
* rename fyear as year as that is the corresponding variable name for the year *
* variable in the NBER CES dataset. Then using "year" and "sic" as the key *****
* variables in this dataset we perform a many to one merge where the master ****
* dataset is the Compustat and the using dataset is the NBER CES. Thus, we will*
* have more columns associated with an obs. identified using the "sic" and ***** 
* "year" pair. Then we basically retain only those observations for which we ***
* have an exact match across the master and using dataset. Then we sort the ****
* dataset on the basis of "gvkey". Then we destring "gvkey" to use it to create *
* a panel data where "gvkey" identifies the panel element and "year" identifies *
* the time or year element. *****************************************************

* Making the key variable compatible across datasets
rename fyear year 
* Merging
merge m:1 year sic using "$filepath1/Data/nber_ces_transform.dta", keep(3)
* Sorting 
sort gvkey
* Destringing the gvkey
destring gvkey, replace
* Making a panel
tsset gvkey year
compress
drop _merge
* Saving
save "$filepath1/Data/compustat_nberces_merged.dta", replace

********************************************************************************
* (1.3): Here we compute some series of interest *******************************
********************************************************************************

clear

use "$filepath1/Data/compustat_nberces_merged.dta", clear 

* sub-parts (a), (b), and (c): We generate "investment", "capital" and "investment rate". 

gen investment = capxv/piinv
gen capital = ppent/piinv
gen irate = (investment/capital)*100

* sub-part (d): We create the value added output series deflated by price index.
gen output_val_add = (sale*value_add_ma10)/piship

* sub-part (e): We compute the growth in output and employment using the *******
* Davis-Haltiwanger formula. So basically the percentage change in the variable* 
* from last year where the base is not the value of the variable last year *****
* but the average of the values of the variable last year and this year. *******

gen output_gr_rate = (output_val_add - L1.output_val_add)/(0.5*(output_val_add + L1.output_val_add)) * 100
gen emp_gr_rate = (emp - L1.emp)/(0.5*(emp + L1.emp)) * 100

* sub-part (f) and (g): We are calculating TFP and its growth rate. ************
* We have the twin assumptions of perfect competition and Cobb-douglas *********
** production function. ********************************************************

* TFP: Total Factor Productivity 
* Basically we are writing it in terms of the Solow residual or residual 
* Essentially the part of the value added in output which is not captured by 
* The two main factors of production - capital and labour. 
gen tot_fac_prod = log(output_val_add) - cap_s_final*log(capital) - lab_s_final*log(emp)

* TFP growth rate: We compute this using the standard way and 
* not using the Davis-Haltiwanger formula. 
gen tot_fac_prod_gr_rate = (tot_fac_prod - L1.tot_fac_prod)/100

* We create some variables for operating income rate, casflow rate, output and 
* the ratio of output to capital.

* sub-part (h): Operating income rate
gen operating_inc_rate = oibdp/ppent
* sub-part (i): Generating cashflow rate
gen cash_fl_rate = (oibdp - intpn - txt + nopi + spi)/ppent
* sub-part (j): Generating output 
gen output = (value_add_ma10/piship)*sale
* sub-part (k): Generating ratio of output to capital 
gen out_cap_ratio = output/capital 

* sub-part (l): If observations are less than for 5 years then drop it *********
drop if obs < 5

ssc install winsor
ssc install winsor2

foreach x in output_gr_rate emp_gr_rate tot_fac_prod_gr_rate operating_inc_rate cash_fl_rate irate {

* We conduct the winsoring for the aforementioned variables here 
* which lie in the tails given by the tail at the 95% and the bottom 5%
winsor2 `x', replace cuts(5 95)

* Now for these transformed variables we look at the mean and standard deviation

summarize `x'
local m = r(mean)
local sd = r(sd)

hist `x', percent 

* Basically these histograms will give us a sense of how the variables look ****
* like after transformation. ***************************************************

* Saving the histograms as graphs **********************************************
graph export "$filepath1/Data/hist_`x'.png", replace

}

* We save the final dataset that we get after this whole editing below as ******
save "$filepath1/Data/compustat_nberces_transform_data.dta", replace

* asdoc will help in exporting the Stata output to MS Word so it is a way of ***
* exporting results in a presentable and ready to use format. Like it is a *****
* command on the back end that creates something which will be used on the *****
* front end. *******************************************************************

ssc install asdoc 
* After installing asdoc we are basically calculating the summary statistics ***
* of some of the variables of interest and exporting those results in MS word **
cd "$filepath1/Data"
// cd "/Users/ojhav/Desktop/Compustat Stata/Data"
asdoc sum output_gr_rate emp_gr_rate tot_fac_prod_gr_rate operating_inc_rate irate cash_fl_rate, save("Summary_Results_Compustat") replace



********************************************************************************
********** The latter part of the empirical exercise ***************************
********************************************************************************

**************************************
*Code for Q2 of the empirical exercise
**************************************

* The objective here is to estimate Q Models and euler equations for the capital stock by panel GMM *

*** We clear the screen and dataset so as to start fresh ***
clear all 
cls

*** We use the final dataset that we had saved in the Q1 part of the empirical exercise ***
*use "/Users/ojhav/Desktop/Compustat Stata/Data/compustat_nberces_transform_data.dta" ,replace
use "$filepath1/Data/compustat_nberces_transform_data.dta", replace

* Generate average-Q: market value of firm/ capital stock
gen aveq = ((prcc_c*csho)+ dt )/capital
label var aveq   "average-Q" 

* Drop outliers in Q
gen outlier = 0 
_pctile aveq, nq(200)
replace outlier=1 if (aveq >r(r190)| aveq <r(r10))
by gvkey: egen outlier1 = total(outlier)
replace outlier = outlier1
drop if outlier ==1
drop outlier
drop outlier1


* Keep only firms that have at least six years of continuous observations.
gen run = .
by gvkey: replace run = cond(L.aveq == ., 1, L.run + 1)
by gvkey: egen maxrun = max(run)
drop if maxrun <6


* Divide the sample into two types of firms based on the number of employees 

* generate employee dummy 
gen emp_const = 0
replace emp_const = 1 if emp < 75
label var emp_const   "equals 1 if number of employees is less than 75, zero otherwise"


************************************************************
* A suggestive test of financial frictions:
* 	Is the residual of the regression more correlated 
*
*	If yes, financial frictions may play a role.
************************************************************

* output_gr_rate emp_gr_rate tot_fac_prod_gr_rate operating_inc_rate cash_fl_rate

foreach x in tot_fac_prod emp_gr_rate irate output_gr_rate tot_fac_prod_gr_rate operating_inc_rate cash_fl_rate out_cap_ratio{
* Remove firm fixed effects
xtreg `x',fe
predict `x'_fe, e
label var `x'_fe "`x' - fixed effects removed" 

}

ssc install estout, replace
ssc install reghdfe

* Estimate Q-model of investment
eststo: reg irate aveq cash_fl_rate, robust 
eststo: reg irate aveq emp_const##c.cash_fl_rate, robust
esttab using "$filepath1/Results/Qmodel_OLS.tex", stats(r2 N) wide replace title(Pooled OLS Q-model\label{tab1})
eststo clear


* Estimate Euler Equation
gen irate_fe_squared = irate_fe*irate_fe
gen irate_squared = irate*irate
gen oc = output/capital
eststo: reg irate_fe L.irate_fe operating_inc_rate L.irate_fe_squared emp_const, robust
eststo: reg irate_fe L.irate_fe operating_inc_rate L.irate_fe_squared oc emp_const, robust
esttab using "$filepath1/Results/Euler_OLS.tex", stats(r2 N) wide replace title(Pooled OLS Euler Equation\label{tab1})
eststo clear



******************************************************************************
* Estimate the Q-model model using "Difference GMM estimation" and save results
******************************************************************************


* Q-model: difference GMM
ssc install xtab
ssc install xtable
ssc install xtabond2

eststo:xtabond2 irate aveq cash_fl_rate i.year, gmm(aveq  cash_fl_rate, lag(2 4)) iv(i.year) noleveleq two robust 
// two-step difference with Windj corr s.e.
eststo:xtabond2 irate aveq cash_fl_rate i.year if emp_const == 0 , gmm(aveq cash_fl_rate, lag(2 4)) iv(i.year) noleveleq two robust
eststo:xtabond2 irate aveq cash_fl_rate i.year if emp_const == 1 , gmm(aveq cash_fl_rate, lag(2 4)) iv(i.year) noleveleq two robust
eststo:xtabond2 irate aveq cash_fl_rate i.year, gmm(aveq cash_fl_rate, lag(2 4)) iv(i.year) noleveleq two 
// two-step difference without Windj corr s.e.
eststo:xtabond2 irate aveq cash_fl_rate i.year if emp_const == 0 , gmm(aveq cash_fl_rate, lag(2 4)) iv(i.year) noleveleq two 
eststo:xtabond2 irate aveq cash_fl_rate i.year if emp_const == 1 , gmm(aveq cash_fl_rate, lag(2 4)) iv(i.year) noleveleq two 
esttab using "$filepath1/Results/Qmodel_diff.tex", stats(sarganp N) replace title(Difference GMM Q-model \label{tab1})
eststo clear


* Euler equation: difference GMM
eststo:xtabond2 irate L.irate operating_inc_rate L.irate_squared i.year, gmm(L.irate operating_inc_rate L.irate_squared, lag(2 4)) iv(i.year) noleveleq two robust // with Windj corr s.e.
eststo:xtabond2 irate L.irate operating_inc_rate L.irate_squared i.year if emp_const == 0, gmm(L.irate operating_inc_rate L.irate_squared, lag(2 4)) iv(i.year) noleveleq two robust // with Windj corr s.e.
eststo:xtabond2 irate L.irate operating_inc_rate L.irate_squared i.year if emp_const == 1, gmm(L.irate operating_inc_rate L.irate_squared, lag(2 4)) iv(i.year) noleveleq two robust // with Windj corr s.e.
eststo:xtabond2 irate L.irate operating_inc_rate L.irate_squared i.year, gmm(L.irate operating_inc_rate L.irate_squared, lag(2 4)) iv(i.year) noleveleq two // with Windj corr s.e.
eststo:xtabond2 irate L.irate operating_inc_rate L.irate_squared i.year if emp_const == 0, gmm(L.irate operating_inc_rate L.irate_squared, lag(2 4)) iv(i.year) noleveleq two // with Windj corr s.e.
eststo:xtabond2 irate L.irate operating_inc_rate L.irate_squared i.year if emp_const == 1, gmm(L.irate operating_inc_rate L.irate_squared, lag(2 4)) iv(i.year) noleveleq two // with Windj corr s.e.
esttab using "$filepath1/Results/Euler_diff.tex", stats(sarganp N) replace title(Difference GMM Euler Equation \label{tab1})
eststo clear


* Q-model: system estimator
eststo:xtabond2 irate aveq cash_fl_rate i.year, gmm(cash_fl_rate aveq, lag(2 4)) iv(i.year) two robust // two-step difference with Windj corr s.e.
eststo:xtabond2 irate aveq cash_fl_rate i.year if emp_const == 0 , gmm(aveq cash_fl_rate, lag(2 4)) iv(i.year) two robust
eststo:xtabond2 irate aveq cash_fl_rate i.year if emp_const == 1 , gmm(aveq cash_fl_rate, lag(2 4)) iv(i.year) two robust
eststo:xtabond2 irate aveq cash_fl_rate i.year, gmm(aveq cash_fl_rate, lag(2 4)) iv(i.year) two // two-step difference without Windj corr s.e.
eststo:xtabond2 irate aveq cash_fl_rate i.year if emp_const == 0 , gmm(aveq cash_fl_rate, lag(2 4)) iv(i.year) two 
eststo:xtabond2 irate aveq cash_fl_rate i.year if emp_const == 1 , gmm(aveq cash_fl_rate, lag(2 4)) iv(i.year) two 
esttab using "$filepath1/Results/Qmodel_system.tex", stats(sarganp N) replace title(System Estimator Q-model \label{tab1})

eststo clear

* Euler equation: system estimator
eststo:xtabond2 irate L.irate operating_inc_rate L.irate_squared i.year, gmm(L.irate operating_inc_rate L.irate_squared, lag(2 4)) iv(i.year) two robust // with Windj corr s.e.
eststo:xtabond2 irate L.irate operating_inc_rate L.irate_squared i.year if emp_const == 0, gmm(L.irate operating_inc_rate L.irate_squared, lag(2 4)) iv(i.year) two robust // with Windj corr s.e.
eststo:xtabond2 irate L.irate operating_inc_rate L.irate_squared i.year if emp_const == 1, gmm(L.irate operating_inc_rate L.irate_squared, lag(2 4)) iv(i.year) two robust // with Windj corr s.e.
eststo:xtabond2 irate L.irate operating_inc_rate L.irate_squared i.year, gmm(L.irate operating_inc_rate L.irate_squared, lag(2 4)) iv(i.year) two // with Windj corr s.e.
eststo:xtabond2 irate L.irate operating_inc_rate L.irate_squared i.year if emp_const == 0, gmm(L.irate operating_inc_rate L.irate_squared, lag(2 4)) iv(i.year) two // with Windj corr s.e.
eststo:xtabond2 irate L.irate operating_inc_rate L.irate_squared i.year if emp_const == 1, gmm(L.irate operating_inc_rate L.irate_squared, lag(2 4)) iv(i.year) two // with Windj corr s.e.
esttab using "$filepath1/Results/Euler_system.tex", stats(sarganp N) replace title(System Estimator Euler Equation \label{tab1})

eststo clear


log close 
