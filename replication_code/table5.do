

   /* [>   0.  Github integration   <] */ 
/*----------------------------------------------------*/
/*----------------------------------------------------*/
/* [> Commit and push any important changes to github regularly. <] */ 
/*
cd "${github}"
! git add "${github}/replication_code/table4.do"
! git commit -m "Added code for table 4. (So far no changes)"
! git push
*/

 /* [> New branch for testing new code <] */
 // git checkout -b name-of-branch
clear 
cd "${main}/regressions"
 

 /*----------------------------------------------------*/
    /* [>   Description of file   <] */ 
 /*----------------------------------------------------*/

* Here we select the baseline 
global lag_specif "lag(1000000) dist(150) lagdist(1000000) partial "
global clus "r cl(id)"
global controlsFE  "govern_* foreign_* unpopular_*          D96_* D30_* D41_* D471_*"
global IVBaseline "rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0 rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 rain_enemies_enemies0 sqrain_enemies_enemies0 rain_enemies_of_allies0 sqrain_enemies_of_allies0 rain_enemies_enemies1 sqrain_enemies_enemies1 rain_enemies_of_allies1 sqrain_enemies_of_allies1 rain_neutral0 sqrain_neutral0 rain_neutral1 sqrain_neutral1"
global baseline_specification "  xtivreg TotFight (TotFight_Enemy TotFight_Allied  TotFight_Neutral =  $IVBaseline)  meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1 $controlsFE, fe i(group) "

* we also select the relevant monadic and dyadic dataset before building adjency matrices
use KRTZ_monadic_AF.dta, clear
save KRTZ_monadic_ref.dta, replace
use KRTZ_dyadic_AF.dta, clear
save KRTZ_dyadic_ref, replace




**********************
* TABLE 5 (Top Panel) 
**********************

** Remove foreign groups
import excel using ../original_data/coding_characteristics_FZ.xls, clear first
keep Group FOREIGNGROUPS
count if FOREIGNGROUPS==1 
local ng = r(N)
rename Group name 
rename FOREIGNGROUPS Foreign_FZ
sort name 
save temp_fz, replace
qui use avgbench_data, clear
sort name
merge name using temp_fz
tab _merge
drop if _merge==2
count
drop _merge
erase temp_fz.dta
table Foreign Foreign_FZ
replace Foreign=Foreign_FZ
qui sort MCref
drop if Foreign ==1
qui sort MCref year
qui save temp_MC, replace
sort group
merge group using obs_share.dta
tab _merge
keep if _merge==3
drop _merge
collapse (sum) observed_share
gen obs_share=1-observed_share
keep obs_share
save observed.dta, replace
use temp_MC, clear
keep MCref
sort MCref
save trash.dta, replace
rename MCref MCref_d
sort MCref_d
save trash_d.dta, replace
qui use bench_aminus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d 
qui save temp_aminus, replace
qui use bench_aplus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
erase trash.dta
erase trash_d.dta
qui sort MCref MCref_d
qui save temp_aplus, replace
qui do ../progs/eq_simul.do
qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_KPresult, replace
use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
save Foreign_KPresult, replace
label var Delta_RD  "count. change in aggregate fight."
merge using observed.dta
save Foreign_KPresult, replace
sum obs_share
local o = r(mean)
sum Delta_RD
local d = r(mean)
local mult = -`d'/`o'
erase observed.dta

clear
tempfile x 
set obs 10 
gen set = ""
gen no = "" 
gen share = ""
gen rd = ""
gen mult = "" 
gen mad = ""
gen newen = ""
gen regcoef = ""

local j = 1
replace set = "Foreign Groups" if _n==`j'| _n==`j'+5
replace no = "`ng'" if _n==`j'| _n==`j'+5
replace share = string(`o', "%9.3f") if _n==`j'| _n==`j'+5
replace rd = string(-`d', "%9.3f") if _n==`j'
replace mult = string(`mult', "%9.1f") if _n==`j'

save `x'




** Remove large groups

qui use avgbench_data, clear
qui sort MCref
sum degree_minus, d
count if degree_minus>4 
local ng = r(N)
drop if degree_minus >4
qui sort MCref year
qui save temp_MC, replace
sort group
merge group using obs_share.dta
tab _merge
keep if _merge==3
drop _merge
collapse (sum) observed_share
gen obs_share=1-observed_share
keep obs_share
save observed.dta, replace
use temp_MC, clear
keep MCref
sort MCref
save trash.dta, replace
rename MCref MCref_d
sort MCref_d
save trash_d.dta, replace

qui use bench_aminus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d 
qui save temp_aminus, replace

qui use bench_aplus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
erase trash.dta
erase trash_d.dta
qui sort MCref MCref_d
qui save temp_aplus, replace

qui do ../progs/eq_simul.do
qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
save Foreign_KPresult, replace
label var Delta_RD  "count. change in aggregate fight."

merge using observed.dta
sum obs_share
local o = r(mean)
sum Delta_RD
local d = r(mean)
local mult = -`d'/`o'
erase observed.dta

clear
use `x'
local j=5
replace set = "Large Groups" if _n==`j' | _n==`j'+5
replace no = "`ng'" if _n==`j' | _n==`j'+5
replace share = string(`o', "%9.3f") if _n==`j' | _n==`j'+5
replace rd = string(-`d', "%9.3f") if _n==`j' 
replace mult = string(`mult', "%9.1f") if _n==`j'

save `x', replace 







** Remove ITURI-related groups
import excel using ../original_data/coding_characteristics_FZ.xls, clear first
keep Group ITURI
rename Group name 
destring ITURI, gen(Ituri)
count if ITURI=="1"
local ng=r(N)
sort name 
save temp_fz, replace
qui use avgbench_data, clear
sort name
merge name using temp_fz
tab _merge
drop if _merge==2
count
drop _merge
erase temp_fz.dta
qui sort MCref
drop if Ituri ==1
qui sort MCref year
qui save temp_MC, replace
sort group
merge group using obs_share.dta
tab _merge
keep if _merge==3
drop _merge
collapse (sum) observed_share
gen obs_share=1-observed_share
keep obs_share
save observed.dta, replace
use temp_MC, clear
keep MCref
sort MCref
save trash.dta, replace
rename MCref MCref_d
sort MCref_d
save trash_d.dta, replace

qui use bench_aminus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d 
qui save temp_aminus, replace

qui use bench_aplus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
erase trash.dta
erase trash_d.dta
qui sort MCref MCref_d
qui save temp_aplus, replace

qui do ../progs/eq_simul.do
qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
save Foreign_KPresult, replace
label var Delta_RD  "count. change in aggregate fight."

merge using observed.dta
sum obs_share
local o = r(mean)
sum Delta_RD
local d = r(mean)
local mult = -`d'/`o'
erase observed.dta

clear
use `x'
local j = 2
replace set = "Ituri" if _n==`j'| _n==`j'+5
replace no = "`ng'" if _n==`j'| _n==`j'+5
replace share = string(`o', "%9.3f") if _n==`j'| _n==`j'+5
replace rd = string(-`d', "%9.3f") if _n==`j'
replace mult = string(`mult', "%9.1f") if _n==`j'

save `x', replace 


** Remove RCD&UG&RWA groups
import excel using ../original_data/coding_characteristics_FZ.xls, clear first
keep Group RCDUGRWA
count if RCDUGRWA=="1" 
local ng=r(N)
rename Group name 
destring RCDUGRWA, gen(remove)
sort name 
save temp_fz, replace
qui use avgbench_data, clear
sort name
merge name using temp_fz
tab _merge
drop if _merge==2
count
drop _merge
erase temp_fz.dta
qui sort MCref
drop if remove ==1
qui sort MCref year
qui save temp_MC, replace
sort group
merge group using obs_share.dta
tab _merge
keep if _merge==3
drop _merge
collapse (sum) observed_share
gen obs_share=1-observed_share
keep obs_share
save observed.dta, replace
use temp_MC, clear
keep MCref
sort MCref
save trash.dta, replace
rename MCref MCref_d
sort MCref_d
save trash_d.dta, replace

qui use bench_aminus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d 
qui save temp_aminus, replace

qui use bench_aplus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
erase trash.dta
erase trash_d.dta
qui sort MCref MCref_d
qui save temp_aplus, replace

qui do ../progs/eq_simul.do
qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
save Foreign_KPresult, replace
label var Delta_RD  "count. change in aggregate fight."
merge using observed.dta
sum obs_share
local o = r(mean)
sum Delta_RD
local d = r(mean)
local mult = -`d'/`o'
erase observed.dta

clear
use `x'
local j = 4
replace set = "Rwa \& Uga \& ass." if _n==`j'| _n==`j'+5
replace no = "`ng'" if _n==`j'| _n==`j'+5
replace share = string(`o', "%9.3f") if _n==`j'| _n==`j'+5
replace rd = string(-`d', "%9.3f") if _n==`j'
replace mult = string(`mult', "%9.1f") if _n==`j'

save `x', replace 


** Remove FDLR&INTER&HUTU groups
import excel using ../original_data/coding_characteristics_FZ.xls, clear first
keep Group FDLRINTERHUTU
count if FDLRINTERHUTU==1
local ng=r(N)
rename Group name 
rename FDLRINTERHUTU remove 
sort name 
save temp_fz, replace
qui use avgbench_data, clear
sort name
merge name using temp_fz
tab _merge
drop if _merge==2
count
drop _merge
erase temp_fz.dta
qui sort MCref
drop if remove ==1
qui sort MCref year
qui save temp_MC, replace
sort group
merge group using obs_share.dta
tab _merge
keep if _merge==3
drop _merge
collapse (sum) observed_share
gen obs_share=1-observed_share
keep obs_share
save observed.dta, replace
use temp_MC, clear
keep MCref
sort MCref
save trash.dta, replace
rename MCref MCref_d
sort MCref_d
save trash_d.dta, replace

qui use bench_aminus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d 
qui save temp_aminus, replace

qui use bench_aplus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
erase trash.dta
erase trash_d.dta
qui sort MCref MCref_d
qui save temp_aplus, replace

qui do ../progs/eq_simul.do
qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
save Foreign_KPresult, replace
label var Delta_RD  "count. change in aggregate fight."
merge using observed.dta
sum obs_share
local o = r(mean)
sum Delta_RD
local d = r(mean)
local mult = -`d'/`o'
erase observed.dta

clear
use `x'
local j = 3
replace set = "Out of Rwanda" if _n==`j'| _n==`j'+5
replace no = "`ng'" if _n==`j'| _n==`j'+5
replace share = string(`o', "%9.3f") if _n==`j'| _n==`j'+5
replace rd = string(-`d', "%9.3f") if _n==`j'
replace mult = string(`mult', "%9.1f") if _n==`j'

foreach v of varlist mad newen regcoef {
	replace `v' = "-" if _n<=5
}

save t5_p1.dta, replace 

















 
/*----------------------------------------------------*/
   /* [>   Next: Endogenous Network Recomposition   <] */ 
/*----------------------------------------------------*/


global MC_draws = 100
scalar MC_draws = 100 // 1000

clear all
cap log close 
******
* SET PARAMETERS
******

* Seed and number of MC draws
set seed 24081972

* Choose Baseline Specification of the structural equation
* col 4 of table 1
global controlsFE  "govern_* foreign_* unpopular_*          D96_* D30_* D41_* D471_*"
global IVBaseline "rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0 rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 rain_enemies_enemies0 sqrain_enemies_enemies0 rain_enemies_of_allies0 sqrain_enemies_of_allies0 rain_enemies_enemies1 sqrain_enemies_enemies1 rain_enemies_of_allies1 sqrain_enemies_of_allies1 rain_neutral0 sqrain_neutral0 rain_neutral1 sqrain_neutral1"
global baseline_specification "  xtivreg TotFight (TotFight_Enemy TotFight_Allied  TotFight_Neutral =  $IVBaseline)  meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1 $controlsFE, fe i(group) "

use KRTZ_dyadic_ref.dta, clear
drop if group==group_d
bysort group group_d: keep if [_n]==1 
rename allied aplus
keep group group_d aplus
save aplus_ref.dta, replace
use KRTZ_dyadic_ref.dta, clear
drop if group==group_d
bysort group group_d: keep if [_n]==1 
rename enemy aminus
keep group group_d aminus
save aminus_ref.dta, replace
use KRTZ_dyadic_ref.dta, clear
drop if group==group_d
bysort group group_d: keep if [_n]==1 
keep group group_d id id_d name name_d
save acled_KRTZ_identifiers.dta, replace
use KRTZ_monadic_ref.dta, clear
save temp_counterfactual, replace
use temp_counterfactual, clear
$baseline_specification
scalar beta=- _b[TotFight_Allied]
scalar gama= _b[TotFight_Enemy]

* Select the multinomial logit
* Caution 1 - we take "a=0" as the alternative of reference 
* Caution 2 - We must remove fixed effects of groups with low degree_minus and degree_plus to avoid perfect predictors - VISUAL INSPECTION HERE !!
use temp_counterfactual, clear
bys group: keep if [_n]==1
tab degree_minus
tab degree_plus
table degree_minus degree_plus
gen FixEff= (degree_minus > 0) & (degree_plus>0)
tab FixEff
list group  if FixEff==1
global FixedEffects "Dgroup1 Dgroup2 Dgroup3 Dgroup4 Dgroup5 Dgroup6 Dgroup7 Dgroup8 Dgroup9 Dgroup10 Dgroup11 Dgroup12 Dgroup13 Dgroup14 Dgroup15 Dgroup16 Dgroup17 Dgroup18 Dgroup19 Dgroup20 Dgroup21 Dgroup22 Dgroup23 Dgroup24 Dgroup25 Dgroup26 Dgroup27 Dgroup29 Dgroup30 Dgroup31 Dgroup33 Dgroup34 Dgroup35 Dgroup37 Dgroup38 Dgroup40 Dgroup41 Dgroup42 Dgroup44 Dgroup56 Dgroup61 Dgroup62 Dgroup63 Dgroup69 Dgroup72 Dgroup73"
global network_cov "common_allied common_enemy common_all_en"
global struc_cov "geodist_dyad same_ethnic_greg same_Hutu_Tutsi different_Hutu_Tutsi  zero_Gov zero_For"
global baseline_logit "asclogit link  csf_surplus ,  case(dyad) alternatives(alternative) casevars($network_cov $struc_cov  $FixedEffects)  basealternative(0)  diff  technique(bfgs)" 



******
* TABLE 5 (bottom panel) FIGURE 4 TABLES B.13 B.14
******

* set conditional draws
global unobs1 "qui replace success=1 if success==0 & (a==-1) & (V_enemity+epsilon_enemity>V_allied+epsilon_allied)& (V_enemity+epsilon_enemity>V_neutral+epsilon_neutral)"
global unobs2 "qui replace success=1 if success==0 & (a==0) & (V_neutral+epsilon_neutral>V_enemity+epsilon_enemity)& (V_neutral+epsilon_neutral>V_allied+epsilon_allied)"
global unobs3 "qui replace success=1 if success==0 & (a==1) & (V_allied+epsilon_allied>V_enemity+epsilon_enemity)& (V_allied+epsilon_allied>V_neutral+epsilon_neutral)"
global exo_endo ""
global remove_violation ""
* Generate the counterfactual networks: Estimation of the multinomial logit and DGP matrix of unobserved MC utility draw
do ../progs/endo_network_Estimation.do

qui use temp_counterfactual, clear
qui sum group
qui scalar nb_group=r(max)


/* [> Table 5 Bottom <] */ 

***
*** Policy 1: drop foreign groups
***
use t5_p1.dta, replace 
tempfile x 
save `x', replace

import excel using ../original_data/coding_characteristics_FZ.xls, clear first
keep Group FOREIGNGROUPS
rename Group name 
rename FOREIGNGROUPS Foreign_FZ
sort name 
save temp_fz, replace
qui use avgbench_data, clear
sort name
merge name using temp_fz
tab _merge
drop if _merge==2
count
drop _merge
erase temp_fz.dta
table Foreign Foreign_FZ
replace Foreign=Foreign_FZ
qui sort MCref
qui drop if Foreign ==1
qui sort MCref year
qui save temp_MC, replace
keep MCref
sort MCref
save trash.dta, replace
rename MCref MCref_d
sort MCref_d
save trash_d.dta, replace
qui use bench_aminus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d 
qui save temp_aminus, replace
qui use bench_aplus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d
qui save temp_aplus, replace


*
* w/o recomposition of the network
*

qui do ../progs/eq_simul.do
* CAUTION !! THE MCref change after this subroutine. Do Not USE THEM FOR THE PURPOSE OF MERGING
qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
label var Delta_RD  "count. change in aggregate fight."
save exo_KPresult_foreign, replace

*
* with endogenous recomposition of the network
*

* Generate counterfactual covariates
qui do ../progs/compute_network_related_covariates.do
expand 3
bysort MCref MCref_d: gen alternative=[_n]-2 
keep MCref MCref_d alternative $network_cov
foreach var in $network_cov {
gen counter_`var'=`var'
drop  `var'
}
* must be triangular
drop if MCref<MCref_d
sort MCref MCref_d alternative
egen dyad=group(MCref MCref_d)
drop if MCref==MCref_d

sort MCref MCref_d alternative
save temp_counter.dta, replace

* counterfactual observed utilities and probabilities 
* redo the logit estimation to be sure that the predicted values are stored  
use temp_baseline, clear
$baseline_logit
* retrieve the counterfactual covariates and merge them with other covariates
sort MCref MCref_d alternative
merge MCref MCref_d alternative using temp_counter
tab _merge
keep if _merge==3
gen counter_csf_surplus=csf_surplus
drop _merge
foreach var in csf_surplus $network_cov {
rename `var' baseline_`var'
gen `var'= counter_`var'

}
predict counter_proba
predict counter_Xbeta, xb
gen exp_counter_Xbeta=exp(counter_Xbeta)
cap drop test counter_inclusive
bysort MCref MCref_d: egen counter_inclusive=sum(exp_counter_Xbeta)
rename link base_link
order MCref MCref_d alternative base* counter* 
keep MCref MCref_d alternative counter_Xbeta counter_proba counter_inclusive base_link base_proba base_Xbeta base_inclusive 
sort MCref MCref_d alternative 
save counter_proba.dta, replace
  
* Generate MC links and network
use MC_draws, clear
sort MCref MCref_d alternative 
merge MCref MCref_d alternative using counter_proba
tab _merge
keep if _merge==3
drop _merge
gen counter_utility=counter_Xbeta+epsilon
sort mcdraw MCref MCref_d alternative
bysort mcdraw MCref MCref_d: egen maxU=max(counter_utility)
gen predicted_link= (counter_utility==maxU)
* we store those values for building future statistics on rewiring (see below)
save endo_network_rewiring.dta, replace

* we now build the counterfactual rewired networks
keep if predicted_link==1
rename alternative a
keep MCref MCref_d mcdraw a
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace
* finally we need to "square" the matrix with mirrors and and then fill the diagonal
use MC_counter_network, clear
rename MCref stor
rename MCref_d MCref
rename stor MCref_d
append using MC_counter_network
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace

bysort mcdraw MCref: keep if [_n]==1 
replace MCref_d=MCref
replace a=0
append using MC_counter_network
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace

gen cond= beta * (a==1) - gama * (a==-1) 
collapse (sum) cond , by (mcdraw MCref) 
replace cond = (1+cond<0)
collapse (sum) cond, by(mcdraw)
rename mcdraw mc_draw
sort mc_draw
save interior_condition_foreign.dta, replace


* MC draws
clear
set obs 1
gen mc_draw=.
save endo_KPresult_foreign, replace

foreach d of numlist  1(1) 1000 {
if `d'<${MC_draws}+1{
di `d'
use MC_counter_network, clear
keep if mcdraw==`d'
gen aminus= (a==-1)
keep MCref MCref_d aminus 
sort MCref MCref_d
save temp_aminus, replace

use MC_counter_network, clear
keep if mcdraw==`d'
gen aplus= (a==1)
keep MCref MCref_d aplus
sort MCref MCref_d
save temp_aplus, replace


qui do ../progs/eq_simul.do

* CAUTION !! THE MCref change after this subroutine. Do Not USE THEM FOR THE PURPOSE OF MERGING
qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_endo_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_endo_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
save Foreign_endo_KPresult, replace
label var Delta_RD  "count. change in aggregate fight."
gen mc_draw=`d'
append using endo_KPresult_foreign
save endo_KPresult_foreign, replace

}
}



use endo_KPresult_foreign, clear
sort mc_draw
save endo_KPresult_foreign, replace

* Retrieve basic statistics on rewiring
use endo_network_rewiring, clear
keep MCref MCref_d alternative base_link predicted_link mcdraw
gen base_aminus=1 if base_link==1 & alternative==-1
gen endo_aminus=1 if predicted_link==1 & alternative==-1
gen base_aplus=1 if base_link==1 & alternative==1
gen endo_aplus=1 if predicted_link==1 & alternative==1
collapse (sum) base_link predicted_link base_aminus endo_aminus base_aplus endo_aplus, by(mcdraw)
gen new_enmities=(endo_aminus - base_aminus) 
gen new_alliances=(endo_aplus - base_aplus) 
keep mcdraw new_enmities new_alliances
rename mcdraw mc_draw
 
sort mc_draw
merge mc_draw using endo_KPresult_foreign
tab _merge
drop _merge
append using exo_KPresult_foreign
drop if mc_draw==. & bench_RD==.
replace mc_draw=-1000 if mc_draw==.
sort mc_draw
replace mc_draw=. if mc_draw==-1000
gen policy="EXO netw." if mc_draw==.
replace policy="ENDO netw." if mc_draw!=.
label var new_enmities "avg rewiring: New enmities (pct)" 
label var new_alliances "avg rewiring: New alliances (pct)"
save endo_KPresult_foreign, replace


replace mc_draw=-1000 if mc_draw==.
sort mc_draw
merge mc_draw using interior_condition_foreign.dta
tab _merge
drop _merge
rename cond condition_violation
label var condition_violation "Is interior condition violated? 0/1 "
replace mc_draw=. if mc_draw==-1000
save endo_KPresult_foreign, replace


sum Delta_RD, d 
local d = r(p50)
gen mads = abs(Delta_RD-r(p50))
sum mads , d 
local mad = r(p50)
sum new_enmities, d 
local ne=r(p50)
sum new_alliances, d 
local na = r(p50)

reg Delta_RD new_enmities new_alliances 
local ce = _b[new_enmities]
local ca = _b[new_alliances]


use `x', replace 
local j=6
destring share, gen(ss)
gen m1 = -`d'/ss if _n==`j'
// replace m1=string(m1)
replace mult = string(m1, "%9.1f") if _n==`j'
replace rd = string(-`d', "%9.3f") if _n==`j'
replace mad = string(`mad', "%9.3f") if _n==`j'
replace mad = string(`mad', "%9.3f") if _n==`j'
replace newen = "["+string(`ne', "%9.0f")+", "+string(`na', "%9.0f")+"]" if _n==`j'
replace regcoef = "["+string(`ce', "%9.3f")+", "+string(`ca', "%9.3f")+"]" if _n==`j'

drop m1 ss 
save `x', replace 


***
*** large groups
***

qui use avgbench_data, clear
qui sort MCref
sum degree_minus, d
drop if degree_minus >4
qui sort MCref year
qui save temp_MC, replace
keep MCref
sort MCref
save trash.dta, replace
rename MCref MCref_d
sort MCref_d
save trash_d.dta, replace

qui use bench_aminus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d 
qui save temp_aminus, replace

qui use bench_aplus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d
qui save temp_aplus, replace



*
* w/o recomposition of the network
*

qui do ../progs/eq_simul.do
* CAUTION !! THE MCref change after this subroutine. Do Not USE THEM FOR THE PURPOSE OF MERGING
qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
label var Delta_RD  "count. change in aggregate fight."
save exo_KPresult_Large_Groups, replace


*
* with endogenous recomposition of the network
*

* Generate counterfactual covariates 
qui do ../progs/compute_network_related_covariates.do
expand 3
bysort MCref MCref_d: gen alternative=[_n]-2 
keep MCref MCref_d alternative $network_cov
foreach var in $network_cov {
gen counter_`var'=`var'
drop  `var'
}
drop if MCref<MCref_d
sort MCref MCref_d alternative
egen dyad=group(MCref MCref_d)
drop if MCref==MCref_d
sort MCref MCref_d alternative
save temp_counter.dta, replace
use temp_baseline, clear
$baseline_logit
sort MCref MCref_d alternative
merge MCref MCref_d alternative using temp_counter
tab _merge
keep if _merge==3
gen counter_csf_surplus=csf_surplus
drop _merge
foreach var in csf_surplus $network_cov {
rename `var' baseline_`var'
gen `var'= counter_`var'

}
predict counter_proba
predict counter_Xbeta, xb
gen exp_counter_Xbeta=exp(counter_Xbeta)
cap drop test counter_inclusive
bysort MCref MCref_d: egen counter_inclusive=sum(exp_counter_Xbeta)
rename link base_link
order MCref MCref_d alternative base* counter* 
keep MCref MCref_d alternative counter_Xbeta counter_proba counter_inclusive base_link base_proba base_Xbeta base_inclusive 
sort MCref MCref_d alternative 
save counter_proba.dta, replace
  
use MC_draws, clear
sort MCref MCref_d alternative 
merge MCref MCref_d alternative using counter_proba
tab _merge
keep if _merge==3
drop _merge
gen counter_utility=counter_Xbeta+epsilon
sort mcdraw MCref MCref_d alternative
bysort mcdraw MCref MCref_d: egen maxU=max(counter_utility)
gen predicted_link= (counter_utility==maxU)
save endo_network_rewiring.dta, replace

keep if predicted_link==1
rename alternative a
keep MCref MCref_d mcdraw a
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace
use MC_counter_network, clear
rename MCref stor
rename MCref_d MCref
rename stor MCref_d
append using MC_counter_network
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace

bysort mcdraw MCref: keep if [_n]==1 
replace MCref_d=MCref
replace a=0
append using MC_counter_network
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace

gen cond= beta * (a==1) - gama * (a==-1) 
collapse (sum) cond , by (mcdraw MCref) 
replace cond = (1+cond<0)
collapse (sum) cond, by(mcdraw)
rename mcdraw mc_draw
sort mc_draw
save interior_condition_foreign.dta, replace

clear
set obs 1
gen mc_draw=.
save endo_KPresult_Large_Groups, replace

foreach d of numlist  1(1) 1000 {
if `d'<${MC_draws}+1{
di `d'
use MC_counter_network, clear
keep if mcdraw==`d'
gen aminus= (a==-1)
keep MCref MCref_d aminus 
sort MCref MCref_d
save temp_aminus, replace

use MC_counter_network, clear
keep if mcdraw==`d'
gen aplus= (a==1)
keep MCref MCref_d aplus
sort MCref MCref_d
save temp_aplus, replace


qui do ../progs/eq_simul.do

qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_endo_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_endo_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
save Foreign_endo_KPresult, replace
label var Delta_RD  "count. change in aggregate fight."
gen mc_draw=`d'
append using endo_KPresult_Large_Groups
save endo_KPresult_Large_Groups, replace


}
}



use endo_KPresult_Large_Groups, clear
sort mc_draw
save endo_KPresult_Large_Groups, replace
use endo_network_rewiring, clear
keep MCref MCref_d alternative base_link predicted_link mcdraw
gen base_aminus=1 if base_link==1 & alternative==-1
gen endo_aminus=1 if predicted_link==1 & alternative==-1
gen base_aplus=1 if base_link==1 & alternative==1
gen endo_aplus=1 if predicted_link==1 & alternative==1
collapse (sum) base_link predicted_link base_aminus endo_aminus base_aplus endo_aplus, by(mcdraw)
gen new_enmities=(endo_aminus - base_aminus) 
gen new_alliances=(endo_aplus - base_aplus) 
keep mcdraw new_enmities new_alliances
rename mcdraw mc_draw
 
sort mc_draw
merge mc_draw using endo_KPresult_Large_Groups
tab _merge
drop _merge
append using exo_KPresult_Large_Groups
drop if mc_draw==. & bench_RD==.
replace mc_draw=-1000 if mc_draw==.
sort mc_draw
replace mc_draw=. if mc_draw==-1000
gen policy="EXO netw." if mc_draw==.
replace policy="ENDO netw." if mc_draw!=.
label var new_enmities "avg rewiring: New enmities (pct)" 
label var new_alliances "avg rewiring: New alliances (pct)"
save endo_KPresult__Large_Groups, replace


replace mc_draw=-1000 if mc_draw==.
sort mc_draw
merge mc_draw using interior_condition_foreign.dta
tab _merge
drop _merge
rename cond condition_violation
label var condition_violation "Is interior condition violated? 0/1 "
replace mc_draw=. if mc_draw==-1000
save endo_KPresult_Large_Groups, replace


sum Delta_RD, d 
local d = r(p50)
gen mads = abs(Delta_RD-r(p50))
sum mads , d 
local mad = r(p50)
sum new_enmities, d 
local ne=r(p50)
sum new_alliances, d 
local na = r(p50)


reg Delta_RD new_enmities new_alliances 
local ce = _b[new_enmities]
local ca = _b[new_alliances]


use `x', replace 
local j=10
destring share, gen(ss)
gen m1 = -`d'/ss if _n==`j'
// replace m1=string(m1)
replace mult = string(m1, "%9.1f") if _n==`j'
replace rd = string(-`d', "%9.3f") if _n==`j'
replace mad = string(`mad', "%9.3f") if _n==`j'
replace mad = string(`mad', "%9.3f") if _n==`j'
replace newen = "["+string(`ne', "%9.0f")+", "+string(`na', "%9.0f")+"]" if _n==`j'
replace regcoef = "["+string(`ce', "%9.3f")+", "+string(`ca', "%9.3f")+"]" if _n==`j'

drop m1 ss 
save `x', replace 



***
*** ITURI-related groups
***


import excel using ../original_data/coding_characteristics_FZ.xls, clear first
keep Group ITURI
rename Group name 
destring ITURI, gen(Ituri)
sort name 
save temp_fz, replace
qui use avgbench_data, clear
sort name
merge name using temp_fz
tab _merge
drop if _merge==2
count
drop _merge
erase temp_fz.dta


qui sort MCref
qui drop if Ituri ==1
qui sort MCref year
qui save temp_MC, replace

keep MCref
sort MCref
save trash.dta, replace
rename MCref MCref_d
sort MCref_d
save trash_d.dta, replace

qui use bench_aminus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d 
qui save temp_aminus, replace

qui use bench_aplus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d
qui save temp_aplus, replace


*
* w/o recomposition of the network
*

qui do ../progs/eq_simul.do
qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
label var Delta_RD  "count. change in aggregate fight."
save exo_KPresult_Ituri, replace

*
* with endogenous recomposition of the network
*

qui do ../progs/compute_network_related_covariates.do
expand 3
bysort MCref MCref_d: gen alternative=[_n]-2 
keep MCref MCref_d alternative $network_cov
foreach var in $network_cov {
gen counter_`var'=`var'
drop  `var'
}
drop if MCref<MCref_d
sort MCref MCref_d alternative
egen dyad=group(MCref MCref_d)
drop if MCref==MCref_d

sort MCref MCref_d alternative
save temp_counter.dta, replace

use temp_baseline, clear
$baseline_logit
sort MCref MCref_d alternative
merge MCref MCref_d alternative using temp_counter
tab _merge
keep if _merge==3
gen counter_csf_surplus=csf_surplus
drop _merge
foreach var in csf_surplus $network_cov {
rename `var' baseline_`var'
gen `var'= counter_`var'

}
predict counter_proba
predict counter_Xbeta, xb
gen exp_counter_Xbeta=exp(counter_Xbeta)
cap drop test counter_inclusive
bysort MCref MCref_d: egen counter_inclusive=sum(exp_counter_Xbeta)
rename link base_link
order MCref MCref_d alternative base* counter* 
keep MCref MCref_d alternative counter_Xbeta counter_proba counter_inclusive base_link base_proba base_Xbeta base_inclusive 
sort MCref MCref_d alternative 
save counter_proba.dta, replace
  
use MC_draws, clear
sort MCref MCref_d alternative 
merge MCref MCref_d alternative using counter_proba
tab _merge
keep if _merge==3
drop _merge
gen counter_utility=counter_Xbeta+epsilon
sort mcdraw MCref MCref_d alternative
bysort mcdraw MCref MCref_d: egen maxU=max(counter_utility)
gen predicted_link= (counter_utility==maxU)
save endo_network_rewiring.dta, replace

keep if predicted_link==1
rename alternative a
keep MCref MCref_d mcdraw a
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace
use MC_counter_network, clear
rename MCref stor
rename MCref_d MCref
rename stor MCref_d
append using MC_counter_network
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace

bysort mcdraw MCref: keep if [_n]==1 
replace MCref_d=MCref
replace a=0
append using MC_counter_network
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace

gen cond= beta * (a==1) - gama * (a==-1) 
collapse (sum) cond , by (mcdraw MCref) 
replace cond = (1+cond<0)
collapse (sum) cond, by(mcdraw)
rename mcdraw mc_draw
sort mc_draw
save interior_condition_foreign.dta, replace


clear
set obs 1
gen mc_draw=.
save endo_KPresult_Ituri, replace

foreach d of numlist  1(1) 1000 {
if `d'<${MC_draws}+1{
di `d'
use MC_counter_network, clear
keep if mcdraw==`d'
gen aminus= (a==-1)
keep MCref MCref_d aminus 
sort MCref MCref_d
save temp_aminus, replace

use MC_counter_network, clear
keep if mcdraw==`d'
gen aplus= (a==1)
keep MCref MCref_d aplus
sort MCref MCref_d
save temp_aplus, replace


qui do ../progs/eq_simul.do

qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_endo_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_endo_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
save Foreign_endo_KPresult, replace
label var Delta_RD  "count. change in aggregate fight."
gen mc_draw=`d'
append using endo_KPresult_Ituri
save endo_KPresult_Ituri, replace

}
}



use endo_KPresult_Ituri, clear
sort mc_draw
save endo_KPresult_Ituri, replace

use endo_network_rewiring, clear
keep MCref MCref_d alternative base_link predicted_link mcdraw
gen base_aminus=1 if base_link==1 & alternative==-1
gen endo_aminus=1 if predicted_link==1 & alternative==-1
gen base_aplus=1 if base_link==1 & alternative==1
gen endo_aplus=1 if predicted_link==1 & alternative==1
collapse (sum) base_link predicted_link base_aminus endo_aminus base_aplus endo_aplus, by(mcdraw)
gen new_enmities=(endo_aminus - base_aminus) 
gen new_alliances=(endo_aplus - base_aplus) 
keep mcdraw new_enmities new_alliances
rename mcdraw mc_draw
 
sort mc_draw
merge mc_draw using endo_KPresult_Ituri
tab _merge
drop _merge
append using exo_KPresult_Ituri
drop if mc_draw==. & bench_RD==.
replace mc_draw=-1000 if mc_draw==.
sort mc_draw
replace mc_draw=. if mc_draw==-1000
gen policy="EXO netw." if mc_draw==.
replace policy="ENDO netw." if mc_draw!=.
label var new_enmities "avg rewiring: New enmities (pct)" 
label var new_alliances "avg rewiring: New alliances (pct)"
save endo_KPresult_Ituri, replace


replace mc_draw=-1000 if mc_draw==.
sort mc_draw
merge mc_draw using interior_condition_foreign.dta
tab _merge
drop _merge
rename cond condition_violation
label var condition_violation "Is interior condition violated? 0/1 "
replace mc_draw=. if mc_draw==-1000
save endo_KPresult_Ituri, replace


sum Delta_RD, d 
local d = r(p50)
gen mads = abs(Delta_RD-r(p50))
sum mads , d 
local mad = r(p50)
sum new_enmities, d 
local ne=r(p50)
sum new_alliances, d 
local na = r(p50)

reg Delta_RD new_enmities new_alliances 
local ce = _b[new_enmities]
local ca = _b[new_alliances]


use `x', replace 
local j=7
destring share, gen(ss)
gen m1 = -`d'/ss if _n==`j'
// replace m1=string(m1)
replace mult = string(m1, "%9.1f") if _n==`j'
replace rd = string(-`d', "%9.3f") if _n==`j'
replace mad = string(`mad', "%9.3f") if _n==`j'
replace mad = string(`mad', "%9.3f") if _n==`j'
replace newen = "["+string(`ne', "%9.0f")+", "+string(`na', "%9.0f")+"]" if _n==`j'
replace regcoef = "["+string(`ce', "%9.3f")+", "+string(`ca', "%9.3f")+"]" if _n==`j'

drop m1 ss 
save `x', replace 



***
*** RCD&UG&RWA groups
***

import excel using ../original_data/coding_characteristics_FZ.xls, clear first
keep Group RCDUGRWA
rename Group name 
destring RCDUGRWA, gen(remove)
sort name 
save temp_fz, replace
qui use avgbench_data, clear
sort name
merge name using temp_fz
tab _merge
drop if _merge==2
count
drop _merge
erase temp_fz.dta

qui sort MCref
qui drop if remove ==1
qui sort MCref year
qui save temp_MC, replace

keep MCref
sort MCref
save trash.dta, replace
rename MCref MCref_d
sort MCref_d
save trash_d.dta, replace

qui use bench_aminus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d 
qui save temp_aminus, replace

qui use bench_aplus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d
qui save temp_aplus, replace


*
* w/o recomposition of the network
*

qui do ../progs/eq_simul.do
qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
label var Delta_RD  "count. change in aggregate fight."
save exo_KPresult_RCD_UG_RWA, replace

*
* with endogenous recomposition of the network
*

qui do ../progs/compute_network_related_covariates.do
expand 3
bysort MCref MCref_d: gen alternative=[_n]-2 
keep MCref MCref_d alternative $network_cov
foreach var in $network_cov {
gen counter_`var'=`var'
drop  `var'
}
drop if MCref<MCref_d
sort MCref MCref_d alternative
egen dyad=group(MCref MCref_d)
drop if MCref==MCref_d

sort MCref MCref_d alternative
save temp_counter.dta, replace

use temp_baseline, clear
$baseline_logit
sort MCref MCref_d alternative
merge MCref MCref_d alternative using temp_counter
tab _merge
keep if _merge==3
gen counter_csf_surplus=csf_surplus
drop _merge
foreach var in csf_surplus $network_cov {
rename `var' baseline_`var'
gen `var'= counter_`var'

}
predict counter_proba
predict counter_Xbeta, xb
gen exp_counter_Xbeta=exp(counter_Xbeta)
cap drop test counter_inclusive
bysort MCref MCref_d: egen counter_inclusive=sum(exp_counter_Xbeta)
rename link base_link
order MCref MCref_d alternative base* counter* 
keep MCref MCref_d alternative counter_Xbeta counter_proba counter_inclusive base_link base_proba base_Xbeta base_inclusive 
sort MCref MCref_d alternative 
save counter_proba.dta, replace
  
use MC_draws, clear
sort MCref MCref_d alternative 
merge MCref MCref_d alternative using counter_proba
tab _merge
keep if _merge==3
drop _merge
gen counter_utility=counter_Xbeta+epsilon
sort mcdraw MCref MCref_d alternative
bysort mcdraw MCref MCref_d: egen maxU=max(counter_utility)
gen predicted_link= (counter_utility==maxU)
save endo_network_rewiring.dta, replace

keep if predicted_link==1
rename alternative a
keep MCref MCref_d mcdraw a
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace
use MC_counter_network, clear
rename MCref stor
rename MCref_d MCref
rename stor MCref_d
append using MC_counter_network
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace

bysort mcdraw MCref: keep if [_n]==1 
replace MCref_d=MCref
replace a=0
append using MC_counter_network
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace

gen cond= beta * (a==1) - gama * (a==-1) 
collapse (sum) cond , by (mcdraw MCref) 
replace cond = (1+cond<0)
collapse (sum) cond, by(mcdraw)
rename mcdraw mc_draw
sort mc_draw
save interior_condition_foreign.dta, replace

clear
set obs 1
gen mc_draw=.
save endo_KPresult_RCD_UG_RWA, replace

foreach d of numlist  1(1) 1000 {
if `d'<${MC_draws}+1{
di `d'
use MC_counter_network, clear
keep if mcdraw==`d'
gen aminus= (a==-1)
keep MCref MCref_d aminus 
sort MCref MCref_d
save temp_aminus, replace

use MC_counter_network, clear
keep if mcdraw==`d'
gen aplus= (a==1)
keep MCref MCref_d aplus
sort MCref MCref_d
save temp_aplus, replace


qui do ../progs/eq_simul.do

qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_endo_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_endo_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
save Foreign_endo_KPresult, replace
label var Delta_RD  "count. change in aggregate fight."
gen mc_draw=`d'
append using endo_KPresult_RCD_UG_RWA
save endo_KPresult_RCD_UG_RWA, replace

}
}



use endo_KPresult_RCD_UG_RWA, clear
sort mc_draw
save endo_KPresult_RCD_UG_RWA, replace

use endo_network_rewiring, clear
keep MCref MCref_d alternative base_link predicted_link mcdraw
gen base_aminus=1 if base_link==1 & alternative==-1
gen endo_aminus=1 if predicted_link==1 & alternative==-1
gen base_aplus=1 if base_link==1 & alternative==1
gen endo_aplus=1 if predicted_link==1 & alternative==1
collapse (sum) base_link predicted_link base_aminus endo_aminus base_aplus endo_aplus, by(mcdraw)
gen new_enmities=(endo_aminus - base_aminus) 
gen new_alliances=(endo_aplus - base_aplus) 
keep mcdraw new_enmities new_alliances
rename mcdraw mc_draw
 
sort mc_draw
merge mc_draw using endo_KPresult_RCD_UG_RWA
tab _merge
drop _merge
append using exo_KPresult_RCD_UG_RWA
drop if mc_draw==. & bench_RD==.
replace mc_draw=-1000 if mc_draw==.
sort mc_draw
replace mc_draw=. if mc_draw==-1000
gen policy="EXO netw." if mc_draw==.
replace policy="ENDO netw." if mc_draw!=.
label var new_enmities "avg rewiring: New enmities (pct)" 
label var new_alliances "avg rewiring: New alliances (pct)"
save endo_KPresult_RCD_UG_RWA, replace


replace mc_draw=-1000 if mc_draw==.
sort mc_draw
merge mc_draw using interior_condition_foreign.dta
tab _merge
drop _merge
rename cond condition_violation
label var condition_violation "Is interior condition violated? 0/1 "
replace mc_draw=. if mc_draw==-1000
save endo_KPresult_RCD_UG_RWA, replace



sum Delta_RD, d 
local d = r(p50)
gen mads = abs(Delta_RD-r(p50))
sum mads , d 
local mad = r(p50)
sum new_enmities, d 
local ne=r(p50)
sum new_alliances, d 
local na = r(p50)

reg Delta_RD new_enmities new_alliances 
local ce = _b[new_enmities]
local ca = _b[new_alliances]


use `x', replace 
local j=9
destring share, gen(ss)
gen m1 = -`d'/ss if _n==`j'
// replace m1=string(m1)
replace mult = string(m1, "%9.1f") if _n==`j'
replace rd = string(-`d', "%9.3f") if _n==`j'
replace mad = string(`mad', "%9.3f") if _n==`j'
replace mad = string(`mad', "%9.3f") if _n==`j'
replace newen = "["+string(`ne', "%9.0f")+", "+string(`na', "%9.0f")+"]" if _n==`j'
replace regcoef = "["+string(`ce', "%9.3f")+", "+string(`ca', "%9.3f")+"]" if _n==`j'

drop m1 ss 
save `x', replace 





***
*** FDLR&INTER&HUTU groups
***

import excel using ../original_data/coding_characteristics_FZ.xls, clear first
keep Group FDLRINTERHUTU
rename Group name 
rename FDLRINTERHUTU remove 
sort name 
save temp_fz, replace
qui use avgbench_data, clear
sort name
merge name using temp_fz
tab _merge
drop if _merge==2
count
drop _merge
erase temp_fz.dta


qui sort MCref
qui drop if remove ==1
qui sort MCref year
qui save temp_MC, replace

keep MCref
sort MCref
save trash.dta, replace
rename MCref MCref_d
sort MCref_d
save trash_d.dta, replace

qui use bench_aminus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d 
qui save temp_aminus, replace

qui use bench_aplus, clear
sort MCref
merge MCref using trash
tab _merge
keep if _merge==3
drop _merge
sort MCref_d
merge MCref_d using trash_d
tab _merge
keep if _merge==3
drop _merge
qui sort MCref MCref_d
qui save temp_aplus, replace


*
* w/o recomposition of the network
*

qui do ../progs/eq_simul.do
qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
label var Delta_RD  "count. change in aggregate fight."
save exo_KPresult_FDLR_INTER_HUTU, replace

*
* with endogenous recomposition of the network
*

qui do ../progs/compute_network_related_covariates.do
expand 3
bysort MCref MCref_d: gen alternative=[_n]-2 
keep MCref MCref_d alternative $network_cov
foreach var in $network_cov {
gen counter_`var'=`var'
drop  `var'
}
drop if MCref<MCref_d
sort MCref MCref_d alternative
egen dyad=group(MCref MCref_d)
drop if MCref==MCref_d

sort MCref MCref_d alternative
save temp_counter.dta, replace

use temp_baseline, clear
$baseline_logit
sort MCref MCref_d alternative
merge MCref MCref_d alternative using temp_counter
tab _merge
keep if _merge==3
gen counter_csf_surplus=csf_surplus
drop _merge
foreach var in csf_surplus $network_cov {
rename `var' baseline_`var'
gen `var'= counter_`var'

}
predict counter_proba
predict counter_Xbeta, xb
gen exp_counter_Xbeta=exp(counter_Xbeta)
cap drop test counter_inclusive
bysort MCref MCref_d: egen counter_inclusive=sum(exp_counter_Xbeta)
rename link base_link
order MCref MCref_d alternative base* counter* 
keep MCref MCref_d alternative counter_Xbeta counter_proba counter_inclusive base_link base_proba base_Xbeta base_inclusive 
sort MCref MCref_d alternative 
save counter_proba.dta, replace
  
use MC_draws, clear
sort MCref MCref_d alternative 
merge MCref MCref_d alternative using counter_proba
tab _merge
keep if _merge==3
drop _merge
gen counter_utility=counter_Xbeta+epsilon
sort mcdraw MCref MCref_d alternative
bysort mcdraw MCref MCref_d: egen maxU=max(counter_utility)
gen predicted_link= (counter_utility==maxU)
save endo_network_rewiring.dta, replace

keep if predicted_link==1
rename alternative a
keep MCref MCref_d mcdraw a
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace
use MC_counter_network, clear
rename MCref stor
rename MCref_d MCref
rename stor MCref_d
append using MC_counter_network
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace

bysort mcdraw MCref: keep if [_n]==1 
replace MCref_d=MCref
replace a=0
append using MC_counter_network
sort mcdraw MCref MCref_d
save MC_counter_network.dta, replace

gen cond= beta * (a==1) - gama * (a==-1) 
collapse (sum) cond , by (mcdraw MCref) 
replace cond = (1+cond<0)
collapse (sum) cond, by(mcdraw)
rename mcdraw mc_draw
sort mc_draw
save interior_condition_foreign.dta, replace

clear
set obs 1
gen mc_draw=.
save endo_KPresult_FDLR_INTER_HUTU, replace

foreach d of numlist  1(1) 1000 {
if `d'<${MC_draws}+1{
di `d'
use MC_counter_network, clear
keep if mcdraw==`d'
gen aminus= (a==-1)
keep MCref MCref_d aminus 
sort MCref MCref_d
save temp_aminus, replace

use MC_counter_network, clear
keep if mcdraw==`d'
gen aplus= (a==1)
keep MCref MCref_d aplus
sort MCref MCref_d
save temp_aplus, replace


qui do ../progs/eq_simul.do

qui use simul, clear
qui collapse (sum) EFFORT
qui rename EFFORT counter_RD
save Foreign_endo_KPresult, replace

use avgbench_data, clear
collapse (sum) TotFight
rename TotFight bench_RD
merge using Foreign_endo_KPresult
drop _merge
gen Delta_RD=((counter_RD /bench_RD)-1)
save Foreign_endo_KPresult, replace
label var Delta_RD  "count. change in aggregate fight."
gen mc_draw=`d'
append using endo_KPresult_FDLR_INTER_HUTU
save endo_KPresult_FDLR_INTER_HUTU, replace


}
}



use endo_KPresult_FDLR_INTER_HUTU, clear
sort mc_draw
save endo_KPresult_FDLR_INTER_HUTU, replace

use endo_network_rewiring, clear
keep MCref MCref_d alternative base_link predicted_link mcdraw
gen base_aminus=1 if base_link==1 & alternative==-1
gen endo_aminus=1 if predicted_link==1 & alternative==-1
gen base_aplus=1 if base_link==1 & alternative==1
gen endo_aplus=1 if predicted_link==1 & alternative==1
collapse (sum) base_link predicted_link base_aminus endo_aminus base_aplus endo_aplus, by(mcdraw)
gen new_enmities=(endo_aminus - base_aminus) 
gen new_alliances=(endo_aplus - base_aplus) 
keep mcdraw new_enmities new_alliances
rename mcdraw mc_draw
 
sort mc_draw
merge mc_draw using endo_KPresult_FDLR_INTER_HUTU
tab _merge
drop _merge
append using exo_KPresult_FDLR_INTER_HUTU
drop if mc_draw==. & bench_RD==.
replace mc_draw=-1000 if mc_draw==.
sort mc_draw
replace mc_draw=. if mc_draw==-1000
gen policy="EXO netw." if mc_draw==.
replace policy="ENDO netw." if mc_draw!=.
label var new_enmities "avg rewiring: New enmities (pct)" 
label var new_alliances "avg rewiring: New alliances (pct)"
save endo_KPresult_FDLR_INTER_HUTU, replace


replace mc_draw=-1000 if mc_draw==.
sort mc_draw
merge mc_draw using interior_condition_foreign.dta
tab _merge
drop _merge
rename cond condition_violation
label var condition_violation "Is interior condition violated? 0/1 "
replace mc_draw=. if mc_draw==-1000


save endo_KPresult_FDLR_INTER_HUTU, replace


sum Delta_RD, d 
local d = r(p50)
gen mads = abs(Delta_RD-r(p50))
sum mads , d 
local mad = r(p50)
sum new_enmities, d 
local ne=r(p50)
sum new_alliances, d 
local na = r(p50)

reg Delta_RD new_enmities new_alliances 
local ce = _b[new_enmities]
local ca = _b[new_alliances]





use `x', replace 
local j=8
destring share, gen(ss)
gen m1 = -`d'/ss if _n==`j'
// replace m1=string(m1)
replace mult = string(m1, "%9.1f") if _n==`j'
replace rd = string(-`d', "%9.3f") if _n==`j'
replace mad = string(`mad', "%9.3f") if _n==`j'
replace mad = string(`mad', "%9.3f") if _n==`j'
replace newen = "["+string(`ne', "%9.0f")+", "+string(`na', "%9.0f")+"]" if _n==`j'
replace regcoef = "["+string(`ce', "%9.3f")+", "+string(`ca', "%9.3f")+"]" if _n==`j'

drop m1 ss
save `x', replace 

save t5_complete.dta, replace 




label var set  "Set of Groups"
label var no "\# Groups"
label var share "Sh. Fight."
label var rd "-\$\Delta \$ RD"
label var mult "Multiplier"
label var mad "MAD"
label var newen "New Enm. \& All. (At the Median)"
label var regcoef "Regression Coefs."


texsave _all ///
using  "../replication_outputs/tables/t5a.tex", replace frag autonumber title("Table 5: Welfare Effects of Removing Selected Subsets of Armed Groups") ///
footnote("The computation of the counterfactual equilibrium is based on the baseline point estimates of column 4 in Table I. For each policy experiment, we display the results with an exogenous network (top panel) and the results with an endogenous network recomposition based on $MC_draws Monte Carlo simulations (bottom panel). For each experiment, we report the set of removed groups (col. 1); the number of removed groups (col. 2); the observed share of total fighting involving this set of groups (col. 3); the counterfactual reduction (or its median in the bottom panel) in rent dissipation associated with their removal (col. 4); a multiplier defined as the ratio of col. 4 over col. 3 (col. 5); the Median Absolute Deviation in reduction in RD (col. 6); the post-recomposition number of new enmities and alliances at the median Monte Carlo draw (col. 7); the OLS coefficients of enmities and alliances of a regression across Monte Carlo draws of post-recomposition reduction in RD on reduction in RD (exogenous network) and the post-recomposition numbers of new enmities and alliances (col. 8).") ///
varlabels nofix hlines(5)

 filefilter "../replication_outputs/tables/t5a.tex" "../replication_outputs/tables/t5b.tex", ///
   replace  from("caption") to("caption*")

 filefilter "../replication_outputs/tables/t5b.tex" "../replication_outputs/tables/t5c.tex", ///
   replace  from("\BSmidrule Foreign Groups") to("\BSaddlinespace \BSmulticolumn{8}{c}{\BStextit{With Endogenous Network Recomposition}} \BS\BS \BSaddlinespace Foreign Groups ")

 filefilter "../replication_outputs/tables/t5c.tex" "../replication_outputs/tables/t5.tex", ///
   replace  from("sep]\nForeign Groups") to("sep]\n \BSmulticolumn{8}{c}{\BStextit{Exogenous Network}} \BS\BS  \BSaddlinespace Foreign Groups ")



erase "../replication_outputs/tables/t5a.tex"
erase "../replication_outputs/tables/t5b.tex"
erase "../replication_outputs/tables/t5c.tex"





