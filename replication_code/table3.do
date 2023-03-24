/*----------------------------------------------------*/
   /* [>   0.  Github integration   <] */ 
/*----------------------------------------------------*/
/* [> Commit and push any important changes to github regularly. <] */ 
/*
cd "${github}"
! git add "${github}/replication_code/table3.do"
! git commit -m "Slightly improved code to replicate table 3 (tempfiles code). Cols 1-3 are complete and understood. Need to understand cols 4-7."
! git push
*/

 /* [> New branch for testing new code <] */
 // git checkout -b name-of-branch

cd "${main}/regressions"
 


 /*----------------------------------------------------*/
    /* [>   Description of file   <] */ 
 /*----------------------------------------------------*/

/*
Column 1: 
        Defines group activity from expert coding (FZ)
        Defines omega in activity window as 0
        Runs standard IV, with neutral groups, and includes activityXgroup dummies

Column 2: 
        Defines group activity from nonzero events (first year, last year)
        Defines omega in activity window as 0
        Runs standard IV, with neutral groups, and includes activityXgroup dummies

Column 3: 
        Defines group activity from nonzero events (first year, last year)
        Defines omega in activity window as 3
        Runs standard IV, with neutral groups, and includes activityXgroup dummies

Column 4: 
        Says this is "unbalanced panel"
        Defines group activity from expert coding (FZ)
        Keeps only active years with omega=0
        Generates degree measures by group AND YEAR
                (Note: In other file, degree is created like this from base file:
                collapse (sum) allied enemy, by (country group year))
                ren allied degreepus, etc 
                Should be time invariant! -- check more in code and also see for network figure
        In this step, the code merges datasets - it uses an old code, I adjusted it
        The estimation relies on the number of enemies and allies at a given point in time - but their fighting effort is unchanged. I will see if it changes
                if we change how TotFightX is being calculated (and make that dependent on the new network). With omega=4, this gives the same thing, with omega=1 and expert coding, correlation is approx. 0.9995
        ### We also don't include bilateral fighting in the calculation of fiighting efforts



Column 5: 
        Says this is "unbalanced panel"
        Defines group activity from nonzero events (first year, last year)
        Defines omega in activity window as 3
        Runs standard IV, with neutral groups, and includes activityXgroup dummies


Column 6: 
        Says this is "unbalanced panel"
        Defines group activity from nonzero events (first year, last year)
        Defines omega in activity window as 3
        Runs standard IV, with neutral groups, and includes activityXgroup dummies


Column 7: 
        Runs Tobit regression (with zero as lower censoring bound for total fighting)
        Includes residuals from XYZ as controls


*/




/*----------------------------------------------------*/
   /* [>   Set locals for analysis   <] */ 
/*----------------------------------------------------*/

local y                 "TotFight" 
local x                 "TotFight_Enemy TotFight_Allied"
local n                 "TotFight_Neutral"
local controls1         "meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1" 
local controls2         "D96_* D30_* D41_* D471_*"
local controls3         "govern_* foreign_* unpopular_*"
local fe                "i.group i.year"
local fe2               "TE* Dgroup*"

local lag_specif_ols    "lag(1000000) dist(150) lagdist(1000000)"
local lag_specif_iv     "lag(1000000) dist(150) lagdist(1000000) partial"

local iv_reduced        "rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1"
local iv_full           "rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0 rain_enemies_enemies0 sqrain_enemies_enemies0 rain_enemies_of_allies0 sqrain_enemies_of_allies0 rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 rain_enemies_enemies1 sqrain_enemies_enemies1 rain_enemies_of_allies1 sqrain_enemies_of_allies1"
local iv_alternative_r  "lag1TotFight_Enemy lag1TotFight_Allied"
local iv_full_neutral   "rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0 rain_enemies_enemies0 sqrain_enemies_enemies0 rain_enemies_of_allies0 sqrain_enemies_of_allies0 rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 rain_enemies_enemies1 sqrain_enemies_enemies1 rain_enemies_of_allies1 sqrain_enemies_of_allies1 rain_neutral0 sqrain_neutral0 rain_neutral1 sqrain_neutral1"

local mys_syntax        "end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) lag(1000000) dist(150) lagdist(1000000)"
local mys_syntax_iv     "end(`x') iv(`iv_reduced') latitude(latitude) longitude(longitude) id(group) time(year) `lag_specif_iv'"
local mys_syntax_iv2    "end(`x') iv(`iv_alternative_r') latitude(latitude) longitude(longitude) id(group) time(year) `lag_specif_iv'"
local mys_syntax_ivfull "end(`x') iv(`iv_full') latitude(latitude) longitude(longitude) id(group) time(year) `lag_specif_iv'"
local mys_syntax_ivneutral "end(`x' `n') iv(`iv_full_neutral') latitude(latitude) longitude(longitude) id(group) time(year) `lag_specif_iv'"


local fet_syntax        "lat(latitude) lon(longitude) panelvar(group) timevar(year) lag(1000000) dist(150)"
local shi_syntax        "lat(latitude) lon(longitude) panelvar(group) timevar(year) lag(1000000) dist(150)"
local ac1_syntax        "latitude(latitude) longitude(longitude) id(group) time(year) spatial lag(1000000) dist(150)"
local ac2_syntax        "latitude(latitude) longitude(longitude) id(group) time(year) spatial lag(1000000) dist(150)  pfe1(group) pfe2(year) dropsingletons"

*globals
global clus "r cl(id)" 

* tempfiles
tempfile temp_active 
tempfile temp_window
tempfile temp_time_varying_network
tempfile temp_active_d 
tempfile temp_tvn_time 


/*----------------------------------------------------*/
   /* [>   Preparation: Generate time-varying data  <] */ 
/*----------------------------------------------------*/

/* [> Import export data for windows of activity <] */ 
import excel using ../original_data/windows_activity_groups_march16_fz.xls, first clear
keep id name group_start group_end militia* dummy*
rename name name_fz
destring group_start group_end militia*, replace
replace group_start=2002 if id==96
replace militia_start= group_start if id==49 & militia_start==.
replace militia_end= group_end if id==49 & militia_end==.
replace group_start= . if id==49 & militia_start!=.
replace group_end= . if id==49 & militia_end!=.
sort id 

save `temp_window', replace 

 
/* [> Merge <] */ 
use KRTZ_monadic_AF.dta, clear
/* [> Drop singletons in controls2 <] */
foreach v of varlist `controls2' {
       drop if `v'==1
        }
sort id
merge m:1 id using `temp_window'
tab _merge
drop if _merge==2
drop _merge

gen start_fz=max(militia_start, group_start)
gen end_fz=min(militia_end, group_end)

/* [> Generate active group dummies with omega=0 <] */ 
gen window_activity_expert=(year > start_fz -1) & (year < end_fz+1)
sort id year
sort group year
gen nonzero=year if TotFight>0
gen active = window_activity_expert

forv num=1/85 {
        cap gen ActiveGroup`num' = active * Dgroup`num'
        }



/*----------------------------------------------------*/
   /* [>   Column 1:  Balanced panel with expert coding windows of activity  <] */ 
/*----------------------------------------------------*/
my_spatial_2sls_jo `y' ActiveGroup* `controls1' `controls3' Dgroup*  , `mys_syntax_ivneutral' 

predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)

est sto t3_c1

*eststo: ivreg2 `y' (`x' `n'=`iv_full_neutral') ActiveGroup*  `controls1' `controls3' i.group, $clus first




****
** (not in table) Balanced panel with dummies " `y'>0 x Dgroup*"
****

use KRTZ_monadic_AF.dta, clear
/* [> Drop singletons in controls2 <] */
foreach v of varlist `controls2' {
       drop if `v'==1
        }
gen nonzero=year if `y'>0
bysort group: egen startyear= min(nonzero)
bysort group: egen endyear= max(nonzero)
gen active = (`y'>0)

/*----------------------------------------------------*/
   /* [>   Column 2:  Balanced panel with dummies " ((year > startyear -1) & (year < endyear+1)) x Dgroup*" <] */ 
/*----------------------------------------------------*/

use KRTZ_monadic_AF.dta, clear
/* [> Drop singletons in controls2 <] */
foreach v of varlist `controls2' {
       drop if `v'==1
        }

/* [> Generate active group dummies with omega=0, but different activity definition <] */ 
gen nonzero=year if `y'>0
bysort group: egen startyear= min(nonzero)
bysort group: egen endyear= max(nonzero)
gen active = $active_window

forv num=1/85 {
        *  display `num'
        cap gen ActiveGroup`num' = active * Dgroup`num'
        }


*regression
my_spatial_2sls_jo `y'  ActiveGroup*  `controls1' `controls3' Dgroup*  if active==1 , `mys_syntax_ivneutral' 
*eststo: ivreg2 `y' (`x' `n'=`iv_full_neutral') ActiveGroup*  `controls1' `controls3' i.group, $clus first
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)
est sto t3_c2

 





/*----------------------------------------------------*/
   /* [>   Column 3:  Balanced panel with dummies " ((year > startyear -4) & (year < endyear+4)) x Dgroup*" */ 
/*----------------------------------------------------*/

use KRTZ_monadic_AF.dta, clear
/* [> Drop singletons in controls2 <] */
foreach v of varlist `controls2' {
       drop if `v'==1
        }

/* [> Generate active group dummies with omega=3. Same activity definition as in col 2. <] */ 
gen nonzero=year if `y'>0
bysort group: egen startyear= min(nonzero)
bysort group: egen endyear= max(nonzero)
gen active = $active_window_extended

forv num=1/85 {
        * display `num'
        cap gen ActiveGroup`num' = active * Dgroup`num'
        }


*regression
my_spatial_2sls_jo `y'  ActiveGroup*  `controls1' `controls3' Dgroup*  , `mys_syntax_ivneutral' 
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)
est sto t3_c3


*eststo: ivreg2 `y' (`x' `n'=`iv_full_neutral') ActiveGroup*  `controls1' `controls3' i.group, $clus first






*/

********************
** Unbalanced Sample 
********************


/*----------------------------------------------------*/
   /* [>   Column 4:  Only using window of activity from expert coding (FZ) */ 
/*----------------------------------------------------*/

use KRTZ_monadic_AF.dta, clear
/* [> Drop singletons in controls2 <] */
foreach v of varlist `controls2' {
       drop if `v'==1
        }
sort id

/* [> Merge with start and end data from FZ <] */ 
merge m:1 id using `temp_window'
tab _merge
drop if _merge==2
drop _merge

gen start_fz=max(militia_start, group_start)
gen end_fz=min(militia_end, group_end)
gen window_activity_expert=(year > start_fz -1) & (year < end_fz+1)
sort id year

sort group year
/* [> Save dataset that has time activity dummy for each group-year <] */ 
save `temp_time_varying_network', replace 
keep if window_activity_expert==1
keep group year
sort group year
gen active=1

/* [> Save group-year-activity dataset <] */ 
save `temp_active', replace 

rename group group_d
sort group_d year
rename active active_d 
/* [> Save group_d-year-activity dataset: To then generate dataset of active enemities and alliances <] */ 
save `temp_active_d', replace


* Unbalanced panel
use KRTZ_dyadic_AF, clear
keep group group_d year allied enemy
sort group_d year


/* [> Changed merge command to m:1 <] */ 
merge m:1 group_d year using `temp_active_d'

tab _merge
drop _merge 
keep if active_d==1 /* [> This throws out unmatched merges <] */ 
sort  group year  group_d

collapse (sum) degree_plus_time=allied degree_minus_time=enemy, by( group year) // Get number of allies and enemies by time. But what we want is fighting effort of those groups over timne, isn't it?
sort group year

/* [> Changed merge command to 1:1 <] */ 
merge 1:1 group year using `temp_time_varying_network'
drop _merge 
save `temp_tvn_time', replace 

/* [> Time-varying fighting effort is NOT adjusted by activity 
        - I am changing the adjustment but the correlation should be extremely strong

         <] */ 
do  "${code}/auxiliary_code/build_time_varying_fighting_efforts.do" "`temp_active'" "`temp_active_d'"
use `temp_tvn_time'
merge 1:1 group year using KRTZ_monadic_base_mt_TIME.dta, keepusing(TotFight_Allied_TIME TotFight_Enemy_TIME TotFight_Neutral_TIME)

tab _merge
drop _merge 
keep if window_activity_expert==1

        /* [> Check correlations: They are very strong and so results should not change. <] */ 
        corr TotFight_Enemy_TIME TotFight_Enemy
        corr TotFight_Allied_TIME TotFight_Allied

qui ivreg2 `y' (`x' `n' =  `iv_full_neutral')  `controls1' `controls3' Dgroup*  ,  partial(Dgroup*  `controls3')
scalar beta  = abs(_b[ `y'_Allied])
scalar gamma = abs(_b[ `y'_Enemy])


/* [> Implement iterated linear least square estimator (Blundell and Robin 1999) <] */ 
local step =1
local prec = 1

while `prec' >0.003 & `step'<1000 { // before: step=0.002, in text: 0.0001
cap drop GAM AGG_GAM phistar
gen GAM=1/(1 + beta * degree_plus_time - gamma * degree_minus_time) 
bysort year: egen AGG_GAM=sum(GAM)
gen phistar= GAM * (1-(1/AGG_GAM)) * (1/AGG_GAM)

qui ivreg2 `y' (`x' `n' =  `iv_full_neutral')  phistar  `controls1' `controls3' Dgroup*  ,  partial(Dgroup*   `controls3')
local prec= (((beta - abs(_b[ `y'_Allied]))^2 + (gamma - abs(_b[ `y'_Enemy]))^2)^0.5)

scalar beta  = abs(_b[ `y'_Allied])
scalar gamma = abs(_b[ `y'_Enemy])

di "Iteration "`step' " with precision " `prec'
local step = `step' + 1

 }


*regression:
my_spatial_2sls_jo `y' phistar  `controls1' `controls3' Dgroup*, `mys_syntax_ivneutral' 
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)
est sto t3_c4


*eststo: ivreg2 `y' (`x' `n'=`iv_full_neutral') phistar  `controls1' `controls3' i.group, $clus first





* We check below that 2SLS and Control Functions deliver the same results
 
/* [> To be honest, I don't see where this is checked yet. We would need to do something with the residuals <] */ 
ivreg2 `y' phistar  `controls1' `controls3' i.group (`x' `n' = `iv_full_neutral'), r
reg TotFight_Enemy  phistar  `controls1' `controls3' i.group `iv_full_neutral'
predict residE, resid
reg TotFight_Allied  phistar  `controls1' `controls3' i.group `iv_full_neutral'
predict residA, resid
reg TotFight_Neutral  phistar  `controls1' `controls3' i.group `iv_full_neutral'
predict residN, resid





/*----------------------------------------------------*/
   /* [>   Column 5: 
   Only window_of_activity "keep if year > startyear -1 & year < endyear+1"
   With or Without dummies " TotFight>0 x Dgroup* " */ 
/*----------------------------------------------------*/


* We build a time varying network based on windows of activity
use KRTZ_monadic_AF.dta, clear
/* [> Drop singletons in controls2 <] */
foreach v of varlist `controls2' {
       drop if `v'==1
        }
gen nonzero=year if `y'>0
bysort group: egen startyear= min(nonzero)
bysort group: egen endyear= max(nonzero)
sort group year
save `temp_time_varying_network', replace

keep if ((year > startyear -1) & (year < endyear+1))

keep group year
sort group year
gen active=1
save `temp_active', replace 
rename group group_d
sort group_d year
rename active active_d 
save `temp_active_d', replace 
* Unbalanced panel
use KRTZ_dyadic_AF, clear
keep group group_d year allied enemy
sort group_d year

/* [> Changed merge command to m:1 <] */ 
merge m:1 group_d year using `temp_active_d'
tab _merge
drop _merge 
keep if active_d==1
sort  group year  group_d
collapse (sum) degree_plus_time=allied degree_minus_time=enemy, by( group year)
sort group year

/* [> Changed merge command to 1:1 <] */ 
merge 1:1 group year using `temp_time_varying_network'
tab _merge
drop _merge 
save `temp_tvn_time', replace 

/* [> Time-varying fighting effort is NOT adjusted by activity 
        - I am changing the adjustment but the correlation should be extremely strong

         <] */ 
qui do  "${code}/auxiliary_code/build_time_varying_fighting_efforts.do" "`temp_active'" "`temp_active_d'"
use `temp_tvn_time'
merge 1:1 group year using KRTZ_monadic_base_mt_TIME.dta, keepusing(TotFight_Allied_TIME TotFight_Enemy_TIME TotFight_Neutral_TIME)

tab _merge
drop _merge 

keep if ((year > startyear -1) & (year < endyear+1))

        /* [> Check correlations: They are very strong and so results should not change. <] */ 
        corr TotFight_Enemy_TIME TotFight_Enemy
        corr TotFight_Allied_TIME TotFight_Allied 


qui ivreg2 `y' (`x' `n' =  `iv_full_neutral')  `controls1' `controls3' Dgroup*,  partial(Dgroup*   `controls3')
scalar beta  = abs(_b[ TotFight_Allied])
scalar gamma = abs(_b[ TotFight_Enemy])

local step =1
local prec = 1


while `prec' >0.002 & `step'<1000 {
cap drop GAM AGG_GAM phistar
gen GAM=1/(1 + beta * degree_plus_time - gamma * degree_minus_time) 
bysort year: egen AGG_GAM=sum(GAM)
gen phistar= GAM * (1-(1/AGG_GAM)) * (1/AGG_GAM)

qui ivreg2 `y' (`x' `n' =  `iv_full_neutral')  phistar  `controls1' `controls3' Dgroup*,  partial(Dgroup*   `controls3')
local prec= 0.5 * (((beta - abs(_b[ TotFight_Allied]))^2 + (gamma - abs(_b[ TotFight_Enemy]))^2)^0.5)

scalar beta  = abs(_b[ TotFight_Allied])
scalar gamma = abs(_b[ TotFight_Enemy])

di "Iteration "`step' " with precision " `prec'
local step = `step' + 1

 }

*regression
my_spatial_2sls_jo `y' phistar  `controls1' `controls3' Dgroup*, `mys_syntax_ivneutral' 

predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)
est sto t3_c5

*eststo: ivreg2 `y' (`x' `n'=`iv_full_neutral') phistar  `controls1' `controls3' i.group, $clus first




/*----------------------------------------------------*/
   /* [>   Column 6: 
   Only window_of_activity_extended "keep if year > startyear -3 & year < endyear+3"
   With or Without dummies " `y'>0 x Dgroup* " */
/*----------------------------------------------------*/


* We build a time varying network based on windows of activity
use KRTZ_monadic_AF.dta, clear
gen nonzero=year if `y'>0
bysort group: egen startyear= min(nonzero)
bysort group: egen endyear= max(nonzero)
sort group year
save `temp_time_varying_network', replace
*global window_of_activity "keep if year > startyear -1 & year < endyear+1"
keep if ((year > startyear -4) & (year < endyear+4))
*keep if `y'>0
keep group year
sort group year
gen active=1
save `temp_active', replace 
rename group group_d
sort group_d year
rename active active_d 
save `temp_active_d', replace 

* Unbalanced panel
use KRTZ_dyadic_AF, clear
keep group group_d year allied enemy
sort group_d year

/* [> Changed merge command to m:1 <] */ 
merge m:1 group_d year using `temp_active_d'

tab _merge
drop _merge 
keep if active_d==1
sort  group year  group_d
collapse (sum) degree_plus_time=allied degree_minus_time=enemy, by( group year)
sort group year

/* [> Changed merge command to 1:1 <] */ 
merge 1:1 group year using `temp_time_varying_network'
tab _merge
drop _merge 

save `temp_tvn_time', replace 

/* [> Time-varying fighting effort is NOT adjusted by activity 
        - I am changing the adjustment but the correlation should be extremely strong

         <] */ 
qui do  "${code}/auxiliary_code/build_time_varying_fighting_efforts.do" "`temp_active'" "`temp_active_d'"
use `temp_tvn_time'
merge 1:1 group year using KRTZ_monadic_base_mt_TIME.dta, keepusing(TotFight_Allied_TIME TotFight_Enemy_TIME TotFight_Neutral_TIME)

tab _merge
drop _merge 

keep if ((year > startyear -4) & (year < endyear+4))

        /* [> Check correlations: They are very strong and so results should not change. <] */ 
        corr TotFight_Enemy_TIME TotFight_Enemy
        corr TotFight_Allied_TIME TotFight_Allied


qui ivreg2 `y' (`x' `n' =  `iv_full_neutral')  `controls1' `controls3' i.group,  partial( i.group  `controls3')
scalar beta  = abs(_b[ TotFight_Allied])
scalar gamma = abs(_b[ TotFight_Enemy])

local step =1
local prec = 1


while `prec' >0.002 & `step'<1000 {
cap drop GAM AGG_GAM phistar
gen GAM=1/(1 + beta * degree_plus_time - gamma * degree_minus_time) 
bysort year: egen AGG_GAM=sum(GAM)
gen phistar= GAM * (1-(1/AGG_GAM)) * (1/AGG_GAM)

qui ivreg2 `y' (`x' `n' =  `iv_full_neutral')  phistar `controls1' `controls3' i.group,  partial( i.group   `controls3')
local prec= 0.5 * (((beta - abs(_b[ TotFight_Allied]))^2 + (gamma - abs(_b[ TotFight_Enemy]))^2)^0.5)

scalar beta  = abs(_b[ TotFight_Allied])
scalar gamma = abs(_b[ TotFight_Enemy])

di "Iteration "`step' " with precision " `prec'
local step = `step' + 1

 }

*regression:
my_spatial_2sls_jo `y' phistar  `controls1' `controls3' Dgroup*, `mys_syntax_ivneutral' 
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)
est sto t3_c6

*eststo: ivreg2 `y' (`x' `n'=`iv_full_neutral') phistar  `controls1' `controls3' i.group, $clus first



/*----------------------------------------------------*/
   /* [>   Column 7: 
   TOBIT and Poisson with Balanced Panel
    */
/*----------------------------------------------------*/

use KRTZ_monadic_AF.dta, clear
/* [> Drop singletons in controls2 <] */
foreach v of varlist `controls2' {
       drop if `v'==1
        }

qui{
cap drop junk1
cap drop junk2
reg TotFight_Enemy `iv_full_neutral' `controls1' `controls3' i.group
predict junk1, residuals
reg TotFight_Allied `iv_full_neutral' `controls1' `controls3' i.group
predict junk2, residuals
reg TotFight_Neutral `iv_full_neutral' `controls1' `controls3' i.group
predict junk3, residuals
}


*  
tobit `y' `x' `n'  junk1 junk2 `controls1' `controls3' i.group,  ll(0) 
estadd scalar r2 = e(r2_p)
estadd local KP = "N/A"
estadd local HJ = "N/A"
est sto t3_c7
 

#delimit ;
estout 
t3_c1 t3_c2 t3_c3 t3_c4 t3_c5 t3_c6 t3_c7
using "../replication_outputs/tables/t3.tex" , style(tex) 
eqlabels(" " " ") 
wrap varwidth(45) 
varlabels(TotFight_Enemy "Enemies (TFE)" TotFight_Allied "Allies (TFA)" TotFight_Neutral "Neutra (TFN)")
keep(`x' `n')
order(`x' `n')
        cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(KP HJ N r2   ,
                fmt(%9.2fc %9.2fc %9.0fc %9.3fc)
                labels("\midrule \addlinespace Kleibergen-Paap F-stat" "Hansen J (p-value)" "Observations" "R-squared"))
starlevels(* 0.1 ** 0.05 *** 0.01) 
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr   





