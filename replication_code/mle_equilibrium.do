   /* [>   0.  Github integration   <] */ 
/*----------------------------------------------------*/
/*----------------------------------------------------*/
/* [> Commit and push any important changes to github regularly. <] */ 
*/*
cd "${github}"
! git add "${github}/replication_code/mle_equilibrium.do"
! git commit -m "Added some code for maximum likelihood estimation."
! git push
*/

 /* [> New branch for testing new code <] */
 // git checkout -b name-of-branch

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



use KRTZ_dyadic_AF.dta, clear
keep if year==2010
 keep group group_d enemy
reshape wide  enemy  , i(group) j(group_d)
forv i = 1/80 {
   replace enemy`i' = 0 if mi(enemy`i')
}
mkmat enemy*, mat(aminus)

save A_minus, replace

use KRTZ_dyadic_AF.dta, clear
keep if year==2010
 keep group group_d allied
reshape wide  allied  , i(group) j(group_d)
forv i = 1/80 {
   replace allied`i' = 0 if mi(allied`i')
}
mkmat allied*, mat(aplus)

save A_plus, replace



use KRTZ_monadic_AF.dta, clear


nl (TotFight = 1 / (1+{beta=0.2}*degree_plus-{gamma=-0.2}*degree_minus))

keep if year==2010
keep group id degree_plus degree_minus TotFight







/* [> Define full likelihood function <] */  
cap program drop full
program full
   
   args lnf beta gamma sigma _cons
   
   tempvar GAMMA LAMBDA CVEC
   // Define expected values of Likelihood functions
   qui gen double `GAMMA' = 1/(1+`beta'*degree_plus-`gamma'*degree_minus)
   qui egen `LAMBDA' = total(`GAMMA') // qui bys year: 
   // bys year: qui egen n = count(group)
   mkmat `GAMMA', mat(G)
   tempname Beta
   mkmat `beta', matrix(`Beta')
  
   matrix define CVEC2 = invsym(I(80) + `Beta'[1,1]*aplus) * G

   svmat CVEC2, name(`CVEC')
   qui replace `lnf' = ln(normalden($ML_y1 - `LAMBDA'*(1-`LAMBDA')*`CVEC')/`sigma')-ln(`sigma')

end 

ml model lf full (TotFight = degree_plus degree_minus) /beta /gamma /sigma
ml maximize , difficult
est sto full

stop 













/* [> Generate symmetric matrix of relations A <] */ 
clear
set obs 100
gen x = _n 
expand 100
gsort x 
gen z=_n-(x-1)*100
gen rel = rbinomial(1,0.2)

reshape wide rel  , i(x) j(z)

mkmat rel*, mat(aplus)
mata aplus = st_matrix("aplus")
mata a = makesymmetric(aplus)
mata st_matrix("axx",a)


/* [> Generate dataset <] */ 
drop _all
set obs 100
gen id = _n 
gen rel = rbinomial(10,0.3)
gen rand1 = rnormal(0,1)
gen y = rand1 + rel + rel*rand1 

/* [> Define ML-function <] */ 
cap program drop mlestimation
program mlestimation
   args lnf beta sigma
   tempvar GAMMA LAMBDA CVEC

   /* [> Generate temporary values/variables <] */ 
   qui gen `GAMMA' = 1/(1+`beta'*rel)
   mkmat `GAMMA', mat(G)
   qui egen `LAMBDA' = total(`GAMMA')
   /* [> Error happens here, as beta is "not found": r(111) <] */
   tempname Beta
   mkmat `beta', matrix(`Beta') 
   matrix define C = invsym(I(100) + `Beta'[1,1] * axx) * G

   svmat C, name(`CVEC')

   /* [> Likelihood function <] */ 
   qui replace `lnf' = ln(normalden($ML_y1 - `LAMBDA'*(1-`LAMBDA')*`CVEC')/`sigma')-ln(`sigma')
end 


/* [> Estimate ML <] */ 
ml model lf mlestimation ( y = rel) /beta /sigma
ml maximize, difficult





*------------------------ 
* TABLE 4     
*------------------------ 

* build adjency matrices
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


******************************************************
* Step 1 - Build the (Benchmark) Observed Sample
* outputs are the following dataset : bench_data.dta bench_aplus.dta bench_aminus.dta
******************************************************

use KRTZ_monadic_ref.dta, clear
save temp_counterfactual, replace

*** Check that we replicate our baseline spec w/o clustering in xtivreg format
use temp_counterfactual, clear

$baseline_specification


sum group
scalar nb_group=r(max)

predict RESID, e
predict FE, u
gen stor1=TotFight_Enemy
gen stor2=TotFight_Allied
replace TotFight_Enemy=0
replace TotFight_Allied=0
predict shifter, xb
replace TotFight_Enemy = stor1
replace TotFight_Allied = stor2
gen check= TotFight -( _b[ TotFight_Allied] * TotFight_Allied +  _b[ TotFight_Enemy] * TotFight_Enemy + shifter + FE + RESID)
sum check, d
drop check
gen gamma= _b[ TotFight_Enemy]
gen beta= - _b[ TotFight_Allied]
gen scale_correction=1
gen SHIFTER= - shifter * scale_correction
replace FE=FE * scale_correction
replace RESID=RESID * scale_correction
gen hostility=1/(1+beta * degree_plus - gamma * degree_minus) 
tab hostility
tab name if hostility<0
bysort year: egen agg_hostility=sum(hostility)
gen PHI=1-[1/(agg_hostility)]
gen U=  PHI * (1- PHI) * hostility

* combine observed/unobserved in one variable
gen TOTAL_SHIFTER=SHIFTER+U-FE-RESID

* separate observed/unobserved variables
rename SHIFTER OBS_SHIFTER
gen EPSILON=-RESID
sum EPSILON, d
gen SD_EPSILON=r(sd)
gen E=U-FE
gen interior=(TotFight>0)
gen Restr_Host = U 
gen Extd_Host = - OBS_SHIFTER + U - E -EPSILON
sum Restr_Host Extd_Host , d
keep Foreign Government_org degree_plus degree_minus Restr_Host Extd_Host hostility interior beta gamma TotFight TotFight_Enemy TotFight_Allied EPSILON  SD_EPSILON E OBS_SHIFTER U TOTAL_SHIFTER hostility degree_plus degree_minus year group name 
order year beta gamma name hostility group degree_plus degree_minus hostility TotFight TotFight_Enemy TotFight_Allied  TOTAL_SHIFTER OBS_SHIFTER U E EPSILON SD_EPSILON Restr_Host Extd_Host interior 
save bench_data, replace
* a test 
reg TotFight TotFight_Enemy TotFight_Allied OBS_SHIFTER U E EPSILON, noc
keep if e(sample)==1
gen RHS= - OBS_SHIFTER + U - E - EPSILON
reg TotFight TotFight_Enemy TotFight_Allied RHS, noc
keep Foreign Government_org  degree_plus degree_minus year beta gamma group name TotFight TotFight_Enemy TotFight_Allied  OBS_SHIFTER  U E EPSILON RHS
sort year group
by year : gen MCref=[_n]
duplicates report group MCref
label var MCref "group id in the Monte Carlo simulation"
sort year MCref
save bench_data, replace
keep if year==2005
keep group MCref
sort group
save MC_merging_key.dta, replace

** build bench networks data
use aminus_ref, clear
sort group
by group: keep if [_n]==1
replace group_d=group
replace aminus=0
tab aminus
sort group group_d
save temp_square, replace
use aminus_ref, clear
append using temp_square
sort group group_d
save bench_aminus, replace
use aplus_ref, clear
sort group
by group: keep if [_n]==1
replace group_d=group
replace aplus=0
tab aplus
sort group group_d
save temp_square, replace
use aplus_ref, clear
append using temp_square
sort group group_d
save bench_aplus, replace
erase temp_square.dta

use MC_merging_key.dta, clear
rename MCref MCref_d
rename group group_d
sort group_d
save temp, replace

use bench_aminus, clear
sort group
merge group using MC_merging_key
tab _merge
drop _merge
sort group_d
merge group_d using temp
tab _merge
drop _merge
drop group group_d
sort MCref MCref_d
save bench_aminus, replace
use bench_aplus, clear
sort group
merge group using MC_merging_key
tab _merge
drop _merge
sort group_d
merge group_d using temp
tab _merge
drop _merge
drop group group_d
sort MCref MCref_d
save bench_aplus, replace


* Generate observed share in total fighting
use bench_data, clear
collapse (sum) TotFight, by(group)
egen AggFight= sum(TotFight)
gen observed_share=TotFight / AggFight
keep group observed_share
sort group
save obs_share.dta, replace

******************************************************
* step 2 - simulation of the benchmark equilibrium 
******************************************************
* we use our subprogram that simulates the equilibrium
use bench_data, clear
save temp_MC, replace
use bench_aminus, clear
save temp_aminus, replace
use bench_aplus, clear
save temp_aplus, replace
global time "1998(1)2010"
qui do ../progs/eq_simul.do
use simul, clear
save bench_simul, replace

* Crucial test : Do we retrieve our data from the simulation based on benchmark values? 
use bench_simul
keep MCref year EFFORT*
sort MCref year
save temp, replace
use bench_data, clear
sort MCref year
merge MCref year using temp
tab _merge 
drop _merge
gen test1=(EFFORT - TotFight)^2
gen test2=(EFFORT_Enemy - TotFight_Enemy)^2
gen test3=(EFFORT_Allied - TotFight_Allied)^2
sum test*, d
drop test*


******************************************************
* Step 3 - TABLE 4
******************************************************
* Benchmark = average data
use bench_data, clear
sort MCref group name year
collapse (mean) Foreign Government_org  degree_plus degree_minus beta gamma TotFight TotFight_Enemy TotFight_Allied OBS_SHIFTER E EPSILON year, by (MCref group name)
replace year=1000
global time "1000"
save avgbench_data, replace

* Simulate the benchmark equilibrium
use avgbench_data, clear
save temp_MC, replace
use bench_aminus, clear
save temp_aminus, replace
use bench_aplus, clear
save temp_aplus, replace
qui do ../progs/eq_simul.do
use simul, clear
save avgbench_simul, replace
collapse(sum) EFFORT
scalar bench_rd = EFFORT


** Patch 1 for removing the two Rwandas Simultaneously
* to desactivate the patch we must select ONLY the lines 1 and 2 below and desactivate the lines that build the merged group (see patch 2 below)
scalar RWApre=100000
scalar RWApost=100000
qui use avgbench_data, clear
sort MCref
sum MCref if name=="Military Forces of Rwanda (1994-1999)"
scalar RWApre=r(max)
sum MCref if name=="Military Forces of Rwanda (2000-)"
scalar RWApost=r(max)
** End of Patch 1

* Simulate each counterfactual equilibrium
foreach kp of numlist 1(1)150 {
if `kp'<nb_group + 1 {
di `kp'
qui {
      use avgbench_data, clear
      sort MCref
      drop if MCref==`kp'
      drop if MCref==RWApost & `kp'==RWApre
      drop if MCref==RWApre & `kp'==RWApost
      sort MCref year
      save temp_MC, replace
      use bench_aminus, clear
      drop if MCref==`kp'
      drop if MCref==RWApost & `kp'==RWApre
      drop if MCref==RWApre & `kp'==RWApost
      drop if MCref_d==`kp'
      drop if MCref_d==RWApost & `kp'==RWApre
      drop if MCref_d==RWApre & `kp'==RWApost
      sort MCref MCref_d 
      save temp_aminus, replace
      use bench_aplus, clear
      drop if MCref==`kp'
      drop if MCref==RWApost & `kp'==RWApre
      drop if MCref==RWApre & `kp'==RWApost
      drop if MCref_d==`kp'
      drop if MCref_d==RWApost & `kp'==RWApre
      drop if MCref_d==RWApre & `kp'==RWApost
      sort MCref MCref_d
      save temp_aplus, replace
      do ../progs/eq_simul.do
      use simul, clear
      save KPcounter_simul_`kp', replace
      }
}
}

* Display the KP results
clear
gen MCref=.
gen bench_RD=.
gen counter_RD=.
save KPresult, replace

foreach kp of numlist 1(1)150 {
if `kp'<nb_group + 1 {
   qui {
         use KPcounter_simul_`kp', clear
         collapse (sum) EFFORT
         rename EFFORT counter_RD
         gen MCref=`kp'
         gen bench_RD=bench_rd
         append using KPresult 
         save KPresult, replace
         capture erase KPcounter_simul_`kp'.dta
         }
}
}

use avgbench_data, clear
keep Foreign Government_org group MCref name TotFight degree_plus degree_minus
sort MCref
save MC_merging_key.dta, replace

use KPresult, clear
sort MCref
merge MCref using MC_merging_key
tab _merge
drop _merge
cap drop Delta_RD
gen Delta_RD=((counter_RD /bench_RD)-1)
egen agg_fight=sum(TotFight)
compare agg_fight bench_RD
gen bench_fighting_share=TotFight/agg_fight
order name degree_minus degree_plus bench_fighting_share Delta_RD 
drop if Government_org==1 & Foreign==0
gsort Delta_RD
gen rank=[_n]
gen multiplier= abs(Delta_RD / bench_fighting_share)
keep degree_plus degree_minus rank name bench_fighting_share Delta_RD multiplier
save KeyPlayer_result, replace

** Patch 2 for removing the two Rwandas Simultaneously
use KeyPlayer_result, clear
keep if name == "Military Forces of Rwanda (1994-1999)"|name=="Military Forces of Rwanda (2000-)"
collapse (sum) bench_fighting_share (mean) Delta_RD
gen multiplier= abs(Delta_RD / bench_fighting_share)
gen name = "Military Forces of Rwanda"
sort name
save tempRWA.dta, replace
use KRTZ_dyadic_ref.dta, clear
keep if name == "Military Forces of Rwanda (1994-1999)"|name=="Military Forces of Rwanda (2000-)"
drop if name_d == "Military Forces of Rwanda (1994-1999)"|name_d=="Military Forces of Rwanda (2000-)"
keep if year==2000
keep name name_d allied enemy
sort name_d
collapse (max) allied enemy , by(name_d)
collapse (sum) degree_plus=allied degree_minus=enemy
gen name = "Military Forces of Rwanda"
sort name
merge name using tempRWA
drop _merge
append using KeyPlayer_result
drop rank
drop if name == "Military Forces of Rwanda (1994-1999)"|name=="Military Forces of Rwanda (2000-)"
gsort Delta_RD
gen rank=[_n]
save KeyPlayer_result, replace
* End of Patch 2

gsort -bench_fighting_share
gen rank_fighting=[_n]
sort rank
order rank name degree_minus degree_plus bench_fighting_share rank_fighting Delta_RD 
save KeyPlayer_result, replace
keep if rank<16
drop rank_fighting

export excel using ../results/KeyPlayer_result.xls, replace first(varl)



/*

file open fh using "../replication_outputs/tables/t4.tex", replace write
      loc line1 " (`q') & `lab_`y''  & ``T'_b_`y''``T'_star_`y'' & ``T'qval`q''& ``T'_mean_`y'' & ``T'_N_`y'' "
      loc line2 " & & (``T'_se_`y'')& &(``T'_sd_`y'')"
      file write fh "`line1'" "\\" _n
      file write fh "`line2'" "\\" _n
   
   file write fh "& Enumerator FE & \checkmark &  &  &  \\" _n                   
   file write fh "\bottomrule" _n                     
   file write fh "\end{tabular}" _n                
   file write fh "\label{tab:Main_`T'}" _n
   file close fh













* Estimation of Confidence Intervals 
global gamaselect "gen gamma= _b[ TotFight_Enemy] + _se[ TotFight_Enemy]" 
global betaselect "gen beta= - _b[ TotFight_Allied] + _se[ TotFight_Allied]"
do ../progs/KeyPlayerAnalysis_plusSD.do
global gamaselect "gen gamma= _b[ TotFight_Enemy] - _se[ TotFight_Enemy]" 
global betaselect "gen beta= - _b[ TotFight_Allied] - _se[ TotFight_Allied]"
do ../progs/KeyPlayerAnalysis_minusSD.do
