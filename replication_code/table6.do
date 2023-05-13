
*------------------------ 
* SECTION 5.3 - PACIFICATION POLICIES
* [incl. TABLE 6 AND FIGURES REPORTED IN THE TEXT]     
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
keep Government_org Foreign Restr_Host Extd_Host hostility interior beta gamma TotFight TotFight_Enemy TotFight_Allied EPSILON  SD_EPSILON E OBS_SHIFTER U TOTAL_SHIFTER hostility degree_plus degree_minus year group name 
order year beta gamma name hostility group degree_plus degree_minus hostility TotFight TotFight_Enemy TotFight_Allied  TOTAL_SHIFTER OBS_SHIFTER U E EPSILON SD_EPSILON Restr_Host Extd_Host interior 
save bench_data, replace
* a test 
reg TotFight TotFight_Enemy TotFight_Allied OBS_SHIFTER U E EPSILON, noc
keep if e(sample)==1
gen RHS= - OBS_SHIFTER + U - E - EPSILON
reg TotFight TotFight_Enemy TotFight_Allied RHS, noc
keep Government_org Foreign year degree_plus degree_minus beta gamma group name TotFight TotFight_Enemy TotFight_Allied  OBS_SHIFTER  U E EPSILON RHS
sort year group
by year : gen MCref=[_n]
duplicates report group MCref
label var MCref "group id in the Monte Carlo simulation"
sort year MCref
save bench_data, replace
keep if year==2000
keep group MCref
sort group
save MC_merging_key.dta, replace
rename group group_d
rename MCref MCref_d
sort group_d
save MC_merging_key_d.dta, replace
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
use bench_simul, clear
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

* Benchmark = average data
use bench_data, clear
sort MCref group name year
collapse (mean) Foreign Government_org  beta gamma degree_minus degree_plus TotFight TotFight_Enemy TotFight_Allied OBS_SHIFTER E EPSILON year, by (MCref group name)
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


**************************
* No enemies
**************************
use avgbench_data, clear
save temp_MC, replace
use bench_aminus, clear
replace aminus=0
save temp_aminus, replace
use bench_aplus, clear
save temp_aplus, replace
qui do ../progs/eq_simul.do
use simul, clear
keep EFFORT year MCref
rename EFFORT no_enemies_EFFORT
save no_enemies_simul, replace
use avgbench_simul, clear
sort MCref year
merge MCref year using no_enemies_simul
tab _merge
drop _merge
sort year MCref
collapse (sum)  EFFORT  no_enemies_EFFORT  , by (year)
gen Delta_RD_no_enemies=(no_enemies_EFFORT /EFFORT)-1
log using ../results/links_removal.txt, text replace
set linesize 150
di "Re-wiring all rivalry links into neutrality, the counterfactual change in aggregate fighting is " Delta_RD_no_enemies 
log close


**************************
* TABLE 6 
**************************
use avgbench_data, clear
save temp_MC, replace
sort MCref year
bysort MCref: keep if [_n]==1
gen DRCgov= (Government_org==1)& (Foreign==0)
replace DRCgov=0 if name =="Military Forces of Zaire (1965-1997)"
tab DRCgov
count if DRCgov==0
scalar Nlinks=r(N)
sort DRCgov MCref
by DRCgov: gen IDlink=[_n] * (DRCgov==0)
keep MCref DRCgov IDlink name group
sort IDlink
save temp_name.dta, replace
keep MCref DRCgov IDlink
sort MCref
save temp_gov.dta, replace
rename MCref MCref_d
rename DRCgov DRCgov_d
rename IDlink IDlink_d
sort MCref_d
save temp_gov_d.dta, replace
use bench_aplus, clear
sort MCref MCref_d
merge MCref using temp_gov
tab _merge
drop _merge
sort MCref_d
merge MCref_d using temp_gov_d
tab _merge
drop _merge
tab IDlink
sort IDlink
save temp_bench_aplus.dta, replace
use bench_aminus, clear
sort MCref MCref_d
merge MCref using temp_gov
tab _merge
drop _merge
sort MCref_d
merge MCref_d using temp_gov_d
tab _merge
drop _merge
tab IDlink
sort IDlink
save temp_bench_aminus.dta, replace
* list of enemies
use temp_bench_aminus, clear
keep if aminus==1 & DRCgov_d==1 & DRCgov==0
bys MCref: keep if [_n]==1
keep MCref IDlink
sort IDlink
* select the subset of players 
levelsof IDlink, local(Reconci)
save enemy_DRCgov.dta, replace
scalar RWApre=100000
scalar RWApost=100000
use temp_name.dta, clear
sum IDlink if name=="Military Forces of Rwanda (1994-1999)"
scalar RWApre=r(max)
sum IDlink if name=="Military Forces of Rwanda (2000-)"
scalar RWApost=r(max)
foreach kp of local Reconci {
 if `kp'<Nlinks+1{
 di `kp'
qui use temp_bench_aplus.dta,clear
qui drop DRCgov DRCgov_d
qui drop IDlink IDlink_d
qui save temp_aplus, replace 

qui use temp_bench_aminus.dta,clear
qui replace aminus=0 if (IDlink==`kp'& DRCgov_d==1)|(IDlink_d==`kp'& DRCgov==1)
if `kp'==RWApre{
qui replace aminus=0 if (IDlink==RWApost& DRCgov_d==1)|(IDlink_d==RWApost& DRCgov==1)
}
if `kp'==RWApost{
qui replace aminus=0 if (IDlink==RWApre& DRCgov_d==1)|(IDlink_d==RWApre& DRCgov==1)
}
qui drop DRCgov DRCgov_d
qui drop IDlink IDlink_d
qui save temp_aminus, replace
qui global time "1000"
qui do ../progs/eq_simul.do
qui use simul, clear
qui keep EFFORT year MCref
qui collapse (sum) EFFORT 
qui rename EFFORT counter_RD
qui gen IDlink=`kp'
qui gen bench_RD=bench_rd
qui save KPcounter_simul_`kp', replace
 }
 }
 
 
clear
set obs 1 
gen trash=1
save KPcounter_simul.dta, replace

foreach kp of local Reconci {
 if `kp'<Nlinks+1{
 append using KPcounter_simul_`kp' 
 erase KPcounter_simul_`kp'.dta
 }
 }
 
cap drop trash
cap drop if bench_RD==. 
save KPcounter_simul.dta, replace


use temp_name, clear
keep group DRCgov IDlink
sort group
save temp_name, replace
rename group group_d
rename DRCgov DRCgov_d
drop IDlink
sort group_d
save temp_name_d, replace

use avgbench_data, clear
keep Foreign Government_org group MCref name TotFight 
sort group
merge group using temp_name
tab _merge
drop _merge
sort IDlink
save MC_merging_key.dta, replace

use KPcounter_simul.dta, clear
sort IDlink
merge IDlink using MC_merging_key
count
tab _merge
drop _merge
sort IDlink
merge IDlink using enemy_DRCgov
tab _merge
gen  enemy_of_gov = (_merge==3)
drop _merge
cap drop Delta_RD
gen Delta_RD=((counter_RD /bench_RD)-1)
egen agg_fight=sum(TotFight)
compare agg_fight bench_RD
gen bench_fighting_share=TotFight/agg_fight
drop if DRCgov==1 
gsort Delta_RD
gen rank=[_n]
order rank name bench_fighting_share Delta_RD group 
keep rank name bench_fighting_share Delta_RD enemy_of_gov
save bilateral_reconciliation.dta, replace
use KRTZ_dyadic_ref, clear
sort group name group_d year
collapse (sum) Nfighting, by (group name group_d)
gen NNfighting=Nfighting
egen agg_fighting=sum(NNfighting)
sort group group_d
merge group using temp_name
tab _merge
drop _merge
keep if IDlink>0
sort group_d
merge group_d using temp_name_d
tab _merge
drop _merge
sort IDlink group DRCgov_d
collapse (sum) Nfighting (mean) agg_fighting , by(IDlink group name DRCgov_d)
gen bilateral_bench_fighting_share=Nfighting/agg_fighting
bysort group: egen bench_fighting_share_alt=sum(bilateral_bench_fighting_share)
keep if DRCgov_d==1
keep name IDlink group bilateral_bench_fighting_share bench_fighting_share_alt
sort name
save temp.dta, replace
use bilateral_reconciliation, clear
sort name
merge name using temp
tab _merge
drop _merge
compare bench_fighting_share bench_fighting_share_alt
drop bench_fighting_share_alt
compare bilateral_bench_fighting_share bench_fighting_share
replace bilateral_bench_fighting_share = bench_fighting_share if bilateral_bench_fighting_share > bench_fighting_share
order rank name bench_fighting_share bilateral_bench_fighting_share Delta_RD
keep  rank name bench_fighting_share bilateral_bench_fighting_share Delta_RD enemy_of_gov
gsort -bench_fighting_share
gen rank_fighting=[_n]
gen multiplier= abs(Delta_RD / bilateral_bench_fighting_share)
gsort -bilateral_bench_fighting_share
gen rank_bil_fighting=[_n]
egen share_gov_fighting=sum(bilateral_bench_fighting_share)
replace share_gov_fighting=bilateral_bench_fighting_share/share_gov_fighting
gsort -share_gov_fighting
sort rank
save bilateral_reconciliation.dta, replace
use bilateral_reconciliation.dta,clear
keep if enemy_of_gov==1
drop rank
gsort Delta_RD
gen rank=[_n]
save bilateral_reconciliation.dta, replace
use bilateral_reconciliation, clear
keep if name == "Military Forces of Rwanda (1994-1999)"|name=="Military Forces of Rwanda (2000-)"
collapse (sum) bench_fighting_share bilateral_bench_fighting_share share_gov_fighting (mean) Delta_RD enemy_of_gov
gen multiplier= abs(Delta_RD / bilateral_bench_fighting_share)
gen name = "Military Forces of Rwanda"
sort name
save tempRWA.dta, replace
append using  bilateral_reconciliation
drop rank*
drop if name == "Military Forces of Rwanda (1994-1999)"|name=="Military Forces of Rwanda (2000-)"
gsort -bench_fighting_share
gen rank_fighting=[_n]
gsort -bilateral_bench_fighting_share
gen rank_bil_fighting=[_n]
gsort Delta_RD
gen rank=[_n]
save bilateral_reconciliation.dta, replace
drop share_gov_fighting 
order rank name bench_fighting_share rank_fighting bilateral_bench_fighting_share rank_bil_fighting Delta_RD multiplier
label var name "Group"
label var bench_fighting_share "obs. share in aggregate fight."
label var Delta_RD  "count. change in aggregate fight."
label var rank "rank"
label var enemy_of_gov "Enemy of DRC gov." 
label var bilateral_bench_fighting_share "obs. bilateral share in aggregate fight."
label var rank_fighting "rank obs. share in agg. fight"
label var rank_bil_fighting "rank obs. bil. share in agg. fight"
save bilateral_reconciliation.dta, replace
keep if rank<16
drop rank_fighting rank_bil_fighting enemy_of_gov

save t6_temp.dta, replace 




 
/*----------------------------------------------------*/
   /* [>   Plus SE   <] */ 
/*----------------------------------------------------*/
* Estimation of Confidence Intervals 
global gamaselect "gen gamma= _b[ TotFight_Enemy] + _se[ TotFight_Enemy]" 
global betaselect "gen beta= - _b[ TotFight_Allied] + _se[ TotFight_Allied]"


******************************************************
* Step 1 - Build the (Benchmark) Observed Sample
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
** Select the coefficients here : those options are set in the mother program KRTZ_Master_program
$gamaselect
$betaselect
gen scale_correction=1
gen SHIFTER= - shifter * scale_correction
replace FE=FE * scale_correction
replace RESID=RESID * scale_correction
gen hostility=1/(1+beta * degree_plus - gamma * degree_minus) 
tab hostility
tab name if hostility<0
bysort year: egen agg_hostility=sum(hostility)
gen PHI=1-[1/(agg_hostility)]
gen U= PHI * (1- PHI) * hostility
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
keep Government_org Foreign Restr_Host Extd_Host hostility interior beta gamma TotFight TotFight_Enemy TotFight_Allied EPSILON  SD_EPSILON E OBS_SHIFTER U TOTAL_SHIFTER hostility degree_plus degree_minus year group name 
order year beta gamma name hostility group degree_plus degree_minus hostility TotFight TotFight_Enemy TotFight_Allied  TOTAL_SHIFTER OBS_SHIFTER U E EPSILON SD_EPSILON Restr_Host Extd_Host interior 
save bench_data, replace
* a test 
reg TotFight TotFight_Enemy TotFight_Allied OBS_SHIFTER U E EPSILON, noc
keep if e(sample)==1
gen RHS= - OBS_SHIFTER + U - E - EPSILON
reg TotFight TotFight_Enemy TotFight_Allied RHS, noc
keep Government_org Foreign year degree_plus degree_minus beta gamma group name TotFight TotFight_Enemy TotFight_Allied  OBS_SHIFTER  U E EPSILON RHS
sort year group
by year : gen MCref=[_n]
duplicates report group MCref
label var MCref "group id in the Monte Carlo simulation"
sort year MCref
save bench_data, replace
keep if year==2000
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
use bench_simul, clear
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

* Benchmark = average data
use bench_data, clear
sort MCref group name year
* collapse (mean) Foreign Government_org  degree_plus degree_minus beta gamma TotFight TotFight_Enemy TotFight_Allied OBS_SHIFTER E EPSILON year, by (MCref group name)
collapse (mean) Foreign Government_org  beta gamma degree_minus degree_plus TotFight TotFight_Enemy TotFight_Allied OBS_SHIFTER E EPSILON year, by (MCref group name)
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

*****
* TABLE 6 
*****

use avgbench_data, clear
save temp_MC, replace

sort MCref year
bysort MCref: keep if [_n]==1
* spot the grooups affiliated to DRC governmental
gen DRCgov= (Government_org==1)& (Foreign==0)
replace DRCgov=0 if name =="Military Forces of Zaire (1965-1997)"
 

tab DRCgov
count if DRCgov==0
scalar Nlinks=r(N)
sort DRCgov MCref
by DRCgov: gen IDlink=[_n] * (DRCgov==0)
keep MCref DRCgov IDlink name group
sort IDlink
save temp_name.dta, replace
keep MCref DRCgov IDlink
sort MCref
save temp_gov.dta, replace
rename MCref MCref_d
rename DRCgov DRCgov_d
rename IDlink IDlink_d
sort MCref_d
save temp_gov_d.dta, replace

use bench_aplus, clear
sort MCref MCref_d
merge MCref using temp_gov
tab _merge
drop _merge
sort MCref_d
merge MCref_d using temp_gov_d
tab _merge
drop _merge
tab IDlink
sort IDlink
save temp_bench_aplus.dta, replace

use bench_aminus, clear
sort MCref MCref_d
merge MCref using temp_gov
tab _merge
drop _merge
sort MCref_d
merge MCref_d using temp_gov_d
tab _merge
drop _merge
tab IDlink
sort IDlink
save temp_bench_aminus.dta, replace

* list of enemies
use temp_bench_aminus, clear
keep if aminus==1 & DRCgov_d==1 & DRCgov==0
bys MCref: keep if [_n]==1
keep MCref IDlink
sort IDlink
* select the subset of players 
levelsof IDlink, local(Reconci)
save enemy_DRCgov.dta, replace

scalar RWApre=100000
scalar RWApost=100000
use temp_name.dta, clear
sum IDlink if name=="Military Forces of Rwanda (1994-1999)"
scalar RWApre=r(max)
sum IDlink if name=="Military Forces of Rwanda (2000-)"
scalar RWApost=r(max)


foreach kp of local Reconci {
 if `kp'<Nlinks+1{
 di `kp'

qui use temp_bench_aplus.dta,clear
qui drop DRCgov DRCgov_d
qui drop IDlink IDlink_d
qui save temp_aplus, replace 

qui use temp_bench_aminus.dta,clear
qui replace aminus=0 if (IDlink==`kp'& DRCgov_d==1)|(IDlink_d==`kp'& DRCgov==1)
if `kp'==RWApre{
qui replace aminus=0 if (IDlink==RWApost& DRCgov_d==1)|(IDlink_d==RWApost& DRCgov==1)
}
if `kp'==RWApost{
qui replace aminus=0 if (IDlink==RWApre& DRCgov_d==1)|(IDlink_d==RWApre& DRCgov==1)
}
qui drop DRCgov DRCgov_d
qui drop IDlink IDlink_d
qui save temp_aminus, replace
 
qui global time "1000"
qui do ../progs/eq_simul.do
qui use simul, clear
qui keep EFFORT year MCref
qui collapse (sum) EFFORT 
qui rename EFFORT counter_RD
qui gen IDlink=`kp'
qui gen bench_RD=bench_rd
qui save KPcounter_simul_`kp', replace
 }
 }
 
 
clear
set obs 1 
gen trash=1
save KPcounter_simul.dta, replace

foreach kp of local Reconci {
 if `kp'<Nlinks+1{
 append using KPcounter_simul_`kp' 
 erase KPcounter_simul_`kp'.dta
 }
 }
 
cap drop trash
cap drop if bench_RD==. 
save KPcounter_simul.dta, replace


use temp_name, clear
keep group DRCgov IDlink
sort group
save temp_name, replace
rename group group_d
rename DRCgov DRCgov_d
drop IDlink
sort group_d
save temp_name_d, replace

use avgbench_data, clear
keep Foreign Government_org group MCref name TotFight 
sort group
merge group using temp_name
tab _merge
drop _merge
sort IDlink
save MC_merging_key.dta, replace

use KPcounter_simul.dta, clear
sort IDlink
merge IDlink using MC_merging_key
count
tab _merge
drop _merge
sort IDlink
merge IDlink using enemy_DRCgov
tab _merge
gen  enemy_of_gov = (_merge==3)
drop _merge
cap drop Delta_RD
gen Delta_RD=((counter_RD /bench_RD)-1)
egen agg_fight=sum(TotFight)
compare agg_fight bench_RD
gen bench_fighting_share=TotFight/agg_fight
drop if DRCgov==1 
gsort Delta_RD
gen rank=[_n]
order rank name bench_fighting_share Delta_RD group 
keep rank name bench_fighting_share Delta_RD enemy_of_gov

label var name "Group"
label var bench_fighting_share "obs. share in aggregate fight."
label var Delta_RD  "count. change in aggregate fight."
label var rank "rank"
label var enemy_of_gov "Enemy of DRC gov." 
save bilateral_reconciliation_plusSD.dta, replace

use KRTZ_dyadic_ref, clear
sort group name group_d year
collapse (sum) Nfighting, by (group name group_d)
gen NNfighting=Nfighting
egen agg_fighting=sum(NNfighting)
sort group group_d
merge group using temp_name
tab _merge
drop _merge
keep if IDlink>0
sort group_d
merge group_d using temp_name_d
tab _merge
drop _merge
sort IDlink group DRCgov_d
collapse (sum) Nfighting (mean) agg_fighting , by(IDlink group name DRCgov_d)
gen bilateral_bench_fighting_share=Nfighting/agg_fighting
bysort group: egen bench_fighting_share_alt=sum(bilateral_bench_fighting_share)
keep if DRCgov_d==1
keep name IDlink group bilateral_bench_fighting_share bench_fighting_share_alt
sort name
save temp.dta, replace
use bilateral_reconciliation_plusSD, clear
sort name
merge name using temp
tab _merge
drop _merge
compare bench_fighting_share bench_fighting_share_alt
drop bench_fighting_share_alt
compare bilateral_bench_fighting_share bench_fighting_share
replace bilateral_bench_fighting_share = bench_fighting_share if bilateral_bench_fighting_share > bench_fighting_share
label var bilateral_bench_fighting_share "obs. bilateral share in aggregate fight."
order rank name bench_fighting_share bilateral_bench_fighting_share Delta_RD
keep  rank name bench_fighting_share bilateral_bench_fighting_share Delta_RD enemy_of_gov
gsort -bench_fighting_share
gen rank_fighting=[_n]
gen multiplier= abs(Delta_RD / bilateral_bench_fighting_share)
gsort -bilateral_bench_fighting_share
gen rank_bil_fighting=[_n]
egen share_gov_fighting=sum(bilateral_bench_fighting_share)
replace share_gov_fighting=bilateral_bench_fighting_share/share_gov_fighting
gsort -share_gov_fighting
sort rank
save bilateral_reconciliation_plusSD.dta, replace

use bilateral_reconciliation_plusSD.dta,clear
keep if enemy_of_gov==1
drop rank
gsort Delta_RD
gen rank=[_n]
save bilateral_reconciliation_plusSD.dta, replace


use bilateral_reconciliation_plusSD, clear
keep if name == "Military Forces of Rwanda (1994-1999)"|name=="Military Forces of Rwanda (2000-)"
collapse (sum) bench_fighting_share bilateral_bench_fighting_share share_gov_fighting (mean) Delta_RD enemy_of_gov
gen multiplier= abs(Delta_RD / bilateral_bench_fighting_share)
gen name = "Military Forces of Rwanda"
sort name
save tempRWA.dta, replace
append using  bilateral_reconciliation_plusSD
drop rank*
drop if name == "Military Forces of Rwanda (1994-1999)"|name=="Military Forces of Rwanda (2000-)"
gsort -bench_fighting_share
gen rank_fighting=[_n]
gsort -bilateral_bench_fighting_share
gen rank_bil_fighting=[_n]
gsort Delta_RD
gen rank=[_n]
order rank name bench_fighting_share rank_fighting bilateral_bench_fighting_share rank_bil_fighting Delta_RD
label var name "Group"
label var bench_fighting_share "obs. share in aggregate fight."
label var Delta_RD  "count. change in aggregate fight."
label var rank "rank"
label var enemy_of_gov "Enemy of DRC gov." 
label var bilateral_bench_fighting_share "obs. bilateral share in aggregate fight."
label var rank_fighting "rank obs. share in agg. fight"
label var rank_bil_fighting "rank obs. bil. share in agg. fight"
save bilateral_reconciliation_plusSD.dta, replace
keep name Delta_RD 

ren Delta_RD drd_plus

save t6_temp_plus.dta, replace 







 
/*----------------------------------------------------*/
   /* [>   MINUS SD] */ 
/*----------------------------------------------------*/

global gamaselect "gen gamma= _b[ TotFight_Enemy] - _se[ TotFight_Enemy]" 
global betaselect "gen beta= - _b[ TotFight_Allied] - _se[ TotFight_Allied]"


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
** Select the coefficients here : those options are set in the mother program KRTZ_Master_program
$gamaselect
$betaselect
gen scale_correction=1
gen SHIFTER= - shifter * scale_correction
replace FE=FE * scale_correction
replace RESID=RESID * scale_correction
gen hostility=1/(1+beta * degree_plus - gamma * degree_minus) 
tab hostility
tab name if hostility<0
bysort year: egen agg_hostility=sum(hostility)
gen PHI=1-[1/(agg_hostility)]
gen U= PHI * (1- PHI) * hostility
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
keep Government_org Foreign Restr_Host Extd_Host hostility interior beta gamma TotFight TotFight_Enemy TotFight_Allied EPSILON  SD_EPSILON E OBS_SHIFTER U TOTAL_SHIFTER hostility degree_plus degree_minus year group name 
order year beta gamma name hostility group degree_plus degree_minus hostility TotFight TotFight_Enemy TotFight_Allied  TOTAL_SHIFTER OBS_SHIFTER U E EPSILON SD_EPSILON Restr_Host Extd_Host interior 
save bench_data, replace
* a test 
reg TotFight TotFight_Enemy TotFight_Allied OBS_SHIFTER U E EPSILON, noc
keep if e(sample)==1
gen RHS= - OBS_SHIFTER + U - E - EPSILON
reg TotFight TotFight_Enemy TotFight_Allied RHS, noc
keep Government_org Foreign year degree_plus degree_minus beta gamma group name TotFight TotFight_Enemy TotFight_Allied  OBS_SHIFTER  U E EPSILON RHS
sort year group
by year : gen MCref=[_n]
duplicates report group MCref
label var MCref "group id in the Monte Carlo simulation"
sort year MCref
save bench_data, replace
keep if year==2000
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
use bench_simul, clear
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

* Benchmark = average data
use bench_data, clear
sort MCref group name year
* collapse (mean) Foreign Government_org  degree_plus degree_minus beta gamma TotFight TotFight_Enemy TotFight_Allied OBS_SHIFTER E EPSILON year, by (MCref group name)
collapse (mean) Foreign Government_org  beta gamma degree_minus degree_plus TotFight TotFight_Enemy TotFight_Allied OBS_SHIFTER E EPSILON year, by (MCref group name)
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



*****
* TABLE 6
*****

use avgbench_data, clear
save temp_MC, replace

sort MCref year
bysort MCref: keep if [_n]==1
* spot the groups affiliated to DRC governmental
gen DRCgov= (Government_org==1)& (Foreign==0)
replace DRCgov=0 if name =="Military Forces of Zaire (1965-1997)"
 

tab DRCgov
count if DRCgov==0
scalar Nlinks=r(N)
sort DRCgov MCref
by DRCgov: gen IDlink=[_n] * (DRCgov==0)
keep MCref DRCgov IDlink name group
sort IDlink
save temp_name.dta, replace
keep MCref DRCgov IDlink
sort MCref
save temp_gov.dta, replace
rename MCref MCref_d
rename DRCgov DRCgov_d
rename IDlink IDlink_d
sort MCref_d
save temp_gov_d.dta, replace

use bench_aplus, clear
sort MCref MCref_d
merge MCref using temp_gov
tab _merge
drop _merge
sort MCref_d
merge MCref_d using temp_gov_d
tab _merge
drop _merge
tab IDlink
sort IDlink
save temp_bench_aplus.dta, replace

use bench_aminus, clear
sort MCref MCref_d
merge MCref using temp_gov
tab _merge
drop _merge
sort MCref_d
merge MCref_d using temp_gov_d
tab _merge
drop _merge
tab IDlink
sort IDlink
save temp_bench_aminus.dta, replace

* list of enemies
use temp_bench_aminus, clear
keep if aminus==1 & DRCgov_d==1 & DRCgov==0
bys MCref: keep if [_n]==1
keep MCref IDlink
sort IDlink
* select the subset of players 
levelsof IDlink, local(Reconci)
save enemy_DRCgov.dta, replace



scalar RWApre=100000
scalar RWApost=100000
use temp_name.dta, clear
sum IDlink if name=="Military Forces of Rwanda (1994-1999)"
scalar RWApre=r(max)
sum IDlink if name=="Military Forces of Rwanda (2000-)"
scalar RWApost=r(max)

foreach kp of local Reconci {
 if `kp'<Nlinks+1{
 di `kp'

qui use temp_bench_aplus.dta,clear
qui drop DRCgov DRCgov_d
qui drop IDlink IDlink_d
qui save temp_aplus, replace 

qui use temp_bench_aminus.dta,clear
qui replace aminus=0 if (IDlink==`kp'& DRCgov_d==1)|(IDlink_d==`kp'& DRCgov==1)
if `kp'==RWApre{
qui replace aminus=0 if (IDlink==RWApost& DRCgov_d==1)|(IDlink_d==RWApost& DRCgov==1)
}
if `kp'==RWApost{
qui replace aminus=0 if (IDlink==RWApre& DRCgov_d==1)|(IDlink_d==RWApre& DRCgov==1)
}
qui drop DRCgov DRCgov_d
qui drop IDlink IDlink_d
qui save temp_aminus, replace
 
qui global time "1000"
qui do ../progs/eq_simul.do
qui use simul, clear
qui keep EFFORT year MCref
qui collapse (sum) EFFORT 
qui rename EFFORT counter_RD
qui gen IDlink=`kp'
qui gen bench_RD=bench_rd
qui save KPcounter_simul_`kp', replace
 }
 }
 
 
clear
set obs 1 
gen trash=1
save KPcounter_simul.dta, replace

foreach kp of local Reconci {
 if `kp'<Nlinks+1{
 append using KPcounter_simul_`kp' 
 erase KPcounter_simul_`kp'.dta
 }
 }
 
cap drop trash
cap drop if bench_RD==. 
save KPcounter_simul.dta, replace


use temp_name, clear
keep group DRCgov IDlink
sort group
save temp_name, replace
rename group group_d
rename DRCgov DRCgov_d
drop IDlink
sort group_d
save temp_name_d, replace

use avgbench_data, clear
keep Foreign Government_org group MCref name TotFight 
sort group
merge group using temp_name
tab _merge
drop _merge
sort IDlink
save MC_merging_key.dta, replace

use KPcounter_simul.dta, clear
sort IDlink
merge IDlink using MC_merging_key
count
tab _merge
drop _merge
sort IDlink
merge IDlink using enemy_DRCgov
tab _merge
gen  enemy_of_gov = (_merge==3)
drop _merge
cap drop Delta_RD
gen Delta_RD=((counter_RD /bench_RD)-1)
egen agg_fight=sum(TotFight)
compare agg_fight bench_RD
gen bench_fighting_share=TotFight/agg_fight
drop if DRCgov==1 
gsort Delta_RD
gen rank=[_n]
order rank name bench_fighting_share Delta_RD group 
keep rank name bench_fighting_share Delta_RD enemy_of_gov

label var name "Group"
label var bench_fighting_share "obs. share in aggregate fight."
label var Delta_RD  "count. change in aggregate fight."
label var rank "rank"
label var enemy_of_gov "Enemy of DRC gov." 
save bilateral_reconciliation_minusSD.dta, replace

use KRTZ_dyadic_ref, clear
sort group name group_d year
collapse (sum) Nfighting, by (group name group_d)
gen NNfighting=Nfighting
egen agg_fighting=sum(NNfighting)
sort group group_d
merge group using temp_name
tab _merge
drop _merge
keep if IDlink>0
sort group_d
merge group_d using temp_name_d
tab _merge
drop _merge
sort IDlink group DRCgov_d
collapse (sum) Nfighting (mean) agg_fighting , by(IDlink group name DRCgov_d)
gen bilateral_bench_fighting_share=Nfighting/agg_fighting
bysort group: egen bench_fighting_share_alt=sum(bilateral_bench_fighting_share)
keep if DRCgov_d==1
keep name IDlink group bilateral_bench_fighting_share bench_fighting_share_alt
sort name
save temp.dta, replace
use bilateral_reconciliation_minusSD, clear
sort name
merge name using temp
tab _merge
drop _merge
compare bench_fighting_share bench_fighting_share_alt
drop bench_fighting_share_alt
compare bilateral_bench_fighting_share bench_fighting_share
replace bilateral_bench_fighting_share = bench_fighting_share if bilateral_bench_fighting_share > bench_fighting_share
label var bilateral_bench_fighting_share "obs. bilateral share in aggregate fight."
order rank name bench_fighting_share bilateral_bench_fighting_share Delta_RD
keep  rank name bench_fighting_share bilateral_bench_fighting_share Delta_RD enemy_of_gov
gsort -bench_fighting_share
gen rank_fighting=[_n]
gen multiplier= abs(Delta_RD / bilateral_bench_fighting_share)
gsort -bilateral_bench_fighting_share
gen rank_bil_fighting=[_n]
egen share_gov_fighting=sum(bilateral_bench_fighting_share)
replace share_gov_fighting=bilateral_bench_fighting_share/share_gov_fighting
gsort -share_gov_fighting
sort rank
save bilateral_reconciliation_minusSD.dta, replace
**
use bilateral_reconciliation_minusSD.dta,clear
keep if enemy_of_gov==1
drop rank
gsort Delta_RD
gen rank=[_n]
save bilateral_reconciliation_minusSD.dta, replace

use bilateral_reconciliation_minusSD, clear
keep if name == "Military Forces of Rwanda (1994-1999)"|name=="Military Forces of Rwanda (2000-)"
collapse (sum) bench_fighting_share bilateral_bench_fighting_share share_gov_fighting (mean) Delta_RD enemy_of_gov
gen multiplier= abs(Delta_RD / bilateral_bench_fighting_share)
gen name = "Military Forces of Rwanda"
sort name
save tempRWA.dta, replace
append using  bilateral_reconciliation_minusSD
drop rank*
drop if name == "Military Forces of Rwanda (1994-1999)"|name=="Military Forces of Rwanda (2000-)"
gsort -bench_fighting_share
gen rank_fighting=[_n]
gsort -bilateral_bench_fighting_share
gen rank_bil_fighting=[_n]
gsort Delta_RD
gen rank=[_n]
order rank name bench_fighting_share rank_fighting bilateral_bench_fighting_share rank_bil_fighting Delta_RD
label var name "Group"
label var bench_fighting_share "obs. share in aggregate fight."
label var Delta_RD  "count. change in aggregate fight."
label var rank "rank"
label var enemy_of_gov "Enemy of DRC gov." 
label var bilateral_bench_fighting_share "obs. bilateral share in aggregate fight."
label var rank_fighting "rank obs. share in agg. fight"
label var rank_bil_fighting "rank obs. bil. share in agg. fight"
save bilateral_reconciliation_minusSD.dta, replace
keep name Delta_RD 

ren Delta_RD drd_minus
save t6_temp_minus.dta, replace 





use t6_temp.dta , clear
merge 1:1 name using t6_temp_plus.dta
drop _merge 
merge 1:1 name using t6_temp_minus.dta  
drop _merge 


gsort rank
drop if rank==.


ren drd_plus rdplus 
ren drd_minus rdminus
gen mplus = -rdplus/bilateral_bench_fighting_share 
gen mminus = -rdminus/bilateral_bench_fighting_share 



replace name="Rwanda" if rank==1
replace name="RCD-G" if rank==2
replace name="RCD-K" if rank==3
replace name="LRA" if rank==4
replace name="MLC" if rank==5
replace name="Uganda" if rank==6
replace name="UPC" if rank==7
replace name="Mutiny FARDC" if rank==8
replace name="CNDP" if rank==9
replace name="Lobala Mil." if rank==10
replace name="FPJC" if rank==11
replace name="FRPI" if rank==12
replace name="BDK" if rank==13
replace name="Munzaya Ethnic Mil." if rank==14
replace name="Enyele Ethnic Mil." if rank==15


foreach v of varlist Delta_RD rdplus rdminus {
   replace `v'=-round(`v',0.001)
}

foreach v of varlist multiplier mplus mminus {
   replace `v'= round(`v', 0.1)
}

cap drop drdpm
gen drdpm = "["+string(rdminus,"%9.3fc")+", "+string(rdplus,"%9.3fc")+"]"

cap drop mpm
gen mpm = "["+string(mminus,"%9.1fc")+", "+string(mplus,"%9.1fc")+"]"

foreach var of varlist bench_fighting_share bilateral_bench_fighting_share Delta_RD  {
   gen `var'x = string(`var', "%9.3f")
   drop `var'
   gen `var' = `var'x
   drop `var'x
}

gen mx  = string(multiplier, "%9.1fc")
drop multiplier 
gen multiplier=mx 
drop mx

label var name "Group"
label var bench_fighting_share "Sh. Fight."
label var Delta_RD "-\$\Delta \$ RD"
label var multiplier "Multipl."
label var drdpm "-\$ \Delta \$ RD (\$ \pm \$ 1SD)"
label var mpm "Multipl. ( \$ \pm \$ 1SD)"
label var bilateral_bench_fighting_share "Sh. Bilat. Fight."
ren bilateral_bench_fighting_share bbfs



texsave name  bench_fighting_share  bbfs Delta_RD multiplier drdpm mpm ///
using  "../replication_outputs/tables/t6a.tex", replace frag autonumber title("Table 6: Welfare effects of pacifying individual armed groups with the FARDC") ///
footnote("The computation of the counterfactual equilibrium is based on the baseline point estimates of column 4 in Table I. For each group, we report the observed share of total fighting involving this group (col. 1); the observed share of total fighting involving this group against the FARDC (col. 2); the counterfactual reduction in rent dissipation associated with its pacification (col. 3); a multiplier defined as the ratio of col. 3 over col. 2 (col. 4); the reduction in RD and its associated multiplier for a set of parameters equal to the baseline estimates $\pm$ 1 SD (cols. 5–6).") ///
varlabels nofix

 filefilter "../replication_outputs/tables/t6a.tex" "../replication_outputs/tables/t6.tex", ///
   replace  from("caption") to("caption*")

erase "../replication_outputs/tables/t6a.tex"


