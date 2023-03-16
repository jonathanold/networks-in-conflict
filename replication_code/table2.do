/*----------------------------------------------------*/
   /* [>   0.  Github integration   <] */ 
/*----------------------------------------------------*/
/* [> Commit and push any important changes to github regularly. <] */ 
/
cd ${github}
! git add "${github}/replication_code/table2.do"
! git commit -m "Started working on replication of Table 2. Problems found so far: Sample size restriction does not work because my2sl does not output e(sample)=."
! git push
*/
 /* [> New branch for testing new code <] */
 // git checkout -b name-of-branch


 
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



do "${main}/progs/nw2sls.do"
do "${main}/progs/nw2sls_partial.do"
do "${main}/progs/my_spatial_2sls.do"

do "${code}/my_spatial_2sls_JDO.do"

 
/*----------------------------------------------------*/
   /* [>   1.  Make exact replication table   <] */ 
/*----------------------------------------------------*/
/*
This table generates first-stage results

*/
use KRTZ_monadic_AF.dta, clear

/* [> Drop singletons in controls2 <] */ 
foreach v of varlist `controls2' {
       drop if `v'==1
        }












global lag_specif_ols "lag(1000000) dist(150) lagdist(1000000) "
global lag_specif "lag(1000000) dist(150) lagdist(1000000) partial "
global clus "r cl(id)" 
global controlsFE_reduced  "D96_* D30_* D41_* D471_*" 
global controlsFE "govern_* foreign_* unpopular_* D96_* D30_* D41_* D471_*" 


*Col 2 of Table 1
/* [> Mark sample <] */ 
qui ivreg2 TotFight meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1 $controlsFE_reduced TE* Dgroup* (TotFight_Enemy TotFight_Allied = rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1) 
gen e = e(sample)

qui my_spatial_2sls_jo `y' `controls1'  `fe2', `mys_syntax_iv'
local KPstat = e(KPstat)
local pValueHansen = e(pValueHansen)

my_spatial_2sls_jo TotFight_Enemy rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1 $controlsFE_reduced TE* Dgroup* if e(sample)==1, end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) $lag_specif_ols  
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = `KPstat'
estadd scalar HJ = `pValueHansen'
est sto t2_c1

my_spatial_2sls_jo TotFight_Allied rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1 $controlsFE_reduced TE* Dgroup* if e(sample)==1, end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) $lag_specif_ols  
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)
est sto t2_c2




*Col 3 of Table 1
use KRTZ_monadic_AF.dta, clear
ivreg2 TotFight meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1 $controlsFE TE* Dgroup* (TotFight_Enemy TotFight_Allied = rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0 rain_enemies_enemies0 sqrain_enemies_enemies0 rain_enemies_of_allies0 sqrain_enemies_of_allies0 rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 rain_enemies_enemies1 sqrain_enemies_enemies1 rain_enemies_of_allies1 sqrain_enemies_of_allies1)
gen e=e(sample)

qui my_spatial_2sls_jo `y' `controls1'  `controls3' `fe2', `mys_syntax_ivfull'
local KPstat = e(KPstat)
local pValueHansen = e(pValueHansen)

my_spatial_2sls_jo TotFight_Enemy rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0 rain_enemies_enemies0 sqrain_enemies_enemies0 rain_enemies_of_allies0 sqrain_enemies_of_allies0 rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 rain_enemies_enemies1 sqrain_enemies_enemies1 rain_enemies_of_allies1 sqrain_enemies_of_allies1 meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1 $controlsFE TE* Dgroup*, end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) $lag_specif_ols  
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)
est sto t2_c3

my_spatial_2sls_jo TotFight_Allied rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0 rain_enemies_enemies0 sqrain_enemies_enemies0 rain_enemies_of_allies0 sqrain_enemies_of_allies0 rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 rain_enemies_enemies1 sqrain_enemies_enemies1 rain_enemies_of_allies1 sqrain_enemies_of_allies1 meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1 $controlsFE TE* Dgroup*, end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) $lag_specif_ols  
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)
est sto t2_c4









#delimit ;
estout 
t2_c1 t2_c2 t2_c3 t2_c4 t2_c1 t2_c2
using "../replication_outputs/tables/t2.tex" , style(tex) 
eqlabels(" " " ") 
wrap varwidth(45) 
varlabels(rain_enemies1 "\textbf{Rain (t-1) Enem.}" sqrain_enemies1 "Sq. Rain (t-1) Enem." rain_allies1 "\textbf{Rain (t-1) All.}" sqrain_allies1 "Sq. Rain (t-1) All."
         rain_enemies0 "\textbf{Current Rain Enem.}" sqrain_enemies0 "Sq. Curr. Rain Enem." rain_allies0 "\textbf{Current Rain All.}" sqrain_allies0 "Sq. Curr. Rain All.")
keep(`iv_reduced'  rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0)
order(`iv_reduced'  rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0)
        cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(KP HJ N r2   ,
                fmt(%9.2fc %9.2fc %9.0fc %9.3fc)
                labels("\midrule \addlinespace Kleibergen-Paap F-stat" "Hansen J" "Observations" "R-squared"))
starlevels(* 0.1 ** 0.05 *** 0.01) 
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr   




stop



*Col 4 of Table 1
use KRTZ_monadic_AF.dta, clear
cap drop e
my_spatial_2sls TotFight meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1 Dgroup* $controlsFE, end(TotFight_Enemy TotFight_Allied TotFight_Neutral) iv(rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0 rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 rain_enemies_enemies0 sqrain_enemies_enemies0 rain_enemies_of_allies0 sqrain_enemies_of_allies0 rain_enemies_enemies1 sqrain_enemies_enemies1 rain_enemies_of_allies1 sqrain_enemies_of_allies1 rain_neutral0 sqrain_neutral0 rain_neutral1 sqrain_neutral1) latitude(latitude) longitude(longitude) id(group) time(year) $lag_specif 
gen e=e(sample)
keep if e~=.
eststo: my_spatial_2sls TotFight_Enemy rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0 rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 rain_enemies_enemies0 sqrain_enemies_enemies0 rain_enemies_of_allies0 sqrain_enemies_of_allies0 rain_enemies_enemies1 sqrain_enemies_enemies1 rain_enemies_of_allies1 sqrain_enemies_of_allies1 rain_neutral0 sqrain_neutral0 rain_neutral1 sqrain_neutral1 meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1 Dgroup* $controlsFE, end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) $lag_specif_ols  
eststo: my_spatial_2sls TotFight_Allied rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0 rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 rain_enemies_enemies0 sqrain_enemies_enemies0 rain_enemies_of_allies0 sqrain_enemies_of_allies0 rain_enemies_enemies1 sqrain_enemies_enemies1 rain_enemies_of_allies1 sqrain_enemies_of_allies1 rain_neutral0 sqrain_neutral0 rain_neutral1 sqrain_neutral1 meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1 Dgroup* $controlsFE, end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) $lag_specif_ols  

log using ../results/Table2.txt, text replace
set linesize 150
esttab, keep(rain_enemies0 sqrain_enemies0 rain_allies0 sqrain_allies0 rain_enemies_enemies0 sqrain_enemies_enemies0 rain_enemies_of_allies0 sqrain_enemies_of_allies0 rain_enemies1 sqrain_enemies1 rain_allies1 sqrain_allies1 rain_enemies_enemies1 sqrain_enemies_enemies1 rain_enemies_of_allies1 sqrain_enemies_of_allies1 meanc_rain0 sqmeanc_rain0 meanc_rain1 sqmeanc_rain1 rain_neutral0 sqrain_neutral0 rain_neutral1 sqrain_neutral1) pr2 r2 starlevels(* 0.1 ** 0.05 *** 0.01)  b(%4.3f) se(%4.3f) label scalars(meanprob) nogaps nolines nodepvars
log close
eststo clear












