/*----------------------------------------------------*/
   /* [>   0.  Github integration   <] */ 
/*----------------------------------------------------*/
/* [> Commit and push any important changes to github regularly. <] */ 
/*
cd "${github}"
! git add "${github}/replication_code/table2.do"
! git commit -m "Changed syntax to my syntax with locals for groups of variables."
! git push
*/

 /* [> New branch for testing new code <] */
 // git checkout -b name-of-branch

cd "${main}/regressions"
 
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

*/




*Col 2 of Table 1
/* [> Mark sample <] */ 
qui ivreg2 `y' `controls1' TE* Dgroup* (`x' = `iv_reduced') 
gen e = e(sample)

qui my_spatial_2sls_jo `y' `controls1'  `fe2', `mys_syntax_iv'
local KPstat = e(KPstat)
local pValueHansen = e(pValueHansen)

my_spatial_2sls_jo TotFight_Enemy `iv_reduced' `controls1' TE* Dgroup* if e(sample)==1, end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) `lag_specif_ols'  
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = `KPstat'
estadd scalar HJ = `pValueHansen'
est sto t2_c1


my_spatial_2sls_jo TotFight_Allied `iv_reduced' `controls1' TE* Dgroup* if e(sample)==1, end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) `lag_specif_ols'  
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = `KPstat'
estadd scalar HJ = `pValueHansen'
est sto t2_c2

drop e


*Col 3 of Table 1
qui ivreg2 `y' `controls1' `controls3' TE* Dgroup* (`x' = `iv_full')
gen e=e(sample)

qui my_spatial_2sls_jo `y' `controls1'  `controls3' `fe2', `mys_syntax_ivfull'
local KPstat = e(KPstat)
local pValueHansen = e(pValueHansen)

my_spatial_2sls_jo TotFight_Enemy `iv_full' `controls1' `controls3' TE* Dgroup* if e==1, end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) `lag_specif_ols'  
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = `KPstat'
estadd scalar HJ = `pValueHansen'
est sto t2_c3

my_spatial_2sls_jo TotFight_Allied `iv_full' `controls1' `controls3' TE* Dgroup* if e==1, end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) `lag_specif_ols'  
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = `KPstat'
estadd scalar HJ = `pValueHansen'
est sto t2_c4

drop e





*Col 4 of Table 1
qui ivreg2 `y' `controls1' $controlsFE TE* Dgroup* (`x' `n' = `iv_full_neutral')
gen e=e(sample)

qui my_spatial_2sls_jo `y' `controls1' `controls3' Dgroup*, `mys_syntax_ivneutral'
local KPstat = e(KPstat)
local pValueHansen = e(pValueHansen)

my_spatial_2sls_jo TotFight_Enemy `iv_full_neutral' `controls1' Dgroup* `controls3' if e==1, end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) `lag_specif_ols' 
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = `KPstat'
estadd scalar HJ = `pValueHansen'
est sto t2_c5

my_spatial_2sls_jo TotFight_Allied `iv_full_neutral' `controls1' Dgroup* `controls3' if e==1, end() iv( ) latitude(latitude) longitude(longitude) id(group) time(year) `lag_specif_ols' 
predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd scalar KP = `KPstat'
estadd scalar HJ = `pValueHansen'
est sto t2_c6

drop e



#delimit ;
estout 
t2_c1 t2_c2 t2_c3 t2_c4 t2_c5 t2_c6
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
                labels("\midrule \addlinespace Kleibergen-Paap F-stat" "Hansen J (p-value)" "Observations" "R-squared"))
starlevels(* 0.1 ** 0.05 *** 0.01) 
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr   



import delim "../replication_outputs/tables/t2.tex", delim("&") clear

replace v2 = " \cellcolor{black!15} \textbf{"+v2+"}" if _n==1 | _n==2
replace v3 = " \cellcolor{black!15} \textbf{"+v3+"}" if _n==5 | _n==6 

replace v4 = " \cellcolor{black!15} \textbf{"+v4+"}" if _n==1 | _n==2 | _n==9 | _n==10
replace v5 = " \cellcolor{black!15} \textbf{"+v5+"}" if _n==5 | _n==6 | _n==13 | _n==14
replace v6 = " \cellcolor{black!15} \textbf{"+v6+"}" if _n==1 | _n==2 | _n==9 | _n==10
replace v7 = " \cellcolor{black!15} \textbf{"+v7+"}" if _n==5 | _n==6 | _n==13 | _n==14

replace v7 = subinstr(v7,"\\}", "}\\",1)

export delim "../replication_outputs/tables/t2.tex", delim("&") novarnames replace
















