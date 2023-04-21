/*----------------------------------------------------*/
   /* [>   0.  Github integration   <] */ 
/*----------------------------------------------------*/
/* [> Commit and push any important changes to github regularly. <] */ 
/*
cd "${github}"
! git add "${github}/replication_code/table1.do"
! git commit -m "Added code to check concavity of problem (condition 3 of paper)."
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

use KRTZ_monadic_AF.dta, clear

/* [> Drop singletons in controls2 <]  */
foreach v of varlist `controls2' {
       drop if `v'==1
        }
*/ 


my_spatial_2sls_jo `y' `x' `controls1'  `fe2', `mys_syntax'
        predict p , xb 
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        count if !mi(p)
        local xx=r(N)
        qui gen gammai = 1/(1+_b[TotFight_Enemy]*degree_minus+_b[TotFight_Allied]*degree_plus)
        count if gammai<0
        local yy=r(N)
        estadd scalar ming=`yy'/`xx'
        qui sum gammai
            if r(min)<0 {
                estadd local scond "No"
            }
            else if r(min)>0 {
                estadd local scond "Yes"
            }
        drop p gammai

        estadd local controls = "Reduced"
        estadd local estimator = "OLS"
        estadd local software = "KRTZ"
        estadd local iv = "N/A"
        estadd local KP = "N/A"
        estadd local HJ = "N/A"

est sto t1_c1





my_spatial_2sls_jo `y' `controls1'  `fe2', `mys_syntax_iv'
        predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        count if !mi(p)
        local xx=r(N)
        qui gen gammai = 1/(1+_b[TotFight_Enemy]*degree_minus+_b[TotFight_Allied]*degree_plus)
        count if gammai<0
        local yy=r(N)
        estadd scalar ming=`yy'/`xx'
        qui sum gammai
            if r(min)<0 {
                estadd local scond "No"
            }
            else if r(min)>0 {
                estadd local scond "Yes"
            }
        drop p gammai

estadd local controls = "Reduced"
estadd local estimator = "IV"
estadd local software = "KRTZ"
estadd local iv = "Restricted"
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)

est sto t1_c2






my_spatial_2sls_jo `y' `controls1'  `controls3' `fe2', `mys_syntax_ivfull'
        predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        count if !mi(p)
        local xx=r(N)
        qui gen gammai = 1/(1+_b[TotFight_Enemy]*degree_minus+_b[TotFight_Allied]*degree_plus)
        count if gammai<0
        local yy=r(N)
        estadd scalar ming=`yy'/`xx'
        qui sum gammai
            if r(min)<0 {
                estadd local scond "No"
            }
            else if r(min)>0 {
                estadd local scond "Yes"
            }
        drop p gammai
estadd local controls = "Full"
estadd local estimator = "IV"
estadd local software = "KRTZ"
estadd local iv = "Full"
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)

est sto t1_c3



my_spatial_2sls_jo `y' `controls1' `controls3' Dgroup*, `mys_syntax_ivneutral'
        predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        count if !mi(p)
        local xx=r(N)
        qui gen gammai = 1/(1+_b[TotFight_Enemy]*degree_minus+_b[TotFight_Allied]*degree_plus)
        count if gammai<0
        local yy=r(N)
        estadd scalar ming=`yy'/`xx'
        qui sum gammai
            if r(min)<0 {
                estadd local scond "No"
            }
            else if r(min)>0 {
                estadd local scond "Yes"
            }
        drop p gammai
estadd local controls = "Full"
estadd local estimator = "IV"
estadd local software = "KRTZ"
estadd local iv = "Full"
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)

est sto t1_c4



preserve 
use temp_battle, clear

/* [> Drop singletons in controls2 <] */ 
foreach v of varlist `controls2' {
       drop if `v'==1
        }

my_spatial_2sls_jo `y' `controls1' `controls3' Dgroup*, `mys_syntax_ivneutral'

        predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        count if !mi(p)
        local xx=r(N)
        qui gen gammai = 1/(1+_b[TotFight_Enemy]*degree_minus+_b[TotFight_Allied]*degree_plus)
        count if gammai<0
        local yy=r(N)
        estadd scalar ming=`yy'/`xx'
        qui sum gammai
            if r(min)<0 {
                estadd local scond "No"
            }
            else if r(min)>0 {
                estadd local scond "Yes"
            }
        drop p gammai
estadd local controls = "Full"
estadd local estimator = "IV"
estadd local software = "KRTZ"
estadd local iv = "Full"
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)

est sto t1_c5

restore 



* Col 6 - Only with d+>0 & d->0
keep if degree_minus>0
keep if degree_plus>0


my_spatial_2sls_jo `y' `controls1' `controls3' Dgroup*, `mys_syntax_ivneutral'
        predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        count if !mi(p)
        local xx=r(N)
        qui gen gammai = 1/(1+_b[TotFight_Enemy]*degree_minus+_b[TotFight_Allied]*degree_plus)
        count if gammai<0
        local yy=r(N)
        estadd scalar ming=`yy'/`xx'
        qui sum gammai
            if r(min)<0 {
                estadd local scond "No"
            }
            else if r(min)>0 {
                estadd local scond "Yes"
            }
        drop p gammai
estadd local controls = "Full"
estadd local estimator = "IV"
estadd local software = "KRTZ"
estadd local iv = "Full"
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)

est sto t1_c6



use temp_ged_coord.dta, clear
/* [> Drop singletons in controls2 <] */ 
foreach v of varlist `controls2' {
       drop if `v'==1
        }


my_spatial_2sls_jo `y' `controls1' `controls3' Dgroup*, `mys_syntax_ivneutral'
        predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        count if !mi(p)
        local xx=r(N)
        qui gen gammai = 1/(1+_b[TotFight_Enemy]*degree_minus+_b[TotFight_Allied]*degree_plus)
        count if gammai<0
        local yy=r(N)
        estadd scalar ming=`yy'/`xx'
        qui sum gammai
            if r(min)<0 {
                estadd local scond "No"
            }
            else if r(min)>0 {
                estadd local scond "Yes"
            }
        drop p gammai
estadd local controls = "Full"
estadd local estimator = "IV"
estadd local software = "KRTZ"
estadd local iv = "Full"
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)

est sto t1_c7


use temp_superdataset.dta, clear
foreach v of varlist `controls2' {
       drop if `v'==1
        }


my_spatial_2sls_jo `y' `controls1' `controls3' Dgroup*, `mys_syntax_ivneutral'
        predict p, xb
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        count if !mi(p)
        local xx=r(N)
        qui gen gammai = 1/(1+_b[TotFight_Enemy]*degree_minus+_b[TotFight_Allied]*degree_plus)
        count if gammai<0
        local yy=r(N)
        estadd scalar ming=`yy'/`xx'
        qui sum gammai
            if r(min)<0 {
                estadd local scond "No"
            }
            else if r(min)>0 {
                estadd local scond "Yes"
            }
        drop p gammai

estadd local controls = "Full"
estadd local estimator = "IV"
estadd local software = "KRTZ"
estadd local iv = "Full"
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)

est sto t1_c8






#delimit ;
estout 
t1_c1 t1_c2 t1_c3 t1_c4  t1_c5 t1_c6  t1_c7 t1_c8
using "../replication_outputs/tables/t1.tex" , style(tex) 
eqlabels(" " " ") 
wrap varwidth(45) 
varlabels(TotFight_Enemy "Enemies (TFE)" TotFight_Allied "Allies (TFA)" TotFight_Neutral "Neutral (TFN)")
keep(`x' `n')
order(`x' `n')
        cells(b(star fmt(%9.3f)) se(par)) 
 hlinechar("{hline @1}")
stats(controls estimator software iv KP HJ N r2 ming scond  ,
                fmt(%9.3fc %9.3fc %9.3fc %9.3fc %9.2fc %9.2fc %9.0fc %9.3fc %9.3fc %9.3fc)
                labels("\midrule \addlinespace Add. Controls" "Estimator" "Software" "Instrum. Var." "Kleibergen-Paap F-stat" "Hansen J (p-value)" "Observations" "R-squared" "\midrule Share $\Gamma_i<0$" "Concavity holds"))
starlevels(* 0.1 ** 0.05 *** 0.01) 
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr   


stop








 
/*----------------------------------------------------*/
   /* [>   2.  Table 1, robustness of OLS results to software   <] */ 
/*----------------------------------------------------*/
use KRTZ_monadic_AF.dta, clear

/* [> Drop singletons in controls2 <] */ 
foreach v of varlist `controls2' {
       drop if `v'==1
        }


xi: my_spatial_2sls_jo `y' `x' `controls1'  `fe', `mys_syntax'
        cap drop p
        predict p , xb 
        corr `y' p  
        estadd scalar r2 = r(rho)^2
        drop p
estadd local controls = "Reduced"
estadd local estimator = "OLS"
estadd local software = "KRTZ"
estadd local iv = "N/A"
estadd local KP = "N/A"
estadd local HJ = "N/A"
est sto t1_c1_mys

reg2hdfespatial  `y' `x' `controls1'   , `fet_syntax'
estadd local controls = "Reduced"
estadd local estimator = "OLS"
estadd local software = "Fetzer"
estadd local iv = "N/A"
estadd local KP = "N/A"
estadd local HJ = "N/A"
est sto t1_c1_fet

spatial_hac_iv `y' `x' `controls1'   `fe', `shi_syntax'
estadd local controls = "Reduced"
estadd local estimator = "OLS"
estadd local software = "Foreman"
estadd local iv = "N/A"
estadd local KP = "N/A"
estadd local HJ = "N/A"
est sto t1_c1_shi

acreg `y' `x' `controls1'   `fe', `ac1_syntax'
estadd local controls = "Reduced"
estadd local estimator = "OLS"
estadd local software = "CLST 1"
estadd local iv = "N/A"
estadd local KP = "N/A"
estadd local HJ = "N/A"
est sto t1_c1_ac1

acreg `y' `x' `controls1'  , `ac2_syntax'
estadd local controls = "Reduced"
estadd local estimator = "OLS"
estadd local software = "CLST 2"
estadd local iv = "N/A"
estadd local KP = "N/A"
estadd local HJ = "N/A"
est sto t1_c1_ac2



#delimit ;
estout 
t1_c1_mys t1_c1_fet t1_c1_shi t1_c1_ac1 t1_c1_ac2
using "../replication_outputs/tables/t1_robustness.tex" , style(tex) 
eqlabels(" " " ") 
wrap varwidth(45) 
varlabels(TotFight_Enemy "Enemies (TFE)" TotFight_Allied "Allies (TFA)")
keep(`x')
order(`x')
        cells(b(star fmt(%9.4f)) se(par)) 
 hlinechar("{hline @1}")
stats(controls estimator software iv KP HJ N r2   ,
                fmt(%9.3fc %9.3fc %9.3fc %9.3fc %9.3fc %9.3fc %9.0fc %9.3fc)
                labels("\midrule \addlinespace Add. Controls" "Estimator" "Software" "Instrum. Var." "Kleibergen-Paap F-stat" "Hansen J (p-value)" "Observations" "R-squared"))
starlevels(* 0.1 ** 0.05 *** 0.01) 
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr   





 
/*----------------------------------------------------*/
   /* [>   3.  Table 1, robustness of column (4) to alternative software   <] */ 
/*----------------------------------------------------*/

use KRTZ_monadic_AF.dta, clear

/* [> Drop singletons in controls2 <] */ 
foreach v of varlist `controls2' {
       drop if `v'==1
        }


my_spatial_2sls_jo `y' `controls1' `controls3' Dgroup*, `mys_syntax_ivneutral'

predict p, xb
corr `y' p  
estadd scalar r2 = r(rho)^2
drop p
estadd local controls = "Full"
estadd local estimator = "IV"
estadd local software = "KRTZ"
estadd local iv = "Full"
estadd scalar KP = e(KPstat)
estadd scalar HJ = e(pValueHansen)
est sto t1_c4a 


acreg `y'  `controls1' `controls3' (`x' `n' = `iv_full_neutral')  , latitude(latitude) longitude(longitude) id(group) time(year) spatial lag(1000000) dist(150)  pfe1(group) dropsingletons

estadd local controls = "Full"
estadd local estimator = "IV"
estadd local software = "CLST"
estadd local iv = "Full"
estadd scalar KP = e(widstat)
estadd local HJ = ""
est sto t1_c4b



acreg `y'  `controls1' `controls3' i.group (`x' `n' = `iv_full_neutral')  , latitude(latitude) longitude(longitude) id(group) time(year) spatial lag(1000000) dist(150)

estadd local controls = "Full"
estadd local estimator = "IV"
estadd local software = "CLST 2"
estadd local iv = "Full"
estadd scalar KP = e(widstat)
estadd local HJ = ""
est sto t1_c4c





spatial_hac_iv  `y'  `controls1' `controls3' i.group (`x' `n' = `iv_full_neutral')  , lat(latitude) lon(longitude) panelvar(group) timevar(year) lagcutoff(1000000) distcutoff(150)
estadd local controls = "Full"
estadd local estimator = "IV"
estadd local software = "Foreman"
estadd local iv = "Full"
        local kp2 : display %9.3fc e(widstat)
estadd scalar KP = `kp2'
estadd local HJ = "N/A"
est sto t1_c4d




#delimit ;
estout 
t1_c4 t1_c4b t1_c4c t1_c4d
using "../replication_outputs/tables/t1_c4_robustness.tex" , style(tex) 
eqlabels(" " " ") 
wrap varwidth(45) 
varlabels(TotFight_Enemy "Enemies (TFE)" TotFight_Allied "Allies (TFA)" TotFight_Neutral "Neutral (TFN)")
keep(`x' `n')
order(`x' `n')
        cells(b(star fmt(%9.4f)) se(par)) 
 hlinechar("{hline @1}")
stats(controls estimator software iv KP HJ N r2   ,
                fmt(%9.3fc %9.3fc %9.3fc %9.3fc %9.3fc %9.3fc %9.0fc %9.3fc)
                labels("\midrule \addlinespace Add. Controls" "Estimator" "Software" "Instrum. Var." "Kleibergen-Paap F-stat" "Hansen J (p-value)" "Observations" "R-squared"))
starlevels(* 0.1 ** 0.05 *** 0.01) 
nolabel replace collabels(none) mlabels(none)
note("\bottomrule")
  ; 
#delimit cr   


 







