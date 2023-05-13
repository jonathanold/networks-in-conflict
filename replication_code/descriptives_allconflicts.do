clear 
set obs 6 
gen name=""
gen n=. 
gen avge=.
gen avga=. 
gen avgevents=.
gen dens=.
gen katz=.

gen beta=. 
gen gamma=.

gen beta_nlls=.
gen gamma_nlls=.

tempfile x 
save `x'



local case Congo 
import delim data_panel_for_matlab.csv, clear
collapse (sum) totfight (firstnm) degree_minus degree_plus, by(group)
count
	local n=r(N)
	sum degree_minus
	local avge=r(mean)
	sum degree_plus
	local avga=r(mean)
	sum totfight
	local avgevents=r(mean)


local i=1


use "/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Networks in Conflict/regressions/KRTZ_dyadic_AF.dta", clear
keep if year==2000
gen dir=enemy+allied
keep group group_d name name_d dir

gen namex = substr(name,1,10)
gen namex_d = substr(name_d,1,10)

nwset name name_d dir, edgelist labs(namex namex_d) labsfromvar(namex namex_d) keeporiginal

		nwsummarize 
		local dens=r(density)

		nwkatz, alpha(0.5)
		local katz=r(mean)
		nwdrop



import delim "matlab_drc.csv", clear 
sum v1 
local b1 = r(mean)
sum v2 
local g1 = r(mean)
sum v3 
local b2=r(mean)
sum v4
local g2=r(mean)




use `x', clear
replace name = proper("`case'") if _n==`i'
replace n = `n' if _n==`i'
replace avga = `avga' if _n==`i'
replace avge = `avge' if _n==`i'
replace avgevents = `avgevents' if _n==`i'
replace dens = `dens' if _n==`i'
replace katz = `katz' if _n==`i'

replace  beta=`b1'  if _n==`i'
replace  gamma=`g1' if _n==`i'

replace  beta_nlls=`b2' if _n==`i'
replace  gamma_nlls=`g2' if _n==`i'

save `x', replace 






foreach case in afghanistan myanmar iraq syria yemen {
	local i=`i'+1
	import delim data_`case'.csv, clear
	count
	local n=r(N)
	sum enemy
	local avge=r(mean)
	sum allied 
	local avga=r(mean)
	sum tf_events 
	local avgevents=r(mean)


		use `case'_dyad.dta, replace 
		keep idA idB allied enemy 
		gen rel = allied+enemy
		nwset idA idB rel, edgelist labs(namex namex_d) labsfromvar(namex namex_d) keeporiginal directed

		nwsummarize 
		local dens=r(density)

		nwkatz, alpha(0.5)
		local katz=r(mean)
		nwdrop

use `x', clear
replace name = proper("`case'") if _n==`i'
replace n = `n' if _n==`i'
replace avga = `avga' if _n==`i'
replace avge = `avge' if _n==`i'
replace avgevents = `avgevents' if _n==`i'
replace dens = `dens' if _n==`i'
replace katz = `katz' if _n==`i'

save `x', replace 

import delim "matlab_`case'.csv", clear 
sum v1 
local b1 = r(mean)
sum v2 
local g1 = r(mean)
sum v3 
local b2=r(mean)
sum v4
local g2=r(mean)

use `x', clear
replace  beta=`b1'  if _n==`i'
replace  gamma=`g1' if _n==`i'

replace  beta_nlls=`b2' if _n==`i'
replace  gamma_nlls=`g2' if _n==`i'

save `x', replace 


import delim "matlab_afghanistan.csv", clear 

}

use `x', clear







foreach var of varlist avge avga avgevents dens katz {
   gen `var'x = string(`var', "%9.2f")
   drop `var'
   gen `var' = `var'x
   drop `var'x
}





foreach var of varlist beta gamma beta_nlls gamma_nlls {
   gen `var'x = string(`var', "%9.3f")
   drop `var'
   gen `var' = `var'x
   drop `var'x
}

label var name "Conflict"
label var n "Network size"
label var avge "Avg. \# Enemies"
label var avga "Avg. \# Allies"
label var avgevents "Avg. \# Events"
label var dens "Network density"
label var katz "Avg. K-B Centrality"
label var beta "$\hat{\beta}_{MD}$"
label var gamma "$\hat{\gamma}_{MD}$"
label var beta_nlls "$\hat{\beta}_{NLLS}$"
label var gamma_nlls "$\hat{\gamma}_{NLLS}$"



texsave name n avge avga avgevents dens katz beta gamma beta_nlls gamma_nlls ///
using  "../replication_outputs/tables/desca.tex", replace frag autonumber title("Descriptive network statistics from six conflicts") ///
footnote("Network density is the average number of links per group. K-B Centrality is Katz-Bonacich Centrality with penalty 0.5. Regression results from minimum distance (MD) and Non-Linear Least Squares (NLLS) estimation.") ///
varlabels nofix label(tab:desc)










