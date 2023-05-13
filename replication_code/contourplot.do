
set scheme mine

clear
set obs 101
gen beta = .
local t=0
forv i = 0(0.01)1.0001 { 
	local t = `t'+1
replace beta = `i' if `t'==_n
}
expand 101 
gsort beta 
gen gamma = .

egen g = seq() , from(0) to(100)
replace gamma = g/100
drop g

gen gmin = .
tempfile x 
save `x'
save raster.dta, replace 


import delim using "./data_reduced_2010.csv", clear
qui {
forv b=0(0.01)1.001 {
	forv g=0(0.01)1.001 {
		preserve 
		gen gammai = 1+`b'*degree_plus-`g'*degree_minus 
		sum gammai 
		use `x', clear
		replace  gmin = r(min) if beta>=`b'-0.001 & beta<=`b'+0.001 & gamma>=`g'-0.001 & gamma<=`g'+0.001
		save `x', replace
		restore 
	}
}
}

use `x', clear
save contour_identificationA.dta, replace 
use  contour_identificationA.dta, clear

replace gmin = -0.2 if gmin<=-1 & gmin>=-1000000
replace gmin = -0.4 if gmin==. | gmin<=-10000 | gmin>=100000

label var gmin "Minimum of {&Gamma}i"



gr tw (contour gmin beta gamma if beta>=0 & beta<=0.3011 & gamma>=0 & gamma<=0.3011, ccuts(-0.4(0.2)1) ccol(red dkorange gold ebg eltblue ebblue edkblue dknavy )) ///
	 (scatteri 0.114 0 0.114 0.3, lcol(black) recast(line)) (scatteri 0 0.083 0.3 0.083, lcol(black) recast(line)), ///
	 legend(off) xti("{&gamma}") yti("{&beta}") ti("DRC")

graph export ../replication_outputs/figures/countourplot_id_zoom.pdf, replace 



gr tw (contour gmin beta gamma, ccuts(-0.4(0.2)1) ccol(red dkorange gold ebg eltblue ebblue edkblue dknavy )) ///
	 (scatteri 0.114 0 0.114 1, lcol(black) recast(line)) (scatteri 0 0.083 1 0.083, lcol(black) recast(line)), ///
	 legend(off) xti("{&gamma}") yti("{&beta}") ti("DRC")

graph export ../replication_outputs/figures/countourplot_id.pdf, replace 




foreach case in afghanistan syria yemen myanmar iraq {

use raster.dta, clear
tempfile x
save `x', replace  


import delim using "./data_`case'.csv", clear
qui {
forv b=0(0.01)1.001 {
	forv g=0(0.01)1.001 {
		preserve 
		gen gammai = 1+`b'*degree_plus-`g'*degree_minus 
		sum gammai 
		use `x', clear
		replace  gmin = r(min) if beta>=`b'-0.001 & beta<=`b'+0.001 & gamma>=`g'-0.001 & gamma<=`g'+0.001
		save `x', replace
		restore 
	}
}
}

use `x', clear



replace gmin = -0.2 if gmin<=-1 & gmin>=-1000000
replace gmin = -0.4 if gmin==. | gmin<=-10000 | gmin>=100000

label var gmin "Minimum of {&Gamma}i"

local ti: di proper("`case'")


gr tw (contour gmin beta gamma if beta>=0 & beta<=0.3011 & gamma>=0 & gamma<=0.3011, ccuts(-0.4(0.2)1) ccol(red dkorange gold ebg eltblue ebblue edkblue dknavy )) ///
	 (scatteri 0.114 0 0.114 0.3, lcol(black) recast(line)) (scatteri 0 0.083 0.3 0.083, lcol(black) recast(line)), ///
	 legend(off) xti("{&gamma}") yti("{&beta}")  ti(`ti')

graph export ../replication_outputs/figures/countourplot_id_zoom_`case'.pdf, replace 

gr tw (contour gmin beta gamma, ccuts(-0.4(0.2)1) ccol(red dkorange gold ebg eltblue ebblue edkblue dknavy )) ///
	 (scatteri 0.114 0 0.114 1, lcol(black) recast(line)) (scatteri 0 0.083 1 0.083, lcol(black) recast(line)), ///
	 legend(off) xti("{&gamma}") yti("{&beta}") ti(`ti')

graph export ../replication_outputs/figures/countourplot_id_`case'.pdf, replace 



}








