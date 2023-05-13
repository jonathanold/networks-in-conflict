// Replicate using other conflicts

import delim using "../data_acled/1900-01-01-2023-05-11-Syria.csv", clear

/* [> Drop if individual rioters/protestors/civilians involved <] */ 
forv i=1/2 {
	replace actor`i'="Iran" if substr(actor`i',1,23)=="Military Forces of Iran"
	replace actor`i'="Iraq" if substr(actor`i',1,23)=="Military Forces of Iraq"
	replace actor`i'="Israel" if substr(actor`i',1,23)=="Military Forces of Isra"
	replace actor`i'="Russia" if substr(actor`i',1,23)=="Military Forces of Russ"
	replace actor`i'="Syria" if substr(actor`i',1,23)=="Military Forces of Syri"
	replace actor`i'="Turkey" if substr(actor`i',1,23)=="Military Forces of Turk"
	replace actor`i'="Police" if substr(actor`i',1,6)=="Police"
}



compress
recast str150 actor1 actor2
compress 
ren assoc_actor_1 assoc_actor_a
ren assoc_actor_2 assoc_actor_b

split assoc_actor_a, p("; ")
local an = r(k_new)
split assoc_actor_b, p("; ")
local bn = r(k_new)
multencode actor1 actor2 		assoc_actor_a1-assoc_actor_a`an' assoc_actor_b1-assoc_actor_b`bn', ///
		gen(actor_aX0 actor_bX0 	assoc_actor_aX1-assoc_actor_aX`an' assoc_actor_bX1-assoc_actor_bX`bn')

ren assoc_* *

order  actor_aX* actor_bX* 



ren actor_aX* actorA# , renumber

ren actor_bX* actorB#, renumber 

save syria_complete.dta, replace 

/* [> Drop if individual rioters/protestors/civilians involved <] */ 
forv i=1/2 {
	drop if inlist(inter`i',5,6,7)
	drop if substr(actor`i',1,8)=="Unidenti"
}


gen one = 1 
bys actor1: egen n_act1 = count(one)
count 
tempfile x 
save `x'
collapse (sum) one, by(actor2)
ren actor2 actor1
ren one n_act2
merge 1:m actor1 using `x'
drop if _merge==1 
drop _merge 

drop if (n_act1<=5 & n_act2<=5) | (n_act1<=5 & n_act2==.) | (n_act1==. & n_act2<=5)

bys actor2: egen n_act2b = count(one)
count 
tempfile y 
save `y'
collapse (sum) one, by(actor1)
ren actor1 actor2
ren one n_act1b
merge 1:m actor2 using `y'
drop if _merge==1 
drop if (n_act1b<=5 & n_act2b<=5) | (n_act1b<=5 & n_act2b==.) | (n_act1b==. & n_act2b<=5)
drop _merge


save syria_temp.dta, replace 


merge 1:1 event_id_cnty using syria_complete.dta
drop _merge 
drop actor_*
save syria_matched.dta, replace 


 
/*----------------------------------------------------*/
   /* [>   Generate total fighting efforts for all groups   <] */ 
/*----------------------------------------------------*/
use syria_matched.dta, replace 
local nmax = max(`an', `bn')
forv i=1/`nmax' {
	foreach n in A B {
		preserve
		cap drop if missing(actor`n'`i')
		if _rc==0 {
		ren actor`n'`i' idA
		collapse (sum) one fatalities, by(idA)
		ren one one`n'`i'
		ren fatalities fat`n'`i'
		tempfile a`n'`i'
		save `a`n'`i'', replace 
	}
	else{
	}
		restore
		}
	}

use `aA1', replace 
forv i=1/`nmax' {
	foreach n in A B {
		if "`i'`n'"=="A1" {
			}
		else {
		cap merge 1:1 idA using `a`n'`i''
		cap drop _merge 
		}
		}
	}


foreach var of varlist _all {
	replace `var'=0 if `var'==.
}

egen tf_events = rowtotal(one*)
egen tf_fatalities = rowtotal(fat*)

keep idA tf* 
save tf_syria.dta, replace 






use syria_temp.dta, replace 

preserve 
/* [> Genrate dataset of dyads <] */ 

collapse (mean) one, by(actorA1)
drop one 
ren actorA1 actorAX
drop if missing(actorAX)
tempfile x
save `x'

restore
collapse (mean) one, by(actorB1)
drop one 
ren actorB1 actorBX
drop if missing(actorBX)
tempfile y
save `y'

cross using `x'
tempfile dyad 
save `dyad', replace 




use `x' 
ren actorAX actorBX
save `x', replace 

use `y'
ren actorBX actorAX 
save `y', replace 
cross using `x'

append using `dyad'
save `dyad', replace 

use `x'
ren actorBX actorAX
cross using `x'
append using `dyad'
save `dyad', replace 

use `y'
ren actorAX actorBX
cross using `y'
append using `dyad'

duplicates drop
count 
di sqrt(r(N))
save `dyad', replace
save dyad_orig.dta , replace 

/*
local z = `z'+1
	local i = 3 
	local j=1
	ren actorA`i' actorAX 
	ren actorB`j' actorBX 
merge m:1 actorAX actorBX using dyad.dta, keep(3)
*/

use syria_matched.dta, replace
local z=0
forv i=1/`an' {
	forv j=1/`bn' {
		qui {
		preserve 
		
		ren actorA`i' actorAX 
		ren actorB`j' actorBX 

		merge m:1 actorAX actorBX using `dyad', keep(2 3)

		capture collapse (sum) one fatalities, by(actorAX actorBX)
		if _rc!=0 {
			}
		else {
			local z = `z'+1
			merge 1:1 actorAX actorBX using `dyad', keep(2 3)
			ren one one_`z'
			ren fatalities fatalities_`z'
			replace fatalities_`z'=0 if one_`z'==0
			drop _merge 
			save `dyad', replace 
			}
		restore  
		}
		di "`i', `j'"
		}
	}

use `dyad'

drop if actorAX==. 
drop if actorBX==. 
gsort actorAX actorBX

egen fatalities_enemy = rowtotal(fatalities_*)
egen events_enemy = rowtotal(one_*)

keep actor* fatalities_enemy events_enemy

save `dyad', replace 
save dyad.dta, replace 

 

/*----------------------------------------------------*/
   /* [>   2.  Allieships   <] */ 
/*----------------------------------------------------*/

use syria_matched.dta, replace
local z=0
forv i=1/`an' {
	forv j=1/`an' {
		if `i'>`j' {
			qui {
				preserve 
				
				ren actorA`i' actorAX 
				ren actorA`j' actorBX 

				merge m:1 actorAX actorBX using `dyad', keep(2 3)

				capture collapse (sum) one fatalities, by(actorAX actorBX)
				if _rc!=0 {
					}
				else {
					local z = `z'+1
					merge 1:1 actorAX actorBX using `dyad', keep(2 3)
					ren one one_`z'
					ren fatalities fatalities_`z'
					replace fatalities_`z'=0 if one_`z'==0
					drop _merge 
					save `dyad', replace 
					}
				restore  
				}
				di "`i', `j'"
				}
		else {
		}
	}
}	


use `dyad'
save dyad1.dta, replace 


drop if actorAX==. 
drop if actorBX==. 
gsort actorAX actorBX

egen fatalities_ally1 = rowtotal(fatalities_*)
egen events_ally1 = rowtotal(one_*)

keep actor* fatalities_enemy events_enemy fatalities_ally1 events_ally1

save `dyad', replace
save dyad1b.dta, replace 




 
/*----------------------------------------------------*/
   /* [>   3.  Allies on B-Side   <] */ 
/*----------------------------------------------------*/
use syria_matched.dta, replace
local z=0
forv i=1/`bn' {
	forv j=1/`bn' {
		if `i'>`j' {
			qui {
				preserve 
				
				ren actorB`i' actorAX 
				ren actorB`j' actorBX 

				merge m:1 actorAX actorBX using `dyad', keep(2 3)

				capture collapse (sum) one fatalities, by(actorAX actorBX)
				if _rc!=0 {
					}
				else {
					local z = `z'+1
					merge 1:1 actorAX actorBX using `dyad', keep(2 3)
					ren one one_`z'
					ren fatalities fatalities_`z'
					replace fatalities_`z'=0 if one_`z'==0
					drop _merge 
					save `dyad', replace 
					}
				restore  
				}
				di "`i', `j'"
				}
		else {
		}
	}
}	


use `dyad'

drop if actorAX==. 
drop if actorBX==. 
gsort actorAX actorBX

egen fatalities_ally2 = rowtotal(fatalities_*)
egen events_ally2 = rowtotal(one_*)

gen fatalities_ally=fatalities_ally1 + fatalities_ally2 
gen events_ally = events_ally1 + events_ally2

keep actor* fatalities_enemy events_enemy fatalities_ally events_ally 

save `dyad', replace
save dyad2.dta, replace 


 
/*----------------------------------------------------*/
   /* [>   Make symmetric matrix, generate enemy and ally relations   <] */ 
/*----------------------------------------------------*/
/* [> Now, need to make matrix of dyadic fighting events symmetric <] */ 
use dyad2.dta, replace 

save dyad_A.dta, replace 

ren (actorAX actorBX) (actorBX actorAX)
ren (fatalities_enemy events_enemy fatalities_ally events_ally) (fatalities_enemyB events_enemyB fatalities_allyB events_allyB)

merge 1:1 actorAX actorBX using dyad_A.dta, keep(2 3)

// Check:
// br if (actorBX==442 & actorAX==1199)  | actorAX==442 & actorBX==1199

foreach v of varlist fatalities_enemy events_enemy fatalities_ally events_ally {
	replace `v' = `v'+`v'B
}

gen enemy = 0
replace enemy = 1 if events_enemy!=0

gen allied = 0
replace allied = 1 if events_ally!=0

gen neutral = 0
replace neutral = 1 if enemy==allied
replace enemy = 0 if neutral==1
replace allied = 0 if neutral==1

ren (actorAX actorBX) (idA idB)

bys idA: egen idBB = rank(idB)
bys idB: egen idAA = rank(idA)
gsort idA idB
save syria_dyad.dta, replace 


 
/*----------------------------------------------------*/
   /* [>   Make necessary matrices for matlab   <] */ 
/*----------------------------------------------------*/

use syria_dyad.dta, replace 
keep idAA idBB enemy 
unique idAA
local n=r(sum)
reshape wide  enemy  , i(idAA) j(idBB)
forv i = 1/`n' {
   replace enemy`i' = 0 if mi(enemy`i')
}
mkmat enemy*, mat(aminus)

save A_minus, replace
export delim A_minus_syria.csv, replace 





use syria_dyad.dta, replace 
keep idAA idBB allied
unique idAA
local n=r(sum)
reshape wide  allied  , i(idAA) j(idBB)
forv i = 1/`n' {
   replace allied`i' = 0 if mi(allied`i')
}
mkmat allied*, mat(aminus)

save A_plus, replace
export delim A_plus_syria.csv, replace 



 
/*----------------------------------------------------*/
   /* [>   Generate dataset of fighting efforts and number of enmities etc.   <] */ 
/*----------------------------------------------------*/
use syria_dyad.dta, replace 

collapse (sum)  enemy allied neutral, by(idA)

merge 1:1 idA using tf_syria.dta
keep if _merge==3
drop _merge
gen degree_minus = enemy 
gen degree_plus = allied
export delim data_syria.csv, replace 






