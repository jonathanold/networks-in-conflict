
use "/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Networks in Conflict/regressions/KRTZ_dyadic_battles.dta", clear
keep if year==2000
gen dir = (enemy==1)
replace dir=2 if allied==1
replace dir=0.0000001 if neutral==1
keep group group_d name name_d dir


gen namex = substr(name,1,10)
gen namex_d = substr(name_d,1,10)

nwset name name_d dir, edgelist labs(namex namex_d) labsfromvar(namex namex_d) keeporiginal

gen n = _n
gen id = _nodeoriginal
nwplotmatrix , ///
	legend(symysize(2) symxsize(2) size(vsmall) col(3) order(1 "Neutral" 2 "Enemies" 3 "Allies")) ///
	sortby(_nodeoriginal) label(n) labelopt(labsize(tiny)) ///
	nodichotomize col(cranberry ebblue)

clear 

use "/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Networks in Conflict/regressions/KRTZ_dyadic_battles.dta", clear
keep if year==2000
gen dir = (enemy==1)
replace dir=2 if allied==1
replace dir=0 if neutral==1
keep group group_d name name_d dir
keep if dir!=0
keep name name_d
saveold "/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Networks in Conflict/graph.dta", version(12) replace



/*

restore 
gen namex = substr(name,1,10)
gen namex_d = substr(name_d,1,10)

nwset name name_d dir, edgelist labs(namex namex_d) labsfromvar(namex namex_d) keeporiginal

gen n = _n
gen id = _nodeoriginal


 nwplot,  label(n) arcstyle(straight)  layout(frucht) // layout(circle)

 stop

nwkatz, alpha(0.3) gen(k)
hist k
list _nodeoriginal k


	stop 



