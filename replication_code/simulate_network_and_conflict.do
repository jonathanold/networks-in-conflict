
/* [> Get moments from original data <] */
use KRTZ_dyadic_AF.dta, clear
keep if year==2010
keep group group_d enemy allied

qui sum enemy
qui local mean_enemy = r(mean)
qui sum allied
qui local mean_allied = r(mean) 


by group: egen e = total(enemy)
by group: egen a = total(allied)

collapse (firstnm) e a , by(group)
sum e 
local em = r(mean)
local emv = r(Var)
sum a 
local am = r(mean)
local amv = r(Var)


drawnorm x1 x2 , corr(C)

gen enemies_sim = invchi2(`em',normal(x1))
gen allieds_sim = invchi2(`am',normal(x2))

replace enemies_sim = round(enemies_sim,1)
replace allieds_sim = round(allieds_sim,1)

sum e a en al 
corr e a en al

gsort group 
gen group_d = 1
expand 80
gsort group 
replace group_d = group_d[_n-1]+1 if group==group[_n-1]
drop if group== group_d 
order group group_d 

gen random_number = uniform()  
bys group: egen ordering = rank(random_number) 
gen en = 0 
replace en = 1 if ordering<=enemies_sim

gen random_number2 = uniform()  
bys group: egen ordering2 = rank(random_number) if en==0 
gen an = 0 
replace an = 1 if ordering2<=allieds_sim

keep group group_d en an 

// make symmetric
tempfile y 
save `y'

ren group gd 
ren group_d group
ren gd group_d 

ren en es 
ren an as 
keep group group_d es as 

tempfile x 
save `x'

use `y'
merge 1:1 group group_d using `x'

order group group_d en es an as 
replace en = es if group>=group_d 
replace an = as if group>=group_d 

keep group group_d en an 
ren (en an) (senem sally)


preserve 
keep group group_d senem 
reshape wide  senem  , i(group) j(group_d)
forv i = 1/80 {
   replace senem`i' = 0 if mi(senem`i')
}
mkmat senem*, mat(aminus)

export delim A_minus_sim.csv, replace 

restore, preserve 
 keep group group_d sally
reshape wide  sally  , i(group) j(group_d)
forv i = 1/80 {
   replace sally`i' = 0 if mi(sally`i')
}
mkmat sally*, mat(aplus)
export delim A_plus_sim.csv, replace 
restore 





// Generate other variables
collapse (sum) senem sally, by(group)

ren senem dminus 
ren sally dplus 

local beta = 0.2 
local gamma = -0.2 

gen gamma = 1/(1+`beta'*dplus+`gamma'*dminus)
egen lambda = total(gamma)
replace lambda = 1-1/lambda

mkmat gamma, mat(G)           
matrix define CVEC2 = invsym(I(80) + `beta'*aplus + `gamma'*aminus) * G
svmat CVEC2, name(CVECx)

gen tf_sim = CVECx // *lambda*(1-lambda)

ren tf_sim TotFight
ren (dplus dminus) (degree_plus degree_minus)
export delim data_simulated.csv, replace 



qui gen evaluate = (TotFight - LAMBDA*(1-LAMBDA)*CVEC)^2
qui sum evaluate
local value = r(mean)
    












matrix define C = J(2, 2, `corr_ea')
forvalues i = 1/2 {
    matrix define C[`i', `i'] = 1 // Not especially necessary. -ovbd- ignores the diagonals
}




qui reghdfe enemy allied, absorb(group group_d)
qui local corr_ea = _b[allied]

matrix input a = J(`mean_enemy', `mean_allied')
matrix define C = J(2, 2, `corr_ea')
forvalues i = 1/2 {
    matrix define C[`i', `i'] = 1 // Not especially necessary. -ovbd- ignores the diagonals
}

count if !mi(group)
local n = r(N)
ovbd enemy_sim allied_sim, means(a) corr(C) n(`n') clear
stop


forvalues i = 1/10 {
    matrix define C[`i', `i'] = 1 // Not especially necessary. -ovbd- ignores the diagonals
}


cap drop rbeta
 gen rbeta = rbeta(2,(2/`mean_enemy'-2))
 replace rbeta = rbeta[_n-1] if group==group[_n-1]
cap drop enemy_sim
gen enemy_sim =  rbinomial(1,rbeta)

gen rbeta_a = 0.00001 + (1-enemy_sim)*rbeta*5
gen allied_sim = rbinomial(1,rbeta_a)

sum allied_sim
reghdfe enemy_sim allied_sim, absorb(group group_d)


// make symmetric
preserve 

ren group gd 
ren group_d group
ren gd group_d 
ren enemy_sim es 
ren allied_sim as 
keep group group_d es as 

tempfile x 
save `x'

restore 
merge 1:1 group group_d using `x'

order group group_d enemy_sim es allied_sim as 
replace es = enemy_sim
replace as = allied_sim 

replace enemy_sim = es if group>=group_d 
replace allied_sim = as if group>=group_d 


keep group group_d enemy allied enemy_sim allied_sim 

by group: egen e = mean(enemy)
by group: egen a = mean(allied)

by group: egen es = mean(enemy_sim)
by group: egen as = mean(allied_sim)



stop



/* [> Generate symmetric matrix of relations, axx <] */
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


