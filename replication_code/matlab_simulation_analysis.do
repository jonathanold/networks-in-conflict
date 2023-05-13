// Aalysis of Matlab simulation output
set scheme mine
 
/*----------------------------------------------------*/
   /* [>   1.  Standard   <] */ 
/*----------------------------------------------------*/



import delim matlab_simulation.csv, clear

ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025

gen borig = -0.083
gen borig2 = -0.083

gen gorig =  0.114
gen gorig2 = 0.114



gen chold = (gmin>0)
cap drop correct
gen correct = gcorrect 
replace correct = 0 if bcorrect==0


reg correct chold

tab correct chold, col



bys beta: egen mbc = mean(bcorrect)

gr tw  (sc bhat beta if (bcorrect==0 | gcorrect==0) & bhat<=1 & bhat>=-1   , yaxis(1) msymbol(smtriangle) col(gray%10) ) ///
(sc bhat beta if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%20) yaxis(1)) ///
(line mbc beta , lwidth(medthick) sort col(cranberry%100)  yaxis(2)) ///
(line beta beta, lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc borig borig2,   yaxis(1) msymbol(X) msize(large) mlwidth(thick) col(black%100)) , ///
ysc(ra(-0.2(0.1)0.2)) ysc(ra(0(0.1)0.6) axis(2)) ylabel(0(0.1)0.6, axis(2))   xti("{&beta}") yti("b = estimated {&beta}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "b={&beta}" 5 "Share correct estimates for {&beta}"))

graph export "../replication_outputs/figures/sim1_beta.pdf", replace 


bys gamma: egen mgc = mean(gcorrect)

gr tw  (sc ghat gamma if (bcorrect==0 | gcorrect==0) & ghat<=1 & ghat>=-1   , yaxis(1) msymbol(smtriangle) col(gray%10) ) ///
(sc ghat gamma if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%20) yaxis(1)) ///
(line mgc gamma , lwidth(medthick) sort col(cranberry%100)  yaxis(2)) ///
(line gamma gamma , lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc gorig gorig2,   yaxis(1) msymbol(X) msize(huge) mlwidth(thick) col(black%100)) , ///
ysc(ra(-0.2(0.1)0.2)) ysc(ra(0(0.1)0.6) axis(2)) ylabel(0(0.1)0.6, axis(2))   xti("{&gamma}") yti("g = estimated {&gamma}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "g={&gamma}" 5 "Share correct estimates for {&gamma}"))

graph export "../replication_outputs/figures/sim1_gamma.pdf", replace 



gr tw  (sc beta gamma  if (bcorrect==0 | gcorrect==0)  , msymbol(x) col(gray%20)) ///
(sc beta gamma if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%35) ) ///
(sc borig gorig  , msymbol(X) msize(large) mlwidth(medthick)  col(black%100) ) ///
, ///
ysc(ra(-1(0.2)1)) xti("{&gamma}") yti("{&beta}") ///
legend(order(2 "Correct estimates" 1 "Incorrect estimates" 3 "Main specification from paper"))

graph export "../replication_outputs/figures/sim1_2dplot.pdf", replace 




/*----------------------------------------------------*/
   /* [>   2.  "Local simulation  <] */ 
/*----------------------------------------------------*/



import delim matlab_simulation_loc.csv, clear


ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025



gen borig = -0.083
gen borig2 = -0.083

gen gorig =  0.114
gen gorig2 = 0.114



gr tw  (sc bhat beta if (bcorrect==0 | gcorrect==0) & bhat<=0.25 & bhat>=-0.2   , yaxis(1) msymbol(smtriangle) col(gray%20) ) ///
(sc bhat beta if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%20) yaxis(1)) ///
(lpoly bcorrect beta, lwidth(medthick) sort col(cranberry%100) yaxis(2) kernel(triangle) bwidth(0.005)) ///
(line beta beta, lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc borig borig2,   yaxis(1) msymbol(X) msize(large) mlwidth(thick) col(black%100)) , ///
xlab(-0.12(0.01)-0.05) ysc(ra(-0.2(0.1)0.2)) ysc(ra(0(0.1)0.6) axis(2)) ylabel(0(0.1)0.6, axis(2))   xti("{&beta}") yti("b = estimated {&beta}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "b={&beta}" 5 "Share correct estimates for {&beta}"))

graph export "../replication_outputs/figures/sim_loc_beta.pdf", replace 



gr tw  (sc ghat gamma if (bcorrect==0 | gcorrect==0) & ghat<=0.2 & ghat>=-0.1   , yaxis(1) msymbol(smtriangle) col(gray%10) ) ///
(sc ghat gamma if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%20) yaxis(1)) ///
(lpoly gcorrect gamma, lwidth(medthick) sort col(cranberry%100) yaxis(2) kernel(triangle) bwidth(0.005)) ///
(line gamma gamma , lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc gorig gorig2,   yaxis(1) msymbol(X) msize(huge) mlwidth(thick) col(black%100)) , ///
ysc(ra(-0.1(0.1)0.2)) xlab(0.08(0.01)0.15) ysc(ra(0(0.1)0.6) axis(2)) ylabel(0(0.1)0.6, axis(2))   xti("{&gamma}") yti("g = estimated {&gamma}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "g={&gamma}" 5 "Share correct estimates for {&gamma}"))

graph export "../replication_outputs/figures/sim_loc_gamma.pdf", replace 



gr tw  (sc beta gamma  if (bcorrect==0 | gcorrect==0)  , msymbol(x) col(gray%20)) ///
(sc beta gamma if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%35) ) ///
(sc borig gorig  , msymbol(X) msize(large) mlwidth(medthick)  col(black%100) ) ///
, ///
xlab(0.08(0.01)0.15) ylab(-0.12(0.01)-0.05) ysc(ra(-0.12(0.1)-0.05)) xti("{&gamma}") yti("{&beta}") ///
legend(order(2 "Correct estimates" 1 "Incorrect estimates" 3 "Main specification from paper"))

graph export "../replication_outputs/figures/sim_loc_2dplot.pdf", replace 








/*----------------------------------------------------*/
   /* [>   3.  Setting beta equal to zero simulation  <] */ 
/*----------------------------------------------------*/

import delim matlab_simulation_onlyg.csv, clear


ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025


gen borig = -0.0
gen borig2 = -0.0

gen gorig =  0.114
gen gorig2 = 0.114


/* [> Check concavity <] */ 
gen chold = (gmin>0)
cap drop correct
gen correct = gcorrect 
replace correct = 0 if bcorrect==0

tab correct chold, col


gr tw  (sc bhat beta if (bcorrect==0 | gcorrect==0) & bhat<=0.25 & bhat>=-0.2   , jitter(10)  msymbol(smtriangle) col(gray%5) ) ///
(sc bhat beta if bcorrect==1 & gcorrect==1  , jitter(10)  msymbol(smcircle)  col(cranberry%5) yaxis(1)), ///
xlab(-0.01(0.01)0.01) ysc(ra(-0.2(0.1)0.2))    xti("{&beta}") yti("b = estimated {&beta}", axis(1))  ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates"))

graph export "../replication_outputs/figures/sim_onlyg_beta.pdf", replace 



gr tw  (sc ghat gamma if (bcorrect==0 | gcorrect==0) & ghat<=1 & ghat>=-1   , yaxis(1) msymbol(smtriangle) col(gray%10) ) ///
(sc ghat gamma if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%20) yaxis(1)) ///
(lpoly gcorrect gamma, lwidth(medthick) sort col(cranberry%100) yaxis(2) kernel(triangle) bwidth(0.025)) ///
(line gamma gamma , lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc gorig gorig2,   yaxis(1) msymbol(X) msize(huge) mlwidth(thick) col(black%100)) , ///
ysc(ra(-0.1(0.1)0.2)) xlab(-0.9(0.2)0.9) ysc(ra(0(0.2)1) axis(2)) ylabel(0(0.2)1, axis(2))   xti("{&gamma}") yti("g = estimated {&gamma}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "g={&gamma}" 5 "Share correct estimates for {&gamma}"))

graph export "../replication_outputs/figures/sim_onlyg_gamma.pdf", replace 











/*----------------------------------------------------*/
   /* [>   3.  Setting gamma equal to zero simulation  <] */ 
/*----------------------------------------------------*/

import delim matlab_simulation_onlyb.csv, clear


ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025


gen borig = -0.083
gen borig2 = -0.083

gen gorig =  0.0
gen gorig2 = 0.0


/* [> Check concavity <] */ 
gen chold = (gmin>0)
cap drop correct
gen correct = gcorrect 
replace correct = 0 if bcorrect==0

tab correct chold, col


gr tw  (sc bhat beta if (bcorrect==0 | gcorrect==0) & bhat<=0.25 & bhat>=-0.2   , jitter(1)  msymbol(smtriangle) col(gray%5) ) ///
(sc bhat beta if bcorrect==1 & gcorrect==1  , jitter(1)  msymbol(smcircle)  col(cranberry%5) yaxis(1)), ///
xlab(-0.01(0.01)0.01) ysc(ra(-0.2(0.1)0.2))    xti("{&beta}") yti("b = estimated {&beta}", axis(1))  ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates"))

gr tw  (sc bhat beta if (bcorrect==0 | gcorrect==0) & bhat<=1 & bhat>=-1   , yaxis(1) msymbol(smtriangle) col(gray%10) ) ///
(sc bhat beta if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%20) yaxis(1)) ///
(lpoly bcorrect beta, lwidth(medthick) sort col(cranberry%100) yaxis(2) kernel(triangle) bwidth(0.025)) ///
(line beta beta , lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc borig borig2,   yaxis(1) msymbol(X) msize(huge) mlwidth(thick) col(black%100)) , ///
ysc(ra(-0.1(0.1)0.2)) xlab(-0.9(0.2)0.9) ysc(ra(0(0.2)1) axis(2)) ylabel(0(0.2)1, axis(2))   xti("{&gamma}") yti("g = estimated {&gamma}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "g={&gamma}" 5 "Share correct estimates for {&gamma}"))



graph export "../replication_outputs/figures/sim_onlyg_beta.pdf", replace 



gr tw  (sc ghat gamma if (bcorrect==0 | gcorrect==0) & ghat<=1 & ghat>=-1   , yaxis(1) msymbol(smtriangle) col(gray%10) ) ///
(sc ghat gamma if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%20) yaxis(1)) ///
(lpoly gcorrect gamma, lwidth(medthick) sort col(cranberry%100) yaxis(2) kernel(triangle) bwidth(0.025)) ///
(line gamma gamma , lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc gorig gorig2,   yaxis(1) msymbol(X) msize(huge) mlwidth(thick) col(black%100)) , ///
ysc(ra(-0.1(0.1)0.2)) xlab(-0.9(0.2)0.9) ysc(ra(0(0.2)1) axis(2)) ylabel(0(0.2)1, axis(2))   xti("{&gamma}") yti("g = estimated {&gamma}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "g={&gamma}" 5 "Share correct estimates for {&gamma}"))

graph export "../replication_outputs/figures/sim_onlyg_gamma.pdf", replace 












 
/*----------------------------------------------------*/
   /* [>   4.  Panel data simulation   <] */ 
/*----------------------------------------------------*/




import delim matlab_simulation_panel.csv, clear



ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025

gen borig = -0.083
gen borig2 = -0.083

gen gorig =  0.114
gen gorig2 = 0.114


bys beta: egen mbc = mean(bcorrect)

gr tw  (sc bhat beta if (bcorrect==0 | gcorrect==0) & bhat<=0.25 & bhat>=-0.2   , yaxis(1) msymbol(smtriangle) col(gray%20) ) ///
(sc bhat beta if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%20) yaxis(1)) ///
(line mbc beta , lwidth(medthick) sort col(cranberry%100)  yaxis(2)) ///
(line beta beta, lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc borig borig2,   yaxis(1) msymbol(X) msize(large) mlwidth(thick) col(black%100)) , ///
xlab(-0.12(0.01)-0.05) ysc(ra(-0.2(0.1)0.2)) ysc(ra(0(0.1)0.6) axis(2)) ylabel(0(0.1)0.6, axis(2))   xti("{&beta}") yti("b = estimated {&beta}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "b={&beta}" 5 "Share correct estimates for {&beta}"))

graph export "../replication_outputs/figures/sim_p1_beta.pdf", replace 


bys gamma: egen mgc = mean(gcorrect)

gr tw  (sc ghat gamma if (bcorrect==0 | gcorrect==0) & ghat<=0.2 & ghat>=-0.1   , yaxis(1) msymbol(smtriangle) col(gray%10) ) ///
(sc ghat gamma if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%20) yaxis(1)) ///
(line mgc gamma , lwidth(medthick) sort col(cranberry%100)  yaxis(2)) ///
(line gamma gamma , lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc gorig gorig2,   yaxis(1) msymbol(X) msize(huge) mlwidth(thick) col(black%100)) , ///
ysc(ra(-0.1(0.1)0.2)) xlab(0.08(0.01)0.15) ysc(ra(0(0.1)0.6) axis(2)) ylabel(0(0.1)0.6, axis(2))   xti("{&gamma}") yti("g = estimated {&gamma}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "g={&gamma}" 5 "Share correct estimates for {&gamma}"))

graph export "../replication_outputs/figures/sim_p1_gamma.pdf", replace 



gr tw  (sc beta gamma  if (bcorrect==0 | gcorrect==0)  , msymbol(x) col(gray%20)) ///
(sc beta gamma if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%35) ) ///
(sc borig gorig  , msymbol(X) msize(large) mlwidth(medthick)  col(black%100) ) ///
, ///
xlab(0.08(0.01)0.15) ylab(-0.12(0.01)-0.05) ysc(ra(-0.12(0.1)-0.05)) xti("{&gamma}") yti("{&beta}") ///
legend(order(2 "Correct estimates" 1 "Incorrect estimates" 3 "Main specification from paper"))

graph export "../replication_outputs/figures/sim_p1_2dplot.pdf", replace 









 
/*----------------------------------------------------*/
   /* [>   5.  Panel data simulation 2  <] */ 
/*----------------------------------------------------*/




import delim matlab_simulation_10reps.csv, clear



ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025

gen borig = -0.083
gen borig2 = -0.083

gen gorig =  0.114
gen gorig2 = 0.114


bys beta: egen mbc = mean(bcorrect)

gr tw  (sc bhat beta if (bcorrect==0 | gcorrect==0) & bhat<=0.25 & bhat>=-0.2   , yaxis(1) msymbol(smtriangle) col(gray%1) ) ///
(sc bhat beta if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%2) yaxis(1)) ///
(line mbc beta , lwidth(medthick) sort col(cranberry%100)  yaxis(2)) ///
(line beta beta, lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc borig borig2,   yaxis(1) msymbol(X) msize(large) mlwidth(thick) col(black%100)) , ///
xlab(-0.12(0.01)-0.05) ysc(ra(-0.2(0.1)0.2)) ysc(ra(0(0.1)0.6) axis(2)) ylabel(0(0.1)0.6, axis(2))   xti("{&beta}") yti("b = estimated {&beta}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "b={&beta}" 5 "Share correct estimates for {&beta}"))

graph export "../replication_outputs/figures/sim_p2_beta.pdf", replace 


bys gamma: egen mgc = mean(gcorrect)

gr tw  (sc ghat gamma if (bcorrect==0 | gcorrect==0) & ghat<=0.2 & ghat>=-0.1   , yaxis(1) msymbol(smtriangle) col(gray%10) ) ///
(sc ghat gamma if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%20) yaxis(1)) ///
(line mgc gamma , lwidth(medthick) sort col(cranberry%100)  yaxis(2)) ///
(line gamma gamma , lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc gorig gorig2,   yaxis(1) msymbol(X) msize(huge) mlwidth(thick) col(black%100)) , ///
ysc(ra(-0.1(0.1)0.2)) xlab(0.08(0.01)0.15) ysc(ra(0(0.1)0.6) axis(2)) ylabel(0(0.1)0.6, axis(2))   xti("{&gamma}") yti("g = estimated {&gamma}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "g={&gamma}" 5 "Share correct estimates for {&gamma}"))

graph export "../replication_outputs/figures/sim_p2_gamma.pdf", replace 



gr tw  (sc beta gamma  if (bcorrect==0 | gcorrect==0)  , msymbol(x) jitter(1) col(gray%1)) ///
(sc beta gamma if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%4) ) ///
(sc borig gorig  , msymbol(X) msize(large) mlwidth(medthick)  col(black%100) ) ///
, ///
xlab(0.08(0.01)0.15) ylab(-0.12(0.01)-0.05) ysc(ra(-0.12(0.1)-0.05)) xti("{&gamma}") yti("{&beta}") ///
legend(order(2 "Correct estimates" 1 "Incorrect estimates" 3 "Main specification from paper"))

graph export "../replication_outputs/figures/sim_p2_2dplot.pdf", replace 













 
/*----------------------------------------------------*/
   /* [>   5.  Panel data simulation 2  <] */ 
/*----------------------------------------------------*/




import delim matlab_simulation_10repsb.csv, clear



ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025

gen borig = -0.083
gen borig2 = -0.083

gen gorig =  0.114
gen gorig2 = 0.114


bys beta: egen mbc = mean(bcorrect)

gr tw  (sc bhat beta if (bcorrect==0 | gcorrect==0) & bhat<=0.25 & bhat>=-0.2   , yaxis(1) msymbol(smtriangle) col(gray%1) ) ///
(sc bhat beta if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%2) yaxis(1)) ///
(line mbc beta , lwidth(medthick) sort col(cranberry%100)  yaxis(2)) ///
(line beta beta, lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc borig borig2,   yaxis(1) msymbol(X) msize(large) mlwidth(thick) col(black%100)) , ///
xlab(-0.12(0.01)-0.05) ysc(ra(-0.2(0.1)0.2)) ysc(ra(0(0.1)0.6) axis(2)) ylabel(0(0.1)0.6, axis(2))   xti("{&beta}") yti("b = estimated {&beta}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "b={&beta}" 5 "Share correct estimates for {&beta}"))

graph export "../replication_outputs/figures/sim_p2b_beta.pdf", replace 


bys gamma: egen mgc = mean(gcorrect)

gr tw  (sc ghat gamma if (bcorrect==0 | gcorrect==0) & ghat<=0.2 & ghat>=-0.1   , yaxis(1) msymbol(smtriangle) col(gray%1) ) ///
(sc ghat gamma if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%2) yaxis(1)) ///
(line mgc gamma , lwidth(medthick) sort col(cranberry%100)  yaxis(2)) ///
(line gamma gamma , lwidth(thick) yaxis(1) col(ebblue%80) sort) ///
(sc gorig gorig2,   yaxis(1) msymbol(X) msize(huge) mlwidth(thick) col(black%100)) , ///
ysc(ra(-0.1(0.1)0.2)) xlab(0.08(0.01)0.15) ysc(ra(0(0.1)0.6) axis(2)) ylabel(0(0.1)0.6, axis(2))   xti("{&gamma}") yti("g = estimated {&gamma}", axis(1)) yti("Share correct estimates", axis(2) ) ///
legend(order( 1 "Incorrect estimates" 2 "Correct estimates" 4 "Main specification from paper" 3 "g={&gamma}" 5 "Share correct estimates for {&gamma}"))

graph export "../replication_outputs/figures/sim_p2b_gamma.pdf", replace 



gr tw  (sc beta gamma  if (bcorrect==0 | gcorrect==0)  , msymbol(x) jitter(1) col(gray%1)) ///
(sc beta gamma if bcorrect==1 & gcorrect==1  , msymbol(smcircle)  col(cranberry%4) ) ///
(sc borig gorig  , msymbol(X) msize(large) mlwidth(medthick)  col(black%100) ) ///
, ///
xlab(0.08(0.01)0.15) ylab(-0.12(0.01)-0.05) ysc(ra(-0.12(0.1)-0.05)) xti("{&gamma}") yti("{&beta}") ///
legend(order(2 "Correct estimates" 1 "Incorrect estimates" 3 "Main specification from paper"))

graph export "../replication_outputs/figures/sim_p2b_2dplot.pdf", replace 



gen chold = (gmin>0)
cap drop correct
gen correct = gcorrect 
replace correct = 0 if bcorrect==0


reg correct chold

tab correct chold, col



 
/*----------------------------------------------------*/
   /* [>   6.  Simulate 100,000 times for estimates in the paper   <] */ 
/*----------------------------------------------------*/


import delim "matlab_simulation_1000runsX100t.csv", clear



ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025

gen borig = -0.114
gen borig2 = -0.114

gen gorig =  0.083
gen gorig2 = 0.083


hist bhat if bhat>=-0.15 & bhat<=0.1, xti("Estimates for {&beta}") xlab(-0.15(0.05)0.1) start(-0.25) width(0.005) xline(-0.114, lwidth(thick)) fcol(%20)

hist ghat if ghat>=-0.1 & ghat<=0.15, xti("Estimates for {&gamma}") xlab(-0.1(0.05)0.15) start(-0.25) width(0.005) xline(0.083, lwidth(thick)) fcol(%20)


gen bdist = (beta-bhat)^2
gen gdist = (gamma-ghat)^2 

reg bdist gmin










/*----------------------------------------------------*/
   /* [>   6.  Simulate 100,000 times for estimates in the paper: 0 a starting point   <] */ 
/*----------------------------------------------------*/
set scheme mine

import delim "matlab_simulation_1000runsX100t_startwith0.csv", clear



ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025

gen borig = -0.114
gen borig2 = -0.114

gen gorig =  0.083
gen gorig2 = 0.083


hist bhat if bhat>=-0.15 & bhat<=0.1, xti("Estimates for {&beta}") xlab(-0.15(0.05)0.1) start(-0.25) width(0.005) xline(-0.114, lwidth(thick)) fcol(%20)

hist ghat if ghat>=-0.1 & ghat<=0.15, xti("Estimates for {&gamma}") xlab(-0.1(0.05)0.15) start(-0.25) width(0.005) xline(0.083, lwidth(thick)) fcol(%20)


gen bdist = (beta-bhat)^2
gen gdist = (gamma-ghat)^2 

reg bdist gmin



/* [> 7. Simulation with first global optimization (genetic algorithm), then local <] */ 

import delim "matlab_simulation_1000runsX100t_genetic_algorithm.csv", clear



ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025

gen borig = -0.114
gen borig2 = -0.114

gen gorig =  0.083
gen gorig2 = 0.083


winsor bhat, p(0.05) gen(bh)
hist bh , xti("Estimates for {&beta}") ///
    xlab(-0.30(0.1)0.5) width(0.01) xline(-0.114, lwidth(medthick)) fcol(%20)

winsor ghat, p(0.05) gen(gh)
hist gh  , xti("Estimates for {&gamma}")  ///
    width(0.01) xline(0.083, lwidth(medthick)) fcol(%20) // start(-0.25)


gen bdist = (beta-bhat)^2
gen gdist = (gamma-ghat)^2 

reg bdist gmin


sc bhat ghat, col(%10)








/* [> 8. Simulation with first global optimization (genetic algorithm), then local <] */ 

import delim "matlab_simulation_1000runsX100t_ga_flipped.csv", clear



ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025

gen borig = 0.114
gen borig2 = 0.114

gen gorig =  -0.083
gen gorig2 = -0.083

winsor bhat, p(0.05) gen(bh)
winsor ghat, p(0.05) gen(gh)

replace bh=. if bhat!=bh 
replace gh=. if ghat!=gh 



hist bh , xti("Estimates for {&beta}") ///
    xlab(-0.30(0.1)0.5) width(0.02) xline(0.114, lwidth(medthick)) fcol(%20)

hist gh  , xti("Estimates for {&gamma}")  ///
    width(0.01) xline(-0.083, lwidth(medthick)) fcol(%20) // start(-0.25)


gen bdist = (beta-bhat)^2
gen gdist = (gamma-ghat)^2 

reg bdist gmin

sc bhat ghat, col(%10)



















/* [> 8. Simulation with patternsearch, then local <] */ 

import delim "matlab_simulation_1000runsX100t_patternsearch.csv", clear



ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025

gen borig = 0.114
gen borig2 = 0.114

gen gorig =  -0.083
gen gorig2 = -0.083

winsor bhat, p(0.05) gen(bh)
winsor ghat, p(0.05) gen(gh)

replace bh=. if bhat!=bh 
replace gh=. if ghat!=gh 



hist bh , xti("Estimates for {&beta}") ///
    xlab(-0.20(0.1)0.3) width(0.01) xline(-0.114, lwidth(medthick)) fcol(%20)

hist gh  , xti("Estimates for {&gamma}")  ///
    width(0.01) xlab(-0.00(0.1)0.7) xline(0.083, lwidth(medthick)) fcol(%20) // start(-0.25)


gen bdist = (beta-bhat)^2
gen gdist = (gamma-ghat)^2 

reg bdist gmin

sc bhat ghat, col(%1) jitter(2) xline(0.083) yline(-0.114)
























/* [> 8. Simulation with patternsearch, then local <] */ 

import delim "matlab_simulation_1000runsX100t_local_correct.csv", clear



ren (v1 v2 v3 v4 v5 v6) (beta gamma fval bhat ghat gmin)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025

gen borig = 0.114
gen borig2 = 0.114

gen gorig =  -0.083
gen gorig2 = -0.083

winsor bhat, p(0.05) gen(bh)
winsor ghat, p(0.05) gen(gh)

replace bh=. if bhat!=bh 
replace gh=. if ghat!=gh 



hist bh , xti("Estimates for {&beta}") ///
    xlab(0(0.1)0.8) width(0.02) xline(0.114, lwidth(medthick)) fcol(%20)
graph export  "../replication_outputs/figures/hist_sim_beta.pdf", replace


hist gh  , xti("Estimates for {&gamma}")  ///
    width(0.01) xlab(0.00(0.1)0.3) xline(0.083, lwidth(medthick)) fcol(%20) // start(-0.25)
graph export  "../replication_outputs/figures/hist_sim_gamma.pdf", replace


gen bdist = (beta-bhat)^2
gen gdist = (gamma-ghat)^2 

reg bdist gmin

sc bhat ghat, col(%1) jitter(2) xline(0.083) yline(0.114)













/* [> 8. Simulation with patternsearch, then local <] */ 

import delim "matlab_simulation_1000runsX100t_local_correct_nlls.csv", clear



ren (v1 v2 v3 v4 v5 v6 v7 v8 v9 v10) (beta gamma fval a b c bhat ghat bse gse)

 gen gcorrect = 0
 replace gcorrect = 1 if ghat>=gamma-0.025 & ghat<=gamma+0.025
 gen bcorrect=0
replace bcorrect = 1 if bhat>=beta-0.025 & bhat<=beta+0.025

gen borig = 0.114
gen borig2 = 0.114

gen gorig =  -0.083
gen gorig2 = -0.083

winsor bhat, p(0.05) gen(bh)
winsor ghat, p(0.05) gen(gh)

replace bh=. if bhat!=bh 
replace gh=. if ghat!=gh 



hist bh , xti("Estimates for {&beta}") ///
    xlab(0(0.1)0.8) width(0.02) xline(0.114, lwidth(medthick)) fcol(%20)
graph export  "../replication_outputs/figures/hist_sim_beta_nlls.pdf", replace


hist gh  , xti("Estimates for {&gamma}")  ///
    width(0.01) xlab(0.00(0.1)0.3) xline(0.083, lwidth(medthick)) fcol(%20) // start(-0.25)
graph export  "../replication_outputs/figures/hist_sim_gamma_nlls.pdf", replace


gen bdist = (beta-bhat)^2
gen gdist = (gamma-ghat)^2 

reg bdist gmin

sc bhat ghat, col(%1) jitter(2) xline(0.083) yline(0.114)

