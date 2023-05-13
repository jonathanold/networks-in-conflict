// Figure 4
// Need to first run table 5!!

use endo_KPresult_foreign, clear
saveold endo_Foreign_Conditional, replace


global  MC_draws = 100 // 1000

do ../progs/endo_network_Policy_KP.do
use endo_KeyPlayer_result, clear
saveold endo_KeyPlayer_result_Conditional, replace

use endo_KeyPlayer_result_Conditional, clear
keep if rank<16
drop endo_rank
sort endo_Delta_RD
sort rank
drop degree_plus degree_minus interior rank* multiplier
gen multiplier = endo_Delta_RD / bench_fighting_share
label var name "Group"
label var bench_fighting_share "obs. share in aggregate fight."
label var Delta_RD  "count. change in aggregate fight."
label var multiplier"multiplier"
label var endo_Delta_RD  "count. change in aggregate fight. with rewiring (median)"
label var sd_endo_Delta_RD  "count. change in aggregate fight. with rewiring (M.A.D)"
save tempFig4.dta, replace
export excel using ../results/TABLE_B14.xls, replace first(varl)



use tempFig4.dta, clear
replace Delta_RD=-Delta_RD * 100
replace endo_Delta_RD=-endo_Delta_RD * 100

replace name="RWANDA" if name=="Military Forces of Rwanda"
replace name=abbrev(name,3)
twoway (scatter endo_Delta_RD Delta_RD, mcolor(black) msymbol(none) mlabel(name) mcolor(black)  xscale(range(0 16))) ///
(scatter endo_Delta_RD Delta_RD , mcolor(black) msymbol(circle) ) ///
 (line Delta_RD Delta_RD, lcolor(gs8)  mlabsize(tiny) mlabcolor(blue)) , /// 
 ytitle(Rent Dissip. - Endogenous Network (pct)) xtitle(Rent Dissipation - Exogenous Network (pct)) title("Reduction in Rent Dissipation with exogenous/endogenous network", size(medium)) legend(off) scheme(s1mono)
graph save "../results/FIGURE4_left.gph", replace	
graph export "../results/FIGURE4_left.pdf", as(pdf) replace 

use endo_Foreign_Conditional, clear
sum Delta_RD if mc_draw!=., d
local med_mc = r(p50)
sum Delta_RD if mc_draw==.
local med_exo = r(mean)
twoway (histogram Delta_RD if mc_draw!=., xline(`med_exo', lc(blue)) xline(`med_mc', lc(red)) bin(20) frequency  fcolor(gs8) lcolor(black) lwidth(medthin) lpattern(solid) scheme(s1mono) xscale(range(-0.45 -0.22)) xlabel(#6) yscale(range(0 15)))  , ///
text( 14 -.315 " exogenous network", color(blue) size(small)) ///
text( 14 -.49 " endogenous network (median)", color(red) size(small)) ///
legend (off) ytitle("# MC draws") xtitle("Change in Rent Dissipation (pct)") title("Effect of Removing Foreign Groups with exogenous/endogenous network", size(medium))
graph save "../results/FIGURE4_right.gph", replace	
graph export "../results/FIGURE4_right.pdf", as(pdf) replace 


graph combine  "../results/FIGURE4_left.gph"   "../results/FIGURE4_right.gph", ysize(5) xsize(13) iscale(*1.2)
graph export "../replication_outputs/figures/fig4.pdf", replace 
