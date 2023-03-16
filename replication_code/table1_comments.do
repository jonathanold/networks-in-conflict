
 
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






use KRTZ_monadic_AF.dta, clear


/*----------------------------------------------------*/
   /* [>   Comment 1: There can be large differences depending on how one defines fixed effects   <] */ 
/*----------------------------------------------------*/

my_spatial_2sls_jo `y' `controls1' `controls2' i.group i.year , `mys_syntax_iv'
xi: my_spatial_2sls_jo `y' `controls1' `controls2' TE* Dgroup*, `mys_syntax_iv'



/*
Some differences also come from the treatment of singletons.
The controls for specific group-years are not used in FE estimation and should be dropped.
        [Sidenote: ivreghdfe seems to behave weird also: In some cases, difference whether variable included in absorb or as FE.]

The estimates become very unstable when using year fixed effects:
*/
ivreghdfe `y' `controls1'  (`x' `n' = `iv_full_neutral')  , absorb(group `controls3')
ivreghdfe `y' `controls1'   (`x' `n' = `iv_full_neutral')  , absorb(group year)
ivreghdfe `y' `controls1' TE*  (`x' `n' = `iv_full_neutral')  , absorb(group year)
ivreg2 `y' `controls1'  i.group i.year  (`x' `n' = `iv_full_neutral')  


/*
Table 1, Column 5: Reports correctly the OLS estimates, but the text describes that IV was used (and same for replication files.)
*/


 /*
/*----------------------------------------------------*/
   /* [>   Comment 2:  The paper reports the wrong IV   <] */ 
/*----------------------------------------------------*/

The paper states that:
        Column 2 replicates the specification of column 1 in a 2SLS setup using the lagged fighting efforts of each group’s set of enemies and allies as excluded instruments. 
 However, when looking at the official replication files and in my own replication, I find that they use rainfall as instrument.

Nevertheless, the description of column 3 works again.



/*----------------------------------------------------*/
   /* [>   2.  Log of changes in files   <] */ 
/*----------------------------------------------------*/

* my_spatial_2sls_JDO.do: 
        * Added code to also export KP-statistic
        * Save as tempfiles not as actual .dta files
        * 
* reghdfespatial:
        * 
* OLS_HAC_spatial:
        * Calculate geographic distance not euclidean (does not make a difference)

* Table 1:
        * Calculate R-squared manually. This matches with OLS but not with IV.

* Test 







