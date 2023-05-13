 
/*----------------------------------------------------*/
   /* [>   Master file for replication   <] */ 
/*----------------------------------------------------*/


/*
I start with the data files provided by the authors.

Execute this section to:
        * Create necessary directories on local machine
        * Install necesarry programs
        * Initialize programs, ado files
        * Create globals, locals, file paths
        * Run the complete analysis from this file.


* Our measure of fighting intensity is coarse insofar as it does not weigh events by the amount of military force involved. Ideally, we would like to have information about the number of casualties or other measures of physical destruction

*/

 

// Look at Conley SE maximum distance for robustness!!


/*----------------------------------------------------*/
   /* [>   0.  Make and set directories   <] */ 
/*----------------------------------------------------*/

/* [> Set working directory here <] */ 
global main "/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Networks in Conflict/"
cd "${main}"
cap mkdir ./regressions/
cap mkdir ./progs/
cap mkdir ./results/
cap mkdir ./original_data/
cap mkdir ./github/networks-in-conflict/replication_code
cap mkdir ./replication_outputs
cap mkdir ./replication_outputs/tables
cap mkdir ./replication_outputs/figures

cd "${main}/regressions"

global code     "${main}/github/networks-in-conflict/replication_code"
global github   "${main}/github/networks-in-conflict/"


/*----------------------------------------------------*/
   /* [>   1.  Github integration   <] */ 
/*----------------------------------------------------*/
/* [> Commit and push any important changes to github regularly. <] * 
cd ${github}
! git add --all // ${github}/replication_code/myfile.do
! git commit -m "Finalized tables 1 and 2."
! git push

 /* [> New branch for testing new code <] */
 // git checkout -b name-of-branch

*/
/* [> Reset CD <] */  
cd "${main}/regressions"

/*----------------------------------------------------*/
   /* [>   2.  Initialize programs   <] */ 
/*----------------------------------------------------*/
do "${code}/my_spatial_2sls_JDO.do"
do "${code}/reg2hdfespatial.do"
do "${code}/ols_spatial_HAC.do"
do "${main}/progs/nw2sls.do"
do "${main}/progs/nw2sls_partial.do"

 
/*----------------------------------------------------*/
   /* [>   3.  Download necessary other programs   <] */ 
/*----------------------------------------------------*/
//cap ssc install acreg
//cap ssc install spatial_hac_iv



 
/*----------------------------------------------------*/
   /* [>   4.  Run files for replication   <] */ 
/*----------------------------------------------------*/
//  do "${code}/table1.do"
// do "${code}/table1_comments.do"
// do "${code}/table2.do"
//  do "${code}/table3.do"
// do "${code}/table4.do"
 do "${code}/table5.do"

// do "${code}/mle_equilibrium.do"



/*
foreach conflict in afghanistan syria myanmar yemen iraq {
    do "${code}/replication_acled_`conflict'.do"
}
*/


