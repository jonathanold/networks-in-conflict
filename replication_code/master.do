 
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
*/


 
/*----------------------------------------------------*/
   /* [>   1.  Make and set directories   <] */ 
/*----------------------------------------------------*/

/* [> Set working directory here <] */ 
cd "/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Networks in Conflict/"
cap mkdir ./regressions/
cap mkdir ./progs/
cap mkdir ./results/
cap mkdir ./original_data/
cap mkdir ./test/
cap mkdir ./github/networks-in-conflict/replication_code
cap mkdir ./replication_outputs
cap mkdir ./replication_outputs/tables
cap mkdir ./replication_outputs/figures

cd "./regressions/"

global code "../github/networks-in-conflict/replication_code"
global github "../github/networks-in-conflict/"


cd ${github}

 
/*----------------------------------------------------*/
   /* [>   2.  Initialize programs   <] */ 
/*----------------------------------------------------*/
do ${code}/my_spatial_2sls_JDO.do
do ${code}/reg2hdfespatial.do
do ${code}/ols_spatial_HAC.do


 
/*----------------------------------------------------*/
   /* [>   3.  Download necessary other programs   <] */ 
/*----------------------------------------------------*/
cap ssc install acreg
cap ssc install spatial_hac_iv



 
/*----------------------------------------------------*/
   /* [>   4.  Run files for replication   <] */ 
/*----------------------------------------------------*/
do ${code}/table1.do





