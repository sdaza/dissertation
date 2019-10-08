#delimit ;

capture log close ;
capture clear ;
capture clear matrix ;
capture program drop _all ;
set mem 750m ;
set matsize 2000 ;
set more off ;

global input "C:\Users\wodtke\Desktop\projects\nhood_msm_educ\data\" ;
global output "C:\Users\wodtke\Desktop\projects\nhood_msm_educ\data\" ;
global library "C:\Users\wodtke\Desktop\projects\nhood_msm_educ\programs\_LOGS\" ;

log using "${library}v17_create_tables.log", replace ;

/*****************************************************
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Name: v17_create_tables
Purpose: descriptive and multivariate analyses
Author: GW
Notes:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
******************************************************/


/************
~~~~~~~~~~~~~
DEFINE MACROS
~~~~~~~~~~~~~
*************/
capture global drop baseline time_dep avg_time_dep time ;
global baseline		female mmarr mage birthwt
					lesshshd_b hsgradhd_b marhd_b emphd_b afdc_b ownhome_b lnfaminc_b workhrhd_b famsize_b
					year_b_70_72 year_b_73_75 year_b_76_78 ;
global time_dep		marhd emphd ownhome afdc lnfaminc workhrhd famsize cum_moves 
					marhd_l1 emphd_l1 ownhome_l1 afdc_l1 lnfaminc_l1 workhrhd_l1 famsize_l1
					marhd_int emphd_int
					;
global avg_time_dep	avg_marhd_2_17 avg_emphd_2_17 avg_lnfaminc_2_17 avg_famsize_2_17
					avg_workhrhd_2_17 avg_afdc_2_17 avg_ownhome_2_17 cum_moves_1_17 ;
global time			wave7_11 wave12_17 ;


/******************************
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TABLE 1: SAMPLE CHARACTERISTICS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*******************************/
use "${input}v16_psid_mi_final.dta", clear ;

/***define program - calculate descriptive stats - continuous vars***/
capture program drop c_describe ;
program define c_describe ;
	local var `1' ;
		di "^^^^^^^^^^^^^^^missing data^^^^^^^^^^^^^^^^^^^" ;
		codebook `var' if _mj==0 & complete==1 ;
		di "^^^^^^^^^^^^age 1 - baseline wave^^^^^^^^^^^^^" ;
		tabstat `var' if _mj==1 & time==1  & complete==1, by(blk) statistics(mean sd n) ;
		di "^^^^^^^^^^^^^^^^^^age 10^^^^^^^^^^^^^^^^^^^^^^" ;
		tabstat `var' if _mj==1 & time==10 & complete==1, by(blk) statistics(mean sd n) ;
		di "^^^^^^^^^^^^^^^^^^age 17^^^^^^^^^^^^^^^^^^^^^^" ;
		tabstat `var' if _mj==1 & time==17 & complete==1, by(blk) statistics(mean sd n) ;
end ;

/***define program - calculate descriptive stats - categorical vars***/
capture program drop n_describe ;
program define n_describe ;
	local var `1' ;
		di "^^^^^^^^^^^^^^^missing data^^^^^^^^^^^^^^^^^^^" ;
		tab `var' if _mj==0 & complete==1, missing ;
		di "^^^^^^^^^^^^age 1 - baseline wave^^^^^^^^^^^^^" ;
		tab `var' blk if _mj==1 & time==1  & complete==1, col ;
		di "^^^^^^^^^^^^^^^^^^age 10^^^^^^^^^^^^^^^^^^^^^^" ;
		tab `var' blk if _mj==1 & time==10 & complete==1, col ;
		di "^^^^^^^^^^^^^^^^^^age 17^^^^^^^^^^^^^^^^^^^^^^" ;
		tab `var' blk if _mj==1 & time==17 & complete==1, col ;
end ;

/***time-invariant vars***/
n_describe hsgrad ;
n_describe female ;
n_describe birthwt ;
n_describe mmarr ;
n_describe lesshshd ;
n_describe hsgradhd ;
c_describe mage ;

/***time dependent vars***/
n_describe di_qnt ;
n_describe marhd ;
n_describe emphd ;
n_describe afdc ;
n_describe ownhome ; 
c_describe faminc ;
c_describe workhrhd ;
c_describe famsize ;
c_describe agehd ;
n_describe moved ; 
c_describe cum_moves ;


/*****************************************
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TABLE 2: SUMMARY OF TREATMENT TRAJECTORIES
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
******************************************/
use "${input}v16_psid_mi_final.dta", clear ;

/***cum waves in disadvantaged nhood (5th quintile)***/
tab cum_di_q5_2_17 blk		if _mj==1 & time==17 & complete==1, col ;

/***num moves in/out of disadvantaged nhood (5th quintile)***/
tab cum_di_q5_chng blk 		if _mj==1 & time==17 & complete==1, col ;

/***average ordinal treatment***/
tab avg_di_qnt_cat blk		if _mj==1 & time==17 & complete==1, col ;

/***number of moves between ordinal levels of nhood disadvantage***/
tab cum_di_qnt_chng blk		if _mj==1 & time==17 & complete==1, col ;


/*************************************************************
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TABLES 3-4: (3) SUMMARY OF WEIGHTS AND (4) NH EFFECT ESTIMATES
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
**************************************************************/
use "${input}v16_psid_mi_final.dta", replace ;
sort _mj uniq_prs_id time ;

/***define program - mi combined unadjusted estimates***/
capture program drop mi_unadj_mdl ;
program define mi_unadj_mdl ;
	local exp1	`1' ;
	local exp2	`2' ;
	local race	`3' ;
	/*
		di "^^^^^^^^^^^^^^^^^^^^^^^^^MODEL 1 UNADJUSTED^^^^^^^^^^^^^^^^^^^^^^^^^" ;
		micombine 	logit	hsgrad `exp1'_14
					if blk==`race' & time==17 & complete==1 
					, cluster(uniq_fam_id) ;
		di "^^^^^^^^^^^^^^^^^^^^^^^^^MODEL 2 UNADJUSTED^^^^^^^^^^^^^^^^^^^^^^^^^" ;
		micombine 	logit 	hsgrad cum_`exp1'_2_17 
					if blk==`race' & time==17 & complete==1 
					, cluster(uniq_fam_id) ;
		di "^^^^^^^^^^^^^^^^^^^^^^^^^MODEL 3 UNADJUSTED^^^^^^^^^^^^^^^^^^^^^^^^^" ;
		micombine 	logit	hsgrad `exp2'_14
					if blk==`race' & time==17 & complete==1 
					, cluster(uniq_fam_id) ;
	*/
		di "^^^^^^^^^^^^^^^^^^^^^^^^^MODEL 4 UNADJUSTED^^^^^^^^^^^^^^^^^^^^^^^^^" ;
		micombine 	logit 	hsgrad avg_`exp2'_2_17 
					if blk==`race' & time==17 & complete==1 
					, cluster(uniq_fam_id) ;	
end ;

/***define program - mi combined regression adjusted estimates***/
capture program drop mi_regadj_mdl ;
program define mi_regadj_mdl ;
	local exp1	`1' ;
	local exp2	`2' ;
	local race	`3' ;
	/*
		di "^^^^^^^^^^^^^^^^^^^^^^^^^MODEL 1 REGRESSION ADJUSTED^^^^^^^^^^^^^^^^^^^^^^^^^" ;
		micombine	logit 	hsgrad `exp1'_14 `exp1'_b $baseline $avg_time_dep 
					if blk==`race' & time==17 & complete==1 
					, cluster(uniq_fam_id) ;
		di "^^^^^^^^^^^^^^^^^^^^^^^^^MODEL 2 REGRESSION ADJUSTED^^^^^^^^^^^^^^^^^^^^^^^^^" ;		
		micombine	logit	hsgrad cum_`exp1'_2_17 `exp1'_b $baseline $avg_time_dep 
					if blk==`race' & time==17 & complete==1 
					, cluster(uniq_fam_id) ;
		di "^^^^^^^^^^^^^^^^^^^^^^^^^MODEL 3 REGRESSION ADJUSTED^^^^^^^^^^^^^^^^^^^^^^^^^" ;
		micombine	logit 	hsgrad `exp2'_14 `exp2'_b $baseline $avg_time_dep 
					if blk==`race' & time==17 & complete==1 
					, cluster(uniq_fam_id) ;					
	*/
		di "^^^^^^^^^^^^^^^^^^^^^^^^^MODEL 4 REGRESSION ADJUSTED^^^^^^^^^^^^^^^^^^^^^^^^^" ;
		micombine	logit	hsgrad avg_`exp2'_2_17 `exp2'_b $baseline $avg_time_dep 
					if blk==`race' & time==17 & complete==1 
					, cluster(uniq_fam_id) ;
end ;

/*
/***define program - mi combined iptw estimates for binary treatment effect***/
capture program drop mi_iptw_bin_mdl ;
program define mi_iptw_bin_mdl ;
	
	/*define local macros*/
	local exp	`1' ;
	local race	`2' ;
		
		/*compute stabilized treatment weights - binary treatment*/
		gen p1=. ;
		forval i=1/5 	{ ;
						quietly logit 	`exp' `exp'_l1 `exp'_b $baseline $time_dep $time 
										if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						predict y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p1=y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						drop y ;
						} ;
		gen p_denom=p1*`exp'+(1-p1)*(1-`exp') if blk==`race' & inrange(time,2,17) & _mj!=0 ;
		replace p_denom=1 if blk==`race' & time==1 & _mj!=0 ;
		sort _mj uniq_prs_id time ;
		by _mj uniq_prs_id: replace p_denom=p_denom*p_denom[_n-1] if _n!=1 & blk==`race' & _mj!=0 ;

		gen p2=. ;
		forval i=1/5 	{ ;
						quietly logit 	`exp' `exp'_l1 `exp'_b $baseline $time 
										if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						predict y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p2=y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						drop y ;
						} ;
		gen p_num=p2*`exp'+(1-p2)*(1-`exp') if blk==`race' & inrange(time,2,17) & _mj!=0 ;
		replace p_num=1 if blk==`race' & time==1 & _mj!=0 ;
		sort _mj uniq_prs_id time ;
		by _mj uniq_prs_id: replace p_num=p_num*p_num[_n-1] if _n!=1 & blk==`race' & _mj!=0 ;
		gen sw=. ;
		replace sw=p_num/p_denom if blk==`race' & _mj!=0 ;
		drop p1 p2 p_num p_denom ;
		
		/*compute stabilized censoring weights*/
		gen p1=. ;
		forval i=1/5 	{ ;
						quietly logit cens `exp'_l1 `exp'_b $baseline $time_dep $time 
											if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						predict y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace y=1-y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p1=y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						drop y ;
						} ;
		sort _mj uniq_prs_id time ;
		gen p_denom=1 if blk==`race' & time==1 & _mj!=0 ;
		replace p_denom=p1 if blk==`race' & time==2 & _mj!=0 ;
		by _mj uniq_prs_id: replace p_denom=p1*p_denom[_n-1] if _n!=1 & blk==`race' & _mj!=0 ;
		
		gen p2=. ;
		forval i=1/5 	{ ;
						quietly logit cens `exp'_l1 `exp'_b $baseline $time if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						predict y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace y=1-y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p2=y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						drop y ;
						} ;
		sort _mj uniq_prs_id time ;
		gen p_num=1 if blk==`race' & time==1 & _mj!=0 ;
		replace p_num=p2 if blk==`race' & time==2 & _mj!=0 ;
		by _mj uniq_prs_id: replace p_num=p2*p_num[_n-1] if _n!=1 & blk==`race' & _mj!=0 ;
		gen cw=. ;
		replace cw=p_num/p_denom if blk==`race' & _mj!=0 ;
		drop p1 p2 p_num p_denom ;
		
		/*compute final weight*/
		gen swcw=. ;
		replace swcw=sw*cw if blk==`race' & _mj!=0 ;
		replace sw=10 if sw>10 & time==17 & complete==1 & blk==`race' & _mj!=0 ;
		replace cw=10 if cw>10 & time==17 & complete==1 & blk==`race' & _mj!=0 ;
		replace swcw=10 if swcw>10 & time==17 & complete==1 & blk==`race' & _mj!=0 ;

		/*weight descriptive statistics by mi dataset*/
		tabstat sw if time==17 & complete==1 & blk==`race'
				, by(_mj) statistics(n mean sd p1 p25 p75 p99) ;
		tabstat cw if time==17 & complete==1 & blk==`race'
				, by(_mj) statistics(n mean sd p1 p25 p75 p99) ;
		tabstat swcw if time==17 & complete==1 & blk==`race'
				, by(_mj) statistics(n mean sd p1 p25 p75 p99) ;
		
		
		/*compute mi combined iptw estimates*/
		di "^^^^^^^^^^^^^^^^^^^^^^^^^MODEL 1 STABILIZED IPTW^^^^^^^^^^^^^^^^^^^^^^^^^" ;
		micombine logit 	hsgrad `exp'_14 `exp'_b $baseline [pw=swcw]
							if blk==`race' & time==17 & complete==1 
							, cluster(uniq_fam_id) ;
		di "^^^^^^^^^^^^^^^^^^^^^^^^^MODEL 2 STABILIZED IPTW^^^^^^^^^^^^^^^^^^^^^^^^^" ;		
		micombine logit 	hsgrad cum_`exp'_2_17 `exp'_b $baseline [pw=swcw]
							if blk==`race' & time==17 & complete==1 
							, cluster(uniq_fam_id) ;
		est stats ;
		
		/*drop temp vars*/
		drop sw cw swcw ;
end ;
*/

/***define program - mi combined iptw estimates for ordinal treatment effect***/
capture program drop mi_iptw_ord_mdl ;
program define mi_iptw_ord_mdl ;
	
	/*define local macros*/
	local exp	`1' ;
	local race	`2' ;

		/*compute stabilized treatment weights - ordinal treatment*/
		gen p1=. ; 
		gen p2=. ; 
		gen p3=. ; 
		gen p4=. ; 
		gen p5=. ;
		forval i=1/5 	{ ;
						quietly ologit 	`exp' `exp'_l1 `exp'_b $baseline $time_dep $time 
										if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						predict y1-y5 if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p1=y1 if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p2=y2 if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p3=y3 if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p4=y4 if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p5=y5 if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						drop y1-y5 ;
						} ;
		quietly tab `exp', generate(a) ;
		gen p_denom=(p1*a1)+(p2*a2)+(p3*a3)+(p4*a4)+(p5*a5) if blk==`race' & inrange(time,2,17) & _mj!=0 ;
		replace p_denom=1 if blk==`race' & time==1 & _mj!=0 ;
		sort _mj uniq_prs_id time ;
		by _mj uniq_prs_id: replace p_denom=p_denom*p_denom[_n-1] if _n!=1 & blk==`race' & _mj!=0 ;
		drop p1-p5 ;

		gen p1=. ; 
		gen p2=. ; 
		gen p3=. ; 
		gen p4=. ; 
		gen p5=. ;
		forval i=1/5 	{ ;
						quietly ologit 	`exp' `exp'_l1 `exp'_b $baseline $time 
										if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						predict y1-y5 if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p1=y1 if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p2=y2 if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p3=y3 if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p4=y4 if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p5=y5 if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						drop y1-y5 ;
						} ;
						
		gen p_num=(p1*a1)+(p2*a2)+(p3*a3)+(p4*a4)+(p5*a5) if blk==`race' & inrange(time,2,17) & _mj!=0 ;
		replace p_num=1 if blk==`race' & time==1 & _mj!=0 ;
		sort _mj uniq_prs_id time ;
		by _mj uniq_prs_id: replace p_num=p_num*p_num[_n-1] if _n!=1 & blk==`race' & _mj!=0 ;
		gen sw=. ;
		replace sw=p_num/p_denom if blk==`race' & _mj!=0 ;
		drop p1-p5 a1-a5 p_num p_denom ;				
		
		/*compute stabilized censoring weights*/
		gen p1=. ;
		forval i=1/5 	{ ;
						quietly logit cens `exp'_l1 `exp'_b $baseline $time_dep $time 
											if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						predict y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace y=1-y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p1=y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						drop y ;
						} ;
		sort _mj uniq_prs_id time ;
		gen p_denom=1 if blk==`race' & time==1 & _mj!=0 ;
		replace p_denom=p1 if blk==`race' & time==2 & _mj!=0 ;
		by _mj uniq_prs_id: replace p_denom=p1*p_denom[_n-1] if _n!=1 & blk==`race' & _mj!=0 ;
		
		gen p2=. ;
		forval i=1/5 	{ ;
						quietly logit cens `exp'_l1 `exp'_b $baseline $time if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						predict y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace y=1-y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						replace p2=y if blk==`race' & inrange(time,2,17) & _mj==`i' ;
						drop y ;
						} ;
		sort _mj uniq_prs_id time ;
		gen p_num=1 if blk==`race' & time==1 & _mj!=0 ;
		replace p_num=p2 if blk==`race' & time==2 & _mj!=0 ;
		by _mj uniq_prs_id: replace p_num=p2*p_num[_n-1] if _n!=1 & blk==`race' & _mj!=0 ;
		gen cw=. ;
		replace cw=p_num/p_denom if blk==`race' & _mj!=0 ;
		drop p1 p2 p_num p_denom ;
		
		/*compute final weight*/
		gen swcw=. ;
		replace swcw=sw*cw if blk==`race' & _mj!=0 ;

		/*truncate wts at 1st and 99th percentile*/
		replace sw=2.544418 if sw>2.544418 & time==17 & complete==1 & blk==0 & _mj!=0 ;
		replace sw=.3247273 if sw<.3247273 & time==17 & complete==1 & blk==0 & _mj!=0 ;
		replace sw=4.622694 if sw>4.622694 & time==17 & complete==1 & blk==1 & _mj!=0 ;
		replace sw=.2721064 if sw<.2721064 & time==17 & complete==1 & blk==1 & _mj!=0 ;
				
		replace cw=1.701428 if cw>1.701428 & time==17 & complete==1 & blk==0 & _mj!=0 ;
		replace cw=.7091097 if cw<.7091097 & time==17 & complete==1 & blk==0 & _mj!=0 ;
		replace cw=1.435707 if cw>1.435707 & time==17 & complete==1 & blk==1 & _mj!=0 ;
		replace cw=.7600925 if cw<.7600925 & time==17 & complete==1 & blk==1 & _mj!=0 ;
				
		replace swcw=2.998196 if swcw>2.998196 & time==17 & complete==1 & blk==0 & _mj!=0 ;
		replace swcw=.3245125 if swcw<.3245125 & time==17 & complete==1 & blk==0 & _mj!=0 ;
		replace swcw=4.801079 if swcw>4.801079 & time==17 & complete==1 & blk==1 & _mj!=0 ;
		replace swcw=.2621777 if swcw<.2621777 & time==17 & complete==1 & blk==1 & _mj!=0 ;
		
		/*weight descriptive statistics by mi dataset*/
		tabstat sw if time==17 & complete==1 & blk==`race'
				, by(_mj) statistics(n mean sd min p25 p75 max) ;
		tabstat cw if time==17 & complete==1 & blk==`race'
				, by(_mj) statistics(n mean sd min p25 p75 max) ;
		tabstat swcw if time==17 & complete==1 & blk==`race'
				, by(_mj) statistics(n mean sd min p25 p75 max) ;
		
		/*compute mi combined iptw estimates*/
		/*
		di "^^^^^^^^^^^^^^^^^^^^^^^^^MODEL 3 STABILIZED IPTW^^^^^^^^^^^^^^^^^^^^^^^^^" ;
		micombine logit 	hsgrad `exp'_14 `exp'_b $baseline [pw=swcw]
							if blk==`race' & time==17 & complete==1 
							, cluster(uniq_fam_id) ;
		*/
		di "^^^^^^^^^^^^^^^^^^^^^^^^^MODEL 4 STABILIZED IPTW^^^^^^^^^^^^^^^^^^^^^^^^^" ;
		micombine logit 	hsgrad avg_`exp'_2_17 `exp'_b $baseline [pw=swcw] 
							if blk==`race' & time==17 & complete==1 
							, cluster(uniq_fam_id) ;

		est stats ;
		
		/*drop temp vars*/
		drop sw cw swcw ;
end ;

/***NHOOD EFFECTS - BLACKS***/
mi_unadj_mdl 	di_q5 	di_qnt 	1 ;
mi_regadj_mdl 	di_q5 	di_qnt 	1 ;
*mi_iptw_bin_mdl	di_q5 		1 ;
mi_iptw_ord_mdl	di_qnt 			1 ;

/***NHOOD EFFECTS - NONBLACKS***/
mi_unadj_mdl 	di_q5 	di_qnt 	0 ;
mi_regadj_mdl 	di_q5 	di_qnt 	0 ;
*mi_iptw_bin_mdl	di_q5 		0 ;
mi_iptw_ord_mdl	di_qnt 			0 ;


/************************************************
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
APPENDIX A: NHOOD DISADVANTAGE INDEX DESCRIPTIVES
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*************************************************/
/***A.1: PCA OF CENSUS TRACT CHARACTERISTICS***/
use "${input}v12_ncdb_long_nvars.dta", clear ;
pca trct_poverty trct_unemprt trct_welfare trct_ffh trct_lesshs trct_colgrad trct_prof_mgr, cor com(1) ;
corr trct_disadv_index trct_poverty trct_unemprt trct_welfare trct_ffh trct_lesshs trct_colgrad trct_prof_mgr ;

/***A.2: DISADVANTAGE INDEX DESCRIPTIVES***/
xtile q=trct_disadv_index, n(5) ;
sum trct_poverty trct_unemprt trct_welfare trct_ffh trct_lesshs trct_colgrad trct_prof_mgr if q==1 ;
sum trct_poverty trct_unemprt trct_welfare trct_ffh trct_lesshs trct_colgrad trct_prof_mgr if q==2 ;
sum trct_poverty trct_unemprt trct_welfare trct_ffh trct_lesshs trct_colgrad trct_prof_mgr if q==3 ;
sum trct_poverty trct_unemprt trct_welfare trct_ffh trct_lesshs trct_colgrad trct_prof_mgr if q==4 ;
sum trct_poverty trct_unemprt trct_welfare trct_ffh trct_lesshs trct_colgrad trct_prof_mgr if q==5 ;


/***************************
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
APPENDIX B: SELECTION MODELS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
****************************/
use "${input}v16_psid_mi_final.dta", replace ;

/***BLACKS***/
*micombine logit di_q5 di_q5_l1 di_q5_b 	$baseline $time_dep $time 	if blk==1 & inrange(time,2,17), cluster(uniq_prs_id) ;
micombine ologit di_qnt di_qnt_l1 di_qnt_b 	$baseline $time_dep $time 	if blk==1 & inrange(time,2,17), cluster(uniq_prs_id) ;
micombine ologit di_qnt di_qnt_l1 di_qnt_b 	$baseline 			$time 	if blk==1 & inrange(time,2,17), cluster(uniq_prs_id) ;

/***NONBLACKS***/
*micombine logit di_q5 di_q5_l1 di_q5_b 	$baseline $time_dep $time 	if blk==0 & inrange(time,2,17), cluster(uniq_prs_id) ;
micombine ologit di_qnt di_qnt_l1 di_qnt_b 	$baseline $time_dep $time 	if blk==0 & inrange(time,2,17), cluster(uniq_prs_id) ;
micombine ologit di_qnt di_qnt_l1 di_qnt_b 	$baseline 			$time 	if blk==0 & inrange(time,2,17), cluster(uniq_prs_id) ;


log close ;
