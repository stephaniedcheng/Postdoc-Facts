/* A1. Descriptive Statistics
Name: Stephanie D. Cheng
Date Created: 2-4-19
Date Updated: 2-19-20

This .do file gives descriptive statistics for worker and job characteristics 
(1) at the time of graduation and (2) in the overall sample.

It also performs some methodological checks for what percentage of the sample
is determined by which steps and surveys.

*/
 
global MAIN "H:/SDC_Postdocs"
global DATA "${MAIN}/Data"
global RESULTS "${MAIN}/Results"
global TEMP "${MAIN}/Temp"
global LOOKUPS "${MAIN}/Lookups"

global COUNT "${TEMP}/Count Tables"

*** 0. WEIGHT SURVEY: Create lookup for worker 1st time weight ***

// All weights
use "${DATA}/OOI_fullsample.dta", clear
keep refid refyr wtsurvy*
save "${LOOKUPS}/SDR Full Weights.dta", replace

// 1st time and average weight
use "${LOOKUPS}/SDR Full Weights.dta", clear
keep refid wtsurvy1_f wtsurvyAvg_f
duplicates drop
save "${LOOKUPS}/SDR First and Avg Weights.dta", replace

*** 1. WORKER CHARACTERISTICS ***

	** a. NON-CHANGING **
	use "${DATA}/OOI_workerchar.dta", clear
	
		// Distribution of Fields
		tab phd_supField if phdcy_min==refyr [aw=wtsurvy1_f]
		tab phd_supField if phdcy_min==refyr & STEM==1 [aw=wtsurvy1_f]
	
		// Percent Female
		tab phd_supField male if phdcy_min==refyr [aw=wtsurvy1_f], row
		tab phd_supField male if phdcy_min==refyr & STEM==1 [aw=wtsurvy1_f], row
		tab phd_supField male if phdcy_min==refyr & STEM==1 & phdcy_min>=1990 & phdcy_min<2000 [aw=wtsurvy1_f], row
	
	keep if STEM==1
	
	// Has professional degree at time of PhD graduation
	gen indProf = (profdeg!=.)
	
	// Underrepresented minority
	gen R_URM = (R_hisp==1 | R_black==1 | R_namer==1)
	
	// Keep one copy
	keep refid male R_* USnative bacarn_* macarn_* phdcarn_* gradYrs indProf wtsurvy1_f phd_supField
	duplicates drop
	
		// Save non-changing characteristics
		sum male R_* USnative *carn_* gradYrs indProf [aw=wtsurvy1_f]
		sum male R_* USnative *carn_* gradYrs indProf [aw=wtsurvy1_f] if phd_supField=="Bio Sciences"
		sum male R_* USnative *carn_* gradYrs indProf [aw=wtsurvy1_f] if phd_supField=="Chemistry"
		sum male R_* USnative *carn_* gradYrs indProf [aw=wtsurvy1_f] if phd_supField=="Engineering"
		sum male R_* USnative *carn_* gradYrs indProf [aw=wtsurvy1_f] if phd_supField=="Physics"

		// Unweighted for disclosure purposes
		sum male R_* USnative *carn_* gradYrs indProf if phd_supField=="Bio Sciences"
		sum male R_* USnative *carn_* gradYrs indProf if phd_supField=="Chemistry"
		sum male R_* USnative *carn_* gradYrs indProf if phd_supField=="Engineering"
		sum male R_* USnative *carn_* gradYrs indProf if phd_supField=="Physics"
		
	** b. AT GRADUATION **
	use "${DATA}/OOI_workerchar.dta", clear
	keep if STEM==1	& refyr==phdcy_min
	
		// Save non-changing characteristics
		sum age US* married anyChild [aw=wtsurvy1_f]
		sum age US* married anyChild [aw=wtsurvy1_f] if phd_supField=="Bio Sciences"
		sum age US* married anyChild [aw=wtsurvy1_f] if phd_supField=="Chemistry"
		sum age US* married anyChild [aw=wtsurvy1_f] if phd_supField=="Engineering"
		sum age US* married anyChild [aw=wtsurvy1_f] if phd_supField=="Physics"

		// Unweighted for disclosure purposes
		sum age US* married anyChild if phd_supField=="Bio Sciences"
		sum age US* married anyChild if phd_supField=="Chemistry"
		sum age US* married anyChild if phd_supField=="Engineering"
		sum age US* married anyChild if phd_supField=="Physics"
	
	
	** c. OVERALL **
	use "${DATA}/OOI_workerchar.dta", clear
	keep if STEM==1
	
		// Mean age
		bys refid: egen MEANage = mean(age)
		
		// Determine if ever change marital status, parental status, citizenship status
		tab married
		tab anyChild
		tab UScit
		tab USpr
		
		// Ever married, parent
		foreach i in married anyChild {
			bys refid: egen ever`i' = max(`i')
		}
		
		// Job type experience
		foreach i in PD AC TE GV ID NP UN NL {
			bys refid: egen MAXyrs`i' = max(yrs`i')
			replace MAXyrs`i' = . if MAXyrs`i'==0
			gen ever`i' = MAXyrs`i'!=.
		}
	
	// Keep one copy
	keep refid phdcy_min phd_supField ever* MAX* MEANage wtsurvy1_f phd_supField
	duplicates drop
	
		// Save overall characteristics
		sum ever* MAX* MEANage [aw=wtsurvy1_f]

		sum ever* MAX* MEANage [aw=wtsurvy1_f] if phd_supField=="Bio Sciences"
		sum ever* MAX* MEANage [aw=wtsurvy1_f] if phd_supField=="Chemistry"
		sum ever* MAX* MEANage [aw=wtsurvy1_f] if phd_supField=="Engineering"
		sum ever* MAX* MEANage [aw=wtsurvy1_f] if phd_supField=="Physics"

		// Unweighted for disclosure purposes
		sum ever* MAX* MEANage if phd_supField=="Bio Sciences"
		sum ever* MAX* MEANage if phd_supField=="Chemistry"
		sum ever* MAX* MEANage if phd_supField=="Engineering"
		sum ever* MAX* MEANage if phd_supField=="Physics"		
			
		// Break up mean postdoc years by field
		bys phd_supField: sum MAXyrsPD [aw=wtsurvy1_f]
		
		// Break up ever in job for pre- and post- 2000
		sum ever* [aw=wtsurvy1_f] if phdcy_min>=1960 & phdcy_min<=1980
		sum ever* [aw=wtsurvy1_f] if phdcy_min>=2000
		
*** 2. JOB CHARACTERISTICS ***	

	** a. JOB TENURE **
	use "${DATA}/OOI_workingsample.dta", clear	
	keep if jobID!=. & STEM==1
	
	bys jobID: gen jobTenure = _N
	
	keep jobID jobTenure wtsurvy1_f phd_supField
	duplicates drop
	
		// Summarize job tenure year
		sum jobTenure [aw=wtsurvy1_f]
		sum jobTenure [aw=wtsurvy1_f] if phd_supField=="Bio Sciences"
		sum jobTenure [aw=wtsurvy1_f] if phd_supField=="Chemistry"
		sum jobTenure [aw=wtsurvy1_f] if phd_supField=="Engineering"
		sum jobTenure [aw=wtsurvy1_f] if phd_supField=="Physics"

	
	** b. OVERALL **
	use "${DATA}/OOI_workingsample.dta", clear	
	
	// Keep if actual job of STEM individual
	keep if jobID!=. & STEM==1	
	
	// Indicator for job type
	foreach i in PD AC TE GV ID NP {
		gen ind`i' = (`i'i>0 & `i'i!=.)
	}
		
		// Summarize overall sample
		sum ind* tenured emsize newbus fs* govsup SAL* jobins jobpens jobproft jobvac ///
			hrswk fullTimeP act* wa* mgr* sup* [aw=wtsurvy1_f]
		tab wapri if wapri!="L" & wapri!="LL"	
		
		sum SAL* jobins jobpens jobproft jobvac hrswk fullTimeP mgr* sup* [aw=wtsurvy1_f] if phd_supField=="Bio Sciences"
		sum SAL* jobins jobpens jobproft jobvac hrswk fullTimeP mgr* sup* [aw=wtsurvy1_f] if phd_supField=="Chemistry"
		sum SAL* jobins jobpens jobproft jobvac hrswk fullTimeP mgr* sup* [aw=wtsurvy1_f] if phd_supField=="Engineering"
		sum SAL* jobins  jobpens jobproft jobvac hrswk fullTimeP mgr* sup* [aw=wtsurvy1_f] if phd_supField=="Physics"

		tab wapri [aw=wtsurvy1_f] if phd_supField=="Bio Sciences" & wapri!="L" & wapri!="LL"
		tab wapri [aw=wtsurvy1_f] if phd_supField=="Chemistry" & wapri!="L" & wapri!="LL"
		tab wapri [aw=wtsurvy1_f] if phd_supField=="Engineering" & wapri!="L" & wapri!="LL"
		tab wapri [aw=wtsurvy1_f] if phd_supField=="Physics" & wapri!="L" & wapri!="LL"
		
		// Unweighted for disclosure purposes
		sum SAL* jobins jobpens jobproft jobvac hrswk fullTimeP mgr* sup* if phd_supField=="Bio Sciences"
		sum SAL* jobins jobpens jobproft jobvac hrswk fullTimeP mgr* sup* if phd_supField=="Chemistry"
		sum SAL* jobins jobpens jobproft jobvac hrswk fullTimeP mgr* sup* if phd_supField=="Engineering"
		sum SAL* jobins jobpens jobproft jobvac hrswk fullTimeP mgr* sup* if phd_supField=="Physics"				
		
		tab wapri if phd_supField=="Bio Sciences" & wapri!="L" & wapri!="LL"
		tab wapri if phd_supField=="Chemistry" & wapri!="L" & wapri!="LL"
		tab wapri if phd_supField=="Engineering" & wapri!="L" & wapri!="LL"
		tab wapri if phd_supField=="Physics" & wapri!="L" & wapri!="LL"
			
		// Sum work activities by principal job type (only if single principal job)
		egen tempTot = rowtotal(pj*)		
		
		foreach i in PD AC TE ID NP GV {
			display "`i'"
			*capture noisily sum act* wa* [aw=wtsurvy1_f] if pj`i'==1 & tempTot==1
			tab wapri [aw=wtsurvy1_f] if pj`i'==1 & tempTot==1 & wapri!="L" & wapri!="LL" 
			*tab wapri [aw=wtsurvy1_f] if pj`i'==1 & tempTot==1 & phd_supField=="Bio Sciences" & wapri!="L" & wapri!="LL" 
		}
		
		// Merge on Carnegie classifications for current institutions
		merge m:1 refyr instcod using "${LOOKUPS}/CCbyYear_max.dta"
		drop if _merge==2
		drop _merge

		// Sum work activities for R1 academic positions
		foreach i in PD AC TE {
		    display "`i'"
			tab wapri [aw=wtsurvy1_f] if pj`i'==1 & tempTot==1 & R1==1
		}
	
	// # of cells that have salary data
	count if SALi!=.
	count
	
*** 3. INDICATOR STEP HIERARCHY ***
// This is meant for the methodology section and describes what percentage of indicators are given by each step in the hierarchy
use "${DATA}/OOI_workingsample.dta", clear

	// Only keep if in STEM
	keep if STEM==1

	foreach i in PD AC TE GV ID NP UN NL {
		tab `i' if `i'!=0
	}

	// Missing information
	tab NIi, m
	tab NIi
	
	// How much explained by older cohorts
	gen pre1991 = (phdcy_min<=1991)
	tab pre1991
	
	
*** 4. LIST SURVEYS EACH REFID ARE IN ***
use "${TEMP}/sdrdrf93_IDs.dta", clear
	gen DRF93 = 1
	keep refid DRF93

	append using "${DATA}/Stata/esdr93.dta", keep(refid) gen(SDR93)

	// Refids in each DRF & SDR
	foreach yr in 95 97 99 01 03 06 08 10 13 15 {

		append using "${TEMP}/sdrdrf`yr'_IDs.dta", keep(refid) gen(DRF`yr')
	 
		append using "${DATA}/Stata/esdr`yr'.dta", keep(refid) gen(SDR`yr')

	}

	order refid DRF* SDR*
	sort refid

	// Sum up indicators across DRF & SDR years
	foreach i in DRF SDR {
		foreach yr in 93 95 97 99 01 03 06 08 10 13 15 {

			bys refid: egen max`i'`yr' = max(`i'`yr')
		
		}
	}

	// Create one copy for each refid
	keep refid max*
	duplicates drop

	// # of times appears in DRF, SDR
	egen numDRF = rowtotal(maxDRF*)
	egen numSDR = rowtotal(maxSDR*)

	// Merge on birth year and PhD graduation so have sense of expected # of times should show up
	merge 1:1 refid using "${DATA}/sdrdrf_FULL.dta", keepusing(byear phdcy_min)
	drop _merge

		// Last possible survey year
		replace byear = . if byear ==-1
		gen lastYr = byear + 76
		
		// Expected in SDR years
		foreach yr in 93 95 97 99 01 03 06 08 10 13 15 {
		
			gen expSDR`yr' = 0
			
			if inlist(`yr', 93, 95, 97, 99) {
				replace expSDR`yr' = (`yr' + 1900 >= phdcy_min & `yr' + 1900 <= lastYr)
			}
			if inlist(`yr', 01, 03, 06, 08, 10, 13, 15) {
				replace expSDR`yr' = (`yr' + 2000 >= phdcy_min & `yr' + 2000 <= lastYr)
			}
		}
		
		egen numExpSDR = rowtotal(expSDR*)
		
	// Compare to actual #
	tab numSDR numExpSDR, col
	tab numSDR numExpSDR if lastYr!=., col

	// In between surveys missing

		// Sum up # SDR each year
		foreach yr in 93 95 97 99 01 03 06 08 10 13 15 {
		
			gen yrsSDR`yr' = 0
			
			foreach yr2 in 95 97 99 01 03 06 08 10 13 15 {
				replace yrsSDR`yr' = yrsSDR`yr' + maxSDR`yr2'
			}
		}
		
		gen lastSDR

	save "${TEMP}/Survey Availability.dta", replace	
	
*** 5. # of Individuals By Cohort, Years Since PhD ***

// a. Individuals in Each Graduating Cohort
use "${DATA}/OOI_workerchar.dta", clear

	// Keep unique copy
	keep refid phdcy_min
	duplicates drop
	
	// Collapse by count (so that can remove fewer than 50 individuals)
	gen count = 1
	collapse (sum) count, by(phdcy_min)
	
	// Graph
	scatter count phdcy_min if count>=50, title("# of Individuals in Each Graduating Cohort") ///
		xtitle("PhD Graduating Cohort") ytitle("Number of Individuals") xlab(1940(10)2020) ///
		note("Note: Shows groups of at least 50 individuals.")
	graph export "${LOOKUPS}/Individuals Per Graduating Cohort.pdf", as(pdf) replace

	// Counts Table
	replace count = . if count<50
	save "${COUNT}/Individuals Per Graduating Cohor.dta", replace	
	
// b. Job Info in Each YrsOut by Graduating Decade
use "${DATA}/OOI_workingsample.dta", clear

	// Keep if actual job (not UN or NL)
	keep if jobID!=. & UNi==0 & NLi==0

	// Years since PhD
	gen yrsOut = refyr - phdcy_min	
	
	// Decade of Graduation
	gen phd_dec = floor(phdcy_min/10)*10
	
	// Collapse by count
	gen count = 1
	collapse (sum) count, by(yrsOut phd_dec)
	
	// Reshape for each years out
	reshape wide count, i(yrsOut) j(phd_dec)
	
	// Graph
	twoway (connected count1940 yrsOut if count1940>=50, mcolor(navy) lcolor(navy)) ///
			(connected count1950 yrsOut if count1950>=50, mcolor(maroon) lcolor(maroon)) ///
			(connected count1960 yrsOut if count1960>=50, mcolor(forest_green) lcolor(forest_green)) ///
			(connected count1970 yrsOut if count1970>=50, mcolor(dkorange) lcolor(dkorange)) ///
			(connected count1980 yrsOut if count1980>=50, mcolor(teal) lcolor(teal)) ///
			(connected count1990 yrsOut if count1990>=50, mcolor(purple) lcolor(purple)) ///
			(connected count2000 yrsOut if count2000>=50, mcolor(black) lcolor(black)) ///
			(connected count2010 yrsOut if count2010>=50, mcolor(gray) lcolor(gray)), ///
			legend(order(1 "1940" 2 "1950" 3 "1960" 4 "1970" 5 "1980" 6 "1990" 7 "2000") cols(4)) ///
			xtitle("Years Since PhD") xlab(0(5)50) ytitle("Number of Jobs") ///
			title("# with Job Info in Each Year Out by Grad Decade") ///
			note("Note: Shows groups of at least 50 individuals.")
	graph export "${LOOKUPS}/Job Info YrsOut by Decade.pdf", as(pdf) replace
	
	// Counts Table
	forvalues i=1940(10)2010 {
		replace count`i' = . if count`i'<50 
	}
	save "${COUNT}/Job Info YrsOut by Decade.dta", replace
	
*** 6. POSTDOC FACTS (Cited in paper) ***

// 1a. Time in Graduate School
// 1b. Time Out of Graduate School
use "${DATA}/OOI_workerchar.dta", clear

	// Keep unique copy 
	keep refid wtsurvy1_f phdcy_min gradYrs togephd
	duplicates drop
	
	// Average 1a. Time in Graduate School
	sum gradYrs [aw=wtsurvy1_f] if phdcy_min>=1960 & phdcy_min<=1980
	sum gradYrs [aw=wtsurvy1_f] if phdcy_min>=2000
	
	// Average 1b. Time Out of Graduate School
	sum togephd [aw=wtsurvy1_f] if phdcy_min>=1960 & phdcy_min<=1980
	sum togephd [aw=wtsurvy1_f] if phdcy_min>=2000

// 2. Early Postdoc Takeup
use "${DATA}/OOI_workingsample.dta", clear

	// Keep 2 years after PhD
	keep if (refyr-phdcy_min)==2
	
	// Any postdocs during this time
	gen earlyPD = (yrsPD>0)

	// Average 2. Early Postdoc Takeup
	sum earlyPD [aw=wtsurvy1_f] if phdcy_min>=1960 & phdcy_min<=1980
	sum earlyPD [aw=wtsurvy1_f] if phdcy_min>=2000	
	
// 3. Postdoc Years ***
use "${DATA}/OOI_workingsample.dta", clear

	// Determine total years spent in postdocs 
	bys refid: egen maxYrsPD = max(yrsPD)
	
	// Keep 1 copy for each refid
	keep refid wtsurvy1_f phdcy_min maxYrsPD
	duplicates drop
	
	// Average 3. Postdoc Years (unconditional, add to grad yrs)
	sum maxYrsPD [aw=wtsurvy1_f] if phdcy_min>=1960 & phdcy_min<=1980
	sum maxYrsPD [aw=wtsurvy1_f] if phdcy_min>=2000
	
	// Average 3. Postdoc Years, conditional on any postdoc
	sum maxYrsPD [aw=wtsurvy1_f] if maxYrsPD!=0 & phdcy_min>=1970
	
// 4a. Job Distribution Ten Years Post PhD
use "${DATA}/OOI_workingsample.dta", clear

	// Years since PhD
	gen yrsOut = refyr - phdcy_min
	keep if yrsOut==10
	
	// Only include if have job info	
	keep if NIi==0	
	
	// Job type
	foreach i in PD AC TE GV ID NP {
		gen ind`i' = (`i'i!=0 & `i'i!=.)
	}
	
	// Going to combine GV and NP together
	gen indGVNP = (indGV==1 | indNP==1)
	
	// Average 4a. Job Distribution Ten Years Post PhD
	sum ind* [aw=wtsurvy1_f] if phdcy_min>=1960 & phdcy_min<=1980
	sum ind* [aw=wtsurvy1_f] if phdcy_min>=2000
	
// 4b. Ever Transition to Tenure-Track Job
use "${DATA}/OOI_workingsample.dta", clear

	// Ever obtain tenure-track position
		bys refid: egen maxYrsAC = max(yrsAC)
		gen everAC = (maxYrsAC>0)
	
	// Keep 1 copy for each refid
	keep refid wtsurvy1_f phdcy_min everAC
	duplicates drop
	
	// Average 4b. Ever Transition to Tenure-Track Job
	sum everAC [aw=wtsurvy1_f] if phdcy_min>=1960 & phdcy_min<=1980
	sum everAC [aw=wtsurvy1_f] if phdcy_min>=2000	
	
// 4c. Early Transition to Tenure-Track Job
use "${DATA}/OOI_workingsample.dta", clear

	// Keep 2 years after PhD
	keep if (refyr-phdcy_min)==2
	
	// Keep if don't have any postdoc experience
	keep if yrsPD==0
	
	// Hold tenure-track by this time without postdoc
	gen earlyAC = (yrsAC>0 & yrsPD==0)
	
	// Average 4c. Early Transition to Tenure-Track Job
	sum earlyAC [aw=wtsurvy1_f] if phdcy_min>=1960 & phdcy_min<=1980
	sum earlyAC [aw=wtsurvy1_f] if phdcy_min>=2000	
	
// 4d. Postdoc Transition to Tenure-Track Job
use "${TEMP}/OOI_workerchar_transitions.dta", clear

	// Keep only if have postdoc experience
	bys refid: egen maxYrsPD = max(yrsPD)
	gen everPD = (maxYrsPD>0)
	keep if everPD==1
	
	// Keep 1 copy for each refid
	keep refid wtsurvy1_f phdcy_min PDt*s maxYrsPD
	duplicates drop
	
	// Keep only if have info on transition
	keep if PDtNI==0
	
	// Average 4d. Postdoc Transition to Tenure-Track Job
	sum PDtACs [aw=wtsurvy1_f] if phdcy_min>=1960 & phdcy_min<=1980
	sum PDtACs [aw=wtsurvy1_f] if phdcy_min>=2000		
	
// 5a. Early Transition to Other Non-Postdoc Job
use "${DATA}/OOI_workingsample.dta", clear

	// Keep 2 years after PhD
	keep if (refyr-phdcy_min)==2
	
	// Hold non-postdoc position by this time without postdoc
	foreach i in AC TE GV ID NP UN NL {
		gen early`i' = (yrs`i'>0 & yrsPD==0)
	}
	
	// Average 5a. Early Transition to Other Non-Postdoc Job
	sum early* [aw=wtsurvy1_f] if phdcy_min>=1960 & phdcy_min<=1980
	sum early* [aw=wtsurvy1_f] if phdcy_min>=2000
	
// 5b. Postdoc Transition to Other Jobs 
use "${TEMP}/OOI_workerchar_transitions.dta", clear


	// Keep only if have postdoc experience
	bys refid: egen maxYrsPD = max(yrsPD)
	gen everPD = (maxYrsPD>0)
	keep if everPD==1
	
	// Keep 1 copy for each refid
	keep refid wtsurvy1_f phdcy_min PDtACs PDtTEs PDtGVs PDtIDs PDtNPs PDtUNs PDtNLs maxYrsPD
	duplicates drop
	
	// Normalize: only keep if have info
	egen totNum = rowtotal(PDt*s)
	keep if totNum>0
	
	foreach i in AC TE GV ID NP UN NL {
		gen PDt`i's_n = PDt`i's / totNum
	}
	
	// Average 5a. Early Transition to Other Non-Postdoc Job
	sum PDt*s_n [aw=wtsurvy1_f] if phdcy_min>=1960 & phdcy_min<=1980
	sum PDt*s_n [aw=wtsurvy1_f] if phdcy_min>=2000

// 6. Absorbing States
use "${TEMP}/OOI_workerchar_transitions.dta", clear

	// Have job type experience
	foreach i in PD AC TE GV ID NP {
		bys refid: egen maxYrs`i' = max(yrs`i')
		gen ever`i' = (maxYrs`i'>0)
	}

	// Average 6. Absorbing States
	sum PDtPDs [aw=wtsurvy1_f] if phdcy_min>=1970 & everPD==1 & PDtNIs==0
	sum ACtACs [aw=wtsurvy1_f] if phdcy_min>=1970 & everAC==1 & ACtNIs==0
	sum TEtTEs [aw=wtsurvy1_f] if phdcy_min>=1970 & everTE==1 & TEtNIs==0
	sum IDtIDs [aw=wtsurvy1_f] if phdcy_min>=1970 & everID==1 & IDtNIs==0
	sum GVtGVs [aw=wtsurvy1_f] if phdcy_min>=1970 & everGV==1 & GVtNIs==0
	sum NPtNPs [aw=wtsurvy1_f] if phdcy_min>=1970 & everNP==1 & NPtNIs==0

// 7a. Average Salary by Job Type
use "${DATA}/OOI_workingsample.dta", clear

	// Keep if actual job
	keep if jobID!=.

	// Keep only survey years
	keep if inlist(refyr, 1993, 1995, 1997, 1999, 2001, 2003, 2006, 2008, 2010, 2013, 2015)

	// Combine pjUN=. with pjUN=0
	replace pjUN=0 if pjUN==.
	
	// Average 7a. Average Salary by Job Type
	sum SALi_Adj [aw=wtsurvy1_f] if pjPD==1
	sum SALi_Adj [aw=wtsurvy1_f] if pjAC==1
	sum SALi_Adj [aw=wtsurvy1_f] if pjTE==1
	sum SALi_Adj [aw=wtsurvy1_f] if pjID==1
	sum SALi_Adj [aw=wtsurvy1_f] if pjGV==1
	sum SALi_Adj [aw=wtsurvy1_f] if pjNP==1

// 7c. Earnings Since First Permanent Job ***
use "${TEMP}/OOI_worksample_transitions.dta", clear

	// Indicator for early postdoc (within 2 years of graduation)
	gen startPD_t = (refyr>=phdcy_min & refyr<=phdcy_min+2 & PDi>0 & PDi!=.)
	bys refid: egen startPD = max(startPD_t)	
	
	// Start in postdoc and transition to tenure-track
	gen startPDAC = (startPD==1 & PDtACs==1)
	
	// Start in postdoc and transition to industry
	gen startPDID = (startPD==1 & PDtIDs==1)
	
	// Start in postdoc and transition to non-tenure track
	gen startPDTE =(startPD==1 & PDtTEs==1)
	
	// Indicator for early academic / industry (within 2 years of graduation) with no postdoc exp
	foreach i in AC ID TE {
		gen start`i'_t = (refyr>=phdcy_min & refyr<=phdcy_min+2 & `i'i>0 & `i'i!=. & yrsPD==0)
		bys refid: egen start`i' = max(start`i'_t)
	}	
	
	// Years Since Start Permanent Job (so grad year if don't postdoc, last year postdoc if postdoc)
	gen yrsSincePJ = .
	replace yrsSincePJ = (refyr - phdcy_min) if startAC==1 | startID==1 | startTE==1
	replace yrsSincePJ = (refyr - lastPD) if startPD==1
	
	// Average 7?. SincePJ Salary
	sum SALi_Adj if yrsSincePJ>=1 & yrsSincePJ<=3 & startPDAC==1
	sum SALi_Adj if yrsSincePJ>=1 & yrsSincePJ<=3 & startAC==1

// 7d. Average of Lifetime Earnings
use "${TEMP}/OOI_worksample_transitions.dta", clear

	// Years since PhD
	gen yrsOut = refyr - phdcy_min
	
	// Indicator for early postdoc (within 2 years of graduation)
	gen startPD_t = (refyr>=phdcy_min & refyr<=phdcy_min+2 & PDi>0 & PDi!=.)
	bys refid: egen startPD = max(startPD_t)	
	
	// Start in postdoc and transition to tenure-track
	gen startPDAC = (startPD==1 & PDtACs==1)
	
	// Start in postdoc and transition to industry
	gen startPDID = (startPD==1 & PDtIDs==1)
	
	// Start in postdoc and transition to non-tenure track
	gen startPDTE =(startPD==1 & PDtTEs==1)
	
	// Indicator for early academic / industry (within 2 years of graduation) with no postdoc exp
	foreach i in AC ID TE {
		gen start`i'_t = (refyr>=phdcy_min & refyr<=phdcy_min+2 & `i'i>0 & `i'i!=. & yrsPD==0)
		bys refid: egen start`i' = max(start`i'_t)
	}	

	// Collapse on salary
	gen count = 1
	collapse (mean) meanSAL=SALi_Adj (sem) seSAL=SALi_Adj ///
			(sum) count [aw=wtsurvy1_f], by(startPDAC startPDID startPDTE startAC startID startTE yrsOut)
			
	// Only keep if have one career trajectory (some weird cases where have multiple job types that can mean being a postdoc and other type)
	egen numPaths = rowtotal(startPDAC startPDID startPDTE startAC startID startTE)		
	keep if numPaths==1		
			
	// Create error bars
	gen SALhi = meanSAL + invttail(count, 0.025)*seSAL
	gen SALlo = meanSAL - invttail(count, 0.025)*seSAL 

	// Only keep 30 years after PhDs
	keep if yrsOut<=30
	
	// Weighted sum of salary over 30 years
	collapse (sum) sumSAL = meanSAL [aweight=count], by(startPDAC startPDID startPDTE startAC startID startTE)
	

// 8a. Job Activity Spend Most Time
use "${DATA}/OOI_workingsample.dta", clear

	// Combine pjUN=. with pjUN=0
	replace pjUN=0 if pjUN==.
	
	// Tabulate wapri to create indicators
	tab wapri, gen(wapri_)
	
	// Combine pjUN=. with pjUN=0
	replace pjUN=0 if pjUN==.

	// Average 8a. Job Activity Spend Most Time
	sum wapri_3 [aw=wtsurvy1_f] if pjPD==1
	sum wapri_3 [aw=wtsurvy1_f] if pjAC==1
	sum wapri_3 [aw=wtsurvy1_f] if pjTE==1
	sum wapri_3 [aw=wtsurvy1_f] if pjID==1
	sum wapri_3 [aw=wtsurvy1_f] if pjGV==1
	sum wapri_3 [aw=wtsurvy1_f] if pjNP==1
	
	// Merge on Carnegie classifications for current institutions
	merge m:1 refyr instcod using "${LOOKUPS}/CCbyYear_max.dta"
	drop if _merge==2
	drop _merge	
	
	// Average 8a. Job Activity Spend Most Time (R1)
	sum wapri_3 [aw=wtsurvy1_f] if pjPD==1 & (R1==1 & R2==0 & D1==0 & D2==0) 
	sum wapri_3 [aw=wtsurvy1_f] if pjAC==1 & (R1==1 & R2==0 & D1==0 & D2==0) 
	sum wapri_3 [aw=wtsurvy1_f] if pjTE==1 & (R1==1 & R2==0 & D1==0 & D2==0) 
	
// 9. Carnegie Classifications (% R1) of Transitions
use "${DATA}/OOI_workingsample.dta", clear

	// Job type
	foreach i in PD AC TE GV ID NP {
		gen ind`i' = (`i'i!=0 & `i'i!=.)
	}
	
	// Indicator for early tenure-track (within 2 years of graduation)
	gen startAC_t = (refyr>=phdcy_min & refyr<=phdcy_min+2 & ACi>0 & ACi!=.)
	bys refid: egen startAC = max(startAC_t)
	
	// Indicator for early non-tenure track (within 2 years of graduation)
	gen startTE_t = (refyr>=phdcy_min & refyr<=phdcy_min+2 & TEi>0 & TEi!=.)
	bys refid: egen startTE = max(startTE_t)	
	
		// Without postdoc (check for early postdoc)
		gen startPD_t = (refyr>=phdcy_min & refyr<=phdcy_min+2 & PDi>0 & PDi!=.)
		bys refid: egen startPD = max(startPD_t)
	
		gen startACnoPD = (startAC==1 & startPD==0)
		gen startTEnoPD = (startTE==1 & startPD==0)
		
	// Merge on transitions data -> has indicator for transition from postdoc to academia
	merge m:1 refid using "${TEMP}/DRF_SDR_All Job Transitions.dta", keepusing(lastPD PDtACs PDtTEs)
	drop if _merge==2
	drop _merge
	
	// Merge on Carnegie classifications for current institutions
	merge m:1 refyr instcod using "${LOOKUPS}/CCbyYear_max.dta"
	drop if _merge==2
	drop _merge
	
	// Create groups
	gen ind_GRtPD = (PDi==6 & startPD==1 & refyr>=phdcy_min & refyr<=phdcy_min+2)
	gen ind_GRtAC = (ACi==6 & startACnoPD==1 & refyr>=phdcy_min & refyr<=phdcy_min+2)
	gen ind_GRtTE = (TEi==6 & startTEnoPD==1 & refyr>=phdcy_min & refyr<=phdcy_min+2)
	gen ind_PDtAC = (ACi==6 & PDtACs>0 & refyr>=lastPD & refyr<=lastPD+2)
	gen ind_PDtTE = (TEi==6 & PDtTEs>0 & refyr>=lastPD & refyr<=lastPD+2)
	
	// Average 9. Carnegie Classifications (% R1) of Transitions
	sum R1 R2 D1 D2 [aw=wtsurvy1_f] if ind_GRtPD==1 & phd_supField=="Physics"
	sum R1 R2 D1 D2 [aw=wtsurvy1_f] if ind_GRtAC==1 & phd_supField=="Physics"
	sum R1 R2 D1 D2 [aw=wtsurvy1_f] if ind_GRtTE==1 & phd_supField=="Physics"
	sum R1 R2 D1 D2 [aw=wtsurvy1_f] if ind_PDtAC==1 & phd_supField=="Physics"
	sum R1 R2 D1 D2 [aw=wtsurvy1_f] if ind_PDtTE==1 & phd_supField=="Physics"
	