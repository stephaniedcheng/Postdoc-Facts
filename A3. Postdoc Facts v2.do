/* A3. Postdoc Facts v2
Name: Stephanie D. Cheng
Date Created: 4-5-21
Date Updated: 4-5-21

This .do file identifies the main facts about the postdoc market. It is an updated version that uses a 3-year moving average and distinguishes between the graph lines better.

*/

global FIELD "Physics"

global MAIN "H:/SDC_Postdocs"
global DATA "${MAIN}/Data"
global RESULTS "${MAIN}/Results/Postdoc Facts 3 Year Moving Average/${FIELD}"
global TEMP "${MAIN}/Temp"
global LOOKUPS "${MAIN}/Lookups"

global COUNT "${TEMP}/Count Tables 3 Year Moving Average"
global TOCLEAN "${TEMP}/Clean for Disclosure 3 Year Moving Average"

********************************

*** 1a. Time in Graduate School ***
use "${DATA}/OOI_workerchar.dta", clear
keep if phd_supField == "${FIELD}"

	// Keep unique copy 
	keep refid wtsurvy1_f phdcy_min gradYrs
	duplicates drop
	
	// Collapse by graduation year and PhD field
	gen count = 1
	collapse (mean) m_gradYrs = gradYrs (sd) sd_gradYrs = gradYrs ///
				(sum) N_gradYrs=count [aw=wtsurvy1_f], by(phdcy_min)
	
	// 3 Year Moving Average
	gen m_gradYrsN = m_gradYrs[_n-1]
	gen m_gradYrsP = m_gradYrs[_n+1]
	
	egen m_gradYrs3 = rowmean(m_gradYrs m_gradYrsN m_gradYrsP)
	
	// Replace n with . if no mean
	replace N_gradYrs = . if m_gradYrs==.
	
	// 3 Year Moving SE
	foreach i in sd N {
		gen `i'_gradYrsN = `i'_gradYrs[_n-1]
		gen `i'_gradYrsP = `i'_gradYrs[_n+1]
	}
	
		// 1. Calculate each (n-1)*sd^2 for each of the 3 years
		gen sptN = (N_gradYrsN-1)*sd_gradYrsN^2
		gen spt = (N_gradYrs-1)*sd_gradYrs^2
		gen sptP = (N_gradYrsP-1)*sd_gradYrsP^2
		
		// 2. Calculate numerator and denominator
		egen spn = rowtotal(sptN spt sptP)
		egen spd = rowtotal(N_gradYrsN N_gradYrs N_gradYrsP)
		egen spnm = rownonmiss(m_gradYrsN m_gradYrs m_gradYrsP)	// count # non-missing
		
		// 3. Calculate SP
		gen sp = sqrt(spn/(spd-spnm))
		
		// 4. Calculate SE
		foreach i in N_gradYrs N_gradYrsN N_gradYrsP {
		    gen i`i' = 1/`i'
		}
		egen ins = rowtotal(iN_gradYrs iN_gradYrsN iN_gradYrsP)
		gen se_gradYrs3 = sp*sqrt(ins)
	
		// 5. Clean up
		gen df_gradYrs3 = spd-spnm
		keep phdcy_min m_gradYrs3 se_gradYrs3 df_gradYrs3 N_gradYrs
	
	// Create error bars
	foreach i in gradYrs {
		gen `i'_hi3 = m_`i'3 + invttail(df_`i'3,0.025)*se_`i'3
		gen `i'_lo3 = m_`i'3 - invttail(df_`i'3,0.025)*se_`i'3			
	}
	
	// Count table
	keep if N_gradYrs>=50 & N_gradYrs!=.
	save "${COUNT}/${FIELD} 3 Yr Mean GradYrs.dta", replace
	
	// Graph of 3-year moving average grad years over time
	twoway (scatter m_gradYrs3 phdcy_min if N_gradYrs>=50) ///
			(rcap gradYrs_hi3 gradYrs_lo3 phdcy_min if N_gradYrs>=50), ///
			legend(off) xlab(1950(10)2015) xtitle("PhD Graduation Year") ytitle("Number of Years") ///
			title("Moving Average: Years Spent in Graduate School") subtitle("For ${FIELD} PhDs") note("Note: Shows cohorts of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr Mean GradYrs ${FIELD}.pdf", as(pdf) replace	
		
*** 1b. Time Out of Graduate School ***
use "${DATA}/OOI_workerchar.dta", clear
keep if phd_supField == "${FIELD}"

	// Keep unique copy
	keep refid wtsurvy1_f phdcy_min togephd phd_supField
	duplicates drop
	
	// Collapse by graduation year and PhD field
	gen count = 1
	collapse (mean) m_togephd = togephd (sd) sd_togephd = togephd ///
				(sum) N_togephd=count [aw=wtsurvy1_f], by(phdcy_min)
	
	// 3 Year Moving Average
	gen m_togephdN = m_togephd[_n-1]
	gen m_togephdP = m_togephd[_n+1]
	
	egen m_togephd3 = rowmean(m_togephd m_togephdN m_togephdP)
		
	// Replace n with . if no mean
	replace N_togephd = . if m_togephd==.
	
	// 3 Year Moving SE
	foreach i in sd N {
		gen `i'_togephdN = `i'_togephd[_n-1]
		gen `i'_togephdP = `i'_togephd[_n+1]
	}
	
		// 1. Calculate each (n-1)*sd^2 for each of the 3 years
		gen sptN = (N_togephdN-1)*sd_togephdN^2
		gen spt = (N_togephd-1)*sd_togephd^2
		gen sptP = (N_togephdP-1)*sd_togephdP^2
		
		// 2. Calculate numerator and denominator
		egen spn = rowtotal(sptN spt sptP)
		egen spd = rowtotal(N_togephdN N_togephd N_togephdP)
		egen spnm = rownonmiss(m_togephdN m_togephd m_togephdP)	// count # non-missing
		
		// 3. Calculate SP
		gen sp = sqrt(spn/(spd-spnm))
		
		// 4. Calculate SE
		foreach i in N_togephd N_togephdN N_togephdP {
		    gen i`i' = 1/`i'
		}
		egen ins = rowtotal(iN_togephd iN_togephdN iN_togephdP)
		gen se_togephd3 = sp*sqrt(ins)
	
		// 5. Clean up
		gen df_togephd3 = spd-spnm
		keep phdcy_min m_togephd3 se_togephd3 df_togephd3 N_togephd
		
	// Create error bars
	foreach i in togephd {
		gen `i'_hi3 = m_`i'3 + invttail(df_`i'3,0.025)*se_`i'3
		gen `i'_lo3 = m_`i'3 - invttail(df_`i'3,0.025)*se_`i'3			
	}	

	// Count table
	keep if N_togephd>=50 & N_togephd!=.
	save "${COUNT}/${FIELD} 3 Yr Mean Time Out BA PhD.dta", replace	
	
	// Graph of 3-year moving average grad years over time
	twoway (scatter m_togephd3 phdcy_min if N_togephd>=50) ///
			(rcap togephd_hi3 togephd_lo3 phdcy_min if N_togephd>=50), ///
			legend(off) xlab(1950(10)2015) xtitle("PhD Graduation Year") ytitle("Number of Years") ///
			title("Moving Average: Years Between BA and PhD Not in School", size(medlarge)) subtitle("For ${FIELD} PhDs") note("Note: Shows cohorts of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr Mean Time Out BA PhD ${FIELD}.pdf", as(pdf) replace	

*** 1c. Distribution of Grad School Years ***	
use "${DATA}/OOI_workerchar.dta", clear
keep if phd_supField == "${FIELD}"

	// Keep unique copy if have # grad years
	keep refid wtsurvy1_f phdcy_min gradYrs
	duplicates drop
	drop if gradYrs==.
	
	// Make 8 upper limit, 4 lower limit
	gen maxYrsGrad_cap = gradYrs
	replace maxYrsGrad_cap = 8 if gradYrs>=8 & gradYrs!=.
	replace maxYrsGrad_cap = 4 if gradYrs<=4 & gradYrs!=.
	
	// Count # of individuals in each group by yrsPD
	gen count = 1
	collapse (sum) count [aw=wtsurvy1_f], by(phdcy_min maxYrsGrad_cap)
	
	// Calculate % of each cohort
	bys phdcy_min: egen pop = sum(count)
	gen per_YrsGrad = count/pop	
	
	// 3 Year Moving Average
	sort maxYrsGrad_cap phdcy_min
	by maxYrsGrad_cap: gen per_YrsGradN = per_YrsGrad[_n-1]
	by maxYrsGrad_cap: gen per_YrsGradP = per_YrsGrad[_n+1]
	
	egen per_YrsGrad3 = rowmean(per_YrsGrad per_YrsGradN per_YrsGradP)
	
	// 3 Year Counts and Populations
	foreach i in count pop {
	    by maxYrsGrad_cap: gen `i'N = `i'[_n-1]
		by maxYrsGrad_cap: gen `i'P = `i'[_n+1]
		
		egen `i'3 = rowtotal(`i' `i'N `i'P)
	}
	
	// Count table
	drop if count<5 | pop<50
	save "${COUNT}/${FIELD} 3 Yr Dist of Grad Yrs.dta", replace
	
	// Line Graph
	twoway (connected per_YrsGrad3 phdcy_min if maxYrsGrad_cap==4, cmissing(n) color(navy) msymbol(Oh) msize(small)) ///
			(connected per_YrsGrad3 phdcy_min if maxYrsGrad_cap==5, cmissing(n) color(maroon) msymbol(Dh) msize(small)) ///
			(connected per_YrsGrad3 phdcy_min if maxYrsGrad_cap==6, cmissing(n) color(forest_green) msymbol(Th) msize(small)) ///
			(connected per_YrsGrad3 phdcy_min if maxYrsGrad_cap==7, cmissing(n) color(dkorange) msymbol(Sh) msize(small)) ///
			(connected per_YrsGrad3 phdcy_min if maxYrsGrad_cap==8, cmissing(n) color(teal) msymbol(A) msize(medsmall)), ///
			legend(order(1 "4 or Fewer Yrs" 2 "5 Yrs" 3 "6 Yrs" 4 "7 Yrs" 5 "8 or More Yrs"))  ///
			xlab(1950(10)2015) xtitle("") ytitle("Fraction of Cohort") title("Moving Average: Distribution of Grad Years") subtitle("${FIELD} PhDs") ///
			note("Note: Shows cohorts of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr Dist of Grad Yrs ${FIELD}.pdf", as(pdf) replace	

*** 2. Early Postdoc Takeup ***
use "${DATA}/OOI_workingsample.dta", clear
keep if phd_supField == "${FIELD}"

	// Keep 2 years after PhD
	keep if (refyr-phdcy_min)==2
	
	// Any postdocs during this time
	gen earlyPD = (yrsPD>0)

	// Prepare unweighted numbers for disclosure purposes
	preserve
		collapse (sum) n_earlyPD=earlyPD, by(phdcy_min)
		save "${TEMP}/EarlyPD Unweighted Counts.dta", replace
	restore
	
	// Collapse by graduation year and PhD field
	gen count = 1
	collapse (mean) m_earlyPD = earlyPD (sd) sd_earlyPD = earlyPD ///
				(sum)  N_earlyPD=count [aw=wtsurvy1_f], by(phdcy_min)
	
	// Merge on unweighted counts
	merge 1:1 phdcy_min using "${TEMP}/EarlyPD Unweighted Counts.dta"
	drop _merge
	
	// 3 Year Moving Average
	gen m_earlyPDN = m_earlyPD[_n-1]
	gen m_earlyPDP = m_earlyPD[_n+1]
	
	egen m_earlyPD3 = rowmean(m_earlyPD m_earlyPDN m_earlyPDP)	
	
	// Replace n with . if no mean
	replace N_earlyPD = . if m_earlyPD==.
	
	// 3 Year Moving SE
	foreach i in sd N {
		gen `i'_earlyPDN = `i'_earlyPD[_n-1]
		gen `i'_earlyPDP = `i'_earlyPD[_n+1]
	}
	
		// 1. Calculate each (n-1)*sd^2 for each of the 3 years
		gen sptN = (N_earlyPDN-1)*sd_earlyPDN^2
		gen spt = (N_earlyPD-1)*sd_earlyPD^2
		gen sptP = (N_earlyPDP-1)*sd_earlyPDP^2
		
		// 2. Calculate numerator and denominator
		egen spn = rowtotal(sptN spt sptP)
		egen spd = rowtotal(N_earlyPDN N_earlyPD N_earlyPDP)
		egen spnm = rownonmiss(m_earlyPDN m_earlyPD m_earlyPDP)	// count # non-missing
		
		// 3. Calculate SP
		gen sp = sqrt(spn/(spd-spnm))
		
		// 4. Calculate SE
		foreach i in N_earlyPD N_earlyPDN N_earlyPDP {
		    gen i`i' = 1/`i'
		}
		egen ins = rowtotal(iN_earlyPD iN_earlyPDN iN_earlyPDP)
		gen se_earlyPD3 = sp*sqrt(ins)
	
		// 5. Clean up
		gen df_earlyPD3 = spd-spnm
		keep phdcy_min m_earlyPD3 se_earlyPD3 df_earlyPD3 N_earlyPD n_earlyPD
	
	// Create error bars
	foreach i in earlyPD {
		gen `i'_hi3 = m_`i'3 + invttail(df_`i'3,0.025)*se_`i'3
		gen `i'_lo3 = m_`i'3 - invttail(df_`i'3,0.025)*se_`i'3			
	}
	
	// Count table
	keep if n_earlyPD>=5 & N_earlyPD>=50 & N_earlyPD!=.
	save "${COUNT}/${FIELD} 3 Yr Early PD.dta", replace		
	
	// Graph of 3-year moving average grad years over time
	twoway (scatter m_earlyPD3 phdcy_min if N_earlyPD>=50 & n_earlyPD>=5) ///
			(rcap earlyPD_hi3 earlyPD_lo3 phdcy_min if N_earlyPD>=50 & n_earlyPD>=5), ///
			legend(off) xlab(1950(10)2015) xtitle("PhD Graduation Year") ytitle("Fraction of Cohort") ///
			title("Moving Average: Postdoc Within 2 Years of PhD Graduation", size(medlarge)) subtitle("For ${FIELD} PhDs") note("Note: Shows cohorts of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr Early PD ${FIELD}.pdf", as(pdf) replace	

*** 3. Distribution of Postdoc Years ***
use "${DATA}/OOI_workingsample.dta", clear
keep if phd_supField == "${FIELD}"

	// Determine total years spent in postdocs -> for graphing purposes, round down half years
	bys refid: egen maxYrsPD_t = max(yrsPD)
	gen maxYrsPD = floor(maxYrsPD)
	
	// Keep 1 copy for each refid
	keep refid wtsurvy1_f phdcy_min maxYrsPD
	duplicates drop
	
	// Group by 0 yrs, 1-2 yrs, 3-4 yrs, and 5+ yrs
	gen maxYrsPD_cap = .
	replace maxYrsPD_cap = 0 if maxYrsPD==0
	replace maxYrsPD_cap = 12 if maxYrsPD==1 | maxYrsPD==2
	replace maxYrsPD_cap = 34 if maxYrsPD==3 | maxYrsPD==4
	replace maxYrsPD_cap = 5 if maxYrsPD>=5
	
	// Count # of individuals in each group by yrsPD
	gen count = 1
	collapse (sum) count [aw=wtsurvy1_f], by(phdcy_min maxYrsPD_cap)
	
	// Calculate % of each cohort
	bys phdcy_min: egen pop = sum(count)
	gen per_YrsPD = count/pop	
	
	// 3 Year Moving Average
	sort maxYrsPD_cap phdcy_min
	by maxYrsPD_cap: gen per_YrsPDN = per_YrsPD[_n-1]
	by maxYrsPD_cap: gen per_YrsPDP = per_YrsPD[_n+1]
	
	egen per_YrsPD3 = rowmean(per_YrsPD per_YrsPDN per_YrsPDP)
	
	// 3 Year Counts and Populations
	foreach i in count pop {
	    by maxYrsPD_cap: gen `i'N = `i'[_n-1]
		by maxYrsPD_cap: gen `i'P = `i'[_n+1]
		
		egen `i'3 = rowtotal(`i' `i'N `i'P)
	}
	
	// Count table
	drop if count<5 | pop<50
	save "${COUNT}/${FIELD} 3 Yr Dist of Postdoc Yrs.dta", replace
	
	// Line Graph
	twoway (connected per_YrsPD3 phdcy_min if maxYrsPD_cap==0, cmissing(n) color(navy) msymbol(Oh) msize(small)) ///
			(connected per_YrsPD3 phdcy_min if maxYrsPD_cap==12, cmissing(n) color(maroon) msymbol(Sh) msize(small)) ///
			(connected per_YrsPD3 phdcy_min if maxYrsPD_cap==34, cmissing(n) color(forest_green) msymbol(Th) msize(small)) ///
			(connected per_YrsPD3 phdcy_min if maxYrsPD_cap==5, cmissing(n) color(dkorange) msymbol(Dh) msize(medsmall)), ///
			legend(order(1 "0 Years" 2 "1-2 Yrs" 3 "3-4 Yrs" 4 "5+ Yrs"))  ///
			xlab(1950(10)2015) xtitle("") ytitle("Fraction of Cohort") title("Moving Average: Distribution of Postdoc Years") subtitle("${FIELD} PhDs") ///
			note("Note: Shows cohorts of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr Dist of Postdoc Yrs ${FIELD}.pdf", as(pdf) replace	
	
*** 4a. Job Distribution Ten Years Post PhD ***
use "${DATA}/OOI_workingsample.dta", clear
keep if phd_supField == "${FIELD}"

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

	// Prepare unweighted numbers for disclosure purposes
	preserve
		collapse (sum) n_indPD=indPD n_indAC=indAC n_indTE=indTE n_indID=indID n_indGVNP=indGVNP, by(phdcy_min)
		save "${TEMP}/10 Years Jobs Unweighted Counts.dta", replace
	restore
	
	// Collapse by graduation year and PhD field
	gen count = 1
	collapse (mean) m_indPD=indPD m_indAC=indAC m_indTE=indTE m_indID=indID  m_indGVNP=indGVNP   ///
			(sum) count [aw=wtsurvy1_f], by(phdcy_min)

	// Merge on unweighted counts
	merge 1:1 phdcy_min using "${TEMP}/10 Years Jobs Unweighted Counts.dta"
	drop _merge
			
	// 3 Year Moving Average
	foreach i in PD AC TE ID GVNP {
		gen m_ind`i'N = m_ind`i'[_n-1]
		gen m_ind`i'P = m_ind`i'[_n+1]
		
		egen m_ind`i'3 = rowmean(m_ind`i' m_ind`i'N m_ind`i'P)
	}
	
	/* 3 Year Cells and Population
	foreach i in PD AC TE ID GVNP {
	 
		gen n_`i'N = n_`i'[_n-1]
		gen n_`i'P = n_`i'[_n+1]
		
		egen n_ind`i'3 = rowtotal(n_`i' n_`i'N n_`i'P)
		
	}
	
	gen countN = count[_n-1]
	gen countP = count[_n+1]
	
	egen count3 = rowtotal(count countN countP)
	*/
	
	// Clean up
	keep phdcy_min m_indPD3 m_indAC3 m_indTE3 m_indID3 m_indGVNP3 ///
		 n_indPD n_indAC n_indTE n_indID n_indGVNP count
	
	// Count Table
	keep if count>=50
	
	foreach i in PD AC TE ID GVNP {
		replace m_ind`i'3 = . if n_ind`i'<5
		replace n_ind`i' = . if n_ind`i'<5
	}
	
	save "${COUNT}/${FIELD} 3 Yr Dist 10 Yr Jobs.dta", replace
	
	// Line Graph
	twoway (connected m_indPD3 phdcy_min if n_indPD>=5 & count>=50, cmissing(n) color(gold) msymbol(A) msize(medsmall)) ///
				(connected m_indAC3 phdcy_min if n_indAC>=5 & count>=50, cmissing(n) color(navy) msymbol(Sh) msize(small)) ///
				(connected m_indTE3 phdcy_min if n_indTE>=5 & count>=50, cmissing(n) color(purple) msymbol(Th) msize(small)) ///
				(connected m_indID3 phdcy_min if n_indID>=5 & count>=50, cmissing(n) color(maroon) msymbol(Oh) msize(small)) ///
				(connected m_indGVNP3 phdcy_min if n_indGVNP>=5 & count>=50, cmissing(n) color(dkgreen) msymbol(Dh) msize(small)), ///
				 legend(order(1 "Postdoc" 2 "Tenure-Track" 3 "Non-Tenure Track" 4 "Industry" 5 "Gov or Non-Profit"))  ///
				xlab(1960(10)2005) xtitle("") ytitle("Fraction of Cohort") title("Moving Average: Distribution of Ten Year Job Type") subtitle("${FIELD} PhDs") ///
				note("Note: Shows cohorts of at least 50 individuals. Some job types combined or repressed due to low cell counts.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr Dist 10 Yr Jobs ${FIELD}.pdf", as(pdf) replace		
	
*** 4b. Ever Transition to Tenure-Track Job - Don't know if we need this anymore, 10 yr graph is better ***
use "${DATA}/OOI_workingsample.dta", clear
keep if phd_supField == "${FIELD}"

	// Ever obtain tenure-track position
		bys refid: egen maxYrsAC = max(yrsAC)
		gen everAC = (maxYrsAC>0)
	
	// Keep 1 copy for each refid
	keep refid wtsurvy1_f phdcy_min everAC phd_supField
	duplicates drop
	
	// Prepare unweighted numbers for disclosure purposes
	preserve
		collapse (sum) n_everAC=everAC, by(phdcy_min)
		save "${TEMP}/EverAC Unweighted Counts.dta", replace
	restore
	
	// Collapse by graduation year and PhD field
	gen count = 1
	collapse (mean) m_everAC=everAC (sd) sd_everAC=everAC ///
			(sum)  N_everAC=count [aw=wtsurvy1_f], by(phdcy_min)
	
	// Merge on unweighted counts
	merge 1:1 phdcy_min using "${TEMP}/EverAC Unweighted Counts.dta"
	drop _merge	
		
	// Replace 0 with .
	foreach i in m sd {
	    replace `i'_everAC=. if `i'_everAC==0
	}
	
	// Replace n with . if no mean
	replace N_everAC = . if m_everAC==.
	
	// 3 Year Moving Average
	gen m_everACN = m_everAC[_n-1]
	gen m_everACP = m_everAC[_n+1]
	
	egen m_everAC3 = rowmean(m_everAC m_everACN m_everACP)	
	
	// 3 Year Moving SE
	foreach i in sd N {
		gen `i'_everACN = `i'_everAC[_n-1]
		gen `i'_everACP = `i'_everAC[_n+1]
	}
	
		// 1. Calculate each (n-1)*sd^2 for each of the 3 years
		gen sptN = (N_everACN-1)*sd_everACN^2
		gen spt = (N_everAC-1)*sd_everAC^2
		gen sptP = (N_everACP-1)*sd_everACP^2
		
		// 2. Calculate numerator and denominator
		egen spn = rowtotal(sptN spt sptP)
		egen spd = rowtotal(N_everACN N_everAC N_everACP)
		egen spnm = rownonmiss(m_everACN m_everAC m_everACP)	// count # non-missing
		
		// 3. Calculate SP
		gen sp = sqrt(spn/(spd-spnm))
		
		// 4. Calculate SE
		foreach i in N_everAC N_everACN N_everACP {
		    gen i`i' = 1/`i'
		}
		egen ins = rowtotal(iN_everAC iN_everACN iN_everACP)
		gen se_everAC3 = sp*sqrt(ins)
	
		// 5. Clean up
		gen df_everAC3 = spd-spnm
		keep phdcy_min m_everAC3 se_everAC3 df_everAC3 n_everAC N_everAC
	
	// Create error bars
	foreach i in everAC {
		gen `i'_hi3 = m_`i'3 + invttail(df_`i'3,0.025)*se_`i'3
		gen `i'_lo3 = m_`i'3 - invttail(df_`i'3,0.025)*se_`i'3			
	}
	
	// Only include if cell>=5 & count>=50
	drop if n_everAC<5 | N_everAC<50
	save "${COUNT}/${FIELD} 3 Yr Ever AC.dta", replace
	
	// Graph of 3-year moving average grad years over time
	twoway (scatter m_everAC3 phdcy_min if n_everAC>=5 & N_everAC>=50) ///
			(rcap everAC_hi3 everAC_lo3 phdcy_min if n_everAC>=5 & N_everAC>=50), ///
			legend(off) xlab(1950(10)2015) xtitle("PhD Graduation Year") ytitle("Fraction of Cohort") ///
			title("Moving Average: Ever Obtain Tenure-Track Job", size(medlarge)) subtitle("For ${FIELD} PhDs") note("Note: Shows cohorts of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr Ever AC ${FIELD}.pdf", as(pdf) replace	
		
*** 4c. Early Transition to Tenure-Track Job ***
use "${DATA}/OOI_workingsample.dta", clear
keep if phd_supField == "${FIELD}"

	// Keep 2 years after PhD
	keep if (refyr-phdcy_min)==2
	
	// Keep if don't have any postdoc experience
	keep if yrsPD==0
	
	// Hold tenure-track by this time without postdoc
	gen earlyAC = (yrsAC>0 & yrsPD==0)

	// Prepare unweighted numbers for disclosure purposes
	preserve
		collapse (count) n_earlyAC=earlyAC, by(phdcy_min)
		save "${TEMP}/EarlyAC Unweighted Counts.dta", replace
	restore
		
	// Collapse by graduation year and PhD field
	gen count = 1
	collapse (mean) m_earlyAC=earlyAC (sd) sd_earlyAC=earlyAC ///
			(sum) N_earlyAC=count [aw=wtsurvy1_f], by(phdcy_min)
	
	// Merge on unweighted counts
	merge 1:1 phdcy_min using "${TEMP}/EarlyAC Unweighted Counts.dta"
	drop _merge
	
	// Replace 0 with .
	foreach i in m sd {
	    replace `i'_earlyAC=. if `i'_earlyAC==0
	}
	
	// Replace n with . if no mean
	replace N_earlyAC = . if m_earlyAC==.
	
	// 3 Year Moving Average
	gen m_earlyACN = m_earlyAC[_n-1]
	gen m_earlyACP = m_earlyAC[_n+1]
	
	egen m_earlyAC3 = rowmean(m_earlyAC m_earlyACN m_earlyACP)	
	
	// 3 Year Moving SE
	foreach i in sd N {
		gen `i'_earlyACN = `i'_earlyAC[_n-1]
		gen `i'_earlyACP = `i'_earlyAC[_n+1]
	}
	
		// 1. Calculate each (n-1)*sd^2 for each of the 3 years
		gen sptN = (N_earlyACN-1)*sd_earlyACN^2
		gen spt = (N_earlyAC-1)*sd_earlyAC^2
		gen sptP = (N_earlyACP-1)*sd_earlyACP^2
		
		// 2. Calculate numerator and denominator
		egen spn = rowtotal(sptN spt sptP)
		egen spd = rowtotal(N_earlyACN N_earlyAC N_earlyACP)
		egen spnm = rownonmiss(m_earlyACN m_earlyAC m_earlyACP)	// count # non-missing
		
		// 3. Calculate SP
		gen sp = sqrt(spn/(spd-spnm))
		
		// 4. Calculate SE
		foreach i in N_earlyAC N_earlyACN N_earlyACP {
		    gen i`i' = 1/`i'
		}
		egen ins = rowtotal(iN_earlyAC iN_earlyACN iN_earlyACP)
		gen se_earlyAC3 = sp*sqrt(ins)
	
		// 5. Clean up
		gen df_earlyAC3 = spd-spnm
		keep phdcy_min m_earlyAC3 se_earlyAC3 df_earlyAC3 N_earlyAC n_earlyAC
	
	// Create error bars
	foreach i in earlyAC {
		gen `i'_hi3 = m_`i'3 + invttail(df_`i'3,0.025)*se_`i'3
		gen `i'_lo3 = m_`i'3 - invttail(df_`i'3,0.025)*se_`i'3			
	}
	
	// Only include if cell>=5 & count>=50
	drop if n_earlyAC<5 | N_earlyAC<50
	save "${COUNT}/${FIELD} 3 Yr Early AC.dta", replace
	
	// Graph of 3-year moving average grad years over time
	twoway (scatter m_earlyAC3 phdcy_min if N_earlyAC>=50 & n_earlyAC>=5) ///
			(rcap earlyAC_hi3 earlyAC_lo3 phdcy_min if N_earlyAC>=50 & n_earlyAC>=5), ///
			legend(off) xlab(1950(10)2015) xtitle("PhD Graduation Year") ytitle("Fraction of Cohort") ///
			title("Moving Average: Early Tenure-Track Job With No Postdoc Exp", size(medlarge)) subtitle("For ${FIELD} PhDs") note("Note: Shows cohorts of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr Early AC ${FIELD}.pdf", as(pdf) replace	
	
*** 4d. Postdoc Transition to Tenure-Track Job ***
use "${TEMP}/OOI_workerchar_transitions.dta", clear
keep if phd_supField == "${FIELD}"

	// Keep only if have postdoc experience
	bys refid: egen maxYrsPD = max(yrsPD)
	gen everPD = (maxYrsPD>0)
	keep if everPD==1
	
	// Keep 1 copy for each refid
	keep refid wtsurvy1_f phdcy_min PDt*s maxYrsPD
	duplicates drop
	
	// Keep only if have info on transition
	keep if PDtNI==0
	
	// Turn into indicators rather than normalized
	gen PDtAC = (PDtACs>0 & PDtACs!=.)
	
	// Prepare unweighted numbers for disclosure purposes
	preserve
		collapse (sum) n_PDtAC=PDtAC, by(phdcy_min)
		save "${TEMP}/PDtAC Unweighted Counts.dta", replace
	restore
		
	// Collapse by graduation year and PhD field
	gen count = 1
	collapse (mean) m_PDtAC=PDtAC (sd) sd_PDtAC=PDtAC ///
			(sum)  N_PDtAC=count [aw=wtsurvy1_f], by(phdcy_min)
		
	// Merge on unweighted counts
	merge 1:1 phdcy_min using "${TEMP}/PDtAC Unweighted Counts.dta"
	drop _merge
	
	// Replace 0 with .
	foreach i in m sd {
	    replace `i'_PDtAC=. if `i'_PDtAC==0
	}
		
	// Replace n with . if no mean
	replace N_PDtAC = . if m_PDtAC==.
	
	// 3 Year Moving Average
	gen m_PDtACN = m_PDtAC[_n-1]
	gen m_PDtACP = m_PDtAC[_n+1]
	
	egen m_PDtAC3 = rowmean(m_PDtAC m_PDtAC m_PDtAC)	
	
	// 3 Year Moving SE
	foreach i in sd N {
		gen `i'_PDtACN = `i'_PDtAC[_n-1]
		gen `i'_PDtACP = `i'_PDtAC[_n+1]
	}
	
		// 1. Calculate each (n-1)*sd^2 for each of the 3 years
		gen sptN = (N_PDtACN-1)*sd_PDtACN^2
		gen spt = (N_PDtAC-1)*sd_PDtAC^2
		gen sptP = (N_PDtACP-1)*sd_PDtACP^2
		
		// 2. Calculate numerator and denominator
		egen spn = rowtotal(sptN spt sptP)
		egen spd = rowtotal(N_PDtACN N_PDtAC N_PDtACP)
		egen spnm = rownonmiss(m_PDtACN m_PDtAC m_PDtACP)	// count # non-missing
		
		// 3. Calculate SP
		gen sp = sqrt(spn/(spd-spnm))
		
		// 4. Calculate SE
		foreach i in N_PDtAC N_PDtACN N_PDtACP {
		    gen i`i' = 1/`i'
		}
		egen ins = rowtotal(iN_PDtAC iN_PDtACN iN_PDtACP)
		gen se_PDtAC3 = sp*sqrt(ins)
	
		// 5. Clean up
		gen df_PDtAC3 = spd-spnm
		keep phdcy_min m_PDtAC3 se_PDtAC3 df_PDtAC3 n_PDtAC N_PDtAC
	
	// Create error bars
	foreach i in PDtAC {
		gen `i'_hi3 = m_`i'3 + invttail(df_`i'3,0.025)*se_`i'3
		gen `i'_lo3 = m_`i'3 - invttail(df_`i'3,0.025)*se_`i'3			
	}
	
	// Only include if cell>=5 & count>=50
	drop if n_PDtAC<5 | N_PDtAC<50
	save "${COUNT}/${FIELD} 3 Yr PD to AC.dta", replace
	
	// Graph of 3-year moving average grad years over time
	twoway (scatter m_PDtAC3 phdcy_min if df_PDtAC3>=50) ///
			(rcap PDtAC_hi3 PDtAC_lo3 phdcy_min if df_PDtAC3>=50), ///
			legend(off) xlab(1950(10)2015) xtitle("PhD Graduation Year") ytitle("Fraction of Cohort") ylab(0(.2)1) ///
			title("Moving Average: Transition to Tenure-Track Job After Postdoc", size(medlarge)) subtitle("For ${FIELD} PhDs") note("Note: Shows cohorts of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr PD to AC ${FIELD}.pdf", as(pdf) replace	
			
*** 5a. Early Transition to Other Non-Postdoc Job ***
use "${DATA}/OOI_workingsample.dta", clear
keep if phd_supField == "${FIELD}"

	// Keep 2 years after PhD
	keep if (refyr-phdcy_min)==2
	
	// Hold non-postdoc position by this time without postdoc
	foreach i in AC TE GV ID NP UN NL {
		gen early`i' = (yrs`i'>0 & yrsPD==0)
	}
	
	// Calculate % start in early positions for 1970-on
	sum early* [aw=wtsurvy1_f] if phdcy_min>=1970
	
	// Normalize: only keep if have info
	egen totNum = rowtotal(early*)
	keep if totNum>0
	
	foreach i in AC TE GV ID NP UN NL {
		gen early`i'_n = early`i' / totNum
	}
	
	// Going to combine GV and NP together
	egen earlyGVNP_n = rowtotal(earlyGV_n earlyNP_n)

	// Prepare unweighted numbers for disclosure purposes
	preserve
	
		// Turn into indicators instead of normalized
		foreach i in AC TE ID GVNP {
			gen n_early`i' = (early`i'_n>0 & early`i'_n!=.)
		}
	
		collapse (sum) n_earlyAC n_earlyTE n_earlyID n_earlyGVNP, by(phdcy_min)
		save "${TEMP}/Early Jobs Unweighted Counts.dta", replace
	restore	
	
	// Count # of individuals in each group (normalized so that each individual only counted once)
	gen count = 1
	collapse (mean) m_earlyAC=earlyAC_n m_earlyTE=earlyTE_n m_earlyID=earlyID_n m_earlyGVNP=earlyGVNP_n ///
			(sum) count [aw=wtsurvy1_f], by(phdcy_min)

	// Merge on unweighted counts
	merge 1:1 phdcy_min using "${TEMP}/Early Jobs Unweighted Counts.dta"
	drop _merge			
			
	// Replace 0 with .
	foreach i in AC TE ID GVNP {
	    replace m_early`i'=. if m_early`i'==0
	}
	
	// 3 Year Moving Average
	foreach i in AC TE ID GVNP {
		gen m_early`i'N = m_early`i'[_n-1]
		gen m_early`i'P = m_early`i'[_n+1]
		
		egen m_early`i'3 = rowmean(m_early`i' m_early`i'N m_early`i'P)
	}
	
	/* 3 Year Cells and Population
	foreach i in AC TE ID GVNP {
	    
		gen n_early`i' = m_early`i'*count
		gen n_early`i'N = n_early`i'[_n-1]
		gen n_early`i'P = n_early`i'[_n+1]
		
		egen n_early`i'3 = rowtotal(n_early`i' n_early`i'N n_early`i'P)
		
	}
	
	gen countN = count[_n-1]
	gen countP = count[_n+1]
	
	egen count3 = rowtotal(count countN countP)
	*/
	
	// Clean up
	keep phdcy_min m_earlyAC3 m_earlyTE3 m_earlyID3 m_earlyGVNP3 ///
		 n_earlyAC n_earlyTE n_earlyID n_earlyGVNP count
	
	// Count Table
	keep if count>=50
	
	foreach i in AC TE ID GVNP {
		replace m_early`i'3 = . if n_early`i'<5
		replace n_early`i' = . if n_early`i'<5
	}
	
	save "${COUNT}/${FIELD} 3 Yr Dist Early Jobs.dta", replace
	
	// Line Graph
	twoway (connected m_earlyAC3 phdcy_min if n_earlyAC>=5 & count>=50, cmissing(n) color(navy) msymbol(Sh) msize(small)) ///
			(connected m_earlyTE3 phdcy_min if n_earlyTE>=5 & count>=50, cmissing(n) color(purple) msymbol(Th) msize(small)) ///
			(connected m_earlyID3 phdcy_min if n_earlyID>=5 & count>=50, cmissing(n) color(maroon) msymbol(Oh) msize(small)) ///
			(connected m_earlyGVNP3 phdcy_min if n_earlyGVNP>=5 & count>=50, cmissing(n) color(dkgreen) msymbol(Dh) msize(small)), ///
			 legend(order(1 "Tenure-Track" 2 "Non-Tenure Track" 3 "Industry" 4 "Gov or Non-Profit"))  ///
			xlab(1960(10)2015) xtitle("") ytitle("Fraction of Cohort") title("Moving Average: Distribution of Early Non-Postdoc Jobs") subtitle("${FIELD} PhDs") ///
			note("Note: Shows cohorts of at least 50 individuals. Some job types combined or repressed due to low cell counts.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr Dist Early Jobs ${FIELD}.pdf", as(pdf) replace		
	
*** 5b. Postdoc Transition to Other Jobs ***
use "${TEMP}/OOI_workerchar_transitions.dta", clear
keep if phd_supField == "${FIELD}"

	// Keep only if have postdoc experience
	bys refid: egen maxYrsPD = max(yrsPD)
	gen everPD = (maxYrsPD>0)
	keep if everPD==1
	
	// Keep 1 copy for each refid
	keep refid wtsurvy1_f phdcy_min PDt*s maxYrsPD
	duplicates drop
	
	// Keep only if have info on transition
	keep if PDtNI==0
	
	// Going to combine GV and NP together
	egen PDtGVNPs = rowtotal(PDtGVs PDtNPs)

	// Prepare unweighted numbers for disclosure purposes (use non-normalized)
	preserve
	
		// Indicators rather than normalized
		foreach i in AC TE ID GVNP {
			gen PDt`i' = (PDt`i's>0 & PDt`i's!=.)
		}
	
		collapse (sum) n_PDtAC=PDtAC n_PDtTE=PDtTE n_PDtID=PDtID n_PDtGVNP=PDtGVNP, by(phdcy_min)
		save "${TEMP}/Postdoc Jobs Unweighted Counts.dta", replace
	restore	
	
	// Count # of individuals in each group (normalized so that each individual only counted once)
	gen count = 1
	collapse (mean) m_PDtAC=PDtACs m_PDtTE=PDtTEs m_PDtID=PDtIDs m_PDtGVNP=PDtGVNPs ///
			(sum) count [aw=wtsurvy1_f], by(phdcy_min)

	// Merge on unweighted counts
	merge 1:1 phdcy_min using "${TEMP}/Postdoc Jobs Unweighted Counts.dta"
	drop _merge
	
	// Replace 0 with .
	foreach i in AC TE ID GVNP {
	    replace m_PDt`i'=. if m_PDt`i'==0
	}
	
	// 3 Year Moving Average
	foreach i in AC TE ID GVNP {
		gen m_PDt`i'N = m_PDt`i'[_n-1]
		gen m_PDt`i'P = m_PDt`i'[_n+1]
		
		egen m_PDt`i'3 = rowmean(m_PDt`i' m_PDt`i'N m_PDt`i'P)
	}
	
	/* 3 Year Cells and Population
	foreach i in AC TE ID GVNP {
	    
		gen n_PDt`i' = m_PDt`i'*count
		gen n_PDt`i'N = n_PDt`i'[_n-1]
		gen n_PDt`i'P = n_PDt`i'[_n+1]
		
		egen n_PDt`i'3 = rowtotal(n_PDt`i' n_PDt`i'N n_PDt`i'P)
		
	}
	
	gen countN = count[_n-1]
	gen countP = count[_n+1]
	
	egen count3 = rowtotal(count countN countP)
	*/
	
	// Clean up
	keep phdcy_min m_PDtAC3 m_PDtTE3 m_PDtID3 m_PDtGVNP3 ///
		 n_PDtAC n_PDtTE n_PDtID n_PDtGVNP count
	
	// Count Table
	keep if count>=50
	
	foreach i in AC TE ID GVNP {
		replace m_PDt`i'3 = . if n_PDt`i'<5
		replace n_PDt`i' = . if n_PDt`i'<5
	}
	
	save "${COUNT}/${FIELD} 3 Yr Dist Postdoc Transitions.dta", replace	
	
	// Line Graph
	twoway (connected m_PDtAC3 phdcy_min if n_PDtAC>=5 & count>=50, cmissing(n) color(navy) msymbol(Sh) msize(small)) ///
			(connected m_PDtTE3 phdcy_min if n_PDtTE>=5 & count>=50, cmissing(n) color(purple) msymbol(Th) msize(small)) ///
			(connected m_PDtID3 phdcy_min if n_PDtID>=5 & count>=50, cmissing(n) color(maroon) msymbol(Oh) msize(small)) ///
			(connected m_PDtGVNP3 phdcy_min if n_PDtGVNP>=5 & count>=50, cmissing(n) color(dkgreen) msymbol(Dh) msize(small)), ///
			 legend(order(1 "Tenure-Track" 2 "Non-Tenure Track" 3 "Industry" 4 "Gov or Non-Profit"))  ///
			xlab(1960(10)2015) xtitle("") ytitle("Fraction of Cohort") title("Moving Average: Distribution of Postdoc Transitions") subtitle("${FIELD} PhDs") ///
			note("Note: Shows cohorts of at least 50 individuals. Some job types combined or repressed due to low cell counts.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr Dist Postdoc Transitions ${FIELD}.pdf", as(pdf) replace		

*** 6. Absorbing States ***
foreach i in PD AC TE GV ID NP {

	if inlist("`i'", "PD") {
		local jobtype "Postdoc"
	}
	if inlist("`i'", "AC") {
		local jobtype "Tenure-Track"
	}
	if inlist("`i'", "TE") {
		local jobtype "Non-Tenure"
	}
	if inlist("`i'", "GV") {
		local jobtype "Government"
	}	
	if inlist("`i'", "ID") {
		local jobtype "Industry"
	}	
	if inlist("`i'", "NP") {
		local jobtype "Non-Profit"
	}	

	use "${TEMP}/OOI_workerchar_transitions.dta", clear
	keep if phd_supField == "${FIELD}"

	// Keep only if have job type experience
	bys refid: egen maxYrs`i' = max(yrs`i')
	gen ever`i' = (maxYrs`i'>0)
	keep if ever`i'==1

	// Keep only if have info on transition
	keep if `i'tNIs==0
	
	// Keep 1 copy for each refid
	keep refid wtsurvy1_f phdcy_min `i't`i's maxYrs`i'
	duplicates drop

	// % Stay in Job Type
	sum `i't`i's [aw=wtsurvy1_f] if phdcy_min>=1970
	
	// Count # of individuals in each group (normalized so that each individual only counted once)
	gen count = 1
	collapse (mean) m_`i't`i' = `i't`i's (sd) sd_`i't`i'=`i't`i's ///
			(sum) n_`i't`i'=`i't`i's N_`i't`i'=count [aw=wtsurvy1_f], by(phdcy_min)
	
	// Replace 0 with .
	foreach j in m sd {
	    replace `j'_`i't`i'=. if `j'_`i't`i'==0
	}
	
	// Replace n with . if no mean
	replace N_`i't`i' = . if m_`i't`i'==.
	
	// 3 Year Moving Average
	gen m_`i't`i'N = m_`i't`i'[_n-1]
	gen m_`i't`i'P = m_`i't`i'[_n+1]
	
	egen m_`i't`i'3 = rowmean(m_`i't`i' m_`i't`i' m_`i't`i')	
	
	// 3 Year Moving SE
	foreach j in sd N {
		gen `j'_`i't`i'N = `j'_`i't`i'[_n-1]
		gen `j'_`i't`i'P = `j'_`i't`i'[_n+1]
	}
	
		// 1. Calculate each (n-1)*sd^2 for each of the 3 years
		gen sptN = (N_`i't`i'N-1)*sd_`i't`i'N^2
		gen spt = (N_`i't`i'-1)*sd_`i't`i'^2
		gen sptP = (N_`i't`i'P-1)*sd_`i't`i'P^2
		
		// 2. Calculate numerator and denominator
		egen spn = rowtotal(sptN spt sptP)
		egen spd = rowtotal(N_`i't`i'N N_`i't`i' N_`i't`i'P)
		egen spnm = rownonmiss(m_`i't`i'N m_`i't`i' m_`i't`i'P)	// count # non-missing
		
		// 3. Calculate SP
		gen sp = sqrt(spn/(spd-spnm))
		
		// 4. Calculate SE
		foreach j in N_`i't`i' N_`i't`i'N N_`i't`i'P {
		    gen i`j' = 1/`j'
		}
		egen ins = rowtotal(iN_`i't`i' iN_`i't`i'N iN_`i't`i'P)
		gen se_`i't`i'3 = sp*sqrt(ins)
	
		// 5. Clean up
		gen df_`i't`i'3 = spd-spnm
		keep phdcy_min m_`i't`i'3 se_`i't`i'3 df_`i't`i'3 n_`i't`i' N_`i't`i'
	
	// Create error bars
	gen `i't`i'_hi3 = m_`i't`i'3 + invttail(df_`i't`i'3,0.025)*se_`i't`i'3
	gen `i't`i'_lo3 = m_`i't`i'3 - invttail(df_`i't`i'3,0.025)*se_`i't`i'3			
	
	// Count Table
	drop if n_`i't`i'<5 | N_`i't`i'<50
	save "${COUNT}/${FIELD} 3 Yr Stay Rates `i'.dta", replace
	
	// Graph of 3-year moving average grad years over time
	twoway (scatter m_`i't`i'3 phdcy_min if N_`i't`i'>=50 & n_`i't`i'>=5) ///
			(rcap `i't`i'_hi3 `i't`i'_lo3 phdcy_min if N_`i't`i'>=50 & n_`i't`i'>=5), ///
			legend(off) xlab(1950(10)2015) xtitle("PhD Graduation Year") ytitle("Fraction of Cohort") ylab(0(.2)1) ///
			title("Moving Average: Fraction Remaining `jobtype' At Last Obs", size(medlarge)) subtitle("For ${FIELD} PhDs") note("Note: Shows cohorts of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr Stay Rates `i' ${FIELD}.pdf", as(pdf) replace	

}

*** 7a. Average Salary by Job Type ***
use "${DATA}/OOI_workingsample.dta", clear
	keep if phd_supField=="${FIELD}"

	// Keep if actual job
	keep if jobID!=.

	// Keep only survey years
	keep if inlist(refyr, 1993, 1995, 1997, 1999, 2001, 2003, 2006, 2008, 2010, 2013, 2015)

	// Combine pjUN=. with pjUN=0
	replace pjUN=0 if pjUN==.
	
	// Break up by time in career
	gen yrsOut = refyr - phdcy_min	
	
	gen careerTime = .
	replace careerTime = 0 if yrsOut<=2
	replace careerTime = 1 if yrsOut>2 & yrsOut<=5
	replace careerTime = 2 if yrsOut>5 & yrsOut<=9
	replace careerTime = 3 if yrsOut>9 & yrsOut<=14
	replace careerTime = 4 if yrsOut>=15
	
		// Label values
		label define careerTime 0 "0-2"
		label define careerTime 1 "3-5", modify
		label define careerTime 2 "6-9", modify
		label define careerTime 3 "10-14", modify
		label define careerTime 4 "15+", modify
		
		label val careerTime careerTime

	// Divide salary by $1000 so easier to read
	gen SALi1000 = SALi_Adj/1000
	
	// Collapse
	gen count = 1
	collapse (mean) m_SALi1000=SALi1000 (sem) se_SALi1000=SALi1000 ///
			(sum) n_SALi1000 = count [aw=wtsurvy1_f], by(pj* careerTime)
			
	// 	Create error bars
	foreach i in SALi1000 {
		gen `i'_hi = m_`i' + invttail(n_`i'-1,0.025)*se_`i'
		gen `i'_lo = m_`i' - invttail(n_`i'-1,0.025)*se_`i'			
	}	
	
	// Keep only the single principal job groups
	egen tempTot = rowtotal(pj*)
	keep if tempTot==1
	
	// Only graph if at least 50 people
	drop if n_SALi100<50
	
	// Line Graph
	twoway (connected m_SALi1000 careerTime if pjPD==1, cmissing(n) color(gold) msymbol(A) msize(medium)) ///
			(rcap SALi1000_hi SALi1000_lo careerTime if pjPD==1, lcolor(gold)) ///
			(connected m_SALi1000 careerTime if pjAC==1, cmissing(n) color(navy) msymbol(Sh) msize(small)) ///
			(rcap SALi1000_hi SALi1000_lo careerTime if pjAC==1, lcolor(navy)) ///
			(connected m_SALi1000 careerTime if pjTE==1, cmissing(n) color(purple) msymbol(Th) msize(small)) ///
			(rcap SALi1000_hi SALi1000_lo careerTime if pjTE==1, lcolor(purple)) ///
			(connected m_SALi1000 careerTime if pjID==1, cmissing(n) color(maroon) msymbol(Oh) msize(small)) ///
			(rcap SALi1000_hi SALi1000_lo careerTime if pjID==1, lcolor(maroon)) ///
			(connected m_SALi1000 careerTime if pjGV==1, cmissing(n) color(midgreen) msymbol(Dh) msize(small)) ///
			(rcap SALi1000_hi SALi1000_lo careerTime if pjGV==1, lcolor(midgreen)) ///
			(connected m_SALi1000 careerTime if pjNP==1, cmissing(n) color(gray) msymbol(X) msize(medium) lpattern(dash)) ///
			(rcap SALi1000_hi SALi1000_lo careerTime if pjNP==1, lcolor(gray)), ///
			legend(order(1 "Postdoc" 3 "Tenure-Track" 5 "Non-Tenure Track" 7 "Industry" 9 "Government" 11 "Non-Profit"))  ///
			xlab(0 "0-2" 1 "3-5" 2 "6-9" 3 "10-14" 4 "15+") xtitle("Years from PhD") ytitle("Salary (Thousands of Dollars)") title("Average Salary by Job Type and Career Stage") subtitle("${FIELD} PhDs") ///
			note("Note: Shows groups of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/Salary Path ${FIELD}.pdf", as(pdf) replace		

		// Create Counts Table
			// Only show the ones being graphed
			keep if pjPD==1 | pjAC==1 | pjTE==1 | pjID==1 | pjGV==1 | pjNP==1
			save "${COUNT}/${FIELD} Salary Path.dta", replace
		
*** 7b. Earnings by Years Since PhD ***
use "${TEMP}/OOI_worksample_transitions.dta", clear
	keep if phd_supField=="${FIELD}"

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
	
	// Make salary in thousands
	gen SAL_1000 = SALi_Adj/1000

	// Collapse on salary
	gen count = 1
	collapse (mean) meanSAL=SAL_1000 (sem) seSAL=SAL_1000 ///
			(sum) count [aw=wtsurvy1_f], by(startPDAC startPDID startPDTE startAC startID startTE yrsOut)
			
	// Only keep if have one career trajectory (some weird cases where have multiple job types that can mean being a postdoc and other type)
	egen numPaths = rowtotal(startPDAC startPDID startPDTE startAC startID startTE)		
	keep if numPaths==1		
			
	// Create error bars
	gen SALhi = meanSAL + invttail(count, 0.025)*seSAL
	gen SALlo = meanSAL - invttail(count, 0.025)*seSAL 
	
	// Counts Table
	keep if yrsOut<=30
	keep if count>=50
	
	save "${COUNT}/${FIELD} Life Cycle Salary by PD.dta", replace
	
	// Graph raw salary for tenure track
	twoway (connected meanSAL yrsOut if startPDAC==1 & count>=50 & yrsOut<=30, mcolor(navy) lcolor(navy) msymbol(O)) ///
			(rcap SALhi SALlo yrsOut if startPDAC==1 & count>=50 & yrsOut<=30, lcolor(navy)) ///
			(connected meanSAL yrsOut if startAC==1 & count>=50 & yrsOut<=30, mcolor(mint) lcolor(mint) msymbol(T)) ///
			(rcap SALhi SALlo yrsOut if startAC==1 & count>=50 & yrsOut<=30, lcolor(mint)), ///
			legend(order(3 "Grad to Tenure-Track" 1 "Postdoc to Tenure-Track")) xlab(0(2)30) ylab(,angle(horizontal)) xtitle("Years Since PhD") ytitle("Salary (Thousands of 2015 Dollars)") ///
			title("Tenure-Track Salary by Postdoc Path", size(medlarge)) subtitle("${FIELD} PhDs", size(medium)) note("Note: Shows groups of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/Life Cycle AC Salary by PD ${FIELD}.pdf", as(pdf) replace	
			
	// Graph raw salary for industry		
	twoway (connected meanSAL yrsOut if startPDID==1 & count>=50 & yrsOut<=30, mcolor(maroon) lcolor(maroon) msymbol(S)) ///
			(rcap SALhi SALlo yrsOut if startPDID==1 & count>=50 & yrsOut<=30, lcolor(maroon)) ///
			(connected meanSAL yrsOut if startID==1 & count>=50 & yrsOut<=30, mcolor(gray) lcolor(gray) msymbol(D)) ///
			(rcap SALhi SALlo yrsOut if startID==1 & count>=50 & yrsOut<=30, lcolor(gray)), ///
			legend(order(3 "Grad to Industry" 1 "Postdoc to Industry")) xlab(0(2)30) ylab(,angle(horizontal)) xtitle("Years Since PhD") ytitle("Salary (Thousands of 2015 Dollars)") ///
			title("Industry Salary by Postdoc Path", size(medlarge)) subtitle("${FIELD} PhDs", size(medium)) note("Note: Shows groups of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/Life Cycle ID Salary by PD ${FIELD}.pdf", as(pdf) replace				
			
	// Graph raw salary for non-tenure track
	twoway (connected meanSAL yrsOut if startPDTE==1 & count>=50 & yrsOut<=30, mcolor(purple) lcolor(purple) msymbol(S)) ///
			(rcap SALhi SALlo yrsOut if startPDTE==1 & count>=50 & yrsOut<=30, lcolor(purple)) ///
			(connected meanSAL yrsOut if startTE==1 & count>=50 & yrsOut<=30, mcolor(dkorange) lcolor(dkorange) msymbol(D)) ///
			(rcap SALhi SALlo yrsOut if startTE==1 & count>=50 & yrsOut<=30, lcolor(dkorange)), ///
			legend(order(3 "Grad to Non-Tenure Track" 1 "Postdoc to Non-Tenure Track")) xlab(0(2)30) ylab(,angle(horizontal)) xtitle("Years Since PhD") ytitle("Salary (Thousands of 2015 Dollars)") ///
			title("Non-Tenure Track Salary by Postdoc Path", size(medlarge)) subtitle("${FIELD} PhDs", size(medium)) note("Note: Shows groups of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/Life Cycle TE Salary by PD ${FIELD}.pdf", as(pdf) replace				

*** 7c. Earnings Since First Permanent Job ***
use "${TEMP}/OOI_worksample_transitions.dta", clear
	keep if phd_supField=="${FIELD}"

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
	
	// Make salary in thousands
	gen SAL_1000 = SALi_Adj/1000

	// Collapse on salary
	gen count = 1
	collapse (mean) meanSAL=SAL_1000 (sem) seSAL=SAL_1000 ///
			(sum) count [aw=wtsurvy1_f], by(startPDAC startPDID startPDTE startAC startID startTE yrsSincePJ)
			
	// Only keep if have one career trajectory (some weird cases where have multiple job types that can mean being a postdoc and other type)
	egen numPaths = rowtotal(startPDAC startPDID startPDTE startAC startID startTE)		
	keep if numPaths==1		
			
	// Create error bars
	gen SALhi = meanSAL + invttail(count, 0.025)*seSAL
	gen SALlo = meanSAL - invttail(count, 0.025)*seSAL 
	
	// Counts Table
	keep if yrsSincePJ<=30 & yrsSincePJ>=0
	keep if count>=50
	
	save "${COUNT}/${FIELD} SincePJ Salary by PD.dta", replace
	
	// Graph raw salary for tenure track
	twoway (connected meanSAL yrsSincePJ if startPDAC==1 & count>=50 & yrsSincePJ<=30 & yrsSincePJ>=0, mcolor(navy) lcolor(navy) msymbol(O)) ///
			(rcap SALhi SALlo yrsSincePJ if startPDAC==1 & count>=50 & yrsSincePJ<=30 & yrsSincePJ>=0, lcolor(navy)) ///
			(connected meanSAL yrsSincePJ if startAC==1 & count>=50 & yrsSincePJ<=30 & yrsSincePJ>=0, mcolor(mint) lcolor(mint) msymbol(T)) ///
			(rcap SALhi SALlo yrsSincePJ if startAC==1 & count>=50 & yrsSincePJ<=30 & yrsSincePJ>=0, lcolor(mint)), ///
			legend(order(3 "Grad to Tenure-Track" 1 "Postdoc to Tenure-Track")) xlab(0(2)30) ylab(,angle(horizontal)) xtitle("Years Since First Permanent Job") ytitle("Salary (Thousands of 2015 Dollars)") ///
			title("Tenure-Track Salary by Postdoc Path", size(medlarge)) subtitle("${FIELD} PhDs", size(medium)) note("Note: Shows groups of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/SincePJ AC Salary by PD ${FIELD}.pdf", as(pdf) replace	
			
	// Graph raw salary for industry		
	twoway (connected meanSAL yrsSincePJ if startPDID==1 & count>=50 & yrsSincePJ<=30 & yrsSincePJ>=0, mcolor(maroon) lcolor(maroon) msymbol(S)) ///
			(rcap SALhi SALlo yrsSincePJ if startPDID==1 & count>=50 & yrsSincePJ<=30 & yrsSincePJ>=0, lcolor(maroon)) ///
			(connected meanSAL yrsSincePJ if startID==1 & count>=50 & yrsSincePJ<=30 & yrsSincePJ>=0, mcolor(gray) lcolor(gray) msymbol(D)) ///
			(rcap SALhi SALlo yrsSincePJ if startID==1 & count>=50 & yrsSincePJ<=30 & yrsSincePJ>=0, lcolor(gray)), ///
			legend(order(3 "Grad to Industry" 1 "Postdoc to Industry")) xlab(0(2)30) ylab(,angle(horizontal)) xtitle("Years Since First Permanent Job") ytitle("Salary (Thousands of 2015 Dollars)") ///
			title("Industry Salary by Postdoc Path", size(medlarge)) subtitle("${FIELD} PhDs", size(medium)) note("Note: Shows groups of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/SincePJ ID Salary by PD ${FIELD}.pdf", as(pdf) replace				
			
	// Graph raw salary for non-tenure track
	twoway (connected meanSAL yrsSincePJ if startPDTE==1 & count>=50 & yrsSincePJ<=30 & yrsSincePJ>=0, mcolor(purple) lcolor(purple) msymbol(S)) ///
			(rcap SALhi SALlo yrsSincePJ if startPDTE==1 & count>=50 & yrsSincePJ<=30 & yrsSincePJ>=0, lcolor(purple)) ///
			(connected meanSAL yrsSincePJ if startTE==1 & count>=50 & yrsSincePJ<=30 & yrsSincePJ>=0, mcolor(dkorange) lcolor(dkorange) msymbol(D)) ///
			(rcap SALhi SALlo yrsSincePJ if startTE==1 & count>=50 & yrsSincePJ<=30 & yrsSincePJ>=0, lcolor(dkorange)), ///
			legend(order(3 "Grad to Non-Tenure Track" 1 "Postdoc to Non-Tenure Track")) xlab(0(2)30) ylab(,angle(horizontal)) xtitle("Years Since First Permanent Job") ytitle("Salary (Thousands of 2015 Dollars)") ///
			title("Non-Tenure Track Salary by Postdoc Path", size(medlarge)) subtitle("${FIELD} PhDs", size(medium)) note("Note: Shows groups of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/SincePJ TE Salary by PD ${FIELD}.pdf", as(pdf) replace				

*** 7d. Postdoc Regression Coefficients ***
// Overall
use "${TEMP}/OOI_worksample_transitions.dta", clear

	// Create indicators for field of study
	tab phd_supField, gen(phdfield_)
	
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

	forvalues a=0(1)30 {
		regress SALi_Adj yrsPD startPDAC startPDID startPDTE startAC startID startTE phdfield_* i.phdcy_min [aw=wtsurvy1_f] if yrsOut==`a'
		estimates store yrsOut_`a'
	}
	
	estimates table yrsOut_*, se keep(yrsPD) stats(N)
	
	coefplot yrsOut_0 || yrsOut_1 || yrsOut_2 || yrsOut_3 || yrsOut_4 || yrsOut_5 || ///
			yrsOut_6 || yrsOut_7 || yrsOut_8 || yrsOut_9 || yrsOut_10 || yrsOut_11 || ///
			yrsOut_12 || yrsOut_13 || yrsOut_14 || yrsOut_15 || yrsOut_16 || yrsOut_17 || ///
			yrsOut_18 || yrsOut_19 || yrsOut_20 || yrsOut_21 || yrsOut_22 || yrsOut_23 || ///
			yrsOut_24 || yrsOut_25 || yrsOut_26 || yrsOut_27 || yrsOut_28 || yrsOut_29 || yrsOut_30 ///
			, keep(yrsPD) yline(0) bycoefs byopts(xrescale) vertical
	graph export "${MAIN}/RESULTS/Postdoc Impact on Salary BMS 2017.pdf", as(pdf) replace
	
*** 8a. Job Activity Spend Most Time ***
use "${DATA}/OOI_workingsample.dta", clear
keep if phd_supField == "${FIELD}"

	// Combine pjUN=. with pjUN=0
	replace pjUN=0 if pjUN==.
	
	// Tabulate wapri to create indicators
	tab wapri, gen(wapri_)
	
	// Combine pjUN=. with pjUN=0
	replace pjUN=0 if pjUN==.

		// Prepare unweighted numbers for disclosure purposes
		preserve
			gen count = 1
			collapse (mean) m_wapri_2=wapri_2 m_wapri_3=wapri_3 m_wapri_8=wapri_8 m_wapri_13=wapri_13 ///
					(sem) se_wapri_2=wapri_2 se_wapri_3=wapri_3 se_wapri_8=wapri_8 se_wapri_13=wapri_13 ///
					(sum) count, by(pj*)

			// Not graphing UN or NL
			drop if pjUN==1 | pjNL==1
			
			// Drop empty ones
			drop if pjPD==.
						
			rename m_* un_m_*
			rename se_* un_se_*
			rename count un_count
	
			save "${COUNT}/${FIELD} Most Hours Worked Unweighted.dta", replace
		restore
		
	// Collapse work activity variables by principal job
	gen count = 1
	collapse (mean) m_wapri_2=wapri_2 m_wapri_3=wapri_3 m_wapri_8=wapri_8 m_wapri_13=wapri_13 ///
			(sem) se_wapri_2=wapri_2 se_wapri_3=wapri_3 se_wapri_8=wapri_8 se_wapri_13=wapri_13 ///
			(sum) count [aw=wtsurvy1_f], by(pj*)
	
		// Merge in unweighted numbers
		merge 1:1 pj* using "${COUNT}/${FIELD} Most Hours Worked Unweighted.dta"
	
	// Create error bars
	foreach i in wapri_2 wapri_3 wapri_8 wapri_13 {
		gen `i'_hi = m_`i' + invttail(count-1,0.025)*se_`i'
		gen `i'_lo = m_`i' - invttail(count-1,0.025)*se_`i'			
	}		
	
	// Keep only the single principal job groups
	egen tempTot = rowtotal(pj*)
	keep if tempTot==1
	
	gen pj = "PD" if pjPD==1
	foreach i in AC TE ID NP GV NL UN {
		replace pj = "`i'" if pj`i'==1
	}
	order pj m_* se_* count *_hi *_lo
	
	// Graph order
	gen pjOrder = 1 if pj=="PD"
	replace pjOrder = 2 if pj=="AC"
	replace pjOrder = 3 if pj=="TE"
	replace pjOrder = 4 if pj=="ID"
	replace pjOrder = 5 if pj=="NP"
	replace pjOrder = 6 if pj=="GV"
	
	// Label for better graphs
	label define jobType 1 "Postdoc" 2 "Ten-Track (TT)" 3 "Non-TT" 4 "For-Profit" 5 "Non-Profit" 6 "Government", modify
	label val pjOrder jobType	

// Create as scatter instead	
	// Only graph if count>=50 or count>=5
		foreach i in 2 3 8 13 {
		    replace m_wapri_`i' = . if un_count<50
			replace se_wapri_`i' = . if un_count<50
			replace wapri_`i'_hi = . if un_count<50
			replace wapri_`i'_lo = . if un_count<50
			
			gen un_n_wapri_`i' = un_m_wapri_`i'*un_count
			replace m_wapri_`i' = . if un_n_wapri_`i'<5
			replace se_wapri_`i' = . if un_n_wapri_`i'<5
			replace wapri_`i'_hi = . if un_n_wapri_`i'<5
			replace wapri_`i'_lo = . if un_n_wapri_`i'<5		
		}	
		
	// Graph
	twoway (scatter m_wapri_2 pjOrder if un_count>=50 & un_n_wapri_2>=5, color(maroon) msymbol(Sh)) ///
			(scatter m_wapri_3 pjOrder if un_count>=50 & un_n_wapri_3>=5, color(navy) msymbol(Oh)) ///
			(scatter m_wapri_8 pjOrder if un_count>=50 & un_n_wapri_8>=5, color(forest_green) msymbol(Th)) ///
			(scatter m_wapri_13 pjOrder if un_count>=50 & un_n_wapri_13>=5, color(purple) msymbol(Dh)), ///
			graphregion(color(white)) bgcolor(white) title("Work Activities with Most Hours Spent") subtitle("By Job Type for ${FIELD} PhDs") ytitle("% of Jobs") ///
			legend(order(1 "Applied Research" 2 "Basic Research" 3 "Management" 4 "Teaching")) ///
			xlab(1 "Postdoc" 2 "Ten-Track (TT)" 3 "Non-TT" 4 "For-Profit" 5 "Non-Profit" 6 "Gov") ytitle("Fraction of Jobs") ylab(0(.2).8) xtitle("")
	graph export "${RESULTS}/Scatter Most Hours Work Activity ${FIELD}.pdf", replace	
	
*** 8b. Job Activity Spend Most Time By Carnegie Classification ***
use "${DATA}/OOI_workingsample.dta", clear
keep if phd_supField == "${FIELD}"

	// Combine pjUN=. with pjUN=0
	replace pjUN=0 if pjUN==.
	
	// Tabulate wapri to create indicators
	tab wapri, gen(wapri_)
	
	// Combine pjUN=. with pjUN=0
	replace pjUN=0 if pjUN==.

	// Merge on Carnegie classifications for current institutions
	merge m:1 refyr instcod using "${LOOKUPS}/CCbyYear_max.dta"
	drop if _merge==2
	drop _merge	
	
		// Prepare unweighted numbers for disclosure purposes
		preserve
			gen count = 1
			collapse (mean) m_wapri_2=wapri_2 m_wapri_3=wapri_3 m_wapri_8=wapri_8 m_wapri_13=wapri_13 ///
					(sem) se_wapri_2=wapri_2 se_wapri_3=wapri_3 se_wapri_8=wapri_8 se_wapri_13=wapri_13 ///
					(sum) count, by(pj* R1 R2 D1 D2)

			// Only keep the ones graphed
			keep if (R1==1 & R2==0 & D1==0 & D2==0) & (pjPD==1 | pjAC==1 | pjTE==1)
						
			rename m_* un_m_*
			rename se_* un_se_*
			rename count un_count
			
			save "${COUNT}/${FIELD} Most Hours Worked R1 Unweighted.dta", replace
		restore
	
	// Collapse work activity variables by principal job
	gen count = 1
	collapse (mean) m_wapri_2=wapri_2 m_wapri_3=wapri_3 m_wapri_8=wapri_8 m_wapri_13=wapri_13 ///
			(sem) se_wapri_2=wapri_2 se_wapri_3=wapri_3 se_wapri_8=wapri_8 se_wapri_13=wapri_13 ///
			(sum) count [aw=wtsurvy1_f], by(pj* R1 R2 D1 D2)

		// Merge in unweighted numbers
		merge 1:1 pj* R1 R2 D1 D2 using "${COUNT}/${FIELD} Most Hours Worked R1 Unweighted.dta"
				
	// Create error bars
	foreach i in wapri_2 wapri_3 wapri_8 wapri_13 {
		gen `i'_hi = m_`i' + invttail(count-1,0.025)*se_`i'
		gen `i'_lo = m_`i' - invttail(count-1,0.025)*se_`i'			
	}		
	
	// Keep only the single principal job groups
	egen tempTot = rowtotal(pj*)
	keep if tempTot==1
	
	gen pj = "PD" if pjPD==1
	foreach i in AC TE ID NP GV NL UN {
		replace pj = "`i'" if pj`i'==1
	}
	order pj m_* se_* count *_hi *_lo
	
	// Graph order
	gen pjOrder = 1 if pj=="PD"
	replace pjOrder = 2 if pj=="AC"
	replace pjOrder = 3 if pj=="TE"
	replace pjOrder = 4 if pj=="ID"
	replace pjOrder = 5 if pj=="NP"
	replace pjOrder = 6 if pj=="GV"
	
	// Label for better graphs
	label define jobType 1 "Postdoc" 2 "Ten-Track (TT)" 3 "Non-TT" 4 "For-Profit" 5 "Non-Profit" 6 "Government", modify
	label val pjOrder jobType	

	// Only graph if count>=50 or count>=5
		foreach i in 2 3 8 13 {
		    replace m_wapri_`i' = . if un_count<50
			replace se_wapri_`i' = . if un_count<50
			replace wapri_`i'_hi = . if un_count<50
			replace wapri_`i'_lo = . if un_count<50
			
			gen un_n_wapri_`i' = un_m_wapri_`i'*un_count
			replace m_wapri_`i' = . if un_n_wapri_`i'<5
			replace se_wapri_`i' = . if un_n_wapri_`i'<5
			replace wapri_`i'_hi = . if un_n_wapri_`i'<5
			replace wapri_`i'_lo = . if un_n_wapri_`i'<5		
		}	
			
	// Graph
	twoway (scatter m_wapri_2 pjOrder if un_count>=50 & un_n_wapri_2>=5 & (R1==1 & R2==0 & D1==0 & D2==0) & (pj=="PD" | pj=="AC" | pj=="TE"), color(maroon) msymbol(Sh)) ///
			(scatter m_wapri_3 pjOrder if un_count>=50 & un_n_wapri_3>=5 & (R1==1 & R2==0 & D1==0 & D2==0) & (pj=="PD" | pj=="AC" | pj=="TE"), color(navy) msymbol(Oh)) ///
			(scatter m_wapri_8 pjOrder if un_count>=50 & un_n_wapri_8>=5 & (R1==1 & R2==0 & D1==0 & D2==0) & (pj=="PD" | pj=="AC" | pj=="TE"), color(forest_green) msymbol(Th))  ///
			(scatter m_wapri_13 pjOrder if un_count>=50 & un_n_wapri_13>=5 & (R1==1 & R2==0 & D1==0 & D2==0) & (pj=="PD" | pj=="AC" | pj=="TE"), color(purple) msymbol(Dh)), ///
			graphregion(color(white)) bgcolor(white) title("Work Activities with Most Hours Spent") subtitle("By Job Type for ${FIELD} PhDs") ytitle("% of Jobs") ///
			legend(order(1 "Applied Research" 2 "Basic Research" 3 "Management" 4 "Teaching")) ///
			xlab(0.5 " " 1 "Postdoc" 2 "Ten-Track (TT)" 3 "Non-TT"  3.5 " ") ytitle("Fraction of Jobs") ylab(0(.2).8) xtitle("")
	graph export "${RESULTS}/Scatter Most Hours Work Activity R1 ${FIELD}.pdf", replace	
	
	

	
	
/* DEFUNCT

*** 7b. Split Postdoc Est. Salary ***
use "${TEMP}/Salary PAI2.dta", clear
keep if phd_supField == "${FIELD}"
	
	// Summarize
	sum SAL_pPAI2 [aw=wtsurvy1_f] if startPDAC==1 & startAC==0 & startID==0 & yrsSincePJ==5
	sum SAL_pPAI2 [aw=wtsurvy1_f] if startPDID==1 & startAC==0 & startID==0 & yrsSincePJ==5	
	sum SAL_pPAI2 [aw=wtsurvy1_f] if startAC==1 & startPD==0 & startID==0 & yrsSincePJ==5
	sum SAL_pPAI2 [aw=wtsurvy1_f] if startID==1 & startAC==0 & startPD==0 & yrsSincePJ==5
	
	// Make salary in thousands
	gen SAL_1000 = SAL_pPAI2/1000	
	
	// Collapse on adjusted salary predicted by GK method
	gen count=1
	collapse (mean) meanSAL=SAL_1000 (sem) seSAL=SAL_1000 ///
			(sum) count [aw=wtsurvy1_f], by(startPDAC startPDID startAC startID yrsSincePJ)
	
	// Only keep starting from first permanent job
	keep if yrsSincePJ>=0
	
	// Create error bars
	gen SALhi = meanSAL + invttail(count, 0.025)*seSAL
	gen SALlo = meanSAL - invttail(count, 0.025)*seSAL 
	
	// Graph expected salary for full sample
	twoway (connected meanSAL yrsSincePJ if startPDAC==1 & startPDID==0 & startAC==0 & startID==0 & count>=50 & yrsSincePJ<=30, mcolor(navy) lcolor(navy) msymbol(Oh)) ///
		(connected meanSAL yrsSincePJ if startPDID==1 & startPDAC==0 & startAC==0 & startID==0 & count>=50 & yrsSincePJ<=30, mcolor(maroon) lcolor(maroon) msymbol(Sh))  ///
		(connected meanSAL yrsSincePJ if startAC==1 & startPDAC==0 & startPDID==0 & startID==0 & count>=50 & yrsSincePJ<=30, mcolor(mint) lcolor(mint) msymbol(Th)) ///
		(connected meanSAL yrsSincePJ if startID==1 & startAC==0 & startPDAC==0 & startPDID==0 & count>=50 & yrsSincePJ<=30, mcolor(gray) lcolor(gray) msymbol(Dh)), ///
		legend(order(3 "Grad to Tenure-Track" 1 "Postdoc to Tenure-Track" 4 "Grad to Industry" 2 "Postdoc to Industry")) xlab(0(2)30) ylab(,angle(horizontal)) xtitle("Years Since First Permanent Job") ytitle("Est. Salary (Thousands of 2015 Dollars)") ///
		title("Est. Salary by Career Path", size(medlarge)) subtitle("${FIELD} PhDs", size(medium)) note("Note: Shows groups of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/Est Salary PAI2 by Split PD ${FIELD}.pdf", as(pdf) replace	

		// Create Counts Table
			// Only show the ones being graphed
			keep if (startPDAC==1 & startPDID==0 & startAC==0 & startID==0 & count>=50) | ///
					(startPDID==1 & startPDAC==0 & startAC==0 & startID==0 & count>=50) | ///
					(startAC==1 & startPDAC==0 & startPDID==0 & startID==0 & count>=50) | ///
					(startID==1 & startAC==0 & startPDAC==0 & startPDID==0 & count>=50)
			save "${COUNT}/${FIELD} Est Salary PAI2 by Split PD.dta", replace
			
*** 7c. Split Postdoc NPV ***
use "${TEMP}/Salary PAI2.dta", clear
keep if phd_supField=="${FIELD}"

	// Years Out from Graduation
	gen yrsOut = refyr - phdcy_min
	
	// Assume interest rate = 3%
	gen NPV_r = 0.03
	gen NPV_t = SAL_pPAI2 / (1+NPV_r)^yrsOut
	sort refid refyr
	by refid: gen NPV = sum(NPV_t)

	// Put in 1000s of dollars
	gen NPV_1000 = NPV/1000
	
	// Collapse on adjusted NPV -> to account for opportunity cost, should go from PhD graduation (include postdoc wages)
	gen count=1
	collapse (mean) meanNPV = NPV_1000 (sem) seNPV = NPV_1000 ///
			(sum) count [aw=wtsurvy1_f], by(startPDAC startPDID startAC startID yrsOut)
	
	// Create error bars
	gen NPVhi = meanNPV + invttail(count, 0.025)*seNPV
	gen NPVlo = meanNPV - invttail(count, 0.025)*seNPV 
	
	// Graph NPV for full sample 
	twoway (connected meanNPV yrsOut if startPDAC==1 & startPDID==0 & startAC==0 & startID==0 & count>=50 & yrsOut<=30, mcolor(navy) lcolor(navy) msymbol(Oh)) ///
		(connected meanNPV yrsOut if startPDID==1 & startPDAC==0 & startAC==0 & startID==0 & count>=50 & yrsOut<=30, mcolor(maroon) lcolor(maroon) msymbol(Sh)) ///
		(connected meanNPV yrsOut if startAC==1 & startPDAC==0 & startPDID==0 & startID==0 & count>=50 & yrsOut<=30, mcolor(mint) lcolor(mint) msymbol(Th)) ///
		(connected meanNPV yrsOut if startID==1 & startAC==0 & startPDAC==0 & startPDID==0 & count>=50 & yrsOut<=30, mcolor(gray) lcolor(gray) msymbol(Dh)), ///
		legend(order(3 "Grad to Tenure-Track" 1 "Postdoc to Tenure-Track" 4 "Grad to Industry" 2 "Postdoc to Industry")) xlab(0(2)30) ylab(,angle(horizontal)) xtitle("Years Since PhD Graduation") ytitle("Est. NPV (Thousands of 2015 Dollars)") ///
		title("Est. NPV by Career Path", size(medlarge)) subtitle("${FIELD} PhDs", size(medium)) note("Note: Shows groups of at least 50 individuals. Interest rate = 3%", size(vsmall))  graphregion(color(white)) bgcolor(white)
	graph export "${RESULTS}/NPV Salary PAI2 by Split PD Full ${FIELD}.pdf", as(pdf) replace	

		// Create Counts Table
			// Only show the ones being graphed
			keep if (startPDAC==1 & startPDID==0 & startAC==0 & startID==0 & count>=50) | ///
					(startPDID==1 & startPDAC==0 & startAC==0 & startID==0 & count>=50) | ///
					(startAC==1 & startPDAC==0 & startPDID==0 & startID==0 & count>=50) | ///
					(startID==1 & startAC==0 & startPDAC==0 & startPDID==0 & count>=50)
			save "${COUNT}/${FIELD} NPV Salary PAI2 by Split PD Full.dta", replace
	
*** 7d. Split Postdoc Pseudo-NPV ***
// Use this graph to demonstrate how important the postdoc shift is

use "${TEMP}/Salary PAI2.dta", clear
keep if phd_supField=="${FIELD}"

	// Only keep first 30 years since start PJ
	keep if yrsSincePJ>=0

	// Assume interest rate = 3%
	gen NPV_r = 0.03
	gen NPV_t = SAL_pPAI2 / (1+NPV_r)^yrsSincePJ
	sort refid refyr
	by refid: gen NPV = sum(NPV_t)

	// Put in 1000s of dollars
	gen NPV_1000 = NPV/1000
	
	// Collapse on adjusted NPV -> to account for opportunity cost, should go from PhD graduation (include postdoc wages)
	gen count=1
	collapse (mean) meanNPV = NPV_1000 (sem) seNPV = NPV_1000 ///
			(sum) count [aw=wtsurvy1_f], by(startPDAC startPDID startAC startID yrsSincePJ)
	
	// Create error bars
	gen NPVhi = meanNPV + invttail(count, 0.025)*seNPV
	gen NPVlo = meanNPV - invttail(count, 0.025)*seNPV 
	
	// Graph NPV for full sample 
	twoway (connected meanNPV yrsSincePJ if startPDAC==1 & startPDID==0 & startAC==0 & startID==0 & count>=50 & yrsSincePJ<=30, mcolor(navy) lcolor(navy) msymbol(Oh)) ///
		(connected meanNPV yrsSincePJ if startPDID==1 & startPDAC==0 & startAC==0 & startID==0 & count>=50 & yrsSincePJ<=30, mcolor(maroon) lcolor(maroon) msymbol(Sh)) ///
		(connected meanNPV yrsSincePJ if startAC==1 & startPDAC==0 & startPDID==0 & startID==0 & count>=50 & yrsSincePJ<=30, mcolor(mint) lcolor(mint) msymbol(Th)) ///
		(connected meanNPV yrsSincePJ if startID==1 & startAC==0 & startPDAC==0 & startPDID==0 & count>=50 & yrsSincePJ<=30, mcolor(gray) lcolor(gray) msymbol(Dh)), ///
		legend(order(3 "Grad to Tenure-Track" 1 "Postdoc to Tenure-Track" 4 "Grad to Industry" 2 "Postdoc to Industry")) xlab(0(2)30) ylab(,angle(horizontal)) xtitle("Years Since First Permanent Job") ytitle("Est. NPV (Thousands of 2015 Dollars)") ///
		title("Est. NPV by Career Path", size(medlarge)) subtitle("${FIELD} PhDs", size(medium)) note("Note: Shows groups of at least 50 individuals. Interest rate = 3%", size(vsmall))  graphregion(color(white)) bgcolor(white)
	graph export "${TEMP}/NPV Salary PAI2 First Permanent Job ${FIELD}.pdf", as(pdf) replace	

		// Create Counts Table
			// Only show the ones being graphed
			keep if (startPDAC==1 & startPDID==0 & startAC==0 & startID==0 & count>=50) | ///
					(startPDID==1 & startPDAC==0 & startAC==0 & startID==0 & count>=50) | ///
					(startAC==1 & startPDAC==0 & startPDID==0 & startID==0 & count>=50) | ///
					(startID==1 & startAC==0 & startPDAC==0 & startPDID==0 & count>=50)
			save "${COUNT}/${FIELD} NPV Salary PAI2 First Permanent Job.dta", replace	


*** 9. Carnegie Classifications (% R1) of Transitions ***
use "${DATA}/OOI_workingsample.dta", clear
keep if phd_supField=="${FIELD}"

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
	
	// Collapse
	gen count = 1
	collapse (mean) m_R1=R1	(sd) sd_R1=R1 ///
			(sum) n_R1=R1 N_R1=count [aw=wtsurvy1_f], by(ind_* phdcy_min)
	
	// Only keep if just has one indicator
	egen numInd = rowtotal(ind_*)
	keep if numInd==1	
	
	// Replace 0 with .
	foreach i in m sd {
	    replace `i'_R1=. if `i'_R1==0
	}
	
	// Replace n with . if no mean
	replace N_R1 = . if m_R1==.
	
	// 3 Year Moving Average
	sort ind_* phdcy_min
	by ind_*: gen m_R1N = m_R1[_n-1]
	by ind_*: gen m_R1P = m_R1[_n+1]
	
	egen m_R13 = rowmean(m_R1 m_R1N m_R1P)	
	
	// 3 Year Moving SE
	foreach i in sd N {
		by ind_*: gen `i'_R1N = `i'_R1[_n-1]
		by ind_*: gen `i'_R1P = `i'_R1[_n+1]
	}
	
		// 1. Calculate each (n-1)*sd^2 for each of the 3 years
		gen sptN = (N_R1N-1)*sd_R1N^2
		gen spt = (N_R1-1)*sd_R1^2
		gen sptP = (N_R1P-1)*sd_R1P^2
		
		// 2. Calculate numerator and denominator
		egen spn = rowtotal(sptN spt sptP)
		egen spd = rowtotal(N_R1N N_R1 N_R1P)
		egen spnm = rownonmiss(m_R1N m_R1 m_R1P)	// count # non-missing
		
		// 3. Calculate SP
		gen sp = sqrt(spn/(spd-spnm))
		
		// 4. Calculate SE
		foreach i in N_R1 N_R1N N_R1P {
		    gen i`i' = 1/`i'
		}
		egen ins = rowtotal(iN_R1 iN_R1N iN_R1P)
		gen se_R13 = sp*sqrt(ins)
	
		// 5. Clean up
		gen df_R13 = spd-spnm
		keep phdcy_min m_R13 se_R13 df_R13 ind_* N_R1 n_R1
	
	// Create error bars
	foreach i in R1 {
		gen `i'_hi3 = m_`i'3 + invttail(df_`i'3,0.025)*se_`i'3
		gen `i'_lo3 = m_`i'3 - invttail(df_`i',0.025)*se_`i'3			
	}
	
	// Count table
	drop if n_R1<5 | N_R1<50
	save "${COUNT}/${FIELD} 3 Yr R1 Transitions.dta", replace
	
	// Line Graph
	twoway (connected m_R13 phdcy_min if N_R1>=50 & n_R1>=5 & ind_GRtPD==1, cmissing(n) lcolor(gold) mcolor(gold) msymbol(A) msize(medsmall)) ///
			(connected m_R13 phdcy_min if N_R1>=50 & n_R1>=5 & ind_GRtAC==1, cmissing(n) lcolor(mint) mcolor(mint) msymbol(Th) msize(small)) ///
			(connected m_R13 phdcy_min if N_R1>=50 & n_R1>=5 & ind_GRtTE==1, cmissing(n) lcolor(dkorange) mcolor(dkorange) msymbol(Dh) msize(small)) ///
			(connected m_R13 phdcy_min if N_R1>=50 & n_R1>=5 & ind_PDtAC==1, cmissing(n) lcolor(navy) mcolor(navy) msymbol(Oh) msize(small)) ///
			(connected m_R13 phdcy_min if N_R1>=50 & n_R1>=5 & ind_PDtTE==1, cmissing(n) lcolor(purple) mcolor(purple) msymbol(Sh) msize(small)), ///
			legend(order(1 "Grad to Postdoc" 2 "Grad to Tenure-Track" 3 "Grad to Non-Tenure Track" 4 "Postdoc to Tenure-Track" 5 "Postdoc to Non-Tenure Track"))  ///
			xtitle("") xlab(1980(10)2015) ytitle("Fraction Transition to R1") title("Moving Average: Graduate and Postdoctoral Transitions to R1 Universities", size(medium)) subtitle("${FIELD} PhDs") ///
			note("Note: Shows cohorts of at least 50 individuals. Some job types combined or repressed due to low cell counts.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/3 Yr R1 Transitions ${FIELD}.pdf", as(pdf) replace		

*** 9. Carnegie Classifications (% R1) of Transitions ***
use "${DATA}/OOI_workingsample.dta", clear
keep if phd_supField=="${FIELD}"

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
	
	// Group by 5 years
	gen phdcy_5 = floor(phdcy_min/5)*5
	
	// Collapse
	gen count = 1
	collapse (mean) m_R1=R1	(sem) se_R1=R1 ///
			(sum) n_R1=R1 N_R1=count [aw=wtsurvy1_f], by(ind_* phdcy_5)
	
	// Only keep if just has one indicator
	egen numInd = rowtotal(ind_*)
	keep if numInd==1	
	
	// Replace 0 with .
	foreach i in m se {
	    replace `i'_R1=. if `i'_R1==0
	}
	
	// Replace n with . if no mean
	replace N_R1 = . if m_R1==.

	// Create error bars
	foreach i in R1 {
		gen `i'_hi3 = m_`i' + invttail(N_`i',0.025)*se_`i'
		gen `i'_lo3 = m_`i' - invttail(N_`i',0.025)*se_`i'			
	}
	
	// Count table
	drop if n_R1<5 | N_R1<50
	save "${COUNT}/${FIELD} 5 Yr R1 Transitions.dta", replace
	
	// Line Graph
	twoway (connected m_R1 phdcy_5 if N_R1>=50 & n_R1>=5 & ind_GRtPD==1, cmissing(n) lcolor(gold) mcolor(gold) msymbol(A) msize(medsmall)) ///
			(connected m_R1 phdcy_5 if N_R1>=50 & n_R1>=5 & ind_GRtAC==1, cmissing(n) lcolor(mint) mcolor(mint) msymbol(Th) msize(small)) ///
			(connected m_R1 phdcy_5 if N_R1>=50 & n_R1>=5 & ind_GRtTE==1, cmissing(n) lcolor(dkorange) mcolor(dkorange) msymbol(Dh) msize(small)) ///
			(connected m_R1 phdcy_5 if N_R1>=50 & n_R1>=5 & ind_PDtAC==1, cmissing(n) lcolor(navy) mcolor(navy) msymbol(Oh) msize(small)) ///
			(connected m_R1 phdcy_5 if N_R1>=50 & n_R1>=5 & ind_PDtTE==1, cmissing(n) lcolor(purple) mcolor(purple) msymbol(Sh) msize(small)), ///
			legend(order(1 "Grad to Postdoc" 2 "Grad to Tenure-Track" 3 "Grad to Non-Tenure Track" 4 "Postdoc to Tenure-Track" 5 "Postdoc to Non-Tenure Track"))  ///
			xtitle("") xlab(1980(10)2015) ytitle("Fraction Transition to R1") title("5-Year Graduate and Postdoctoral Transitions to R1 Universities", size(medium)) subtitle("${FIELD} PhDs") ///
			note("Note: Shows 5-year graduation cohorts of at least 50 individuals.", size(vsmall)) graphregion(color(white)) bgcolor(white)	
	graph export "${RESULTS}/5 Yr R1 Transitions ${FIELD}.pdf", as(pdf) replace		
	
*** 10. Transition from GR/PD to AC/TE, Stay at Same Institution? ***
use "${TEMP}/OOI_worksample_transitions.dta", clear
keep if phd_supField=="${FIELD}"

	// Job type
	foreach i in PD AC TE GV ID NP {
		gen ind`i' = (`i'i!=0 & `i'i!=.)
	}
	
	// Destring grad university
	rename phdinst phdinst_ORIG
	destring phdinst_ORIG, gen(phdinst) force

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
	
		// a. Transition from GR->PD at same university
		gen sameUniv_GP = 0 if startPD==1
		replace sameUniv_GP = 1 if startPD==1 & indPD==1 & instcod==phdinst
		
		// b. Indicator for tenure-track at grad university
		gen sameUniv_GT = 0 if startAC==1
		replace sameUniv_GT = 1 if startAC==1 & indAC==1 & instcod==phdinst
		
		// c. Indicator for non-tenure track at grad university
		gen sameUniv_GN = 0 if startTE==1
		replace sameUniv_GN = 1 if startTE==1 & indTE==1 & instcod==phdinst

		// d. Indicator for tenure-track at postdoc university
		gen sameUniv_PTt = 0 if startPDAC==1 & instcod!=.
		
		sort refid refyr
		by refid: replace sameUniv_PTt = 1 if startPDAC==1 & indAC==1 & indPD==0 & indPD[_n-1]==1 & indAC[_n-1]==0 & instcod==instcod[_n-1] & instcod!=.
		bys refid: egen sameUniv_PT = max(sameUniv_PTt)
		
		// e. Indicator for non-tenure track at postdoc university
		gen sameUniv_PNt = 0 if startPDTE==1 & instcod!=.
		
		sort refid refyr
		by refid: replace sameUniv_PNt = 1 if startPDTE==1 & indTE==1 & indPD==0 & indPD[_n-1]==1 & indTE[_n-1]==0 & instcod==instcod[_n-1] & instcod!=.
		bys refid: egen sameUniv_PN = max(sameUniv_PNt)	
		
	// Keep single copy per individual
	keep refid sameUniv_GP sameUniv_GT sameUniv_GN sameUniv_PT sameUniv_PN
	duplicates drop

	sum sameUniv_*
		