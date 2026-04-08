*--------------------------------------------------------------------------------
*         	 S-052: Intermediate and Advanced Statistical Methods 
*						for Applied Educational Research
*                             		Spring 2026
*		 		      Week 8 Data Analytic Memo Starter Code
*
*
* Programming:
*   Author:         Melanie Rucinski
*   Last Modified:  October 2025
*--------------------------------------------------------------------------------

*--------------------------------------------------------------------------------
* Set-up: clear memory, set local directory, and open a log file
*--------------------------------------------------------------------------------
* Clear all computer memory and delete any existing stored graphs and matrices:
    clear all
  
* Define the working directory:
    cd "your directory here"		

* Open a log file (the file will save in the directory you set above)
	* Replace "filename" with an informative name of your choice
	log using "filename.smcl", replace

*************Part 1: Multilevel modeling in SEDA********************************	
	
*--------------------------------------------------------------------------------
* Exploratory data analysis (not for any questions)
*--------------------------------------------------------------------------------

use SEDA19.dta, clear

	* Drop DC and HI which each have only have 1 school district.
	drop if stateabb == "DC" | stateabb == "HI" 

	* Set plot range
	gen range = sesavgall < 3 & sesavgall >- 4 & meanavg > 1 & meanavg < 10

	* Trellis plot illustrating separate regression lines by state
	graph twoway (scatter meanavg sesavgall if range, msize(vtiny)) ///
		(lfit meanavg sesavgall, range(-4 3)), xla(-4(2)3) ///
		by(stateabb, compact legend(off) graphregion(color(white))) ///
		ytitle("District Average Test Score, 2009-2016") ///
		xtitle(District Socioeconomic Status) name(trellis,replace)

	* Saving intercepts and slopes for each state
	statsby intercept = _b[_cons] slope = _b[sesavgall], by(stateabb) ///
		saving(sepreg.dta, replace): regress meanavg sesavgall 
	
	* Merging in intercepts and slopes for each state
	sort stateabb 
	merge m:1 stateabb using sepreg.dta
	drop _merge
	
	* Predicted average test scores for each district given SES
	gen predicted = intercept + slope*sesavgall
	sort stateabb sesavgall 
	
	* Overlaid linear fit
	graph twoway line predicted sesavgall, connect(ascending) ///
		ytitle("District Average Test Score, 2009-2016") ///
		xtitle(District Socioeconomic Status) yla(1(1)9) xla(-4(2)3) ///
		graphregion(color(white)) xsize(7) name(overlaid,replace)

*--------------------------------------------------------------------------------
* Building multilevel regression models
*--------------------------------------------------------------------------------
	
	* Establish a binary variable for states that had growing public school 
	* enrollments from 2009 to 2016. 
	* Data from https://nces.ed.gov/programs/digest/d19/tables/dt19_203.20.asp
	// Generate an indicator variable here that equals 1 for the following states:
		// AL, CT, IL, ME, MI, MS, MO, NH, NY, OH, PA, RI, VT, WV, WI
	
	* Use the xtset command here to set state as the level 2 variable:
		// fill in code here
	
	* Model 0: Run your intercept-only model here
	eststo clear // Clear any currently stored estimates
	/* Use the xtreg command with the ", mle" option to run your intercept-only random effects model. 
		Important!: Restrict your regression to districts with non-missing SES so that the sample 
		is the same between this regression and the following regressions. */
	eststo: // your regression code here
		// These commands store the variance components for output in the table:
		estadd scalar sigma2_u = e(sigma_u)^2
		estadd scalar sigma2_e = e(sigma_e)^2
		estadd scalar neg2ll = -2*e(ll)

	* Model 1: This code runs a fixed effects model. We provide it since it is not a focus of this assignment.
	eststo: xtreg meanavg sesavgall, fe

	* Model 2: Here, run a random intercepts regression of test scores on SES, again using the ", mle" option.
	eststo: // your regression code here 
		// Store variance components:
		estadd scalar sigma2_u = e(sigma_u)^2
		estadd scalar sigma2_e = e(sigma_e)^2
		estadd scalar neg2ll = -2*e(ll)

	* Model 3: Now add the level-2 predictor "growing" to your regression.
	eststo: // your regression code here 
		// Store variance components:
		estadd scalar sigma2_u = e(sigma_u)^2
		estadd scalar sigma2_e = e(sigma_e)^2
		estadd scalar neg2ll = -2*e(ll)
	
	* Output the table:
	esttab, se scalar(df_m neg2ll pr2 sigma2_u sigma2_e rho) compress nonumbers ///
	title("Predicting mathematics achievement from SES and school type") ///
	mtitles("Model 0" "Model 1" "Model 2" "Model 3") 
	
	* For the optional question: Fixed effects model with level 2 predictor. Why does this fail?	
	xtreg meanavg sesavgall growing, fe
	
*--------------------------------------------------------------------------------
* Supplementary visualization: A caterpillar plot to visualize random intercepts
*--------------------------------------------------------------------------------	
	
	* A trick to tag one district per state for plotting
	egen statetag = tag(stateabb)
	
	* Fit a mixed model equivalent to Model 3 and save random intercepts
	mixed meanavg sesavgall || state: , mle
	predict randints, reffects
	predict reffse, reses
	
	* Preparing for the caterpillar plot
	* A trick to calculate the rank of the 49 states
	gsort +randints -statetag
	generate rank = sum(statetag)
	* A trick to place the labels of each state just above the top of the error bar
	generate labelpos = randints + 1.96*reffse
	
	* Caterpillar plot of Empirical Bayes random intercepts with 95% Confidence Intervals
	serrbar randints reffse rank if statetag, addplot(scatter labelpos rank if statetag, msize(tiny) ///
		msymbol(none) mlabel(stateabb) mlabsize(small) mlabangle(90) mlabpos(1)) ///
		scale(1.96) xtitle(Rank) ytitle("Random state intercept with 95% CI, in grade levels") ///
		legend(off) yline(0) xsize(7) graphregion(color(white)) name(caterpillar,replace)

*************Part 2: Interrater and interitem reliability***********************

*--------------------------------------------------------------------------------
* RQ1 Data: Intraclass Correlations for Interrater Reliability (Raters Nested)
*--------------------------------------------------------------------------------

use KoreanMedicalPortfolios.dta, clear

	hist score, discrete graphregion(color(white)) freq name(allscores,replace)
	
	graph box score, over(rater) b1title(rater) graphregion(color(white)) name(raterscores,replace)
	tab rater, summarize(score)
	
	graph box score, over(item) b1title(item) graphregion(color(white)) name(itemscores,replace)
	tab item, summarize(score)

	* Begin with the overall score
	
	xtset student
	eststo clear
	
	* Estimate intraclass correlations for each item
	forvalues i = 1/5 {
		* With small sample sizes and balanced panels, use "re" with no "mle" option
		eststo: xtreg score if item == `i', re    
		  estadd scalar sigma2_u = e(sigma_u)^2
		  estadd scalar sigma2_e = e(sigma_e)^2
	}
	
	esttab, se scalar(sigma2_u sigma2_e rho) compress nonumbers ///
	title("Interrater reliability coefficients for item scores") ///
	mtitles("Goals" "Process" "Reflect" "Plans" "Clarity")

*--------------------------------------------------------------------------------
* Optional: What are interrater reliabilities for average item scores?
*--------------------------------------------------------------------------------

	* Estimate the intraclass correlation for the average of the 5 item scores
	egen srgroup = group(student rater) // Create an indicator for each student-rater.
	egen avgscore = mean(score), by(srgroup) // Average rater's score across 5 items.
	
	* Estimate interrater reliability for rater average scores across items
	xtreg avgscore if item == 1, re 
	
*--------------------------------------------------------------------------------
* RQ2 Analysis: Intraclass Correlations for Interitem Reliability (Items Crossed)
*--------------------------------------------------------------------------------

	xtset student
	eststo clear
	
	* Estimate intraclass correlations for each item
	forvalues r = 1/5 {
		
		* With small sample sizes and balanced panels, use "reml" option
		eststo: mixed score || _all:R.student || _all:R.item if rater==`r', reml
		
	* This strange looking code grabs each estimated variance component 
	* See: http://www.ats.ucla.edu/stat/stata/faq/diparm.htm 
	_diparm lns1_1_1, f(exp(@)^2) d(2*exp(@)^2)
	estadd scalar sig2pers = r(est) : est`r'

	_diparm lns1_2_1, f(exp(@)^2) d(2*exp(@)^2)
	estadd scalar sig2item = r(est) : est`r'
	
	_diparm lnsig_e, f(exp(@)^2) d(2*exp(@)^2)
	estadd scalar sig2e = r(est) : est`r'

	}
	
	* Ignore the "lns" rows, these are the log of the variance components.
	esttab, se scalar(sig2pers sig2item sig2e) compress nonumbers ///
	title("Interitem reliability coefficients by rater") ///
	mtitles("Rater 1" "Rater 2" "Rater 3" "Rater 4" "Rater 5")
	
	eststo clear
	
	* Compare to alpha for Rater 1
	keep if rater == 1
	keep student item score
	reshape wide score, i(student) j(item)
	
	* Cronbach's alpha
	alpha score1-score5, item

// END
		