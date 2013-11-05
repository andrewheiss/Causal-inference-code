clear
cd "~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 4/"
use "Assignment4.dta" if YOB < 40, clear

*-------------
* Question 1
*-------------
* See R code for graph, since Stata sucks at plotting stuff


*--------------------
* Questions 2 and 3
*--------------------

* Table 5, column 1
reg LWKLYWGE EDUC i.YOB
outreg2 using Question2.xls, se dec(5) title(Quarter of Birth Instrument) ctitle(OLS) keep(EDUC) nocons addn("Constants surpressed") addtext(Year fixed effects, Yes) alpha(0.001, 0.01, 0.05) replace

* Table 5, column 2
* Manually
* First stage
* regress EDUC YOB YOB##QOB
* predict double educ_predicted

* Second stage
* regress LWKLYWGE educ_predicted i.YOB
* IT WORKS! Except standard errors are wrong
* See http://www.stata.com/support/faqs/statistics/instrumental-variables-regression/ to fix that

* Automatically
* ivregress 2sls LWKLYWGE i.YOB (EDUC = i.YOB i.YOB##i.QOB)
* Get the chi-squared
* predict e, resid
* xi: regress e i.YOB i.QOB##i.YOB, robust
* display e(r2) * e(N)

* Faster, better way
* First stage
regress EDUC i.YOB i.YOB##QOB
local first_f = e(F)

* Full IV
ivregress 2sls LWKLYWGE i.YOB (EDUC = i.YOB i.YOB##i.QOB), first
estat overid
local chi_score = r(score)
local chi_df = r(df)

estat firststage
local real_f = r(mineig)

* Output everything
outreg2 using Question2.xls, se dec(5) ctitle(2SLS) keep(EDUC) nocons addtext(Year fixed effects, Yes) addstat(Chi-squared, `chi_score', Chi-squared df, `chi_df', F-statistic, `real_f', Raw F-statistic from first stage, `first_f') append


*-------------
* Question 4
*-------------
* LIML IV
ivregress liml LWKLYWGE i.YOB (EDUC = i.YOB i.YOB##i.QOB), first
estat overid
local chi_score = r(ar)
local chi_df = r(ar_df)

estat firststage
local real_f = r(mineig)

* Output everything
outreg2 using Question2.xls, se dec(5) ctitle(LIML) keep(EDUC) nocons addtext(Year fixed effects, Yes) addstat(Chi-squared, `chi_score', Chi-squared df, `chi_df', F-statistic, `real_f', Raw F-statistic from first stage, `first_f') append


*-------------
* Question 5
*-------------
generate hs_grad = EDUC >= 12

* OLS
reg LWKLYWGE hs_grad i.YOB
outreg2 using Question5.xls, se dec(5) title(High School Graduation Instrument) ctitle(OLS) keep(hs_grad) nocons addn("Constants surpressed") addtext(Year fixed effects, Yes) alpha(0.001, 0.01, 0.05) replace


* 2SLS
* First stage
regress hs_grad i.YOB i.YOB##QOB
local first_f = e(F)

* Full IV
ivregress 2sls LWKLYWGE i.YOB (hs_grad = i.YOB i.YOB##i.QOB), first
estat overid
local chi_score = r(score)
local chi_df = r(df)

estat firststage
local real_f = r(mineig)

* Output everything
outreg2 using Question5.xls, se dec(5) ctitle(2SLS) keep(hs_grad) nocons addtext(Year fixed effects, Yes) addstat(Chi-squared, `chi_score', Chi-squared df, `chi_df', F-statistic, `real_f', Raw F-statistic from first stage, `first_f') append


* LIML IV
ivregress liml LWKLYWGE i.YOB (hs_grad = i.YOB i.YOB##i.QOB), first
estat overid
local chi_score = r(ar)
local chi_df = r(ar_df)

estat firststage
local real_f = r(mineig)

* Output everything
outreg2 using Question5.xls, se dec(5) ctitle(LIML) keep(hs_grad) nocons addtext(Year fixed effects, Yes) addstat(Chi-squared, `chi_score', Chi-squared df, `chi_df', F-statistic, `real_f', Raw F-statistic from first stage, `first_f') append


*-------------
* Question 6
*-------------
generate college_grad = EDUC >= 16

* OLS
reg LWKLYWGE college_grad i.YOB
outreg2 using Question6.xls, se dec(5) title(College Graduation Instrument) ctitle(OLS) keep(college_grad) nocons addn("Constants surpressed") addtext(Year fixed effects, Yes) alpha(0.001, 0.01, 0.05) replace


* 2SLS
* First stage
regress college_grad i.YOB i.YOB##QOB
local first_f = e(F)

* Full IV
ivregress 2sls LWKLYWGE i.YOB (college_grad = i.YOB i.YOB##i.QOB), first
estat overid
local chi_score = r(score)
local chi_df = r(df)

estat firststage
local real_f = r(mineig)

* Output everything
outreg2 using Question6.xls, se dec(5) ctitle(2SLS) keep(college_grad) nocons addtext(Year fixed effects, Yes) addstat(Chi-squared, `chi_score', Chi-squared df, `chi_df', F-statistic, `real_f', Raw F-statistic from first stage, `first_f') append


* LIML IV
ivregress liml LWKLYWGE i.YOB (college_grad = i.YOB i.YOB##i.QOB), first
estat overid
local chi_score = r(ar)
local chi_df = r(ar_df)

estat firststage
local real_f = r(mineig)

* Output everything
outreg2 using Question6.xls, se dec(5) ctitle(LIML) keep(college_grad) nocons addtext(Year fixed effects, Yes) addstat(Chi-squared, `chi_score', Chi-squared df, `chi_df', F-statistic, `real_f', Raw F-statistic from first stage, `first_f') append
