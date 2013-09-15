* Load cleaned data
clear
cd "~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 1/"
use "PS1 cleaned.dta", clear

*-------------
* Question 1
*-------------
* Table 1, panel A
* Free lunch across kindergarten class types
tabulate gkclasst gkfreelu, row
oneway gkfreelu gkclasst, tabulate

* Race
tabulate gkclasst st_whiteasian, row
oneway st_whiteasian gkclasst, tabulate

* Age in 1985
tabulate gkclasst, sum(age1985)
oneway age1985 gkclasst, tabulate

* Attrition
tabulate gkclasst stay_in_sample, row
oneway stay_in_sample gkclasst, tabulate

* Class size in kindergarten
tabulate gkclasst, sum(gkclasss)  
oneway gkclasss gkclasst, tabulate

* Percentile score in kindergarten
tabulate gkclasst, sum(avg_score)
oneway avg_score gkclasst, tabulate

* Missing school lunch
tabulate gkclasst lunch_present, row
oneway lunch_present gkclasst, tabulate


*-------------
* Question 2
*-------------
* Model 1
foreach x in gktreads gktmaths gktlists gkwordsk {
  regress `x' i.gkclasst, cluster(gktchid)
  outreg2 using Problem2_`x', word se dec(2) title(OLS: Actual Class Size - `x') ctitle(1) addn("Standard errors corrected with Huber-White clustering for gktchid") replace
}

* Model 2
foreach x in gktreads gktmaths gktlists gkwordsk {
  xtreg `x' i.gkclasst, i(gkschid) fe cluster(gktchid) nonest
  outreg2 using Problem2_`x', word se dec(2) title(OLS: Actual Class Size - `x') ctitle(2) append
}

* Model 3
foreach x in gktreads gktmaths gktlists gkwordsk {
  xtreg `x' i.gkclasst st_whiteasian st_girl freelunch, i(gkschid) fe cluster(gktchid) nonest
  outreg2 using Problem2_`x', word se dec(2) title(OLS: Actual Class Size - `x') ctitle(3) append
}

* Model 4
foreach x in gktreads gktmaths gktlists gkwordsk {
  xtreg `x' i.gkclasst st_whiteasian st_girl freelunch t_whiteasian gktyears teacher_MA, i(gkschid) fe cluster(gktchid) nonest
  outreg2 using Problem2_`x', word se dec(2) title(OLS: Actual Class Size - `x') ctitle(4) append
}


*-------------
* Question 3
*-------------
*---------
* Part a
xtreg g8tmaths i.gkclasst st_whiteasian st_girl freelunch, i(gkschid) fe vce(cluster g8schid) nonest
outreg2 using Problem3A, word se dec(2) title(OLS: Actual Class Size) ctitle(1) replace


*---------
* Part b
* Imputation
mi set wide
mi register imputed g8tmaths
mi impute regress g8tmaths st_whiteasian st_girl freelunch, add(1) rseed(1234) force

* Indicator variable for imputed values
gen imputed = 0
replace imputed = 1 if g8tmaths != _1_g8tmaths
label var imputed "Score was imputed" 

* Save imputed column to dataset
mi set, clear 

* Regression!
xtreg g8tmaths_1_ i.gkclasst st_whiteasian st_girl freelunch imputed, i(g8schid) fe vce(cluster g8schid) nonest


*---------
* Part c
summ gktmaths, d
gen median_math = r(p50)
gen min_math = r(min)
gen max_math = r(max)
gen attritor = (g8tmaths == .)
gen med_attr_flag = (gktmaths > median_math & attritor ==1)
replace med_attr_flag = . if attritor == 0
gen med_attr_score = min_math if med_attr_flag == 0
replace med_attr_score = max_math if med_attr_flag == 1

* Is the difference between 
sum med_attr_score, d
sum g8tmaths, d

tabulate gkclasst, sum(med_attr_score)
tabulate gkclasst, sum(g8tmaths)
oneway med_attr_score gkclasst, tabulate
oneway g8tmaths gkclasst, tabulate

* Differences grow with impuations, but with a lot more uncertainty (126 stdev vs 46)


*---------
* Part d
* Do part a, b, and c for reading, science, and social science test scores
* Do part c for reading test scores (can't do c for science and social science)


*-------------
* Question 4
*-------------
* Teacher race
tabulate gkclasst t_whiteasian, row
oneway t_whiteasian gkclasst, tabulate

* Teacher education
tabulate gkclasst teacher_MA, row
oneway teacher_MA gkclasst, tabulate

* Teacher experience
tabulate gkclasst, sum(gktyears)  
oneway gktyears gkclasst, tabulate