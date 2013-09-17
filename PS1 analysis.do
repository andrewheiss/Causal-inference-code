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

* Combined table of final model for each subject test
foreach x in gktreads gktmaths gktlists gkwordsk {
  xtreg `x' i.gkclasst st_whiteasian st_girl freelunch t_whiteasian gktyears teacher_MA, i(gkschid) fe cluster(gktchid) nonest
  outreg2 using Combined, word se dec(2) title(OLS: Actual Class Size - All four tests) ctitle(`x') append
}


*-------------
* Question 3
*-------------
*---------
* Part a
xtreg g8tmaths i.gkclasst st_whiteasian st_girl freelunch, i(gkschid) fe vce(cluster g8schid) nonest
outreg2 using Problem3A, word se dec(2) title(OLS: Actual Class Size) ctitle(1) replace

* Check if attrition is equal across class types
* Missing 8th grade data
recode g8tmaths (nonmissing = 1 "Present") (missing = 0 "Missing"), gen(g8maths_present)
label var g8maths_present "Indicate whether 8th grade math test data is present"
oneway g8maths_present gkclasst, tabulate


*---------
* Part b
* Imputation
mi set wide
mi register imputed g8tmaths
mi impute regress g8tmaths st_whiteasian st_girl freelunch, add(1) rseed(1234) force

* Indicator variable for imputed values
gen imputed = 0
replace imputed = 1 if g8tmaths != _1_g8tmaths
label var imputed "Math score was imputed" 

* Save imputed column to dataset
mi set, clear 

* Regression!
xtreg g8tmaths_1_ i.gkclasst st_whiteasian st_girl freelunch imputed, i(g8schid) fe vce(cluster g8schid) nonest
outreg2 using Problem3B, word se dec(2) title(OLS: Actual Class Size) ctitle(1) replace


*---------
* Part c
* Get median, min, and max from summary information
format g8tmaths %4.2f

quietly centile(gktmaths)
generate median_math_k = r(c_1)

quietly summ g8tmaths, d
generate min_math_8 = r(min)
generate max_math_8 = r(max)

* Indicator variable to mark imputation
generate attritor_math_8 = (g8tmaths == .)
label var attritor_math_8 "Math score was imputed"

* Max 8th grade score for those above the kindergarten median; min 8th grade score for those below the kindergarten median
generate attr_math_8_score = max_math_8 if gktmaths >= median_math_k & attritor_math_8 == 1
replace attr_math_8_score = min_math_8 if gktmaths < median_math_k & attritor_math_8 == 1

* Combine scores
generate adjusted_math_8 = g8tmaths
replace adjusted_math_8 = attr_math_8_score if attr_math_8_score != .
label var adjusted_math_8 "8th grade math scores with extreme imputations"

* Look at differences in medians
centile(adjusted_math_8)
centile(g8tmaths)
local orig_median = r(c_1)

signrank adjusted_math_8 = `orig_median'

* Do the differences between treatment groups grow or shrink after imputation?
oneway adjusted_math_8 gkclasst, tabulate
oneway g8tmaths gkclasst, tabulate

xtreg adjusted_math_8 i.gkclasst st_whiteasian st_girl freelunch attritor_math_8, i(g8schid) fe vce(cluster g8schid) nonest
outreg2 using Problem3C, word se dec(2) title(OLS: Actual Class Size) ctitle(1) replace


*---------
* Part d
* Do parts a and b for reading, science, and social science test scores
* For some reason Stata complains about this...
rename g8tmaths_1_ g8tmaths_stata

* Loop!
foreach x of varlist g8treads g8scienc g8social {
  * Part a
  xtreg `x' i.gkclasst st_whiteasian st_girl freelunch, i(gkschid) fe vce(cluster g8schid) nonest
  outreg2 using Problem3D_`x', word se dec(2) title(OLS: Actual Class Size) ctitle(1) replace

  * Part b
  * Imputation
  mi set wide
  mi register imputed `x'
  mi impute regress `x' st_whiteasian st_girl freelunch, add(1) rseed(1234) force

  * Indicator variable for imputed values
  gen imputed_`x' = 0
  replace imputed_`x' = 1 if `x' != _1_`x'
  label var imputed_`x' "Score was imputed" 

  * Save imputed column to dataset
  mi set, clear 

  * Regression!
  xtreg `x'_1_ i.gkclasst st_whiteasian st_girl freelunch imputed, i(g8schid) fe vce(cluster g8schid) nonest
  outreg2 using Problem3D2_`x', word se dec(2) title(OLS: Actual Class Size) ctitle(1) replace

  rename `x'_1_ `x'_stata
}


* Do part c for reading test scores (can't do c for science and social science)
format g8treads %4.2f

quietly centile(gktreads)
generate median_read_k = r(c_1)

quietly summ g8treads, d
generate min_read_8 = r(min)
generate max_read_8 = r(max)

* Indicator variable to mark imputation
generate attritor_read_8 = (g8treads == .)
label var attritor_read_8 "Reading score was imputed"

* Max 8th grade score for those above the kindergarten median; min 8th grade score for those below the kindergarten median
generate attr_read_8_score = max_read_8 if gktreads >= median_read_k & attritor_read_8 == 1
replace attr_read_8_score = min_read_8 if gktreads < median_read_k & attritor_read_8 == 1

* Combine scores
generate adjusted_read_8 = g8treads
replace adjusted_read_8 = attr_read_8_score if attr_read_8_score != .
label var adjusted_read_8 "8th grade reading scores with extreme imputations"

* Look at differences in medians
centile(adjusted_read_8)
centile(g8treads)
local orig_median = r(c_1)

signrank adjusted_read_8 = `orig_median'

* Do the differences between treatment groups grow or shrink after imputation?
oneway adjusted_read_8 gkclasst, tabulate
oneway g8treads gkclasst, tabulate

xtreg adjusted_read_8 i.gkclasst st_whiteasian st_girl freelunch attritor_read_8, i(g8schid) fe vce(cluster g8schid) nonest
outreg2 using Problem3D3, word se dec(2) title(OLS: Actual Class Size) ctitle(1) replace


*-------------
* Question 4
*-------------
* Teacher experience
* Simple way
tabulate gkclasst, sum(gktyears)  
oneway gktyears gkclasst, tabulate

* Control for school differences
xtreg gktyears i.gkclasst, i(gkschid) fe vce(cluster gkschid)
test 1.gkclasst 3.gkclasst


* Teacher race
* Simple way
tabulate gkclasst t_whiteasian, row
oneway t_whiteasian gkclasst, tabulate

* Control for school differences
xtreg gktrace i.gkclasst, i(gkschid) fe vce(cluster gkschid)
test 1.gkclasst 3.gkclasst


* Teacher education
* Simple way
tabulate gkclasst teacher_MA, row
oneway teacher_MA gkclasst, tabulate

* Control for school differences
xtreg gkthighd i.gkclasst, i(gkschid) fe vce(cluster gkschid)
test 1.gkclasst 3.gkclasst
