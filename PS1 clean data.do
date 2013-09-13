* Load data and start this thing
clear
cd "~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 1/"
use PS1.dta, clear

*---------------------------------------------
* Create new variables and clean the dataset
*---------------------------------------------
* Student race
recode race (1 3 = 1 "White/Asian") (nonmissing = 0 "Not White"), gen(st_whiteasian)
label var st_whiteasian "Student race is white or Asian"

* Birthday
* Convert to date
generate birthday_actual=mdy(birthmon, birthday, birthyea)  
* Format appropriately
format birthday_actual %dM_d,_CY
* Determine age on September 1, 1985
generate age1985=(mdy(9,1,1985) - birthday_actual) / 365.25  
label var birthday_actual "Student birthday"
label var age1985 "Age on September 1, 1985"

* Attrition rate
generate stay_in_sample = flagsgk==1 & flagsg1==1 & flagsg2==1 & flagsg3==1
label define attrition_label 0 "Dropped" 1 "Stayed"  
label values stay_in_sample attrition_label
label var stay_in_sample "Student remained in sample through 3rd grade"

* Missing free/reduced lunch
recode gkfreelu (nonmissing = 1 "Present") (missing = 0 "Missing"), gen(lunch_present)
label var lunch_present "Indicate whether free/reduced lunch data is present"

* Free/reduced lunch
recode gkfreelu (1 = 1 "Yes") (2 = 0 "No"), gen(freelunch)
label var freelunch "Kindergarten student received free lunch"  

* Student is girl
recode gender (1 = 0 "Non-girl") (2 = 1 "Girl"), gen(st_girl)
label var st_girl "Student is a girl"

* Teacher race
recode gktrace (1 3 = 1 "White/Asian") (nonmissing = 0 "Not White"), gen(t_whiteasian)
label var t_whiteasian "Teacher race is white or asian"

* Teacher MA
recode gkthighd(3 4 5 6 = 1 Masters_Degree) (nonmissing = 0 no_Masters), gen(teacher_MA)
label var teacher_MA "Teacher has MA or higher"

* Miscellaneous stuff
* Format variables correctly 
format gkclasss %4.2f
format gktyears %4.2f
* Permanently set the base level for gkclasst
fvset base 2 gkclasst


*--------------------------------------
* Super dumb percentile rank variable
*--------------------------------------
* Prepare tempfiles, get rid of extra variables to prepare for small percentile loop
tempfile small regular main
sort stdntid
save `main', replace
save `small', replace
save `regular', replace

* Calculate percentiles for regular class sizes
use `regular', clear
keep if gkclasst !=1
keep gktreads gkwordsk gktmaths stdntid

* For some reason xtile, nq(100) doesn't work right... its scores are just a little bit off.
* So you have to use your own round(x)/length(x) function
foreach x in gktreads gkwordsk gktmaths {
  gen `x'_regular = `x'
  egen `x'_reg_rank = rank(`x'_regular)
  egen `x'_reg_count = count(`x'_regular)
  gen `x'_reg_perc = round(`x'_reg_rank / `x'_reg_count, .01) 
  drop `x'_regular `x'_reg_count `x'_reg_rank
}

* Merge regular classes back into main dataset
save `regular', replace
merge 1:1 stdntid using `main'
drop _merge
sort stdntid
save `main', replace

* Create small dataset
use `small', clear
keep if gkclasst == 1
keep gktreads gkwordsk gktmaths stdntid
generate obs = _n
local end = _N
save `small', replace


* Calculate percentile rank for small class scores using regular class distribution
* --------------------------------------------------------------------------- *
* --------------------------------- WARNING --------------------------------- *
* --------------------------------------------------------------------------- *
* This takes like 15 minutes to run because of all the temp files and nested
* loops and stuff. Unfortunately there seems to be no other way to get this to
* work. Running this in R with sapply() and parallel takes under 90 seconds.
* Grr...
forvalues i = 1/1900 {
  foreach x in gktreads gkwordsk gktmaths {
    * Keep track of progress
    display "`i'.`x'"

    * Open the small dataset, append one observation to the regular dataset 
    use `small', clear
    quietly keep if obs == `i'
    append using `regular'

    * Determine percentile rank
    quietly egen `x'_small_rank = rank(`x')
    quietly egen `x'_small_count = count(`x')
    quietly generate `x'_small_perc = round(`x'_small_rank / `x'_small_count, .01)
    drop `x'_small_count `x'_small_rank

    * Only keep the small class observation
    quietly keep if obs == `i'
    sort stdntid

    * Merge single small observation into the main dataset
    quietly merge 1:1 stdntid using `main'
    sort stdntid
    drop _merge
    quietly save `main', replace
  }
}

* Determine average scores for three tests
egen avg_small_score = rowmean(gktreads_small_perc gkwordsk_small_perc gktmaths_small_perc)
egen avg_reg_score = rowmean(gktreads_reg_perc gkwordsk_reg_perc gktmaths_reg_perc)

* Combine class types into one column
gen avg_score = avg_small_score if gkclasst ==1
replace avg_score = avg_reg_score if gkclasst != 1
label var avg_score "Average percentile rank score for reading, word, and math test"

* Remove extra variables
drop obs avg_small_score avg_reg_score

* Save this so you never, ever have to run it again!
save "PS1 cleaned.dta", replace

* Boom.