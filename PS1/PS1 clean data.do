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
* UPDATE! There's no need to do the giant 20-minute-long loop anymore. This new method (thanks to Ben!) works instantly and is just as accurate. Go back to this file's first commit in the git repository to see the old loop.

* Stata unfortunately doesn't let you pass multiple variables into a loop, unlike Python, where you can do:
*   for thing1, thing2, thing3 in mylist:
*     print(thing1, thing2, thing3)
* So instead, we can fake it in Stata by creating a string with quoted chunks insde. -tokenize- will then separate each chunk into space-delimited parts, accessible in the loop with `1', `2', etc.
* It's hacky, but it works.
local vars_to_parse = `" "gktreads reading" "gkwordsk words" "gktmaths math" "' 

* Create necessary variables (*_reg/small and *_reg_perc)
foreach x of local vars_to_parse {
  * Separate x into two parts: 1=variable, 2=label)
  tokenize `x'

  * Mark if observation is in a small or regular class
  gen `2'_reg = `1' if gkclasst != 1
  gen `2'_small = `1' if gkclasst == 1

  * Calculate requisite parts of the rank(x)/length(x) formula
  egen `2'_reg_rank = rank(`2'_reg)
  egen `2'_reg_count = count(`2'_reg_rank)
  gen `2'_reg_perc = round(`2'_reg_rank / `2'_reg_count, .01)

  * Don't need these anymore
  drop `2'_reg_rank `2'_reg_count
}

* Magic percentile thing
foreach x of local vars_to_parse {
  tokenize `x'
  
  * Save a snapshot of current dataset
  preserve

  * Drop all unnecessary data and calculate percentiles for remaining data
  keep if flagsgk == 1
  keep `1' `2'_reg_perc
  drop if `1' == .
  sort `1'
  duplicates drop `1' `2'_reg_perc, force
  replace `2'_reg_perc = `2'_reg_perc[_n-1] if missing(`2'_reg_perc)

  * Save to a temporary file and restore original dataset
  tempfile `2'_mapping
  save `2'_mapping, replace
  restore
}

* Get rid of temporary *_reg_perc variables 
drop reading_reg_perc words_reg_perc math_reg_perc

* Merge the percentiles from the temporary datasets back in
foreach x of local vars_to_parse {
  tokenize `x'
  merge m:m `1' using `2'_mapping
  drop _merge
}

* Create average score
egen avg_score = rowmean(reading_reg_perc words_reg_perc math_reg_perc)

* Save file
save "PS1 cleaned.dta", replace

* Boom.
