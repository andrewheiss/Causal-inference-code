* Load data and start this thing
clear
cd "~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 2/"

*-------------
* Question 1
*-------------
* Quasi-experiment with no control group and no pre-test, looking at 2006 data
use PS2.dta if year == 2006 & everevac == 1, clear

* Weeks worked last year
mean wkswork

* Earnings
mean earnings 

* Unemployment compensation
mean unempinc

* Health status
tabulate health


*-------------
* Question 2
*-------------
* Pre-evaluation outcomes compared to post-evacuation outcomes
use PS2.dta if everevac == 1, clear

* Weeks worked last year
oneway wkswork year 
ttest wkswork, by(year) 

* Earnings
oneway earnings year
ttest earnings, by(year) unequal

* Unemployment compensation
oneway unempinc year
ttest unempinc, by(year) unequal

* Health status
tabulate health year, chi2 column 
* spineplot health year

* Regressions
regress wkswork year age black sex hsgrad someco ba postgrad, robust
regress earnings year age black sex hsgrad someco ba postgrad, robust
regress unempinc year age black sex hsgrad someco ba postgrad, robust
ologit health year age black sex hsgrad someco ba postgrad, robust

* Cheat and use graphics from R. 
* There's no way I'm spending hours figuring out how to program Monte Carlo
* simulations of predicted ologit probabilities in Stata... R is better for
* that anyway :)


*-------------
* Question 3
*-------------
* Non-evacuees as control, using March 2006 data
use PS2.dta if year == 2006, clear

* Weeks worked last year
oneway wkswork everevac 
ttest wkswork, by(everevac) unequal 

* Earnings
oneway earnings everevac
ttest earnings, by(everevac) unequal

* Unemployment compensation
oneway unempinc everevac
ttest unempinc, by(everevac) unequal

* Health status
tabulate health everevac, chi2 column cchi2 
* spineplot health everevac

regress wkswork everevac age black sex hsgrad someco ba postgrad
regress earnings everevac age black sex hsgrad someco ba postgrad
regress unempinc everevac age black sex hsgrad someco ba postgrad
ologit health everevac age black sex hsgrad someco ba postgrad

* Use R graphics again :)


*-------------
* Question 4
*-------------
use PS2.dta, clear

* Two groups: evacuees, non-evacuees (everevac)
* Two times: 2005, 2006 
recode year (2005 = 0) (2006 = 1), gen(after_storm) 
gen interaction = after_storm*everevac

* Simple models
regress wkswork after_storm everevac interaction, robust
regress earnings after_storm everevac interaction, robust
regress unempinc after_storm everevac interaction, robust
ologit health after_storm everevac interaction, robust

* With controls
regress wkswork after_storm everevac interaction age black sex hsgrad someco ba postgrad, robust
regress earnings after_storm everevac interaction age black sex hsgrad someco ba postgrad, robust
regress unempinc after_storm everevac interaction age black sex hsgrad someco ba postgrad, robust
ologit health after_storm everevac interaction age black sex hsgrad someco ba postgrad, robust


*-------------
* Question 5
*-------------
* Part A
* Noncompliers (i.e. people who are not currently evacuated) vs. compliers (i.e. people who are still evacuated) in 2006
* Split evacpost into two variables: complier*post and noncomplier*post
* complier*post = evacnow
* noncomplier = 1 when denier and backhome are 1
gen noncomplier=1 if denier==1 | backhome==1
replace noncomplier=0 if denier==0 & backhome==0

reg wkswork noncomplier evacnow year everevac

regress wkswork noncomplier evacnow after_storm everevac age black sex hsgrad someco ba postgrad, robust
regress earnings noncomplier evacnow after_storm everevac age black sex hsgrad someco ba postgrad, robust
regress unempinc noncomplier evacnow after_storm everevac age black sex hsgrad someco ba postgrad, robust
ologit health noncomplier evacnow after_storm everevac black sex hsgrad someco ba postgrad, robust


* Part B
tabulate evacnow everevac if year==2006
regress evacnow everevac if year==2006