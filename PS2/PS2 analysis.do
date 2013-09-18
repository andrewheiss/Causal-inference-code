* Load data and start this thing
clear
cd "~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 2/"

*-------------
* Question 1
*-------------
* Quasi-experiment with no control group and no pre-test, looking at 2006 data
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
tabulate health everevac, chi2 column 
spineplot health everevac


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
spineplot health year

* Regressions
regress wkswork evacpost age black sex hsgrad someco ba postgrad
regress earnings evacpost age black sex hsgrad someco ba postgrad
regress unempinc evacpost age black sex hsgrad someco ba postgrad
ologit health evacpost age black sex hsgrad someco ba postgrad

* Cheat and use graphics from R. 
* There's no way I'm spending hours figuring out how to program Monte Carlo simulations of predicted ologit probabilities in Stata...