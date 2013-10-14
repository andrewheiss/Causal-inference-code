clear
cd "~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 3/"
use "PS3.dta", clear
* Main research question: does failing to get a bonus in one year improve performance in the next year?

*-------------
* Question 1
*-------------
gen complier = (avgrow05 > 0 & expgrow == 1) | (avgrow05 <= 0 & expgrow == 0)
gen growth = avgrow05 > 0
tabulate growth expgrow, chi2 column cchi2


*-------------
* Question 2
*-------------
* rd syntax: rd outcomevar [treatmentvar] assignmentvar
* The default is 50 bootstrap replications, but that takes a while
bootstrap, rep(10) : rd avgrow06 expgrow avgrow05, mbw(100)


*-------------
* Question 3
*-------------
* Parts a and b
* Make lots of graphs
rd avgrow06 expgrow avgrow05, graph bdep

* Part c
* Check the covariates
* Do these in R probably, since Stata graphs are ugly
scatter enroll avgrow05
scatter pblack avgrow05
scatter phisp avgrow05
scatter pfrl avgrow05


*-------------
* Question 4
*-------------
* Create parametric variables
gen avgrow05_2 = avgrow05^2
gen avgrow05_3 = avgrow05^3
gen avgrow05_4 = avgrow05^4

* Cubic regression
reg avgrow06 avgrow05 avgrow05_2 avgrow05_3 pblack phisp pfrl enroll elemschl midschl highschl expgrow

* Fancy cheating way to do this... interact the continuous version of the variable 3 times
reg avgrow06 c.avgrow05##c.avgrow05##c.avgrow05 pblack phisp pfrl enroll elemschl midschl highschl expgrow


*-------------
* Question 5
*-------------
* Part a
reg avgrow06 avgrow05 avgrow05_2 avgrow05_3 expgrow

* Part b
reg avgrow06 avgrow05 avgrow05_2 pblack phisp pfrl enroll elemschl midschl highschl expgrow

* Part c
reg avgrow06 avgrow05 avgrow05_2 avgrow05_3 avgrow05_4 pblack phisp pfrl enroll elemschl midschl highschl expgrow

* Part d
reg avgrow06 avgrow05 avgrow05_2 avgrow05_3 pblack phisp pfrl enroll expgrow if elemschl == 1
