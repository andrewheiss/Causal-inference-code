clear
cd "~/Documents/Duke 2013-2014/Fall 2013/PubPol 604/Problem set 4/"
use "Assignment4.dta" if YOB < 40, clear


*-------------
* Question 1
*-------------
* See R code for graph, since Stata sucks at plotting stuff


*-------------
* Question 2
*-------------

* Table 5, column 1
reg LWKLYWGE EDUC i.YOB

* Table 5, column 2

* Manually
* First stage
regress EDUC YOB YOB##QOB
predict double educ_predicted

* Second stage
regress LWKLYWGE educ_predicted i.YOB
* IT WORKS! Except standard errors are wrong
* See http://www.stata.com/support/faqs/statistics/instrumental-variables-regression/ to fix that

* Automatically
ivregress 2sls LWKLYWGE i.YOB (EDUC = i.YOB i.YOB##i.QOB)


*-------------
* Question 3
*-------------
regress EDUC YOB YOB##QOB
* Or...
ivregress 2sls LWKLYWGE i.YOB (EDUC = i.YOB i.YOB##i.QOB), first


*-------------
* Question 4
*-------------
ivregress liml LWKLYWGE i.YOB (EDUC = i.YOB i.YOB##i.QOB), first


*-------------
* Question 5
*-------------
generate hs_grad = EDUC >= 12
reg LWKLYWGE hs_grad i.YOB
ivregress 2sls LWKLYWGE i.YOB (hs_grad = i.YOB i.YOB##i.QOB), first
ivregress liml LWKLYWGE i.YOB (hs_grad = i.YOB i.YOB##i.QOB)


*-------------
* Question 6
*-------------
generate college_grad = EDUC >= 16
reg LWKLYWGE college_grad i.YOB
ivregress 2sls LWKLYWGE i.YOB (college_grad = i.YOB i.YOB##i.QOB), first
ivregress liml LWKLYWGE i.YOB (college_grad = i.YOB i.YOB##i.QOB)
