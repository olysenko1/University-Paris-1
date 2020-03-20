set more off
clear all
set linesize 90
set scheme s1mono

*Data load, set up as a panel 
use gfin1870.dta
describe
list ident year country3 cy by my in 1/20, clean
tabulate country
tabulate year
sort ident year
xtset ident year, yearly
describe
*tobin q is a string, need to
destring tobin, replace float dpcomma

*1)Compute the log of all variables, except savings.
global X "pa ik gers gert inc incpc cy by my xq dtftfp aage agr union spi ir life vola scap1 irr house_pr tobin"
foreach var in $X {
gen log`var' = log(`var')
}

*2)	Compute the difference of ln(gdp) and the difference of ln(gdpc) per capita
xtunitroot llc loginc, lags(5)
xtunitroot llc logincpc, lags(5)
*Panel data unit root test confirmes that ln(gdp) and ln(gdpc) contain unit root
global Y "loginc logincpc"
foreach var in $Y {
gen D`var' = d.`var'
}
*First observation for each country is missed since we took FD, thus we generated 21 missing values

*3)Average variables (after transformation of logs) over five year since 1870 to 2009, reducing the number of observations to 588 observations
global X "sav Dloginc Dlogincpc logpa logik loggers loggert loginc logincpc logcy logby logmy logxq logdtftfp logaage logagr logunion logspi logir loglife logvola logscap1 logirr loghouse_pr "

gen period = 5 *floor(year/5)
tab period
collapse $X logtobin, by (ident period)

*4)Replicate table 1 (descriptive statistics) and add a table for the remaining variables 
*Label variables just for convenience 
label variable logpa "patent flow"
label variable sav  "savings rate"
label variable logik "investment rate"
label variable loggers "secondary education"
label variable loggert "tertiary education"
label variable  loginc    "GDP"
label variable  logincpc  "GDP per capita"
label variable logcy "Credit/GDP"
label variable logby  "Bank assets/GDP"
label variable logmy "Money/GDP"
label variable logxq "R&D/GDP"
label variable logdtftfp  "Distance to USA productivity frontier"
label variable  logaage    "Fraction of population below 15 and over 64."
label variable  logagr  "Agriculture share"
label variable logunion "Unionization"
label variable logspi "Interaction term."
label variable logir "Interest rate"
label variable  loglife "Age expectancy at age 10"
label variable logvola "Volatility"
label variable logscap1 "Stock market capitalization"
label variable logirr "Real interest rate "
label variable loghouse_pr   "House price"

sum


*Set as panel data again after collapsing we have new time variable
egen time = group(period)
list time period in 1/20, sepby (period)
xtset ident time
xtdescribe

*4)Do the between and within transformation and the two way transformation for all variables.

*********BETWEEN transformation

foreach var in $X {
by ident: egen m`var' = mean(`var')
}

global T "msav mDloginc mDlogincpc mlogpa mlogik mloggers mloggert mloginc mlogincpc mlogcy mlogby mlogmy mlogxq mlogdtftfp mlogaage mlogagr mlogunion mlogspi mlogir mloglife mlogvola mlogscap1 mlogirr mloghouse_pr"

*********WITHIN transformation

foreach var in $X {
gen w`var' = `var' - m`var'
}
******** TWO WAY transformation
sort time ident

foreach var in $X {
by time: egen y`var'  = mean(`var')
}

global Y "ysav yDloginc yDlogincpc ylogpa ylogik yloggers yloggert yloginc ylogincpc ylogcy ylogby ylogmy ylogxq ylogdtftfp ylogaage ylogagr ylogunion ylogspi ylogir yloglife ylogvola ylogscap1 ylogirr yloghouse_pr"

sort ident time

foreach var in $Y {
by ident: egen m`var'  = mean(`var')
}

global Z "mysav myDloginc myDlogincpc mylogpa mylogik myloggers myloggert myloginc mylogincpc mylogcy mylogby mylogmy mylogxq mylogdtftfp mylogaage mylogagr mylogunion mylogspi mylogir myloglife mylogvola mylogscap1 mylogirr myloghouse_pr"

****** TWO WAY TRANSFORMED
foreach var in $X {
gen `var'2   = `var' - m`var' - y`var' + my`var'
}


*time variable has been changed again
keep if period >= 1950 
drop time
egen time = group(period)
xtset ident time

*6)Draft : For your dependent variable and credit/GDP and your period : 
//compare within (one way), between and two way distributions (graphs) and simple correlation (graphs) and comment.
global D "wlogik mlogik logik2 wlogcy mlogcy logcy2 "
foreach var in $D {
histogram `var', normal kdensity saving(distr`var')
tabstat `var', stat(min max mean median sd p25 p75)
}
graph combine distrwlogik.gph distrmlogik.gph distrlogik2.gph, saving(distcomb`var')
graph combine distrwlogcy.gph distrmlogcy.gph distrlogcy2.gph, saving(distcomb`var')

correlate mlogik mlogcy wlogik wlogcy logik2 logcy2
graph matrix mlogik mlogcy wlogik wlogcy logik2 logcy2

//7.	Report and comment results of one way (country fixed effects) and two way (country and year) fixed effect regressions with only credit/GDP 
//and then with other available covariates of the table corresponding to your dependent variable. 
//At this stage, you do not use additional instrument variables.
xtset ident time
***Regression with only credit/GDP and one way fixed effects 
xtserial logik logcy //null hypothesis of no serial correlation is strongly rejected.
xtreg logik logcy, fe vce(cluster ident) //we cluster errors to reduce the impact of serial correlation
*Ramsey test 
predict fit, xbu
gen fit_2=fit^2
gen fit_3=fit^3
xtreg logik logcy fit_2 fit_3, fe vce(cluster ident)
test fit_2=fit_3=0 //we have to reject the null and coclude that we have omitted variable bias 

***Regression with credit/GDP and two way fixed effects 
xtreg logik logcy i.time, fe vce(cluster ident)
testparm i.time //significant
predict u_hat, xbu
gen u_hat2=u_hat^2
gen u_hat3=u_hat^3
xtreg logik logcy u_hat2 u_hat3, fe vce(cluster ident)
test u_hat2=u_hat3=0 //correctly specified 

***Regression with credit/GDP, differenced GDPpc and two way fixed effects
xtserial logik logcy Dlogincpc//.
xtreg logik logcy Dlogincpc i.time, fe vce(cluster ident) //we cluster errors to reduce the impact of serial correlation
testparm i.time
drop fit fit_2 fit_3
predict fit, xbu
gen fit_2=fit^2
gen fit_3=fit^3 
xtreg logik logcy fit_2 fit_3, fe vce(cluster ident)
test fit_2=fit_3=0

***Regression with bank assets/GDP, differenced GDPpc and two way fixed effects
drop fit fit_2 fit_3
xtreg logik logby Dlogincpc i.time, fe vce(cluster ident) //we cluster errors to reduce the impact of serial correlation
testparm i.time
predict fit, xbu
gen fit_2=fit^2
gen fit_3=fit^3 
xtreg logik logcy fit_2 fit_3, fe vce(cluster ident)
test fit_2=fit_3=0

***Regression with money supply/GDP, differenced GDPpc and two way fixed effects
drop fit fit_2 fit_3
xtreg logik logmy Dlogincpc i.time, fe vce(cluster ident) //we cluster errors to reduce the impact of serial correlation
testparm i.time
predict fit, xbu
gen fit_2=fit^2
gen fit_3=fit^3 
xtreg logik logcy fit_2 fit_3, fe vce(cluster ident)
test fit_2=fit_3=0

*8)Propose at least five interesting regressions of your choice. 
*panel data test for serial correlation
xtserial logik Dlogincpc logmy 
xtserial logik Dlogincpc logby
*A significant test statistic indicates the presence of serial correlation.
keep $X ident time mlogik
corr
****INTERESTING REGRESSIONS*******
xtreg logik i.time, fe vce(cluster ident) // R^2 overall - 0,43

***1st interesting regression VAR
pvarsoc logik Dlogincpc, maxlag(3) pvaropts(instl(1/4)) //first-order panel VAR is the preferred model, since this has the smallest MBIC, MAIC and MQIC
pvar logik Dlogincpc, instl(1/4) gmmstyle overid 
pvargranger
pvarstable, graph
pvarfevd, mc(200)
pvarirf, mc(200)


***2nd interesting regresion 
xtabond2 logik Dlogincpc logcy, ///
    gmmstyle(L.logik, equation(diff) laglimits(1 2) collapse) ///
    gmmstyle(L.logik, equation(level) laglimits(0 0) collapse) iv(logagr, equation(diff)) robust
	
***3rd interesting regression
xtreg logik Dlogincpc logirr i.time, fe vce(cluster ident)
testparm i.time
predict fit, xbu
gen fit_2=fit^2
gen fit_3=fit^3 
xtreg logik logcy fit_2 fit_3, fe vce(cluster ident)
test fit_2=fit_3=0

***4th interesting regression
xtivreg2 logik Dlogincpc (logscap1=logvola), fe cl(ident)
xtoverid, cluster(ident)
xtreg logscap1 logvola, fe cluster(ident) //first stage
	
***5th interesting regression 
generate dummyik = 0 
replace dummyik = 1 if logik > mlogik
tab time, gen(y)
sum dummyik
count if dummyik==0
xtlogit dummyik Dlogincpc logcy y*, fe




