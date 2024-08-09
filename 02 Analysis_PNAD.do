
/*******************************************************************************
Written by Samyam
This .do file takes the clean data filfe and performs most of the necessary
analysis.
*******************************************************************************/

	set more off

	cd "C:/Users/Samyam/OneDrive - University of Georgia/Samyam - Hugo/Brazil - Venezuela refugees"

	use "final.dta", clear

*Dropping Amazonas and Para from the dataset since we don't need them.

	keep if state=="AC"|state=="AP"|state=="RR"
	
*Creating real income variables taking 2012 as the benchmark year.

	foreach i in main_usual_cash main_usual_goods main_refer_cash main_refer_goods part_usual_cash part_usual_goods part_refer_cash ///
	part_refer_goods other_usual_cash other_usual_goods other_refer_cash other_refer_goods {
		replace `i'="" if `i'=="NA"
		destring `i', replace
	}

*More cleaning.

	foreach i in education occupation_detailed sector {
		replace `i'="" if `i'=="NA"
		destring `i', replace
	}

*Defining formal and informal employment (for this, we referred to the Brazilian government's instructions on what they identify as informal sector)

	*NOTE: The previous definition of formal was: formal=(occupation_detailed==1|occupation_detailed==3|((occupation_detailed==8 | occupation_detailed==9) & cnpj1=="1"))
	
	gen formal=(occupation_detailed==1|occupation_detailed==3|((occupation_detailed==8 | occupation_detailed==9) & cnpj1=="1"))
	gen informal=(occupation_detailed==2|occupation_detailed==4|occupation_detailed==10|((occupation_detailed==8 | occupation_detailed==9) & cnpj1=="2"))
	gen neither=!(formal==1|informal==1)

	gen formal_with_public=(occupation_detailed==1|occupation_detailed==3|occupation_detailed==5)
	gen informal_with_public=(occupation_detailed==2|occupation_detailed==4|occupation_detailed==6)
	gen neither_with_public=!(formal==1|informal==1)

	*Reference: https://valorinveste.globo.com/mercados/brasil-e-politica/noticia/2020/12/29/taxa-de-informalidade-avanca-no-trimestre-movel-ate-outubro-aponta-ibge.ghtml
	*We found that share of the labor force in informal sector is .577.

*Generating income variables. We don't need all of these variables, but just in case. We are mostly sticking with total_income_refer. We check robustness using other variables.

	gen total_income_refer = cond(missing(main_refer_cash), 0, main_refer_cash) + cond(missing(main_refer_goods), 0, main_refer_goods) + ///
		cond(missing(part_refer_cash), 0, part_refer_cash) + cond(missing(part_refer_goods), 0, part_refer_goods) + ///
		cond(missing(other_refer_cash), 0, other_refer_cash) + cond(missing(other_refer_goods), 0, other_refer_goods)
	replace total_income_refer=. if missing(main_refer_cash) & missing(main_refer_goods) & missing(part_refer_cash) & missing(part_refer_goods) & ///
		missing(other_refer_cash) & missing(other_refer_goods)
	gen log_income_refer=log(total_income_refer+1)
	gen asinh_income_refer=asinh(total_income_refer)

	gen total_income_usual = cond(missing(main_usual_cash), 0, main_usual_cash) + cond(missing(main_usual_goods), 0, main_usual_goods) + ///
		cond(missing(part_usual_cash), 0, part_usual_cash) + cond(missing(part_usual_goods), 0, part_usual_goods) + ///
		cond(missing(other_refer_cash), 0, other_refer_cash) + cond(missing(other_refer_goods), 0, other_refer_goods)
	replace total_income_usual =. if missing(main_usual_cash) & missing(main_usual_goods) & missing(part_usual_cash) & missing(part_usual_goods) & ///
		missing(other_usual_cash) & missing(other_usual_goods)
	gen log_income_usual=log(total_income_usual+1)
	gen asinh_income_usual=asinh(total_income_usual)

	gen total_income_main_usual = cond(missing(main_usual_cash), 0, main_usual_cash) + cond(missing(main_usual_goods), 0, main_usual_goods)
	replace total_income_main_usual=. if missing(main_usual_cash) & missing(main_usual_goods)
	gen log_income_main_usual = log(total_income_main_usual+1)
	gen asinh_income_main_usual=asinh(total_income_main_usual)
	
	gen total_income_main_refer = cond(missing(main_refer_cash), 0, main_refer_cash) + cond(missing(main_refer_goods), 0, main_refer_goods)
	replace total_income_main_refer=. if missing(main_refer_cash) & missing(main_refer_goods)
	gen log_income_main_refer = log(total_income_main_refer+1)
	gen asinh_income_main_refer=asinh(total_income_main_refer)
	
	gen total_income_main_refer2 = cond(missing(main_refer_cash), 0, main_refer_cash)
	replace total_income_main_refer2 =. if missing(main_refer_cash)
	gen log_income_main_refer2 = log(total_income_main_refer+1)
	gen asinh_income_main_refer2 =asinh(total_income_main_refer)

	*There are a lot of observations with main_usual_cash>0 but main_refer_cash=0. We talked about the reference month being the one we'll use.

*This is for the state dummies, which needs to be in numeric form.

	gen state_num=state
	replace state_num="1" if state=="AC"
	replace state_num="2" if state=="AP"
	replace state_num="3" if state=="RR"
	destring state_num, replace

*Keeping observations only of individuals who are of working age.

	//drop if age<18 | age>65
	*drop if age<14

	*The data covers wages for individuals from 14 years of age and above. But I'm afraid including 14-18 year olds will affect unemployment analysis we're doing below.

********************************************************************************
********************************************************************************
***************************ANALYSIS STARTS HERE*********************************
********************************************************************************
********************************************************************************

*First, I create some variables that I will eventually use in the analysis.

	gen age_sq = age^2
	gen treat=(state=="RR")			//Roraima is the treatment group.
	gen yearxtreat = year*treat		//Mind you, it's year x treat, not year dummies x treat.
	gen post=(year>=2014)
	gen treatxpost=treat*post
	
/*
	*Hourly earnings analysis
	replace main_usual_hours="" if main_usual_hours=="NA"
	destring main_usual_hours, replace
	gen hourly_wage = (cond(missing(main_usual_cash), 0, main_usual_cash) + cond(missing(main_usual_goods), 0, main_usual_goods)) / main_usual_hours
	replace hourly_wage=. if missing(main_usual_cash) & missing(main_usual_goods)
	gen asinh_hourly = asinh(hourly_wage)
	gen log_hourly = log(hourly_wage)

	*Unemployment analysis
	replace searching_job="" if (searching_job=="2"|searching_job=="NA")
	destring searching_job, replace
	gen employed=!missing(occupation_detailed)
	gen unemployed=(searching_job==1)
	gen economically_active=(employed==1|unemployed==1)
	gen neither_employed=!(employed==1|unemployed==1)
*/

*************************PNAD COMPOSITION PRE- VS POST-*************************

/*Since PNAD is repeated cross-section, I test whether the composition of PNAD
has changed before and after the treatment.

	reg male treat post treatxpost, cluster(state)
	nbreg education treat post treatxpost, cluster(state)
	nbreg age treat post treatxpost, cluster(state)
*/
**********************************PRE VS POST***********************************

*reg log_income_main_refer treatxpost education young white i.year i.state if economically_active==1, fe vce(cluster state)

/*
preserve
twfe unemployed treatxpost education young white if economically_active==1, ids(state_num year) cluster(state_num)
restore

preserve
twfe log_income_main_refer2 treatxpost education young white if formal==1, ids(state_num year) cluster(state_num)
restore

preserve
twfe log_income_main_refer2 treatxpost education young white if informal==1, ids(state_num year) cluster(state_num)
restore

preserve
twfe log_income_main_refer2 treatxpost education young white if formal_with_public==1, ids(state_num year) cluster(state_num)
restore

preserver
twfe log_income_main_refer2 treatxpost education young white if informal_with_public==1, ids(state_num year) cluster(state_num)
restore
*/


keep log_income_main_refer treat post treatxpost age age_sq education race male state_num year state formal informal occupation_detailed trimester total_income_main_refer yearxtreat sector blue_collar capital
drop if year>2017
order state_num, a(state)
order age_sq, a(age)
order trimester, a(year)
br
tab state, m
tab year, m
******************/
save "C:/Users/Samyam/OneDrive - University of Georgia/Samyam - Hugo/Brazil - Venezuela refugees/Hugo.dta", replace

	xi i.state_num i.year
*Overall
	*eststo reg1: reg log_income_main_refer treat post treatxpost _I*, cluster(state)										//Overall market, without covariates
	*eststo reg2: reg log_income_main_refer treat post treatxpost age age_sq education race male _I*, cluster(state)		//Overall market, with covariates
	*eststo reg3: reg log_income_main_refer treat post treatxpost _I* if informal==1, cluster(state)						//Informal market, without covariates
	eststo reg4: reg log_income_main_refer treat post treatxpost age age_sq education race male _I* if informal==1, cluster(state)		//Informal market, with covariates
	*eststo reg5: reg log_income_main_refer treat post treatxpost _I* if formal==1, cluster(state)						//Formal market, without covariates
	*eststo reg6: reg log_income_main_refer treat post treatxpost age age_sq education race male _I* if formal==1, cluster(state)						//Formal market, with covariates

*Overall
	*eststo reg7: reg log_income_main_refer treat post treatxpost _I* if educ<=12, cluster(state)										//Overall market, without covariates
	*eststo reg8: reg log_income_main_refer treat post treatxpost age age_sq education race male _I* if educ<=12, cluster(state)		//Overall market, with covariates
	*eststo reg9: reg log_income_main_refer treat post treatxpost _I* if informal==1 &  educ<=12, cluster(state)						//Informal market, without covariates
	*eststo reg10: reg log_income_main_refer treat post treatxpost age age_sq education race male _I* if informal==1 & educ<=12, cluster(state)						//Informal market, with covariates
	*eststo reg11: reg log_income_main_refer treat post treatxpost _I* if formal==1 & educ<=12, cluster(state)						//Formal market, without covariates
	*eststo reg12: reg log_income_main_refer treat post treatxpost age age_sq education race male _I* if formal==1 & educ<=12, cluster(state)						//Formal market, with covariates

	*			esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 reg9 reg10 reg11 reg12 using "Analysis and Cleaning/Outputs/testing_201213vs2017_including_young.csv", ///
	*		stats(N r2) title("Effect on logged earnings") starlevels( * 0.10 ** 0.05 *** 0.010) se r2 replace b(3) label

*EVENT STUDI:			
			
			reg log_income_main_refer treat io2013.yearxtreat male education age age_sq race _I* if informal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq race _cons) yline(0) vertical omitted ci(95) title("INFORMAL market wages") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
		
*Conditional on education

e
	*Total income

		//These six regressions have different covariates as per Hugo's approach.
		eststo reg1: reg log_income_main_refer post treatxpost education young white _I*, cluster(state)
		eststo reg1: reg log_income_main_refer post treatxpost education young white _I* if formal==1, cluster(state)
		eststo reg1: reg log_income_main_refer post treatxpost education young white _I* if informal==1, cluster(state)
		eststo reg1: reg log_income_main_refer post treatxpost education young white _I* if formal_with_public==1, cluster(state)
		eststo reg1: reg log_income_main_refer post treatxpost education young white _I* if informal_with_public==1, cluster(state)

		*Entire sample
		eststo reg1: reg log_income_main_refer treat post treatxpost male education age age_sq _I*, cluster(state)

		*Formal: Full sample
		eststo reg2: reg log_income_main_refer treat post treatxpost male education age age_sq _I* if formal==1, cluster(state)

		*Informal: Full sample
		eststo reg3: reg log_income_main_refer treat post treatxpost male education age age_sq _I* if informal==1, cluster(state)

		*Formal: Food, sales, construction
		eststo reg4: reg log_income_main_refer treat post treatxpost male education age age_sq _I* if formal==1 & (sector==3|sector==4|sector==6), cluster(state)

		*Informal: Food, sales, construction
		eststo reg5: reg log_income_main_refer treat post treatxpost male education age age_sq _I* if informal==1 & (sector==3|sector==4|sector==6), cluster(state)

		esttab reg1 reg2 reg3 reg4 reg5 using "Analysis and Cleaning/Outputs/diff-in-diff_1.tex", ///
			stats(N r2) title("Effect on logged earnings") starlevels( * 0.10 ** 0.05 *** 0.010) se r2 replace b(3) label

		eststo clear

	*Unemployment, and involvement in formal or informal sectors

//These regressions have different covariates.
reg unemployed post treatxpost education young white _I* if economically_active==1, cluster(state)
reg unemployed post treatxpost education young white _I* if economically_active==1 & !(occupation_detailed==5 | occupation_detailed==6 | occupation_detailed==7), cluster(state)

reg formal post treatxpost education young white _I* if economically_active==1, cluster(state)
reg formal post treatxpost education young white _I* if economically_active==1 & !(occupation_detailed==5 | occupation_detailed==6 | occupation_detailed==7), cluster(state)

reg informal post treatxpost education young white _I* if economically_active==1, cluster(state)
reg informal post treatxpost education young white _I* if economically_active==1 & !(occupation_detailed==5 | occupation_detailed==6 | occupation_detailed==7), cluster(state)

		xi i.state_num i.year
		eststo reg1: reg unemployed treat post treatxpost male education age age_sq _I* if economically_active==1, cluster(state)
		eststo reg2: reg unemployed treat post treatxpost male education age age_sq _I* if economically_active==1 & !(occupation_detailed==5 | ///
			occupation_detailed==6 | occupation_detailed==7), cluster(state)

		xi i.state_num i.year i.sector
		eststo reg3: reg formal treat post treatxpost male education age age_sq _I* if economically_active==1, cluster(state)
		eststo reg4: reg formal treat post treatxpost male education age age_sq _I* if economically_active==1 & !(occupation_detailed==5 | ///
			occupation_detailed==6 | occupation_detailed==7), cluster(state)

		eststo reg5: reg informal treat post treatxpost male education age age_sq _I* if economically_active==1, cluster(state)
		eststo reg6: reg informal treat post treatxpost male education age age_sq _I* if economically_active==1 & !(occupation_detailed==5 | ///
			occupation_detailed==6 | occupation_detailed==7), cluster(state)

		esttab reg1 reg2 reg3 reg4 reg5 reg6 using "Analysis and Cleaning/Outputs/diff-in-diff_2.tex", ///
			stats(N r2) title("Effect on employment and type") starlevels( * 0.10 ** 0.05 *** 0.010) se r2 replace b(3) label

		eststo clear

********************************TOTAL EARNINGS**********************************

*2013 is the dummy since it's a year before the treatment started.
*Should also try with 2014 as the dummy for robustness checks.
	
	*Formal
	reg asinh_income_main_usual treat io2014.yearxtreat male education age age_sq _I* if formal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Impact on FORMAL labor market earnings") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/01.01 Income (Formal - Full Sample).png", replace

	*Informal
	reg asinh_income_main_usual treat io2014.yearxtreat male education age age_sq _I* if informal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Impact on INFORMAL labor market earnings") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/01.02 Income (Informal - Full Sample).png", replace

*********************************HOURLY EARNINGS********************************

*Formal labor market: Hourly wages

	reg log_hourly treat io2014.yearxtreat male education age age_sq _I* if formal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Impact on hourly earnings in FORMAL labor market") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/02.01 Hourly Income (Formal - Full Sample).png", replace

*Informal labor market: Hourly wages

	reg log_hourly treat io2014.yearxtreat male education age age_sq _I* if informal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Impact on hourly earnings in INFORMAL labor market") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/02.02 Hourly Income (Informal - Full Sample).png", replace

**********************************HOURS WORKED**********************************

*Formal labor market: Hours worked

	reg main_usual_hours treat io2014.yearxtreat male education age age_sq _I* if formal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Impact on hours worked in FORMAL labor market") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/03.01 Hours Worked (Formal - Full Sample).png", replace

*Informal labor market: Hours worked

	reg main_usual_hours treat io2014.yearxtreat male education age age_sq _I* if informal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Impact on hours worked in INFORMAL labor market") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/03.02 Hours Worked (Informal - Full Sample).png", replace

**********************************UNEMPLOYMENT**********************************

	xi i.state_num i.year
	reg unemployed treat io2014.yearxtreat male education age age_sq _I* if economically_active==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Impact on probability of being unemployment") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/04.01 Unemployed Probability.png", replace

************PROBABILITY OF BEING INVOLVED IN FORMAL/INFORMAL SECTORS************

xi i.state_num i.year i.sector

*Formal

	reg formal treat io2014.yearxtreat male education age age_sq _I*, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the FORMAL labor market") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/05.01 Formal Probability.png", replace

*Informal

	reg informal treat io2014.yearxtreat male education age age_sq _I*, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the INFORMAL labor market") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/05.02 Informal Probability.png", replace

***************PROBABILITY OF BEING INVOLVED IN A SPECIFIC SECTOR***************

*Food or construction or sales industry

	gen food_con_sales_industry=(sector==3|sector==4|sector==6)

	*Overall
	reg food_con_sales_industry treat io2014.yearxtreat male education age age_sq _I* if economically_active==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the food industry")

	*Formal
	reg food_con_sales_industry treat io2014.yearxtreat male education age age_sq _I* if formal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the FORMAL food industry")

	*Informal
	reg food_con_sales_industry treat io2014.yearxtreat male education age age_sq _I* if informal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the INFORMAL food industry")

*Food industry

	gen food_industry=(sector==6)

	*Overall
	reg food_industry treat io2014.yearxtreat male education age age_sq _I* if economically_active==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the food industry")

	*Formal
	reg food_industry treat io2014.yearxtreat male education age age_sq _I* if formal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the FORMAL food industry")

	*Informal
	reg food_industry treat io2014.yearxtreat male education age age_sq _I* if informal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the INFORMAL food industry")

*Construction industry

	gen constr_industry=(sector==3)

	*Overall
	reg constr_industry treat io2014.yearxtreat male education age age_sq _I* if economically_active==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the construction industry")

	*Formal
	reg constr_industry treat io2014.yearxtreat male education age age_sq _I* if formal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the FORMAL construction industry")

	*Informal
	reg constr_industry treat io2014.yearxtreat male education age age_sq _I* if informal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the INFORMAL construction industry")

*Sales industry

	gen sales_industry=(sector==4)

	*Overall
	reg sales_industry treat io2014.yearxtreat male education age age_sq _I* if economically_active==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the construction industry")

	*Formal
	reg sales_industry treat io2014.yearxtreat male education age age_sq _I* if formal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the FORMAL construction industry")

	*Informal
	reg sales_industry treat io2014.yearxtreat male education age age_sq _I* if informal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the INFORMAL construction industry")

*Health, education, and social service industry

	gen healtheduc_industry=(sector==9)

	*Overall
	reg healtheduc_industry treat io2014.yearxtreat male education age age_sq _I* if economically_active==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the health and education sector")

	*Formal
	reg healtheduc_industry treat io2014.yearxtreat male education age age_sq _I* if formal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the FORMAL health and education sector")

	*Informal
	reg healtheduc_industry treat io2014.yearxtreat male education age age_sq _I* if informal==1, cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("Probability of being in the INFORMAL health and education sector")

*Which other industry should I look at? Education and health? To see if there is any impacts of the aid/UNHCR, etc.

********************************************************************************
********************************************************************************
********************************************************************************
************************HETEROGENEITY BY ECONOMIC SECTOR************************
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
****************TOTAL INCOME IN FOOD/CONSTRUCTION/SALES SECTOR******************
********************************************************************************

	xi i.state_num i.year i.sector

	*Formal
	reg asinh_income_main_usual treat io2014.yearxtreat male education age age_sq _I* if formal==1 & (sector==3|sector==4|sector==6), cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("FORMAL labor market income - Food/Sales/Constr") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/11.01 Income (Formal - Food Sales Constr).png", replace

	*Informal
	reg asinh_income_main_usual treat io2014.yearxtreat male education age age_sq _I* if informal==1 & (sector==3|sector==4|sector==6), cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("INFORMAL labor market income - Food/Sales/Constr") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/11.02 Income (Informal - Food Sales Constr).png", replace

**********************************HOURLY WAGES**********************************

	*Formal labor market: Hourly wages
	reg asinh_hourly treat io2014.yearxtreat male education age age_sq _I* if formal==1 & (sector==3|sector==4|sector==6), cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("FORMAL market hourly income - Food/Sales/Constr") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/12.01 Hourly income (Formal - Food Sales Constr).png", replace

	*Informal labor market: Hourly wages
	reg asinh_hourly treat io2014.yearxtreat male education age age_sq _I* if informal==1 & (sector==3|sector==4|sector==6), cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("INFORMAL market hourly income - Food/Sales/Constr") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/12.02 Hourly income (Informal - Food Sales Constr).png", replace

**********************************HOURS WORKED**********************************

	*Formal labor market: Hours worked
	reg main_usual_hours treat io2014.yearxtreat male education age age_sq _I* if formal==1 & (sector==3|sector==4|sector==6), cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("FORMAL market hours worked - Food/Sales/Constr") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/13.01 Hours worked (Formal - Food Sales Constr).png", replace

	*Informal labor market: Hours worked
	reg main_usual_hours treat io2014.yearxtreat male education age age_sq _I* if informal==1 & (sector==3|sector==4|sector==6), cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("INFORMAL market hours worked - Food/Sales/Constr") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/13.02 Hours worked (Informal - Food Sales Constr).png", replace

********************************************************************************
*************************TOTAL INCOME IN OTHER SECTORS**************************
********************************************************************************

	*Formal
	reg asinh_income_main_usual treat io2014.yearxtreat male education age age_sq _I* if formal==1 & !(sector==3|sector==4|sector==6), cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("FORMAL labor market income - Other Sector") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/14.01 Income (Formal - Other Sector).png", replace
	
	*Informal
	reg asinh_income_main_usual treat io2014.yearxtreat male education age age_sq _I* if informal==1 & !(sector==3|sector==4|sector==6), cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("INFORMAL labor market income - Other Sector") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/14.02 Income (Informal - Other Sector).png", replace

**********************************HOURLY WAGES**********************************

	*Formal labor market: Hourly wages
	reg asinh_hourly treat io2014.yearxtreat male education age age_sq _I* if formal==1 & !(sector==3|sector==4|sector==6), cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("FORMAL market hourly income - Other Sector") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/15.01 Hourly income (Formal - Other Sector).png", replace

	*Informal labor market: Hourly wages
	reg asinh_hourly treat io2014.yearxtreat male education age age_sq _I* if informal==1 & !(sector==3|sector==4|sector==6), cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("INFORMAL market hourly income - Other Sector") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/15.02 Hourly income (Informal - Other Sector).png", replace

**********************************HOURS WORKED**********************************

	*Formal labor market: Hours worked
	reg main_usual_hours treat io2014.yearxtreat male education age age_sq _I* if formal==1 & !(sector==3|sector==4|sector==6), cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("FORMAL market hours worked - Other Sector") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/16.01 Hours worked (Formal - Other Sector).png", replace

	*Informal labor market: Hours worked
	reg main_usual_hours treat io2014.yearxtreat male education age age_sq _I* if informal==1 & !(sector==3|sector==4|sector==6), cluster(state)
	coefplot, drop(treat _I* male education age age_sq _cons) yline(0) vertical omitted ci(95) title("INFORMAL market hours worked - Other Sector") ///
		coeflabel(2012.yearxtreat="2012" 2013.yearxtreat="2013" 2014.yearxtreat="2014" 2015.yearxtreat="2015" 2016.yearxtreat="2016" 2017.yearxtreat="2017" ///
		2018.yearxtreat="2018" 2019.yearxtreat="2019") ciopts(recast(rcap))
	graph export "Graphs, etc/PNAD (Samyam) (Stata)/16.02 Hours worked (Informal - Other Sector).png", replace

********************************************************************************
********************************************************************************
***********TO CHECK WHETHER THE SAMPLE COMPOSITION CHANGES AFTER POST***********
***********(NECESSARY TO CHECK SINCE I HAVE A REPEATED CROSS-SECTION)***********
********************************************************************************
********************************************************************************
********************************************************************************

/*Generally, we'd want the coefficients on oour postxtreat to be close to zero
and/or statistically insignificant. I did this following Dr. Skira's suggestion*/

	reg male treat postxtreat year state_time_trend, robust
	reg education treat postxtreat year state_time_trend, robust
	reg age treat postxtreat year state_time_trend, robust

**********************************WAGE PROFILE**********************************

forval i=2/9 {
	gen y201`i'=(year==201`i')
}

*Formal

	reg log_income y2013 y2014 y2015 y2016 y2017 y2018 y2019 if state=="RO" & formal==1
	reg log_income y2013 y2014 y2015 y2016 y2017 y2018 y2019 if state=="RR" & formal==1
	reg log_income y2013 y2014 y2015 y2016 y2017 y2018 y2019 if state=="AC" & formal==1
	reg log_income y2013 y2014 y2015 y2016 y2017 y2018 y2019 if state=="AP" & formal==1

*Informal

	reg log_income y2013 y2014 y2015 y2016 y2017 y2018 y2019 if state=="RO" & informal==1
	reg log_income y2013 y2014 y2015 y2016 y2017 y2018 y2019 if state=="RR" & informal==1
	reg log_income y2013 y2014 y2015 y2016 y2017 y2018 y2019 if state=="AC" & informal==1
	reg log_income y2013 y2014 y2015 y2016 y2017 y2018 y2019 if state=="AP" & informal==1

*Wage profile graph for formal labor market

	import excel using "Analysis and Cleaning/wageprofiles_formal.xlsx", firstrow clear
	lab var roraima "Roraima"
	lab var rondonia "Rondonia"
	lab var acre "Acre"
	lab var amapa "Amapa"
	twoway line roraima year, xline(2014) title("Wage Profile - Formal Labor Market") xtitle("Year") ytitle("Log(Wage)") ///
		|| line rondonia year || line acre year || line amapa year

*Wage profile graph for informal labor market

	import excel using "Analysis and Cleaning/wageprofiles_informal.xlsx", firstrow clear
	lab var roraima "Roraima"
	lab var rondonia "Rondonia"
	lab var acre "Acre"
	lab var amapa "Amapa"
	twoway line roraima year, xline(2014) title("Wage Profile - Informal Labor Market") xtitle("Year") ytitle("Log(Wage)") ///
		|| line rondonia year || line acre year || line amapa year
