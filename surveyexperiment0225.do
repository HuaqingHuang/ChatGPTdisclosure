
**********************************************************************
/*  																 *			
*	Survey Data Analysis                      						 *
*	by Huaqing Huang						        				 *
* 	Lastly Update: 2024/02/25						         		 *
*	                             									 *
*																	 *
*/																	 *					
**********************************************************************




* Import Data                                             
**********************************************************************
clear
cd "D:\\2.2\ChatGPT\GPT_Data and Code"
use surveyexperiment0225.dta
**********************************************************************




* Data Cleaning
**********************************************************************
* standardize writers' demographic and socioeconomic variables
*male
replace male=0 if male==2
label var male "male"

*age
gen age=2023-year
gen age30=(age<=30)

*equal or above bachelor
gen higheduc=(educ>4) 

**agricultural hukou=1, non-agricultural hukou=0
replace hukou=0 if hukou==2 

*equal or above 10,000RMB
gen highincome=(income>6) 

*married=1, unmarried=0
replace marriage=(marriage==2)

*employed=1, other=0
gen workstatus0=(workstatus==1)

*rename variables
rename ChatGPT usegpt
label variable usegpt "prior ChatGPT experience"

*generate variables
gen group1=(group==1)
gen group2=(group==2)
gen group3=(group==3)
label var group1 "Human"
label var group2 "Human-ChatGPT"
label var group3 "ChatGPT"

* variable preparation for regression
gen group1label=group1*label
gen group2label=group2*label
gen group3label=group3*label

label var group1label "Disclose*Human"
label var group2label "Disclose*Human-ChatGPT"
label var group3label "Disclose*ChatGPT"
label var label "Disclose"

**********************************************************************




*Figure2
**********************************************************************
preserve
collapse (mean) mavgvalue=avgvalue (sem) sdavgvalue=avgvalue, by(group label)
gen upper=mavgvalue+1.96*sdavgvalue
gen lower=mavgvalue-1.96*sdavgvalue
egen group_label = group(label group)
twoway (scatter mavgvalue group_label,  mlabel(mavgvalue) mlabp(4) mlabsize(vsmall) mlabf(%9.2f)) (rcap upper lower group_label), graphregion(color(white)) xla(, labsize(small)) title(" ") ytitle("Average Score") xscale(range(1 6.2)) xtitle("") xlabel(1 "Human" 2 "Human-ChatGPT" 3 "ChatGPT" 4 "Disclosed Human" 5 "Disclosed Human-ChatGPT" 6 "Disclosed ChatGPT",labsize(*0.85) angle(10)) legend(off)
graph export results\Figure2.png,replace
restore
**********************************************************************




*Table2
**********************************************************************
reg avgvalue group2 group3 i.topicid i.order i.respondent_id if label==0, cluster(respondent_id)
test group2=group3
local p_value23 = r(p)
est sto Undisclosed
outreg2 [Undisclosed] using results\Table2.xls, replace keep(group2 group3) addstat(p_value(HumanGPT=GPT), `p_value23') addtext(Topic FE, Yes, Order FE, Yes, Respondent FE, Yes) dec(3) ctitle("Undisclosed") label

reg avgvalue group2 group3 i.topicid i.order i.respondent_id if label==1, cluster(respondent_id)
test group2=group3
local p_value23 = r(p)
est sto Disclosed
outreg2 [Disclosed] using results\Table2.xls, append keep(group2 group3) addstat(p_value(HumanGPT=GPT), `p_value23')  addtext(Topic FE, Yes, Order FE, Yes, Respondent FE, Yes) dec(3) ctitle("Disclosed") label

reg avgvalue label group2label group3label group2 group3 male age highedu workstatus0 highincome marriage usegpt time i.topicid i.order, cluster(respondent_id)
test group2label=group3label
local p_value23=r(p)
est sto avg0
outreg2 [avg0] using results\Table2.xls, append keep(label group2label group3label group2 group3) addstat(p_value(Disclose*HumanGPT=Disclose*GPT), `p_value23') addtext(Topic FE, Yes, Order FE, Yes, Respondent Control, Yes) dec(3) ctitle("Disclosure Effect") label

reg avgvalue group2label group3label group2 group3 i.topicid i.order i.respondent_id, cluster(respondent_id)
test group2label=group3label
local p_value23=r(p)
est sto avg1
outreg2 [avg1] using results\Table2.xls, append keep(group2label group3label group2 group3) sortvar(group2 group3 group2label group3label) addstat(p_value(Disclose*HumanGPT=Disclose*GPT), `p_value23') addtext(Topic FE, Yes, Order FE, Yes, Respondent FE, Yes) dec(3) ctitle("Disclosure Effect") label
**********************************************************************

