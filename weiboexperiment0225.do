
**********************************************************************
/*  																 *			
*	Weibo Data Analysis                      						 *
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
use weiboexperiment0225.dta
**********************************************************************




* Data Cleaning
**********************************************************************
*generate group variables
gen group=1
replace group=2 if Series=="雪"
replace group=3 if Series=="茶"
replace group=4 if Series=="萃"

*undisclosed human group
gen group1=(Series=="墨") 
*undisclosed humangpt group
gen group2=(Series=="雪")
*undisclosed humangpt group
gen group3=(Series=="茶") 
*disclosed groups
gen group4=(Series=="萃") 

*differeniate two disclosed groups
gen label_length=length(string(labelaccount, "%12.0f"))
gen label_humangpt=(label_length>4)
gen group4humangpt=group4*label_humangpt
gen group4gpt=group4-group4humangpt
replace group=5 if group4humangpt==1
replace group=6 if group4gpt==1

*figure_num
replace figure_num=0 if figure_num==.

*label variables
label define group_labels 1 "Human" 2 "Human-ChatGPT" 3 "ChatGPT" 5 "Disclose Human-ChatGPT" 6 "Disclose ChatGPT"
label values group group_labels

label var group2 HumanChatGPT
label var group3 ChatGPT

label define figure 0 "nofigure" 1"withfigure"
label values figure figure

*standardize writers' demographic and socioeconomic variables
*equal or above bachelor
gen highedu=(educ>4)
replace highedu=. if educ==.

*equal or above 10,000RMB
gen highincome=(income>3&income!=.)
replace highincome=. if income==.

*male
replace male=0 if male==2

*married=1, unmarried=0
replace marriage=0 if marriage==1
replace marriage=1 if marriage==2 

*agricultural hukou=1, non-agricultural hukou=0
replace hukou=0 if hukou==2

*self-report having marketing experience equal or above 6 months
gen mktexper0=(mktexper>2&mktexper!=.) 
replace mktexper0=. if mktexper==.

* post screening
* drop unsuccessful advertisig campaign
keep if cost==200
* incomplete bg survey of the writer
drop if writerid==48095 
* system error
drop if writerid==17494 
drop if (group==5|group==6)&Day==624&Hour==10 

* generate date of week variable
gen month = substr(string(Day), 1, 1)
destring month, replace
gen dayy = substr(string(Day), 2, 2)
destring dayy, replace
gen Date = mdy(month, dayy, 2023)
gen Weekday = dow(Date) 

*check response and its sub-index
count if response!=clickpurchaselink+social_response+read_response //0
count if social_response!=retweet+comment+like+favorite+clickprofilephoto+clicknickname+clickat+clicktag+follow+playvideo //0
count if read_response!=clickfigure+clickbodytext //0
count if clickat==0
count if clicktag==0
count if playvideo==0

* categorize response into click and interaction
gen click=clickpurchaselink+clickprofilephoto+clicknickname+clickfigure+clickbodytext
gen interaction=response-click

* Data Preparation for Regression
gen groupp=group
replace groupp=2 if group==5
replace groupp=3 if group==6

gen labell=0
replace labell=1 if group==5|group==6

gen groupp2=(groupp==2)
gen groupp3=(groupp==3)

gen groupp2labell=groupp2*labell
gen groupp3labell=groupp3*labell

label var groupp2 "Human-ChatGPT"
label var groupp3 "ChatGPT"
label var labell "Disclose"
label var groupp2labell "Disclose*Human-ChatGPT"
label var groupp3labell "Disclose*ChatGPT"

**********************************************************************



*Figure 1
**********************************************************************
preserve
collapse (mean) mrate=response_rate (sem) sdrate=response_rate, by(group)
replace group=4 if group==5
replace group=5 if group==6
gen upper=mrate+1.96*sdrate
gen lower=mrate-1.96*sdrate
gen mratep = string(mrate*100, "%9.4f") + "%"
twoway (scatter mrate group,  mlabel(mratep) mlabp(4) mlabsize(vsmall)) (rcap upper lower group), graphregion(color(white)) xla(, labsize(small)) title(" ") yscale(range(0.0020 0.0042)) ylabel(0.0020 "0.20%" 0.0030 "0.30%" 0.0040 "0.40%") ytitle("Response Rate") xscale(range(1 5.2)) xtitle("") xlabel(1 "Human" 2 "Human-ChatGPT" 3 "ChatGPT" 4 "Disclosed Human-ChatGPT" 5 "Disclosed ChatGPT",labsize(*0.85)) legend(off)
graph export results\Figure1.png,replace
restore
**********************************************************************




* Table1
**********************************************************************
reg response groupp2 groupp3 impression figure i.Weekday i.Hour i.topicid i.sendorder if labell==0,r
test groupp2=groupp3
local p_value23 = r(p)
est store Weibo
outreg2 [Weibo] using results\Table1.xls, replace keep(groupp2 groupp3 impression figure) addstat(p_value(humangpt=gpt), `p_value23') addtext(Day of Week FE, Yes, Hour FE, Yes, Topic FE, Yes, Order FE, Yes, Base Group, Unlabeled Human) dec(3) ctitle("Undisclosed") label

reg response groupp2 impression figure i.Weekday i.Hour i.topicid i.sendorder if labell==1,r
est store AllSample
outreg2 [AllSample] using results\Table1.xls, append keep(groupp2 impression figure) addtext(Day of Week FE, Yes, Hour FE, Yes, Topic FE, Yes, Order FE, Yes, Base Group, Labeled ChatGPT) dec(3) ctitle("Disclosed") label

preserve
*delete all unmatched copies in undisclosed and disclosed Human-GPT and GPT Groups
drop if group==1
drop if Day>628
drop if group==3&Day==624&Hour==10
drop if group==3&Day==617&Hour==16
drop if group==3&Day==613&Hour==14
drop if group==6&Day==615&Hour==15

reg response groupp2 labell groupp2labell impression figure i.Weekday i.Hour i.topicid i.sendorder if (group==3&labelaccount==puregpt)|(group==2&labelaccount==humangpt)|group==5|group==6, r
est sto Disclosure
outreg2 [Disclosure] using results\Table1.xls, append keep(groupp2 labell groupp2labell impression figure) addtext(Day of Week FE, Yes, Hour FE, Yes, Topic FE, Yes, Order FE, Yes, Base Group, Unlabeled GPT) dec(3) ctitle("Disclosure Effect") label
restore
**********************************************************************



