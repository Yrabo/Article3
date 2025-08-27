*=====================================================================================
* Replication code for:
* Mobile money services and gender inequality in developing countries

* *
* Dabou Yrabo
* Université Clermont Auvergne-CERDI/CNRS (Clermont-Ferrand, France)
* E-mail: dabouyrabo5@gmail.com

*==================================================================================

* Data downloaded in Mai 2025
////////////////////////////////////////////////////////////////////////////////
//Clean and set memory
clear all 
cls
set more off, permanently

//Set the Working Directory:
cd "~/OneDrive/Mobile money and gender inequality/Data"

//Create a folder for Results outputs
capture mkdir Results
capture confirm file "../Results/nul" //check if "Results" sub-directory exists
if _rc { //_rc will be >0 if it doesn't exist
**# Bookmark #1
 !md "Results"
 }

//Saving the window's results
//capture log close
//log using "./Results/myresults.txt", text replace


////////////////////////////////////////////////////////////////////////////////
********************************************************************************
//Load data

// GSMA dataset : mobile money services
use mobile_money_panel.dta, clear
keep countrycode year stageofdevIMF incomelevel adopt_year mms_country mms_adopt has_P2Gdom has_Billpay has_P2G has_G2P has_Otherpay has_Airtimetopup has_Merchpay has_Inremitt has_Inremitt_send has_Inremitt_receiv has_Cashin has_Cashout cum_providers
replace adopt_year=0 if adopt_year==.
sort countrycode year, stable
save interim.dta, replace


//Merging with Main DataBase: WDI Data
* Women Political Empowerment= Proportion of seats held by women in national parliaments (%)
use WDIdataset.dta, clear
keep countrycode year countryname regionname incomelevel incomelevelname voicacc contcor goveff polistab ///
 rulelaw reguqual popt lfprtfe popt1564 txpoprural deprat density hci fhci taxy govconhy conhy conhca ///
 natresy remitxy revadm_eff elecaccs ict2 ict1 ict3 creditfy fdiy aid gdpcod gdpcodc gfcfcd ///
 gfcfpr ginindex agrvay healthexpy lifexp gequal_eff servdebt educprtx tradey infla cpi creditby  ///
 wom_parly wom_law credity bmoney srvvay indvay umrat umratfe urbpop litracyfe litracyma litracy schoolsecfe schoolprmfe frtrate mortrate empself_fe
 
*Merge
sort countrycode year, stable
merge 1:1 countrycode year using interim.dta, sorted keep(match master) 
drop _merge 
erase interim.dta
order countryname countrycode year, first
save DataBase.dta, replace

//////////////////////////////////////////////////
// Financial Access Survey (FAS): mobile money acocunt
*ssc install sdmxuse
/*
sdmxuse dataflow IMF,clear
sdmxuse datastructure IMF, dataset(FAS) clear
sdmxuse data IMF, ///
    clear dataset(FAS) ///
    dimensions(.FCAK_NUM .A) ///
    start(2020) ///
    end(2023) ///
	 mergedsd */

use imf_fas.dta,clear
local vars mm_tx_pogdp mm_act mm_reg_acc mm_reg_ag mm_tx_num ///
           mm_bal_pogdp mm_act_ag mm_tx_xdc mm_bal_xdc cb_tx_val cb_tx_num
foreach v of local vars {
    destring `v', replace force
	*replace `v'=0 if (`v'==. & mms_adopt==0)
}

keep countryname countrycode year mm_tx_pogdp mm_act mm_reg_acc mm_reg_ag mm_tx_num mm_bal_pogdp mm_act_ag 
sort countrycode year, stable
save interim.dta, replace

* Merge 
use DataBase.dta, clear
sort countrycode year, stable
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
order countryname countrycode year, first
local vars mm_tx_pogdp mm_act mm_reg_acc mm_reg_ag mm_tx_num mm_bal_pogdp mm_act_ag 
foreach v of local vars {
	replace `v'=0 if (`v'==. & mms_adopt==0)
}
save DataBase.dta, replace

///////////////////////////////////////
// Global findex database : mobile money
use DatabankWide.dta, clear
rename countrynewwb countryname
rename codewb countrycode

global vars account_t_d account_t_d_1 account_t_d_2 ///
mobileaccount_t_d mobileaccount_t_d_1 mobileaccount_t_d_2 ///
g20_t_d g20_t_d_1 g20_t_d_2 Own_phone fin17a_17a1_d_1 fin17a_17a1_d_2 save_any_1 fin28_t_d_1 fin28_t_d_9
keep countryname countrycode regionwb21_hi year $vars

gen mobileaccount_t_d_g = mobileaccount_t_d_2-mobileaccount_t_d_1
label var mobileaccount_t_d  "Mobile money account (% age 15+)"
label var mobileaccount_t_d_1 "Mobile money account, female(% age 15+)"
label var mobileaccount_t_d_2 "Mobile money account, male(% age 15+)"
label var mobileaccount_t_d_g "Mobile money account, gap(% age 15+)"
label var save_any_1 "Save money,female"
label var fin28_t_d_1 "Received domestic remittances, female (% age 15+)"
label var fin28_t_d_9 "Received domestic remittances, rural (% age 15+)"


gen g20_t_d_g = g20_t_d_2 - g20_t_d_1
label var Own_phone "Own a mobile phone (% age 15+))" 
label var  g20_t_d_1 "Made or received a digital payment, female (% age 15+)" 
label var  g20_t_d_2 "Made or received a digital payment, male (% age 15+)" 
label var  g20_t_d_g "Made or received a digital payment, gap (% age 15+)" 
sort countrycode year, stable
save interim.dta, replace

use DataBase.dta, clear
sort countrycode year, stable
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta

foreach var of varlist $vars  {
	 replace `var' = `var'*100
}

order countryname countrycode year, first
save DataBase.dta, replace


**# Bookmark #7
////////////////////////////////////////////////////////////////////////////////
//United Nation Development Program (UNDP) Database
use undp.dta, clear 
label var gii "Gender inequality index"
/*
foreach var of varlist hdi hdife  mys mysfe gii gnipc gdi  {
	 replace `var' = `var'*100
}*/

keep countrycode year hdi hdife gii gdi mysfe 
gen GapGDI = 100 * (gdi - 1)
label var GapGDI "Gender Parity Gap (percentage points)"
sort countrycode year, stable
save interim.dta, replace


 *//Merging UNDP data with Main Database
use DataBase.dta, clear
sort countrycode year, stable
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
order countryname countrycode year, first
save DataBase.dta, replace


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//Global Data Lab Area DataSet
use "GlobalDataLab.dta", clear
* Mean years schooling of population 25+: school
* Mean years schooling of population 25+, female: schoolfe
* Mean years schooling of population 25+, male: schoolma
order countryname countrycode year, first
keep countryname countrycode year school schoolfe schoolma
sort countrycode year, stable
save interim.dta, replace

use DataBase.dta, clear
sort countrycode year, stable
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
save DataBase.dta, replace

/////////////////////////////////////////////////////////////////////////////
// International Télecommunication union DATA(ITU dataset)

use final_ITU_data,clear
sort countrycode year, stable
save interim.dta, replace

use DataBase.dta, clear
sort countrycode year, stable
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
save DataBase.dta, replace


/////////////////////////////////////////////////////////////////////////////////
// V-Dem Democracy Indices database
use base_v_dem,clear
sort countrycode year, stable
save interim.dta, replace

use DataBase.dta, clear
sort countrycode year, stable
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
save DataBase.dta, replace

//////////////////////////////////////////////////////////////////////////////
// Our World in Data
// Data sources: Barro and Lee (2015)Lee and Lee (2016)– with major processing by Our World in Data
use msy_owd,clear 
sort countrycode year, stable
save interim.dta, replace
use DataBase.dta, clear
sort countrycode year, stable
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
drop entity
save DataBase.dta, replace

* Data litracy 
use literacy_owd,clear 
sort countrycode year, stable
save interim.dta, replace
use DataBase.dta, clear
sort countrycode year, stable
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
save DataBase.dta, replace


////////////////////////////////////////////////////
// CONFLICT data 
/////////////////////////////////////////////////////////////////////////////
* Conflits data 

import excel using "conflict", clear first sheet("Tabelle1") 
rename ISO countrycode
rename country countryname
sort countrycode year, stable 
save interim.dta, replace

use DataBase.dta, clear
sort countrycode year, stable 
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
replace conflict=0 if conflict==.
replace deaths=0 if deaths==.
save DataBase.dta, replace


//////////////////////////////////////////////////////////////////////////////////
// IMF financial inclusion dataset (Financial Access Survey (FAS))
* Access to ATMs =Number of ATMs per 100,000 adults
* Access to Bank Branches= Number of branches per 100,000 adults
* Access to loans Outstanding loans from commercial banks (% of GDP)
import delimited "imf_FI_data.txt",clear
rename economyiso3 countrycode
rename economyname countryname
rename geographicaloutreachnumberofauto ATM
rename keyindicatorsgeographicaloutreac ATM_100000A
rename v6 ATM_1000km2
rename v7 TFI_bank
rename outstandingloansfromcommercialba TFi_loans

sort countrycode year, stable
keep countrycode countryname year ATM ATM_100000A ATM_1000km2 TFI_bank TFi_loans
local vars ATM ATM_100000A ATM_1000km2 TFI_bank TFi_loans
foreach var of local vars {
    capture confirm numeric variable `var'
    if !_rc {
        display "`var' is already numeric."
    }
    else {
        destring `var', force replace
    }
}

label var TFI_bank "Bank branches/100,000 adults"
label var ATM_100000A "Number ATM/100,000 adults"
label var ATM "ATM"
label var ATM_1000km2 "Number ATM/1000km2"
label var TFi_loans "Commercial banks to household sector(%)"
save interim.dta, replace

use DataBase.dta, clear
sort countrycode year, stable
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
order countryname countrycode year, first
save DataBase.dta, replace

//  Financial development index of IMF data
use financial_dvt.dta, clear
rename isocode countrycode
rename country countryname
keep countrycode year FD
rename FD fdi_fmi
label var fdi_fmi "Financial development index"
sort countrycode year, stable
save interim.dta, replace

*Merge 
use DataBase.dta, clear
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
order countrycode year, first
save DataBase.dta, replace

// IMF Public investment data
use public_infr_dvt,clear
rename isocode countrycode
sort countrycode year, stable
save interim.dta, replace
*Merge 
use DataBase.dta, clear
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
order countrycode year, first
drop country
save DataBase.dta, replace


///////////////////////////////////////////////////////////////////////////////
// Informal data 
import excel "informal-economy-database.xlsx", firstrow sheet("MIMIC_p")  clear
local colnames "C D E F G H I J K L M N O P Q R S T U V W X Y Z AA AB AC AD AE AF AG"
local start = 1990
local i = 0
foreach var in `colnames' {
    local yr = `=`start' + `i''
    rename `var' y`yr'
    local ++i
}

reshape long y, i(Economy Code) j(year)
gen informy =100*y
label var informy "Informal sector (% of GDP)"
rename Code countrycode
sort countrycode year, stable
save interim.dta, replace

use DataBase.dta, clear
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
order countrycode year, first
save DataBase.dta, replace

// ////////////////////////////////////////////////////////////////////////////////
* The International Country Risk Guide (ICRG) DATA 
********************************************************************************
use ICRG.dta, clear
rename isocode countrycode
rename Country countryname
sort countrycode year, stable
save interim.dta, replace


*Merge 
use DataBase.dta, clear
merge 1:1 countrycode year using interim.dta, sorted keep(match master)
drop _merge
erase interim.dta
order countrycode year, first
save DataBase.dta, replace


// ////////////////////////////////////////////////////////////////////////////////
********************************************************************************
//Load data
use DataBase.dta, clear
order countryname countrycode  incomelevel incomelevelname year stageofdevIMF regionname ///
hdi gii gdi, first


//Converting string codes into numeric codes for indivuduals in the panel
//Numerical codes for individuals are needed in panel set-up in Stata 
encode countrycode, generate(code)
sort code year, stable
order countryname countrycode code year, first

//Panel data specification:
xtset code year, yearly


// Gen variable 

gen lGDPC = 100*log(gdpcodc) 
gen GDPCG = 100*(gdpcodc/L.gdpcodc - 1)
gen GDPG = 100*(gdpcod/L.gdpcod - 1)
gen CPIG = 100*(cpi/L.cpi - 1)
gen LATM =100*log(ATM+1)
gen LATM_100000A =100*log(ATM_100000A+1)
gen LATM_1000km2=100*log(ATM_1000km2+1)
*gen LTFI_bank=100*log(TFI_bank)
gen lgfcfcd=100*log(gfcfcd)
gen Log_density=log(density)*100
gen Lict1=(ict1/100000)
gen Log_mm_act=log(mm_act+1)
gen log_mm_tx_num=log(mm_tx_num+1)


label var lGDPC "100*(Log of real GDP per capita)"
label var GDPCG "Growth rate of real GDP per capita (%)"
label var GDPG "Growth rate of real GDP (%)"
label var CPIG "Inflation rate based on CPI (%)"
label var LATM_100000A "100*(log Number of ATMs per 100,000 adults)"
label var LATM_1000km2 "100*(log Number of Automated Teller Machines (ATMs))"
label var LATM "100*(log Number of ATMs per 1,000 km2)"
label var lgfcfcd "100*(Log Gross fixed capital formation)"

*Bank branches/100000 adults

*Lag des variables
gen Lag_gii=L.gii
gen Lag_gdi=L.gdi
gen Lag_wom_law=L.wom_law
gen Lag_GDPCG=L.GDPCG
gen Lag_lGDPC=L.lGDPC
gen Lag_fdiy =L.fdiy 
gen Lag_schoolfe=L.schoolfe
gen Lag_CPIG =L.CPIG 
gen Lag_elecaccs=L.elecaccs
gen Lag_goveff =L.goveff
gen Lag_polistab=L.polistab
gen Lag_LATM_100000A=L.LATM_100000A
gen Lag_LATM_1000km2=L.LATM_1000km2
gen Lag_LTFI_bank=L.TFI_bank	
gen Lag_sociorisk=L.sociorisk
gen Lag_agrvay=L.agrvay
gen Lag_rulelaw=L.rulelaw
gen Lag_reguqual=L.reguqual 
gen Lag_ict3 =L.ict3 
gen Lag_ict1 =L.Lict1
gen Lag_ict2 =L.ict2
gen Lag_fdi_fmi=L.fdi_fmi
gen Lag_urbpop=L.urbpop
gen Lag_lfprtfe=L.lfprtfe
gen Lag_tradey =L.tradey 
gen Lag_density=L.Log_density
gen Lag_txpoprural=L.txpoprural
gen Lag_creditby=L.creditby
gen Lag_credity=L.credity


* Initial gender equality
gen pre_adopt = year == adopt_year - 1
by code: gen gii_pre  = gii if pre_adopt
by code: egen gii_init = mean(gii_pre)
by code: gen gdi_pre  = gdi if pre_adopt
by code: egen gdi_init = mean(gdi_pre)


/*gen pre_adopt = year == 1990
by code: gen GapGDI_pre  = GapGDI if pre_adopt
by code: egen GapGDI_init = mean(GapGDI_pre)

order code year pre_adopt gdi GapGDI GapGDI_pre GapGDI_init*/

// Label 
*label var gdi_init      "Initial level GDI"
*label var gii_init      "Initial level GII"
label var Lag_lGDPC     "Lag[log Real GDP per capita]"
label var Lag_lfprtfe   "Lag[Female labor force]"
label var Lag_wom_law    "Lag[Women Business and Law Index]"
label var Lag_txpoprural "Lag[Rural population growth]"
label var Lag_urbpop     "Lag[Urban population growth]"
label var Lag_LTFI_bank  "Lag[bank branches/100,000 adults"
label var Lag_ict1       "Lag[Fixed telephone]"
label var Lag_creditby   "Lag[Financial depening]"
 label var ICTPricesMBGNI  "Mobile Broadband Cost (% GNI/hab)" 
 label var Log_mm_act        "Log Active Mobile Money Accounts"
 label var network_covered   "Mobile Network Coverage (%)" 
       
    
//SAMPLE RESTRICTIONS
keep if (stageofdevIMF=="Developing")
*keep if (stageofdevIMF=="Developed")
*keep if (regionname=="Sub-Saharan Africa"|regionname=="Middle East and North Africa")
*keep if year>=2000 & year<=2023
keep if year>=1990 & year<=2023
encode regionname,  gen(region_id)

* Drop countries without gender equality observations 
bys countrycode: egen nonmiss_gdi = total(!missing(gdi) & inrange(year,1990,2023))
drop if nonmiss_gdi <= 0
drop nonmiss_gdi


* Initial gender inequality ( 1990-2001)
preserve
    keep if inrange(year,1990,2001)
    bysort code: egen gdi_initl = mean(GapGDI)
	bysort code: egen gii_initl = mean(gii)
    keep code gdi_initl gii_initl
    duplicates drop            // ne garder qu'une ligne par iso
    save "init_gdi_90_01.dta", replace
restore
merge m:1 code using "init_gdi_90_01.dta"
save mmsgender_data,replace

//Panel data specification base on t:
use mmsgender_data,clear
sort code year, stable
*by code: gen t = _n
xtset code year,


/*** Dummy year variables
forvalues t = 1990/2023 {
    gen byte yr_`t' = (year == `t')
}
*/


//==========================================================================

    *// Entropy balancing: Generating weights

//============================================================================

* Covariables 

global controls_vars  gdi_init Lag_lGDPC Lag_lfprtfe Lag_wom_law Lag_urbpop Lag_creditby Lag_ict1
global controls_vars1 gii_init Lag_lGDPC Lag_lfprtfe Lag_wom_law Lag_urbpop Lag_creditby Lag_ict1

*drop weight*
ebalance mms_adopt  $controls_vars i.year, gen(weight11) tolerance(0.52)   
ebalance mms_adopt  $controls_vars1 i.year, gen(weight12) tolerance(0.52)  

save data_final, replace


 //===========================================================================
  * Table 1: Summary statistics of main variables
// ==============================================================================


// SUMMARY STATISTICS
estimates clear
estpost summarize mms_adopt GapGDI gdi gii $controls_vars , detail
estout using "./Results/SumStats.xls", ///
    cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) p50(fmt(2)) p75(fmt(2))") ///
    title("Summary statistics of main variables") ///
    prehead(@title) ///
    collabels(, lhs("Variable")) mlabels(none) label varlabels() ///
    note("Source: Auteurs") ///
    postfoot(@note) ///
    replace

order code year gdi gii GapGDI


// CORRELATION COVARIABLES 
eststo clear
corr GapGDI $controls_vars 	   
mat CorrMat = r(C)
//Filling upper triangle of a symmetric matrix by missing values
forvalues j=2/`=colsof(CorrMat)' {
forvalues i=1/`=rowsof(CorrMat)' {
          if `i'<`j' {
             qui mat CorrMat[`i',`j']=.a
          }
}
}
//Excel Table using -estout-
estout matrix(CorrMat, fmt(%6.2f))                                              ///
using "./Results/CorrMat.xls",                                                  ///
title("Correlation matrix of main variables")                                   ///
prehead(@title)                                                                 ///
collabels(,lhs("Variable")) mlabels(none) label varlabels()                     ///
note("Source: Authors' computations.")                                          ///
postfoot(@note)                                                                 ///
replace

// Number of countries using in the regression // 132
*drop in_fe tag_fe 
reg GapGDI gdi_initl Lag_lGDPC Lag_lfprtfe Lag_wom_law Lag_urbpop Lag_creditby mms_adopt i.code i.year
gen byte in_fe = e(sample)
egen byte tag_fe = tag(code) if in_fe
count if tag_fe==1


// STYLIZED FAITS

* //. Graphs: Evolution of adoption & providers (1990–2024)
*-------------------------------------------------------------------------------
* Fig1: Number of countries adopting per year and numbers of mm providers
preserve
collapse (sum) mms_adopt cum_providers, by(year)
rename mms_adopt     n_countries
rename cum_providers n_providers

twoway ///
    (bar n_countries year, barwidth(0.8)) ///
    (scatter n_providers year, connect(l) ///
        msymbol(circle) msize(small) yaxis(2)), ///
    xlabel(, angle(45) labsize(vsmall)) ///
    ytitle("Number of adopting countries") ///
    ytitle("Cumulative providers", axis(2)) ///
    legend(order(1 "Adopting countries" 2 "Providers") ///
           pos(11) ring(2)) ///
    scheme(s1color) ///
    note("Source: Author with GSMA data")

graph export "../Figures/mobile_money_combined.png", as(png) replace 
restore


*Fig2. Gender inequality evolution
//	-------------------------------------------------------

preserve
collapse (mean) gii gdi, by(year)
twoway ///
    (line gii year, sort lcolor(blue)  lwidth(medium) msymbol(circle) msize(small) yaxis(1)) /// GII → axis 1
    (line gdi year, sort lcolor(red)   lwidth(medium) msymbol(square) msize(small) yaxis(2)), /// ← virgule ici !
    xlabel(, angle(45) labsize(vsmall)) ///
    ytitle("Gender Inequality Index (GII)",      axis(1)) ///
    ytitle("Gender Development Index (GDI)",     axis(2)) ///
    legend(order(1 "GII" 2 "GDI") pos(11) ring(2)) ///
    scheme(s1color) ///
    note("Source: author's calculations with UNDP data")

graph export "../Figures/GII_GDI.png", as(png) replace
restore



*Fig3. Mobile money versus no mobile money
//	-------------------------------------------------------
graph bar (mean) gii [aweight=weight11], ///
    over(mms_adopt, gap(30) label(labsize(medium))) ///
    asyvars ///
    bar(1, color(blue%20) lcolor(blue) lwidth(medium)) ///
    bar(2, color(blue%80) lcolor(blue) lwidth(medium)) ///
    ytitle("Gender parity gap (Percentage points)", size(medium)) ///
    blabel(bar, format(%9.2f) size(small)) ///
    legend(pos(5) label(1 "No mobile money") label(2 "Mobile money")) ///
    note("Source: author's calculations with UNDP data", size(vsmall) position(6)) ///
    scheme(s2color)

graph export "../Figures/mms_adaopt12.png", as(png) replace

// Test de différence de moyenne le group traité et le group synthetic
reg GapGDI mms_adopt [aweight=weight11]
reg gii mms_adopt [aweight=weight11]

* 2. Pretty box-plot with two custom fills

graph box gdi [aweight=weight11], ///
    over(mms_adopt, gap(30) label(labsize(medium))) /// spacing & label size
    asyvars nooutsides                                          /// one box per categor
    box(1, color(blue%80) lcolor(blue) lwidth(medium)) /// box #1 fill=20% blue
    box(2, color(blue%30) lcolor(blue) lwidth(medium)) /// box #2 fill=80% blue
    ytitle("Gender development Index", size(medium)) ///
    subtitle("By mobile-money adoption status", size(small)) ///
    note("Source: author's calculations with UNDP data", size(vsmall) position(6)) ///
    legend(pos(5) label(1 "No Money money countries") label(2 "mobile money countries")) ///
    scheme(s2color)
graph export "../Figures/mms_adaopt22.png", as(png) replace


//   ==========================================================================

   // MAIN RESULTS 
//===========================================================================



  * PANEL A Table 2:  Balance of covariates before weighting
*//======================================================================================
 
 // Covariate balance: Treated versus untreated country-year observations
preserve 
*keep if complete==1
order $controls_vars 
table (command) (result), ///
command(Yes=r(mu_2) No=r(mu_1) Difference=(r(mu_2)-r(mu_1)) p_value=r(p): ttest gii_initl  , by(mms_adopt) unequal) ///
command(Yes=r(mu_2) No=r(mu_1) Difference=(r(mu_2)-r(mu_1)) p_value=r(p): ttest Lag_lGDPC, by(mms_adopt) unequal) ///
command(Yes=r(mu_2) No=r(mu_1) Difference=(r(mu_2)-r(mu_1)) p_value=r(p): ttest Lag_lfprtfe  ,by(mms_adopt) unequal) ///
command(Yes=r(mu_2) No=r(mu_1) Difference=(r(mu_2)-r(mu_1)) p_value=r(p): ttest Lag_wom_law  ,by(mms_adopt) unequal) ///
command(Yes=r(mu_2) No=r(mu_1) Difference=(r(mu_2)-r(mu_1)) p_value=r(p): ttest Lag_urbpop, by(mms_adopt) unequal) ///
command(Yes=r(mu_2) No=r(mu_1) Difference=(r(mu_2)-r(mu_1)) p_value=r(p): ttest Lag_creditby, by(mms_adopt) unequal) ///
command(Yes=r(mu_2) No=r(mu_1) Difference=(r(mu_2)-r(mu_1)) p_value=r(p): ttest Lag_ict1, by(mms_adopt) unequal) ///
nformat (%9.3f) stars(p_value 0.1 "*" 0.05 "**" 0.01 "***", shownote) name(t_test1) replace
collect set t_test1 // after running the table command
collect label levels command 1 "GDI initial" 2 "Lag[100*Log(Real GDP per capita)]" 3 "Lag[Female labor force(%)]" ///
4 "lag[Women in parliaments (%)]" 5 "Urban population growth (%)" 6 "Lag[Financial freedoom(%GDP)]" 7 "Lag[(Fixed telephone)]" ,modify
collect title "Table XX. T tests comparing union vs non union members across different metrics"
collect preview
collect style cell command[2 13]#result[Mobile-money No-Mobile-money Difference p_value stars], shading(background(lightsteelblue))
collect export "./Results/tableau_imbalance.txt", replace
restore


   ** PANEL B Table 2:  Balance of covariates after weighting
// ===================================================================================
 
 // Covariate balance: Treatment group versus synthetic control group

local vars1 "gdi_init Lag_lGDPC Lag_wom_law  Lag_lfprtfe"
local vars2 "Lag_urbpop Lag_creditby Lag_ict1 "

eststo clear
foreach var of local vars1 {
    eststo: quietly regress `var' mms_adopt [aweight=weight11]
}
esttab, star(* 0.05 ** 0.01) label varwidth(24) modelwidth(24) b(4) p(4)  t obslast stardetach keep(mms_adopt) 

eststo clear
foreach var of local vars2 {
    eststo: quietly regress `var' mms_adopt [aweight=weight11],
}
esttab, star(* 0.05 ** 0.01) label varwidth(24) modelwidth(24) b(4) p(4) t obslast stardetach keep(mms_adopt) 


* // Determinant de l'adoption du mobile money

reg gdi mms_adopt $controls_vars  i.year i.code,r
pwcorr gdi $controls_vars 

reg mms_adopt $controls_vars i.year i.code,r



* Covariate balance: Treatment group versus synthetic control group
xtset code year
gen time = year - 1990


* Average treatment effects on the treated
eststo clear
eststo: quietly regress gdi mms_adopt  $controls_vars             [aweight=weight11],cluster(code)
eststo: quietly regress gdi mms_adopt  $controls_vars i.code      [aweight=weight11],cluster(code)
eststo: quietly regress gdi mms_adopt  $controls_vars i.year      [aweight=weight11],cluster(code)
eststo: quietly regress gdi mms_adopt  $controls_vars i.region_id [aweight=weight11],cluster(code)
eststo: quietly regress gdi mms_adopt  $controls_vars i.region_id i.year [aweight=weight11],cluster(code)

eststo: quietly regress gdi mms_adopt  $controls_vars i.code i.year [aweight=weight11],cluster(code)
eststo: quietly regress gdi mms_adopt  $controls_vars i.code i.year i.time [aweight=weight11],cluster(code)


esttab, star(* 0.1 ** 0.05 *** 0.01) keep(mms_adopt)


eststo clear
eststo: quietly regress GapGDI mms_adopt  $controls_vars  [aweight=weight11],cluster(code)
eststo: quietly regress GapGDI  mms_adopt  $controls_vars i.code [aweight=weight11],cluster(code)
eststo: quietly regress GapGDI  mms_adopt  $controls_vars i.year [aweight=weight11],cluster(code)
eststo: quietly regress GapGDI  mms_adopt  $controls_vars i.region_id [aweight=weight11],cluster(code)
eststo: quietly regress GapGDI  mms_adopt  $controls_vars i.region_id i.year [aweight=weight11],cluster(code)
eststo: quietly regress GapGDI  mms_adopt  $controls_vars i.region_id##i.year [aweight=weight11],cluster(code)
eststo: quietly regress GapGDI  mms_adopt  $controls_vars i.code i.year [aweight=weight11],cluster(code)
esttab, star(* 0.1 ** 0.05 *** 0.01) keep(mms_adopt)



eststo clear
eststo: quietly regress gii mms_adopt  $controls_vars1  [aweight=weight12],cluster(code)
eststo: quietly regress gii mms_adopt  $controls_vars1 i.code [aweight=weight12],cluster(code)
eststo: quietly regress gii mms_adopt  $controls_vars1 i.year [aweight=weight12],cluster(code)
eststo: quietly regress gii mms_adopt  $controls_vars1 i.region_id [aweight=weight12],cluster(code)
eststo: quietly regress gii mms_adopt  $controls_vars1 i.region_id i.year [aweight=weight12],cluster(code)
eststo: quietly regress gii mms_adopt  $controls_vars1 i.code i.year [aweight=weight12],cluster(code)
esttab, star(* 0.1 ** 0.05 *** 0.01) keep(mms_adopt)



*// ==================================================

                // ROBUSTESSE 

				
				
				
//==========================================================
// 1- Robustesse to additionnal variables

/*
Variables économiques

    Croissance du PIB, inflation, chômage, ouverture aux services : elles capturent le contexte macro qui peut influencer à la fois l'adoption du mobile money (plus de richesse, de stabilité ou d'ouverture facilite l'usage) et l'égalité de genre (des économies en croissance créent souvent plus d'opportunités pour les femmes).

    Dépenses publiques sociales : un État providence fort peut réduire les inégalités sans passer par le mobile money.

Conditions politiques et institutionnelles

    Indice de démocratie, État de droit, Contrôle de la corruption, Femmes au parlement : ces variables mesurent la qualité du cadre légal et de la représentation, qui sont des déterminants clés de l'égalité des sexes, indépendamment de la finance mobile.

    Les inclure teste si votre résultat sur le mobile money tient une fois qu'on neutralise la dimension institutionnelle.

Considérations climatiques

    Exposition aux catastrophes, précipitations, températures : les chocs climatiques affectent souvent plus durement les activités informelles et la consommation des femmes.

    Si vous voulez montrer que le mobile money aide à amortir ces chocs, contrôler l'intensité des risques climatiques par pays est judicieux.

Infrastructure et enjeux structurels

    Accès électricité, routes pavées, eau potable, points Mobile Money, Internet fixe : ces variables renseignent sur la capacité matérielle d'utiliser le mobile money (ou pas).

    Vous vérifiez ainsi que ce n'est pas seulement un effet d'infrastructure "classique" qui fait bouger votre GDI, mais bien la finance mobil

	
	Créer un 5ᵉ groupe "Démographique/Social", aux côtés de vos groupes existants, et y placer :

    Taux de fécondité total (Fertility rate)

    Espérance de vie

    Structure par âge (part de la population jeune/fertile)

*/



* Définir les groupes
est clear
global global_controls gdi_init Lag_lGDPC Lag_lfprtfe Lag_wom_law Lag_urbpop Lag_creditby Lag_ict1
global eco_vars infla tradey igov_rppp_gdp agrvay remitxy aid fdiy fdi_fmi informy // Macroéconomie & flux
global inst_vars intconflict extconflict religrisk corruption govstab laworder // 	Institutions générales & risques politiques
global inst_vars2 v2x_gencl v2clprptyw                     // + Institutions genre
global demog schoolfe frtrate mortrate                     // + structure démographique et reproductrice féminine

eststo: quietly regress GapGDI mms_adopt  $global_controls                  i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI mms_adopt $global_controls  $eco_vars        i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI mms_adopt  $global_controls $inst_vars       i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI mms_adopt  $global_controls $inst_vars2      i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI mms_adopt  $global_controls  $demog          i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI mms_adopt $global_controls $eco_vars  $inst_vars2 i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI mms_adopt $global_controls $eco_vars  $inst_vars1 i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI mms_adopt  $global_controls $inst_vars2  $demog   i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI mms_adopt $global_controls $eco_vars $inst_vars2 $demog i.code i.year [aweight=weight11],r

esttab, star(* 0.1 ** 0.05 *** 0.01) 


// // Gradual addition of control variables and country specific trend 
eststo clear
eststo: quietly regress GapGDI mms_adopt i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI mms_adopt gdi_init i.code i.year [aweight=weight11],r

eststo: quietly regress GapGDI  mms_adopt gdi_init Lag_lGDPC i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI  mms_adopt gdi_init Lag_lGDPC Lag_lfprtfe  i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI  mms_adopt gdi_init Lag_lGDPC Lag_lfprtfe  Lag_wom_law i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI  mms_adopt gdi_init Lag_lGDPC Lag_lfprtfe  Lag_wom_law Lag_urbpop i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI  mms_adopt gdi_init Lag_lGDPC Lag_lfprtfe  Lag_wom_law Lag_urbpop Lag_creditby i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI mms_adopt  $controls_vars i.code i.year [aweight=weight11],r
eststo: quietly regress GapGDI mms_adopt  $controls_vars i.code year [aweight=weight11],r
eststo: quietly regress GapGDI mms_adopt  $controls_vars i.code##c.year [aweight=weight11],r

esttab, star(* 0.1 ** 0.05 *** 0.01) keep(mms_adopt)



////////////////////////////
// 2- Autre spécification 


* // a) variables de traitement pour les fenêtres ±5 à ±1)
* régressions sur fenêtres symétriques
gen distance_adopt = year - adopt_year
replace distance_adopt = . if distance_adopt >= 2000

* [1]
foreach n in 5 4 3 2 1 {
    preserve
    keep if distance_adopt >= -`n' & distance_adopt <= `n'
    reg GapGDI mms_adopt $controls_vars i.region_id i.year [aweight=weight11]
    eststo result_`n'
    restore
}
esttab result_5 result_4 result_3 result_2 result_1, r2 ///
    p star(* 0.1 ** 0.05 *** 0.01) ///
    label keep(mms_adopt) title("Effet du mobile money selon fenêtres symétriques")


	
	
// b) Altering mobile money definition	

*  [2] Mobile money (sans la première année d'adoption)
gen mms_exclfirst = mms_adopt
replace mms_exclfirst = . if year == adopt_year 


* [3] mobile money anticipé et [4] retardé d'un an (lag)
gen byte mms_lead1 = F1.mms_adopt
gen byte mms_lag1 =  L1.mms_adopt

*order mms_* year* adopt_year start_placebo rand_adopt mms_placebo2 

*[5] Deux première année d'adoption seulement
*drop mms_2first
gen mms_2first = (year == adopt_year|year == adopt_year+1)
replace mms_2first = . if (year>adopt_year+1 & adopt_year!=0)


eststo clear
*eststo: regress gdi mms_nobank $controls_vars i.region_id i.year [aweight=weight11], vce(cluster code)
eststo: regress gdi mms_exclfirst $controls_vars i.code i.code i.year [aweight=weight11],r
eststo: regress gdi mms_lead1 $controls_vars i.code i.year [aweight=weight11],r
eststo: regress gdi mms_lag1 $controls_vars i.code i.year [aweight=weight11], r
eststo: regress gdi mms_2first $controls_vars i.code i.year [aweight=weight11], r

* [6] Placebo : dates randomisées

* Seed pour reproductibilité
set seed 12345
*drop *placebo*
*drop u rand_adopt is_chosen rand_adopt_country
gen double u = runiform()
bysort code (u): gen byte is_chosen = (_n == 1)
gen int rand_adopt = year if is_chosen
bysort code: egen int rand_adopt_country = max(rand_adopt)
gen byte mms_placebo = (year >= rand_adopt_country)
sort code year



* [7] Placebo : traitement commençant 2 an AVANT la vraie adoption
*gen start_placebo2 = adopt_year - 6
gen start_placebo7 = adopt_year - 7
replace start_placebo7=0 if adopt_year==0 
gen mms_placebo7 = (year >= start_placebo7 &  adopt_year!=0)


eststo: regress gdi mms_placebo $controls_vars i.code i.year [aweight=weight11], ro
eststo: regress gdi mms_placebo7 $controls_vars i.code i.year [aweight=weight11], ro
esttab, se  star(* 0.1 ** 0.05 *** 0.01) ///
    label varwidth(25) modelwidth(20) ///
    title("Table A2 – Robustesse aux définitions alternatives du traitement") ///
    keep(mms_exclfirst mms_lead1 mms_lag1 mms_2first mms_placebo* ) ///
    obslast nonum
	
// d) Autres mesure de les inégalité de genre	
eststo clear
eststo: regress gii mms_adopt $controls_vars1 i.code i.year [aweight=weight11],r
**# Bookmark #1
eststo: regress gequal_eff mms_adopt $controls_vars i.code i.year  [aweight=weight11],ro
eststo: regress hdife mms_adopt $controls_vars i.code i.year [aweight=weight11], ro

esttab, p r2 star(* 0.1 ** 0.05 *** 0.01) ///
    label varwidth(25) modelwidth(20) ///
    title("Table A2 – Robustesse aux définitions alternatives du traitement") ///
    keep(mms_adopt) ///
    obslast nonum	
	


// Autre mesure du mobile money (FAS) data 

*sum mm_act mm_tx_num
* mm_act_pop

eststo clear
eststo: reg GapGDI cum_providers $controls_vars i.code i.year ,r	
*reg gdi mm_act_ag $controls_vars i.code i.year ,r	
eststo: reg GapGDI Log_mm_act $controls_vars i.code i.year ,r
eststo: reg GapGDI log_mm_tx_num $controls_vars i.code i.year ,r		
eststo:reg GapGDI mobileaccount_t_d $controls_vars i.code ,r
eststo:reg GapGDI mobileaccount_t_d_1 $controls_vars i.code ,r	
esttab, p r2 star(* 0.1 ** 0.05 *** 0.01) ///
    label varwidth(25) modelwidth(20) ///
    title("Table A2 – Robustesse aux définitions alternatives du traitement") ///
    keep(cum_providers Log_mm_act log_mm_tx_num  mobileaccount_t_d mobileaccount_t_d_1) ///
    obslast nonum	

	


 // Modification de l'echantillon

* Exclusion des pays communistes
*Russie(RUS), China(CHN) , North Korea, Laos(LAO), Cuba(CUB), and Vietnam(VNM).
est clear 
qui reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] ///
    if !inlist(countrycode, "RUS","LAO","CUB","VNM","PRK"), cluster(code)
estimates store excl1
* Exclusion des pays ayant adopté nouvelement
qui reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] ///
    if !inlist(adopt_year, 2021,2022,2023), cluster(code)
 
 
* Outliers
*Cook's distance are greater than 4/NT as outliers.*/
quietly {
    local ireg = 0
    foreach outcome in gdi gii {
        local ireg = `++ireg'
        xi: reg `outcome' mms_adopt $controls_vars i.year 
		
		//OUTLIERS BASED ON STANDARDIZED RESIDUALS
			capture drop stdres
			predict stdres if e(sample), rstandard  
			
		* Identify observations whose standardized residual exceed +1.96 or -1.96, 
	    * i.e whose standardized residual in absolute value exceed 1.96, as outliers.
			capture drop outlier1
			gen outlier1 = 0 if e(sample)==1
			replace outlier1 = 1 if abs(stdres)>1.96 & e(sample)==1
			
		//OUTLIERS BASED ON COOK's DISTANCE
			capture drop cooksdis
			predict cooksdis if e(sample), cooksd
			
		**Identify observations whose Cook's Distance are greater than 4/NT,
	    ** i.e. cooksd>4/NT, as outliers.
			capture drop outlier2
			gen outlier2 = 0 if e(sample)==1
			replace outlier2 = 1 if cooksdis>(4/e(N)) & e(sample)==1
			
	 *****************
	//ROBUSTNESS CHECKS TO OUTLIERS
	     *reg `outcome' ZMT* $controls_vars i.year_id [aweight=weight2] if data_zm==1 & (outlier1!=1 & outlier2!=1), cluster(codgeo_id)
		 quietly reg `outcome' mms_adopt i.code i.year $controls_vars [aweight=weight11] if (outlier1!=1 & outlier2!=1), 
         estimates store regfe_`ireg', title("`outcome'")
    }
}

esttab excl* regfe_* ,ar2 starlevels(* 0.10 ** 0.05 *** 0.01) keep(mms_adopt) title("Résultats des régressions")

	
///////////////	
// Methode alternatives

* // [1] MCO
*===========================================
est clear
eststo: reg gdi mms_adopt $controls_vars,r
eststo: reg gdi mms_adopt $controls_vars                         i.code, r
eststo: reg gdi mms_adopt $controls_vars                         i.year, r
eststo: reg gdi mms_adopt $controls_vars                         i.region_id, r
eststo: reg gdi mms_adopt $controls_vars                         i.code i.year,r
eststo: reg gdi mms_adopt $controls_vars                         i.region_id i.year, r

esttab, star(* 0.1 ** 0.05 *** 0.01) keep(mms_adopt)






// [2 ] Estimation IV en panel avec effets fixes et erreurs-clusterisées par pays
*-=========================================================================

*Mobile money : Continue variable
est clear
global iv1 "Log_mm_act ICTPricesMBGNI network_covered lGDPC lfprtfe wom_law creditby urbpop Lict1"
global iv2 "GapGDI (Log_mm_act = ICTPricesMBGNI network_covered) lGDPC lfprtfe wom_law creditby urbpop Lict1"

// Première étape (Fixed Effects OLS)
quietly xtivreg2 $iv1, fe i(code) t(year)  robust
estimates store FirstStage

// Deuxième étape (2SLS FE) avec diagnostics
quietly 
xtivreg2 $iv2, fe i(code) t(year)  robust first

// Ajouter les statistiques de diagnostic
estadd scalar F_first       = e(cdf)   // Cragg-Donald Wald F
estadd scalar F_first1      = e(rkf)  // Kleibergen-Paap rk Wald F
estadd scalar KP_LM         = e(arf)   // Kleibergen-Paap rk LM statistic
estadd scalar chi2_KP_LM    = e(idp)     // p-value KP rk LM statistic     // p-value Hansen J
estadd scalar Hansen_J      = e(j)      // Hansen J overid
estadd scalar chi2_Hansen_p = e(jp)     // p-value Hansen J
estimates store SecondStage


*Mobile money : binary variable	
global iv3 " mms_adopt network_covered lGDPC lfprtfe wom_law creditby urbpop Lict1"
global iv4 "GapGDI (mms_adopt =  network_covered) lGDPC lfprtfe wom_law creditby urbpop Lict1"

// Première étape (Fixed Effects OLS)
quietly xtivreg2 $iv3, fe i(code) t(year)  robust
estimates store FirstStage2

// Deuxième étape (2SLS FE) avec diagnostics
quietly xtivreg2 $iv4, fe i(code) t(year)  robust first

// Ajouter les statistiques de diagnostic
estadd scalar F_first       = e(cdf)   // Cragg-Donald Wald F
estadd scalar F_first1      = e(rkf)  // Kleibergen-Paap rk Wald F
estadd scalar KP_LM         = e(arf)   // Kleibergen-Paap rk LM statistic
estadd scalar chi2_KP_LM    = e(idp)     // p-value KP rk LM statistic     // p-value Hansen J
estadd scalar Hansen_J      = e(j)      // Hansen J overid
estadd scalar chi2_Hansen_p = e(jp)     // p-value Hansen J
estimates store SecondStage2

// 4) Exporter le tableau comparatif
esttab FirstStage FirstStage2 SecondStage  SecondStage2 using "./Results/IV_stages_table2.txt", replace label  ///
     order(ICTPricesMBGNI network_covered Log_mm_act mms_adopt) b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
    collabels("First stage" "First stage" "Second stage" "Second stage") ///
    stats(N_g N F_first F_first1 KP_LM chi2_KP_LM Hansen_J chi2_Hansen_p, ///
          labels("Countries" "Observations" "Cragg-Donald F" "KP rk Wald F" "KP rk LM" "p-value KP LM" "Hansen J" "Chi-sq(1) p-value H" )) ///
    title("First- and Second-Stage Regression Results") ///
    note("Standard errors clustered at country level; FE by country-year; Instruments: price & coverage")
// Statistiques clés à retenir : :
// - F_first  : Cragg-Donald Wald F (force instruments)
// - F_first1 : KP rk Wald F (robust to clustering)
// - KP_LM    : KP rk LM test (underid)
// - Hansen_J : test overidentification
// - chi2_Hansen_p: p-value correspondant

//--------



*// [2] ra, ipw, ipwa, PSM
**==================================
est clear 
qui eststo ra :
teffects ra (gdi $controls_vars ) (mms_adopt),vce(bootstrap,reps(50)) //Regression ajustement
qui eststo ipw : teffects ipw (gdi) (mms_adopt $controls_vars ),vce(bootstrap,reps(500)) //IPW
qui eststo ipwra: teffects ipwra (gdi $controls_vars) (mms_adopt $controls_vars ),vce(bootstrap,reps(500)) // AIPW
*esttab ra ipw ipwra



** NN‐matching (k = 1,2,3) avec logit, common support et bootstrap
*eststo clear
foreach k in 1 2 3 {
    bootstrap r(att), reps(500) nodots: ///
        psmatch2 mms_adopt $controls_vars , ///
            outcome(gdi) neighbor(`k') logit common ate
    eststo nn`k': estadd scalar ATT = r(att)
}


** Radius matching : 
foreach r in 0.005 0.01 0.05 {
    bootstrap r(att), reps(500) nodots: ///
        psmatch2 mms_adopt $controls_vars , ///
            outcome(gdi) radius caliper(`r') logit common ate
    * pour nommer rad005, rad01, rad05 :
    local label = subinstr("`r'", "0.", "", .)
    eststo rad`label': estadd scalar ATT = r(att)
}

** Kernel matching 
bootstrap r(att),reps(500) nodots:psmatch2 mms_adopt $controls_vars ,  outcome(gdi) kernel k(biweight)  logit common ate
eststo kern: estadd scalar ATT = r(att)

** Local‐linear (option ll)
bootstrap r(att),reps(500) : psmatch2 mms_adopt $controls_vars , outcome(gdi) llr k(biweight) logit common ate
eststo llr: estadd scalar ATT = r(att)

esttab ra ipw ipwra nn1 nn2 nn3 rad005 rad01 rad05 kern using "./Results/Robust_PSM.txt",replace ///
    title("Table A4 – PSM specifications") ///
    star(* 0.1 ** 0.05 *** 0.01) ///
    stats(N_treat N_ctrl N, fmt(0 0 0) ///
          labels("Number of Treated Obs." "Number of Controls Obs." "Observations")) ///
    addnotes("Bootstrapped standard errors (500 reps). *** p<0.01, ** p<0.05, * p<0.1. No difference with Table 5 obs.")


	
*[3] Event studies
global controls2 Lag_lGDPC Lag_lfprtfe Lag_wom_law Lag_urbpop Lag_creditby Lag_ict1
// Estimation with did_imputation of Borusyak et al. (2021)
tsset code year
replace adopt_year=. if adopt_year==0
gen K = year-adopt_year	
gen D = K>=0 & adopt_year!=.

order code year K D mms_adopt gdi lGDPC lfprtfe wom_law creditby urbpop Lict1 adopt_year mms_adopt

did_imputation gdi code year adopt_year , horizons(0/10) autosample pretrends(10)

did_imputation gdi code year adopt_year , horizons(0/10) ///
 controls($controls2 ) fe(code year ) ///
 autosample pretrends(10) cluster(code)  tol(0.005)
 
event_plot, default_look graph_opt(xtitle("Periods since the event") ytitle("Averagecausal effect") title("Borusyak et al. (2021) imputation estimator") xlabel(-10(1)10))

// Estimation with did_multiplegt of de Chaisemartin and D'Haultfoeuille (2020)
 did_multiplegt (dyn) gdi code year D, effects(10) placebo(5) cluster(code) 
event_plot e(estimates)#e(variances), default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
	title("de Chaisemartin and D'Haultfoeuille (2020)") xlabel(-5(1)5)) stub_lag(Effect_#) stub_lead(Placebo_#) together

	
* Modèle sans contrôles
est clear
did_multiplegt_dyn gdi code year mms_adopt, effects(10) effects_equal("all")  placebo(8) 
estadd scalar p_joint = e(p_equality_effects)   
estadd scalar p_placebo = e(p_jointplacebo) 
estadd scalar ATT = e(Av_tot_effect)
estadd scalar Obs = e(N_avg_total_effect) 
estadd local controls = "No"
est sto model_1

* Modèle avec contrôles
did_multiplegt_dyn gdi code year mms_adopt, effects(8) effects_equal("all")  placebo(8) ///
    controls($controls2) 
estadd scalar p_joint = e(p_equality_effects)  
estadd scalar p_placebo = e(p_jointplacebo) 
estadd scalar ATT = e(Av_tot_effect)
estadd scalar Obs = e(N_avg_total_effect) 
estadd local controls = "Yes"
est sto model_2

* Export des résultats
esttab model_* , replace booktabs se  star(* 0.1 ** 0.05 *** 0.01) ///
    s(ATT p_joint p_placebo controls Obs )

	
//  (Sun & Abraham, 2021) : estimateur flexible qui contrôle explicitement les tendances pré-traitement
tsset code year
gen event_time=year-adopt_year
tab event_time

replace adopt_year=. if adopt_year==0
 gen never_mms = (adopt_year == .)
 
 order adopt_year never_mms
 
 forvalues k = 15(-1)2 {
           gen g_`k' = event_time == -`k'
        }
		
   forvalues k = 0/15 {
             gen g`k' = event_time == `k'
        }
        
eventstudyinteract gdi g_* g0-g15, cohort(adopt_year) control_cohort(never_mms) covariates($controls2) absorb(i.code i.year) vce(cluster code)
	
// 
ssc install xthdidregress,replace
qui xthdidregress twfe (gdi $controls2) (mms_adopt), group(code)

   
   
*[5] DID 
use data_final,clear
keep if year>=2000
* &  year<=2023
*drop if adopt_year>=2023 
order  code year gdi lGDPC lfprtfe wom_law creditby urbpop Lict1 adopt_year mms_adopt adopt_year
bysort year : tab mms_adopt


**# Bookmark #3
csdid gdi $controls_vars i.region_id [weight=weight11], ivar(code) time(year) gvar(adopt_year) method(dripw) wboot rseed(1)  
estat event,  estore(cs) 

event_plot cs, default_look graph_opt(xtitle("Periods since the event") ytitle("Average effect") ///
	title("csdid") xlabel(-10(1)10)) stub_lag(Tp#) stub_lead(Tm#) together	
**# Bookmark #1
estat all
estat simple
estat calendar




// +================================================================
   * Heterogeneité du traitement
// ================================================================

* // Traite seulement l'événement k années après l'adoption
use data_final,clear
gen distance_adopt = year - adopt_year
tab distance_adopt

*drop treat*
forvalues k = 0/7 {
    gen treat`k' = inrange(distance_adopt, 0, `k')
	  
}
forvalues k = 0/7 {
    preserve
	keep if distance_adopt <= `k'
        reg gdi treat`k' $controls_vars ///
            i.code i.year [aweight=weight11],cluster(code)
        eststo res`k'
    restore
}

esttab res*, ///
    se star(* 0.1 ** 0.05 *** 0.01) ///
    label ///
    keep(treat*) ///
    title("Mobile money effect over time") ///
    mtitles(                                                ///
      "t₀ (adoption)"                                      ///
      "t+1 (1ʳᵉ année après)"                              ///
      "t+2 (2ᵉ année après)"                              ///
      "t+3 (3ᵉ année après)"                              ///
      "t+4 (4ᵉ année après)"                              ///
      "t+5 (5ᵉ année après)"                              ///
	   "t+6 (6ᵉ année après)"                              ///
	    "t+7 (7ᵉ année après)"                              ///
    )
/*nreported constant included. Standard errors in brackets. *** p<0.01, ** p<0.05, * p<0.1. Note: Compared to Table 5, observations related to the treatment variable differ.
The first column constructs a new treatment variable by dropping all observations following the mobile money initiation year. Columns [2–6] consider only observations for
1, 2, 3, 4 and 5 years after the mobile money adoption year, respectively.*/



*// Disaggregating mobile money services
est clear
local services has_Inremitt has_P2Gdom has_P2G ///
               has_G2P has_Billpay has_Airtimetopup ///
               has_Merchpay has_Cashin has_Cashout 

foreach svc of local services {
    eststo `svc': ///
        reg gdi `svc' $controls_vars ///
            i.code i.year [aweight=weight11], ro
}

esttab ///
    has_Inremitt has_P2Gdom has_P2G has_G2P ///
	has_Billpay has_Airtimetopup has_Merchpay ///
	has_Cashin has_Cashout, ///
    se star(* 0.1 ** 0.05 *** 0.01) ///
    label ///
	 keep(has_*) ///
    title("Effets par type de service mobile money")

	
// Niveau de revenu
reg gdi mms_adopt $controls_vars ///
        i.code i.year [aweight=weight11] if (incomelevel=="HIC" |incomelevel=="UMC"), r

reg gdi mms_adopt $controls_vars ///
        i.code i.year [aweight=weight11] if (incomelevel=="LIC" |incomelevel=="LMC"),  r

//  Régions heterogeneité
reg gdi mms_adopt $controls_vars ///
        i.code i.year [aweight=weight11] if (regionname=="Sub-Saharan Africa"),  r
		
reg gdi mms_adopt $controls_vars ///
        i.code i.year [aweight=weight11] if (regionname=="Middle East and North Africa"),  r
		
reg gdi mms_adopt $controls_vars ///
        i.code i.year [aweight=weight11] if (regionname=="Latin America and Caribbean"),  r

reg gdi mms_adopt $controls_vars ///
        i.code i.year [aweight=weight11] if (regionname=="Europe and Central Asia"),  r

reg gdi mms_adopt $controls_vars ///
        i.code i.year [aweight=weight11] if (regionname=="East Asia and Pacific"),  r	
		

		
// Facteurs structurelle

twoway (scatter gdi litracyfe) ///
       (lfitci gdi litracyfe if mms_adopt==1) ///
       (lfitci gdi litracyfe if mms_adopt==0), ///
       legend(order(1 "GDI vs Litracy" 2 "Adopt=1" 3 "Adopt=0")) 

 // Niveau initial gdi elevé
drop Hig_gdi_init
sum gdi_init,detail		   
gen Hig_gdi_init=(gdi_init>r(p50))
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_gdi_init==1 ,  
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_gdi_init==0 ,r	
	
	
	
drop Hig_litracyfe
sum litracyfe,detail		   
gen Hig_litracyfe=(litracyfe>r(p50))
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_litracyfe==1,r	 
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_litracyfe==0 ,r	
		   
drop Hig_schoolfe
sum schoolfe,detail		   
gen Hig_schoolfe=(schoolfe>r(p50))
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_schoolfe==1,r  
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_schoolfe==0 ,r			   


		
drop Hig_txpoprural
sum txpoprural,detail		   
gen Hig_txpopruralt=(txpoprural>r(p50))
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_txpoprural==  
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_txpoprural==0 ,r		
			   		   

// Women business and Law		   
sum wom_law, detail		   
gen Hig_wom_law=(wom_law>r(p50))			   
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_wom_law==1 ,r
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_wom_law==0 ,r

// Women political empowerment index		   		   
sum v2x_gender, detail		   
gen Hig_v2x_gender=(v2x_gender>r(p50))			   
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_v2x_gender==1 ,r
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_v2x_gender==0 ,r
		   		 
// women civil liberties index			   
sum v2x_gencl, detail		   
gen Hig_v2x_gencl=(v2x_gencl>r(p50))			   
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_v2x_gencl==1 ,r
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_v2x_gencl==0 ,r
 
 
sum  v2clprptyw, detail		   
gen Hig_v2clprptyw=(v2clprptyw>r(p50))			   
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_v2clprptyw==1 ,r
reg gdi mms_adopt $controls_vars i.code i.year [aweight=weight11] if Hig_v2clprptyw==0 ,r

		
		
* Définition de vos variables continues et interactions
est clear
foreach X in Lag_schoolfe txpoprural v2x_gender v2x_gencl hdife  {
    qui reg gdi i.mms_adopt##c.`X' $controls_vars i.code i.year [aweight=weight11] , robust
    *qui testparm c.mms_adopt#c.`X'    // test de Wald pour effet différentiel
    est store int_`X'
}
		
esttab int_*,  se star(* 0.1 ** 0.05 *** 0.01) ///
    	drop(*code*  *year*) 
  	
////////////////////////////////////////////////////////

// TRANSMISSION CHANNEL

/////////////////////////////////////////////////////////
* Canal 1: Inclusion financière & épargne
* Canal 2: Flux de remittances
* Canal 3: Entrepreneuriat & emploi

// Correlation between gdi et main channel
use data_final,clear
keep if year>=2000
est clear
pwcorr gdi account_t_d_1 save_any_1 fin28_t_d_1 lfprtfe remitxy empself_fe
global controls  gii_init Lag_lGDPC Lag_urbpop Lag_wom_law Lag_creditby Lag_ict1  
eststo: reg gdi  account_t_d_1  mms_adopt $controls  i.year ,ro   // Women inclusion 
eststo: reg gdi  fin28_t_d_1    mms_adopt  $controls   i.year , ro // remittance women
eststo: reg gdi  remitxy        mms_adopt   $controls    i.year i.code ,ro  // remitxy: Remittance 
eststo: reg gdi  lfprtfe        mms_adopt   $controls    i.year i.code,ro // labor force women
eststo: reg gdi  hdi            mms_adopt   $controls     i.year i.code ,ro // hdi=indice de developpement humain 
eststo: reg gdi  hdife          mms_adopt   $controls    i.year i.code ,ro // hdife indice de developpement humain femme
esttab ,  p star(* 0.1 ** 0.05 *** 0.01) keep(account_t_d_1 fin28_t_d_1 remitxy lfprtfe hdi hdife) 
    	
// Impact of Mobile money on channels
est clear
**# Bookmark #11
eststo: reg account_t_d_1      L(0/3).mms_adopt $controls_vars   i.code [aweight=weight11],ro
eststo: reg fin28_t_d_1        mms_adopt       $controls_vars   i.year i.code [aweight=weight11] , ro
eststo: reg remitxy            mms_adopt       $controls_vars    i.year i.code [aweight=weight11], ro
eststo: reg hdi                mms_adopt       $controls_vars    i.code i.year [aweight=weight11] , ro 
eststo: reg hdife              mms_adopt       $controls_vars i.code i.year [aweight=weight11] , ro 
eststo: reg lfprtfe            mms_adopt       gdi_init Lag_lGDPC Lag_urbpop Lag_wom_law Lag_creditby Lag_ict1 i.year [aweight=weight11]

esttab ,  p star(* 0.1 ** 0.05 *** 0.01) keep(mms_adopt) 


	

		
		
		

  
