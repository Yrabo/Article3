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