***************************************************************
*-------------- USER ------------------------------------------

				* Yrabo DABOU 1
				* Juste SOME  2
************************************************************
clear all
set more off, permanently


*----------------------------------------------
/*local user 1   // Current user for this file
	
  if $user==1 {
   cd "G:\Mon Drive\Article   Gap FIG and tax revenue\Données\All controls variables"
		
	}
	
	if $user==2 {
		
	} */
	
	
*----------------------------------------------
global directory "G:\Mon Drive\Article - Gap FIG and tax revenue\Donnees\All controls variables" 

global sourcedata "$directory/Base-used" 

global gendata "$directory/Base-generated"  

global logfiled "$directory/Logfiles" 

cd "$logfiled" // On se met dans le dossier du logfile pour l'enregistrer.
capture log close

log using control_variables.log, replace 


*******************************
///////////////////////////////////////////////////////////////////////////

**Téléchargement des données de la qualité institutionnelle

wbopendata, ///
indicator( ///
VA.EST; /// % Voice and Accountability
CC.EST; /// % Control of Corruption
GE.EST; /// % Government Effectiveness
PV.EST; /// % Political Stability and Absence of Violence/Terrorism
RL.EST; /// % Rule of Law
RQ.EST; /// % Regulatory Quality
SP.POP.TOTL; /// % Population, total
SP.POP.1564.TO; /// % Population aged 15-64, total
SP.RUR.TOTL.ZG; /// % Rural population growth (annual %)
SL.EMP.TOTL.SP.ZS; /// % Employment to population ratio, 15+, total (%) (modeled ILO estimate)
SP.POP.DPND; ///      % Age dependency ratio (% of working-age population)
EN.POP.DNST; /// % Population density (people per sq. km of land area)
SL.EMP.TOTL.SP.FE.ZS; /// % Employment to population ratio, 15+, female (%) 
SL.EMP.TOTL.SP.MA.ZS; /// % Employment to population ratio, 15+, male (%) 
SL.UEM.ADVN.ZS; /// % Unemployment with advanced education (% of total labor force with advanced education)
SL.TLF.CACT.ZS; /// Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)
SL.TLF.CACT.MA.ZS; /// % Labor force participation rate, male (% of male population ages 15+) (modeled ILO estimate)
SL.TLF.CACT.FE.ZS; /// % Labor force participation rate, female (% of female population ages 15+) (modeled ILO estimate)
HD.HCI.OVRL; /// Human capital index (HCI) (scale 0-1)
HD.HCI.OVRL.FE; /// Human capital index (HCI), female (scale 0-1)
GC.TAX.TOTL.GD.ZS; /// % Tax revenue (% of GDP)
NE.CON.GOVT.ZS; /// % General government final consumption expenditure (% of GDP)
NE.CON.PRVT.ZS; /// % Households and NPISHs final consumption expenditure (% of GDP)
NE.CON.PRVT.PC.KD; /// % Households and NPISHs final consumption expenditure per capita (constant 2015 US$)
NY.GDP.TOTL.RT.ZS; ///  % Total natural resources rents (% of GDP)
BX.TRF.PWKR.DT.GD.ZS; /// % Personal remittances, received (% of GDP)
NV.IND.MANF.ZS; /// % Manufacturing, value added (% of GDP)
IQ.CPA.REVN.XQ; /// % CPIA efficiency of revenue mobilization rating (1=low to 6=high)
IC.TAX.DURS; /// % Time to prepare and pay taxes (hours)
EG.ELC.ACCS.ZS; /// % Access to electricity (% of population)
IT.CEL.SETS.P2; /// % Mobile cellular subscriptions (per 100 people)
IT.MLT.MAIN; /// Fixed telephone subscriptions
IT.NET.USER.ZS; /// % Individuals using the Internet (% of population)
FS.AST.DOMS.GD.ZS; /// % financial deeping, Domestic credit provided by financial sector (% of GDP)
DT.ODA.ALLD.KD; /// % Net official development assistance and official aid received (constant 2020 US$)
BX.KLT.DINV.WD.GD.ZS; /// % Foreign direct investment, net inflows (% of GDP)
NY.GDP.MKTP.KD; /// % GDP (constant 2015 US$) *
NY.GDP.PCAP.KD; /// % GDP per capita (constant 2015 US$)
NY.GDP.PCAP.KD.ZG; ///  % GDP per capita growth (annual %)
NE.GDI.FTOT.KD; ///  %Gross fixed capital formation (constant 2015 US$)
NE.GDI.FTOT.ZS; ///	% Gross fixed capital formation (% of GDP)
NE.GDI.FPRV.ZS; /// % Gross fixed capital formation, private sector (% of GDP)
SI.POV.GINI; /// % Gini index
NV.AGR.TOTL.ZS; /// % Agriculture, forestry, and fishing, value added (% of GDP)
SH.XPD.CHEX.GD.ZS; /// % Current health expenditure (% of GDP)
SP.DYN.LE00.IN; /// % Life expectancy at birth, total (years)
IQ.CPA.GNDR.XQ; /// % CPIA gender equality rating (1=low to 6=high)
DT.TDS.DECT.CD; /// % Debt service on external debt, public and publicly guaranteed (PPG) (TDS, current US$)
SE.PRM.NENR; /// % School enrollment, primary (% net)
NE.TRD.GNFS.ZS; /// % Trade (% of GDP)
NE.IMP.GNFS.ZS; /// % Imports of goods and services (% of GDP)
NE.EXP.GNFS.ZS; /// % Exports of goods and services (% of GDP)
FP.CPI.TOTL.ZG; /// %Inflation, consumer prices (annual %)
FP.CPI.TOTL; /// % Consumer price index (2010 = 100)
SG.GEN.PARL.ZS; /// % Proportion of seats held by women in national parliament (%)
SG.LAW.INDX; /// % Women businesses and law index score (scale 1–100)
FS.AST.PRVT.GD.ZS; /// % Domestic credit to private sector (% of GDP)
FD.AST.PRVT.GD.ZS; /// % Domestic credit to private sector by banks (% of GDP)
FM.LBL.BMNY.GD.ZS; /// % Broad money (% of GDP)
FS.AST.DOMS.GD.ZS; /// % Domestic credit provided by financial sector (% of GDP)
NV.IND.MANF.ZS; /// % Manufacturing, value added (% of GDP)
NV.SRV.TOTL.ZS; /// % Services, value added (% of GDP)
NV.IND.TOTL.ZS; /// % Industry, value added (% of GDP)
SL.UEM.TOTL.FE.NE.ZS; /// % Unemployment, female (% of female labor force) 
SL.UEM.TOTL.NE.ZS; ///   % Unemployment, total (% of total labor force) 
) clear long year(1960:2021) nometadata 

*************************************

********** Rename variable

***----------------------Instutionnal quality -------------------------------
rename va_est voicacc
label variable voicacc "Voice and Accountability"

rename cc_est contcor
label variable contcor "Control of Corruption"

rename ge_est goveff
label variable goveff "Government Effectiveness"

rename pv_est polistab
label variable polistab "Political Stability"

rename rl_est rulelaw
label variable rulelaw "Rule of Law"
rename rq_est reguqual
label variable reguqual "Regulatory Quality"

***---------------------popupation, pop active-------------------------
rename sp_pop_totl popt
label variable popt "Population, total"

rename sp_pop_1564_to popt1564
label variable popt1564 "Population aged 15-64, total"

rename sp_rur_totl_zg txpoprural
label var txpoprural "Rural population growth (annual %)"

rename sl_emp_totl_sp_zs rxemptotl
label var rxemptotl "Employment to population ratio, 15+, total (%)"

rename sp_pop_dpnd deprat
label var deprat "Age dependency ratio (% of working-age population)"

rename en_pop_dnst density 
label var density "Population density (people per sq. km of land area)"

rename sl_emp_totl_sp_fe_zs emratfe
label var emratfe "Employment to population ratio, ages 15+, female (%"

rename sl_emp_totl_sp_ma_zs emratma
label var emratma "Employment to population ratio, ages 15+, male (%)"
 
rename sl_uem_advn_zs unemrate
label var unemrate "% Unemployment with advanced education (% of total labor force with advanced education)"



rename sl_tlf_cact_zs lfprt15
label var lfprt15 "Labor force participation rate, total (% of total population ages 15+)"

rename sl_tlf_cact_ma_zs lfprtma
label var lfprtma "Labor force participation rate, male (% of male population ages 15+)" 

rename sl_tlf_cact_fe_zs lfprtfe 
label var lfprtfe "Labor force participation rate, female (% of female population ages 15+)"

rename hd_hci_ovrl hci
label var hci "Human capital index (HCI) (scale 0-1)"

rename hd_hci_ovrl_fe fhci
label var fhci "Human capital index (HCI) female (scale 0-1)"


***---------------------Tax revenue et gov consumption------------
rename gc_tax_totl_gd_zs taxy
label var taxy "Tax revenue (% of GDP)"

rename ne_con_govt_zs govconhy
label var  govconhy " General government final consumption expenditure (% of GDP)"

rename ne_con_prvt_zs conhy
label var conhy "Households and NPISHs final consumption expenditure (% of GDP)"

rename ne_con_prvt_pc_kd conhca
label var conhca "Households and NPISHs final consumption expenditure per capita (constant 2015 US$)"

rename ny_gdp_totl_rt_zs natresy
label var natresy "Total natural resources rents (% of GDP)"
 
rename nv_ind_manf_zs manufy   
label var manufy "Manufacturing, value added (% of GDP)"
 
rename bx_trf_pwkr_dt_gd_zs remitxy
label var remitxy " Personal remittances, received (% of GDP)"

rename iq_cpa_revn_xq revadm_eff
label var revadm_eff "CPIA efficiency of revenue mobilization rating (1=low to 6=high)"

**---------------Fintechs ------------------------------------
rename ic_tax_durs taxtime
label var taxtime "Time to prepare and pay taxes (hours)"

rename eg_elc_accs_zs elecaccs
label var elecaccs "Access to electricity (% of population)"

rename it_cel_sets_p2 ict2
label var ict2 "Mobile cellular subscriptions (per 100 people)"

rename it_mlt_main ict1
label var ict1 "Fixed telephone subscriptions(per 100 people)"

rename it_net_user_zs ict3
label var ict3 "Individuals using the Internet (% of population)"


*** -------------------Financial----------------------------
rename fs_ast_doms_gd_zs financial_deep 
label var financial_deep "financial deeping, Domestic credit provided by financial sector (% of GDP)"

**--------------------- Aid----------------------------------------
rename dt_oda_alld_kd aid
label var aid "Net official development assistance and official aid received (constant 2020 US$)"

**---------------------------IDE----------------------------
rename bx_klt_dinv_wd_gd_zs fdiy
label var fdiy "FDI net inflows (% of GDP)"

**-------------------------------gdp----------------------------------

rename ny_gdp_mktp_kd gdpcod
label variable gdpcod "GDP (constant 2015 US$)"

rename ny_gdp_pcap_kd gdpcodc
label variable gdpcodc "GDP per capita (constant 2015 US$)"

rename ny_gdp_pcap_kd_zg gdpcodcy
label variable gdpcodcy "GDP per capita (annual %)"

*-------------- Investment -------------------------------------------
rename ne_gdi_ftot_kd gfcfcd
label variable gfcfcousd "Gross fixed capital formation (constant 2015 USD)"

rename ne_gdi_ftot_zs gfcf 
label variable gfcf "Gross fixed capital formation (% PIB)"

rename ne_gdi_fprv_zs gfcfpr
label variable gfcfpr "Gross fixed capital formation, private sector (% of GDP)"

**----------------Gini-------------------------------------------
rename si_pov_gini ginindex
label var ginindex "Gini index"

*----------------Agricol----------------------------------
rename nv_agr_totl_zs agrvay
label var agrvay "Agriculture, forestry, and fishing, value added (% of GDP)"

**----------------Health------------------------
rename sh_xpd_chex_gd_zs healthexpy
label variable healthexpy "Current health expenditure (% of GDP)"

rename  sp_dyn_le00_in lifexp
label var lifexp "Life expectancy at birth, total (years)" 

*---------------Education---------------
*rename se_xpd_totl_gd_zs educ_expt
*label var educ_expt "Government expenditure on education, total (% of GDP)"

rename se_prm_nenr educprtx
label var educprtx "School enrollment, primary (% net)"

**----------------------Gender equality------------------------
rename iq_cpa_gndr_xq gequal_eff
label var gequal_eff "CPIA gender equality rating (1=low to 6=high)"

rename dt_tds_dect_cd servdebt
label var servdebt "Debt service on external debt, public and publicly guaranteed"
 
**------------------------Ouverture-----------------------------------
rename ne_trd_gnfs_zs tradey
label var tradey "Trade (% of GDP)"

rename ne_imp_gnfs_zs importsy
label variable importsy "Imports of goods and services (% of GDP)"

rename ne_exp_gnfs_zs exportsy
label variable exportsy "Exports of goods and services (% of GDP)"

 ***--------------------- Inflation ---------------------------------
rename fp_cpi_totl_zg infla
label var infla "Inflation, consumer prices (annual %)"

rename fp_cpi_totl cpi
label var cpi "Consumer price index (2010 = 100)"

*---------
rename sg_gen_parl_zs wom_parly  
label var wom_parly "Proportion of seats held by women in national parliament (%)"

rename sg_law_indx wom_law
label var wom_law "Women businesses and law index score (scale 1–100)"

rename fs_ast_prvt_gd_zs credity
label var credity "Domestic credit to private sector (% of GDP)"

rename fd_ast_prvt_gd_zs creditby
label var creditby "Domestic credit to private sector by banks (% of GDP)"

rename fm_lbl_bmny_gd_zs bmoney
label var bmoney "Broad money (% of GDP)"

renam fd_ast_doms_gd_zs creditfy
label var creditfy "Domestic credit provided by financial sector (% of GDP)"

rename nv_ind_manf_zs mvay
label var  mvay "Manufacturing, value added (% of GDP)"

rename nv_srv_totl_zs srvvay 
label var srvvay "Services, value added (% of GDP)"

rename nv_ind_totl_zs indvay
label var indvay "Industry, value added (% of GDP)"

rename  sl_uem_totl_fe_ne_zs umratfe
label umratfe "Unemployment, female (% of female labor force) "

rename  sl_uem_to  umrat
label var "Unemployment, total (% of total labor force) "


********************************************************************************
*replace countrycode="ROM" if countrycode=="ROU"
*replace countrycode="ZAR" if countrycode=="COD"

drop if regionname=="Aggregates"|regionname==""
drop adminregion adminregionname lendingtypename lendingtype region
save "$gendata/WDI", replace  // 217 pays
*-------------------------------------------------------------------------------
**************************************************************************************************
cd "$sourcedata"
use V-Dem-CY-Full+Others-v13, clear
keep country_name country_text_id year v2clprptym v2clprptyw v2cldmovew v2cldmovem v2clstown v2clprptym v2clprptyw v2peedueq ///
 v2peasbgen v2x_gender v2x_gencs v2x_genpp v2x_civlib v2x_gencl e_peaveduc e_ti_cpi e_peaveduc e_peedgini e_pefeliex /// 
 e_civil_war e_miinteco e_miinterc e_pt_coup

/*
label var v2x_gender "Women political empowerment index (D)"
label var  v2x_gencl "Women civil liberties index (D)"
label var v2x_gencs "Women civil society participation index (D)" 
label var v2x_genpp "Women political participation index (D)" 
label var v2xed_ed_poed "Political education effort in education"
label var e_peaveduc "Education 15+"
label var e_peedgini "Educational inequality, Gini "
label var e_pefeliex "Life expectancy "
label var e_civil_war "Civil war"
label var e_miinteco "Armed conflict, international"
label var e_miinterc "Armed conflict, internal"
label var e_pt_coup  "Number of successful coup attempts in a year" */
rename country_name country
rename country_text_id isocode
save "$gendata/dem_data", replace
*    Merge 

********************************************************************************************
*IMF Investment and Capital Stock Datase (Public infrastructure development) of IMF dataset
**********************************************************************************************
cd"$sourcedata"

*import spss using "afrobarometer_release-dataset_merge-34ctry_r8_en_2023-03-01(1).sav", clear

import excel using "IMF Investment and Capital Stock Dataset2021.xlsx", clear first sheet("Dataset")

* Divide all variables by gdp to solve the measurement problem
foreach var of varlist igov_rppp kgov_rppp ipriv_rppp kpriv_rppp ippp_rppp kppp_rppp {
	
	replace `var'=`var'/GDP_rppp*100 if !missing(`var')
	gen `var'_gdp=`var' 
}
label var igov_rppp_gdp "government investment  (% gdp)"
label var kgov_rppp_gdp "government Stocks capital (% gdp)"
label var ipriv_rppp_gdp "Private investment (% gdp)"
label var kpriv_rppp_gdp "Private Stocks capital (% gdp)"
label var ippp_rppp_gdp "Public-Private Partnership investment (% gdp)"
label var kppp_rppp_gdp "Public-Private Partnership Stocks capital  (% gdp)"

drop igov_rppp kgov_rppp ipriv_rppp kpriv_rppp ippp_rppp kppp_rppp GDP_rppp igov_n kgov_n ipriv_n kpriv_n kppp_n GDP_n ifscode  income
order country isocode year 
*replace isocode="ROM" if  isocode=="ROU"
*replace isocode="ZAR" if isocode=="COD"
*drop if isocode=="WBG"
drop if isocode=="WBG"
drop if isocode=="TWN"
keep if year>=2000 & year<2022
save"$gendata/public_infr_dvt", replace  // 194 pays 

********************************************************************************************
*--------------United Nations Development Programme (UNDP) data
******************************************************************************************

cd"$sourcedata"

// import of Human development index data
import excel using "HDR21-22_Composite_indices_complete_time_series.xlsx", clear first sheet("HDI")  
forvalues i=1990/2021 {
	destring hdi_`i' , replace force   
}
rename iso3 isocode
rename country countryname
egen id=group(isocode)
reshape long hdi_, i(id) j(year)
rename hdi_ hdi
label var hdi "Human Development Index "
drop region 
save "$gendata/gii1", replace


*--------------------// import of gender development index data --------------------------------
import excel using "HDR21-22_Composite_indices_complete_time_series.xlsx", clear first sheet("gdi")  
forvalues i=1990/2021 {
	destring gdi_`i' , replace force   
}
rename iso3 isocode
rename country countryname
egen id=group(isocode)
reshape long gdi_, i(id) j(year)
rename gdi_  gdi
label var gdi "Gender Development Index"
save "$gendata/gii2", replace

**************************************************************************************
*----------------// import of gender inequality index  -----------------------------
import excel using "HDR21-22_Composite_indices_complete_time_series.xlsx", clear first sheet("gii")  
forvalues i=1990/2021 {
	destring gii_`i' , replace force   
}
rename iso3 isocode
rename country countryname
egen id=group(isocode)
reshape long gii_, i(id) j(year)
rename gii_ gii
label var gii "Gender Inequality Index"
save "$gendata/gii3", replace
*-----------------------------------
import excel using "HDR21-22_Composite_indices_complete_time_series.xlsx", clear first sheet("Mean_years_of_schooling")  // import of  mean years of schooling
forvalues i=1990/2021 {
	destring mys_`i' , replace force   
}
rename iso3 isocode
rename country countryname
egen id=group(isocode)
reshape long mys_, i(id) j(year)
rename mys_ mys 
label var mys "Mean years of schooling of population 25+"
save "$gendata/mys", replace

*--------------------------------------------------------------
import excel using "HDR21-22_Composite_indices_complete_time_series.xlsx", clear first sheet("Mean_years_of_schooling_male")  // import of  mean years of schooling
forvalues i=1990/2021 {
	destring mys_m_`i' , replace force   
}
rename iso3 isocode
rename country countryname
egen id=group(isocode)
reshape long mys_m_, i(id) j(year)
rename mys_ mysma 
label var mysma "Mean years of schooling of population 25+, male "
save "$gendata/mysma", replace

*-----------------------------------------------------------
import excel using "HDR21-22_Composite_indices_complete_time_series.xlsx", clear first sheet("Mean_years_of_schooling_female")  // import of  mean years of schooling
forvalues i=1990/2021 {
	destring mys_f_`i' , replace force   
}
rename iso3 isocode
rename country countryname
egen id=group(isocode)
reshape long mys_f_, i(id) j(year)
rename mys_ mysfe 
label var mysfe "Mean years of schooling of population 25+, female "
save "$gendata/mysfe", replace
br

************************************************************************
***************************************************************************

**Climate change : University of Notre Dame Global Adaptation Initiative (NG GAIN) database

**************************************************************************************************
*---Vulnerability
cd"$sourcedata"
import excel using "vulnerability", clear first sheet("vulnerability")   
forvalues i=1995/2021 {
	destring Vulnerability`i' , replace force   
}
rename ISO3 isocode
rename Name countryname
egen id=group(isocode)

reshape long Vulnerability, i(id) j(year)
label var Vulnerability "Climate change vulnerability"
keep if year>=1990 & year<2022
*replace isocode="ROM" if  isocode=="ROU"
*replace isocode="ZAR" if isocode=="COD"
save "$gendata/Vulnerability", replace   // 192 pays 
*----------------------------------
*******************************************************************
cd"$gendata"
use gii1, clear
merge 1:1 isocode year using gii2
drop _merge
merge 1:1 isocode year using gii3
drop _merge
merge 1:1 isocode year using mys
drop _merge
merge 1:1 isocode year using mysma
drop _merge
merge 1:1 isocode year using mysfe
drop _merge
merge 1:1 isocode year using Vulnerability
keep if id<=195

keep countryname isocode year hdi gdi gii mys mysma mysfe Vulnerability
label var isocode "Country Code"
label var countryname "Country Name"
save "$gendata/undp", replace
*drop _merge
*************************************************************************
* Financial development index of IMF data

**********************************************************************************
cd"$gendata"
use Financial_development_index_Database, clear 
rename code isocode
keep if year>=2000 & year<2022
sort country year
drop if country=="AM"|country=="Africa"|country=="Europe"|country=="Western Hemisphere"|country=="Asia Pacific"|country=="EM"|country=="LIC"|country=="All countries"|country=="Asia and Pacific"|country=="Middle East and Central Asia"
drop ifs imf_income
*replace isocode="ROM" if  isocode=="ROU"
*replace isocode="ZAR" if isocode=="COD"
*drop FI FM FID FIA FIE FMD FMA FME
save "$gendata/financial_dvt", replace    // 183 pays

************************************************************************************
 *ICRG data that contains control of corruption index, gov stability, democraty index,
 
****************************************************************************************
use base_icrg_final, clear
*keep if year>=2000 & year<2022
*replace isocode="ROU" if  isocode=="ROM"
*replace isocode="COD" if isocode=="ZAR"
*rename Country country
save "$gendata/base_icrg_final", replace

******************************************************************************
*V-Dem Democracy Indices database

**************************************************
/*cd "$sourcedata"
use V-Dem-CY-Full+Others-v13, clear
keep country_name country_text_id year v2clprptym v2clprptyw v2cldmovew v2cldmovem v2clstown v2clprptym v2clprptyw v2peedueq v2peasbgen v2x_gender v2x_civlib v2x_gencl e_ti_cpi e_peaveduc e_peedgini 

rename country_name country
rename country_text_id isocode

*replace isocode="ROM" if  isocode=="ROU"
*replace isocode="ZAR" if isocode=="COD"
drop if isocode=="PSE" 
drop if isocode=="PSG"
drop if isocode=="ZZB" 
drop if isocode=="TWN"
drop if isocode=="SML"
sort country isocode year

distinct
keep if year>=2000 & year<2022
save "$gendata/base_v_dem", replace*/
*****************************************************
*Conflict database

******************************************************
cd"$gendata"
use conflict, clear
*replace isocode="ROU" if  isocode=="ROM"
*replace isocode="COD" if isocode=="ZAR"
save "$gendata/conflict", replace

********  Merge of all database

use control_variables, clear
rename countrycode isocode
keep if year>=1990
*se public_infr_dvt
*Taiwan Province of China
*West Bank and Gaza
*merge 1:1 isocode year using public_infr_dvt
*drop _merge
merge 1:1 isocode year using undp
drop _merge
*merge 1:1 isocode year using financial_dvt
drop _merge
merge 1:1 isocode year using Vulnerability
drop _merge

**********Merge with ICRG data that contains control of corruption index, gov stability, democraty index, 
merge 1:1 isocode year using base_icrg_final
drop _merge country

merge 1:1 isocode year using conflict
drop _merge
*drop if isocode=="CHI"|isocode=="BMU"|isocode=="ASM"|isocode=="ABW"
merge 1:1 isocode year using base_v_dem
*drop _merge
drop if isocode=="PSG"
drop _merge
drop country
save "$gendata/control_variables_all", replace

*keep if year >2010
*keep if incomelevel!="HIC"
*********************************************************************************
log close
distinct