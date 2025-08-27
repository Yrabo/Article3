*=====================================================================================
* Replication code for:
* Mobile money services and gender inequality in developing countries

* *
* Dabou Yrabo
* Université Clermont Auvergne-CERDI/CNRS (Clermont-Ferrand, France)
* E-mail: dabouyrabo5@gmail.com

*==================================================================================


* Data preprocessing

run Data_preprocessing_MMS_and_gender.do



//Panel data specification base on t:
use mmsgender_data,clear
sort code year, stable
xtset code year,

tab year, gen(yr)
//==========================================================================

    *// Entropy balancing: Generating weights

//============================================================================

* Covariables 
drop weight*
global controls_vars  gdi_init Lag_lGDPC Lag_lfprtfe Lag_wom_law Lag_urbpop Lag_creditby Lag_ict1
global controls_vars1 gii_init Lag_lGDPC Lag_lfprtfe Lag_wom_law Lag_urbpop Lag_creditby Lag_ict1

drop weight*
ebalance mms_adopt  $controls_vars i.year, gen(weight11) tolerance(0.52)   
ebalance mms_adopt  $controls_vars1 i.year, gen(weight12) tolerance(0.52)  

save data_final, replace


=============================================================
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

// Number of countries using in the regression 
*
drop in_fe tag_fe 
reg GapGDI gdi_init Lag_lGDPC Lag_lfprtfe Lag_wom_law Lag_urbpop Lag_creditby mms_adopt i.code i.year
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
graph bar (mean) GapGDI [aweight=weight11], ///
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



// ==========================================================================

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



//===================================================================
// [1] Entropy balancing: Impact of mobile money on gender gap parity (measure gender equality)

eststo clear
eststo: quietly regress GapGDI mms_adopt  $controls_vars  [aweight=weight11],cluster(code)
eststo: quietly regress GapGDI  mms_adopt  $controls_vars i.code [aweight=weight11],cluster(code)
eststo: quietly regress GapGDI  mms_adopt  $controls_vars i.year [aweight=weight11],cluster(code)
eststo: quietly regress GapGDI  mms_adopt  $controls_vars i.region_id [aweight=weight11],cluster(code)
eststo: quietly regress GapGDI  mms_adopt  $controls_vars i.region_id i.year [aweight=weight11],cluster(code)
eststo: quietly regress GapGDI  mms_adopt  $controls_vars i.region_id##i.year [aweight=weight11],cluster(code)
eststo: quietly regress GapGDI  mms_adopt  $controls_vars i.code i.year [aweight=weight11],cluster(code)
esttab, star(* 0.1 ** 0.05 *** 0.01) keep(mms_adopt)


// ===========================================================================
// [2 ] Panel estimation IV with fixed effects and country-clustered errors

* a) Mobile money : Continue variable
est clear
global iv1 "Log_mm_act ICTPricesMBGNI network_covered gdi_init lGDPC lfprtfe wom_law creditby urbpop Lict1"
global iv2 "GapGDI (Log_mm_act = ICTPricesMBGNI network_covered) gdi_init lGDPC lfprtfe wom_law creditby urbpop Lict1"

// Première étape (Fixed Effects OLS)
quietly 
xtivreg2 $iv1, fe i(code) t(year)  robust
estimates store FirstStage

// Deuxième étape (2SLS FE) avec diagnostics
quietly 
xtivreg2 $iv2 , fe i(code) t(year)  robust first

// Ajouter les statistiques de diagnostic
estadd scalar F_first       = e(cdf)   // Cragg-Donald Wald F
estadd scalar F_first1      = e(rkf)  // Kleibergen-Paap rk Wald F
estadd scalar KP_LM         = e(arf)   // Kleibergen-Paap rk LM statistic
estadd scalar chi2_KP_LM    = e(idp)     // p-value KP rk LM statistic     // p-value Hansen J
estadd scalar Hansen_J      = e(j)      // Hansen J overid
estadd scalar chi2_Hansen_p = e(jp)     // p-value Hansen J
estimates store SecondStage


* b) Mobile money : binary variable	
global iv3 " mms_adopt network_covered gdi_init lGDPC lfprtfe wom_law creditby urbpop Lict1"
global iv4 "GapGDI (mms_adopt =  network_covered) gdi_init lGDPC lfprtfe wom_law creditby urbpop Lict1"

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
esttab FirstStage FirstStage2 SecondStage  SecondStage2 using "IV_stages_table2.rtf", replace label  ///
     order(ICTPricesMBGNI network_covered Log_mm_act mms_adopt) b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
    collabels("First stage" "First stage" "Second stage" "Second stage") ///
    stats(N_g N F_first F_first1 KP_LM chi2_KP_LM Hansen_J chi2_Hansen_p, ///
          labels("Countries" "Observations" "Cragg-Donald F" "KP rk Wald F" "KP rk LM" "p-value KP LM" "Hansen J" "Chi-sq(1) p-value H" )) ///
    title("First- and Second-Stage Regression Results") ///
    note("Standard errors clustered at country level; FE by country-year; Instruments: price & coverage")
**# Bookmark #2
	
	
	//=====================================================================
	
                // ROBUSTESSE 

				
	//============================================================================			

* Additionnal controls variables
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

esttab using "R01.rtf", star(* 0.1 ** 0.05 *** 0.01) drop(*code* *year*) label replace


// // Gradual addition of control variables and country specific trend 
eststo clear
eststo: quietly 
regress GapGDI mms_adopt  [aweight=weight11],r
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
esttab using "R02.rtf", star(* 0.1 ** 0.05 *** 0.01) drop(*code* *year*) label replace

