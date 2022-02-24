qui{

local lista "arg_2017n bol_2017n bra_2017n1 chl_2017n col_2017n1 cri_2017n1 ecu_2017n slv_2017n gtm_2014n hnd_2016n mex_2016n nic_2014n pan_2017n1 pry_2017n per_2017n dom_2017n1 ury_2017n ven_2014n"

local cutoffs "30 35 40"

cd c:\bases_ipm1

noi di 			_col(20) "H_AF_35" _col(40) "H_AV_35" _col(60) "H_AM_35"

foreach base of local lista {
use "`base'_ipm1", clear


keep 	id_hogar id_pers _feh _fep parentco persindo edad sexo condact3 condactr categ5_p /*
	*/ 	nbi_matviv_ee nbi_hacina21_ee nbi_compuhog_ee nbi_interhog_ee nbi_agua_ee nbi_saneamiento_ee nbi_elect_ee nbi_combus_ee /*
	*/ 	niveduc_ee asiste_ee nbi_asistencia_ee _rezagado lee_ee nbi_logro_sc_ee nbi_logro_bs_ee lee_ee _cotizadef _TPtotal

local ini = upper(substr("`base'",1,3))


***************************************
*IPMs INDIVIDUALES 
***************************************

gen adultos=((edad>=18 & edad<60 & sexo==2) | (edad>=18 & edad<65 & sexo==1))
gen a_mayores=((edad>=60 & sexo==2)| (edad>=65 & sexo==1))


** Para las variables de carencias de la vivienda y servicios con missing las pongo como missing y saco los -1 que generan problemas al sumar privaciones
foreach var in nbi_matviv_ee  nbi_hacina21_ee  nbi_agua_ee nbi_saneamiento_ee nbi_elect_ee nbi_combus_ee {
	replace `var'=. if `var'==-1  | `var'==99
}

*TICS
if inlist("`ini'","BOL","GTM","NIC") {
	gen nbi_tics_h=(nbi_interhog_ee==1)  //privado si no tiene ni compu ni coneccion a internet//
}
else {
	gen nbi_tics_h=(nbi_compuhog_ee==1 & nbi_interhog_ee==1)  //privado si no tiene ni compu ni coneccion a internet//
}
replace nbi_tics_h=. if nbi_compuhog_ee==.  & nbi_interhog_ee==.
label var nbi_tics_h "1 Si el hogar no tiene ni computadora ni acceso a internet"

*Energia conjunta
gen nbi_energia=(nbi_elect_ee==1 | nbi_combus_ee==1)
replace nbi_energia=. if nbi_elect_ee==. & nbi_combus_ee==.
label var nbi_energia "1 si no tiene electricidad o no tiene energia limpia para cocinar"

*Agua y Saneamiento en forma conjunta
gen nbi_asan=(nbi_agua_ee==1 | nbi_saneamiento_ee==1)

***Vble 18 y 19 no asiste y no termino la secundaria
gen _noasis18=(asiste_ee==0 & niveduc_ee<5 & edad>=18 & edad<=19)

**Privacion en asistencia o rezago escolar en conjunto
gen nbi_asrez=(nbi_asistencia_ee==1 | _rezagado==1 | _noasis18==1 ) & edad<20
label var nbi_asrez "El niño no asiste a la escuela o esta rezagado"

*Conclusion educativa
gen nbi_conclued=(nbi_logro_sc_ee==1 | nbi_logro_bs_ee==1 | _noasis18==1)
label var nbi_conclued "Adulto que no termino el secundario completo (19-29) o la baja secundaria (30-59)"

*Analfabetismo de adultos mayores //que significan los -1?//
gen 	analfab_ee=1 if lee_ee==0
replace analfab_ee=0 if lee_ee==1
replace analfab_ee=99 if lee_ee==99
replace analfab_ee=-1 if lee_ee==-1

gen amay_analfab=(analfab_ee==1 & a_mayores==1)
replace amay_analfab=. if amay_analfab==99

**Percibe pension o jubilacion
* if "`ini'" == "DOM" gen nbi_pen=(_TCont==0 & a_mayores)		/* ojo, no está en la base */
* else				gen nbi_pen=(_TPtotal==0 & a_mayores)
gen nbi_pen=(_TPtotal==0 & a_mayores)
replace nbi_pen=. if a_mayores==0
label var nbi_pen "Percibe una pension de algun tipo"


**Definicion de privacion en empleo para adultos

*INCORPORA TDNR
gen ocup_priv2= (condactr_ee == 2 | categ5_p == 5 | (_cotizadef==0 & condactr_ee==1) | condactr_ee == 4) 
label var ocup_priv2 "Desempleados, fliar no remunerado, ocupados que no aportan a la seg soc y amas de casa"


**Indicadores de privacion

*Variables de los Adultos: nbi_matviv_ee nbi_hacina21_ee nbi_tics_h nbi_agua_ee nbi_saneamiento_ee nbi_energia nbi_conclued ocup_priv2

*Variables de Adultos Mayores: nbi_matviv_ee nbi_hacina21_ee nbi_tics_h nbi_agua_ee nbi_saneamiento_ee nbi_energia amay_analfab nbi_pen

*Gaps de dimension VIVIENDA para los tres grupos
foreach var in nbi_matviv_ee nbi_hacina21_ee {
	gen g0_A_`var'=(1/8)*`var' if adultos==1
	gen g0_AM_`var'=(1/8)*`var' if a_mayores==1
}

*Gaps de dimension SERVICIOS para los tres grupos
if "`ini'" == "ARG" {
	foreach var in nbi_agua_ee nbi_saneamiento_ee nbi_energia {
		gen g0_A_`var'=(1/12)*`var' if adultos==1
		gen g0_AM_`var'=(1/12)*`var' if a_mayores==1
	}
}
else{
	foreach var in nbi_tics_h nbi_agua_ee nbi_saneamiento_ee nbi_energia {
	gen g0_A_`var'=(1/16)*`var' if adultos==1
	gen g0_AM_`var'=(1/16)*`var' if a_mayores==1
	}
}

*Gaps del IPM Adultos
foreach var in nbi_conclued ocup_priv2{
	gen g0_A_`var'=(1/4)*`var' if adultos==1
}

*Gaps del IPM Adultos Mayores
foreach var in amay_analfab nbi_pen{
	gen g0_AM_`var'=(1/4)*`var' if a_mayores==1
}

* COUNTING VECTOR **************************************************************

egen c_A = rowtotal(g0_A_*)
replace c_A=. if adultos==0

egen c_AM = rowtotal(g0_AM_*)
replace c_AM=. if a_mayores==0

lab var c_A "Cantidad de Privaciones del IPM Adultos"
lab var c_AM "Cantidad de Privaciones del IPM Adultos Mayores"

tab	c_A [iw = _fep], miss
tab	c_AM [iw = _fep], miss

tab	c_A [iw = _fep]
tab	c_AM [iw = _fep]


* IDENTIFICATION **************************************************************
** Using different poverty cut-offs (i.e. different k)


foreach k of local cutoffs {
	gen	AM_PMD_`k' = (c_AM >= `k'/100 & c_AM!=.) if a_mayores==1
	lab var AM_PMD_`k' "Adulto Mayore es pobre multidimensional con k=`k'"

	gen	A_PMD_`k' = (c_A >= `k'/100 & c_A!=. )  if adultos==1
	lab var A_PMD_`k' "Adulto es pobre multidimensional con k=`k'"

}

* CENSORED COUNTING VECTOR *****************************************************
** Generate the censored vector of individual weighted deprivation count 'c(k)'

foreach k of local cutoffs {

	gen	cen_c_AM_`k' = c_AM
	replace cen_c_AM_`k' = 0 if AM_PMD_`k'==0 //Damos puntaje cero si el adulto mayor no es pobre con ese k//
	
	gen	cen_c_A_`k' = c_A
	replace cen_c_A_`k' = 0 if A_PMD_`k'==0 //Damos puntaje cero si el adulto no es pobre con ese k//
	
}


****ESTADISTICAS DE INTERES
*Tasa de IPM, M0 y A de cada IPM grupal

foreach k of local cutoffs {
	foreach var in A AM {
		sum cen_c_`var'_`k'  [iw=_fep]
		gen M0_`var'_`k'=r(mean)

		sum `var'_PMD_`k' [iw=_fep]
		gen H_`var'_`k'=r(mean)

		sum cen_c_`var'_`k'  [iw=_fep] if `var'_PMD_`k'==1
		gen A_`var'_`k'=r(mean)
	}
}


foreach k of local cutoffs {
	foreach var in A {
		**Mujeres
		sum cen_c_`var'_`k'  [iw=_fep] if sexo==2
		gen M0_`var'F_`k'=r(mean)

		sum `var'_PMD_`k' [iw=_fep] if sexo==2
		gen H_`var'F_`k'=r(mean)

		sum cen_c_`var'_`k'  [iw=_fep] if `var'_PMD_`k'==1 & sexo==2
		gen A_`var'F_`k'=r(mean)

		*Varones
		sum cen_c_`var'_`k'  [iw=_fep] if sexo==1
		gen M0_`var'V_`k'=r(mean)

		sum `var'_PMD_`k' [iw=_fep] if sexo==1
		gen H_`var'V_`k'=r(mean)

		sum cen_c_`var'_`k'  [iw=_fep] if `var'_PMD_`k'==1 & sexo==1
		gen A_`var'V_`k'=r(mean)
	}
}

foreach k of local cutoffs {
	label var M0_AM_`k' "IPM de Adultos Mayores"
	label var H_AM_`k' "Tasa de Pobreza Multidimensional de Adultos Mayores"
	label var A_AM_`k' "Intensidad de la Pobreza Multidimensional de Adultos Mayores"
}

noi di "`base'" _col(20) H_AF_35 _col(40) H_AV_35 _col(60) H_AM_35

}
}
