* Encoding: UTF-8.
get file "c:\Users\jiri.vinopal\OneDrive - Filozofická fakulta, Univerzita Karlova\Výuka\Kursy\Výzkumný projekt\Alkohol\Úkoly\6_Datový soubor a technická zpráva\!orig\Alkohol 2025_v0.sav" .

save outfile "c:\Users\jiri.vinopal\OneDrive - Filozofická fakulta, Univerzita Karlova\Výuka\Kursy\Výzkumný projekt\Alkohol\Úkoly\6_Datový soubor a technická zpráva\SAV\Alkohol 2025_v01.sav".

get file "c:\Users\jiri.vinopal\OneDrive - Filozofická fakulta, Univerzita Karlova\Výuka\Kursy\Výzkumný projekt\Alkohol\Úkoly\6_Datový soubor a technická zpráva\SAV\Alkohol 2025_v01.sav".

use all.
split file off.
weight off.

* -------------------------------------------------------------------------------------------------------------------------------

fre all.


*** ---------------------------------------------------------------------------------------------------------------------------
*** ODSTRANĚNÍ REDUNDANTNÍCH PROMĚNNÝCH
*** ---------------------------------------------------------------------------------------------------------------------------

DELETE VARIABLES
    nQ66_1_0
    nQ68_1_0
    nQ70_1_0
    nQ72_1_0
    nQ74_1_0
    nQ76_1_0
    nQ80_1_0
    nQ81_1_0
    nQ82_1_0
    nQ83_1_0.


*** ---------------------------------------------------------------------------------------------------------------------------
*** kVANTIFIKACE POČTU "MÉNĚ NEŽ..." a ODSTRANĚNÍ REDUNDANTNÍCH PROMĚNNÝCH
*** ---------------------------------------------------------------------------------------------------------------------------

* ----------------------------------------------------------------------
* nQ12

recode nQ12_1_r (88 = 0.5) (90 = 0) (else = copy) into nQ12_1_rK.
recode nQ12_2_r (88 = 0.5) (90 = 0) (else = copy) into nQ12_2_rK.
recode nQ12_3_r (88 = 0.5) (90 = 0) (else = copy) into nQ12_3_rK.
recode nQ12_4_r (88 = 0.5) (90 = 0) (else = copy) into nQ12_4_rK.
exec.
     
Variable labels
nQ12_1_rK "Jak často za týden běžně pijete následující druhy nápojů? (R) [Pivo] - KVANTI"
nQ12_2_rK  "Jak často za týden běžně pijete následující druhy nápojů? (R) [Víno] - KVANTI"
nQ12_3_rK "Jak často za týden běžně pijete následující druhy nápojů? (R) [Tvrdý alkohol] - KVANTI"
nQ12_4_rK "Jak často za týden běžně pijete následující druhy nápojů? (R) [Ostatní druhy alkoholu] - KVANTI".

VALUE LABELS
nQ12_1_rK nQ12_2_rK  nQ12_3_rK  nQ12_4_rK 
0 "0 - Vůbec/Nikdy"
0.5 "0,5 - Méně než 1..."
89 "Neví / Nedokáže odhadnout".

cros nQ12_1_rK by nQ12_1_r.
cros nQ12_2_rK by nQ12_2_r.
cros nQ12_3_rK by nQ12_3_r.
cros nQ12_4_rK by nQ12_4_r.

Missing values
nQ12_1_rK nQ12_2_rK  nQ12_3_rK  nQ12_4_rK 
(89).


DELETE VARIABLES
tQ12_1_1
tQ12_2_1
tQ12_3_1
tQ12_4_1
nQ12_1_2
nQ12_1_3
nQ12_1_4
nQ12_2_2
nQ12_2_3
nQ12_2_4
nQ12_3_2
nQ12_3_3
nQ12_3_4
nQ12_4_2
nQ12_4_3
nQ12_4_4.

*  ----------------------------------------------------------------------
* nQ13

recode nQ13_1_r (888 = 0.5) (890 = 0) (else = copy) into nQ13_1_rK.
recode nQ13_2_r (888 = 0.5) (890 = 0) (else = copy) into nQ13_2_rK.
recode nQ13_3_r (888 = 0.5) (890 = 0) (else = copy) into nQ13_3_rK.
recode nQ13_4_r (888 = 0.5) (890 = 0) (else = copy) into nQ13_4_rK.
exec.
     
Variable labels
nQ13_1_rK	"	Jaký počet sklenic následujících druhů nápojů vypijete celkově za běžný týden? (R) [Pivo]	 - KVANTI"
nQ13_2_rK	"	Jaký počet sklenic následujících druhů nápojů vypijete celkově za běžný týden? (R) [Víno]	 - KVANTI"
nQ13_3_rK	"	Jaký počet sklenic následujících druhů nápojů vypijete celkově za běžný týden? (R) [Tvrdý alkohol]	 - KVANTI"
nQ13_4_rK	"	Jaký počet sklenic následujících druhů nápojů vypijete celkově za běžný týden? (R) [Ostatní druhy alkoholu]	 - KVANTI"..

VALUE LABELS
nQ13_1_rK	nQ13_2_rK	nQ13_3_rK	nQ13_4_rK
0 "0 - Vůbec/Nikdy"
0.5 "0,5 - Méně než 1..."
889 "Neví / Nedokáže odhadnout".

cros	nQ13_1_rK	by	nQ13_1_r	.
cros	nQ13_2_rK	by	nQ13_2_r	.
cros	nQ13_3_rK	by	nQ13_3_r	.
cros	nQ13_4_rK	by	nQ13_4_r	.

Missing values
nQ13_1_rK	nQ13_2_rK	nQ13_3_rK	nQ13_4_rK
(889).

DELETE VARIABLES
tQ13_1_1
tQ13_2_1
tQ13_3_1
tQ13_4_1
nQ13_1_2
nQ13_1_3
nQ13_2_2
nQ13_2_3
nQ13_3_2
nQ13_3_3
nQ13_4_2
nQ13_4_3.

 ----------------------------------------------------------------------
* tQ28 tQ29

MISSING VALUES tQ28_0_1  tQ29_0_1 ( ).

fre tQ28_0_1 tQ29_0_1.

recode tQ28_0_1 (88 = 0.5) (else = copy) into tQ28_0_1K.
recode tQ29_0_1 (88 = 0.5) (else = copy) into tQ29_0_1K.
exec.
     
Variable labels
tQ28_0_1K	"	Jak časté u Vás tyto situace jsou? [Počet za týden)	 - KVANTI"
tQ29_0_1K	"	Jak časté u Vás tyto situace jsou? [Počet za měsíc)	 - KVANTI".

VALUE LABELS
tQ28_0_1K	tQ29_0_1K
0 "0 - Vůbec/Nikdy"
0.5 "0,5 - Méně než 1..."
99 "Neví / Nedokáže odhadnout".

cros	tQ28_0_1K	by	tQ28_0_1	.
cros	tQ29_0_1K	by	tQ29_0_1	.

Missing values
tQ28_0_1K	tQ29_0_1K ( 99 ).



* ----------------------------------------------------------------------
* nQ32

recode nQ32_1_r (888 = 0.5) (890 = 0) (else = copy) into nQ32_1_rK.
recode nQ32_2_r (888 = 0.5) (890 = 0) (else = copy) into nQ32_2_rK.
recode nQ32_3_r (888 = 0.5) (890 = 0) (else = copy) into nQ32_3_rK.
recode nQ32_4_r (888 = 0.5) (890 = 0) (else = copy) into nQ32_4_rK.
exec.
     
Variable labels
nQ32_1_rK	"	Jaký počet sklenic následujících druhů nápojů při jedné takovéto příležitosti obvykle vypijete? (R) [Pivo]	 - KVANTI"
nQ32_2_rK	"	Jaký počet sklenic následujících druhů nápojů při jedné takovéto příležitosti obvykle vypijete? (R) [Víno]	 - KVANTI"
nQ32_3_rK	"	Jaký počet sklenic následujících druhů nápojů při jedné takovéto příležitosti obvykle vypijete? (R) [Tvrdý alkohol]	 - KVANTI"
nQ32_4_rK	"	Jaký počet sklenic následujících druhů nápojů při jedné takovéto příležitosti obvykle vypijete? (R) [Ostatní druhy alkoholu]	 - KVANTI"..

VALUE LABELS
nQ32_1_rK	nQ32_2_rK	nQ32_3_rK	nQ32_4_rK
0 "0 - Vůbec/Nikdy"
0.5 "0,5 - Méně než 1..."
889 "Neví / Nedokáže odhadnout".

cros	nQ32_1_rK	by	nQ32_1_r	.
cros	nQ32_2_rK	by	nQ32_2_r	.
cros	nQ32_3_rK	by	nQ32_3_r	.
cros	nQ32_4_rK	by	nQ32_4_r	.

Missing values
nQ32_1_rK	nQ32_2_rK	nQ32_3_rK	nQ32_4_rK
(889).

DELETE VARIABLES
tQ32_1_1
tQ32_2_1
tQ32_3_1
tQ32_4_1
nQ32_2_2
nQ32_2_3
nQ32_2_4
nQ32_3_2
nQ32_3_3
nQ32_3_4
nQ32_4_2
nQ32_4_3
nQ32_4_4
nQ32_1_2
nQ32_1_3
nQ32_1_4.



*  ----------------------------------------------------------------------
* tQ46 tQ47

MISSING VALUES tQ46_0_1  tQ47_0_1 ( ).

fre tQ46_0_1 tQ47_0_1.

recode tQ46_0_1 (88 = 0.5) (else = copy) into tQ46_0_1K.
recode tQ47_0_1 (88 = 0.5) (else = copy) into tQ47_0_1K.
exec.
     
Variable labels
tQ46_0_1K	"	Jak časté u Vás tyto situace jsou? [Počet za týden)	 - KVANTI"
tQ47_0_1K	"	Jak časté u Vás tyto situace jsou? [Počet za měsíc)	 - KVANTI".

VALUE LABELS
tQ46_0_1K	tQ47_0_1K
0 "0 - Vůbec/Nikdy"
0.5 "0,5 - Méně než 1..."
99 "Neví / Nedokáže odhadnout".

cros	tQ46_0_1K	by	tQ46_0_1	.
cros	tQ47_0_1K	by	tQ47_0_1	.

Missing values
tQ46_0_1K	tQ47_0_1K ( 99 ).


* ----------------------------------------------------------------------
* nQ50

recode nQ50_1_r (888 = 0.5) (890 = 0) (else = copy) into nQ50_1_rK.
recode nQ50_2_r (888 = 0.5) (890 = 0) (else = copy) into nQ50_2_rK.
recode nQ50_3_r (888 = 0.5) (890 = 0) (else = copy) into nQ50_3_rK.
recode nQ50_4_r (888 = 0.5) (890 = 0) (else = copy) into nQ50_4_rK.
exec.
     
Variable labels
nQ50_1_rK	"	Jaký počet sklenic následujících druhů nápojů při jedné takovéto příležitosti obvykle vypijete? (R) [Pivo]	 - KVANTI"
nQ50_2_rK	"	Jaký počet sklenic následujících druhů nápojů při jedné takovéto příležitosti obvykle vypijete? (R) [Víno]	 - KVANTI"
nQ50_3_rK	"	Jaký počet sklenic následujících druhů nápojů při jedné takovéto příležitosti obvykle vypijete? (R) [Tvrdý alkohol]	 - KVANTI"
nQ50_4_rK	"	Jaký počet sklenic následujících druhů nápojů při jedné takovéto příležitosti obvykle vypijete? (R) [Ostatní druhy alkoholu]	 - KVANTI"..

VALUE LABELS
nQ50_1_rK	nQ50_2_rK	nQ50_3_rK	nQ50_4_rK
0 "0 - Vůbec/Nikdy"
0.5 "0,5 - Méně než 1..."
889 "Neví / Nedokáže odhadnout".

cros	nQ50_1_rK	by	nQ50_1_r	.
cros	nQ50_2_rK	by	nQ50_2_r	.
cros	nQ50_3_rK	by	nQ50_3_r	.
cros	nQ50_4_rK	by	nQ50_4_r	.

Missing values
nQ50_1_rK	nQ50_2_rK	nQ50_3_rK	nQ50_4_rK
(889).


DELETE VARIABLES
tQ50_1_1
tQ50_2_1
tQ50_3_1
tQ50_4_1
nQ50_1_2
nQ50_2_2
nQ50_3_2
nQ50_4_2
nQ50_1_3
nQ50_2_3
nQ50_3_3
nQ50_4_3
nQ50_1_4
nQ50_2_4
nQ50_3_4
nQ50_4_4.



*  ----------------------------------------------------------------------
* tQ84_r tQ85_r

MISSING VALUES tQ84_0_1  tQ85_0_1 ( ).

fre tQ84_r tQ85_r.

recode tQ84_r (888 = 0.5) (889 = 0) (else = copy) into tQ84_rK.
recode tQ85_r (888 = 0.5) (889 = 0) (else = copy) into tQ85_rK.
exec.
     
Variable labels
tQ84_rK	" Kolikrát v průměru do měsíce se při pití alkoholu cítíte v povznesené náladě nebo lehounce opilý/á? (R) - KVANTI"
tQ85_rK	" Kolikrát v průměru do měsíce se po pití alkoholu cítíte silně opilý/á? (R) - KVANTI".

VALUE LABELS
tQ84_rK	tQ85_rK
0 "0 - Vůbec/Nikdy"
0.5 "0,5 - Méně než 1..."
890 "Neví / Nedokáže odhadnout".

cros	tQ84_rK	by	tQ84_r	.
cros	tQ85_rK	by	tQ85_r	.

Missing values
tQ84_rK	tQ85_rK ( 890 ).

DELETE VARIABLES
tQ84_0_0
nQ84_1_0
nQ84_2_0
nQ84_3_0
tQ85_0_0
nQ85_1_0
nQ85_2_0
nQ85_3_0.




*** ---------------------------------------------------------------------------------------------------------------------------
*** ABSTINENCE
*** ---------------------------------------------------------------------------------------------------------------------------

fre tQ02_0_0.
cros tQ02_0_0 nQ02_1_0 BY nQ01_r1.

* doplnění informace "nikdy nepil/a" do proměnné s délkou abstinence

if nQ02_1_0 = 1 tQ02_0_0 = 8888.
exec.
value labels tQ02_0_0 
    8888 "Nikdy nepil/a".
fre tQ02_0_0.


*** ---------------------------------------------------------------------------------------------------------------------------
*** VZDĚLÁNÍ    
*** ---------------------------------------------------------------------------------------------------------------------------

fre nQ91_r1.
recode nQ91_r1
(	1	thru 	2	 = 	1	)
(	3	thru 	4	 = 	2	)
(	5	thru 	6	 = 	3	)
(	7	thru 	8	 = 	4	)
(	9                      thru                10	 = 	5	)
(	else 	= 	sysmis	)
into vzd5.
EXECUTE.

VARIABLE LABELS vzd5 "vzd5 Vzdělání (5 kat)".
VALUE LABELS
/vzd5
1 "Základní"
2 "Střední bez maturity"
3 "Střední s maturitou"
4 "Vyšší odborné a bakalářské"
5 "Vysokoškolské".

fre vzd5.
cros nQ91_r1 by vzd5.

* ----------------------------------------------

fre nQ91_r1.
recode nQ91_r1
(	1	thru 	2	 = 	1	)
(	3	thru 	4	 = 	2	)
(	5	thru 	6	 = 	3	)
(	7	thru 	10	 = 	4	)
(	else 	= 	sysmis	)
into vzd4.
EXECUTE.

VARIABLE LABELS vzd4 "vzd4 Vzdělání (4 kat)".
VALUE LABELS
/vzd4
1 "Základní"
2 "Střední bez maturity"
3 "Střední s maturitou"
4 3 "VOŠ, Bc. a VŠ".

fre vzd4.
cros nQ91_r1 by vzd4.

* ----------------------------------------------

recode nQ91_r1
(	1	thru 	4	 = 	1	)
(	5	thru 	6	 = 	2	)
(	7	thru 	10	 = 	3	)
(	else 	= 	sysmis	)
into vzd3.
EXECUTE.

VARIABLE LABELS vzd3 "vzd3 Vzdělání (3 kat)".
VALUE LABELS
/vzd3
1 "ZŠ a SŠ bez maturity"
2 "SŠ s maturitou"
3 "VOŠ, Bc. a VŠ".

fre vzd3.
cros nQ91_r1 by vzd3.


*** ---------------------------------------------------------------------------------------------------------------------------
*** VĚK 
*** ---------------------------------------------------------------------------------------------------------------------------

fre tQ89_0_0.

recode tQ89_0_0
(	18	thru 	29	 = 	1	)
(	30	thru 	39	 = 	2	)
(	40	thru 	49	 = 	3	)
(	50	thru 	59	 = 	4	)
(	60	thru 	69	 = 	5	)
(	70	thru 	highest	 = 	6	)
(else = copy)
into vek6.
VARIABLE LABELS vek6 "Věk (6 kat)".
VALUE LABELS
/vek6
1 "18  -  29 "
2 "30  -  39 "
3 "40  -  49 "
4 "50  -  59 "
5 "60 - 69 "
6 "70+".

fre vek6.
cros tQ89_0_0 by vek6.

    

*** ---------------------------------------------------------------------------------------------------------------------------
*** EKONOMICKÁ AKTIVITA
*** ---------------------------------------------------------------------------------------------------------------------------

fre nQ96_r1.

If nQ96_r1 > 4 AND nQ96_r1 < 12 ea2 = 1.
If nQ96_r1 > 0 AND nQ96_r1 < 5   ea2 = 2.
exec.
VARIABLE LABELS ea2 "Ekonomická aktivita (ano/ne)".
VALUE LABELS
/ea2
1 "Ekonomicky aktivní"
2 "Ekonomicky neaktivní".

fre ea2.
cros nQ96_r1 by ea2.


*** ---------------------------------------------------------------------------------------------------------------------------
*** OSOBNÍ PŘÍJEM
*** ---------------------------------------------------------------------------------------------------------------------------

fre tQ98_0_0.

* oprava hodnot

missing values tQ98_0_0 (999999).
    
if tQ98_0_0 = 	12	tQ98_0_0 =	12000	.
if tQ98_0_0 = 	16	tQ98_0_0 =	16000	.
if tQ98_0_0 = 	20	tQ98_0_0 =	20000	.
if tQ98_0_0 = 	22	tQ98_0_0 =	22000	.
if tQ98_0_0 = 	25	tQ98_0_0 =	25000	.
if tQ98_0_0 = 	26	tQ98_0_0 =	26000	.
if tQ98_0_0 = 	28	tQ98_0_0 =	28000	.
if tQ98_0_0 = 	29	tQ98_0_0 =	29000	.
if tQ98_0_0 = 	35	tQ98_0_0 =	35000	.
if tQ98_0_0 = 	888888	tQ98_0_0 =	0	.
exec.

fre tQ98_0_0.

* kategorizace 

recode tQ98_0_0
(	0	thru 	11300	 = 	1	)
(	11301	thru 	21000	 = 	2	)
(	21001	thru 	30000             = 	3	)
(	30001	thru 	40000             = 	4	)
(	40001	thru 	60000	 = 	5	)
(	else                                                 = 	SYSMIS        )
into prijem_osob.
VARIABLE LABELS prijem_osob "Osobní příjem (15 - 20 - 30 - 20 - 15)".
VALUE LABELS
/prijem_osob
1 "Nejnižší (cca 15 %)"
2 "Podprůměrný (cca 20 %)"
3 "Průměrný (cca 30 %)"
4 "Nadprůměrný (cca 20 %)"
5 "Nejvyšší (cca 15 %)".
fre prijem_osob.
cros tQ98_0_0 by prijem_osob.


*** ---------------------------------------------------------------------------------------------------------------------------
*** PŘÍJEM DOMÁCNOSTI
*** ---------------------------------------------------------------------------------------------------------------------------

fre tQ95_0_0.

* oprava hodnot
  
if tQ95_0_0= 	20	tQ95_0_0=	20000	.
if tQ95_0_0= 	29	tQ95_0_0=	29000	.
if tQ95_0_0= 	34	tQ95_0_0=	34000	.
if tQ95_0_0= 	40	tQ95_0_0=	40000	.
if tQ95_0_0= 	41	tQ95_0_0=	41000	.
if tQ95_0_0= 	43	tQ95_0_0=	43000	.
if tQ95_0_0= 	44	tQ95_0_0=	44000	.
if tQ95_0_0= 	45	tQ95_0_0=	45000	.
if tQ95_0_0= 	50	tQ95_0_0=	50000	.
if tQ95_0_0= 	51	tQ95_0_0=	51000	.
if tQ95_0_0= 	55	tQ95_0_0=	55000	.
if tQ95_0_0= 	58	tQ95_0_0=	58000	.
if tQ95_0_0= 	90	tQ95_0_0=	90000	.
if tQ95_0_0= 	2000000	tQ95_0_0=	200000.
if tQ95_0_0= 	888888	tQ95_0_0=	0.
exec.

fre tQ95_0_0.


*** ---------------------------------------------------------------------------------------------------------------------------
*** ČLENOVÉ DOMÁCNOSTI
*** ---------------------------------------------------------------------------------------------------------------------------

* oprava hodnot

fre tQ107_0_0.
   
if tQ107_0_0= 	0	tQ107_0_0=	1	.
if tQ107_0_0= 	199	tQ107_0_0=	1	.
exec.

fre tQ107_0_0.

cros tQ102_0_0 by tQ107_0_0.


*** ---------------------------------------------------------------------------------------------------------------------------
*** DĚTI
*** ---------------------------------------------------------------------------------------------------------------------------

* Dichotomizované proměnné má/nemá dítě v daném věku.

fre tQ101_0_1 tQ101_1_1 tQ101_2_1 tQ101_3_1.

compute 	tQ101_0_1D	 =	tQ101_0_1	.
compute 	tQ101_1_1D	 =	tQ101_1_1	.
compute 	tQ101_2_1D	 =	tQ101_2_1	.
compute 	tQ101_3_1D	 =	tQ101_3_1	.
exec.

if	tQ101_0_1	> 0 	tQ101_0_1D	 = 1.
if	tQ101_1_1	> 0 	tQ101_1_1D	 = 1.
if	tQ101_2_1	> 0 	tQ101_2_1D	 = 1.
if	tQ101_3_1	> 0 	tQ101_3_1D	 = 1.
exec.

VARIABLE LABELS
tQ101_0_1D	"	Má děti ve věku:: [0 - 5 let]	                        "
tQ101_1_1D	"	Má děti ve věku:: [6 - 14 let]	"
tQ101_2_1D	"	Má děti ve věku:: [15 – 17 let]	"
tQ101_3_1D	"	Má děti ve věku:: [18 a více let]	".

VALUE LABELS 
tQ101_0_1D tQ101_1_1D tQ101_2_1D tQ101_3_1D
0 "Nemá děti v tomto věku"
1 "Má děti v tomto věku".

cros	tQ101_0_1	by	tQ101_0_1D	.
cros	tQ101_1_1	by	tQ101_1_1D	.
cros	tQ101_2_1	by	tQ101_2_1D	.
cros	tQ101_3_1	by	tQ101_3_1D	.


* Celkový počet dětí

compute tQ101_celk = SUM(tQ101_0_1, tQ101_1_1, tQ101_2_1, tQ101_3_1).
VARIABLE LABELS tQ101_celk "Celkový počet dětí".
fre tQ101_celk.


*** ---------------------------------------------------------------------------------------------------------------------------
*** VĚK A ZKUŠENOSTI S ALKOHOLEM
*** ---------------------------------------------------------------------------------------------------------------------------

fre tQ80_0_0 tQ82_0_0.

cros tQ80_0_0 tQ82_0_0 by tQ89_0_0.

USE ALL.
COMPUTE filter_$=(tQ89_0_0 = 20 & tQ82_0_0 = 30).
VARIABLE LABELS filter_$ 'tQ89_0_0 = 20 & tQ82_0_0 = 30 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FRE respondent_id_internal.
* 21799238

* oprava hodnoty

if tQ82_0_0 = 30 tQ82_0_0 = 3.

DELETE VARIABLES filter_$.
USE ALL.




*** ---------------------------------------------------------------------------------------------------------------------------
*** CELKOVÁ SPOTŘEBA  ALKOHOLU - v1
*** ---------------------------------------------------------------------------------------------------------------------------

* Encoding: UTF-8.
COMPUTE celk_spotreba=SUM(tQ13_1_1,tQ13_2_1,tQ13_3_1,tQ13_4_1).
EXECUTE.
DO IF  (MISSING(celk_spotreba)=1).
RECODE nQ13_1_2 nQ13_2_2 nQ13_3_2 nQ13_4_2 (1=0) INTO celk_spotreba celk_spotreba celk_spotreba celk_spotreba.
END IF.
VARIABLE LABELS  celk_spotreba 'Celková týdenní spotřeba alkoholu (v sklenicích)'.
EXECUTE.
VALUE LABELS celk_spotreba 0 'Méně než 1 týdně'.
*Syntax nahore vypocita kardinalni promennou celkove spotreby za tyden v sklenicich. 
*Odpoved mene nez jednou tydne je zakodovana jako 0
*Lidi, kteri nevedi kolik vypiji alkoholu jsou jako missing, je ich celkem 58. 
*V tabulkach od nms jsou ti co nevedi pripocteni ke kategorii mene nez jednou tydne, mne to prislo jako chybne, protoze lidi co nevedi muzou vypit jak malo tak i hodne alkoholu za tyden.

*Tato cast skriptu premeni spocitanou spotrebu na kategorialni promennou se 4 kategoriemi.
DATASET ACTIVATE DataSet1.
RECODE celk_spotreba (0=1) (1 thru 5=2) (6 thru 10=3) (11 thru Highest=4) INTO celk_spotreba_kat.
VARIABLE LABELS  celk_spotreba_kat 'Celková týdenní spotřeba alkoholu v kategoriích'.
EXECUTE.
VALUE LABELS celk_spotreba_kat 1 'Méně než 1 týdně' 2 '1–5 sklenic' 3 '6–10 sklenic' 4 'více než 10 sklenic'.



*** ---------------------------------------------------------------------------------------------------------------------------
*** CELKOVÁ SPOTŘEBA  ALKOHOLU - v2
*** ---------------------------------------------------------------------------------------------------------------------------

* alternativní výpočet a kategorizace - započítává jako 0,5 i odpovědi "méně než 1"
* je otázka, co udělat, když na u některého nápoje respondent neví - lze započítat jen ty, u nichž ví, nebo ho z výpočtu úplně vyřadit...

MISSING VALUES nQ13_1_rK nQ13_2_rK nQ13_3_rK nQ13_4_rK ( 889 ).
fre nQ13_1_rK nQ13_2_rK nQ13_3_rK nQ13_4_rK.

COMPUTE celk_spotr2=SUM(nQ13_1_rK, nQ13_2_rK, nQ13_3_rK, nQ13_4_rK).
VARIABLE LABELS celk_spotr2 "Celková spotřeba (sklenice za týden)".
FRE celk_spotr2.

RECODE celk_spotr2 (0 thru 0.5 = 1) (1 thru 2=2) (3 thru 5=3) (6 thru 9 = 4) (10 thru Highest=5) INTO celk_spotr2_5kat.
VARIABLE LABELS celk_spotr2_5kat "Celková spotřeba (sklenice za týden) 5kat".
VALUE LABELS celk_spotr2_5kat 
    1 "0- 0,5"
    2 "1 - 2"
    3 "3 - 5"
    4 "6 - 9"
    5 "10+".
fre celk_spotr2_5kat.

*** ---------------------------------------------------------------------------------------------------------------------------
*** CELKOVÁ SPOTŘEBA  ALKOHOLU - v3
*** ---------------------------------------------------------------------------------------------------------------------------

* upravili jsme tak, aby kategorizace zahrnovala i respondenty, kteří mají v celkovém součtu "půl nápoje".

RECODE celk_spotr2 (0 thru 0.5 = 1) (1 thru 2=2) (2.5 thru 5=3) (5.5 thru 9.5 = 4) (10 thru Highest=5) INTO celk_spotr3_5kat.
VARIABLE LABELS celk_spotr3_5kat "Celková spotřeba (sklenice za týden) 5kat - upraveno".
VALUE LABELS celk_spotr3_5kat 
    1 "0- 0,5"
    2 "1 - 2"
    3 "2,5 - 5"
    4 "5,5 - 9,5"
    5 "10+".
fre celk_spotr2_5kat.
fre celk_spotr3_5kat.

*** ---------------------------------------------------------------------------------------------------------------------------
*** ELIMINACE ABSTINENTŮ
*** ---------------------------------------------------------------------------------------------------------------------------

FILTER OFF.
USE ALL.
SELECT IF (status = "FINISHED").
EXECUTE.




*** ---------------------------------------------------------------------------------------------------------------------------
*** ---------------------------------------------------------------------------------------------------------------------------
*** ---------------------------------------------------------------------------------------------------------------------------

save outfile "c:\Users\jiri.vinopal\OneDrive - Filozofická fakulta, Univerzita Karlova\Výuka\Kursy\Výzkumný projekt\Alkohol\Úkoly\6_Datový soubor a technická zpráva\SAV\Alkohol 2025_v01.sav".





