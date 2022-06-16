* DIISLI TARBIMISELASTSUSE LEIDMINE 
clear
cd "V:\FPO\1. Uurimisprojektid\Diisliaktsiisi langetuse mõjuanalüüs\Hinnaelastsused\Andmed"	//töökaust, siit võtab ja siia salvestab andmefailid, graafikud jms
import excel "Diisli_elastsus.xlsx", sheet("Statale") firstrow clear

*Loome ajatunnuse, mis on masinloetav
*Tuleb üle kontrollida, et oleks õige kvartal!!!
*Praegu algab 2003. aastaga
gen kvartal=jrk+171
*Anname programmile teada, et see on ajamuutuja
tsset kvartal, quarterly

tsline Diisel_S
tsline D_hind_ee
tsline Kuu_palk

gen hinnavahe_lv=D_hind_ee-D_hind_lv
gen hinnavahe_lt=D_hind_ee-D_hind_lt
gen dummy25=0
replace dummy25=1 if hinnavahe_lv>=25
replace dummy25=. if hinnavahe_lv==.
gen dummy5=0
replace dummy5=1 if hinnavahe_lv>=50
replace dummy5=. if hinnavahe_lv==.
gen dummy75=0
replace dummy75=1 if hinnavahe_lv>=75
replace dummy75=. if hinnavahe_lv==.
gen dummy10=0
replace dummy10=1 if hinnavahe_lv>=100
replace dummy10=. if hinnavahe_lv==.

*Logartmid
gen log_Diisel_S =ln(Diisel_S)
gen log_D_hind_ee =ln(D_hind_ee)
gen log_D_hind_ee2 =ln(D_hind_ee2)
gen log_B_hind_ee =ln(B_hind_ee)
gen log_D_hind_lv =ln(D_hind_lv)
gen log_B_hind_lv =ln(B_hind_lv)
gen log_D_hind_lt =ln(D_hind_lt)
gen log_B_hind_lt =ln(B_hind_lt)
gen log_Kuu_palk =ln(Kuu_palk)
gen et=Era_real+100
gen log_et =ln(et)
gen log_skpph=ln(SKP_ph)
gen log_dakt=ln(D_akt)

ac log_Diisel_S
pac log_Diisel_S
xi: ac D1.log_Diisel_S
xi: pac D1.log_Diisel_S
ac log_D_hind_ee
pac log_D_hind_ee
ac log_Kuu_palk
pac log_Kuu_palk

*Lihtne elastsus
xi: gen D_kasv=D1.Diisel_S/L1.Diisel_S
xi: gen H_kasv=D1.D_hind_ee/L1.D_hind_ee
xi: gen P_kasv=D1.Kuu_palk/L1.Kuu_palk

gen hinnaelastsus=D_kasv/H_kasv
gen palgaelastsus=D_kasv/P_kasv

list hinnaelastsus palgaelastsus

ameans hinnaelastsus palgaelastsus

xi: gen D_kasvy=D4.Diisel_S/L4.Diisel_S
xi: gen H_kasvy=D4.D_hind_ee/L4.D_hind_ee
xi: gen P_kasvy=D4.Kuu_palk/L4.Kuu_palk

gen hinnaelastsusy=D_kasvy/H_kasvy
gen palgaelastsusy=D_kasvy/P_kasvy

list hinnaelastsusy palgaelastsusy

ameans hinnaelastsusy palgaelastsusy

*Regressioonid
* välja visata viimane aasta
* mõelda, kuidas B_hind mõjutab
* aktsiisitõusu dummyd
xi: arima log_Diisel_S log_D_hind_ee          //mudel oluline, positiivne elastsus
xi: arima log_Diisel_S log_D_hind_ee2          //mudel pole oluline, positiivne elastsus
xi: arima log_Diisel_S log_Kuu_palk				//mudel oluline, positiivne elastsus
xi: arima log_Diisel_S log_D_hind_ee log_Kuu_palk		//mudel oluline, hinnaelastsus negatiivne, palgaelastsus positiivne
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk		//mudel pole oluline
xi: arima log_Diisel_S log_D_hind_ee log_Kuu_palk, ma(1/2)    		//mudel oluline, hinnaelastsus negatiivne, palgaelastsus positiivne
xi: arima log_Diisel_S log_D_hind_ee log_Kuu_palk jrk, ma(1/2)    		//mudel oluline, hinnaelastsus negatiivne, palgaelastsus positiivne, statistiliselt väheolulised
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk, ma(1/2)    		//mudel oluline, parameetrid mitte
xi: arima log_Diisel_S log_D_hind_ee log_Kuu_palk log_B_hind_ee		//mudel oluline
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk D1.log_B_hind_ee, ma(1/2)    		//mudel oluline, parameetrid õiged
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk D1.log_B_hind_ee D1.log_et, ma(1/2)    		//mudel oluline, parameetrid õiged
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk D1.log_B_hind_ee D1.log_skpph, ma(1/2)    		//mudel oluline, parameetrid õiged
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk D1.log_B_hind_ee D1.log_dakt, ma(1/2)    		//mudel oluline, parameetrid õiged
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk D1.log_B_hind_ee D1.log_dakt D1.log_et D1.log_skpph, ma(1/2)    		//mudel oluline, parameetrid õiged
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk D1.log_D_hind_lv, ma(1/2)    		//mudel oluline, parameetrid õiged
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk D1.log_D_hind_lt, ma(1/2)    		//mudel oluline, parameetrid õiged
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk D1.log_D_hind_lv D1.log_D_hind_lt, ma(1/2)    		//mudel oluline, parameetrid õiged
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk D1.hinnavahe_lv, ma(1/2)    		//mudel imelik
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk dummy25, ma(1/2)    		//mudel imelik
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk dummy5, ma(1/2)    		//mudel imelik
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk dummy75, ma(1/2)    		//mudel imelik
xi: arima D1.log_Diisel_S D1.log_D_hind_ee D1.log_Kuu_palk dummy10, ma(1/2)    		//mudel imelik



gen erinevus=log

*EGRANGER (kointegratsioon ja veaparandusmudel)
egranger log_Diisel_S log_D_hind_ee
egranger log_Diisel_S log_D_hind_ee, trend
egranger log_Diisel_S log_D_hind_ee, ecm
egranger log_Diisel_S log_D_hind_ee, trend ecm

egranger log_Diisel_S log_Kuu_palk
egranger log_Diisel_S log_Kuu_palk, trend
egranger log_Diisel_S log_D_hind_ee, ecm
egranger log_Diisel_S log_Kuu_palk, trend ecm
