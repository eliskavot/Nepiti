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



