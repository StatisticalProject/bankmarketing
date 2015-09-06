/*Chargement des données*/
DATA bankmarketing;
    infile "bank-additional-full.csv"  firstobs=2 delimiter=';';
    INPUT  age job $ marital $ education $ default $ housing $ loan $ contact $ month $ day_of_week $ duration campaign pdays  previous poutcome $ empVarRate consPriceIdx consConfIdx euribor3m nrEmployed y $
	;
	if education = "unknown" then delete;
	if marital = "unknown" then delete;
	if default = "unknown" then delete;
	if job = "unknown" then delete;
	if housing = "unknown" then delete;
	if loan = "unknown" then delete;

	campaignCat = "  ";
  	IF (campaign<2) THEN campaignCat = "1";
    IF (campaign=2) THEN campaignCat = "2";
    IF (campaign=3) THEN campaignCat = "3";
    IF (campaign>3) THEN campaignCat = ">3";

	ageCat = "     ";
  	IF (age<31) THEN ageCat = "<31";
    IF (age>30) and (age<35) THEN ageCat = "31-34";
    IF (age>34) and (age<40) THEN ageCat = "35-39";
    IF (age>39) and (age<48) THEN ageCat = "40-47";
	IF (age>47) THEN ageCat = ">47";
	pdaysCat = "    ";
  	IF (pdays<4) THEN pdaysCat = "<4";
    IF (pdays>3) and (pdays<7) THEN pdaysCat = "4-6";
    IF (pdays>6) and (pdays<999) THEN pdaysCat = ">7";
    IF (pdays=999) THEN pdaysCat = "999";

    yInt=0;
	IF (y="yes") THEN yInt = 1;
	IF (y="no") THEN yInt = 0;
RUN; 
/*Données d'apprentissage et de validation*/
Proc Surveyselect data=bankmarketing seed=7 out=bankmarketingSplit samprate=.7 outall;
Run;

Data bktraining bkvalidation;
Set bankmarketingSplit;
if selected = 1 then output bktraining;
else output bkvalidation;
Run;

/*Analyse univariee*/
/* Données numériques */
proc univariate data=bankmarketing plots;
   var age duration empVarRate consPriceIdx consConfIdx euribor3m nrEmployed;
run;
/* Les ages par y */
PROC SGPLOT DATA=bankmarketing ;
  VBOX age /category = y;
RUN ;

/* Données catégorielles */
proc freq data=bankmarketing;
tables y job marital education default housing loan contact month day_of_week campaign pdays previous poutcome/ plots=freqplot;
run; 

/* Analyse des effets de chaque variable sur y */
proc freq data=bankmarketing;
tables (job marital education default housing loan contact month day_of_week campaign pdays previous poutcome)*y /   NOCUM NOFREQ NOSPARSE NOWARN;
run; 
/* description des categories de pdaysCat */ 
proc freq data=bankmarketing ;
tables pdaysCat ;

run;

/* Modèle complet avec contrast */
proc logistic Data = bktraining ; 
      class job marital education default housing loan 
       contact month day_of_week  pdaysCat poutcome /PARAM=effect ;
      model y(event='yes')= age job marital education default 
       housing loan contact month day_of_week previous 
       campaign pdaysCat poutcome/ ctable link=logit 
       RIDGING=ABSOLUTE outroc=roc;
      output out=pred p=phat lower=lcl upper=ucl
       predprob=(individual crossvalidate);
	  score data=bkvalidation out = Logit_File outroc=vroc;
	  contrast 'student vs retired' job 0 0 0 0 0 1 0 0 -1 0 0;
	  contrast 'divorced vs married' marital 1 -1 0;
	  contrast 'illetra vs universi' education 0 0 0 0 1 0 -1;
	
run;

/* Selection de variable */
proc logistic Data = bktraining ; 
      class job marital education default housing loan 
       contact month day_of_week  pdaysCat poutcome  ;
      model y(event='yes')= age job marital education default 
       housing loan contact month day_of_week previous 
       campaign pdaysCat poutcome/ ctable link=logit 
       RIDGING=ABSOLUTE outroc=roc  selection=forward 
       details lackfit;
      output out=pred p=phat lower=lcl upper=ucl
       predprob=(individual crossvalidate);
	  score data=bkvalidation out = Logit_File outroc=vroc;
run;




ods rtf close;
