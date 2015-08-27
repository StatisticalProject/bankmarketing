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
proc surveyselect data=bankmarketing
   method=srs n=1000 out=bankmarketingSelect;
   
run;
data bankmarketingSelect;
merge bankmarketingSelect;
seqno = _n_;
run;
PROC FREQ DATA=bankmarketing;
  TABLES age ageCat;
RUN;
PROC FREQ DATA=bankmarketing;
  TABLES pdays pdaysCat;
RUN;	
PROC FREQ DATA=bankmarketing;
  TABLES nrEmployed;
RUN;		
PROC FREQ DATA=bankmarketing;
  TABLES consConfIdx;
RUN;		

title 'Stepwise Regression on Cancer Remission Data';
   proc logistic data=bankmarketing outest=betas covout;
      class  job marital education default housing loan contact month day_of_week  previous poutcome pdaysCat ageCat/param=glm ;
      model y(event='yes')=duration pdaysCat ageCat age job marital education default housing loan contact month day_of_week previous poutcome empVarRate consPriceIdx  euribor3m nrEmployed / link=logit outroc=roc;
       output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;
   proc logistic data=bankmarketing outest=betas covout;
      class  job marital education default housing loan contact month day_of_week  previous poutcome pdaysCat ageCat/param=glm ;
      model y(event='yes')=duration pdaysCat ageCat job marital education default housing loan contact month day_of_week previous poutcome empVarRate consPriceIdx  euribor3m nrEmployed / link=logit outroc=roc;
       output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;
proc logistic data=bankmarketing outest=betas covout;
      class  job marital education default housing loan contact month day_of_week  previous poutcome pdaysCat ageCat/param=glm ;
      model y(event='yes')= pdaysCat ageCat job marital education default housing loan contact month day_of_week previous poutcome empVarRate consPriceIdx  euribor3m nrEmployed / link=logit selection=stepwise details lackfit outroc=roc;
       output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;  
    

   proc logistic data=bankmarketing outest=betas covout;
      class  job  contact month day_of_week poutcome ageCat pdaysCat/param=glm ;
      model y(event='yes')=duration job ageCat pdaysCat  contact month day_of_week poutcome empVarRate consPriceIdx  euribor3m / link=logit ctable pprob = (0 to 1 by 0.025) outroc=roc;
       output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;
   proc logistic data=bankmarketing outest=betas covout;
      class  job  contact month day_of_week poutcome ageCat pdaysCat/param=glm ;
      model y(event='yes')=job ageCat pdaysCat  contact month day_of_week poutcome empVarRate consPriceIdx  euribor3m / link=logit ctable pprob = (0 to 1 by 0.025) outroc=roc;
       output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;
   proc plot data=pred;
plot (phat lcl ucl)*duration / overlay;
run;

proc logistic data=bankmarketing outest=betas covout;
      class  job contact month day_of_week poutcome/param=glm ;
      model y(event='yes')=duration job  contact month day_of_week poutcome empVarRate consPriceIdx  euribor3m / link=logit outroc=roc selection=stepwise details lackfit;
       output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;
proc logistic data=bankmarketing outest=betas covout;
      class  job marital education default housing loan contact month day_of_week campaign  previous poutcome /param=glm ;
      model y=duration age job marital education default housing loan contact month day_of_week campaign  pdays previous poutcome empVarRate consPriceIdx consConfIdx euribor3m nrEmployed / link=logit outroc=roc;
       output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;
   
PROC TRANSREG DATA = bankmarketingSelect
DESIGN NOPRINT ;

MODEL CLASS (ageCat pdaysCat job  contact month day_of_week poutcome) ;

OUTPUT OUT = TableauDisjonctifComplet ;

RUN ;

   PROC corresp
 data=TableauDisjonctifComplet
 outc=corresp (WHERE = (_TYPE_='OBS'))
 DIMENS=5;
 VAR &_trgind;
run; 

data correspN;
merge corresp;
seqno = _n_;
run;
data bankmarketingDiscrim;
   merge correspN bankmarketingSelect;
   by seqno;
run;
proc discrim  data=bankmarketingDiscrim method=normal pool=yes list crossvalidate;
class y;
var Dim1--Dim5;
run;

proc discrim  data=bankmarketingDiscrim method=npar  K=3 pool=yes list crossvalidate;
class y;
var Dim1--Dim5;
run;

ods rtf file='resultat.rtf' style=journal;
ods graphics on;

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

/* Modèle complet */
proc logistic data=bankmarketing outest=betas covout;
      class job marital education default housing loan contact month day_of_week  pdaysCat poutcome/param=glm ;
      model y(event='yes')=age job marital education default housing loan contact month day_of_week campaign pdaysCat poutcome/ link=logit outroc=roc;
       output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;

/* Selection de variable */
   proc logistic data=bankmarketing outest=betas covout;
      class job marital education default housing loan contact month day_of_week  pdaysCat poutcome/param=glm ;
      model y(event='yes')=age job marital education default housing loan contact month campaign pdaysCat poutcome/ link=logit outroc=roc selection=stepwise details lackfit;
       output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;
   /* Selection de variable avec effet du second ordre*/
   proc logistic data=bankmarketing outest=betas covout;
      class job marital education default housing loan contact month day_of_week  pdaysCat poutcome/param=glm ;
      model y(event='yes')= day_of_week month*day_of_week contact*poutcome pdaysCat*contact age job marital education default housing loan contact month campaign pdaysCat poutcome/ link=logit outroc=roc selection=stepwise details lackfit;
       output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;

ods rtf close;
