DATA bankmarketing;
    infile "bank-additional-full.csv" delimiter=';';
    INPUT  age job $ marital $ education $ default $ housing $ loan $ contact $ month $ day_of_week $ duration campaign	$ pdays  previous $ poutcome $ empVarRate consPriceIdx consConfIdx euribor3m nrEmployed y $
	;
	if education = "unknown" then delete;
	if marital = "unknown" then delete;
	if default = "unknown" then delete;
	if job = "unknown" then delete;
	if housing = "unknown" then delete;
	if loan = "unknown" then delete;
;
RUN; 
title 'Stepwise Regression on Cancer Remission Data';
   proc logistic data=bankmarketing outest=betas covout;
      class  job marital education default housing loan contact month day_of_week  previous poutcome/param=glm ;
      model y(event='yes')=duration age job marital education default housing loan contact month day_of_week previous poutcome empVarRate consPriceIdx  euribor3m nrEmployed / link=logit outroc=roc;
       output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;

   proc logistic data=bankmarketing outest=betas covout;
      class  job  contact month day_of_week poutcome/param=glm ;
      model y(event='yes')=duration job   contact month day_of_week poutcome empVarRate consPriceIdx  euribor3m / link=logit ctable pprob = (0 to 1 by 0.025) outroc=roc;
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
   
PROC TRANSREG DATA = bankmarketing
DESIGN NOPRINT ;

MODEL CLASS (job  contact month day_of_week poutcome) ;

OUTPUT OUT = TableauDisjonctifComplet ;

RUN ;
proc corresp mca  data=TableauDisjonctifComplet outc=Coor outf=freqs dimens=27;
      tables job  contact month day_of_week poutcome;
   run;

   PROC corresp
 data=TableauDisjonctifComplet
 outc=corresp (WHERE = (_TYPE_='OBS'))
 DIMENS=5;
 VAR &_trgind;
run; 
proc corresp  data=TableauDisjonctifComplet outc=Coor (WHERE = (_TYPE_='OBS'))  outf=freqs dimens=27;
      tables job  contact month day_of_week poutcome;
	  VAR &_trgind; 
   run;

