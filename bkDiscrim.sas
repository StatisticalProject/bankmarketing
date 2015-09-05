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




/** Approche disqual */
/** tableau disjonctif */
PROC TRANSREG DATA = bankmarketing
DESIGN NOPRINT ;

MODEL CLASS ( job marital education default housing loan 
contact month day_of_week  pdaysCat poutcome ) ;

OUTPUT OUT = TableauDisjonctifComplet ;

RUN ;
/** Analyse des correspondances */
PROC corresp
 data=TableauDisjonctifComplet
 outc=corresp (WHERE = (_TYPE_='OBS')) plot=none noprint
 DIMENS=38;
 VAR &_trgind;
run;
/** on melange avec les données orginelles */
data bankmarketingDiscrim;
   merge corresp bankmarketing;
run;

proc standard data=bankmarketingDiscrim mean=0 std=1 out=bankmarketingDiscrimCR;
   var age previous campaign;
run;

/*Données d'apprentissage et de validation*/
Proc Surveyselect data=bankmarketingDiscrimCR seed=7 out=bankmarketingDiscrimCRSplit samprate=.7 outall;
Run;

Data bktrainingDiscrim bkvalidationDiscrim;
Set bankmarketingDiscrimCRSplit;
if selected = 1 then output bktrainingDiscrim;
else output bkvalidationDiscrim;
Run;


/** Dicriminante */
proc discrim  data=bktrainingDiscrim testdata=bkvalidationDiscrim method=normal pool=yes list crossvalidate ALL;
class y;
var Dim1--Dim38 age previous campaign ;
run;

ods rtf close;
