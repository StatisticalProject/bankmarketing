DATA bankmarketing;
    infile "bank-additional-full.csv" delimiter=';';
    INPUT y $ gender $ job $ marital $education $default $housing $loan $contact duration
RUN; 
title 'Stepwise Regression on Cancer Remission Data';
   proc logistic data=bankmarketing outest=betas covout;
      model y=job marital education default housing loan contact;
      output out=pred p=phat lower=lcl upper=ucl
             predprob=(individual crossvalidate);
   run;

