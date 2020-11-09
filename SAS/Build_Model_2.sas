

libname x "R:\RESZhu\Article\2019_ATUS_NTHS_sijun\Data_Analysis";

**************************************************************************************************************************************;
* read into data set;
PROC IMPORT DATAFILE='R:\RESZhu\Article\2019_ATUS_NTHS_sijun\Data_Analysis\combined_output.csv' dbms=csv
OUT=combined_output REPLACE;
GETNAMES=yes;
run;

data combined_output;
	set combined_output;
	log_VMT = log(VMT);
	log_DD = log(DD);
	log_POP = log(pop);
run;

PROC SQL;
	CREATE TABLE temp AS
	SELECT AGEGROUP, SUM(VMT) as VMT, SUM(DD) AS DD
	FROM combined_output
	GROUP BY AGEGROUP;
QUIT;





proc contents data=combined_output;
run;

%macro test_model_2(dataSet=, distribution=, exposure=, error=0);
	%if &error=0 %then %do;
		proc glimmix data=&dataSet IC=PQ;
	%end;
	%else %do;
		proc glimmix data=&dataSet IC=PQ method=quad empirical;
	%end;
			class AGEGROUP(ref='2') SEX(ref='2') weekend(ref="1") nighttime(ref="0") quarter(ref="1");
			model counts=AGEGROUP SEX weekend nighttime quarter
					 /link=log offset=&exposure dist=&distribution solution;
			NLOPTIONS MAXITER=5000;
			estimate "16-24 vs 45-64" AGEGROUP 1 0 0 -1/cl exp;
			estimate "45-64 vs 45-64" AGEGROUP 0 1 0 -1/cl exp;
			estimate "65 plus vs 45-64" AGEGROUP 0 0 1 -1/cl exp;
			estimate "Male vs female" SEX 1 -1/cl exp;
			estimate "nighttime VS daytime" nighttime 1 -1/cl exp;
			estimate "weekday vs weekend" weekend 1 -1/cl exp;
			estimate "quarter 2 vs quarter 1" quarter 1 0 0 -1/cl exp;
			estimate "quarter 3 vs quarter 1" quarter 0 1 0 -1/cl exp;
			estimate "quarter 4 vs quarter 1" quarter 0 0 1 -1/cl exp;		
		run;
%mend;

%test_model_2(dataSet=combined_output, distribution=negbin, exposure=log_VMT, error=0);
%test_model_2(dataSet=combined_output, distribution=negbin, exposure=log_DD, error=0);
%test_model_2(dataSet=combined_output, distribution=negbin, exposure=log_POP, error=0);


