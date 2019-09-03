/* PRACTICA 2: CLASIFICACIÓN */

/* PASO 1:
Objetivo: Identificar población cuyos bebés son más propensos de nacer con poco 
peso ( bajo peso relacionado con alto nivel de mortalidad infantil) */



/* PASO 2: Recogida de Datos */
data dsPesoBebe_Mort;
	set sashelp.birthwgt;
	orden= _n_;
run;


/* PASO 3: EXPLORACIÓN DE LOS DATOS */
/* 3.1 Depuración */
proc sort nodupkey data=dsPesoBebe_Mort;
	by LowBirthWgt AgeGroup Death Drinking Married Race Smoking SomeCollege;	
run;


/* 3.2. Identificación de: outliers, missing y ruido. */
proc freq data=dsPesoBebe_Mort;
run;


/* 3.3. Pasar a 1/0 las variables dicotómicas */

data dsPesoBebe_mortal (drop=LowBirthWgt AgeGroup Death Drinking Married Smoking SomeCollege Race);
	set dsPesoBebe_Mort;

		if LowBirthWgt="Yes" then LowBirthWgt_n=1;
		else LowBirthWgt_n=0;

		if AgeGroup=1 then AgeGroup_1=1;
		else AgeGroup_1=0;

		if AgeGroup=2 then AgeGroup_2=1;
		else AgeGroup_2=0;

		if AgeGroup=3 then AgeGroup_3=1;
		else AgeGroup_3=0;

		if Death="Yes" then Death_n=1;
		else Death_n=0;

		if Drinking="Yes" then Drinking_n=1;
		else Drinking_n=0;

		if Married="Yes" then Married_n=1;
		else Married_n=0;

		if Smoking="No" then Smoking_n=0;
		else Smoking_n=1;

		if SomeCollege="Yes" then SomeCollege_n=1;
		else SomeCollege_n=0;

		if Race="Asian" then Race_Asian=1;
		else Race_Asian=0;

		if Race="Black" then Race_Black=1;
		else Race_Black=0;

		if Race="Hispanic" then Race_Hispanic=1;
		else Race_Hispanic=0;

		if Race="Native" then Race_Native=1;
		else Race_Native=0;

		if Race="White" then Race_White=1;
		else Race_White=0;
		orden= _n_;
run;



proc print data=dsPesoBebe_mortal (obs=10) noobs;



/* 3.4. Análisis estadístico inicial */

/* 3.4.1. Análisis de LowBirthWgt */
PROC GCHART DATA=dsPesoBebe_Mort;
	VBAR LowBirthWgt / TYPE=FREQ;
RUN;

/* 3.4.2. Resumen de la naturaleza de las variables */
ods graphics on;
proc ttest data=dsPesoBebe_mortal;
	var LowBirthWgt_n AgeGroup_1 AgeGroup_2 AgeGroup_3 Death_n
Drinking_n Married_n Race_Asian Race_Black Race_Hispanic Race_Native Race_White Smoking_n SomeCollege_n;
run;
ods graphics off;

/* 3.5. Análisis de correlación */
proc corr data=dsPesoBebe_mortal;		
var LowBirthWgt_n AgeGroup_1 
	AgeGroup_2 AgeGroup_3 Death_n 
	Drinking_n Married_n 
Race_Asian Race_Black Race_Hispanic Race_Native Race_White Smoking_n SomeCollege_n;
run;

/* PASO 4: MODELIZACIÓN */
/* Modelo de aprendizaje no supervisado */

ods graphics on;
proc cluster data=dsPesoBebe_mortal method=centroid 
					nonorm ccc pseudo rmsstd rsquare 
					
              out=results_class plots=den(height=rsq) PRINT=20 plots(maxpoints=700);
 
		      var  LowBirthWgt_n Death_n Drinking_n Married_n Smoking_n SomeCollege_n
                   AgeGroup_1 AgeGroup_2 AgeGroup_3 
                   Race_Asian Race_Black Race_Hispanic Race_Native Race_White;
run;

ods graphics off;

ods graphics on;

goptions vsize=9in hsize=6.4in htext=.9pct htitle=3pct;
axis1 order=(0 to 1 by 0.2);
proc tree data=dsPesoBebe_mortal out=results_tree nclusters=16
haxis=axis1 horizontal;
height=rsq;
copy Death_n Drinking_n Married_n Smoking_n SomeCollege_n
                   AgeGroup_1 AgeGroup_2 AgeGroup_3 
                   Race_Asian Race_Black Race_Hispanic Race_Native Race_White;
id LowBirthWgt_n;
run;

ods graphics off;


ods graphics on;
proc cluster data=dsPesoBebe_mortal method=centroid 
					nonorm ccc pseudo rmsstd rsquare 
					
              out=results_class plots=den(height=rsq) PRINT=20 plots(maxpoints=700);
			  id LowBirthWgt_n;
		      var  Death_n Drinking_n Married_n Smoking_n SomeCollege_n
                   AgeGroup_1 AgeGroup_2 AgeGroup_3 
                   Race_Asian Race_Black Race_Hispanic Race_Native Race_White;
run;

ods graphics off;















