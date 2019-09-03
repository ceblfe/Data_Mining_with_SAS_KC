/* PRACTICA 1 MODELIZACIÓN */

/* PASO 1: 
Obj: Predecir el peso infantil al nacer un niño a partir de las variables analíticas 
recogidas a tal fin */

*Conexion a Libreria permanente, por usuario;

libname lib_data '/home/ceblfe0/Cesar/data';

/* PASO 2: Recogida de datos */
/*Paso DATA*/
data dataPesoBebe; 
	set sashelp.bweight; 
	orden= _n_ ;
run;

title 'Primeras 10 observaciones';
proc print data= dataPesoBebe (obs=10);
run;

/* PASO 3: Exploración de datos */
/* Análisis de frecuencias */

proc freq 
   data= dataPesoBebe;
run;

/* Análisis de frecuencias separando en tablas cada variable */
title 'Tabla de frecuencias para todas las variables categóricas';
ods graphics on;
proc freq
	data= dataPesoBebe;
	tables Black Married Boy MomSmoke Visit MomEdLevel/ nocum plots=freqplot;
run;
ods graphics off;

/* Estadísticos descriptivos para variables continuas */

title 'Estadísticos descriptivos para variables continuas';
ods noproctitle;
proc means data= dataPesoBebe n nmiss min mean std skew kurt max;
	var weight MomAge CigsPerDay MomWtGain;
run

/* Eliminación de duplicados */

proc sort nodupkey data=dataPesoBebe;
by Weight Black Married Boy MomAge MomSmoke CigsPerDay MomWtGain Visit MomEdLevel ;
run;

/* Ordenación del dataset */
proc sort data=dataPesoBebe;
by orden;
run;

/* Análisis de frecuencias */

proc freq data=dataPesoBebe; run;

/*Analisis de normalidad, outliers*/

proc univariate data=dataPesoBebe normal plot;
 var Weight;
 qqplot Weight  /  NORMAL (MU=EST SIGMA=EST COLOR=RED L=1);
 HISTOGRAM / NORMAL(COLOR=MAROON W=4) CFILL = pink CFRAME = LIGR;
 INSET MEAN STD /CFILL=BLANK FORMAT=5.2;
run;

/*Analisis de Correlacion*/

proc corr data=dataPesoBebe;   
      var _numeric_;
run;

/* Análisis de la interacción entre MomSmoke*CigsPerDay */
ods noproctitle;
ods selct chisq;
proc freq data= dataPesoBebe;
tables MomSmoke*CigsPerDay / chisq;
run;





/* PASO 4: Modelo con todos los datos */

/* Modelo con todos los datos */

ods exclude ParameterEstimates;
proc glm data= dataPesoBebe;
class Married Boy Black MomAge Visit CigsPerDay MomEdLevel MomWtGain;
model weight=Married Boy Black MomAge Visit CigsPerDay MomEdLevel MomWtGain / ss3 solution;
run;

/* PASO 5: Preparación de datos */

/* 5.1. Agrupación MomAge */

ods graphics on;
ods select meanplot;
proc glm data=dataPesoBebe plots(only)=meanplot(cl);
	class MomAge;
	model weight=MomAge / ss3 solution;
	lsmeans MomAge;
run;
ods graphics off;

/* 5.2. Agrupación MomWtGain */

ods graphics on;
ods select meanplot;
proc glm data=dataPesoBebe plots(only)=meanplot(cl);
	class MomWtGain;
	model weight=MomWtGain ;
	lsmeans MomWtGain;
run;
ods graphics off;

/* 5.3. Agrupación CigsPerDay */

ods graphics on;
ods select meanplot;
proc glm data=dataPesoBebe plots(only)=meanplot(cl);
	class CigsPerDay;
	model weight=CigsPerDay ;
	lsmeans CigsPerDay;
run;
ods graphics off;

/* 5.4 Código con la agrupacion de variables: MomAge, MomWtGain, CigsPerDay, Visit */
 data _dataPesoBebe;
	set dataPesoBebe;

	if MomAge >=0 then
		_momage= 0;
	else if MomAge in (-4:-1) then
		_momage= 1;
	else
		_momage='2';

	if MomWtGain in (-30:-10) then
		_momwtgain= 0;
	else if MomWtGain in (-9:4) then
		_momwtgain= 1;
	else
		_momwtgain= 2;

	if CigsPerDay <=0 then
		_cigsperday= 0;
	else if CigsPerDay > 0 and CigsPerDay <=5 then
		_cigsperday= 1;
	else
		_cigsperday= 2;

	if Visit=0 then
		_visit= 0;
	else
		_visit= 1;

run;

/* PASO 6: Modificación y transformación de datos */

/* Modificación y transformación de datos */
/* Selección datos de Training */
data PesoBebe_Train validacion_Test;
set _dataPesoBebe;
if _N_ <= 38934 
then output PesoBebe_Train;
else output validacion_Test;
run;

/* Selección datos de Test y Validación */

data PesoBebe_Test PesoBebe_validacion;
set validacion_Test;
 	if _N_ <= 4900 
then output PesoBebe_Test;
  		else output PesoBebe_validacion;
run;


/* PASO 7: Modelización */

/* 7.1 Modelo 1 */
/* Variable de clasificación Boy. Variables explicativas todas + interacciones */

proc glmselect data=pesoBebe_Train;
   class Boy;  			
   model weight = 
	_cigsperday 
	_momage
	_momwtgain
	_visit 
	Boy 
	MomEdLevel
	Married 
	_cigsperday*_momage
	_cigsperday*_momwtgain
	_cigsperday*_visit
	_cigsperday*Boy 
	_cigsperday*MomEdLevel _cigsperday*Married  
	_momage*_momwtgain
	_momage*_visit
	_momage*Boy _momage*MomEdLevel _momage*Married
	_momwtgain*_visit
	_momwtgain*Boy _momwtgain*MomEdLevel _momwtgain*Married
	_visit*Boy _visit*MomEdLevel _visit*Married
	Boy*MomEdLevel Boy*Married
	MomEdLevel*Married
	/ selection=stepwise
	(select=SL) stats=all;
run;


/* Sacamos el modelo GLM para las variables seleccionadas Modelo 1 */
proc glm data=pesoBebe_Train;
   	class Boy;
  	model weight = 
				_momage
 				MomEdLevel
				_cigsperday*_momage _cigsperday*_visit
				_cigsperday*Boy _cigsperday*MomEdLevel
				_cigsperday*Married 
				_momage*_momwtgain _momage*_visit 
				_momage*MomEdLevel _momage*Married
				_momwtgain*_visit _momwtgain*Boy _momwtgain*MomEdLevel _momwtgain*Married
				_visit*Boy
				Married*Boy
				MomEdLevel*Married
				/ solution e;
run;


/* Eliminamos las variables: _cigsperday*_visit _momage*_momwtgain _momage*_visit
_momage*Married _visit*_momwtgain MomEdLeve*_momwtgain Married*_momwtgain
MomEdLevel*Married _visit*Boy */

proc glm data=pesoBebe_Train;
   	class Boy;
  	model weight = 
				_momage
 				MomEdLevel
				_cigsperday*_momage
				_cigsperday*Boy _cigsperday*MomEdLevel
				_cigsperday*Married 
				_momage*MomEdLevel
				_momwtgain*Boy
				Married*Boy
				/ solution e;
run;


/* 7.2 Modelo 2 */
/* Variables de clasificación todas. Variables explicativas todas + interacciones */
proc glmselect data=pesoBebe_Train;
   class _cigsperday _momage _momwtgain _visit Boy MomEdLevel Married;  			  			
   model weight = 
	_cigsperday 
	_momage
	_momwtgain
	_visit 
	Boy 
	MomEdLevel
	Married 
	_cigsperday*_momage
	_cigsperday*_momwtgain
	_cigsperday*_visit
	_cigsperday*Boy 
	_cigsperday*MomEdLevel _cigsperday*Married  
	_momage*_momwtgain
	_momage*_visit
	_momage*Boy _momage*MomEdLevel _momage*Married
	_momwtgain*_visit
	_momwtgain*Boy _momwtgain*MomEdLevel _momwtgain*Married
	_visit*Boy _visit*MomEdLevel _visit*Married
	Boy*MomEdLevel Boy*Married
	MomEdLevel*Married
	/ selection=stepwise
	(select=SL) stats=all;
run;

/* Sacamos el modelo GLM para las variables seleccionadas Modelo 2 */

proc glm data=pesoBebe_Train;
   class _cigsperday _momage _momwtgain _visit Boy MomEdLevel Married;  			  			
     model weight =  
	_cigsperday*_momage
	_cigsperday*_visit
	_cigsperday*Boy 
	_cigsperday*MomEdLevel
	_cigsperday*Married  
	_momage*_momwtgain
	_momage*_visit
	_momage*Married
	_momwtgain*_visit
	_momwtgain*Boy
 	_momwtgain*MomEdLevel
 	_momwtgain*Married
	/ solution e;
run;

/* Eliminamos las variables:
_cigsperday*_visit
_cigsperday*MomEdLevel
_cigsperday*Married
_momage*_visit
_momage*Married 
_momwtgain*_visit
_momwtgain*Boy 
_momwtgai*MomEdLevel */

proc glm data=pesoBebe_Train;
   class _cigsperday _momage _momwtgain _visit Boy MomEdLevel Married;  			  			
     model weight =  
	_cigsperday*_momage
	_cigsperday*Boy 
_momage*_momwtgain
 	_momwtgain*Married
	/ solution e;
run;


/* PASO 8: Test */
/* Test con modelo ganador I */
proc glm data=pesoBebe_Test;
   class _cigsperday _momage _momwtgain _visit Boy MomEdLevel Married;  			  			
     model weight =  
	_cigsperday*_momage
	_cigsperday*_visit
	_cigsperday*Boy 
	_cigsperday*MomEdLevel
	_cigsperday*Married  
	_momage*_momwtgain
	_momage*_visit
	_momage*Married
	_momwtgain*_visit
	_momwtgain*Boy
 	_momwtgain*MomEdLevel
 	_momwtgain*Married
	/ solution e;
run;

/* Test con modelo ganador II */

proc glm data=pesoBebe_Test;
   class _cigsperday _momage _momwtgain _visit Boy MomEdLevel Married;  			  			
     model weight =  
	_cigsperday*_momage
	_cigsperday*Boy 
	_momage*_momwtgain
 	_momwtgain*Married
	/ solution e;
run;


/* PASO 8: Validación */

/* Validación con modelo ganador I */

proc glm data=pesoBebe_validacion;
   class _cigsperday _momage _momwtgain _visit Boy MomEdLevel Married;  			  			
     model weight =  
	_cigsperday*_momage
	_cigsperday*_visit
	_cigsperday*Boy 
	_cigsperday*MomEdLevel
	_cigsperday*Married  
	_momage*_momwtgain
	_momage*_visit
	_momage*Married
	_momwtgain*_visit
	_momwtgain*Boy
 	_momwtgain*MomEdLevel
 	_momwtgain*Married
	/ solution e;
run;


/* Validación con modelo ganador II */

proc glm data=pesoBebe_validacion;
   class _cigsperday _momage _momwtgain _visit Boy MomEdLevel Married;  			  			
     model weight =  
	_cigsperday*_momage
	_cigsperday*Boy 
	_momage*_momwtgain
 	_momwtgain*Married
	/ solution e;
run;
