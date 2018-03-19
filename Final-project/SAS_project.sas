/* Final SAS project for STAT202A;
* Complete the code;*/

/* Import car data;*/
proc import out= work.data
datafile= "/folders/myfolders/Stats/regression_auto.csv"
dbms=csv replace; getnames=yes; datarow=2;
run;

/* Compute the correlation between car length and mpg;*/
proc corr data=data;
var length mpg;
run;

/* Make a scatterplot of price (x-axis) and mpg (y-axis);*/
title 'Scatterplot of Price vs Mpg';
proc sgplot data=data;
scatter x=price y=mpg;
run;

/* Make a box plot of mpg for foreign vs domestic cars;*/
title 'Boxplot of Mpg for Foreign vs Domestic cars';
proc boxplot data=data;
plot mpg*foreign;

/* Perform simple linear regression, y = mpg, x = price1; 
* Do NOT include the intercept term;*/
proc reg data=data;
model mpg = price1 /noint;
run;

/* Perform linear regression, y = mpg, x1 = length, x2 = length^2; 
* Include the intercept term;*/
proc glm data = data;
model mpg = length length*length;
run;
