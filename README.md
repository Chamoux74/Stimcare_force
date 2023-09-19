#Ordre d'extraction 

D'abord runner les deux script extraction puis anova 

#extractionforcepatch 

## détail script
### IMVC

isolement des variable qui m'interesse dans le fichier xml 

Selection de la valeur maximal de chaque instant de mesure pour chaque sujet 

Conversion de la force en newton 

###RFD

Trouver le début de la contraction, en cherchant lorsque deux différence consécutive entre 2 valeurs sont supérieur à 2 

Ensuite on garde les 20 première valeurs correspondant au 200 première milliseconde

Puis valeurs moyenne sur ce temps 

#extractionforceplacebo 

même chose pour les fichiers placebo 

#anovaforce 

