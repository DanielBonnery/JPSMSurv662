\name{casen2009}
\alias{casen2009}
\docType{data}
\title{casen2009}
\description{
  
}
\usage{data(casen2009)}
\format{
 This table is a selection of variable from Casen 2009 data (see Example below for complete description of the creation of the table)
  \describe{
    \name{casen2009}
    \item{REGION}{Region}
    \item{ZONA}{Zone  (Urban:1, Rural:2)}
\item{COMUNA}{Comune}
\item{EXPC}{Weight for comuna (weight calibrated to comuna population count)}
\item{EXPR}{Weight for Region (weight calibrated to region population count)}
\item{SEGMENTO}{Segment (Primary sampling unit)}
\item{ESTRATO}{Design stratum ID}
\item{IDVIV}{housing unit ID (secondary stage unit); this is no unique meaning that two HU from different psu may have the same IDVIV}
\item{FOLIO}{Household ID}
\item{EDAD}{Age}
\item{SEXO}{Gender}
\item{CORTE}{Povery indicator ("Indigente": very poor,
"Pobre no Indigente": poor "No pobre": not poor)}
\item{HOGAR}{Number of households in a HU}
\item{O}{Order number of person in the household. "1": head of household}
\item{PROVINCIA}{Province}
\item{PCO1}{Kinship with head of household}
\item{ECIVIL}{Marital status: 
Casado(a): married, 
Conviviente o pareja: domestic partner,
Anulado(a): canceled,
Separado(a): separated,
Divorciado(a): divorced,
Viudo(a): widowed,
"Soltero(a):single }
\item{NUCLEO}{Number of "nucleos"}
\item{PCO2}{Kinship with head of nucleo}
\item{NUMPER}{Number of people in a household (excludes domestic service living in the house) }
\item{ESC}{Scholarity}
\item{EDUC}{Educational level}
\item{ASISTE}{Attend to school (Si: yes,  No: no)}
\item{YTOTHAJ}{Household total income   (ymonehaj +yaimhaj)}
\item{YMONEHAJ}{Household monetary income}
\item{YAIMHAJ}{Imputed rent for housing}
}
Notes:  Comuna and Zona were used for design stratification. 
YTOTHAJ/NUmper gives per-capita HH income.  This is then compared with poverty line (one for rural and the other for urban) to get the poverty status of the HH.  Person poverty status is same as the HH poverty status.  So, both person level and HH level poverty statistics can be obtained from HH level data (say we keep o=1 corresponding to the head of household).
All weights are person level.  HH and HU weights can be taken as the weight of head of household.
}
\source{
\url{http://observatorio.ministeriodesarrollosocial.gob.cl/layout/doc/bases/Casen2009spss.rar}
In \url{http://datos.gob.cl/datasets/ver/4744}, one can read:


La Encuesta de Caracterizaci\'{o}n Socioecon\'{o}mica Nacional recolecta datos acerca de caracter?sticas sociales y econ\'{o}micas de una muestra representativa de la poblaci\'{o}n que reside en hogares particulares. La muestra 2009 fue dise\~{n}ada para producir estimaciones para los dominios de representaci\'{o}n geogr?fica definidos por el nivel pa?s y todas sus regiones. Si se desea producir estimaciones a menores niveles de desagregaci\'{o}n, se recomienda evaluar su confiabilidad estad?stica utilizando criterios como el coeficiente de variaci\'{o}n o el tama\~{n}o efectivo de muestra.


Para efectos de reconocer los cr?ditos de CASEN en los documentos y publicaciones que resulten de su manipulaci\'{o}n, se debe usar el siguiente texto:


1. En la secci\'{o}n introductoria: "Esta investigaci\'{o}n utiliz\'{o} informaci\'{o}n de la Encuesta de Caracterizaci\'{o}n Socioecon\'{o}mica Nacional. El autor agradece al Ministerio de Desarrollo Social, propietario intelectual de la Encuesta, haberle permitido disponer de la base de datos. Todos los resultados del estudio son de responsabilidad del autor y en nada comprometen a dicho Ministerio".

2. En las referencias bibliogr?ficas: Ministerio de Desarrollo Social, Divisi\'{o}n Social, Encuesta CASEN www.ministeriodesarrollosocial.gob.cl/observatorio/casen/

Advertencia
Los datos CASEN deben utilizarse con particular cuidado al realizar an?lisis a nivel comunal, en tanto, dado el tama\~{n}o muestral de la Encuesta CASEN, las estad?sticas generadas a nivel comunal est?n sujetas a un error muestral significativamente mayor que las estad?sticas generadas a nivel regional o nacional.

El Ministerio de Desarrollo Social pone a disposici\'{o}n del p\'{u}blico:


1. Bases de datos Encuesta Casen 2009.
2. Libro de C\'{o}digos Encuesta Casen 2009.
3. Metodolog?a del Dise\~{n}o Muestral y Factores de Expansi\'{o}n Encuesta Casen 2009.
4. Manual del Investigador Encuesta Casen 2009.
}

\examples{
if(FALSE){
download.file("http://observatorio.ministeriodesarrollosocial.gob.cl/layout/doc/bases/Casen2009spss.rar")
library(utils)
system('"C:/Program Files/7-Zip/7z.exe" x "Casen2009Spss.rar"')
casen2009=foreign::read.spss(file.path(tmpfolder,"Casen2009Spss.sav"))}

data(casen2009)}
\keyword{datasets}
  