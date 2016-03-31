\name{casen2013}
\alias{casen2013}
\docType{data}
\title{casen2013}
\description{
}
\usage{data(casen2013)}
\format{
 This table is a selection of variable from Casen 2013 data (see Example below for complete description of the creation of the table)
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
\url{http://observatorio.ministeriodesarrollosocial.gob.cl/layout/doc/bases/Casen2013spss.rar}

}
\examples{
#Program used to create the file
if(FALSE){
file.remove(list.files(tmpfolder))
download.file("http://observatorio.ministeriodesarrollosocial.gob.cl/documentos/CASEN_2013_MN_B_Principal_spss.rar",
              destfile=file.path(tmpfolder,"CASEN_2013_MN_B_Principal_spss.rar"))
system(
  if(.Platform$OS.type == "unix") {
    'unrar e "CASEN_2013_MN_B_Principal_spss.rar"'
  } else {
    '"C:\\Program Files\\7-Zip\\7z.exe" x "CASEN_2013_MN_B_Principal_spss.rar" -y'  
  }
  )
casen2013=foreign::read.spss(file.path(tmpfolder,"CASEN_2013_MN_B_Principal.sav"),to.data.frame=TRUE)
file.remove(list.files(tmpfolder))
casen2013<-casen2013[is.element(tolower(names(casen2013)),
                                       tolower(c("REGION","COMUNA","EXPC","EXPR","SEGMENTO","ESTRATO","IDVIV2","FOLIO","EDAD","SEXO","CORTE")))]
}}
\keyword{datasets}
  