library(SPARQL)
library(ggplot2)
library(ggrepel) 
library(plotly)
library(cluster)
library(ggfortify)
library(ggmap) 

endpoint <- "http://localhost:3030/bando/query" 
query <-
  "
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> 
PREFIX Ban: <http://Bandara3.com/> 
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT *
WHERE {

SERVICE <http://data.intip.in/sparql/> {
SELECT ?Provinsi ?Populasi_Proyeksi_BPS_2014 ?PDRB_per_kapita_2014_juta_rupiah ?Longitude ?Latitude
WHERE {}
} .

?ins rdfs:label ?Provinsi .
?ins Ban:MemilikiBandara ?Bandara .
?Bandara Ban:jenis ?Jenis .
?Jenis Ban:PenumpangBerangkat ?PenB .
?Jenis Ban:PenumpangDatang ?PenD .
?Jenis Ban:PesawatBerangkat ?PesB .
?Jenis Ban:PesawatDatang ?PesD .

}
"
# query from sparql endpoint
quedata <- SPARQL(endpoint,query) 
# parsing result
bandara = quedata$results #kamu bikin tabel dari hasil query
dt = bandara 
de = barunastra #tampung tabel biar hasil query tidak perlu di eksekusi lagi, di duplicate
de$Provinsi <- NULL 
de$Populasi_Proyeksi_BPS_2014 <- NULL
de$Longitude <- NULL
de$Latitude <- NULL
de$ins <- NULL
de$Bandara <- NULL
de$Jenis <- NULL
de$PesB <- NULL
de$PesD <- NULL
hasil <- kmeans(de,3)
hasil 
de$cluster <- hasil$cluster
bandaraMatrix=as.matrix(cbind(de$cluster, barunastra$Longitude, barunastra$Latitude),ncol=3) 
bandaraData = data.frame(bandaraMatrix) #bikin frame dari yg di cluster berdasar lat long
ina_center = as.numeric(geocode("Indonesia")) #load dari ggmap untuk peta indonesia
INAMap = ggmap(get_googlemap(center=ina_center, scale=2, zoom=4), extent="normal") 
circle_scale_amt = 0.10 # mengukur besar kecilnya lingkaran oranye di peta
INAMap + 
  coord_fixed(xlim= c(95, 140), ylim = c(-12, 10)) + 
  geom_point(aes(x=barunastra$Longitude, y=barunastra$Latitude), data=bandaraData, col="orange", alpha=0.4, size=barunastra$PDRB_per_kapita_2014_juta_rupiah*circle_scale_amt) +
  scale_size_continuous(range=range(barunastra$PDRB_per_kapita_2014_juta_rupiah))
 #dari peta ini ditambahkan styling menggunakan ggplot2 