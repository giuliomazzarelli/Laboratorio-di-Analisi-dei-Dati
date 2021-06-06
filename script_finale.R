Titolo <- "esame di analisi dati"
Autore <- "Giulio Mazzarelli"
Nmatricola <- "0000916405"
Analisi <- "sulle emissioni di co2 e la produzione di energia nel mondo"

info <- c(Titolo,Autore,Nmatricola,Analisi)

info[1:4]

#importo il dataset da Excel
library(readxl)
Energy <- read_excel("C:/Users/mazza/Desktop/Energy.xlsx", sheet = "unionedati")

#per visualizzarlo
View(Energy)

#ci sono valori mancanti?
## se il valore è maggiore di zero allora mancano volori
sum(is.na(Energy))

###

#trasformo la variabile region nel tipo "factor"
factor(Energy$region)
#quanti paesi considero di ciascuna macroarea
table(Energy$region)

#distribuzione di frequenza dell'appartenenza di ciascun Paese alla rispettiva macroarea
tabella <- table(Energy$region)
tabella
round(prop.table(tabella),digits = 2)

#assegno alla variabile 'percentuali' i valori percentuali delle frequenze relative
percentuali <- round(prop.table(tabella),digits = 4)*100
names(percentuali)

#grafico sulla ripartizione dei Paesi nelle macroaree definite
barplot(percentuali, names.arg=names(tabella), main ="Ripartizione dei Paesi nelle macroaree geografiche", col="powderblue")

###

# converto il dataset in matrice
df.piccolo <- Energy
df.piccolo$country <- NULL
df.piccolo$region <- NULL
mat <- as.matrix(df.piccolo)

# esporto le tebelle in formato txt così posso importarle su Excel e successivamente inserirle nel report

#non ho usato quella relativa alle medie perchè incorporata nella tabella summary
mat.medie <- round(apply(mat,2,mean),2)                                        
write.csv(mat.medie, "medie.txt")

#alcune informazioni sulle variabili
mat.summary <- round(apply(mat,2,summary),2)
write.csv(mat.summary, "summary.txt")

# per varianza, deviazione standard, coefficiente di correlazione e correlazione escludo le varibili
## Energy$s.elerinnov e Energy$s.windsolar perchè valori percentuali e dunque un analisi su di essi produrrebbe
### risultati con poco significato
df.piccolo$s.elerinnov <- NULL
df.piccolo$s.windsolar <- NULL

#per poter ottenere tabelle relative alla varianza, deviazione standard e coefficiente di variazione 
## ho dovuto definire delle funzioni

#varianza delle variabili
varianza <- function(x){
  n <-  length(x) #questo calcola quanti dati ho
  devianza <- sum(x^2)-(mean(x)^2) ## questa è la formula semplificata della DEVIANZA
  return(devianza/n) # in output restituisce la devianza/n
  
}
mat.varianza <- round(apply(mat,2,varianza),2)
write.csv(mat.varianza, "varianza.txt")


#deviazione standard delle variabili
sd <- function(x){
  n <- varianza(x)^0.5
  return(n)
  
}
mat.sd <- round(apply(mat,2,sd),2)
write.csv(mat.sd, "sd.txt")

#coefficiente di variazione delle variabili
cv <- function(x){        
  n <- varianza(x)^0.5
  n <- n/abs(mean(x))
  return(n)
  
}
mat.cv <- round(apply(mat,2,cv),2)
write.csv(mat.cv, "cv.txt")

###


boxplot(Energy$p.greggio, xlab="Greggio",ylab="Mt prodotte",data=Energy)
boxplot(Energy$p.raffinati,xlab="Raffinati",ylab="Mt prodotte",data=Energy)
boxplot(Energy$p.coalignite,xlab="Carbone e lignite",ylab="Mt prodotte",data=Energy)
boxplot(Energy$p.gas,xlab="Gas",ylab="bcm prodotti",data=Energy)
boxplot(Energy$p.ele,xlab="Elettrcità",ylab="TWh prodotti",data=Energy)
boxplot(Energy$co2,xlab="CO2",ylab="Mt emesse",data=Energy)


#istogrammi delle variabili considerate
## ho preferito i boxplot agli istogrammi

#hist(Energy$p.greggio,breaks=7)
#hist(Energy$p.raffinati,breaks=7)
#hist(Energy$p.coalignite,breaks=7)                    
#hist(Energy$p.gas,breaks=7)
#hist(Energy$p.ele,breaks=7)
#hist(Energy$co2,breaks=7)

###

###

# istogramma dello share di energia elettrica ottenuta da fonti rinnovabili
hist(Energy$s.elerinnov,breaks=5,main=" ",ylab="Frequenza",xlab="Share di energia rinnovabile " ,col="powderblue")
#quali sono i Paesi con uno share di energia elettrica maggiore del 65%?
Energy$country[Energy$s.elerinnov>0.65]
## creo una tabella con i Paesi che hanno uno share di energia elettrica rinnovabile superiore al 65% e il rispettivo share
topmore65c <- Energy$country[Energy$s.elerinnov>0.65]
topmore65s <- Energy$s.elerinnov[Energy$s.elerinnov>0.65]
topmore65 <- rbind(topmore65c,topmore65s)
topmore65
write.csv(topmore65, "topmore65.txt")


# istogramma dello share di energia elettrica solare ed eolica
hist(Energy$s.windsolar,breaks=6,main=" ",ylab="Frequenza",xlab="Share di energia solare ed eolica " ,col="powderblue")
#quali sono i paesi con uno share di energia solare ed eolica maggiore del 20%?
Energy$country[Energy$s.windsolar>0.20]
## creo una tabella con i Paesi che hanno uno share di energia elettriva rinnovabile superiore al 65% e il rispettivo share
topmore20c <- Energy$country[Energy$s.windsolar>0.20]
topmore20s <- Energy$s.windsolar[Energy$s.windsolar>0.20]
topmore20 <- rbind(topmore20c,topmore20s)
topmore20
write.csv(topmore20, "topmore20.txt")

###

#creo una variabile categorica a partire dalla variabile Energy$co2
## gli intervalli che considero sono i seguenti
co2.categorica <- cut(Energy$co2,breaks= c(0,200,1200,9466.5))
co2.categorica <- factor(co2.categorica, order=T)
#assegno un nome a ciascun livello della categorica
levels(co2.categorica) <- c("basso","medio","elevato")
#visualizzo quante osservazioni ci sono per ciascun livello di emissioni di co2
table(co2.categorica)
#le relative distribuzioni di frequenza
prop.table(table(co2.categorica))

#visualizzo una tabella a doppia entrata tra la variabile region e co2 e la esporto
table(Energy$region, co2.categorica)
prop.table(table(Energy$region, co2.categorica))
macroaco2 <- prop.table(table(Energy$region, co2.categorica))
write.csv(macroaco2, "rel_macroaco2.txt")
write.csv((table(Energy$region, co2.categorica)), "ass_macroaco2.txt")

###

#ho definito una funzione che immesso il valore della v di cramer restituisse una stringa che informasse
##sul livello di connessione tra le due variabili categoriche in questione

# oltre all'analisi sulla connessione tra macroarea e co2 e tra energia elettrica prodotta e co2 
## non mi sono mosso oltre perchè fintanto che sono io a definire arbitrariamente gli intervalli delle variabili 
### categoriche ha poco senso un'analisi di questo tipo

vcramer <- function(x){
  #la seguenti operazioni permettono un rapido giusdizio sul livello di connessione tra due variabili
  a <- paste(round(x,2)*100,"% di connessione")
  if(a>0&&a<=25){b <- "connessione debole"} 
  else if(a>25&&a<=50){b <- "connessione discreta"} 
  else if(a>50&&a<=75){b <- "connessione buona"} 
  else if(a>75){b <- "connessione forte"} 
  else{b <- "le variabili sono sconnesse"}
  return(b)
}

#connessione tra macroarea e livelli di co2
chisq.test(table(Energy$region,co2.categorica))
x <- 3.2431
x <- x/length(Energy$country)
v <- sqrt(x/2)
vcramer(v)
table(Energy$region,co2.categorica)
# c'è una debole dipendenza tra regione e livelli di co2 emessi

#connessione tra elettricità prodotta e livelli di co2
ele.categorica <- cut(Energy$p.ele,breaks= c(0,140,310,7091.79))
ele.categorica <- factor(ele.categorica, order=T)
levels(ele.categorica) <- c("bassa","media","elevata")
table(ele.categorica)
chisq.test(table(ele.categorica,co2.categorica))
x2 <- 31.065
x2 <- x2/length(Energy$country)
v2 <- sqrt(x2/2)
vcramer(v2)
write.csv(table(ele.categorica,co2.categorica), "ass_eleco2.txt")
ele_co2 <- prop.table(table(ele.categorica, co2.categorica))
write.csv(ele_co2, "rel_eleco2.txt")
# c'è una buona dipendenza tra livelli di co2 emessi e elettricità prodotta

###

#boxplot della quantità di co2 emessa condizionatamente alla macroarea
boxplot(Energy$co2 ~ Energy$region,xlab="Macroarea", ylab="Mt di CO2 emessi", data=Energy, col=c(2,3,5,4))

#per i seguenti confronti ho creato altre due variabili categoriche a partire da quelle numeriche della 
##produzione di greggio e dalla produzione elettrica

#emissione di co2 dipendentemente dalla quantità di energia elettrica prodotta
ele.categorica <- cut(Energy$p.ele,breaks= c(0,150,320,7091.79))
ele.categorica <- factor(ele.categorica, order=T)
levels(ele.categorica) <- c("bassa","media","elevata")
boxplot(Energy$co2 ~ ele.categorica,ylab = "Mt di CO2 emessi",xlab="Produzione di elettricità", data=Energy, col=c("greenyellow","gold1","firebrick3"))                            

#emissione di co2 dipendentemente dalla quantità di greggio prodotta
#greggio.categorica <- cut(Energy$p.greggio,breaks= c(0,100,350,700))  
#greggio.categorica <- factor(greggio.categorica, order=T)    
#levels(greggio.categorica) <- c("bassa","media","elevata")
#boxplot(Energy$co2 ~ greggio.categorica, ylab = "Mt di CO2 emessi",xlab="Produzione di greggio",data=Energy)                 


###


# correlazioni tra le varibili all'interno della matrice di dati
write.csv(cor(mat),"r.txt")

#plot tra TWh di elettricità prodotti e Mt di CO2 emessi
plot(Energy$p.greggio, Energy$co2,ylab="Mt di CO2 emessi",xlab="Mt di greggio prodotti", pch=19, col=1)

#plot tra TWh di elettricità prodotti e Mt di CO2 emessi
plot(Energy$p.raffinati, Energy$co2,ylab="Mt di CO2 emessi",xlab="Mt di raffinati prodotti", pch=19, col=1)

#plot tra Mt di carbone e lignite prodotti e Mt di CO2 emessi
plot(Energy$p.coalignite, Energy$co2,ylab="Mt di CO2 emessi",xlab="Mt di carbone e lignite prodotti", pch=19, col=1)

#plot tra TWh di elettricità prodotti e Mt di CO2 emessi
plot(Energy$p.gas, Energy$co2,ylab="Mt di CO2 emessi",xlab="bcm di gas prodotti", pch=19, col=1)

#plot tra TWh di elettricità prodotti e Mt di CO2 emessi
plot(Energy$p.ele, Energy$co2,ylab="Mt di CO2 emessi",xlab="TWh di elettricità prodotti", pch=19, col=1)


###

## Un grafico a barre con le emissioni di co2 per paese, ordinati per quantità di emissione
library(ggplot2)
num <- Energy$co2 
cat <- Energy$country
data <- data.frame(num, cat)    
df <- data[order(data$num,decreasing = TRUE),]
barplot(df$num,names.arg = df$cat,las=2,main="Emissioni di CO2 per Paese",ylab="Emissioni di CO2 (Mt)", col=1:44)

## Un grafico a barre con la produzione di elettricità per paese, ordinati per quantità di W prodotti
num2 <- Energy$p.ele
data2 <- data.frame(num2, cat) 
df2 <- data2[order(data2$num2,decreasing = TRUE),]
barplot(df2$num2,names.arg = df2$cat,las=2,main="Produzione elettrica per Paese",ylab="Produzione di elettricità (TWh)", col=1:44)

###

#Quanti Mt di CO2 emettono insieme i tre Paesi più impattanti?
country.top3 <- Energy$country[Energy$co2>2000]
co2.top3 <- Energy$co2[Energy$co2>2000]
ele.top3 <- round(Energy$p.ele[Energy$co2>2000],2)

sum(Energy$co2)
sum(co2.top3)
sum(co2.top3)/sum(Energy$co2)*100

#matrice con i dati relativi ai tre Paesi che emettono più co2
top3 <- rbind(country.top3,co2.top3,ele.top3)
top3
write.csv(top3, "top3.txt")


###


# un' analisi sulle emissioni di CO2 non considerando i singoli Paesi ma le macroaree
# dunque ho sommato tutte le emissioni di tutti i Paesi appartenenti a ciascuna macroarea
# l'analisi prevede la costruzione di una tabella e di un grafico a barre

factor(Energy$region)

paesi.africa <- Energy$co2[Energy$region=="africa"]
somma.africa <- sum(paesi.africa)
paesi.americas <- Energy$co2[Energy$region=="americas"]
somma.americas <- sum(paesi.americas)
paesi.asia <- Energy$co2[Energy$region=="asia"]
somma.asia <- sum(paesi.asia)
paesi.europe <- Energy$co2[Energy$region=="europe"]
somma.europe <- sum(paesi.europe)

#tabella per somma emissioni co2 all'interno delle quattro regioni
vet.somme <- c(somma.africa,somma.americas,somma.asia,somma.europe)
vet.somme
mat.somme1 <- rbind(names(tabella),vet.somme)
mat.somme1
mat.somme2 <- matrix(mat.somme1,nrow =2, ncol =4)
mat.somme2
write.csv(mat.somme2, "mat.somme2.txt")

#grafico a barre: Emissioni di CO2 per ciascuna macroarea geografica
vet.etichette <- paste(names(tabella),"(",vet.somme,"Mt )")
barplot(vet.somme, names.arg = vet.etichette, main ="Emissioni di CO2 per ciascuna macroarea geografica", col="powderblue")

###

