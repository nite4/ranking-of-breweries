#Ranking - wersja ostateczna funkcji

library(clusterSim)
setwd("/Users/magdalenanitefor/Documents/V\ semestr/Statystyczna\ analiza\ danych")

#krafty<-read.csv("data_final.csv", sep=";", dec=".", row.names=1, header=TRUE)
#obejrzyjmy dane
dim(krafty)
str(krafty)
summary(krafty)

#macierz korelacji
round(cor(krafty),2)

#współczynnik zmienności cech
wz <- c()
suma_wz<-0
for (i in 1:6)
{
  wz[i] = sd(krafty[,i])/mean(krafty[,i])
  suma_wz = suma_wz+wz[i]
}
names(wz)<-colnames(krafty)
wz

#wagi informacyjne
wagi_wz<-c()
for (i in 1:6)
  wagi_wz[i] = round(wz[i]/suma_wz,2)
wagi_wz
krafty_std_wwz<-krafty_std*sqrt(wagi_wz)

#wagi eksperckie
wagi_e<-c(0.25,0.2,0.2,0.15,0.1,0.15)
krafty_std_we<-krafty_std*sqrt(wagi_e)

#standaryzacja i normalizacja
krafty_std<-scale(krafty)
krafty_norm<-data.Normalization(krafty)

#MSS bez wag
si1<-c()
for(i in 1:20){
  si1[i]<-mean(krafty_std[i,])
}
names(si1)<-rownames(krafty)
MSS<-(si1-min(si1))/(max(si1-min(si1)))
#MSS<-sort(MSS,decreasing=TRUE)
MSS

#MSS z wagami eksperckimi
si2<-c()
for (i in 1:20)
{
  for (j in 1:6)
    si2[i]<-mean(krafty_std_we[i,])
}
names(si2)<-rownames(krafty)
MSS_we<-(si2-min(si2))/(max(si2-min(si2)))
#MSS_we<-sort(MSS_we,decreasing=TRUE)
MSS_we

#MSS z wagami informacyjnymi
si3<-c()
for (i in 1:20)
{
  for (j in 1:6)
    si3[i]<-mean(krafty_std_wwz[i,])#tu coś jest nie tak jak być powinno
}
names(si3)<-rownames(krafty)
MSS_wwz<-(si3-min(si3))/max(si3-min(si3)) 
#MSS_wwz<-sort(MSS_wwz,decreasing=TRUE)
MSS_wwz

#metoda Hellwiga bez wag
wzorzec1<-c()
wzorzec1[1]<-max(krafty_std[,1])
for(i in 1:6) 
{
  wzorzec1[i]<-max(krafty_std[,i])
}
di0<-c()
for (i in 1:20) 
{
  dane<-rbind(krafty_std[i,],wzorzec)
  di0[i]<-dist(dane)
}
d0<-mean(di0)+2*sd(di0)
sih1<-1-di0/d0
names(sih1)<-rownames(krafty)
#H1<-sort(sih1,decreasing=TRUE)
H1

#metoda Hellwiga z wagami eksperckimi
wzorzec2<-c()
wzorzec2[1]<-max(krafty_std_we[,1])
for(i in 1:6) 
{
  wzorzec2[i]<-max(krafty_std_we[,i])
}
di02<-c()
for (i in 1:20) 
{
  dane<-rbind(krafty_std_we[i,],wzorzec)
  di02[i]<-dist(dane)
}
d02<-mean(di02)+2*sd(di02)
sih2<-1-di02/d02
names(sih2)<-rownames(krafty)
#H2<-sort(sih2,decreasing=TRUE)
H2

#metoda Hellwiga z wagami informacyjnymi
wzorzec3<-c()
wzorzec3[1]<-max(krafty_std_wwz[,1])
for(i in 1:6) 
{
  wzorzec3[i]<-max(krafty_std_wwz[,i])
}
di03<-c()
for (i in 1:20) 
{
  dane<-rbind(krafty_std_wwz[i,],wzorzec)
  di03[i]<-dist(dane)
}
d03<-mean(di03)+2*sd(di03)
sih3<-1-di03/d03
names(sih3)<-rownames(krafty)
#H3<-sort(sih3,decreasing=TRUE)
H3

#TOPSIS bez wag
krafty_norm1<-data.Normalization(krafty)
wzorzect1<-c()
antyt1<-c()
for (i in 1:6)
{
  krafty_norm1[,i]<-krafty_norm1[,i]/sqrt(sum(krafty_norm1[,i]^2))
  wzorzect1[i]<-max(krafty_norm1[,i])
  antyt1[i]<-min(krafty_norm1[,i])
}
di_wzt1<-c()
di_antywzt1<-c()
for(i in 1:20)
{
  dane<-rbind(krafty_norm1[i,],wzorzect1)
  di_wzt1[i]<-dist(dane)
}
for(i in 1:20)
{
  dane<-rbind(krafty_norm1[i,],antyt1)
  di_antywzt1[i]<-dist(dane)
}
T1<-di_antywzt1/(di_antywzt1+di_wzt1)
names(T1)<-rownames(krafty)
#T1<-sort(T1, decreasing=TRUE)
T1

#TOPSIS z wagami eksperckimi
krafty_norm2<-wagi_e*data.Normalization(krafty)
wzorzect2<-c()
antyt2<-c()
for (i in 1:6)
{
  krafty_norm2[,i]<-krafty_norm2[,i]/sqrt(sum(krafty_norm2[,i]^2))
  wzorzect2[i]<-max(krafty_norm2[,i])
  antyt2[i]<-min(krafty_norm2[,i])
}
di_wzt2<-c()
di_antywzt2<-c()
for(i in 1:20)
{
  dane<-rbind(krafty_norm2[i,],wzorzect2)
  di_wzt2[i]<-dist(dane)
}
for(i in 1:20)
{
  dane<-rbind(krafty_norm2[i,],antyt2)
  di_antywzt2<-dist(dane)
}
T2<-di_antywzt2/(di_antywzt2+di_wzt2)
names(T2)<-rownames(krafty)
#T2<-sort(T2, decreasing=TRUE)
T2

#TOPSIS z wagami informacyjnymi
krafty_norm3<-wagi_wz*data.Normalization(krafty)
wzorzect3<-c()
antyt3<-c()
for (i in 1:6)
{
  krafty_norm3[,i]<-krafty_norm3[,i]/sqrt(sum(krafty_norm3[,i]^2))
  wzorzect3[i]<-max(krafty_norm3[,i])
  antyt3[i]<-min(krafty_norm3[,i])
}
di_wzt3<-c()
di_antywzt3<-c()
for(i in 1:20)
{
  dane<-rbind(krafty_norm3[i,],wzorzect3)
  di_wzt3[i]<-dist(dane)
}
for(i in 1:20)
{
  dane<-rbind(krafty_norm3[i,],antyt3)
  di_antywzt3<-dist(dane)
}
T3<-di_antywzt3/(di_antywzt3+di_wzt3)
names(T3)<-rownames(krafty)
#T3<-sort(T3, decreasing=TRUE)
T3

#interpretacja wyników rankingu
grupy<-function(rank)
{
  m<-matrix(0,20,4)
  for (i in 1:20)
  {
    if (rank[i]>(mean(rank)+sd(rank)))
      m[i,1] = m[i,1]+1
    else if (rank[i]<=(mean(rank)+sd(rank)) && rank[i]>mean(rank))
      m[i,2] = m[i,2]+1
    else if (rank[i]<=mean(rank) && rank[i]>(mean(rank)-sd(rank)))
      m[i,3] = m[i,3]+1
    else if (rank[i]<(mean(rank)-sd(rank)))
      m[i,4] = m[i,4]+1
  }
  print (m)
  return (m)
}

gr<-matrix(20,4)
rownames(gr)<-c("AleBrowar","Artezan","BroKreacja","FunkyFluid","Golem",
                "InneBeczki","Maryensztadt","Olimp","Pinta","PiwnePodziemie",
                "Piwojad","Podgorz","PracowniaPiwa","Raduga","ReCraft","Rockmill",
                "StuMostow","TrzechKumpli","Widawa","ZiemiaObiecana")
colnames(gr)<-c("grupa I","grupa II","grupa III", "grupa IV")
gr<-grupy(MSS)
gr<-gr+grupy(MSS_we)
gr<-gr+grupy(MSS_wwz)
gr<-gr+grupy(sih1)
gr<-gr+grupy(sih2)
gr<-gr+grupy(sih3)
gr<-gr+grupy(T1)
gr<-gr+grupy(T2)
gr<-gr+grupy(T3)
gr