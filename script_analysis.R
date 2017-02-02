##############################
#Exploring patterns in 40 years
#of french presidential election
###########################

#Data are fom:
#Résultats des élections présidentielles 1965-2012, PRESIDENTIELLES1965-2012.zip [fichier informatique], 
#Grenoble : Banque de Données Socio-Politiques, Paris : Ministère de l’Intérieur, France [producteurs], 
#Centre de Données Socio-Politiques [diffuseur], 2014.

#Original data available here: https://www.data.gouv.fr/fr/datasets/elections-presidentielles-1965-2012-1/
#Modified data used in this post available here: 

#Shapefiles are available here: http://gadm.org/
#select France, R -SpatialPolygon and Level 2 data

#load the libraries
library(plyr)
library(stringr)
library(reshape2)
library(tidyverse)
library(sp)
library(rgdal)
library(viridis)

#set working directory
setwd("Desktop/Blog/Election/")
#list files
li<-list.files(pattern="cdsp")

#loop through the files and format them
dat<-NULL
for(i in 1:9){
  tmp<-readLines(li[i])
  tmp<-gsub("'","",tmp)
  tmp<-strsplit(tmp,split=",")
  name<-tmp[1]
  tmp<-ldply(tmp[-1],function(x) x)
  names(tmp)<-name[[1]]
  names(tmp)[7]<-"Blancs"
  tmp$Year<-substr(li[i],11,14)
  tmp[names(tmp)[4:dim(tmp)[2]]]<-sapply(tmp[names(tmp)[4:dim(tmp)[2]]],as.numeric)
  tmp%>%
    mutate(Abstention=(Inscrits-Votants)+Blancs)%>%
    rename(Dep_ID=`Code département`)%>%
    select(-(5:7))%>%
    gather(Candidats,Nombre,-(Dep_ID:Inscrits),-Year)%>%
    group_by(Dep_ID,Candidats,Year)%>%
    summarise(Nombre=sum(Nombre),Inscrits=sum(Inscrits))%>%
    mutate(Prop=Nombre/Inscrits)->tmp2
  tmp2$Candidats<-gsub(" \\(.*$","",tmp2$Candidats)
  tmp2$Dep_ID<-str_pad(tmp2$Dep_ID,2,side="left",pad="0")
  dat<-rbind(dat,tmp2)
}
#put candidate names in small caps
dat$Candidats<-str_to_title(dat$Candidats)
#correct one candidate name
dat$Candidats[grep("Villier",dat$Candidats)]<-"de Villiers"
#add the general political movement of each candidate
cand_list<-read.table("courant.csv",sep=",",header = TRUE,stringsAsFactors = FALSE)
#merge with the data
dat2<-merge(dat,cand_list,by.x=c("Candidats","Year"),by.y=c("Name","Year"))

#get total number of potential voters per year
dat2%>%
  filter(Candidats=="Abstention")%>%
  group_by(Year)%>%
  summarise(Tot_inscrit=sum(Inscrits))->tot_inscrit


dat2%>%
  group_by(Year,Courant)%>%
  summarise(Nombre=sum(Nombre))%>%
  left_join(tot_inscrit,by="Year")%>%
  mutate(Prop=Nombre/Tot_inscrit)->dat_time

col_courant<-c("#333300","#ff9900","#99ccff","#ff99ff","#0000ff","#33cc33","#001a33","#ff0000","#ff6666")#choose better colors
#plot per movement
ggplot(dat_time,aes(x=Year,y=Prop,color=Courant))+geom_path(size=1.5)+
  scale_color_manual(values=col_courant)+labs(y="Proportion of voters")


#load shapefile
fr<-readRDS("FRA_adm2.rds")

#re-arrange data for interation by courant into the polygon dataframe
dat2$Courant<-gsub(" ","_",dat2$Courant)
dat3<-dcast(dat2,formula = Dep_ID~Courant+Year,value.var = "Prop",fun.aggregate = sum)
#merge to the spatial data
fr2<-merge(fr,dat3,by.x="CCA_2",by.y="Dep_ID")
#plot the spatial patterns of abstention over the years
spplot(fr2,zcol=paste("Abstention",c(1965,1969,1974,1981,1988,1995,2002,2007,2012),sep="_"),col.regions=rev(viridis(100)),col="white",
       names.attr=c("1965","1969","1974","1981","1988","1995","2002","2007","2012"),main="Proportion of Abstention")
#plot the spatial patterns of far right vote over the years
spplot(fr2,zcol=paste("Extreme_Droite",c(1965,1974,1988,1995,2002,2007,2012),sep="_"),col.regions=rev(inferno(100)),col="white",
       names.attr=c("1965","1974","1988","1995","2002","2007","2012"),main="Proportion of vote for the Far Right")

#look at temporal patterns per regions and focusing on the main courant
dat2<-merge(dat2,fr@data[,c("NAME_1","NAME_2","CCA_2")],by.x="Dep_ID",by.y="CCA_2")
#first get per region and year the total number of potential voter
dat2%>%
  filter(Candidats=="Abstention")%>%
  group_by(Year,NAME_1)%>%
  summarise(Tot_inscrit=sum(Inscrits))->tot_inscrit_reg
#then add this to the data and compute the proportions
dat2%>%
  group_by(Year,NAME_1,Courant)%>%
  summarise(Nombre=sum(Nombre))%>%
  left_join(tot_inscrit_reg,by=c("Year","NAME_1"))%>%
  mutate(Prop=Nombre/Tot_inscrit)%>%
  filter(Courant%in%c("Abstention","Centriste","Droite","Extreme_Droite","Socialiste"))->dat_reg
dat_reg$NAME_1<-gsub("-","-\n",dat_reg$NAME_1)
#ggplot2 thene
theme_el<-theme(legend.position = "bottom",panel.background = element_rect(fill="white",color=NA),
                panel.grid.major.y = element_line(linetype="dashed",color="black"),
                panel.border=element_rect(color="black",fill=NA),axis.text.x=element_text(size=7),
                strip.background = element_rect(fill = "grey85",colour = "grey20"), legend.key = element_rect(fill = "white",colour = NA))
#the plot
ggplot(dat_reg,aes(x=Year,y=Prop,color=Courant))+
  geom_path(size=2)+facet_grid(Courant~NAME_1)+
  scale_x_continuous(breaks=c(1970,1990,2010))+
  theme_el

