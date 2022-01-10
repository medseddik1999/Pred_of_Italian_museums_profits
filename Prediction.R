# libriries ----------
library(dplyr)
library(ggplot2) 
#library(plyr) ##run plyr before dplyr
library(forcats)
library(GGally)
library(hrbrthemes) 
library(VIM)
library(FactoMineR)
library(missMDA)
library(naniar) 
library(kohonen)
library(maptools)
library(sp)
library(rgdal) 
library(fBasics)  
library(e1071)
library(caTools)
library(class)  
library(FNN) 
library(tune) 
library(rpart) 
require(ipred) #bagging
require(e1071) #svm
require(mlbench) 
require(rpart) #recursive partitiong
require(party)  
library(ROCR) 
library(pROC)  
library(plotROC)  
library(plotly) 
library(gpclib)
library(maps)  
require(ggplot2) 
library(mapproj) 
library(RColorBrewer)
require(tidyverse) 


### data importation ------
data1=read.csv("/Users/midou/Desktop/Gruzeni /data1.csv")  
an13=read.csv("/Users/midou/Desktop/Gruzeni /an13.csv")
in13=read.csv("/Users/midou/Desktop/Gruzeni /in13.csv")  
colnames(in13)[8]<-"codcliente"  
data11 <- merge(an13, data1,  by =  "codcliente", all= T) 
data2 <- merge(data11, in13, by.x = "codcliente", by.y = "codcliente", all=T)
data11=data11 %>% mutate(age=2014-as.numeric(data_nascita)) 
#price analysis-----
##femme-homme###  
library(plyr)
stats13an=an13 %>% filter(sesso =='M' | sesso =='F' ) 
Ms=ddply(stats13an, "sesso", summarise, importo=mean(importo))  
p=ggplot(stats13an,aes(x=importo , color=sesso))+
  geom_density(aes(x=importo))+geom_vline(data=Ms, aes(xintercept=importo, color=sesso),linetype="dashed")+
  ggtitle("price distribution by gender ") +xlab('Price')  
p
###nouvo aboone old ###
Ag=an13 %>% group_by(importo ,nuovo_abb) %>% count()
agen=ggplot(Ag ,aes(x=importo , color=nuovo_abb))+geom_boxplot()+
  ggtitle('the price distribution according to the newness of the subscription')+xlab("Price") 
agen  

###price in the twons##  
tw=an13 %>% group_by(comune ) %>% summarise(impo_mean=mean(importo))  
twon=ggplot(tw,aes(x=impo_mean)) +geom_histogram(aes(y=..density..),color="Black",fill='blue2') + 
  geom_density(alpha=.2, fill="#FF6666") +ggtitle("Price distribution in the comune area")+xlab("Price")  
##price and agencia type## 
agtip=an13 %>% group_by(agenzia_tipo) %>% summarise(menp=mean(importo))  
agtip1=agtip %>% arrange(desc(menp)) 
pag=ggplot(agtip1 ,aes(x=fct_reorder(agenzia_tipo,menp ,.desc = TRUE) ,y=menp)) + geom_bar(stat = "identity" ,color='Black',fill='honeydew2')+
                coord_flip() 
pag=pag+ggtitle("the distribution of prices in the types of agency ") +xlab("Price")

library(gridExtra) 

grid1=grid.arrange(p,agen) 
grid2=grid.arrange(twon,pag) 


##removing some data_set 
rm(ag)
rm(agtip) 
rm(agtip)  
rm(agen)
rm(Ms)


## others descriptive analysis -----  
###nombre de visit___age_sex__price__churn colleration ----
####age in 2014   
data2=data2 %>% mutate(age=2014-as.numeric(data_nascita))  
data2age=data2 %>% filter(age>3 & age<102 ) 
visitdata=in13 %>% group_by(codcliente) %>% count()     
colnames(visitdata)[2]<-"nombre_de_visit" 
fr_cor=data.frame(age=data2age$age ,nonchurn=data2age$si2014 ,codcliente=data2age$codcliente,
                  price= data2age$importo.x,
                   sexe=ifelse(data2age$sesso=="M",1,ifelse(data2age$sesso=="F",0,NA)))
                                                                                                         
fr_cor <- fr_cor[-which(duplicated(fr_cor)), ] 
visitdata$codcliente=unique(visitdata$codcliente, incomparables = FALSE) 
fr_cor$codcliente=unique(fr_cor$codcliente, incomparables = FALSE)
toto=merge(fr_cor, visitdata)  
toto=toto[,2:6] 
ptcor=ggcorr(toto)   
ptcor=ptcor+ggtitle("Correlation Matrix ")+theme_dark()
ptcor
rm(toto) 
rm(fr_cor) 
rm(visitdata)  
## how are cherners ?## ----
data_churn=data11 %>% filter(si2014==0 & age>3 & age<102)      
churnage=ggplot(data_churn , aes(x=age , color=sesso)) +geom_density()  
twt=ggplot(data_churn) +geom_density2d(aes(x=data_churn$importo , y=data_churn$age , color=data_churn$nuovo_abb) )  
twt = twt+ggtitle("concentration of churners by age and card price ")+xlab("Price")+ylab("Age")
twt
###NA analysis and data preparing  ------- 

dim(na.omit(data2)) 
require(ggplot2)
gg_miss_var(data11) 
gg_miss_var(in13)  
data11= subset(data11, select = -c(professione,X.y,X.x) )  
colnames(data11)
colnames(data2) 
data2=subset(data2 , select =-c(professione,X.y,X.x,X) ) 

####clusturing ------  
# Determine number of clusters
visitdata=in13 %>% group_by(codcliente ) %>% count()         
visitdata2=in13 %>% group_by(codcliente , museo)  %>% count()
mv=visitdata2 %>% group_by(codcliente)%>% count() 
colnames(mv)[2]<-'num_meso_visited' 
mprice=in13 %>% group_by(codcliente) %>% summarise(mean(importo))  
colnames(mprice)[2]<-'mean_price_Meso'
colnames(visitdata)[2]<-"nombre de visit" 
visit1=merge(visitdata,mv)
visi2=merge(visit1,mprice) 

culdata=merge(visi2, data11)   
culdata=culdata %>% mutate(churn=ifelse(si2014==0,1,0)) 
culdata %>% group_by(churn) %>% count()
culdata$sconto=ifelse(culdata$sconto=='NESSUNO SCONTO',0,1)  
culdata$nuovo_abb=ifelse(culdata$nuovo_abb=='NUOVO ABBONATO',1,0) 
culdata$sesso=ifelse(is.na(culdata$sesso),NA,ifelse(culdata$sesso=="M",1,0))  
culdata=culdata %>% mutate(ifturin=ifelse(comune=="TORINO" ,1,0)) 
colnames(culdata) 
culdata=culdata[,c(1,2,3,7,8,12,13,16,17,18,20,23,24,25,26)]   
culdata %>% group_by(churn) %>% count()
require(kohonen)
library(fastDummies)
library(DMwR2)
require(tidyverse)
require(magrittr)
library(RColorBrewer)
library(stats)
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

 
#culdata=na.omit(culdata) 
colnames(culdata) 
set.seed(123) 
culdata1=culdata[,c("importo" ,"nombre de visit", "churn","age","sesso","ifturin" ,"num_meso_visited" , "nuovo_abb")] 
som_m=culdata1[sample(nrow(culdata), 40000),]   
data_train=som_m[,c("nombre de visit", "churn","sesso","ifturin", "nuovo_abb")] 
data_train_matrix <- as.matrix(scale(data_train))
som_grid <- somgrid(xdim = 10, ydim=10, topo="hexagonal")  
som_model <- som(data_train_matrix,  
                 grid=som_grid, 
                 rlen=500, 
                 alpha=c(0.05,0.01), 
                 keep.data = FALSE )


plot(som_model, type="codes")
#plot(som_model, type = "property", property = as.data.frame(som_model$codes)[,4]
     #, #main=names(som_model$data)[4], palette.name=coolBlueHotRed) 


titi=som_model$codes  
titi=titi[1] 
titi=titi[[1]] 
titi=as.matrix(titi)
mydata=titi  


wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))  
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}  
plot(wss)  


som_cluster <- cutree(hclust(dist(mydata)), 7)          
culA=plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters")+
      add.cluster.boundaries(som_model, som_cluster)

Churncol=plot(som_model, type="property",property = as.data.frame(som_model$codes)[,4] ,
              bgcol = pretty_palette[som_cluster], main = "Clusters Churn ")+add.cluster.boundaries(som_model, som_cluster) 
   
   
grid3=grid.arrange(culA,Churncol)  

#som_data=culdata
#som_data=na.omit(som_data)
#som_data =som_data %>% mutate(sesso=ifelse(sesso=='M','YES','No'))  
#som_data =som_data %>% mutate(churn=ifelse(churn==1,'YES','No'))  
#som_data =som_data %>% mutate(`nombre de visit`=ifelse(`nombre de visit`>15,'Lot','Less')) 
#som_data =som_data %>% mutate(sconto=ifelse(sconto==1,'YES','No'))
#som_data=som_data %>% 

####Geo importing##-----

 
shp <- readOGR(dsn = "/Users/midou/Desktop/Gruzeni /CAP_NordOvest_Shp", layer="cap_NO")
#head(shp) 

####cap
data11$cap<-as.numeric(as.character(data11$cap))
data11$cap=ifelse(data11$cap<10000  ,NA, data11$cap)  
data11$cap=ifelse(data11$comune=="TORINO" & is.na(data11$cap) , 10100, data11$cap)
data11$cap_N=ifelse(data11$cap>=10010 & data11$cap<=46100 ,data11$cap,0) 
colnames(data11)[22]="IT_CAP"
###### GEO ANAlysis -----

client_in_to=data11 %>% group_by(IT_CAP) %>% count()
client_in_to=client_in_to[-1,]
colnames(client_in_to)[2]<-'client' 
shp@data=data.frame(join(shp@data,client_in_to,by="IT_CAP")) 
spplot(shp,"client")  
gpclibPermit()
shp=spTransform(shp,CRS("+init=epsg:4326")) 

map_point = fortify(shp, region="IT_CAP") 
map_point[6]
colnames(map_point)[6]="IT_CAP"
map_point_data = join(map_point, shp@data, by="IT_CAP") 

#title='geographical distribution of card holers in Turin' 
cardholders=ggplot(data = map_point_data, aes(x=long, y=lat, group = group ))+geom_polygon(aes(fill=client), color = 'black')+
  scale_fill_viridis_c(name='Client frenquency', option = "plasma", trans = "sqrt")+theme(
    text = element_text(color = "#22211d"), 
    panel.background = element_rect(fill = "gray88", color = NA), 
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    
    panel.grid.major = element_line(color = "grey45"),
    panel.grid.minor = element_line(color = "grey25"),
    
    legend.background = element_rect(fill = "gray88", color = NA)
  )+ggtitle("Geographical distribution of card holers in Turin")
cardholders  

data11=data11 %>% mutate(churn=ifelse(si2014==0,1,0))
client_in_churn=data11 %>% group_by(IT_CAP) %>%summarise( chi=sum(churn))  
didi=merge(client_in_churn,client_in_to , by="IT_CAP")  
didi$chi=as.numeric(didi$chi)  
didi$client=as.numeric(didi$client)   

didi=didi %>% mutate(prop=chi/client) 
didi$prop=didi$prop*100
didi=didi[,-(2:3)] 

shp@data=data.frame(join(shp@data,didi,by="IT_CAP")) 
map_point_data = join(map_point, shp@data, by="IT_CAP") 
churngr=ggplot(data = map_point_data, aes(x=long, y=lat, group = group )) +geom_polygon(aes(fill=prop), color = 'black' , 
                                                                                size=0.05, alpha=0.9)+ 
  scale_fill_gradientn(
    colours=brewer.pal(7,"BuPu"), name="Churners %",  
    guide=guide_legend( keyheight = unit(3, units = "mm"), 
                        keywidth=unit(12, units = "mm"), 
                        title.position = 'top', 
                        label.position = "bottom") 
  )+ggtitle( "geographical distribution of churners by sector " )+theme_void()+
  theme(
    legend.position = c(1, 0.5),
    legend.direction = "vertical",
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "antiquewhite", color = NA), 
    panel.background = element_rect(fill = "antiquewhite", color = NA), 
    legend.background = element_rect(fill = "antiquewhite3", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) +coord_map()

 client_in_to

length(shp)
length(shp)
length(client_in_to$client) 


####GLM####   

colnames(culdata)[2]="num_visit"
summary(culdata) 
culdata=culdata[,c(1,2,3,4,5,7,13,14,15)] 
culdata=culdata %>% mutate(visit_per_museom=num_visit/num_meso_visited) 
set.seed(123) 
sample_split<- sample.split(Y = culdata$churn, SplitRatio = 0.56)  
training <- subset(x = culdata, sample_split == TRUE)
testing <- subset(x = culdata, sample_split == FALSE)
summary(training)
logit_churn=glm(churn ~ age+sesso +ifturin +visit_per_museom +sconto , data =training ,family = "binomial")    
summary(logit_churn)

pr<- predict(logit_churn, newdata = testing, type = "response")    
pr=ifelse(pr >=0.5 ,1 ,0)  
confusionMatrix(factor(pr), factor(testing$churn), positive = as.character(1)) 

coef(logit_churn) 
exp(coef(logit_churn)) 
dita=with(training,data.frame(age=mean(age), sesso=mean(sesso), 
                              visit_per_museom=mean(visit_per_museom), 
                              ifturin=mean(ifturin) , sconto=mean(sconto)))
Dotta=as.data.frame(exp(coef(logit_churn))[2:6])  
Dotta$y <- rownames(Dotta)  
colnames(Dotta)<- c("x", "y")  
Dotta
E<- ggplot(Dotta, aes(x=x, y=y))+
  geom_segment( aes(y=y, yend=y, x=0, xend=x ))

E <- ggplot(Dotta, aes(x=x, y=y))+ 
  geom_segment( aes(y=y, yend=y, x=0, xend=x ), color="red", size=2)

E <- ggplot(Dotta, aes(x=x, y=y))+
  geom_segment( aes(y=y, yend=y, x=0, xend=x ), color="red", size=2)+
  geom_point( color= "grey", size=3) 

E <- ggplot(Dotta, aes(x=x, y=y)) +
  geom_segment( aes(y=y, yend=y, x=0, xend=x ), color="lightcoral", size=2) +
  geom_point( color= "midnightblue", size=3) +
  xlab("") +
  ylab("Impact") +
  ggtitle("Impact of exogene variable") 
E
### reppp ------ 
# Installing Packages

colnames(culdata) 
culdata=culdata[,c(1,2,3,4,5,7,13,14,15)]   
culdata %>% mutate(churn=as.factor(churn)) 


#classifier_knn <- knn(train = train_scale, test = test_scale,cl = train_cl$churn, k = 5) 

fit_rp <- rpart(factor(churn) ~ age+sesso +ifturin +visit_per_museom +sconto, data=training)      
rp.pred <- predict(fit_rp, newdata=testing , type = "prob")       
rp.t=ifelse(predict(fit_rp, newdata=testing , type = "prob")[,2] >=0.5 ,1,0)
confusionMatrix(factor(rp.t), factor(testing$churn), positive = as.character(1))   

#####tree 
fit_tree <- ctree(churn~age+sesso +ifturin +num_meso_visited +sconto+importo, data=training)     
tr.pred <-(1- unlist(treeresponse(fit_tree, testing), use.names=F)[seq(1,nrow(testing)*2,2)])    
tree.classi=ifelse(tr.pred >=0.5 ,1,0) 
confusionMatrix(factor(tree.classi), factor(testing$churn), positive = as.character(1))  


###ROCR ----- 

log_prob= pr 
train_cl$probtr=log_prob  
perf_lg<- roc(testing$churn ~ log_prob)  
perf_rp<-roc(testing$churn ~ predict(fit_rp, newdata=testing , type = "prob")[,2] )  
perf_tr<-roc(testing$churn ~ tr.pred)  
plot(perf_lg ,legacy.axes=TRUE ,xlab='False positive rate',ylab='True positive rate',main="ROC curves comparing classification performance ",col="2")    
lines(perf_rp , col="4")  
lines(perf_tr , col="mediumaquamarine")   
legend("bottom",pch = 18,c("ROC GLM","ROC RP","ROC Tree") ,col=c("2","4","mediumaquamarine"))  
#plot(perf_lg,legacy.axes=TRUE,percent=TRUE ,xlab='False positive rate',ylab='True positive rate',col=4, add=TRUE) 

A=ggplot()+geom_density(aes(x = predict(logit_churn, newdata = testing, type = "response") 
                          ,color='Probabilty churn Glm')) + 
  geom_density(aes(x = 1-predict(logit_churn, newdata = testing, type = "response"),color='Probabilty non-churn Glm'))+ 
                            labs(x="Probabilty") +ggtitle('Distribution of pretcted probabilites GLM')    


B=ggplot()+geom_density(aes(x =tree_prob 
                          ,color='Probabilty churn Tree')) + 
  geom_density(aes(x = 1-tree_prob  
                   ,color='Probabilty non-churn Tree')) + 
  labs(x="Probabilty") +ggtitle('Distribution of pretcted probabilites Tree') 



C=ggplot()+geom_density(aes(x =predict(fit_rp, newdata=testing , type = "prob")[,2]
                          ,color='Probabilty churn RP')) + 
  geom_density(aes(x = predict(fit_rp, newdata=testing , type = "prob")[,1]  
                   ,color='Probabilty non-churn RP')) + 
  labs(x="Probabilty") +ggtitle('Distribution of pretcted probabilites RP')

A

B

C
####Expected profts --------- 
testing$profit=testing$importo-(testing$num_visit*5)  
testing$Eprofit_glm=((1-predict(logit_churn, newdata = testing, type = "response")) *testing$profit)  
testing$Eprofit_rp=((1-predict(fit_rp, newdata=testing , type = "prob")[,2])*testing$profit)  
testing$Eprofit_tr=((1-tr.pred)*testing$profit)  



colors <- c("real Profit" = "red", "RP expected profit" = 15, "tree expected profit" = 22, "glm expected profit"=1 )
ggplot(testing, aes(x=1:length(testing$profit)))+geom_line(aes(y=cumsum(sort(testing$profit ,decreasing = TRUE)),color="Real Profit"))+
                  geom_line(aes(y = cumsum(sort(Eprofit_rp,decreasing = TRUE)),color="RP expected profit")) + 
  geom_line(aes(y = cumsum(sort(Eprofit_tr , decreasing = TRUE)),color="tree expected profit"))+
                      geom_line(aes( y =cumsum(profit_glm),color="glm expected profit")) + ggtitle('Real Profit VS Expected Profit') + 
                                labs(x="number of customers" ,y="Profit cumulated Value ") 


#labs(col=c("real Profit","RP expected profit","tree expected profit","glm expected profit"))








 




