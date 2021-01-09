## transmission rate betweene eco system
library(ggplot2)
library(dplyr)
library(tidytree)
library(reshape2)
library(tidyr)

df<-read.table("seg7/seg7_discrete_flyway_rates_combined.log", header = TRUE, sep = "\t" )
df2<- df %>% select(contains('flyway.rates'),state)
## reshape, convert column name to category column
df3<-melt(df2,id.vars="state",
          variable.name ="pattern",
          value.name="rates")


##filter the statistical support rate
df4<-df3 %>% filter(pattern %in% seg7)
df4$category<-ifelse(grepl("OB",df4$pattern),"between ATL","within ATL")

df5<-df4 %>% group_by(state,category) %>%
  dplyr::summarise(mean_rate = mean(rates))
## density plot
ggplot(df4,aes(x=rates,fill=category))+
  geom_density(alpha=0.6)+
  theme_classic()

BF_label<- paste("BF == ", BF)
##calculate the Bayes Factor 
df6<-dcast(df5,state~category)
ggplot(df6,aes(x=`within ATL`,y=`between ATL`))+
  geom_point(color = "steelblue")+
  geom_abline(slope=1, intercept = 0, colour = "red")+
  theme_classic()+
  coord_equal()+
  annotate(geom = 'text', x = -Inf, y = Inf, label = BF_label, hjust = -0.2, vjust = 1, parse = TRUE,fontface = 'bold',size=4)+
  annotate(geom ='text', x = Inf, y = Inf, label="x=y",hjust=3,vjust=6, color = 'red', size = 5,)


BF<-function(df){
  p1<-length(which(df6$`between ATL`>df6$`within ATL`))
  p2<-length(which(df6$`between ATL`< df6$`within ATL`))
  if(p1/p2 >=1){
    post_odds<-p1/p2
  } else {
    post_odds<-p2/p1
  }
  return(round(post_odds,digits=3))
}

BF<-BF(df6)
#################################################################
## wrap to function
## for density plot dataframe
mean_rate<-function(rate_file,support){
  df<-read.table(rate_file, header = TRUE, sep = "\t" )
  df2<- df %>% 
    select(contains('flyway.rates'),state)
  df3<-melt(df2,id.vars="state",
            variable.name ="pattern",
            value.name="rates")
  df4<-df3 %>% filter(pattern %in% support)
  df4$category<-ifelse(grepl("OB",df4$pattern),"Outside to ATL-flyway","ATL-flyway to ATL-flyway")
  df5<-df4 %>% group_by(state,category) %>%
    dplyr::summarise(mean_rate = mean(rates))
  return(df5)
}
seg1<-c("flyway.rates.GU.OB","flyway.rates.OB.SB","flyway.rates.OB.DD","flyway.rates.SB.GU","flyway.rates.OB.PO","flyway.rates.DD.OB","flyway.rates.DD.PO","flyway.rates.DD.SB")
seg2<-c("flyway.rates.OB.DD","flyway.rates.SB.GU","flyway.rates.OB.SB","flyway.rates.DD.SB","flyway.rates.DD.PO","flyway.rates.DD.OB","flyway.rates.OB.PO","flyway.rates.DD.GU")
seg3<-c("flyway.rates.OB.SB","flyway.rates.OB.PO","flyway.rates.OB.DD","flyway.rates.SB.GU","flyway.rates.DD.OB","flyway.rates.DD.SB","flyway.rates.SB.DD")
seg5<-c("flyway.rates.OB.PO","flyway.rates.OB.SB","flyway.rates.OB.DD","flyway.rates.SB.GU","flyway.rates.GU.OB","flyway.rates.DD.SB","flyway.rates.DD.PB")
seg7<-c("flyway.rates.OB.PO","flyway.rates.OB.SB","flyway.rates.OB.DD","flyway.rates.SB.GU","flyway.rates.GU.OB","flyway.rates.SB.OB","flyway.rates.PO.OB","flyway.rates.DD.OB")
seg8<-c("flyway.rates.GU.OB","flyway.rates.OB.SB","flyway.rates.OB.DD","flyway.rates.SB.GU","flyway.rates.OB.OB","flyway.rates.DD.SB","flyway.rates.DD.OB")

seg1_meanrate<-mean_rate("seg1/seg1_flyway_rates_combined.log",seg1)
seg2_meanrate<-mean_rate("seg2/seg2_discrete_flyway_rates_combined.log",seg2)
seg3_meanrate<-mean_rate("seg3/seg3_discrete_flyway_rates_combined.log",seg3)
seg5_meanrate<-mean_rate("seg5/seg5_discrete_flyway_rates_combined.log",seg5)
seg7_meanrate<-mean_rate("seg7/seg7_discrete_flyway_rates_combined.log",seg7)
seg8_meanrate<-mean_rate("seg8/seg8_discrete_flyway_rates_combined.log",seg8)

## combine multiple df
seg1_meanrate$segment<-"PB2"
seg2_meanrate$segment<-"PB1"
seg3_meanrate$segment<-"PA"
seg5_meanrate$segment<-"NP"
seg7_meanrate$segment<-"MP"
seg8_meanrate$segment<-"NS"
internal_meanrate<-rbind(seg1_meanrate,seg2_meanrate,seg3_meanrate,seg5_meanrate,seg7_meanrate,seg8_meanrate)
order <- c("PB2","PB1","PA","NP","MP","NS")
## density plot
ggplot(transform(internal_meanrate,segment=factor(segment,levels=order)),aes(x=mean_rate,fill=category))+
  geom_density(alpha=0.6)+
  theme_classic()+
  xlab("Mean transition rate/ecosystem")+
  ylab("Density")+
  facet_wrap(.~segment,scales="free",nrow=2)+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.title=element_blank())
  

######################################################################
## surface segment
H1<-c("flyway.rates.OB.DD","flyway.rates.OB.SB","flyway.rates.OB.PO")
H3<-c("flyway.rates.OB.DD","flyway.rates.SB.GU","flyway.rates.OB.SB")
H5<-c("flyway.rates.OB.DD","flyway.rates.OB.PO","flyway.rates.OB.SB","flyway.rates.DD.SB")
N1<-c("flyway.rates.OB.DD","flyway.rates.OB.SB","flyway.rates.DD.OB","flyway.rates.OB.PO")
N2<-c("flyway.rates.OB.DD","flyway.rates.OB.SB","flyway.rates.SB.OB","flyway.rates.PO.OB","flyway.rates.DD.SB")
N8<-c("flyway.rates.OB.SB","flyway.rates.OB.DD","flyway.rates.SB.GU","flyway.rates.SB.DD")

H1_meanrate<-mean_rate("seg4H1/seg4H1_flyway_rate_combined.log",H1)
H3_meanrate<-mean_rate("seg4H3/seg4H3_discrete_flyway_rates_combined.log",H3)
H5_meanrate<-mean_rate("seg4H5/seg4H5_discrete_flyway_rates_combined.log",H5)
N1_meanrate<-mean_rate("seg6N1/seg6N1_discrete_flyway_rates_combined.log",N1)
N2_meanrate<-mean_rate("seg6N2/seg6N2_discrete_flyway_rates_combined.log",N2)
N8_meanrate<-mean_rate("seg6N8/seg6N8_discrete_flyway_rates_combined.log",N8)

H1_meanrate$segment<-"H1"
H3_meanrate$segment<-"H3"
H5_meanrate$segment<-"H5"
N1_meanrate$segment<-"N1"
N2_meanrate$segment<-"N2"
N8_meanrate$segment<-"N8"

surface_meanrate<-rbind(H1_meanrate,H3_meanrate,H5_meanrate,N1_meanrate,N2_meanrate,N8_meanrate)

order <- c("H1","H3","H5","N1","N2","N8")

ggplot(transform(surface_meanrate,segment=factor(segment,levels=order)),aes(x=mean_rate,fill=category))+
  geom_density(alpha=0.6)+
  theme_classic()+
  xlab("Mean transition rate/ecosystem")+
  ylab("Density")+
  facet_wrap(.~segment,scales="free",nrow=2)+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.title=element_blank())
######################################################################################
## statistical distribution


BF<-function(df){
  p1<-length(which(df$`Outside to ATL-flyway`>df$`ATL-flyway to ATL-flyway`))
  p2<-length(which(df$`Outside to ATL-flyway`< df$`ATL-flyway to ATL-flyway`))
#  if(p1 >=p2){
#    post_odds<-p1/p2
#  } else {
#    post_odds<-p2/p1
#  }
  post_odds<-p1/p2
  BF<-(round(post_odds,digits=3))
  BF_label<- paste("BF = ", BF)
  return(BF_label)
}




seg1_meanrate_stat<-dcast(seg1_meanrate,state~category)
seg2_meanrate_stat<-dcast(seg2_meanrate,state~category)
seg3_meanrate_stat<-dcast(seg3_meanrate,state~category)
seg5_meanrate_stat<-dcast(seg5_meanrate,state~category)
seg7_meanrate_stat<-dcast(seg7_meanrate,state~category)
seg8_meanrate_stat<-dcast(seg8_meanrate,state~category)
seg1_BF<-(BF(seg1_meanrate_stat))
seg2_BF<-(BF(seg2_meanrate_stat))
seg3_BF<-(BF(seg3_meanrate_stat))
seg5_BF<-(BF(seg5_meanrate_stat))
seg7_BF<-(BF(seg7_meanrate_stat))
seg8_BF<-(BF(seg8_meanrate_stat))


## combine multiple df
seg1_meanrate_stat$segment<-"PB2"
seg2_meanrate_stat$segment<-"PB1"
seg3_meanrate_stat$segment<-"PA"
seg5_meanrate_stat$segment<-"NP"
seg7_meanrate_stat$segment<-"MP"
seg8_meanrate_stat$segment<-"NS"
internal_meanrate_stat<-rbind(seg1_meanrate_stat,seg2_meanrate_stat,seg3_meanrate_stat,seg5_meanrate_stat,seg7_meanrate_stat,seg8_meanrate_stat)
order <- c("PB2","PB1","PA","NP","MP","NS")
#install.packages("lemon")
library(lemon)
internal_rate_stat<-ggplot(transform(internal_meanrate_stat,segment=factor(segment,levels=order)),aes(x=internal_meanrate_stat$`ATL-flyway to ATL-flyway`,y=internal_meanrate_stat$`Outside to ATL-flyway`))+
  geom_point(color = "steelblue")+
  geom_abline(slope=1, intercept = 0, colour = "red")+
  theme_classic()+
  coord_equal()+
  annotate(geom ='text', x = Inf, y = Inf, label="x=y",hjust=3,vjust=6, color = 'red', size = 5,)+
  facet_rep_wrap(.~segment,nrow=2)+
  theme(axis.line=element_line())+
  xlab("ATL-flyway to ATL-flyway")+
  ylab("Outside to ATL-flyway")
## annotation layer
internal_text<-data.frame(label=c(seg1_BF,seg2_BF,seg3_BF,seg5_BF,seg7_BF,seg8_BF),
                          segment=c("PB2","PB1","PA","NP","MP","NS"))
internal_text$segment<-factor(internal_text$segment,levels = order)
internal_rate_stat+geom_text(
  data=internal_text,
  mapping=aes(x = -Inf, y = Inf, label = label),
  hjust = -0.2, 
  vjust = 2
)

########################################################################3
## surface segment
H3_meanrate_stat<-dcast(H3_meanrate,state~category)
H5_meanrate_stat<-dcast(H5_meanrate,state~category)

N2_meanrate_stat<-dcast(N2_meanrate,state~category)
N8_meanrate_stat<-dcast(N8_meanrate,state~category)
H3_BF<-(BF(H3_meanrate_stat))
H5_BF<-(BF(H5_meanrate_stat))
N2_BF<-(BF(N2_meanrate_stat))
N8_BF<-(BF(N8_meanrate_stat))

## combine multiple df
H3_meanrate_stat$segment<-"H3"
H5_meanrate_stat$segment<-"H5"
N2_meanrate_stat$segment<-"N2"
N8_meanrate_stat$segment<-"N8"

surface_meanrate_stat<-rbind(H3_meanrate_stat,H5_meanrate_stat,N2_meanrate_stat,N8_meanrate_stat)
order <- c("H3","H5","N2","N8")

surface_rate_stat<-ggplot(transform(surface_meanrate_stat,segment=factor(segment,levels=order)),aes(x=surface_meanrate_stat$`ATL-flyway to ATL-flyway`,y=surface_meanrate_stat$`Outside to ATL-flyway`))+
  geom_point(color = "steelblue")+
  geom_abline(slope=1, intercept = 0, colour = "red")+
  theme_classic()+
  coord_equal()+
  annotate(geom ='text', x = Inf, y = Inf, label="x=y",hjust=3,vjust=6, color = 'red', size = 5,)+
  facet_rep_wrap(.~segment,nrow=2)+
  theme(axis.line=element_line())+
  xlab("ATL-flyway to ATL-flyway")+
  ylab("Outside to ATL-flyway")

## annotation layer
surface_text<-data.frame(label=c(H3_BF,H5_BF,N2_BF,N8_BF),
                          segment=c("H3","H5","N2","N8"))
surface_text$segment<-factor(surface_text$segment,levels = order)
surface_rate_stat+geom_text(
  data=surface_text,
  mapping=aes(x = -Inf, y = Inf, label = label),
  hjust = -0.2, 
  vjust = 2
)
