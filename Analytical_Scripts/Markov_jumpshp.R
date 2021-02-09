## generate heatmap for Markov jumps

library(ggplot2)
library(dplyr)
library(tidytree)
library(reshape2)
library(tidyr)
library(lubridate)
library(ggpubr)
df<-read.table("seg1/jumpTimes.txt", header = TRUE, sep = "\t" )
state<-n_distinct(df$state)
df$from_to = paste(df$from,"_",df$to)
df <- df %>% mutate(time = 2018.147945 - as.numeric(time))
df$year <- format(date_decimal(df$time), "%Y")
  
count<-df %>%
  group_by(from_to,year)%>%
  count()%>%
  filter(as.numeric(year)>=2015)

count<-count%>% mutate(ave=n/state)
count2<-cbind(count, read.table(text = as.character(count$from_to), sep = "_"))

write.csv(count2,"PB2_MJ_edited.csv")
count2<-read.csv("PB2_MJ_edited2.csv")
# Heatmap 
order_f<-c("OB ","DD ","GU ","PO ","SB ")
internal_jumps<-internal_jumps%>%filter(as.numeric(year)>=2015)
internal_jumps$V1_f = factor(internal_jumps$V1, levels=c("OB ","DD ","GU ","PO ","SB "))
internal_jumps$year=factor(internal_jumps$year,levels=c("2015","2016","2017"))
internal_jumps$segment=factor(internal_jumps$segment,levels=c("PB2","PB1","PA","NP","MP","NS"))
ggplot(internal_jumps, aes(x =as.factor(year), y=from_to,fill= as.numeric(ave))) + 
  geom_tile()+
  scale_fill_gradient2(low = '#ffffd9', mid = '#7fcdbb', high = '#1f78b4',midpoint = 12.5,limits=c(0,25))+
  theme_bw()+
  xlab("year") +
  ylab("To")+
  labs(fill="Number of Jumps")+
  #facet_grid(rows = vars(V1_f),,scales="free")+
  facet_grid(rows = vars(V1_f),cols=vars(segment),scales="free")+
  scale_x_discrete(position = "top")

str(count2)
###################################################################################
## wrap into function
jump_to_hp<-function(file,mrsd){
  df<-read.table(file, header = TRUE, sep = "\t" )
  state<-n_distinct(df$state)
  df$from_to = paste(df$from,"_",df$to)
  df <- df %>% mutate(time = mrsd - as.numeric(time))
  df$year <- format(date_decimal(df$time), "%Y")
  
  count<-df %>%
    group_by(from_to,year)%>%
    count()%>%
    filter(as.numeric(year)>= 2013 && as.numeric(year)<2018)
  count<-count%>% mutate(ave=n/state)
  count2<-cbind(count, read.table(text = as.character(count$from_to), sep = "_"))
#  p<-ggplot(count2, aes(x = V1, y=V2, fill= ave,group=year)) + 
#    geom_tile()+
#    scale_fill_gradient2(low="blue", high="red")+
#    theme_classic()+
##    xlab("From") +
#    ylab("To")+
#    labs(fill="Number of Jumps")+
#    facet_grid(.~year,scales="free")
  return(count2)
  
}

seg4H1<-jump_to_hp("seg4H1/jumpTimes.txt",2018.150684931507)
seg4H3<-jump_to_hp("seg4H3/jumpTimes.txt",2018.147945)
seg4H5<-jump_to_hp("seg4H5/jumpTimes.txt",2017.90684)
seg6N1<-jump_to_hp("seg6N1/jumpTimes.txt",2018.150684931507)
seg6N2<-jump_to_hp("seg6N2/jumpTimes.txt",2017.9452054)
seg6N8<-jump_to_hp("seg6N8/jumpTimes.txt",2018.147945)


## combine multiple df
seg4H1$segment<-"H1"
seg4H3$segment<-"H3"
seg4H5$segment<-"H5"
seg6N1$segment<-"N1"
seg6N2$segment<-"N2"
seg6N8$segment<-"N8"

surface_jumps<-rbind(seg4H1,seg4H3,seg4H5,seg6N1,seg6N2,seg6N8)

order <- c("H1","H3","H5","N1","N2","N8")
order_f<-c("OB ","DD ","GU ","PO ","SB ")
surface_jumps<-surface_jumps %>% mutate(V1 = factor(V1, levels=c("SB ","PO ","GU ","DD ","OB "))) 
surface_jumps$V1<-factor(surface_jumps$V1,levels=order_f)
order_t<-c(" OB"," DD"," GU"," PO"," SB")

ggplot(transform(surface_jumps,segment=factor(segment,levels=order)), aes(x=factor(V2,levels = order_t), y=V1, fill= ave)) + 
  geom_tile()+
  scale_fill_gradient2(low="blue", high="red")+
  theme_bw()+
  xlab("From") +
  ylab("To")+
  labs(fill="Number of Jumps")+
  facet_grid(segment~year,scales="free")+
  scale_x_discrete(position = "top") 


############################################################
## internal segment
#seg1<-jump_to_hp("seg1/jumpTimes.txt",2018.147945)
seg1<-read.csv("PB2_MJ_edited2.csv")
seg2<-jump_to_hp("seg2/jumpTimes.txt",2017.969863)
seg3<-jump_to_hp("seg3/jumpTimes.txt",2017.969863)
seg5<-jump_to_hp("seg5/jumpTimes.txt",2017.969863)
seg7<-jump_to_hp("seg7/jumpTimes.txt",2017.969863)
seg8<-jump_to_hp("seg8/jumpTimes.txt",2017.969863)
## combine multiple df
seg1$segment<-"PB2"
seg2$segment<-"PB1"
seg3$segment<-"PA"
seg5$segment<-"NP"
seg7$segment<-"MP"
seg8$segment<-"NS"
internal_jumps<-rbind(seg1,seg2,seg3,seg5,seg7,seg8)
order <- c("PB2","PB1","PA","NP","MP","NS")
order_f<-c("SB ","PO ","GU ","DD ","OB ")
order_t<-c(" OB"," DD"," GU"," PO"," SB")
ggplot(transform(internal_jumps,segment=factor(segment,levels=order)), aes(x=factor(V2,levels = order_t), y=factor(V1,levels=order_f), fill= ave,group=year)) + 
  geom_tile()+
  scale_fill_gradient2(low="blue", high="red")+
  theme_bw()+
  xlab("To") +
  ylab("From")+
  labs(fill="Number of Jumps")+
  facet_grid(segment~year,scales="free")+
  scale_x_discrete(position = "top") 