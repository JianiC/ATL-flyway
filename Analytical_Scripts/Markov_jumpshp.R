## generate heatmap for Markov jumps

library(ggplot2)
library(dplyr)
library(tidytree)
library(reshape2)
library(tidyr)
library(lubridate)
library(ggpubr)
df<-read.table("seg6N1/jumpTimes.txt", header = TRUE, sep = "\t" )
state<-n_distinct(df$state)
df$from_to = paste(df$from,"_",df$to)
df <- df %>% mutate(time = 2018.150684931507 - as.numeric(time))
df$year <- format(date_decimal(df$time), "%Y")
  
count<-df %>%
  group_by(from_to,year)%>%
  count()%>%
  filter(as.numeric(year)>=2013)

count<-count%>% mutate(ave=n/state)
count2<-cbind(count, read.table(text = as.character(count$from_to), sep = "_"))

# Heatmap 
ggplot(count2, aes(x = V1, y=V2, fill= ave,group=year)) + 
  geom_tile()+
  scale_fill_gradient2(low="blue", high="red")+
  theme_classic()+
  xlab("From") +
  ylab("To")+
  labs(fill="Number of Jumps")+
  facet_grid(.~year,scales="free")

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

ggplot(transform(surface_jumps,segment=factor(segment,levels=order)), aes(x = V1, y=V2, fill= ave,group=year)) + 
  geom_tile()+
  scale_fill_gradient2(low="blue", high="red")+
  theme_classic()+
  xlab("From") +
  ylab("To")+
  labs(fill="Number of Jumps")+
  facet_grid(segment~year,scales="free")


############################################################
## internal segment
seg1<-jump_to_hp("seg1/jumpTimes.txt",2018.147945)
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
ggplot(transform(internal_jumps,segment=factor(segment,levels=order)), aes(x = V1, y=V2, fill= ave,group=year)) + 
  geom_tile()+
  scale_fill_gradient2(low="blue", high="red")+
  theme_classic()+
  xlab("From") +
  ylab("To")+
  labs(fill="Number of Jumps")+
  facet_grid(segment~year,scales="free")