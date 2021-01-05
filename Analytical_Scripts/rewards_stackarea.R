library(ggtree)
require(treeio)
library(ggplot2)
library(dplyr)
library(tidytree)
library(reshape2)
library(tidyr)
library(ggnewscale)
library("RColorBrewer")

beast_tree <- read.beast("seg1/seg1_flyway_history_anot.tree")
data<-as_tibble(beast_tree)
df<- data %>% select(ends_with('reward'),height)%>% 
  mutate_all(funs(replace_na(.,0)))%>%
  mutate(year = 2018.150684931507 - as.numeric(height))

df = as.data.frame(sapply(df, as.numeric))
## combine the value of same year
df2<-aggregate(. ~ year, data=df, FUN=sum)
## get propotion
df2<-df2%>%
  mutate(sum = rowSums(.[2:5]))

df2[,2:5]<-df2[,2:5] / df2$sum

plot <-df2%>% 
  filter(df2$sum!=0)%>%
  mutate_all(funs(replace_na(.,0)))%>%
  select(ends_with('reward'),year) %>% 
  gather(location,value,DD_reward:SB_reward)

plot$location<-gsub("_reward","",plot$location)
  
# stacked area chart
p<-ggplot(plot, aes(x=year, y=value, fill=location)) + geom_area()+
  theme_classic()+
  xlab("Year")+
  ylab("Trunk Reward Propotion")+
  labs(fill = "Location")+
  scale_fill_manual(values = c("DD" = "#66c2a5", "GU" = "#fc8d62", "OB" = "#8da0cb", "PO" = "#e78ac3","SB"="#a6d854"))
p

## wrap function 
rewards_stackarea<-function(file,mrsd){
  beast_tree <- read.beast(file)
  data<-as_tibble(beast_tree)
  df<- data %>% select(ends_with('reward'),height)%>% 
    mutate_all(funs(replace_na(.,0)))%>%
    mutate(year = mrsd - as.numeric(height))
  
  df = as.data.frame(sapply(df, as.numeric))

  df2<-aggregate(. ~ year, data=df, FUN=sum)
  df2<-df2%>%
    mutate(sum = rowSums(.[2:6]))
 
  df2[,2:6]<-df2[,2:6] / df2$sum
  
  plot <-df2%>% 
    filter(df2$sum!=0)%>%
    mutate_all(funs(replace_na(.,0)))%>%
    select(ends_with('reward'),year) %>% 
    gather(location,value,DD_reward:SB_reward)
  
  plot$location<-gsub("_reward","",plot$location)
#  p<-ggplot(plot, aes(x=year, y=value, fill=location)) + geom_area()+
#    theme_classic()+
#    xlab("Year")+
#    ylab("Trunk Reward Propotion")+
#    labs(fill = "Location")+
#    scale_fill_manual(values = c("DD" = "#66c2a5", "GU" = "#fc8d62", "OB" = "#8da0cb", "PO" = "#e78ac3","SB"="#a6d854"))
  
  return(plot)
}

seg4H1<-rewards_stackarea("seg4H1/seg4H1_flywayhistory_anot.tree",2018.150684931507)
seg4H3<-rewards_stackarea("seg4H3/seg4H3_discrete_flyway_history_anot.tree",2018.147945)
seg4H5<-rewards_stackarea("seg4H5/seg4H5_discret_flyway_history_anot.tree", 2017.90684)
seg6N1<-rewards_stackarea("seg6N1/seg6N1_discrete_flyway_history_anot.tree",2018.150684931507)
seg6N2<-rewards_stackarea("seg6N2/seg6N2_discrete_flyway_history_anot.tree",2017.9452054)
seg6N8<-rewards_stackarea("seg6N8/seg6N8_discrete_flyway_history_anot.tree",2018.147945)


## combine multiple df
seg4H1$segment<-"H1"
seg4H3$segment<-"H3"
seg4H5$segment<-"H5"
seg6N1$segment<-"N1"
seg6N2$segment<-"N2"
seg6N8$segment<-"N8"

surface_rewards<-rbind(seg4H1,seg4H3,seg4H5,seg6N1,seg6N2,seg6N8)

order <- c("H1","H3","H5","N1","N2","N8")
p<-ggplot(transform(surface_rewards,segment=factor(segment,levels=)), aes(x=year, y=value, fill=location)) + 
  geom_area()+
  theme_classic()+
  xlab("Year")+
  ylab("Trunk Reward Propotion")+
  labs(fill = "Location")+
  scale_fill_manual(values = c("DD" = "#66c2a5", "GU" = "#fc8d62", "OB" = "#8da0cb", "PO" = "#e78ac3","SB"="#a6d854"))+
  facet_wrap(.~ segment,scales="free", nrow=2)

print(p)

############################################################
## internal segment
seg1<-rewards_stackarea("seg1/seg1_flyway_history_anot.tree",2018.147945)
seg2<-rewards_stackarea("seg2/seg2_discrete_flyway_history_anot.tree",2017.969863)
seg3<-rewards_stackarea("seg3/seg3_discrete_flyway_history_anot.tree",2017.969863)
seg5<-rewards_stackarea("seg5/seg5_discrete_flyway_history_anot.tree",2017.969863)
seg7<-rewards_stackarea("seg7/seg7_flywayhistory_anot.tree",2017.969863)
seg8<-rewards_stackarea("seg8/seg8_discrete_flyway_history_anot.tree",2017.969863)

## combine multiple df
seg1$segment<-"PB2"
seg2$segment<-"PB1"
seg3$segment<-"PA"
seg5$segment<-"NP"
seg7$segment<-"MP"
seg8$segment<-"NS"
surface_rewards<-rbind(seg1,seg2,seg3,seg5,seg7,seg8)

order <- c("PB2","PB1","PA","NP","MP","NS")
p<-ggplot(transform(surface_rewards,segment=factor(segment,levels=)), aes(x=year, y=value, fill=location)) + 
  geom_area()+
  theme_classic()+
  xlab("Year")+
  ylab("Trunk Reward Propotion")+
  labs(fill = "Location")+
  scale_fill_manual(values = c("DD" = "#66c2a5", "GU" = "#fc8d62", "OB" = "#8da0cb", "PO" = "#e78ac3","SB"="#a6d854"))+
  facet_wrap(.~ segment,scales="free", nrow=2)

print(p)
