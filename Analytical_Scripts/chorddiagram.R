#install.packages("circlize")
library(circlize)
library(tidytree)
## read PB2 dataframe as an example
df<-read.csv("chord_df.csv")
circos.par(gap.after = c("S-OB" = 5,"S-GU"=0,"S-DD"=0,"S-SB"=15,"S-PO" = 15, "OB"=5,"OB"=0,"GU"=0,"DD"=0,"SB"=0,"PO"=15))

grid.col = c(`S-OB`="#66c2a5", OB = "#66c2a5", `S-GU` = "#fc8d62", GU ="#fc8d62", `S-DD` = "#8da0cb", DD = "#8da0cb", `S-SB` = "#e78ac3", SB = "#e78ac3", `S-PO` = "#a6d854",PO="#a6d854")

chordDiagram(df,order=c("S-OB","S-GU","S-DD","S-SB","S-PO","OB","GU","DD","SB","PO"),grid.col = grid.col,col = df$color,directional = 1,
             direction.type = c("diffHeight", "arrows"),link.arr.type = "big.arrow")
test<-c("GU","DD","SB","PO")
highlight.sector(test, track.index = 1, col = "red", 
                 text = "A", cex = 0.8, text.col = "white", niceFacing = TRUE)             
circos.clear()


##################################################
circos.par(start.degree = 180)
levels = c("s_OB1","s_OB2","s_OB3","s_OB5","s_OB7","s_OB8","s_OBH1","s_OBH3","s_OBH5","s_OBN1","s_OBN2","s_OBN8",
           "s_DD1","s_DD2","s_DD3","s_DD5","s_DD7","s_DD8","s_DDH5","s_DDN1","s_DDN2","s_GU1","s_GU5","s_GU7",
           "s_PO7","s_PON2","s_SB1","s_SB2","s_SB3","s_SB5","s_SB7","s_SB8","s_SBH3","s_SBN2","s_SBN8",
           "OB1","OB2","OB3","OB5","OB7","OB8","OBN1","OBN2",
           "DD1","DD2","DD3","DD5","DD7","DD8","DDH1","DDH3","DDH5","DDN1","DDN2","DDN8",
           "GU1","GU2","GU3","GU5","GU7","GU8","GUH3","GUN8",
           "PO1","PO2","PO3","PO5","PO7","PO8","POH1","POH5","PON1",
           "SB1","SB2","SB3","SB5","SB7","SB8","SBH1","SBH3","SBH5","SBN1","SBN2","SBN8")

grid.col = c(s_OB1="#66c2a5",s_OB2="#66c2a5",s_OB3="#66c2a5",s_OB5="#66c2a5",s_OB7="#66c2a5",s_OB8="#66c2a5",s_OBH1="#66c2a5",s_OBH3="#66c2a5",s_OBH5="#66c2a5",s_OBN1="#66c2a5",s_OBN2="#66c2a5",s_OBN8="#66c2a5",
             OB1="#66c2a5",OB2="#66c2a5",OB3="#66c2a5",OB5="#66c2a5",OB7="#66c2a5",OB8="#66c2a5",OBN1="#66c2a5",OBN2="#66c2a5",
             s_DD1="#8da0cb",s_DD2="#8da0cb",s_DD3="#8da0cb",s_DD5="#8da0cb",s_DD7="#8da0cb",s_DD8="#8da0cb",s_DDH5="#8da0cb",s_DDN1="#8da0cb",s_DDN2="#8da0cb",
             DD1="#8da0cb",DD2="#8da0cb",DD3="#8da0cb",DD5="#8da0cb",DD7="#8da0cb",DD8="#8da0cb",DDH1="#8da0cb",DDH3="#8da0cb",DDH5="#8da0cb",DDN1="#8da0cb",DDN2="#8da0cb",DDN8="#8da0cb",
             s_GU1="#fc8d62",s_GU5="#fc8d62",s_GU7="#fc8d62", 
             GU1="#fc8d62",GU2="#fc8d62",GU3="#fc8d62",GU5="#fc8d62",GU7="#fc8d62",GU8="#fc8d62",GUH3="#fc8d62",GUN8="#fc8d62",
             s_PO7="#a6d854",s_PON2="#a6d854",
             PO1="#a6d854",PO2="#a6d854",PO3="#a6d854",PO5="#a6d854",PO7="#a6d854",PO8="#a6d854",POH1="#a6d854",POH5="#a6d854",PON1="#a6d854",
             s_SB1="#e78ac3",s_SB2="#e78ac3",s_SB3="#e78ac3",s_SB5="#e78ac3",s_SB7="#e78ac3",s_SB8="#e78ac3",s_SBH3="#e78ac3",s_SBN2="#e78ac3",s_SBN8="#e78ac3",
             SB1="#e78ac3",SB2="#e78ac3",SB3="#e78ac3",SB5="#e78ac3",SB7="#e78ac3",SB8="#e78ac3",SBH1="#e78ac3",SBH3="#e78ac3",SBH5="#e78ac3",SBN1="#e78ac3",SBN2="#e78ac3",SBN8="#e78ac3")


circos.par(gap.after = c("s_OB1"=0,"s_OB2"=0,"s_OB3"=0,"s_OB5"=0,"s_OB7"=0,"s_OB8"=0,"s_OBH1"=0,"s_OBH3"=0,"s_OBH5"=0,"s_OBN1"=0,"s_OBN2"=0,"s_OBN8"=5,
                         "s_DD1"=0,"s_DD2"=0,"s_DD3"=0,"s_DD5"=0,"s_DD7"=0,"s_DD8"=0,"s_DDH5"=0,"s_DDN1"=0,"s_DDN2"=3,"s_GU1"=0,"s_GU5"=0,"s_GU7"=3,
                         "s_PO7"=0,"s_PON2"=3,"s_SB1"=0,"s_SB2"=0,"s_SB3"=0,"s_SB5"=0,"s_SB7"=0,"s_SB8"=0,"s_SBH3"=0,"s_SBN2"=0,"s_SBN8"=16,
                         "OB1"=0,"OB2"=0,"OB3"=0,"OB5"=0,"OB7"=0,"OB8"=0,"OBN1"=0,"OBN2"=5,
                         "DD1"=0,"DD2"=0,"DD3"=0,"DD5"=0,"DD7"=0,"DD8"=0,"DDH1"=0,"DDH3"=0,"DDH5"=0,"DDN1"=0,"DDN2"=0,"DDN8"=3,
                         "GU1"=0,"GU2"=0,"GU3"=0,"GU5"=0,"GU7"=0,"GU8"=0,"GUH3"=0,"GUN8"=3,
                         "PO1"=0,"PO2"=0,"PO3"=0,"PO5"=0,"PO7"=0,"PO8"=0,"POH1"=0,"POH5"=0,"PON1"=3,
                         "SB1"=0,"SB2"=0,"SB3"=0,"SB5"=0,"SB7"=0,"SB8"=0,"SBH1"=0,"SBH3"=0,"SBH5"=0,"SBN1"=0,"SBN2"=0,"SBN8"=16
                          ))

s_OB<-c("s_OB1","s_OB2","s_OB3","s_OB5","s_OB7","s_OB8","s_OBH1","s_OBH3","s_OBH5","s_OBN1","s_OBN2","s_OBN8")
s_GU<-c("s_GU1","s_GU5","s_GU7")
s_DD<-c("s_DD1","s_DD2","s_DD3","s_DD5","s_DD7","s_DD8","s_DDH5","s_DDN1","s_DDN2")
s_SB<-c("s_SB1","s_SB2","s_SB3","s_SB5","s_SB7","s_SB8","s_SBH3","s_SBN2","s_SBN8")
s_PO<-c("s_PO7","s_PON2")
OB<-c("OB1","OB2","OB3","OB5","OB7","OB8","OBN1","OBN2")
GU<-c("GU1","GU2","GU3","GU5","GU7","GU8","GUH3","GUN8")
DD<-c("DD1","DD2","DD3","DD5","DD7","DD8","DDH1","DDH3","DDH5","DDN1","DDN2","DDN8")
SB<-c("SB1","SB2","SB3","SB5","SB7","SB8","SBH1","SBH3","SBH5","SBN1","SBN2","SBN8")
PO<-c("PO1","PO2","PO3","PO5","PO7","PO8","POH1","POH5","PON1")

## try with different segment
df2<-read.csv("test2.csv")

chordDiagram(df2,order=levels,grid.col = grid.col,col = df2$color,directional = 1,
             direction.type = c("diffHeight", "arrows"),link.arr.type = "big.arrow",
             annotationTrack = c("grid", "axis"),
             preAllocateTracks = list(
               track.height = mm_h(2),
               track.margin = c(mm_h(2), 0)
             ))
circos.track(track.index = 2, panel.fun = function(x, y) {
  sector.index = get.cell.meta.data("sector.index")
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  circos.text(mean(xlim), mean(ylim), sector.index, cex = 0.6, niceFacing = TRUE)
}, bg.border = 0.5)




highlight.sector(s_OB, track.index = 1, col = "#66c2a5", 
                 text = "OB", cex = 0.8, text.col = "black", niceFacing = TRUE)
highlight.sector(OB, track.index = 1, col = "#66c2a5", 
                 text = "OB", cex = 0.8, text.col = "black", niceFacing = TRUE)  
highlight.sector(s_GU, track.index = 1, col = "#fc8d62", 
                 text = "GU", cex = 0.8, text.col = "black", niceFacing = TRUE)  
highlight.sector(GU, track.index = 1, col = "#fc8d62", 
                 text = "GU", cex = 0.8, text.col = "black", niceFacing = TRUE)  
highlight.sector(s_DD, track.index = 1, col = "#8da0cb", 
                 text = "DD", cex = 0.8, text.col = "black", niceFacing = TRUE)  
highlight.sector(DD, track.index = 1, col = "#8da0cb", 
                 text = "DD", cex = 0.8, text.col = "black", niceFacing = TRUE)  
highlight.sector(s_SB, track.index = 1, col = "#e78ac3", 
                 text = "SB", cex = 0.8, text.col = "black", niceFacing = TRUE)  
highlight.sector(SB, track.index = 1, col = "#e78ac3", 
                 text = "SB", cex = 0.8, text.col = "black", niceFacing = TRUE)  
highlight.sector(s_PO, track.index = 1, col = "#a6d854", 
                 text = "PO", cex = 0.8, text.col = "black", niceFacing = TRUE)
highlight.sector(PO, track.index = 1, col = "#a6d854", 
                 text = "PO", cex = 0.8, text.col = "black", niceFacing = TRUE)
abline(h = 0, lty = 2, col = "#00000080")

circos.clear()


