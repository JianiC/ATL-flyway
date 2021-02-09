m <- matrix(c(0,0,0,0,0,0,0,0.890439332,1.862532527,0,0,0,0,0,0,0,0,0,0.999710962,0,0,0,0.554476869,0,0,0,0,0,0,0,1.818940125,0.431746269,0,0,1.024085405,0,0,0,0,0,0,0,0,0,0,0.5,0,0,0,0,0,0,0,0,0,0,1.79823414,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0.999710962,1.818940125,0,0,0,0,0,0,0,0,0,0.890439332,0,0.431746269,0,0,0,0,0,0,0,0,0,1.862532527,0,0,0,1.79823414,0,0,0,0,0,0,0,0,0,0,0.5,0,0,0,0,0,0,0,0,0,0.554476869,1.024085405,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,),
            byrow = TRUE,
            nrow = 12, ncol = 12)


categories <- c("src Sex-M","src Sex-F","src MSM","Null","src IDU","Null","sink Sex-M","sink Sex-F","sink MSM","Null","sink IDU","Null" )
dimnames(m) <- list(source = categories , sink = categories)
print(m)

library(chorddiag)
pander::pandoc.table(m, style = "rmarkdown")
groupColors <- c()
chorddiag(m, groupColors = c("#2980B9","#3498DB","#1ABC9C", "#ffffff","#16A085", "#ffffff","#2980B9","#3498DB","#1ABC9C", "#ffffff","#16A085", "#ffffff"), showTicks = FALSE, chordedgeColor = NULL)


