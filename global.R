# libraries
library(shiny)
library(leaflet)
library(sp)
library(rgdal)
library(RColorBrewer)
library(dplyr)     # provides data manipulating functions.
library(magrittr)  # ceci n'est pas un pipe
library(tidyr)
library(rgeos)
library(colorspace)
library(intervals)
library(ggplot2)
library(grid)
library(reshape2)

#Functions of the Application
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

desat <- function(cols, sat=0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}

Intervals_Proportions <- function(Interval_Param, Age_Reference_Intervals, Age_Reference_Length){
  Int_Parameter <- Intervals(rbind(B1=Interval_Param))
  overlap <- t(as.data.frame(interval_overlap(Age_Reference_Intervals, Int_Parameter)))
  overlap[overlap=='integer(0)']<-0
  overlap <- as.numeric(overlap)
  Check <- as.data.frame(interval_intersection(Age_Reference_Intervals,Int_Parameter))
  Interval_Length <- overlap*Age_Reference_Length
  Interval_Length <- Interval_Length[Interval_Length>0]
  Check$Proportion <- (Check$V2 - Check$V1)/Interval_Length
  Check <- t(Check$Proportion)
  overlap[which(overlap==1)]<-overlap[which(overlap==1)]*Check  
  return(overlap)
}

#Load App Data
load("AppData_Geo.Rdata", envir=.GlobalEnv)
