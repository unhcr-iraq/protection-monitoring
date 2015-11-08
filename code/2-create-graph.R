## preparing a few graphs

library(ggplot2)

format_si <- function(...) {
  # Format a vector of numeric values according
  # to the International System of Units.
  # http://en.wikipedia.org/wiki/SI_prefix
  #
  # Based on code by Ben Tupper
  # https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
  # Args:
  #   ...: Args passed to format()
  #
  # Returns:
  #   A function to format a vector of strings using
  #   SI prefix notation
  #
  
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "µ",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")
    
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)
    
    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}


## box plot to display 
ggplot(data, aes(x=Religion, y=total, fill=Ethnicity) ) + 
  geom_boxplot() 

## Relevel the Ethnicity by total individual
data$Ethnicity <-factor(data$Ethnicity, levels=data[order(data$total), "Ethnicity"])
data$Ethnicity <- reorder(data$Ethnicity, data$total)
levels(data$Ethnicity)

#### Bar graph to show repartition for categories
bar.Ethnicity <- ggplot(data=data, aes(x=reorder(Ethnicity,Ethnicity,
                                                 function(x)-length(x)) , y=total)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  # coord_flip()+
  xlab("Ethnicity") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  coord_flip()+ 
  ggtitle("Ethnicity")
ggsave("out/barEthnicity.png", bar.Ethnicity, width=8, height=6,units="in", dpi=300)

bar.Religion <- ggplot(data=data, aes(x=reorder(Religion,Religion,
                                                 function(x)-length(x)) , y=total)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  # coord_flip()+
  xlab("Religion") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  coord_flip()+ 
  ggtitle("Religion")
ggsave("out/barReligion.png", bar.Religion, width=8, height=6,units="in", dpi=300)


### Let's try to generate chart for all variables

for (i in 1:20 ) {
  rm(variablename)
  variablename <- names(data)[i]
  rm(plottitle)
  plottitle <- substr(attributes(data)$variable.labels[i], 1, gregexpr(pattern =']', attributes(data)$variable.labels[i]))
  rm(plot)
  plot <- ggplot(data=data, aes(x=reorder(variablename,variablename,
                                          function(x)-length(x)) , y=total)) + 
    geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
    # geom_text(aes(label=variable), vjust=0) +
    guides(fill=FALSE) + 
    # coord_flip()+
    xlab(variablename) + 
    ylab("# of Ind") +
    scale_y_continuous(labels=format_si())+
    coord_flip()+ 
    ggtitle(plottitle)
  assign(paste("plot",variablename,sep=""), plot)
  ggsave(filename=paste("out/plot",variablename,".png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
}



