#Subsetting by species then medium, then temp
for (s in species){ 
  species_data <- filter(LGrowthData, Species==s)
  medium <- unique(species_data$Medium)
  for (m in medium){
    medium_data <- filter(species_data, Medium==m)
    temperature <- unique(medium_data$Temp)
    pdf(paste("../Results/",s,m,".pdf")) #open pdf
    par(mfrow=c(2,2))
    for (t in temperature){
      temp_data <- filter(medium_data, Temp==t)
      plot(temp_data$Time, temp_data$PopBio, log="y", main=paste(s,m,t, sep = "\n"))
    }
    dev.off() #close pdf file
  }
}

#using ggplot
pdf("../Results/ggplot.pdf")
for (s in species){
  s_data <- filter(LGrowthData, Species==s)
  p <- 
    ggplot(s_data, aes(x=Time, y=PopBio, color= Medium))+
    geom_point(shape = 20) + 
    facet_grid(rows = vars(Temp)) + 
    scale_y_log10() + 
    xlab("Time") + 
    ylab("Abundance") +
    theme_bw() +
    ggtitle(s)
  print(p)
}
dev.off()

x <- unique(LGrowthData$Citation)
length(x)
head(LGrowthData)
paste()