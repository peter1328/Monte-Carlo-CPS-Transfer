##Set working directory where data are saved
setwd("C:/Users/petere/Desktop/Experimente-Antrag/Simulation CPS post/")

library(foreign)

for(r in 1:1000){

  sd <- read.table("MCTransfer_r.dat")
  sd[,7] <- (sd[,3]*sd[,4])
  write.table(sd, "sdint_r", sep="\t")
}
  
  
ss <- read.table("MCTransfer_1.dat")

ss[,7] <- 1 

  ss <- n_[r]
  for (t in 1:length(i)){
    is <- i[t]
    for (s in 1:m){
      sm <- sim.rasch(ss, is)
      rm <- RM(sm) # Estimate Rasch parameters
      pp <- person.parameter(rm) #Read person parameters
      fs <- itemfit(pp) #Compute fit statistics
      ifmat_rasch <- rbind(ifmat_rasch, fs$i.infitMSQ)
      ifmat_rasch$Infit_0713[nrow(ifmat_rasch)] <- (sum(ifmat_rasch[nrow(ifmat_rasch),1:is]<0.7)+sum(ifmat_rasch[nrow(ifmat_rasch),1:is]>1.3))
      ifmat_rasch$Infit_0812[nrow(ifmat_rasch)] <- (sum(ifmat_rasch[nrow(ifmat_rasch),1:is]<0.8)+sum(ifmat_rasch[nrow(ifmat_rasch),1:is]>1.2))
      ifmat_rasch$Infit_085115[nrow(ifmat_rasch)] <- (sum(ifmat_rasch[nrow(ifmat_rasch),1:is]<0.85)+sum(ifmat_rasch[nrow(ifmat_rasch),1:is]>1.15))
      ifmat_rasch$ss[nrow(ifmat_rasch)] <- ss
      ifmat_rasch$is[nrow(ifmat_rasch)] <- is
      ifmat_rasch$ni[nrow(ifmat_rasch)] <- ncol(ifmat_rasch[nrow(ifmat_rasch),])
    }
  }
}

##Activate required packages

library(foreign)
library(reshape)
library(plyr)
library(gdata)
library(BayesFactor)
library(ggthemes)
library(grid)
library(XLConnect)
library(gridExtra)


#Save settings for "Pety"-theme to be rather APA
theme_pety <- function(base_size = 16, base_family = "") {
  theme(text = element_text(size=30), #Larger text font size
        panel.background = element_rect(fill = 'white', colour = 'black'), #Figure background colour
        panel.grid.major=element_blank(), #Remove major grid
        panel.grid.minor=element_blank(), #Remove minor grid
        axis.text.x = element_text(colour="black"), #x-Axis text colour
        axis.text.y = element_text(colour="black")) #y-Axis text colour
}

petyPalette <- c("blue4","dodgerblue4","dodgerblue2", "cornflowerblue")
EstherPalette <- c("blue", "red", "green", "purple")
EstherLinetype <- c("solid", "dashed", "solid", "dashed")

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


plotdata <- read.spss("plotdata.sav", use.value.labels=TRUE,
                  max.value.labels=Inf, to.data.frame=TRUE) #Read data set from SPSS-file

str (plotdata) #Check Data set



#Bar graph for intervention effects on immediate learning measures
bar_gra <- ggplot(subset(plotdata,
                         !is.na(il_measure)),
              aes(x=il_intervention,
                  y=il_mean)) +
  facet_wrap(~il_measure) +
  geom_bar(position=position_dodge(),
           stat="identity",
           ) + 
  theme_pety() +
#  coord_fixed(ratio=0.15) +
  scale_x_discrete(name="Intervention") +
  scale_y_continuous(limits=c(0, 100),
                     breaks=seq(0, 100, 10),
                     name="Solution rate (in %)") +
  theme(legend.title=element_blank()) +
  geom_errorbar(aes(ymin=il_mean-(il_CI*(1/1.96)),
                    ymax=il_mean+(il_CI*(1/1.96)),
                    group=il_intervention),
                width=0.1,
                position=position_dodge(0.9),
                alpha=0.85,
                colour="black",
                fill="black")
bar_gra

#Bar graph for intervention effects on immediate learning measures: Try to cut ordinate
bar_gra_a <- ggplot(subset(plotdata,
                         !is.na(il_measure)),
                  aes(x=il_intervention,
                      y=il_mean)) +
  coord_cartesian(ylim=c(75, 100)) +
  facet_wrap(~il_measure) +
  geom_bar(position=position_dodge(),
           stat="identity",
  ) + 
  #theme_pety() +
  #  coord_fixed(ratio=0.15) +
  scale_x_discrete(name="Intervention") +
  scale_y_continuous(breaks=seq(75, 100, 5),
                     name="Solution rate (in %)") +
  theme(legend.title=element_blank()) +
  geom_errorbar(aes(ymin=il_mean-(il_CI*(1/1.96)),
                    ymax=il_mean+(il_CI*(1/1.96)),
                    group=il_intervention),
                width=0.1,
                position=position_dodge(0.9),
                alpha=0.85,
                colour="black",
                fill="black") +
  theme(text = element_text(size=40), #Larger text font size
        panel.background = element_rect(fill = 'white', colour = 'black'), #Figure background colour
        panel.grid.major=element_blank(), #Remove major grid
        panel.grid.minor=element_blank(), #Remove minor grid
        axis.text.x = element_text(colour="black"), #x-Axis text colour
        axis.text.y = element_text(colour="black")) #y-Axis text colour

bar_gra_a

tiff("bar_gra_a.tiff", width = 2200, height = 1100)
plot(bar_gra_a)
dev.off()


##Line Graphs##

#Data preparation line graphs
fup_1 <- plotdata[1:12,] #Data for transformation knowledge
fup_2 <- plotdata[13:24,] #Data for explicit content knowledge
fup_3 <- plotdata[25:36,] #Data for misconceptions

#Line graph for intervention effects on I) transformation knowledge
fupline_1 <- ggplot(fup_1,
                    aes(x = Time,
                        y = mean,
                        group = Intervention,
                        shape=Intervention,
                        fill = Intervention)) + 
  geom_line(size = 2,
            position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin=mean-(CI*(1/1.96)), ymax=mean+(CI*(1/1.96))),
                linetype = 1,
                position = position_dodge(width = 0.1),
                width=0.15,
                size = 0.75) +     
  geom_point(size = 7,
             position = position_dodge(width = 0.1)) +
  scale_shape_manual(values=c(22,22,24,24)) +
  scale_fill_manual(values=c("black", "white", "black", "white")) +
  scale_x_continuous(breaks=seq(1,3,1),
                     labels =c("T1", "T2", "T3")) +
  scale_y_continuous(limits = c(30, 58),
                     breaks=seq(30, 58, 5)) +
  #scale_linetype_manual(values = EstherLinetype) +
  theme_pety() +
  labs(x = "Transformation knowledge",
       y = "Score (max = 58)",
       shape = "Intervention") +
  theme(legend.position=c(0.2,0.2),
        legend.text=element_text(size=30)) +
  theme(axis.line = element_line(colour = "black"),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank(),
       panel.background = element_blank()) +
  guides(linetype=guide_legend(override.aes=list(size=4)),
            shape=guide_legend(override.aes=list(size=7)))
fupline_1


tiff("fupline_1.tiff", width = 3000, height = 2000)
plot(fupline_1)
dev.off()

#Line graph for intervention effects on I) transformation knowledge
fupline_1_a <- ggplot(fup_1,
                    aes(x = Time,
                        y = mean,
                        group = Intervention,
                        shape=Intervention,
                        fill = Intervention)) + 
  geom_line(size = 4,
            position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin=mean-(CI*(1/1.96)), ymax=mean+(CI*(1/1.96))),
                linetype = 1,
                position = position_dodge(width = 0.1),
                width=0.15,
                size = 0.75) +     
  geom_point(size = 20,
             position = position_dodge(width = 0.1)) +
  scale_shape_manual(values=c(22,22,24,24)) +
  scale_fill_manual(values=c("black", "white", "black", "white")) +
  scale_x_continuous(breaks=seq(1,3,1),
                     labels =c("T1", "T2", "T3")) +
  scale_y_continuous(limits = c(30, 58),
                     breaks=seq(30, 58, 5)) +
  #scale_linetype_manual(values = EstherLinetype) +
  #theme_pety() +
  labs(x = "Transformation knowledge",
       y = "Score (max = 58)",
       shape = "Intervention") +
  theme(legend.position=c(0.2,0.2),
        legend.text=element_text(size=70)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(linetype=guide_legend(override.aes=list(size=17)),
         shape=guide_legend(override.aes=list(size=20))) +
  theme(text = element_text(size=70), #Larger text font size
        #panel.background = element_rect(fill = 'white', colour = 'black'), #Figure background colour
        panel.grid.major=element_blank(), #Remove major grid
        panel.grid.minor=element_blank(), #Remove minor grid
        axis.text.x = element_text(colour="black"), #x-Axis text colour
        axis.text.y = element_text(colour="black")) #y-Axis text colour

fupline_1_a


tiff("fupline_1_a.tiff", width = 2400, height = 1800)
plot(fupline_1_a)
dev.off()


#Line graph for intervention effects on II) explicit concept knowledge
fupline_2 <- ggplot(fup_2,
                    aes(x = Time,
                        y = mean,
                        group = Intervention,
                        shape=Intervention,
                        fill = Intervention)) + 
  geom_line(size = 2, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin=mean-(CI*(1/1.96)), ymax=mean+(CI*(1/1.96))),
                linetype = 1,
                position = position_dodge(width = 0.1),
                width=0.15,
                size = 0.75) +     
  geom_point(size = 7,
             position = position_dodge(width = 0.1)) +
  scale_shape_manual(values=c(22,22,24,24)) +
  scale_fill_manual(values=c("black", "white", "black", "white")) +
  scale_x_continuous(breaks=seq(1,3,1),
                     labels =c("T1", "T2", "T3")) +
  scale_y_continuous(limits=c(2, 12),
                     breaks=seq(2, 12, 2)) +
  theme_pety() +
  labs(x = "Explicit concept knowledge",
       y = "Score (max = 19)",
       shape = "Intervention") +
  theme(legend.position=c(0.2,0.2),
        legend.text=element_text(size=30)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(linetype=guide_legend(override.aes=list(size=7)),
         shape=guide_legend(override.aes=list(size=7)))
fupline_2


#Line graph for intervention effects on II) explicit concept knowledge
fupline_2_a <- ggplot(fup_2,
                    aes(x = Time,
                        y = mean,
                        group = Intervention,
                        shape=Intervention,
                        fill = Intervention)) + 
  geom_line(size = 4, position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin=mean-(CI*(1/1.96)), ymax=mean+(CI*(1/1.96))),
                linetype = 1,
                position = position_dodge(width = 0.1),
                width=0.15,
                size = 0.75) +     
  geom_point(size = 20,
             position = position_dodge(width = 0.1)) +
  scale_shape_manual(values=c(22,22,24,24)) +
  scale_fill_manual(values=c("black", "white", "black", "white")) +
  scale_x_continuous(breaks=seq(1,3,1),
                     labels =c("T1", "T2", "T3")) +
  scale_y_continuous(limits=c(2, 12),
                     breaks=seq(2, 12, 2)) +
  theme_pety() +
  labs(x = "Explicit concept knowledge",
       y = "Score (max = 19)",
       shape = "Intervention") +
  theme(legend.position=c(0.2,0.2),
        legend.text=element_text(size=70)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(linetype=guide_legend(override.aes=list(size=17)),
         shape=guide_legend(override.aes=list(size=20))) +
  theme(text = element_text(size=70), #Larger text font size
        #panel.background = element_rect(fill = 'white', colour = 'black'), #Figure background colour
        panel.grid.major=element_blank(), #Remove major grid
        panel.grid.minor=element_blank(), #Remove minor grid
        axis.text.x = element_text(colour="black"), #x-Axis text colour
        axis.text.y = element_text(colour="black")) #y-Axis text colour
fupline_2_a

tiff("fupline_2_a.tiff", width = 2400, height = 1800)
plot(fupline_2_a)
dev.off()

#Line graph for intervention effects on III) misconceptions
fupline_3 <- ggplot(fup_3,
                    aes(x = Time,
                        y = mean,
                        group = Intervention,
                        shape=Intervention,
                        fill = Intervention)) + 
  geom_line(size = 2,
            position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin=mean-(CI*(1/1.96)), ymax=mean+(CI*(1/1.96))),
                linetype = 1,
                position = position_dodge(width = 0.1),
                width=0.15,
                size = 0.75) +     
  geom_point(size = 7,
             position = position_dodge(width = 0.1)) +
  scale_shape_manual(values=c(22,22,24,24)) +
  scale_fill_manual(values=c("black", "white", "black", "white")) +
  scale_x_continuous(breaks=seq(1,3,1),
                     labels =c("T1", "T2", "T3")) +
  scale_y_continuous(limits = c(0, 2),
                     breaks=seq(0, 2, 0.5)) +
  theme_pety() +
  labs(x = "Misconceptions",
       y = "Errors",
       shape = "Intervention") +
  theme(legend.position=c(0.2,0.4),
        legend.text=element_text(size=30)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(linetype=guide_legend(override.aes=list(size=7)),
         shape=guide_legend(override.aes=list(size=7)))
fupline_3





tiff("fupline_2_a.tiff", width = 2400, height = 1800)
plot(fupline_2_a)
dev.off()

#Line graph for intervention effects on III) misconceptions
fupline_3_a <- ggplot(fup_3,
                    aes(x = Time,
                        y = mean,
                        group = Intervention,
                        shape=Intervention,
                        fill = Intervention)) + 
  geom_line(size = 4,
            position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin=mean-(CI*(1/1.96)), ymax=mean+(CI*(1/1.96))),
                linetype = 1,
                position = position_dodge(width = 0.1),
                width=0.15,
                size = 0.75) +     
  geom_point(size = 20,
             position = position_dodge(width = 0.1)) +
  scale_shape_manual(values=c(22,22,24,24)) +
  scale_fill_manual(values=c("black", "white", "black", "white")) +
  scale_x_continuous(breaks=seq(1,3,1),
                     labels =c("T1", "T2", "T3")) +
  scale_y_continuous(limits = c(0, 2),
                     breaks=seq(0, 2, 0.5)) +
  #theme_pety() +
  labs(x = "Misconceptions",
       y = "Errors",
       shape = "Intervention") +
  theme(legend.position=c(0.2,0.4),
        legend.text=element_text(size=70)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(linetype=guide_legend(override.aes=list(size=17)),
         shape=guide_legend(override.aes=list(size=20))) +
  theme(text = element_text(size=70), #Larger text font size
        #panel.background = element_rect(fill = 'white', colour = 'black'), #Figure background colour
        panel.grid.major=element_blank(), #Remove major grid
        panel.grid.minor=element_blank(), #Remove minor grid
        axis.text.x = element_text(colour="black"), #x-Axis text colour
        axis.text.y = element_text(colour="black")) #y-Axis text colour
fupline_3_a

tiff("fupline_3_a.tiff", width = 2400, height = 1800)
plot(fupline_3_a)
dev.off()




grid.arrange(fupline_1, fupline_2, fupline_3, nrow=1)



##MISC

#colour="black"

#scale_fill_manual(values = EstherPalette)
#scale_fill_hue(l=50)
#scale_fill_brewer(palette="Set1") +
#scale_shape_manual(values = c(1, 5, 10, 15)) +
#facet_grid( ~ Measure) +
#coord_cartesian(ylim=c(30, 58)) + #different option to regulate axis range

#pd_2 <- subset(plotdata, Measure=1)

#pd_2 <-plotdata[plotdata$Measure==1,]

#at line aes aes(linetype = Intervention),
# at main aes , fill=Intervention
#black scale for shape fillings   #scale_fill_manual(values=c("black", "black", "black", "black")) +
#guides(colour = guide_legend("Region", 
#                             override.aes = list(size = 6)))
#scale_shape_discrete(guide=guide_legend(override.aes=aes(size=3)))
#scale_shape_discrete(guide=guide_legend(override.aes=aes(size=3, 
#                                                         linetype=0))
#                     + guides(colour = guide_legend(override.aes = list(size=10)))
#                     