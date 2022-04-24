
library(ggplot)
library(reshape2)



prs_score<-read.csv("../Source_Data/2022-03-12_Data_figure4.csv")
head(prs_score)
#rename the column
colnames(prs_score)<-c("ID","Pan UKBB-HTN","MVP-SBP","MVP-DBP","PRSsum","Background")
#reshape data frame
prs_score<- reshape2::melt(prs_score, ids=c("ID","Background"))
# reorder the study
prs_score$variable<-factor(prs_score$variable, levels = c("MVP-DBP","MVP-SBP","Pan UKBB-HTN","PRSsum"))

density_plot<- ggplot(prs_score, aes(value, fill=Background))+
  geom_density(alpha = 0.3)+
  scale_fill_discrete(na.translate=FALSE)+
  theme_bw()+  
  xlab("PRS")+
  labs(fill="Background")+ 
  theme(legend.position = "top",
        strip.text = element_text(size = 7))

pdf("output/here", height = 8, width = 8)

print(density_plot+ facet_wrap(~variable, scales = "free_x"))

dev.off()



