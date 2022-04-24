library(ggplot2)
library(data.table)
library(dplyr)
library(viridis)

prs_decile<-read.csv("../Source_Data/2022-03-12_Data_figure5.csv")
tail(prs_decile)


### Reoreder htn type
prs_decile$htn_type<- factor(prs_decile$htn_type, levels = c("Always Hypertension","Worsen","Improve","Always Normal & Elevated"))
prs_decile$quantile<- factor(prs_decile$quantile, levels = 1:10, labels = c(paste0("Q",1:10)))
head(prs_decile)



decile_plot<-ggplot(prs_decile, aes(x = quantile,fill = htn_type)) +
                    geom_bar(position = "fill") +
                    scale_y_continuous(breaks = seq(0, 1, .2), 
                    label = percent) +
                    stat_count(geom = "text", 
                              aes(label = paste(round((..count..)))),
                              position=position_fill(vjust=0.5), colour="red", size=3)+
                    scale_fill_viridis(discrete = TRUE)+ 
                    labs(y = "Percent", 
                         fill = "HTN levels",
                         x = "") +
                    theme_bw() + 
                    theme(legend.position = "top",
                          legend.text=element_text(size=9),
                          legend.title=element_text(size=9.5))




pdf("output/here", height = 6, width = 7.25)

decile_plot

dev.off()





