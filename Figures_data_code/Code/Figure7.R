library(ggplot2)
library(dplyr)


figure7_df<- read.csv("../Source_Data/2022-03-12_Data_figure7.csv")

#reorder facet wrap
figure7_df$Race<-factor(figure7_df$Race, levels = c("Black","White","Combined sample"))

plot<-ggplot(figure7_df, aes(age, OR)) + 
            geom_smooth(aes(ymin = low_OR, 
                            ymax = high_OR, 
                            fill = group, 
                            colour = group), stat = "identity" ) + 
            scale_colour_manual("PRS strata",values=c('#000000','#d95f0e', '#2b8cbe','#7BC8A4' ),
                              labels=c("< 10 %", "10-50 %", "50-90 %", "> 90 %")) + 
            scale_fill_manual("PRS strata",values=c('#000000','#d95f0e', '#2b8cbe','#7BC8A4'), 
                              labels=c("< 10 %", "10-50 %", "50-90 %", "> 90 %")) + 
            xlab("Age") + 
            geom_hline(yintercept = 1, 
                       size=0.5) + 
            scale_y_continuous(trans = "log", breaks = c(1,1.4,1.8,2.2,2.6))+
            ylab("OR (log scale) of hypertension relative to age 17 ") + 
            theme_bw() +
            theme(text = element_text(size=10),
                  legend.title=element_text(size=11), 
                  legend.text=element_text(size=10),
                  strip.text = element_text(size = 10), legend.position = "top")


pdf("output/here",  width=8, height = 4)

plot+ facet_wrap(~Race, scales = "free_x")

dev.off()




