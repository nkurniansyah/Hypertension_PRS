library(ggplot2)
library(dplyr)
library(purrr)
library(RColorBrewer)

figure3_df<-read.csv("../Source_Data/2022-03-12_Data_figure3.csv")

# group the studies and separate with an empty line
figure3_df<- figure3_df %>% 
  split(.$study) %>% 
  purrr::map(~add_row(.,study=unique(.$study),OR=NA ,pval_assoc_with_outcome=NA)) %>% 
  bind_rows()

# Reorder row based on gwas studies
figure3_df<- figure3_df %>% arrange(match(study, c("MVP-DBP","MVP-SBP","UKBB-HTN", "PRSsum")))

# remove last row 
figure3_df<-figure3_df[-nrow(figure3_df),]

# Add 2 new empty line on the top for the title purpose
figure3_df<- figure3_df %>% add_row(study=c(NA,NA),.before = 1)

# rename row number
figure3_df$ID<-1:nrow(figure3_df)

# reverse the order levels
figure3_df$ID<-factor(figure3_df$ID, levels = c(nrow(figure3_df):1))

forest_plot <- ggplot(figure3_df, aes(x = OR, y = ID, colour=Threshold)) +
                      geom_errorbar(aes(xmin=ci_low, xmax=ci_hi), width=.3, position=position_dodge(.6))+
                      geom_point(position=position_dodge(.5), size=2) +
                      scale_colour_discrete(na.translate = F)+ 
                      geom_vline(xintercept = 1, linetype = 3) + 
                      xlab("Odd ratios (log scale)") + ggtitle(" ")+
                      scale_y_discrete(limits = rev(figure3_df$ID),expand = c(0.0005,0.1)) + 
                      theme_minimal() +
                      scale_color_brewer(palette = "Dark2",na.translate=FALSE,  
                                        labels = c("Genome-wide Significant","Selected CV-PRS","Selected PVAL-PRS"),
                                        name="Threshold" )+
                      scale_x_continuous(trans = "log",breaks=c(1,1.2,1.4,1.6,1.8,2,2.2))+
                      theme(axis.text.y = element_blank(),
                            axis.title.y = element_blank(), 
                            text = element_text(size=10),
                            plot.title = element_text(size=16,hjust = 0.5))


## Add other information (pvalue, auc and etc)
forest_plot_w_info<-forest_plot+ 
                    geom_rect(data=figure3_df, 
                              aes(xmin=2.22, xmax=2.37, ymin=-Inf, ymax=Inf), 
                              fill="#efefe3",colour = NA)+
                    geom_text(data=figure3_df, aes(label=pval_assoc_with_outcome, y=ID, x=2.29), 
                              fontface="bold", size=2.5, colour= "black")+
                    ### Add pval header
                    annotate("text", x = 2.29, y= 16.2, label = "PVAL", size=3, fontface="bold",family="")+ 
                    # Add column for AUC [95% CI]
                    geom_rect(data=figure3_df, aes(xmin=2.4, xmax=2.8, ymin=-Inf, ymax=Inf), 
                                  fill="#efefe3",colour = NA)+
                    ## Add  AUC [95% CI] value 
                    geom_text(data=figure3_df, aes(label=AUC_CI, y=ID, x=2.6), 
                              fontface="bold", size=2.5, colour= "black")+
                    ### Add AUC [95% CI] value 
                    annotate("text", x = 2.6, y= 16.2,label = "AUC [95% CI]", 
                              size=2.5, fontface="bold",family="")+
                    theme(legend.position = "top")+
                    # Add title holder
                    geom_hline(yintercept =15.6, colour="black", size=6, alpha=0.6)+
                    geom_hline(yintercept =12.1, colour="black", size=6, alpha=0.6)+
                    geom_hline(yintercept =8.1, colour="black", size=6, alpha=0.6)+
                    geom_hline(yintercept =4.1, colour="black", size=6, alpha=0.6)+
                    ### Add PRS Name in title holder
                    annotate("text", x = 1.25, y=15.5,label = "Diastolic blood pressure - MVP ", 
                              size=2.8, fontface="italic",family="", vjust=0,colour="white")+
                    annotate("text", x = 1.25, y=12, label = "Systolic blood pressure - MVP",
                              size=2.8, fontface="italic",family="", vjust=0,colour="white")+
                    annotate("text", x = 1.25, y=8, label = "Hypertension - UKBB", 
                              size=2.8, fontface="italic",family="", vjust=0,colour="white")+
                    annotate("text", x =1.25,  y=4, label = "PRSsum", 
                              size=2.8,fontface="italic",family="", vjust=0,colour="white")

#Save plot

jpeg("output/here", units = "in",height = 6, width = 7, res = 300)

forest_plot_w_info

dev.off()





