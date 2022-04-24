library(grid)
library(gridExtra)
library(ggplot2)
library(purrr)
library(data.table)
library(dplyr)
library(scales)


figure6_df<-read.csv("../Source_Data/2022-03-12_Data_figure6.csv")


# group the Trait and separate with an empty line
figure6_df<- figure6_df %>% 
             split(.$Trait) %>%
             purrr::map(~add_row(.,Trait=unique(.$Trait))) %>% 
             bind_rows()
  
head(figure6_df)

# Reorder row based on Trait
figure6_df<- figure6_df %>% arrange(match(Trait, c("HTN_at_baseline","normal_to_hypertension","elevated_to_hypertension")))

# remove last row 
figure6_df<-figure6_df[-nrow(figure6_df),]

# Add 2 new empty line on the top for the title purpose
figure6_df<- figure6_df %>% add_row(Trait=c(NA,NA),.before = 1)

#Add row number
figure6_df$ID<-1:nrow(figure6_df)

# reverse the order levels
figure6_df$ID<-factor(figure6_df$ID, levels = c(nrow(figure6_df):1))

#set min x axis

min_x<-min(figure6_df$ci_low, na.rm = T)+0.2

# add arrow position(>0), if OR ci_hi > 2.8, set arrow pos at 2.7
figure6_df <- figure6_df %>% mutate( arrow_max = ifelse(ci_hi > 2.8, 2.7 ,NA))

#add arrow position (< 0), if OR ci_hi < 0.8, set arrow pos at 0.9
figure6_df <- figure6_df %>% mutate( arrow_min = ifelse(ci_low < 0.8, 0.9 ,NA))

forest_plot <- ggplot(figure6_df, aes(x = OR, y = ID)) +
                      geom_errorbar(aes(xmin=ci_low, xmax=ci_hi), 
                                    width=.3, position=position_dodge(.6))+
                      geom_point(position=position_dodge(.5), size=2)+
                      geom_vline(xintercept = 1, linetype = 3) + 
                      xlab("Odd ratios (log scale)") + ggtitle(" ")+
                      coord_cartesian(xlim=c(0.75,2.63))+
                      scale_colour_identity() +
                      scale_y_discrete(limits = rev(figure6_df$ID),
                                       expand = c(0.0005,0.01)) + 
                      theme_minimal() +
                      scale_x_continuous(trans = "log", 
                                         breaks = c(0.6,1,1.4,1.8,2.4))+
                      theme(axis.text.y = element_blank(), 
                            axis.title.y = element_blank(), 
                            text = element_text(size=12),
                            plot.title = element_text(size=25))+
                    geom_segment(aes(y=ID, xend=arrow_max+.1, yend=ID), 
                                 arrow=arrow(length = unit(0.3, "cm")), show.legend = F)+
                    geom_segment(aes(y=ID, xend=arrow_min-0.195, yend=ID), 
                                 arrow=arrow(length = unit(0.3, "cm")), show.legend = F)+
                    annotate("text",x=1.8,y=16.5,
                             label = "Heterogeneity P < 1e-04", 
                             size=3.5,family="",fontface="italic" )  +
                    annotate("text",x=1.9,y=10.5,label = "Heterogeneity P=0.12",
                             size=3.5,family="",fontface="italic" )+
                    annotate("text",x=1.9,y=4.5,label = "Heterogeneity P=0.16", 
                             size=3.5,family="",fontface="italic" )




#### Add table / Info

## add separator for each trait with diffrent color
figure6_df$colour <- c("gray70","gray95" ,"white","white","white","white","white",
                       "gray95","white","white" ,"white","white","white",
                       "gray95","white" ,"white","white","white")

# make sure pvalue in scientific format
figure6_df$pval_assoc_with_outcome<-formatC(figure6_df$pval_assoc_with_outcome, format = "e", digits = 2)
figure6_df$AUC<-format(round(figure6_df$AUC, digits=3), nsmall = 3) 

figure6_df[]<-lapply(figure6_df, function(x) replace(x, grep("NA", x), ""))

# Add another empty line on the top(Title purpose)
figure6_df<-figure6_df %>% add_row(.before = 1)

info_table <- ggplot(data = figure6_df, aes(y = ID)) +
                    geom_hline(aes(yintercept = ID, colour = colour), 
                               size = 7) +
                    annotate("text", x = 2.6, y=18, 
                             label = "N", 
                             size=3.25, fontface="bold")+
                    annotate("text", x = 3.45, y=18, 
                              label = "OR [95% CI]", 
                              size=3.25,fontface="bold")+
                    annotate("text", x = 4.5, y=18, 
                              label = "P", 
                              size=3.25, fontface="bold")+
                    annotate("text", x = 5.1, y=18, 
                              label = "AUC", 
                              size=3.25, fontface="bold")+
                    annotate("text", x = 1.79, y=17, 
                              label = "Hypertension at baseline", 
                              size=3.5, fontface="italic")+
                    annotate("text", x = 2.3, y=11, 
                              label = "New onset hypertension (normotensive)", 
                              size=3.5, fontface="italic")+
                    annotate("text", x = 2.125, y=5, 
                              label = "New onset hypertension (elevated)", 
                              size=3.5, fontface="italic")+
                    geom_text(aes(x = 0.8, label = Race), 
                              size=3.25,hjust = 0, vjust=-0.3) +
                    geom_text(aes(x = 2.6, label = sample_size),
                              size=3.25,vjust=-0.3) +
                    geom_text(aes(x = 3.45, label = OR_CI),
                              size=3.25,vjust=-0.3)+ 
                    geom_text(aes(x = 4.77, label = pval_assoc_with_outcome),
                              size=3.25,vjust=-0.3,hjust = 1) + 
                    geom_text(aes(x = 5.3, label = AUC),
                              size=3.25,vjust=-0.3,hjust = 1) + 
                    scale_colour_identity() + 
                    ggtitle("")+
                    scale_y_discrete(limits = rev(figure6_df$ID), 
                                     expand = c(0.001,0.012)) +
                    theme_void() +
                    theme(plot.margin = margin(5, 5, 35, 5))

pdf("output/here", width = 8, height = 8)

grid.arrange(info_table,forest_plot, ncol = 2)

dev.off()



