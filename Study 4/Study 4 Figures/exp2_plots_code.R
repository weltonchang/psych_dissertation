### Study 4 ###
### Freedonia Experiment 2 plots code###
### welton@sas.upenn.edu ###
# last updated: 26 Nov 2017 #

options(scipen=999)
study_2_cleaned <- data.frame(study_2_cleaned)
data <- study_2_cleaned
rm(study_2_cleaned)

study_2_cleaned

#Factor conversion 
data$clarity <- as.factor(data$clarity)
data$account <- as.factor(data$account)

# Figure 3
require(grid)

theme_minimal <- function() {
  theme_bw() +
    theme(panel.border = element_blank(),
          axis.ticks.x = element_line(color = "grey80", size = 0),
          axis.ticks.y = element_line(color = "grey60", size = 0),
          axis.text = element_text(color = "grey30"),
          axis.text.y = element_text(color = "grey30", hjust = 1),
          panel.grid.major.x = element_line(colour = "grey90", size = 0.15),
          panel.grid.major.y = element_line(colour = "grey60", size = 0.15),
          panel.grid.minor = element_line(size = 0),
          #text = element_text(size = 14, family = "Open Sans"),
          panel.margin = unit(2, "lines"),
          plot.margin = unit(c(1.75, 1.75, 1.75, 1.75), "lines"),
          plot.title = element_text(hjust = 0)
    )
}

table2_graph
table2_graph <- data.frame(table2_graph)
table2_graph$clarity <- as.factor(table2_graph$X.clarity)

plot4 <- ggplot(data=table2_graph, aes(x=evidence, y=change, group=clarity, colour=clarity)) +
  theme_minimal() +
  geom_line(size=2) +
  #geom_point(size=3, color="black") +
  labs(y="Average Percent Change",x="Evidence") +
  scale_colour_manual(values=c("blue","red"),
                      name="Conditions",
                      labels=c("Weak Prior", "Strong Prior"))+
  guides(colour = guide_legend(reverse = TRUE)) +
  theme(axis.title.x=element_text(size=12), #increase x-axis label size and add padding
        axis.title.y=element_text(size=12), #increase y-axis label size and add padding
        axis.text=element_text(size=12), #increase axis tick text
        legend.title=element_text(size=12), #increase legend title text
        legend.text=element_text(size=12), #increase legend label text
        legend.key.size=unit(5,"lines"), #increase size of legend label keys
        legend.key=element_rect(fill="white")) #remove background of legend label keys

plot4

Freedonia_clean_all_answers
data<- Freedonia_clean_all_answers

# Interactions
data$avgabspercent <- as.numeric(data$avgabspercent)
data<- na.omit(data)

#Summarize data 
avg.abs.percent.all.conditions <- ddply(data, c("account","clarity"),summarise,
                                        N=length(id), 
                                        mean=mean(avgabspercent), 
                                        sd=sd(avgabspercent), 
                                        se=sd/sqrt(N))

#split summarized data in two (high/low account)
avg.abs.percent.all.conditions.account.0 <- avg.abs.percent.all.conditions[which(avg.abs.percent.all.conditions$account == 0),]
avg.abs.percent.all.conditions.account.1 <- avg.abs.percent.all.conditions[which(avg.abs.percent.all.conditions$account == 1),]

#for styling graph
colors = c('darkblue', 'darkred', 'grey','black')

#begin plot 
plot3 <- ggplot(data = avg.abs.percent.all.conditions, aes(x=clarity, y=mean)) + 
  theme_minimal()+
  geom_line(data=avg.abs.percent.all.conditions.account.0, aes(group = account, colour="Not Accountable"), size=1.3) +
  geom_line(data=avg.abs.percent.all.conditions.account.1, aes(group = account, colour="Accountable"), size=1.3) +
  geom_point(size=1)+ geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.06) +
  labs(x="Strength of Prior") +
  labs(y="Mean of the Absolute Percent Change across all pieces of evidence") +
  theme(legend.text=element_text(size=8)) +
  scale_x_discrete(breaks=c("0", "1"), labels=c("Weak Prior", "Strong Prior")) + 
  scale_color_manual(values=colors, name="Legend")+
  theme(axis.title.x=element_text(size=16), #increase x-axis label size and add padding
        axis.title.y=element_text(size=16), #increase y-axis label size and add padding
        axis.text=element_text(size=16), #increase axis tick text
        legend.title=element_text(size=16), #increase legend title text
        legend.text=element_text(size=16), #increase legend label text
        legend.key.size=unit(5,"lines"), #increase size of legend label keys
        legend.key=element_rect(fill="white")) #remove background of legend label keys
plot3
