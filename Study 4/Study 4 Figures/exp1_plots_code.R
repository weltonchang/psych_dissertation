### Study 4 ###
### Freedonia - Experiment 1 plots code###
### welton@sas.upenn.edu ###
# last updated: 26 Nov 2017 #

options(scipen=999)

data <- Freedonia_clean
rm(Freedonia_clean)

#Factor conversion 
data$clarity <- as.factor(data$clarity)
data$account <- as.factor(data$account)
data$feltacc <- as.factor(data$feltacc)

#Creating subsets 
LC <- data[which(data$clarity == 0),]
HC <- data[which(data$clarity == 1),]
LA <- data[which(data$account == 0),]
HA <- data[which(data$feltacc == 1),]

#Results, Table 2, Calculations
#Percent Difference 1-2
temp1 <- (abs(LC$ev2-LC$ev1)/LC$ev1)
temp2 <- (abs(HC$ev2-HC$ev1)/HC$ev1)
temp1[!is.finite(temp1)] <- NA #Set infinite values within the array test to NA
temp2[!is.finite(temp2)] <- NA
ev1to2 <- t.test(temp1,temp2)
ev1to2

#Percent Difference 2-3
temp1 <- (abs(LC$ev3-LC$ev2)/LC$ev2)
temp2 <- (abs(HC$ev3-HC$ev2)/HC$ev2)
temp1[!is.finite(temp1)] <- NA
temp2[!is.finite(temp2)] <- NA
ev2to3 <- t.test(temp1,temp2)
ev2to3 

#Percent Difference 3-4
temp1 <- (abs(LC$ev4-LC$ev3)/LC$ev3)
temp2 <- (abs(HC$ev4-HC$ev3)/HC$ev3)
temp1[!is.finite(temp1)] <- NA
temp2[!is.finite(temp2)] <- NA
ev3to4 <- t.test(temp1,temp2)
ev3to4

#Percent Difference 4-5
temp1 <- (abs(LC$ev5-LC$ev4)/LC$ev4)
temp2 <- (abs(HC$ev5-HC$ev4)/HC$ev4)
temp1[!is.finite(temp1)] <- NA
temp2[!is.finite(temp2)] <- NA
ev4to5 <- t.test(temp1,temp2)
ev4to5

#Percent Difference 5-6
temp1 <- (abs(LC$ev6-LC$ev5)/LC$ev5)
temp2 <- (abs(HC$ev6-HC$ev5)/HC$ev5)
temp1[!is.finite(temp1)] <- NA
temp2[!is.finite(temp2)] <- NA
ev5to6 <- t.test(temp1,temp2)
ev5to6

#Percent Difference 6-7
temp1 <- (abs(LC$ev7-LC$ev6)/LC$ev6)
temp2 <- (abs(HC$ev7-HC$ev6)/HC$ev6)
temp1[!is.finite(temp1)] <- NA
temp2[!is.finite(temp2)] <- NA
ev6to7 <- t.test(temp1,temp2)
ev6to7

#Results of these t-tests are in the freedonia results document

#Begin Graph of Table 2

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

library(ggplot2)

# Study 4, Figure 1

table2_graph$clarity <- as.factor(table2_graph$clarity)

plot <- ggplot(data=table2_graph, aes(x=evidence, y=change, group=clarity, colour=clarity)) +
  theme_minimal() +
  geom_line(size=3) +
  #geom_point(size=3, color="black") +
  labs(y="Average Percent Change",x="Evidence") +
  scale_colour_manual(values=c("blue","red"),
                      name="Conditions",
                      labels=c("Weak Prior", "Strong Prior"))+
  guides(colour = guide_legend(reverse = TRUE)) +
  theme(axis.title.x=element_text(size=16), #increase x-axis label size and add padding
        axis.title.y=element_text(size=16), #increase y-axis label size and add padding
        axis.text=element_text(size=16), #increase axis tick text
        legend.title=element_text(size=16), #increase legend title text
        legend.text=element_text(size=16), #increase legend label text
        legend.key.size=unit(5,"lines"), #increase size of legend label keys
        legend.key=element_rect(fill="white")) #remove background of legend label keys

plot

# Interactions
data$avgabspercent <- as.numeric(data$avgabspercent)

#Summarize data 
avg.abs.percent.all.conditions <- ddply(data, c("account","clarity"),summarise,
                                        N=length(userid), 
                                        mean=mean(avgabspercent), 
                                        sd=sd(avgabspercent), 
                                        se=sd/sqrt(N))

#split summarized data in two (high/low account)
avg.abs.percent.all.conditions.account.0 <- avg.abs.percent.all.conditions[which(avg.abs.percent.all.conditions$account == 0),]
avg.abs.percent.all.conditions.account.1 <- avg.abs.percent.all.conditions[which(avg.abs.percent.all.conditions$account == 1),]

#for styling graph
colors = c('darkblue', 'darkred', 'grey','black')

#begin plot 
plot2 <- ggplot(data = avg.abs.percent.all.conditions, aes(x=clarity, y=mean)) + 
  theme_minimal() +
  geom_line(data=avg.abs.percent.all.conditions.account.0, aes(group = account, colour="Not Accountable"), size=1.3) +
  geom_line(data=avg.abs.percent.all.conditions.account.1, aes(group = account, colour="Accountable"), size=1.3) +
  geom_point(size=4)+ geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.08) +
  labs(x="Strength of Prior") +
  labs(y="Mean of the Absolute Percent Change across all pieces of evidence") +
  theme(legend.text=element_text(size=8)) +
  scale_x_discrete(breaks=c("0", "1"), labels=c("Weak Prior", "Strong Prior")) + 
  scale_color_manual(values=colors, name="Legend") +
  theme(axis.title.x=element_text(size=12), #increase x-axis label size and add padding
        axis.title.y=element_text(size=12), #increase y-axis label size and add padding
        axis.text=element_text(size=12), #increase axis tick text
        legend.title=element_text(size=12), #increase legend title text
        legend.text=element_text(size=12), #increase legend label text
        legend.key.size=unit(5,"lines"), #increase size of legend label keys
        legend.key=element_rect(fill="white")) #remove background of legend label keys
plot2
