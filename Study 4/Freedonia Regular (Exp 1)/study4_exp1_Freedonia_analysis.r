### Study 4 ###
### Freedonia Experiment 1 analysis###
### welton@sas.upenn.edu ###
# last updated: 26 Nov 2017 #

#Load libraries
library(plyr)
library(ggplot2)
options(scipen=999)


#Manually import data sets {Freedonia_clean.csv}

#Clone and rename data sets
original <- Freedonia_clean

#Remove old data sets 
rm(Freedonia_clean)

#Coverting the condition columns into factors 
original$clarity <- as.factor(original$clarity)
original$account <- as.factor(original$account)
original$feltacc <- as.factor(original$feltacc)


#Creating low and high clarity subsets of data sets
original.LC <- original[which(original$clarity == 0),] #N=123
original.HC <- original[which(original$clarity == 1),] #N=123


#Creating low and high accountability subsets of data sets 
original.LA <- original[which(original$account == 0),]
original.HA <- original[which(original$feltacc == 1),]


#Creating Accountability/Clarity subsets
original.LC_HA <- original.LC[which(original.LC$feltacc == 1),]
original.LC_LA <- original.LC[which(original.LC$account == 0),]
original.HC_HA <- original.HC[which(original.LC$feltacc == 1),]
original.HC_LA <- original.HC[which(original.LC$account == 0),]


#Palette creation
colors = c('darkblue', 'darkred', 'grey','black')

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Accountability vs. No Accountability

#original_low_vs_high_accountability_average_response
evidence <- c(1,2,3,4,5,6,7)
LA.ave <- c(mean(original.LA$ev1),mean(original.LA$ev2), mean(original.LA$ev3), mean(original.LA$ev4), mean(original.LA$ev5), mean(original.LA$ev6), mean(original.LA$ev7))
HA.ave <- c(mean(original.HA$ev1),mean(original.HA$ev2), mean(original.HA$ev3), mean(original.HA$ev4), mean(original.HA$ev5), mean(original.HA$ev6), mean(original.HA$ev7))
chart <- data.frame(evidence, LA.ave, HA.ave)
original.low_vs_high_accountability_average_response <- ggplot(chart, aes(x=evidence, y=LA.ave, colour="No Accountability")) + geom_line(size=1.3)
original.low_vs_high_accountability_average_response <- original.low_vs_high_accountability_average_response + geom_line(aes(y=HA.ave, colour="Accountability"),size=1.3)
original.low_vs_high_accountability_average_response <- original.low_vs_high_accountability_average_response + labs(y="Average Assigned Probability")
original.low_vs_high_accountability_average_response <- original.low_vs_high_accountability_average_response + labs(x="Evidence Number")
original.low_vs_high_accountability_average_response <- original.low_vs_high_accountability_average_response + scale_color_manual(values=colors, name = "Conditions")
original.low_vs_high_accountability_average_response <- original.low_vs_high_accountability_average_response + theme(legend.text=element_text(size=8))
original.low_vs_high_accountability_average_response_50_opacity <- original.low_vs_high_accountability_average_response + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.low_vs_high_accountability_average_response_transparent <- original.low_vs_high_accountability_average_response + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_low_vs_high_accountability_relative_difference_average
evidence <- c(1,2,3,4,5,6,7)
LA.ave <- c(mean(original.LA$ev1reldiff)/50,mean(original.LA$ev2reldiff)/mean(original.LA$ev1), mean(original.LA$ev3reldiff)/mean(original.LA$ev2), mean(original.LA$ev4reldiff)/mean(original.LA$ev3), mean(original.LA$ev5reldiff)/mean(original.LA$ev4), mean(original.LA$ev6reldiff)/mean(original.LA$ev5), mean(original.LA$ev7reldiff)/mean(original.LA$ev6))
HA.ave <- c(mean(original.HA$ev1reldiff)/50,mean(original.HA$ev2reldiff)/mean(original.HA$ev1), mean(original.HA$ev3reldiff)/mean(original.HA$ev2), mean(original.HA$ev4reldiff)/mean(original.HA$ev3), mean(original.HA$ev5reldiff)/mean(original.HA$ev4), mean(original.HA$ev6reldiff)/mean(original.HA$ev5), mean(original.HA$ev7reldiff)/mean(original.HA$ev6))
chart <- data.frame(evidence, LA.ave, HA.ave)
original.low_vs_high_accountability_relative_difference_average <- ggplot(chart, aes(x=evidence, y=LA.ave, colour="No Accountability")) + geom_line(size=1.3)
original.low_vs_high_accountability_relative_difference_average <- original.low_vs_high_accountability_relative_difference_average + geom_line(aes(y=HA.ave, colour="Accountability"),size=1.3)
original.low_vs_high_accountability_relative_difference_average <- original.low_vs_high_accountability_relative_difference_average + labs(x="Evidence Number")
original.low_vs_high_accountability_relative_difference_average <- original.low_vs_high_accountability_relative_difference_average + labs(y="Relative Change in Assigned Probability (average)")
original.low_vs_high_accountability_relative_difference_average <- original.low_vs_high_accountability_relative_difference_average + scale_color_manual(values=colors, name = "Conditions")
original.low_vs_high_accountability_relative_difference_average <- original.low_vs_high_accountability_relative_difference_average + theme(legend.text=element_text(size=8))
original.low_vs_high_accountability_relative_difference_average_50_opacity <- original.low_vs_high_accountability_relative_difference_average + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.low_vs_high_accountability_relative_difference_average_transparent <- original.low_vs_high_accountability_relative_difference_average + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_low_vs_high_accountability_relative_difference_average_per_condition_per_condition
evidence <- c(1,2,3,4,5,6,7)
LA.ave <- c(mean(original.LA$ev1reldiff)/50,mean(original.LA$ev2reldiff)/mean(original.LA$ev1), mean(original.LA$ev3reldiff)/mean(original.LA$ev2), mean(original.LA$ev4reldiff)/mean(original.LA$ev3), mean(original.LA$ev5reldiff)/mean(original.LA$ev4), mean(original.LA$ev6reldiff)/mean(original.LA$ev5), mean(original.LA$ev7reldiff)/mean(original.LA$ev6))
HA.ave <- c(mean(original.HA$ev1reldiff)/50,mean(original.HA$ev2reldiff)/mean(original.HA$ev1), mean(original.HA$ev3reldiff)/mean(original.HA$ev2), mean(original.HA$ev4reldiff)/mean(original.HA$ev3), mean(original.HA$ev5reldiff)/mean(original.HA$ev4), mean(original.HA$ev6reldiff)/mean(original.HA$ev5), mean(original.HA$ev7reldiff)/mean(original.HA$ev6))
chart <- data.frame(evidence, LA.ave, HA.ave)
original.low_vs_high_accountability_relative_difference_average_per_condition <- ggplot(chart, aes(x=evidence, y=LA.ave, colour="No Accountability")) + geom_line(size=1.3)
original.low_vs_high_accountability_relative_difference_average_per_condition <- original.low_vs_high_accountability_relative_difference_average_per_condition + geom_line(aes(y=HA.ave, colour="Accountability"),size=1.3)
original.low_vs_high_accountability_relative_difference_average_per_condition <- original.low_vs_high_accountability_relative_difference_average_per_condition + labs(x="Evidence Number")
original.low_vs_high_accountability_relative_difference_average_per_condition <- original.low_vs_high_accountability_relative_difference_average_per_condition + labs(y="Relative Change in Assigned Probability (average per condition)")
original.low_vs_high_accountability_relative_difference_average_per_condition <- original.low_vs_high_accountability_relative_difference_average_per_condition + scale_color_manual(values=colors, name = "Conditions")
original.low_vs_high_accountability_relative_difference_average_per_condition <- original.low_vs_high_accountability_relative_difference_average_per_condition + theme(legend.text=element_text(size=8))
original.low_vs_high_accountability_relative_difference_average_per_condition_50_opacity <- original.low_vs_high_accountability_relative_difference_average_per_condition + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.low_vs_high_accountability_relative_difference_average_per_condition_transparent <- original.low_vs_high_accountability_relative_difference_average_per_condition + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_low_vs_high_accountability_absolute_accuracy_average
evidence <- c(1,2,3,4,5,6,7)
LA.ave <- c(mean(original.LA$ev1abs_acc),mean(original.LA$ev2abs_acc), mean(original.LA$ev3abs_acc), mean(original.LA$ev4abs_acc), mean(original.LA$ev5abs_acc), mean(original.LA$ev6abs_acc), mean(original.LA$ev7abs_acc))
HA.ave <- c(mean(original.HA$ev1abs_acc),mean(original.HA$ev2abs_acc), mean(original.HA$ev3abs_acc), mean(original.HA$ev4abs_acc), mean(original.HA$ev5abs_acc), mean(original.HA$ev6abs_acc), mean(original.HA$ev7abs_acc))
chart <- data.frame(evidence, LA.ave, HA.ave)
original.low_vs_high_accountability_absolute_accuracy_average <- ggplot(chart, aes(x=evidence, y=LA.ave, colour="No Accountability")) + geom_line(size=1.3)
original.low_vs_high_accountability_absolute_accuracy_average <- original.low_vs_high_accountability_absolute_accuracy_average + geom_line(aes(y=HA.ave, colour="Accountability"), size=1.3)
original.low_vs_high_accountability_absolute_accuracy_average <- original.low_vs_high_accountability_absolute_accuracy_average + labs(x="Evidence Number")
original.low_vs_high_accountability_absolute_accuracy_average <- original.low_vs_high_accountability_absolute_accuracy_average + labs(y="Absolute Accuracy in Assigned Probability (average)")
original.low_vs_high_accountability_absolute_accuracy_average <- original.low_vs_high_accountability_absolute_accuracy_average + scale_color_manual(values=colors, name = "Conditions")
original.low_vs_high_accountability_absolute_accuracy_average <- original.low_vs_high_accountability_absolute_accuracy_average + theme(legend.text=element_text(size=8))
original.low_vs_high_accountability_absolute_accuracy_average_50_opacity <- original.low_vs_high_accountability_absolute_accuracy_average + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.low_vs_high_accountability_absolute_accuracy_average_transparent <- original.low_vs_high_accountability_absolute_accuracy_average + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_low_vs_high_accountability_absolute_accuracy_average_per_condition
evidence <- c(1,2,3,4,5,6,7)
LA.ave <- c(mean(original.LA$ev1abs_acc),mean(original.LA$ev2abs_acc), mean(original.LA$ev3abs_acc), mean(original.LA$ev4abs_acc), mean(original.LA$ev5abs_acc), mean(original.LA$ev6abs_acc), mean(original.LA$ev7abs_acc))
HA.ave <- c(mean(original.HA$ev1abs_acc),mean(original.HA$ev2abs_acc), mean(original.HA$ev3abs_acc), mean(original.HA$ev4abs_acc), mean(original.HA$ev5abs_acc), mean(original.HA$ev6abs_acc), mean(original.HA$ev7abs_acc))
chart <- data.frame(evidence, LA.ave, HA.ave)
original.low_vs_high_accountability_absolute_accuracy_average_per_condition <- ggplot(chart, aes(x=evidence, y=LA.ave, colour="No Accountability")) + geom_line(size=1.3)
original.low_vs_high_accountability_absolute_accuracy_average_per_condition <- original.low_vs_high_accountability_absolute_accuracy_average_per_condition + geom_line(aes(y=HA.ave, colour="Accountability"), size=1.3)
original.low_vs_high_accountability_absolute_accuracy_average_per_condition <- original.low_vs_high_accountability_absolute_accuracy_average_per_condition + labs(x="Evidence Number")
original.low_vs_high_accountability_absolute_accuracy_average_per_condition <- original.low_vs_high_accountability_absolute_accuracy_average_per_condition + labs(y="Absolute Accuracy in Assigned Probability (average per condition)")
original.low_vs_high_accountability_absolute_accuracy_average_per_condition <- original.low_vs_high_accountability_absolute_accuracy_average_per_condition + scale_color_manual(values=colors, name = "Conditions")
original.low_vs_high_accountability_absolute_accuracy_average_per_condition <- original.low_vs_high_accountability_absolute_accuracy_average_per_condition + theme(legend.text=element_text(size=8))
original.low_vs_high_accountability_absolute_accuracy_average_per_condition_50_opacity <- original.low_vs_high_accountability_absolute_accuracy_average_per_condition + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.low_vs_high_accountability_absolute_accuracy_average_per_condition_transparent <- original.low_vs_high_accountability_absolute_accuracy_average_per_condition + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Low vs. High Clarity 

#original_low_vs_high_clarity_average_response
evidence <- c(1,2,3,4,5,6,7)
LC.ave <- c(mean(original.LC$ev1),mean(original.LC$ev2), mean(original.LC$ev3), mean(original.LC$ev4), mean(original.LC$ev5), mean(original.LC$ev6), mean(original.LC$ev7))
HC.ave <- c(mean(original.HC$ev1),mean(original.HC$ev2), mean(original.HC$ev3), mean(original.HC$ev4), mean(original.HC$ev5), mean(original.HC$ev6), mean(original.HC$ev7))
chart <- data.frame(evidence, LC.ave, HC.ave)
original.original_low_vs_high_clarity_average_response <- ggplot(chart, aes(x=evidence, y=LC.ave, colour="Low Clarity")) + geom_line(size=1.3)
original.original_low_vs_high_clarity_average_response <- original.original_low_vs_high_clarity_average_response + geom_line(aes(y=HC.ave, colour="High Clarity"),size=1.3)
original.original_low_vs_high_clarity_average_response <- original.original_low_vs_high_clarity_average_response + labs(y="Average Assigned Probability")
original.original_low_vs_high_clarity_average_response <- original.original_low_vs_high_clarity_average_response + labs(x="Evidence Number")
original.original_low_vs_high_clarity_average_response <- original.original_low_vs_high_clarity_average_response + scale_color_manual(values=colors, name = "Conditions")
original.original_low_vs_high_clarity_average_response <- original.original_low_vs_high_clarity_average_response + theme(legend.text=element_text(size=8))
original.original_low_vs_high_clarity_average_response_50_opacity <- original.original_low_vs_high_clarity_average_response + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.original_low_vs_high_clarity_average_response_transparent <- original.original_low_vs_high_clarity_average_response + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_low_vs_high_credibility_relative_difference_average
evidence <- c(1,2,3,4,5,6,7)
LC.ave <- c(mean(original.LC$ev1reldiff)/50,mean(original.LC$ev2reldiff)/mean(original.LC$ev1), mean(original.LC$ev3reldiff)/mean(original.LC$ev2), mean(original.LC$ev4reldiff)/mean(original.LC$ev3), mean(original.LC$ev5reldiff)/mean(original.LC$ev4), mean(original.LC$ev6reldiff)/mean(original.LC$ev5), mean(original.LC$ev7reldiff)/mean(original.LC$ev6))
HC.ave <- c(mean(original.HC$ev1reldiff)/50,mean(original.HC$ev2reldiff)/mean(original.HC$ev1), mean(original.HC$ev3reldiff)/mean(original.HC$ev2), mean(original.HC$ev4reldiff)/mean(original.HC$ev3), mean(original.HC$ev5reldiff)/mean(original.HC$ev4), mean(original.HC$ev6reldiff)/mean(original.HC$ev5), mean(original.HC$ev7reldiff)/mean(original.HC$ev6))
chart <- data.frame(evidence, LC.ave, HC.ave)
original.low_vs_high_accountability_relative_difference_average <- ggplot(chart, aes(x=evidence, y=LC.ave, colour="Low Clarity")) + geom_line(size=1.3)
original.low_vs_high_accountability_relative_difference_average <- original.low_vs_high_accountability_relative_difference_average + geom_line(aes(y=HC.ave, colour="High Clarity"),size=1.3)
original.low_vs_high_accountability_relative_difference_average <- original.low_vs_high_accountability_relative_difference_average + labs(x="Evidence Number")
original.low_vs_high_accountability_relative_difference_average <- original.low_vs_high_accountability_relative_difference_average + labs(y="Relative Change in Assigned Probability (average)")
original.low_vs_high_accountability_relative_difference_average <- original.low_vs_high_accountability_relative_difference_average + scale_color_manual(values=colors, name = "Conditions")
original.low_vs_high_accountability_relative_difference_average <- original.low_vs_high_accountability_relative_difference_average + theme(legend.text=element_text(size=8))
original.low_vs_high_accountability_relative_difference_average_50_opacity <- original.low_vs_high_accountability_relative_difference_average + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.low_vs_high_accountability_relative_difference_average_transparent <- original.low_vs_high_accountability_relative_difference_average + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_low_vs_high_credibility_relative_difference_average_per_condition
evidence <- c(1,2,3,4,5,6,7)
LC.ave <- c(mean(original.LC$ev1reldiff)/50,mean(original.LC$ev2reldiff)/mean(original.LC$ev1), mean(original.LC$ev3reldiff)/mean(original.LC$ev2), mean(original.LC$ev4reldiff)/mean(original.LC$ev3), mean(original.LC$ev5reldiff)/mean(original.LC$ev4), mean(original.LC$ev6reldiff)/mean(original.LC$ev5), mean(original.LC$ev7reldiff)/mean(original.LC$ev6))
HC.ave <- c(mean(original.HC$ev1reldiff)/50,mean(original.HC$ev2reldiff)/mean(original.HC$ev1), mean(original.HC$ev3reldiff)/mean(original.HC$ev2), mean(original.HC$ev4reldiff)/mean(original.HC$ev3), mean(original.HC$ev5reldiff)/mean(original.HC$ev4), mean(original.HC$ev6reldiff)/mean(original.HC$ev5), mean(original.HC$ev7reldiff)/mean(original.HC$ev6))
chart <- data.frame(evidence, LC.ave, HC.ave)
original.low_vs_high_accountability_relative_difference_average_per_condition <- ggplot(chart, aes(x=evidence, y=LC.ave, colour="Low Clarity")) + geom_line(size=1.3)
original.low_vs_high_accountability_relative_difference_average_per_condition <- original.low_vs_high_accountability_relative_difference_average_per_condition + geom_line(aes(y=HC.ave, colour="High Clarity"),size=1.3)
original.low_vs_high_accountability_relative_difference_average_per_condition <- original.low_vs_high_accountability_relative_difference_average_per_condition + labs(x="Evidence Number")
original.low_vs_high_accountability_relative_difference_average_per_condition <- original.low_vs_high_accountability_relative_difference_average_per_condition + labs(y="Relative Change in Assigned Probability (average per condition)")
original.low_vs_high_accountability_relative_difference_average_per_condition <- original.low_vs_high_accountability_relative_difference_average_per_condition + scale_color_manual(values=colors, name = "Conditions")
original.low_vs_high_accountability_relative_difference_average_per_condition <- original.low_vs_high_accountability_relative_difference_average_per_condition + theme(legend.text=element_text(size=8))
original.low_vs_high_accountability_relative_difference_average_per_condition_50_opacity <- original.low_vs_high_accountability_relative_difference_average_per_condition + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.low_vs_high_accountability_relative_difference_average_per_condition_transparent <- original.low_vs_high_accountability_relative_difference_average_per_condition + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_low_vs_high_credibility_absolute_accuracy_average
evidence <- c(1,2,3,4,5,6,7)
LA.ave <- c(mean(original.LA$ev1abs_acc),mean(original.LA$ev2abs_acc), mean(original.LA$ev3abs_acc), mean(original.LA$ev4abs_acc), mean(original.LA$ev5abs_acc), mean(original.LA$ev6abs_acc), mean(original.LA$ev7abs_acc))
HA.ave <- c(mean(original.HA$ev1abs_acc),mean(original.HA$ev2abs_acc), mean(original.HA$ev3abs_acc), mean(original.HA$ev4abs_acc), mean(original.HA$ev5abs_acc), mean(original.HA$ev6abs_acc), mean(original.HA$ev7abs_acc))
chart <- data.frame(evidence, LA.ave, HA.ave)
original.low_vs_high_credibility_absolute_accuracy_average <- ggplot(chart, aes(x=evidence, y=LA.ave, colour="Low Clarity")) + geom_line(size=1.3)
original.low_vs_high_credibility_absolute_accuracy_average <- original.low_vs_high_credibility_absolute_accuracy_average + geom_line(aes(y=HA.ave, colour="High Credibility"), size=1.3)
original.low_vs_high_credibility_absolute_accuracy_average <- original.low_vs_high_credibility_absolute_accuracy_average + labs(x="Evidence Number")
original.low_vs_high_credibility_absolute_accuracy_average <- original.low_vs_high_credibility_absolute_accuracy_average + labs(y="Absolute Accuracy in Assigned Probability (average)")
original.low_vs_high_credibility_absolute_accuracy_average <- original.low_vs_high_credibility_absolute_accuracy_average + scale_color_manual(values=colors, name = "Conditions")
original.low_vs_high_credibility_absolute_accuracy_average <- original.low_vs_high_credibility_absolute_accuracy_average + theme(legend.text=element_text(size=8))
original.low_vs_high_credibility_absolute_accuracy_average_50_opacity <- original.low_vs_high_credibility_absolute_accuracy_average + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.low_vs_high_credibility_absolute_accuracy_average_transparent <- original.low_vs_high_credibility_absolute_accuracy_average + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_low_vs_high_credibility_absolute_accuracy_average_per_condition
evidence <- c(1,2,3,4,5,6,7)
LA.ave <- c(mean(original.LA$ev1abs_acc),mean(original.LA$ev2abs_acc), mean(original.LA$ev3abs_acc), mean(original.LA$ev4abs_acc), mean(original.LA$ev5abs_acc), mean(original.LA$ev6abs_acc), mean(original.LA$ev7abs_acc))
HA.ave <- c(mean(original.HA$ev1abs_acc),mean(original.HA$ev2abs_acc), mean(original.HA$ev3abs_acc), mean(original.HA$ev4abs_acc), mean(original.HA$ev5abs_acc), mean(original.HA$ev6abs_acc), mean(original.HA$ev7abs_acc))
chart <- data.frame(evidence, LA.ave, HA.ave)
original.low_vs_high_credibility_absolute_accuracy_average_per_condition <- ggplot(chart, aes(x=evidence, y=LA.ave, colour="Low Credibility")) + geom_line(size=1.3)
original.low_vs_high_credibility_absolute_accuracy_average_per_condition <- original.low_vs_high_credibility_absolute_accuracy_average_per_condition + geom_line(aes(y=HA.ave, colour="High Credibility"), size=1.3)
original.low_vs_high_credibility_absolute_accuracy_average_per_condition <- original.low_vs_high_credibility_absolute_accuracy_average_per_condition + labs(x="Evidence Number")
original.low_vs_high_credibility_absolute_accuracy_average_per_condition <- original.low_vs_high_credibility_absolute_accuracy_average_per_condition + labs(y="Absolute Accuracy in Assigned Probability (average per condition)")
original.low_vs_high_credibility_absolute_accuracy_average_per_condition <- original.low_vs_high_credibility_absolute_accuracy_average_per_condition + scale_color_manual(values=colors, name = "Conditions")
original.low_vs_high_credibility_absolute_accuracy_average_per_condition <- original.low_vs_high_credibility_absolute_accuracy_average_per_condition + theme(legend.text=element_text(size=8))
original.low_vs_high_credibility_absolute_accuracy_average_per_condition_50_opacity <- original.low_vs_high_credibility_absolute_accuracy_average_per_condition + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.low_vs_high_credibility_absolute_accuracy_average_per_condition_transparent <- original.low_vs_high_credibility_absolute_accuracy_average_per_condition + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#All Conditions

#original_all_conditions_average_response_and
evidence <- c(1,2,3,4,5,6,7)
LC_HA.ave <- c(mean(original.LC_HA$ev1),mean(original.LC_HA$ev2), mean(original.LC_HA$ev3), mean(original.LC_HA$ev4), mean(original.LC_HA$ev5), mean(original.LC_HA$ev6), mean(original.LC_HA$ev7))
LC_LA.ave <- c(mean(original.LC_LA$ev1),mean(original.LC_LA$ev2), mean(original.LC_LA$ev3), mean(original.LC_LA$ev4), mean(original.LC_LA$ev5), mean(original.LC_LA$ev6), mean(original.LC_LA$ev7))
HC_HA.ave <- c(mean(original.HC_HA$ev1),mean(original.HC_HA$ev2), mean(original.HC_HA$ev3), mean(original.HC_HA$ev4), mean(original.HC_HA$ev5), mean(original.HC_HA$ev6), mean(original.HC_HA$ev7))
HC_LA.ave <- c(mean(original.HC_LA$ev1),mean(original.HC_LA$ev2), mean(original.HC_LA$ev3), mean(original.HC_LA$ev4), mean(original.HC_LA$ev5), mean(original.HC_LA$ev6), mean(original.HC_LA$ev7))
chart <- data.frame(evidence, LC_HA.ave, LC_LA.ave, HC_HA.ave, HC_LA.ave)
original.all_conditions_average_response_and <- ggplot(chart, aes(x=evidence, y=LC_LA.ave, colour="Low Clarity and No Accountability")) + geom_line(size=1.3)
original.all_conditions_average_response_and <- original.all_conditions_average_response_and + geom_line(aes(y=LC_HA.ave, colour="Low Clarity and Accountability"), size=1.3)
original.all_conditions_average_response_and <- original.all_conditions_average_response_and + geom_line(aes(y=HC_LA.ave, colour="High Clarity and No Accountability"), size=1.3) 
original.all_conditions_average_response_and <- original.all_conditions_average_response_and + geom_line(aes(y=HC_HA.ave, colour="High Clarity and Accountability"), size=1.3)
original.all_conditions_average_response_and <- original.all_conditions_average_response_and + labs(x="Evidence Number")
original.all_conditions_average_response_and <- original.all_conditions_average_response_and + labs(y="Average Assigned Probability")
original.all_conditions_average_response_and <- original.all_conditions_average_response_and + scale_color_manual(values=colors, name = "Conditions")
original.all_conditions_average_response_and <- original.all_conditions_average_response_and + theme(legend.text=element_text(size=8))
original.all_conditions_average_response_and_50_opacity <- original.all_conditions_average_response_and+ theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.all_conditions_average_response_and_transparent <- original.all_conditions_average_response_and + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_all_conditions_average_response_with
evidence <- c(1,2,3,4,5,6,7)
LC_HA.ave <- c(mean(original.LC_HA$ev1),mean(original.LC_HA$ev2), mean(original.LC_HA$ev3), mean(original.LC_HA$ev4), mean(original.LC_HA$ev5), mean(original.LC_HA$ev6), mean(original.LC_HA$ev7))
LC_LA.ave <- c(mean(original.LC_LA$ev1),mean(original.LC_LA$ev2), mean(original.LC_LA$ev3), mean(original.LC_LA$ev4), mean(original.LC_LA$ev5), mean(original.LC_LA$ev6), mean(original.LC_LA$ev7))
HC_HA.ave <- c(mean(original.HC_HA$ev1),mean(original.HC_HA$ev2), mean(original.HC_HA$ev3), mean(original.HC_HA$ev4), mean(original.HC_HA$ev5), mean(original.HC_HA$ev6), mean(original.HC_HA$ev7))
HC_LA.ave <- c(mean(original.HC_LA$ev1),mean(original.HC_LA$ev2), mean(original.HC_LA$ev3), mean(original.HC_LA$ev4), mean(original.HC_LA$ev5), mean(original.HC_LA$ev6), mean(original.HC_LA$ev7))
chart <- data.frame(evidence, LC_HA.ave, LC_LA.ave, HC_HA.ave, HC_LA.ave)
original.all_conditions_average_response_with <- ggplot(chart, aes(x=evidence, y=LC_LA.ave, colour="Low Clarity with No Accountability")) + geom_line(size=1.3)
original.all_conditions_average_response_with <- original.all_conditions_average_response_with + geom_line(aes(y=LC_HA.ave, colour="Low Clarity with Accountability"), size=1.3)
original.all_conditions_average_response_with <- original.all_conditions_average_response_with + geom_line(aes(y=HC_LA.ave, colour="High Clarity with No Accountability"), size=1.3) 
original.all_conditions_average_response_with <- original.all_conditions_average_response_with + geom_line(aes(y=HC_HA.ave, colour="High Clarity with Accountability"), size=1.3)
original.all_conditions_average_response_with <- original.all_conditions_average_response_with + labs(x="Evidence Number")
original.all_conditions_average_response_with <- original.all_conditions_average_response_with + labs(y="Average Assigned Probability")
original.all_conditions_average_response_with <- original.all_conditions_average_response_with + scale_color_manual(values=colors, name = "Conditions")
original.all_conditions_average_response_with <- original.all_conditions_average_response_with + theme(legend.text=element_text(size=8))
original.all_conditions_average_response_with_50_opacity <- original.all_conditions_average_response_with+ theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.all_conditions_average_response_with_transparent <- original.all_conditions_average_response_with + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_all_conditions_relative_difference_and_average_per_condition
evidence <- c(1,2,3,4,5,6,7)
LC_HA.ave <- c(mean(original.LC_HA$ev1reldiff)/50,mean(original.LC_HA$ev2reldiff)/mean(original.LC_HA$ev1), mean(original.LC_HA$ev3reldiff)/mean(original.LC_HA$ev2), mean(original.LC_HA$ev4reldiff)/mean(original.LC_HA$ev3), mean(original.LC_HA$ev5reldiff)/mean(original.LC_HA$ev4), mean(original.LC_HA$ev6reldiff)/mean(original.LC_HA$ev5), mean(original.LC_HA$ev7reldiff)/mean(original.LC_HA$ev6))
LC_LA.ave <- c(mean(original.LC_LA$ev1reldiff)/50,mean(original.LC_LA$ev2reldiff)/mean(original.LC_LA$ev1), mean(original.LC_LA$ev3reldiff)/mean(original.LC_LA$ev2), mean(original.LC_LA$ev4reldiff)/mean(original.LC_LA$ev3), mean(original.LC_LA$ev5reldiff)/mean(original.LC_LA$ev4), mean(original.LC_LA$ev6reldiff)/mean(original.LC_LA$ev5), mean(original.LC_LA$ev7reldiff)/mean(original.LC_LA$ev6))
HC_HA.ave <- c(mean(original.HC_HA$ev1reldiff)/100,mean(original.HC_HA$ev2reldiff)/mean(original.HC_HA$ev1), mean(original.HC_HA$ev3reldiff)/mean(original.HC_HA$ev2), mean(original.HC_HA$ev4reldiff)/mean(original.HC_HA$ev3), mean(original.HC_HA$ev5reldiff)/mean(original.HC_HA$ev4), mean(original.HC_HA$ev6reldiff)/mean(original.HC_HA$ev5), mean(original.HC_HA$ev7reldiff)/mean(original.HC_HA$ev6))
HC_LA.ave <- c(mean(original.HC_LA$ev1reldiff)/100,mean(original.HC_LA$ev2reldiff)/mean(original.HC_LA$ev1), mean(original.HC_LA$ev3reldiff)/mean(original.HC_LA$ev2), mean(original.HC_LA$ev4reldiff)/mean(original.HC_LA$ev3), mean(original.HC_LA$ev5reldiff)/mean(original.HC_LA$ev4), mean(original.HC_LA$ev6reldiff)/mean(original.HC_LA$ev5), mean(original.HC_LA$ev7reldiff)/mean(original.HC_LA$ev6))
chart <- data.frame(evidence, LC_HA.ave, LC_LA.ave, HC_HA.ave, HC_LA.ave)
original.all_conditions_relative_difference_and_average_per_condition <- ggplot(chart, aes(x=evidence, y=LC_LA.ave, colour="Low Clarity and No Accountability")) + geom_line(size=1.3)
original.all_conditions_relative_difference_and_average_per_condition <- original.all_conditions_relative_difference_and_average_per_condition + geom_line(aes(y=LC_HA.ave, colour="Low Clarity and Accountability"),size=1.3)
original.all_conditions_relative_difference_and_average_per_condition <- original.all_conditions_relative_difference_and_average_per_condition + geom_line(aes(y=HC_LA.ave, colour="High Clarity and No Accountability"),size=1.3) 
original.all_conditions_relative_difference_and_average_per_condition <- original.all_conditions_relative_difference_and_average_per_condition + geom_line(aes(y=HC_HA.ave, colour="High Clarity and Accountability"),size=1.3)
original.all_conditions_relative_difference_and_average_per_condition <- original.all_conditions_relative_difference_and_average_per_condition + labs(x="Evidence Number")
original.all_conditions_relative_difference_and_average_per_condition <- original.all_conditions_relative_difference_and_average_per_condition + labs(y="Relative Change in Assigned Probability (average per condition)")
original.all_conditions_relative_difference_and_average_per_condition <- original.all_conditions_relative_difference_and_average_per_condition + scale_color_manual(values=colors, name = "Conditions")
original.all_conditions_relative_difference_and_average_per_condition <- original.all_conditions_relative_difference_and_average_per_condition + theme(legend.text=element_text(size=8))
original.all_conditions_relative_difference_and_average_per_condition_50_opacity <- original.all_conditions_relative_difference_and_average_per_condition + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.all_conditions_relative_difference_and_average_per_condition_transparent <- original.all_conditions_relative_difference_and_average_per_condition + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_all_conditions_relative_difference_with_average_per_condition
evidence <- c(1,2,3,4,5,6,7)
LC_HA.ave <- c(mean(original.LC_HA$ev1reldiff)/50,mean(original.LC_HA$ev2reldiff)/mean(original.LC_HA$ev1), mean(original.LC_HA$ev3reldiff)/mean(original.LC_HA$ev2), mean(original.LC_HA$ev4reldiff)/mean(original.LC_HA$ev3), mean(original.LC_HA$ev5reldiff)/mean(original.LC_HA$ev4), mean(original.LC_HA$ev6reldiff)/mean(original.LC_HA$ev5), mean(original.LC_HA$ev7reldiff)/mean(original.LC_HA$ev6))
LC_LA.ave <- c(mean(original.LC_LA$ev1reldiff)/50,mean(original.LC_LA$ev2reldiff)/mean(original.LC_LA$ev1), mean(original.LC_LA$ev3reldiff)/mean(original.LC_LA$ev2), mean(original.LC_LA$ev4reldiff)/mean(original.LC_LA$ev3), mean(original.LC_LA$ev5reldiff)/mean(original.LC_LA$ev4), mean(original.LC_LA$ev6reldiff)/mean(original.LC_LA$ev5), mean(original.LC_LA$ev7reldiff)/mean(original.LC_LA$ev6))
HC_HA.ave <- c(mean(original.HC_HA$ev1reldiff)/100,mean(original.HC_HA$ev2reldiff)/mean(original.HC_HA$ev1), mean(original.HC_HA$ev3reldiff)/mean(original.HC_HA$ev2), mean(original.HC_HA$ev4reldiff)/mean(original.HC_HA$ev3), mean(original.HC_HA$ev5reldiff)/mean(original.HC_HA$ev4), mean(original.HC_HA$ev6reldiff)/mean(original.HC_HA$ev5), mean(original.HC_HA$ev7reldiff)/mean(original.HC_HA$ev6))
HC_LA.ave <- c(mean(original.HC_LA$ev1reldiff)/100,mean(original.HC_LA$ev2reldiff)/mean(original.HC_LA$ev1), mean(original.HC_LA$ev3reldiff)/mean(original.HC_LA$ev2), mean(original.HC_LA$ev4reldiff)/mean(original.HC_LA$ev3), mean(original.HC_LA$ev5reldiff)/mean(original.HC_LA$ev4), mean(original.HC_LA$ev6reldiff)/mean(original.HC_LA$ev5), mean(original.HC_LA$ev7reldiff)/mean(original.HC_LA$ev6))
chart <- data.frame(evidence, LC_HA.ave, LC_LA.ave, HC_HA.ave, HC_LA.ave)
original.all_conditions_relative_difference_with_average_per_condition <- ggplot(chart, aes(x=evidence, y=LC_LA.ave, colour="Low Clarity with No Accountability")) + geom_line(size=1.3)
original.all_conditions_relative_difference_with_average_per_condition <- original.all_conditions_relative_difference_with_average_per_condition + geom_line(aes(y=LC_HA.ave, colour="Low Clarity with Accountability"),size=1.3)
original.all_conditions_relative_difference_with_average_per_condition <- original.all_conditions_relative_difference_with_average_per_condition + geom_line(aes(y=HC_LA.ave, colour="High Clarity with No Accountability"),size=1.3) 
original.all_conditions_relative_difference_with_average_per_condition <- original.all_conditions_relative_difference_with_average_per_condition + geom_line(aes(y=HC_HA.ave, colour="High Clarity with Accountability"),size=1.3)
original.all_conditions_relative_difference_with_average_per_condition <- original.all_conditions_relative_difference_with_average_per_condition + labs(x="Evidence Number")
original.all_conditions_relative_difference_with_average_per_condition <- original.all_conditions_relative_difference_with_average_per_condition + labs(y="Relative Change in Assigned Probability (average per condition)")
original.all_conditions_relative_difference_with_average_per_condition <- original.all_conditions_relative_difference_with_average_per_condition + scale_color_manual(values=colors, name = "Conditions")
original.all_conditions_relative_difference_with_average_per_condition <- original.all_conditions_relative_difference_with_average_per_condition + theme(legend.text=element_text(size=8))
original.all_conditions_relative_difference_with_average_per_condition_50_opacity <- original.all_conditions_relative_difference_with_average_per_condition + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.all_conditions_relative_difference_with_average_per_condition_transparent <- original.all_conditions_relative_difference_with_average_per_condition + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_all_conditions_relative_difference_and_average
evidence <- c(1,2,3,4,5,6,7)
LC_HA.ave <- c(mean(original.LC_HA$ev1reldiff)/50,mean(original.LC_HA$ev2reldiff)/mean(original.LC_HA$ev1), mean(original.LC_HA$ev3reldiff)/mean(original.LC_HA$ev2), mean(original.LC_HA$ev4reldiff)/mean(original.LC_HA$ev3), mean(original.LC_HA$ev5reldiff)/mean(original.LC_HA$ev4), mean(original.LC_HA$ev6reldiff)/mean(original.LC_HA$ev5), mean(original.LC_HA$ev7reldiff)/mean(original.LC_HA$ev6))
LC_LA.ave <- c(mean(original.LC_LA$ev1reldiff)/50,mean(original.LC_LA$ev2reldiff)/mean(original.LC_LA$ev1), mean(original.LC_LA$ev3reldiff)/mean(original.LC_LA$ev2), mean(original.LC_LA$ev4reldiff)/mean(original.LC_LA$ev3), mean(original.LC_LA$ev5reldiff)/mean(original.LC_LA$ev4), mean(original.LC_LA$ev6reldiff)/mean(original.LC_LA$ev5), mean(original.LC_LA$ev7reldiff)/mean(original.LC_LA$ev6))
HC_HA.ave <- c(mean(original.HC_HA$ev1reldiff)/100,mean(original.HC_HA$ev2reldiff)/mean(original.HC_HA$ev1), mean(original.HC_HA$ev3reldiff)/mean(original.HC_HA$ev2), mean(original.HC_HA$ev4reldiff)/mean(original.HC_HA$ev3), mean(original.HC_HA$ev5reldiff)/mean(original.HC_HA$ev4), mean(original.HC_HA$ev6reldiff)/mean(original.HC_HA$ev5), mean(original.HC_HA$ev7reldiff)/mean(original.HC_HA$ev6))
HC_LA.ave <- c(mean(original.HC_LA$ev1reldiff)/100,mean(original.HC_LA$ev2reldiff)/mean(original.HC_LA$ev1), mean(original.HC_LA$ev3reldiff)/mean(original.HC_LA$ev2), mean(original.HC_LA$ev4reldiff)/mean(original.HC_LA$ev3), mean(original.HC_LA$ev5reldiff)/mean(original.HC_LA$ev4), mean(original.HC_LA$ev6reldiff)/mean(original.HC_LA$ev5), mean(original.HC_LA$ev7reldiff)/mean(original.HC_LA$ev6))
chart <- data.frame(evidence, LC_HA.ave, LC_LA.ave, HC_HA.ave, HC_LA.ave)
original.all_conditions_relative_difference_and_average <- ggplot(chart, aes(x=evidence, y=LC_LA.ave, colour="Low Clarity and No Accountability")) + geom_line(size=1.3)
original.all_conditions_relative_difference_and_average <- original.all_conditions_relative_difference_and_average + geom_line(aes(y=LC_HA.ave, colour="Low Clarity and Accountability"),size=1.3)
original.all_conditions_relative_difference_and_average <- original.all_conditions_relative_difference_and_average + geom_line(aes(y=HC_LA.ave, colour="High Clarity and No Accountability"),size=1.3) 
original.all_conditions_relative_difference_and_average <- original.all_conditions_relative_difference_and_average + geom_line(aes(y=HC_HA.ave, colour="High Clarity and Accountability"),size=1.3)
original.all_conditions_relative_difference_and_average <- original.all_conditions_relative_difference_and_average + labs(x="Evidence Number")
original.all_conditions_relative_difference_and_average <- original.all_conditions_relative_difference_and_average + labs(y="Relative Change in Assigned Probability (average)")
original.all_conditions_relative_difference_and_average <- original.all_conditions_relative_difference_and_average + scale_color_manual(values=colors, name = "Conditions")
original.all_conditions_relative_difference_and_average <- original.all_conditions_relative_difference_and_average + theme(legend.text=element_text(size=8))
original.all_conditions_relative_difference_and_average_50_opacity <- original.all_conditions_relative_difference_and_average + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.all_conditions_relative_difference_and_average_transparent <- original.all_conditions_relative_difference_and_average + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_all_conditions_relative_difference_with_average
evidence <- c(1,2,3,4,5,6,7)
LC_HA.ave <- c(mean(original.LC_HA$ev1reldiff)/50,mean(original.LC_HA$ev2reldiff)/mean(original.LC_HA$ev1), mean(original.LC_HA$ev3reldiff)/mean(original.LC_HA$ev2), mean(original.LC_HA$ev4reldiff)/mean(original.LC_HA$ev3), mean(original.LC_HA$ev5reldiff)/mean(original.LC_HA$ev4), mean(original.LC_HA$ev6reldiff)/mean(original.LC_HA$ev5), mean(original.LC_HA$ev7reldiff)/mean(original.LC_HA$ev6))
LC_LA.ave <- c(mean(original.LC_LA$ev1reldiff)/50,mean(original.LC_LA$ev2reldiff)/mean(original.LC_LA$ev1), mean(original.LC_LA$ev3reldiff)/mean(original.LC_LA$ev2), mean(original.LC_LA$ev4reldiff)/mean(original.LC_LA$ev3), mean(original.LC_LA$ev5reldiff)/mean(original.LC_LA$ev4), mean(original.LC_LA$ev6reldiff)/mean(original.LC_LA$ev5), mean(original.LC_LA$ev7reldiff)/mean(original.LC_LA$ev6))
HC_HA.ave <- c(mean(original.HC_HA$ev1reldiff)/100,mean(original.HC_HA$ev2reldiff)/mean(original.HC_HA$ev1), mean(original.HC_HA$ev3reldiff)/mean(original.HC_HA$ev2), mean(original.HC_HA$ev4reldiff)/mean(original.HC_HA$ev3), mean(original.HC_HA$ev5reldiff)/mean(original.HC_HA$ev4), mean(original.HC_HA$ev6reldiff)/mean(original.HC_HA$ev5), mean(original.HC_HA$ev7reldiff)/mean(original.HC_HA$ev6))
HC_LA.ave <- c(mean(original.HC_LA$ev1reldiff)/100,mean(original.HC_LA$ev2reldiff)/mean(original.HC_LA$ev1), mean(original.HC_LA$ev3reldiff)/mean(original.HC_LA$ev2), mean(original.HC_LA$ev4reldiff)/mean(original.HC_LA$ev3), mean(original.HC_LA$ev5reldiff)/mean(original.HC_LA$ev4), mean(original.HC_LA$ev6reldiff)/mean(original.HC_LA$ev5), mean(original.HC_LA$ev7reldiff)/mean(original.HC_LA$ev6))
chart <- data.frame(evidence, LC_HA.ave, LC_LA.ave, HC_HA.ave, HC_LA.ave)
original.all_conditions_relative_difference_with_average <- ggplot(chart, aes(x=evidence, y=LC_LA.ave, colour="Low Clarity with No Accountability")) + geom_line(size=1.3)
original.all_conditions_relative_difference_with_average <- original.all_conditions_relative_difference_with_average + geom_line(aes(y=LC_HA.ave, colour="Low Clarity with Accountability"),size=1.3)
original.all_conditions_relative_difference_with_average <- original.all_conditions_relative_difference_with_average + geom_line(aes(y=HC_LA.ave, colour="High Clarity with No Accountability"),size=1.3) 
original.all_conditions_relative_difference_with_average <- original.all_conditions_relative_difference_with_average + geom_line(aes(y=HC_HA.ave, colour="High Clarity with Accountability"),size=1.3)
original.all_conditions_relative_difference_with_average <- original.all_conditions_relative_difference_with_average + labs(x="Evidence Number")
original.all_conditions_relative_difference_with_average <- original.all_conditions_relative_difference_with_average + labs(y="Relative Change in Assigned Probability (average)")
original.all_conditions_relative_difference_with_average <- original.all_conditions_relative_difference_with_average + scale_color_manual(values=colors, name = "Conditions")
original.all_conditions_relative_difference_with_average <- original.all_conditions_relative_difference_with_average + theme(legend.text=element_text(size=8))
original.all_conditions_relative_difference_with_average_50_opacity <- original.all_conditions_relative_difference_with_average + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.all_conditions_relative_difference_with_average_transparent <- original.all_conditions_relative_difference_with_average + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_all_conditions_absolute_accuracy_and_average
evidence <- c(1,2,3,4,5,6,7)
LC_HA.ave <- c(mean(original.LC_HA$ev1abs_acc),mean(original.LC_HA$ev2abs_acc), mean(original.LC_HA$ev3abs_acc), mean(original.LC_HA$ev4abs_acc), mean(original.LC_HA$ev5abs_acc), mean(original.LC_HA$ev6abs_acc), mean(original.LC_HA$ev7abs_acc))
LC_LA.ave <- c(mean(original.LC_LA$ev1abs_acc),mean(original.LC_LA$ev2abs_acc), mean(original.LC_LA$ev3abs_acc), mean(original.LC_LA$ev4abs_acc), mean(original.LC_LA$ev5abs_acc), mean(original.LC_LA$ev6abs_acc), mean(original.LC_LA$ev7abs_acc))
HC_HA.ave <- c(mean(original.HC_HA$ev1abs_acc),mean(original.HC_HA$ev2abs_acc), mean(original.HC_HA$ev3abs_acc), mean(original.HC_HA$ev4abs_acc), mean(original.HC_HA$ev5abs_acc), mean(original.HC_HA$ev6abs_acc), mean(original.HC_HA$ev7abs_acc))
HC_LA.ave <- c(mean(original.HC_LA$ev1abs_acc),mean(original.HC_LA$ev2abs_acc), mean(original.HC_LA$ev3abs_acc), mean(original.HC_LA$ev4abs_acc), mean(original.HC_LA$ev5abs_acc), mean(original.HC_LA$ev6abs_acc), mean(original.HC_LA$ev7abs_acc))
chart <- data.frame(evidence, LC_HA.ave, LC_LA.ave, HC_HA.ave, HC_LA.ave)
original.all_conditions_absolute_accuracy_and_average <- ggplot(chart, aes(x=evidence, y=LC_LA.ave, colour="Low Clarity and No Accountability")) + geom_line(size=1.3)
original.all_conditions_absolute_accuracy_and_average <- original.all_conditions_absolute_accuracy_and_average + geom_line(aes(y=LC_HA.ave, colour="Low Clarity and Accountability"), size=1.3)
original.all_conditions_absolute_accuracy_and_average <- original.all_conditions_absolute_accuracy_and_average + geom_line(aes(y=HC_LA.ave, colour="High Clarity and No Accountability"), size=1.3) 
original.all_conditions_absolute_accuracy_and_average <- original.all_conditions_absolute_accuracy_and_average + geom_line(aes(y=HC_HA.ave, colour="High Clarity and Accountability"), size=1.3)
original.all_conditions_absolute_accuracy_and_average <- original.all_conditions_absolute_accuracy_and_average + labs(x="Evidence Number")
original.all_conditions_absolute_accuracy_and_average <- original.all_conditions_absolute_accuracy_and_average + labs(y="Proximity to Accurate Probability Assignment (average)")
original.all_conditions_absolute_accuracy_and_average <- original.all_conditions_absolute_accuracy_and_average + scale_color_manual(values=colors, name = "Conditions")
original.all_conditions_absolute_accuracy_and_average <- original.all_conditions_absolute_accuracy_and_average + theme(legend.text=element_text(size=8))
original.all_conditions_absolute_accuracy_and_average_50_opacity <- original.all_conditions_absolute_accuracy_and_average + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.all_conditions_absolute_accuracy_and_average_transparent <- original.all_conditions_absolute_accuracy_and_average + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_all_conditions_absolute_accuracy_with_average
evidence <- c(1,2,3,4,5,6,7)
LC_HA.ave <- c(mean(original.LC_HA$ev1abs_acc),mean(original.LC_HA$ev2abs_acc), mean(original.LC_HA$ev3abs_acc), mean(original.LC_HA$ev4abs_acc), mean(original.LC_HA$ev5abs_acc), mean(original.LC_HA$ev6abs_acc), mean(original.LC_HA$ev7abs_acc))
LC_LA.ave <- c(mean(original.LC_LA$ev1abs_acc),mean(original.LC_LA$ev2abs_acc), mean(original.LC_LA$ev3abs_acc), mean(original.LC_LA$ev4abs_acc), mean(original.LC_LA$ev5abs_acc), mean(original.LC_LA$ev6abs_acc), mean(original.LC_LA$ev7abs_acc))
HC_HA.ave <- c(mean(original.HC_HA$ev1abs_acc),mean(original.HC_HA$ev2abs_acc), mean(original.HC_HA$ev3abs_acc), mean(original.HC_HA$ev4abs_acc), mean(original.HC_HA$ev5abs_acc), mean(original.HC_HA$ev6abs_acc), mean(original.HC_HA$ev7abs_acc))
HC_LA.ave <- c(mean(original.HC_LA$ev1abs_acc),mean(original.HC_LA$ev2abs_acc), mean(original.HC_LA$ev3abs_acc), mean(original.HC_LA$ev4abs_acc), mean(original.HC_LA$ev5abs_acc), mean(original.HC_LA$ev6abs_acc), mean(original.HC_LA$ev7abs_acc))
chart <- data.frame(evidence, LC_HA.ave, LC_LA.ave, HC_HA.ave, HC_LA.ave)
original.all_conditions_absolute_accuracy_with_average <- ggplot(chart, aes(x=evidence, y=LC_LA.ave, colour="Low Clarity with No Accountability")) + geom_line(size=1.3)
original.all_conditions_absolute_accuracy_with_average <- original.all_conditions_absolute_accuracy_with_average + geom_line(aes(y=LC_HA.ave, colour="Low Clarity with Accountability"), size=1.3)
original.all_conditions_absolute_accuracy_with_average <- original.all_conditions_absolute_accuracy_with_average + geom_line(aes(y=HC_LA.ave, colour="High Clarity with No Accountability"), size=1.3) 
original.all_conditions_absolute_accuracy_with_average <- original.all_conditions_absolute_accuracy_with_average + geom_line(aes(y=HC_HA.ave, colour="High Clarity with Accountability"), size=1.3)
original.all_conditions_absolute_accuracy_with_average <- original.all_conditions_absolute_accuracy_with_average + labs(x="Evidence Number")
original.all_conditions_absolute_accuracy_with_average <- original.all_conditions_absolute_accuracy_with_average + labs(y="Proximity to Accurate Probability Assignment (average)")
original.all_conditions_absolute_accuracy_with_average <- original.all_conditions_absolute_accuracy_with_average + scale_color_manual(values=colors, name = "Conditions")
original.all_conditions_absolute_accuracy_with_average <- original.all_conditions_absolute_accuracy_with_average + theme(legend.text=element_text(size=8))
original.all_conditions_absolute_accuracy_with_average_50_opacity <- original.all_conditions_absolute_accuracy_with_average + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.all_conditions_absolute_accuracy_with_average_transparent <- original.all_conditions_absolute_accuracy_with_average + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_all_conditions_absolute_accuracy_and_average_per_condition
evidence <- c(1,2,3,4,5,6,7)
LC_HA.ave <- c(mean(original.LC_HA$ev1abs_acc),mean(original.LC_HA$ev2abs_acc), mean(original.LC_HA$ev3abs_acc), mean(original.LC_HA$ev4abs_acc), mean(original.LC_HA$ev5abs_acc), mean(original.LC_HA$ev6abs_acc), mean(original.LC_HA$ev7abs_acc))
LC_LA.ave <- c(mean(original.LC_LA$ev1abs_acc),mean(original.LC_LA$ev2abs_acc), mean(original.LC_LA$ev3abs_acc), mean(original.LC_LA$ev4abs_acc), mean(original.LC_LA$ev5abs_acc), mean(original.LC_LA$ev6abs_acc), mean(original.LC_LA$ev7abs_acc))
HC_HA.ave <- c(mean(original.HC_HA$ev1abs_acc),mean(original.HC_HA$ev2abs_acc), mean(original.HC_HA$ev3abs_acc), mean(original.HC_HA$ev4abs_acc), mean(original.HC_HA$ev5abs_acc), mean(original.HC_HA$ev6abs_acc), mean(original.HC_HA$ev7abs_acc))
HC_LA.ave <- c(mean(original.HC_LA$ev1abs_acc),mean(original.HC_LA$ev2abs_acc), mean(original.HC_LA$ev3abs_acc), mean(original.HC_LA$ev4abs_acc), mean(original.HC_LA$ev5abs_acc), mean(original.HC_LA$ev6abs_acc), mean(original.HC_LA$ev7abs_acc))
chart <- data.frame(evidence, LC_HA.ave, LC_LA.ave, HC_HA.ave, HC_LA.ave)
original.all_conditions_absolute_accuracy_and_average_per_condition <- ggplot(chart, aes(x=evidence, y=LC_LA.ave, colour="Low Clarity and No Accountability")) + geom_line(size=1.3)
original.all_conditions_absolute_accuracy_and_average_per_condition <- original.all_conditions_absolute_accuracy_and_average_per_condition + geom_line(aes(y=LC_HA.ave, colour="Low Clarity and Accountability"), size=1.3)
original.all_conditions_absolute_accuracy_and_average_per_condition <- original.all_conditions_absolute_accuracy_and_average_per_condition + geom_line(aes(y=HC_LA.ave, colour="High Clarity and No Accountability"), size=1.3) 
original.all_conditions_absolute_accuracy_and_average_per_condition <- original.all_conditions_absolute_accuracy_and_average_per_condition + geom_line(aes(y=HC_HA.ave, colour="High Clarity and Accountability"), size=1.3)
original.all_conditions_absolute_accuracy_and_average_per_condition <- original.all_conditions_absolute_accuracy_and_average_per_condition + labs(x="Evidence Number")
original.all_conditions_absolute_accuracy_and_average_per_condition <- original.all_conditions_absolute_accuracy_and_average_per_condition + labs(y="Proximity to Accurate Probability Assignment (average per condition)")
original.all_conditions_absolute_accuracy_and_average_per_condition <- original.all_conditions_absolute_accuracy_and_average_per_condition + scale_color_manual(values=colors, name = "Conditions")
original.all_conditions_absolute_accuracy_and_average_per_condition <- original.all_conditions_absolute_accuracy_and_average_per_condition + theme(legend.text=element_text(size=8))
original.all_conditions_absolute_accuracy_and_average_per_condition_50_opacity <- original.all_conditions_absolute_accuracy_and_average_per_condition + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.all_conditions_absolute_accuracy_and_average_per_condition_transparent <- original.all_conditions_absolute_accuracy_and_average_per_condition + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))

#original_all_conditions_absolute_accuracy_with_average_per_condition
evidence <- c(1,2,3,4,5,6,7)
LC_HA.ave <- c(mean(original.LC_HA$ev1abs_acc),mean(original.LC_HA$ev2abs_acc), mean(original.LC_HA$ev3abs_acc), mean(original.LC_HA$ev4abs_acc), mean(original.LC_HA$ev5abs_acc), mean(original.LC_HA$ev6abs_acc), mean(original.LC_HA$ev7abs_acc))
LC_LA.ave <- c(mean(original.LC_LA$ev1abs_acc),mean(original.LC_LA$ev2abs_acc), mean(original.LC_LA$ev3abs_acc), mean(original.LC_LA$ev4abs_acc), mean(original.LC_LA$ev5abs_acc), mean(original.LC_LA$ev6abs_acc), mean(original.LC_LA$ev7abs_acc))
HC_HA.ave <- c(mean(original.HC_HA$ev1abs_acc),mean(original.HC_HA$ev2abs_acc), mean(original.HC_HA$ev3abs_acc), mean(original.HC_HA$ev4abs_acc), mean(original.HC_HA$ev5abs_acc), mean(original.HC_HA$ev6abs_acc), mean(original.HC_HA$ev7abs_acc))
HC_LA.ave <- c(mean(original.HC_LA$ev1abs_acc),mean(original.HC_LA$ev2abs_acc), mean(original.HC_LA$ev3abs_acc), mean(original.HC_LA$ev4abs_acc), mean(original.HC_LA$ev5abs_acc), mean(original.HC_LA$ev6abs_acc), mean(original.HC_LA$ev7abs_acc))
chart <- data.frame(evidence, LC_HA.ave, LC_LA.ave, HC_HA.ave, HC_LA.ave)
original.all_conditions_absolute_accuracy_with_average_per_condition <- ggplot(chart, aes(x=evidence, y=LC_LA.ave, colour="Low Clarity with No Accountability")) + geom_line(size=1.3)
original.all_conditions_absolute_accuracy_with_average_per_condition <- original.all_conditions_absolute_accuracy_with_average_per_condition + geom_line(aes(y=LC_HA.ave, colour="Low Clarity with Accountability"), size=1.3)
original.all_conditions_absolute_accuracy_with_average_per_condition <- original.all_conditions_absolute_accuracy_with_average_per_condition + geom_line(aes(y=HC_LA.ave, colour="High Clarity with No Accountability"), size=1.3) 
original.all_conditions_absolute_accuracy_with_average_per_condition <- original.all_conditions_absolute_accuracy_with_average_per_condition + geom_line(aes(y=HC_HA.ave, colour="High Clarity with Accountability"), size=1.3)
original.all_conditions_absolute_accuracy_with_average_per_condition <- original.all_conditions_absolute_accuracy_with_average_per_condition + labs(x="Evidence Number")
original.all_conditions_absolute_accuracy_with_average_per_condition <- original.all_conditions_absolute_accuracy_with_average_per_condition + labs(y="Proximity to Accurate Probability Assignment (average per condition)")
original.all_conditions_absolute_accuracy_with_average_per_condition <- original.all_conditions_absolute_accuracy_with_average_per_condition + scale_color_manual(values=colors, name = "Conditions")
original.all_conditions_absolute_accuracy_with_average_per_condition <- original.all_conditions_absolute_accuracy_with_average_per_condition + theme(legend.text=element_text(size=8))
original.all_conditions_absolute_accuracy_with_average_per_condition_50_opacity <- original.all_conditions_absolute_accuracy_with_average_per_condition + theme(panel.background = element_rect(fill =alpha("#EBEBEB", 0.5)))
original.all_conditions_absolute_accuracy_with_average_per_condition_transparent <- original.all_conditions_absolute_accuracy_with_average_per_condition + theme(panel.border = element_rect(fill = "transparent", colour = "#000000"), panel.background = element_rect(fill = "transparent", colour = NA), legend.key = element_rect(fill = "transparent", colour = NA))



#------------------------------------------------------------------------
#T Tests for each piece of evidence

#Summary of original
summary.original<-ddply(original, c("clarity","account"),summarise, N=length(userid), mean=mean(totdeviation), sd=sd(totdeviation), se=sd/sqrt(N))
summary.original
 # clarity account  N     mean       sd       se
 #       0       0 63 13.09524 10.99665 1.385447
 #       0       1 60 16.48333 14.58870 1.883393
 #       1       0 66 14.51515 11.29499 1.390317
 #       1       1 57 13.52632 10.40107 1.377656

#T Tests
t_test.original.clarity.avedev <- t.test(original.HC$avedev,original.LC$avedev)
t_test.original.clarity.avedev
# Welch Two Sample t - test
# 
# data:original.HC$avedev and original.LC$avedev
# t = -0.3058, df = 243.4, p - value = 0.76
# alternative hypothesis:true difference in means is not equal to 0
# 95 percent confidence interval:-1.2865181  0.9407408
# sample estimates:mean of x mean of y
# 8.818318  8.991206 

t_test.original.clarity.aveabs_acc <- t.test(original.HC$aveabs_acc,original.LC$aveabs_acc)
t_test.original.clarity.aveabs_acc
# Welch Two Sample t-test
# 
# data:  original.HC$aveabs_acc and original.LC$aveabs_acc
# t = -0.089244, df = 244, p-value = 0.929
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.081539  2.814408
# sample estimates:
#   mean of x mean of y 
# 51.55865  51.69222 

t_test.original.clarity.avereldiff <- t.test(original.HA$avereldiff,original.LA$avereldiff)
t_test.original.clarity.avereldiff
# Welch Two Sample t-test
# 
# data:  original.HA$avereldiff and original.LA$avereldiff
# t = 0.43392, df = 243.99, p-value = 0.6647
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.299697  2.034114
# sample estimates:
#   mean of x mean of y 
# 11.44309  11.07588 

t_test.original.accountability.avedev <- t.test(original.HA$avedev,original.LA$avedev)
t_test.original.accountability.avedev
# Welch Two Sample t-test
# 
# data:  original.HA$avedev and original.LA$avedev
# t = -0.3058, df = 243.4, p-value = 0.76
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.2865181  0.9407408
# sample estimates:
#   mean of x mean of y 
# 8.818318  8.991206 

t_test.original.accountability.aveabs_acc <- t.test(original.HA$aveabs_acc,original.LA$aveabs_acc)
t_test.original.accountability.aveabs_acc
# Welch Two Sample t-test
# 
# data:  original.HA$aveabs_acc and original.LA$aveabs_acc
# t = -0.089244, df = 244, p-value = 0.929
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.081539  2.814408
# sample estimates:
#   mean of x mean of y 
# 51.55865  51.69222 

t_test.original.accountability.avereldiff <- t.test(original.HA$avereldiff,original.LA$avereldiff)
t_test.original.accountability.avereldiff
# Welch Two Sample t-test
# 
# data:  original.HA$avereldiff and original.LA$avereldiff
# t = 7.8919, df = 243.55, p-value = 1.009e-13
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   4.507685 7.506253
# sample estimates:
#   mean of x mean of y 
# 16.70035  10.69338 



#------------------------------------------------------------------------
#ANOVAs
original.avedev.anova <- aov(original$avedev ~ original$clarity + original$account)
summary(original.avedev.anova)
# Df Sum Sq Mean Sq F value Pr(>F)
# original$clarity   1      2    1.84   0.094  0.760
# original$account   1     37   36.76   1.877  0.172
# Residuals        243   4760   19.59  

original.avereldiff.anova <- aov(original$avereldiff ~ original$clarity + original$account)
summary(original.avereldiff.anova)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# original$clarity   1   2219  2219.1  62.216 1.04e-13 ***
#   original$account   1     26    26.3   0.739    0.391    
# Residuals        243   8667    35.7                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

original.aveabs_acc.anova <- aov(original$aveabs_acc ~ original$clarity + original$account)
summary(original.aveabs_acc.anova)
# Df Sum Sq Mean Sq F value Pr(>F)
# original$clarity   1      1    1.10   0.008  0.929
# original$account   1     66   65.72   0.476  0.491
# Residuals        243  33546  138.05   

original.avedev.anova.with_interaction <- aov(original$avedev ~ original$clarity + original$account + original$clarity * original$account)
summary(original.avedev.anova.with_interaction)
# Df Sum Sq Mean Sq F value  Pr(>F)   
# original$clarity                    1      2    1.84   0.097 0.75632   
# original$account                    1     37   36.76   1.930 0.16604   
# original$clarity:original$account   1    151  150.55   7.904 0.00534 **
#   Residuals                         242   4609   19.05                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

original.absreldiff.anova.with_interaction <- aov(original$avereldiff ~ original$clarity + original$account + original$clarity * original$account)
summary(original.absreldiff.anova.with_interaction)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# original$clarity                    1   2219  2219.1  62.955 7.85e-14 ***
#   original$account                    1     26    26.3   0.747   0.3882    
# original$clarity:original$account   1    137   137.0   3.886   0.0498 *  
#   Residuals                         242   8530    35.2                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

original.aveabs_acc.anova.with_interaction <- aov(original$aveabs_acc ~ original$clarity + original$account + original$clarity * original$account)
summary(original.aveabs_acc.anova.with_interaction)
# Df Sum Sq Mean Sq F value Pr(>F)
# original$clarity                    1      1    1.10   0.008  0.929
# original$account                    1     66   65.72   0.475  0.491
# original$clarity:original$account   1     49   48.85   0.353  0.553
# Residuals                         242  33498  138.42  
