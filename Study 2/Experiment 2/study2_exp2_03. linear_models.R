options(scipen=999)

data <- Differential_perception_of_probabilities_observer_Sona
rm(Differential_perception_of_probabilities_observer_Sona)

#Conversion from int to factor 
data$low_high <- as.factor(data$low_high)
data$verb_num <- as.factor(data$verb_num)
data$neg_pos <- as.factor(data$neg_pos)
data$noact_act <- as.factor(data$noact_act)
data$act_pos <- as.factor(data$act_pos)
data$act_neg <- as.factor(data$act_neg)
data$noact_neg <- as.factor(data$noact_neg)
data$noact_pos <- as.factor(data$noact_pos)
data$outcome <- as.factor(data$outcome)
data$condition_1 <- as.factor(data$condition_1)
data$condition_2 <- as.factor(data$condition_2)

#Regressions
#R1
model.justification2.noact_act.outome <- lm(justification_2 ~ 
                                              noact_act + 
                                              outcome + 
                                              noact_act*outcome, data=data) #tests if the interaction between (whether the policymaker took action or not and if his decision was an error[given the outcome]) had an impact on justification_2 
summary(model.justification2.noact_act.outome) #s

#R2
model.diff.condition1.condition2 <- lm(justification_diff ~ 
                                         condition_1 + 
                                         condition_2, data=data) #tests if (being placed in any condition) had an impact on justification_diff 
summary (model.diff.condition1.condition2) #s

#R3
model.justification2.condition1.condition2 <- lm(justification_2 ~ 
                                                   condition_1 + 
                                                   condition_2, data=data) #tests if (being placed in any condition) had an impact on justification_2
summary(model.diff.condition1.condition2) #s

#R4
model.diff.low_high.verb_num.noact_act.outcome <- lm(justification_diff ~ 
                                                       low_high +
                                                       verb_num + 
                                                       noact_act + 
                                                       outcome, data=data) #tests if (the magnitude of probability stated, the mode in which the probability was stated, whether the policymaker acted, and if the policymaker's final choice was a good decision) had an impact on justifucation_diff 
summary(model.diff.low_high.verb_num.noact_act.outcome) #s

#R5
model.justification2.low_high.verb_num.noact_act.outcome <- lm(justification_2 ~ 
                                                                 low_high + 
                                                                 verb_num + 
                                                                 noact_act + 
                                                                 outcome, data=data) #tests if (the magnitude of probability stated, the mode in which the probability was stated, whether the policymaker acted, and if the policymaker's final choice was a good decision) had an impact on justifucation_2 
summary(model.justification2.low_high.verb_num.noact_act.outcome) #s

#R6
model.justification1.low_high.verb_num <- lm(justification_1 ~ 
                                               low_high + 
                                               verb_num + 
                                               low_high*verb_num, data=data) #tests if (the interaction between the stated probability level and whether that was given verbally or numerically) had an impact on justification_1
summary(model.justification1.low_high.verb_num) #s

#R7
model.justification2.low_high.verb_num.noact_act.outcome.interaction <- lm(justification_2 ~ 
                                                                             low_high + 
                                                                             verb_num + 
                                                                             noact_act + 
                                                                             outcome +
                                                                             noact_act*outcome, data=data)  #tests if (the magnitude of probability stated, the mode in which the probability was stated, whether the policymaker acted, if the policymaker's final choice was a good decision, and the interaction between wether action was taken and whether this resulted in a good decision) had an impact on justifucation_2 
summary(model.justification2.low_high.verb_num.noact_act.outcome.interaction) #s

#-------------------------------------------------------------------
#Summaries
library(plyr)
just1.summary <- ddply(data, 
                       c("low_high","verb_num"),
                       summarise, 
                       N=length(userid), 
                       mean=mean(justification_1, na.rm = TRUE), 
                       sd=sd(justification_1, na.rm = TRUE), 
                       se=sd/sqrt(N))
just1.summary 
# low_high verb_num  N     mean        sd        se
# 1        0        0 30 4.000000 1.7113069 0.3124405
# 2        0        1 37 5.108108 1.3495634 0.2218669
# 3        1        0 33 5.575758 0.8671182 0.1509459
# 4        1        1 29 5.793103 0.8185052 0.1519926
# 5     <NA>     <NA> 12       NA        NA        NA

just.2.summary <- ddply(data, c("neg_pos","noact_act"),summarise, N=length(userid), mean=mean(justification_2), sd=sd(justification_2), se=sd/sqrt(N))
just.2.summary 
# neg_pos noact_act  N     mean       sd        se
# 1       0         0 36 3.750000 1.696635 0.2827726
# 2       0         1 33 5.121212 1.139012 0.1982766
# 3       1         0 29 5.172414 1.283660 0.2383697
# 4       1         1 31 5.967742 1.016001 0.1824792
# 5    <NA>      <NA> 12       NA       NA        NA

#-------------------------------------------------------------------
#Descruiption of Dependent Variables
library(psych)
describe(data$justification_1)
# vars   n mean   sd median trimmed  mad min max range  skew kurtosis   se
# 1    1 128 5.13 1.39      5    5.28 1.48   1   7     6 -1.03     0.44 0.12

describe(data$justification_2)
# vars   n mean   sd median trimmed  mad min max range  skew kurtosis   se
# 1    1 129 4.95 1.55      5    5.09 1.48   1   7     6 -0.82    -0.16 0.14

describe(data$justification_diff)
# vars   n  mean   sd median trimmed  mad min max range  skew kurtosis   se
# 1    1 141 -0.13 2.61      0   -0.12 1.48  -7   7    14 -0.01     0.84 0.22

describe(data$justification_abs_diff)
# vars   n mean   sd median trimmed  mad min max range skew kurtosis   se
# 1    1 141 1.76 1.92      1    1.45 1.48   0   7     7 1.14     0.18 0.16

t.test(data$justification_1, data$justification_2)
# Welch Two Sample t-test
# 
# data:  data$justification_1 and data$justification_2
# t = 0.9769, df = 252.714, p-value = 0.3295
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.1821745  0.5408228
# sample estimates:
#   mean of x mean of y 
# 5.132812  4.953488 

#-------
#Ploting Regression 6
colors = c('darkblue', 'darkred', 'grey','black')

regression_6_summary <- ddply(na.omit(data), c("low_high","verb_num"),summarise, #na.omit(data) used to omit NAs w/o having to create a new variable 
                                        N=length(userid), 
                                        mean=mean(justification_1), 
                                        sd=sd(justification_1), 
                                        se=sd/sqrt(N))

reg_6.verb <- regression_6_summary[which(regression_6_summary$verb_num == 0),]
reg_6.num <- regression_6_summary[which(regression_6_summary$verb_num == 1),]

plot <- ggplot(data = regression_6_summary, aes(x=low_high, y=mean)) + 
		  geom_line(data=reg_6.verb, aes(group = verb_num, colour="Verbal probability"), size=1.3) +
		  geom_line(data=reg_6.num, aes(group = verb_num, colour="Numerical probability"), size=1.3) +
		  geom_point(size=4)+ geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2) +
		  labs(x="Probability of Event Ocurring") +
		  labs(y="Level of felt Justification for the President taking action") +
      ggtitle("Observer") +
		  theme(legend.text=element_text(size=8)) +
		  scale_x_discrete(breaks=c("0", "1"), labels=c("Low", "High"))

plot <- plot + scale_color_manual(values=colors, name="Conditions")
plot

#-------------
#combo regressions from reg 4

low_high_and_verb_num.on.justification_dif <- lm(justification_diff ~ 
                                                       low_high * verb_num, data=data)
summary(low_high_and_verb_num.on.justification_dif) #s

low_high_and_noact_act.on.justification_dif <- lm(justification_diff ~ 
                                                   low_high * noact_act, data=data)
summary(low_high_and_noact_act.on.justification_dif) #s

low_high_and_outcome.on.justification_dif <- lm(justification_diff ~ 
                                                    low_high * outcome, data=data)
summary(low_high_and_outcome.on.justification_dif) #s

verb_num_and_noact_act.on.justification_dif <- lm(justification_diff ~ 
                                                  verb_num * noact_act, data=data)
summary(verb_num_and_noact_act.on.justification_dif) #s

verb_num_and_outcome.on.justification_dif <- lm(justification_diff ~ 
                                                    verb_num * outcome, data=data)
summary(verb_num_and_outcome.on.justification_dif) #ns

noact_act_and_outcome.on.justification_dif <- lm(justification_diff ~ 
                                                  noact_act * outcome, data=data)
summary(noact_act_and_outcome.on.justification_dif) #s

summary(lm(justification_diff ~ 
             low_high * verb_num * noact_act, data=data)) #s

summary(lm(justification_diff ~ 
             low_high * verb_num * outcome, data=data)) #s

summary(lm(justification_diff ~ 
             low_high * noact_act * outcome, data=data)) #s

summary(lm(justification_diff ~ 
             noact_act * verb_num * outcome, data=data)) #s
