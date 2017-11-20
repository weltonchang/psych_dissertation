#### Study 2a: Policymakers
#### updated 6/22/2017

options(scipen=666)

library(plyr)

data <- Actors
rm(Actors)

#Factorize
data$notaccountable_accountable <- as.factor(data$notaccountable_accountable)
data$noground_ground <- as.factor(data$notaccountable_accountable)
data$low_high <- as.factor(data$low_high)
data$num_verb <- as.factor(data$num_verb)
data$noground_ground <- as.factor(data$noground_ground)
data$act_pos <- as.factor(data$act_pos)
data$act_neg <- as.factor(data$act_neg)
data$inact_pos <- as.factor(data$inact_pos)
data$inact_neg <- as.factor(data$inact_neg)
data$educ <- as.factor(data$educ)
data$age <- as.factor(data$age)
data$gender <- as.factor(data$gender)
data$race <- as.factor(data$race)
data$gov.exper <- as.factor(data$gov.exper)
data$policy <- as.factor(data$policy)
data$outcome <- as.factor(data$outcome)   
data$orient <- as.factor(data$orient)

# Demographics

ddply(data, c("educ"), summarise,
      N = length(userid) )

ddply(data, c("age"), summarise,
      N = length(userid) )

ddply(data, c("gov.exper"), summarise,
      N = length(userid) )

ddply(data, c("policy"), summarise,
      N = length(userid) )

(58+15)/113 # 65% have Masters or Professional Degrees
74/113 # 65% served in government for more than 1 year but less than 10 
65/113 # 57% were between the ages of 30 and 40
33/113 # 30% served in a policy role

# Descriptive Stats for conditions

ddply(data, c("low_high", "num_verb"), summarise,
      N = length(userid) )

ddply(data, c("low_high"), summarise,
      N = length(userid) )

ddply(data, c("low_high","act_pos", "act_neg", "inact_pos", "inact_neg"), summarise,
      N = length(userid) )

ddply(data, c("num_verb","act_pos", "act_neg", "inact_pos", "inact_neg"), summarise,
      N = length(userid) )


#Single predictor 
summary(aov(justification ~ low_high, data=data)) #ns
summary(aov(justification ~ notaccountable_accountable, data=data)) #ns
summary(aov(justification ~ num_verb, data=data)) #ns
summary(aov(justification ~ outcome, data=data)) #s
summary(aov(noground_ground ~ age, data=data)) #ns
summary(aov(noground_ground ~ agree, data=data))  #ns
summary(aov(noground_ground ~ aomt, data=data)) #ns
summary(aov(noground_ground ~ consc, data=data)) #ns
summary(aov(noground_ground ~ crt, data=data)) #ns
summary(aov(noground_ground ~ educ, data=data)) #ns
summary(aov(noground_ground ~ extra, data=data))  #ns
summary(aov(noground_ground ~ gender, data=data)) #ns
summary(aov(noground_ground ~ gov.exper, data=data)) #ns
summary(aov(noground_ground ~ low_high, data=data)) #ns


# New Analysis

## First choice
ddply(data, c("num_verb"), summarise,
      N = length(userid))

54/113 # numeric
59/113 # verbal

### Does high probability influence decision to choose numeric over verbal?
data$num_verb <- as.numeric(data$num_verb)
prob_pref <- lm(num_verb~low_high, data=data)
summary(prob_pref)

# significance check
t.test(num_verb~low_high, data=data)

# Create table to show who chose which type of probability

ddply(data, c("num_verb", "low_high"), summarise,
      N = length(userid))


### Does low or high probability influence decision to ground?
data$noground_ground <- as.numeric(data$noground_ground)
decision1 <- lm(noground_ground~low_high, data=data)
summary(decision1) # yes, p < 0.03 B = 0.21



###################

summary(aov(noground_ground ~ need.cog, data=data)) #ns
summary(aov(noground_ground ~ neuro, data=data))  #ns
summary(aov(noground_ground ~ notaccountable_accountable, data=data)) #ns
summary(aov(noground_ground ~ open, data=data)) #ns
summary(aov(noground_ground ~ orient, data=data)) #ns
summary(aov(noground_ground ~ orient.2, data=data)) #ns
summary(aov(noground_ground ~ policy, data=data)) #ns almost significant
summary(aov(noground_ground ~ race, data=data)) #ns
summary(aov(num_verb ~ age, data=data)) #ns
summary(aov(num_verb ~ agree, data=data)) #ns
summary(aov(num_verb ~ aomt, data=data)) #ns
summary(aov(num_verb ~ consc, data=data)) #ns
summary(aov(num_verb ~ crt, data=data)) #s
summary(aov(num_verb ~ educ, data=data)) #ns
summary(aov(num_verb ~ extra, data=data)) #ns
summary(aov(num_verb ~ gender, data=data)) #ns
summary(aov(num_verb ~ gov.exper, data=data)) #ns

summary(aov(num_verb ~ low_high, data=data)) #s
summary(aov(num_verb ~ low_high, data=data)) #s
summary(aov(num_verb ~ need.cog, data=data)) #as
summary(aov(num_verb ~ neuro, data=data)) #ns
summary(aov(num_verb ~ notaccountable_accountable, data=data)) #ns
summary(aov(num_verb ~ open, data=data)) #ns
summary(aov(num_verb ~ orient, data=data)) #ns
summary(aov(num_verb ~ orient.2, data=data)) #ns
summary(aov(num_verb ~ policy, data=data)) #ns
summary(aov(num_verb ~ race, data=data)) #ns

#Two predictors 
summary(aov(justification ~ low_high + num_verb, data=data)) #ns
summary(aov(justification ~ low_high + outcome, data=data)) #s
summary(aov(justification ~ notaccountable_accountable + outcome, data=data)) #ns
summary(aov(justification ~ notaccountable_accountable + low_high, data=data)) #ns
summary(aov(justification ~ notaccountable_accountable + num_verb, data=data)) #ns
summary(aov(justification ~ num_verb + outcome, data=data)) #s
summary(aov(noground_ground ~ crt + notaccountable_accountable, data=data)) #ns
summary(aov(noground_ground ~ low_high + crt, data=data)) #ns
summary(aov(noground_ground ~ low_high + need.cog, data=data)) #ns
summary(aov(noground_ground ~ need.cog + crt, data=data)) #ns
summary(aov(num_verb ~ crt + notaccountable_accountable, data=data)) #s (crt becomes worse predictor than single predictor test)
summary(aov(num_verb ~ low_high + crt, data=data)) #s (crt becomes better predictor than single predictor test)
summary(aov(num_verb ~ low_high + need.cog, data=data)) #s (need.cog becomes better predictor than single predictor test)
summary(aov(num_verb ~ need.cog + crt, data=data)) #s (crt becomes worse predictor than single predictor test)

summary(aov(justification ~ low_high * num_verb, data=data)) #ns
summary(aov(justification ~ low_high * outcome, data=data)) #s
summary(aov(justification ~ notaccountable_accountable * low_high, data=data)) #ns
summary(aov(justification ~ notaccountable_accountable * num_verb, data=data)) #ns
summary(aov(justification ~ notaccountable_accountable * outcome, data=data)) #s
summary(aov(justification ~ num_verb * outcome, data=data)) #s
summary(aov(noground_ground ~ crt * aomt, data=data)) #ns
summary(aov(noground_ground ~ crt * gov.exper, data=data)) #ns
summary(aov(noground_ground ~ crt * num_verb, data=data)) #ns
summary(aov(noground_ground ~ low_high * aomt, data=data)) #ns
summary(aov(noground_ground ~ low_high * crt, data=data)) #ns
summary(aov(noground_ground ~ low_high * gov.exper, data=data)) #ns
summary(aov(noground_ground ~ low_high * need.cog, data=data)) #ns
summary(aov(noground_ground ~ low_high * num_verb, data=data)) #as
summary(aov(noground_ground ~ need.cog * aomt, data=data)) #ns
summary(aov(noground_ground ~ need.cog * crt, data=data)) #ns
summary(aov(noground_ground ~ need.cog * gov.exper, data=data)) #ns
summary(aov(noground_ground ~ need.cog * num_verb, data=data)) #ns
summary(aov(num_verb ~ crt * aomt, data=data)) #s
summary(aov(num_verb ~ crt * gov.exper, data=data)) #s
summary(aov(num_verb ~ crt * noground_ground, data=data)) #s
summary(aov(num_verb ~ low_high * aomt, data=data)) #s
summary(aov(num_verb ~ low_high * crt, data=data)) #s
summary(aov(num_verb ~ low_high * gov.exper, data=data)) #s
summary(aov(num_verb ~ low_high * need.cog, data=data)) #s
summary(aov(num_verb ~ low_high * noground_ground, data=data)) #s
summary(aov(num_verb ~ need.cog * aomt, data=data)) #ns
summary(aov(num_verb ~ need.cog * crt, data=data)) #s
summary(aov(num_verb ~ need.cog * gov.exper, data=data)) #s
summary(aov(num_verb ~ need.cog * noground_ground, data=data)) #ns

# collapse into positive and negative outcomes

install.packages("car")
require("car")

data$outcome_pos_neg <- recode(data$outcome, "c('1','3')='0'; else='1'")
summary(data$outcome_pos_neg)
data$outcome_pos_neg <- as.factor(data$outcome_pos_neg)

### How justified did people feel?

ddply(data, c("outcome_pos_neg"), summarise,
      N = length(userid),
      mean = mean(justification),
      sd = sd(justification))

t.test(justification~outcome_pos_neg, data=data)
mean(data$justification)

# Examine outcome bias

outcome_bias <- lm(justification~outcome_pos_neg, data=data)
summary(outcome_bias)

data$noground_ground <- as.factor(noground_ground)

outcome_bias_2 <- lm(justification~noground_ground, data=data)
summary(outcome_bias_2)

outcome_bias_3 <- lm(justification~low_high*noground_ground*outcome_pos_neg, data=data)
summary(outcome_bias_3)

outcome_bias_4 <- lm(justification~num_verb*low_high*noground_ground*outcome_pos_neg, data=data)
summary(outcome_bias_4)


#Three predictors 
summary(aov(justification ~ low_high + outcome + num_verb, data=data)) #s
summary(aov(justification ~ outcome * num_verb * low_high, data=data)) #s
summary(aov(noground_ground ~ low_high + need.cog + crt, data=data))#ns
summary(aov(num_verb ~ low_high + need.cog + crt, data=data)) #s (need.cog becomes better predictor than all tests before. crt becomes worse than when only predictor)

#Four predictors 
summary(aov(justification ~ outcome * num_verb * low_high * notaccountable_accountable, data=data)) #s
 
#Direction
summary(lm(justification ~ outcome, data=data))
summary(lm(num_verb ~ low_high, data=data))
summary(lm(justification ~ low_high + outcome, data=data))
summary(lm(justification ~ num_verb + outcome, data=data))
summary(lm(justification ~ low_high * outcome, data=data))
summary(lm(justification ~ num_verb * outcome, data=data))
summary(lm(justification ~ notaccountable_accountable * outcome, data=data))
summary(lm(justification ~ low_high + outcome + num_verb, data=data))
summary(lm(justification ~ outcome * num_verb * low_high, data=data))

#### 3-Way Interaction ###
interaction_model <- ddply(`3_way_interaction`, c("probability", "outcome"), summarise,
                           N    = length(justification),
                           mean = mean(justification),
                           sd   = sd(justification),
                           se   = sd / sqrt(N))
                           
interaction_model[["sd"]] <- c(1.2043876, 1.4591664, 0.8207827, 1.8451048, 0.7507572, 0.7559289, 0.9962049, 1.8211718)
interaction_model[["se"]] <- c(0.311022015, 0.380172658, 0.19893476, 0.434729589, 0.41659779, 0.266496544, 0.282936423, 0.488478722)

interaction_model.low <- interaction_model[which(interaction_model$probability == 0),]
interaction_model.low.act <- interaction_model.low[which(interaction_model.low$outcome == "Act:Neg" | interaction_model.low$outcome == "Act:Pos"),]
interaction_model.low.inact <- interaction_model.low[which(interaction_model.low$outcome == "Inact:Neg" | interaction_model.low$outcome == "Inact:Pos"),]

interaction_model.high <- interaction_model[which(interaction_model$probability == 1),]
interaction_model.high.act <- interaction_model.high[which(interaction_model.high$outcome == "Act:Neg" | interaction_model.high$outcome == "Act:Pos"),]
interaction_model.high.inact <- interaction_model.high[which(interaction_model.high$outcome == "Inact:Neg" | interaction_model.high$outcome == "Inact:Pos"),]

colors = c('darkblue', 'darkred', 'grey','black')

plot <- ggplot(data = interaction_model, aes(x=outcome, y=mean)) + 
  theme_bw() +
  geom_line(data=interaction_model.low.act, aes(group = probability, colour="Low probability"), size=1) +
  geom_line(data=interaction_model.high.act, aes(group = probability, colour="High probability"), size=1) +
  geom_line(data=interaction_model.low.inact, aes(group = probability, colour="Low probability"), size=1) +
  geom_line(data=interaction_model.high.inact, aes(group = probability, colour="High probability"), size=1) +
  geom_point(size=2)+ geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2) +
  labs(x="Outcome") +
  labs(y="Justification ") +
  ggtitle("") +
  theme(legend.text=element_text(size=8)) +
  theme(plot.title = element_text(hjust = 0.5),  legend.position="bottom", legend.title=element_blank()) +
  scale_x_discrete(breaks=c("Act:Neg","Act:Pos", "Inact:Neg", "Inact:Pos"), labels=c("Act & Negative", "Act & Positive", "Inact & Negative", "Inact & Positive"))
plot <- plot + scale_color_manual(values=colors, name="Conditions")
plot

outcome.low_high.interaction <- ddply(data, 
                                      c("low_high","outcome"),
                                      summarise, 
                                      N=length(userid), 
                                      mean=mean(justification, na.rm = TRUE), 
                                      sd=sd(justification, na.rm = TRUE), 
                                      se=sd/sqrt(N))
                            
outcome.low_high.interaction

### new interaction plots

data$num_verb <- Actors$num_verb

interaction.plot(data$low_high, data$num_verb, data$justification,
                 xlab="Likelihood",
                 ylab="Justification",
                 trace.label="Prob Type Choice"
                 )

data$num_verb <- factor(data$num_verb,
                        levels=c(0,1),
                        labels=c("Num", "Verbal"))

data$low_high <- factor(data$low_high,
                          levels=c(0,1),
                          labels=c("Low", "High"))

### Misc

#### Plot theme
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
          panel.margin = unit(2, "lines"),
          plot.margin = unit(c(1.75, 1.75, 1.75, 1.75), "lines"),
          plot.title = element_text(hjust = 0)
    )
} 

############################################################################################

#Add two_party trait
temp <- data$orient
levels(temp) <- c("0","1",NA,NA,NA,NA,NA,NA,NA)
data$two_party <- as.factor(temp)

#Add three_party trait
temp <- data$orient
levels(temp) <- c("0","1","2","2","2","2","2","2","2")
data$three_party <- as.factor(temp)

#Cleanup
rm(temp)

# Check if party orientation affected whether grounded plane 
data.ground = subset(data, noground_ground ==0 | noground_ground ==1)
data.two_party = subset(data.ground, two_party == 0 | two_party == 1)

lm.ground.func.two_party <- aov(noground_ground ~ two_party, data=data.two_party) #ns
summary(lm.ground.func.two_party)
