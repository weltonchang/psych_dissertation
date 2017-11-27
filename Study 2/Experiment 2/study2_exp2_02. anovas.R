### Study 2, Experiment 2 ###
### Observers ###
### welton@sas.upenn.edu ###
# last updated: 26 Nov 2017 #

options(scipen=999)

### Start data manipulation

data2 <- political_posturing_observers
data2 <- na.omit(data2)
data2 <- data.frame(data2)
summary(data2)

#Conversion from int to factor 
data2$low_high <- as.factor(data2$low_high)
data2$verb_num <- as.factor(data2$verb_num)
data2$neg_pos <- as.factor(data2$neg_pos)
data2$noact_act <- as.factor(data2$noact_act)

# START TASK 1
aov.1 <- aov(justification_1 ~ low_high + verb_num, data=data2)
aov.1.int <- aov(justification_1 ~ low_high * verb_num, data=data2)
summary(aov.1.int)

### Study 2 Figure 3
### Plot interaction

data2$verb_num <- factor(data2$verb_num,
                        levels=c(0,1),
                        labels=c("Verbal", "Num"))

data2$low_high <- factor(data2$low_high,
                        levels=c(0,1),
                        labels=c("Low", "High"))

interaction.plot(data2$low_high, data2$verb_num, data2$justification_1,
                 xlab="Event Likelihood", 
                 ylab="Justification Level",
                 trace.label="Probability Type",
                 lwd=2.5,
                 cex.lab=1.3,
                 cex.axis=1.3,
                 legend="TRUE",
                 leg.bty="y")


# Call:
#   aov(formula = justification_1 ~ low_high + verb_num, data = data)
# 
# Terms:
#   low_high  verb_num Residuals
# Sum of Squares   35.66350  14.40935 196.66934
# Deg. of Freedom         1         1       125
# 
# Residual standard error: 1.254334
# Estimated effects may be unbalanced
# 10 observations deleted due to missingness

summary(aov(justification_1 ~ low_high + verb_num, data=data2))

# Df Sum Sq Mean Sq F value   Pr(>F)    
# low_high      1  35.66   35.66  22.667 5.24e-06 ***
#   verb_num      1  14.41   14.41   9.158  0.00301 ** 
#   Residuals   125 196.67    1.57                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 10 observations deleted due to missingness

#START TASK 2

summary(aov(justification_2 ~ low_high + verb_num + neg_pos + noact_act, data=data2))

summary(lm(justification_2 ~ low_high + verb_num + neg_pos + noact_act, data=data2))

# Df Sum Sq Mean Sq F value   Pr(>F)    
# low_high      1   0.27    0.27   0.149    0.700    
# verb_num      1   1.53    1.53   0.859    0.356    
# neg_pos       1  49.17   49.17  27.601 6.86e-07 ***
#   noact_act     1  29.86   29.86  16.765 7.86e-05 ***
#   Residuals   116 206.64    1.78                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 17 observations deleted due to missingness

summary(aov(justification_2 ~ low_high + verb_num + low_high * verb_num + neg_pos + noact_act + neg_pos * noact_act, data=data2))

summary(lm(justification_2 ~ low_high + verb_num + low_high * verb_num + neg_pos + noact_act + neg_pos * noact_act, data=data2))

summary(lm(justification_diff ~ low_high + verb_num + low_high * verb_num + neg_pos + noact_act + neg_pos * noact_act, data=data2))

# Df Sum Sq Mean Sq F value   Pr(>F)    
# low_high            1   0.27    0.27   0.156   0.6940    
# verb_num            1   1.53    1.53   0.894   0.3464    
# neg_pos             1  49.17   49.17  28.738 4.38e-07 ***
#   noact_act           1  29.86   29.86  17.456 5.79e-05 ***
#   low_high:verb_num   1   8.46    8.46   4.945   0.0281 *  
#   neg_pos:noact_act   1   3.14    3.14   1.836   0.1781    
# Residuals         114 195.04    1.71                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 17 observations deleted due to missingness

#Subset all negative outcomes 

all.bad <- subset(data, neg_pos == 0, select = userid:race)
all.bad_act <- subset(all.bad, noact_act == 1, select = userid:race)
all.bad_noact <- subset(all.bad, noact_act == 0, select = userid:race)
all.good <- subset(data, neg_pos == 1, select = userid:race)
all.good_act <- subset(all.good, noact_act == 1, select = userid:race)
all.good_noact <- subset(all.good, noact_act == 0, select = userid:race)

summary(aov(justification_2 ~ act_pos + act_neg + noact_pos + noact_neg, data=data2))

# Df Sum Sq Mean Sq F value  Pr(>F)   
# act_pos       1  21.39  21.386   9.748 0.00223 **
#   act_neg       1   0.25   0.247   0.112 0.73803   
# noact_pos     1   5.73   5.734   2.614 0.10848   
# noact_neg     1   6.31   6.312   2.877 0.09236 . 
# Residuals   124 272.04   2.194                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 12 observations deleted due to missingness


data2$bad_outcome <- as.factor(data2$bad_outcome)
data2$good_outcome <- as.factor(data2$good_outcome)

summary(aov(justification_diff ~ good_outcome + bad_outcome, data=data2))

# Df Sum Sq Mean Sq F value Pr(>F)
# good_outcome   1   11.9  11.884   1.758  0.187
# Residuals    139  939.8   6.761 

data.summary <- ddply(data, c("good_outcome","bad_outcome"),summarise, N=length(userid), mean=mean(justification_diff), sd=sd(justification_diff), se=sd/sqrt(N))

#START TASK 3

#Check if justifications are related
lm_just <- lm(justification_1 ~ justification_2, data=data)
summary(lm_just)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.3694 -0.2512  0.1037  0.8671  2.3402 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      4.54151    0.42306  10.735   <2e-16 ***
#   justification_2  0.11827    0.08182   1.445    0.151    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.387 on 118 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.0174,	Adjusted R-squared:  0.009072 
# F-statistic: 2.089 on 1 and 118 DF,  p-value: 0.151

### Set Theme

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