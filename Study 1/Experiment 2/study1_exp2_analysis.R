### Study 1, Experiment 2 ###
### Sub-Study (40-60) Complete ###
### welton@sas.upenn.edu ###
# last updated: 26 Nov 2017 #
options(scipen = 666)

#################
### Libraries ###
#################

require(psych)
require(plyr)
require(grid)
require(ggplot2)
require(compute.es)
require(effsize)
install.packages('effsize')
library(plyr)
library(grid)
library(ggplot2)
library(compute.es)
library(effsize)

###################
### Graph Theme ###
###################
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

##############
### Import ###
##############
data
data <- X20160217_study2_LQ_4060
data <-X20160217_study1_exp2_LQ_4060
rm(X20160217_study2_LQ_4060)

####################
##### Analysis #####
####################

### Factorization ###

data$Accountability <- as.factor(data$Accountability)
data$Environment <- as.factor(data$Environment)


### PAOA Check ###

# Create vectors of only accountable subjects
acc1 <- data[ which(data$Accountability == 1), ]
acc2 <- data[ which(data$Accountability == 2), ]

# Perform PAOA check
t.test(acc1$paoa.avg, acc2$paoa.avg)
# t = -0.54968, df = 21.121, p-value = 0.5883
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.9158425  0.5328055
# sample estimates:
#   mean of x mean of y 
# 2.029070  2.220588



### Summary Statistics ###

# Score
ddply(data, c("Environment", "Accountability"), summarise,
      N    = length(R60),
      mean = mean(R60),
      sd   = sd(R60),
      se   = sd / sqrt(N))
# ---------------------------------------------------------
# Environment Accountability  N       mean       sd        se
#           0              0 17  -7.147059 37.20331  9.023128
#           0              1 29  -9.344828 28.63349  5.317106
#           0              2  9 -23.833333 19.32130  6.440432
#           1              0 14   5.285714 32.72362  8.745755
#           1              1 14  -1.928571 23.02829  6.154570
#           1              2  8  39.625000 32.50577 11.492525

# Moves
ddply(data, c("Environment", "Accountability"), summarise,
      N    = length(Moves),
      mean = mean(Moves),
      sd   = sd(Moves),
      se   = sd / sqrt(N))
# ---------------------------------------------------------
# Environment Accountability  N     mean        sd       se
#           0              0 17 19.94118 14.306775 3.469903
#           0              1 29 24.89655 15.428047 2.864916
#           0              2  9 21.66667 14.097872 4.699291
#           1              0 14 16.85714 16.023335 4.282416
#           1              1 14 21.42857 13.642210 3.646034
#           1              2  8  7.75000  5.147815 1.820027

# Hits
ddply(data, c("Environment", "Accountability"), summarise,
      N    = length(Hits),
      mean = mean(Hits),
      sd   = sd(Hits),
      se   = sd / sqrt(N))
# ---------------------------------------------------------
# Environment Accountability  N     mean        sd       se
#           0              0 17 20.94118 11.431703 2.772595
#           0              1 29 21.03448  7.711229 1.431939
#           0              2  9 15.66667  5.074446 1.691482
#           1              0 14 24.57143  8.473618 2.264670
#           1              1 14 22.92857  6.402695 1.711192
#           1              2  8 34.50000 10.596495 3.746427

# Misses
ddply(data, c("Environment", "Accountability"), summarise,
      N    = length(Misses),
      mean = mean(Misses),
      sd   = sd(Misses),
      se   = sd / sqrt(N))
# ---------------------------------------------------------
# Environment Accountability  N     mean        sd       se
#           0              0 17 39.05882 11.431703 2.772595
#           0              1 29 38.96552  7.711229 1.431939
#           0              2  9 44.33333  5.074446 1.691482
#           1              0 14 35.42857  8.473618 2.264670
#           1              1 14 37.07143  6.402695 1.711192
#           1              2  8 25.50000 10.596495 3.746427

### Comments and Length of Rationales

ddply(data, c("Environment", "Accountability"), summarise,
      comment = mean(CommentCount),
      length = mean(CommentAvgLen))


### MAIN EFFECTS ANOVAS ###

# Accountability
summary(aov(R60 ~ Accountability, data=data)) #ns
summary(aov(Moves ~ Accountability, data=data)) #ns
summary(aov(Hits ~ Accountability, data=data)) #ns
summary(aov(Misses ~ Accountability, data=data)) #ns
summary(aov(CommentCount ~ Accountability, data=data)) #s
summary(aov(CommentAvgLen ~ Accountability, data=data)) #s

# Environment
summary(aov(R60 ~ Environment, data=data)) #s
summary(aov(Moves ~ Environment, data=data)) #s
summary(aov(Hits ~ Environment, data=data)) #s
summary(aov(Misses ~ Environment, data=data)) #s
summary(aov(CommentCount ~ Environment, data=data)) #ns
summary(aov(CommentAvgLen ~ Environment, data=data)) #ns

# Accountability + Environment
summary(aov(R60 ~ Accountability + Environment, data=data)) #s (env)
summary(aov(Moves ~ Accountability + Environment, data=data)) #ns
summary(aov(Hits ~ Accountability + Environment, data=data)) #s (env)
summary(aov(Misses ~ Accountability + Environment, data=data)) #s (env)
summary(aov(CommentCount ~ Accountability + Environment, data=data)) #s (acc)
summary(aov(CommentAvgLen ~ Accountability + Environment, data=data)) #s (acc)

# Accountability * Environment
summary(aov(R60 ~ Accountability * Environment, data=data)) #s (env; env:acc)
summary(aov(Moves ~ Accountability * Environment, data=data)) #s (env)
summary(aov(Hits ~ Accountability * Environment, data=data)) #s (env; env:acc)
summary(aov(Misses ~ Accountability * Environment, data=data)) #s (env; env:acc)
summary(aov(CommentCount ~ Accountability * Environment, data=data)) #s (acc)
summary(aov(CommentAvgLen ~ Accountability * Environment, data=data)) #s (acc)

# Interaction plot

data$Accountability <- factor(data$Accountability,
                              levels = c(0,1,2),
                              labels = c("None", "Outcome", "Process"))

data$Environment <- factor(data$Environment,
                           levels=c(0,1),
                           labels=c("Random", "Non-Random"))

graphics.off()

# Study 1 Figure 11

interaction.plot(data$Environment, data$Accountability, data$R60, type=c("l"), 
                 trace.label="Accountability", xlab="Environmental Validity", ylab="Total Score",
                 lwd=2.5, cex=2, leg.bty = "y")


### Calculate Effect Size
library(effsize)
control <- subset(data, Accountability==0 & Environment==0)
non.random <- subset(data, Accountability==0 & Environment==1)
outcome.random <- subset(data, Accountability==1 & Environment==0)
process.random <- subset(data, Accountability==2 & Environment==0)
outcome.nonrandom <- subset(data, Accountability==1 & Environment==1)
process.nonrandom <- subset(data, Accountability==2 & Environment==1)

## compute Cohen's d for score
cohen.d(non.random$R60,control$R60)
cohen.d(outcome.random$R60,control$R60)
cohen.d(process.random$R60,control$R60)
cohen.d(outcome.nonrandom$R60, control$R60)
cohen.d(process.nonrandom$R60, control$R60)

## compute Cohen's d for score
cohen.d(non.random$R60,control$R60)
cohen.d(outcome.random$R60,control$R60)
cohen.d(process.random$R60,control$R60)
cohen.d(outcome.nonrandom$R60, control$R60)
cohen.d(process.nonrandom$R60, control$R60)

## compute Cohen's d for moves
cohen.d(non.random$Moves,control$Moves)
cohen.d(outcome.random$Moves,control$Moves)
cohen.d(process.random$Moves,control$Moves)
cohen.d(outcome.nonrandom$Moves, control$Moves)
cohen.d(process.nonrandom$Moves, control$Moves)


## compute Cohen's d for Comment Count
cohen.d(non.random$CommentCount,control$CommentCount)
cohen.d(outcome.random$CommentCount,control$CommentCount)
cohen.d(process.random$CommentCount,control$CommentCount)
cohen.d(outcome.nonrandom$CommentCount, control$CommentCount)
cohen.d(process.nonrandom$CommentCount, control$CommentCount)

## compute Cohen's d for Comment Length
cohen.d(non.random$CommentAvgLen,control$CommentAvgLen)
cohen.d(outcome.random$CommentAvgLen,control$CommentAvgLen)
cohen.d(process.random$CommentAvgLen,control$CommentAvgLen)
cohen.d(outcome.nonrandom$CommentAvgLen, control$CommentAvgLen)
cohen.d(process.nonrandom$CommentAvgLen, control$CommentAvgLen)

### Wilcox Tests ###

# Score
wilcox.test(acc1$R60, acc2$R60, paired = FALSE)
# W = 313.5, p-value = 0.3981
# alternative hypothesis: true location shift is not equal to 0
# 
# Warning message:
#   In wilcox.test.default(acc1$R60, acc2$R60, paired = FALSE) :
#   cannot compute exact p-value with ties

# Moves
wilcox.test(acc1$Moves, acc2$Moves, paired = FALSE)
# W = 499, p-value = 0.02903
# alternative hypothesis: true location shift is not equal to 0
# 
# Warning message:
#   In wilcox.test.default(acc1$Moves, acc2$Moves, paired = FALSE) :
#   cannot compute exact p-value with ties

# Hits
wilcox.test(acc1$Hits, acc2$Hits, paired = FALSE)
# W = 348.5, p-value = 0.7863
# alternative hypothesis: true location shift is not equal to 0
# 
# Warning message:
#   In wilcox.test.default(acc1$Hits, acc2$Hits, paired = FALSE) :
#   cannot compute exact p-value with ties

# Misses
wilcox.test(acc1$Misses, acc2$Misses, paired = FALSE)
# W = 382.5, p-value = 0.7863
# alternative hypothesis: true location shift is not equal to 0
# 
# Warning message:
#   In wilcox.test.default(acc1$Misses, acc2$Misses, paired = FALSE) :
#   cannot compute exact p-value with ties



### Anova copies of Wilcox tests ###

# Create dataframe with only observations where accountability= 1 or 2
only_acc_obs <- rbind(acc1, acc2)

# Score
summary(aov(R60 ~ Accountability, data=only_acc_obs))
#                 Df Sum Sq Mean Sq F value Pr(>F)
# Accountability  1   2046  2046.2   2.053  0.157
# Residuals      58  57808   996.7 

# Moves
summary(aov(Moves ~ Accountability, data=only_acc_obs))
#                 Df Sum Sq Mean Sq F value Pr(>F)  
# Accountability  1    912   911.5   4.483 0.0385 *
# Residuals      58  11793   203.3 

# Hits
summary(aov(Hits ~ Accountability, data=only_acc_obs))
#                 Df Sum Sq Mean Sq F value Pr(>F)
# Accountability  1    101  100.93   1.239   0.27
# Residuals      58   4726   81.48 

# Misses
summary(aov(Misses ~ Accountability, data=only_acc_obs))
#                 Df Sum Sq Mean Sq F value Pr(>F)
# Accountability  1    101  100.93   1.239   0.27
# Residuals      58   4726   81.48  

###############
#### Graph ####
###############

# Create vector for each condition combination
env0_acc0 <- data[ which(data$Environment == 0 & data$Accountability == 0), ]
env0_acc0 <- c(mean(env0_acc0$R01), mean(env0_acc0$R02),mean(env0_acc0$R03),mean(env0_acc0$R04),mean(env0_acc0$R05),
               mean(env0_acc0$R06), mean(env0_acc0$R07),mean(env0_acc0$R08),mean(env0_acc0$R09),mean(env0_acc0$R10),
               mean(env0_acc0$R11), mean(env0_acc0$R12),mean(env0_acc0$R13),mean(env0_acc0$R14),mean(env0_acc0$R15),
               mean(env0_acc0$R16), mean(env0_acc0$R17),mean(env0_acc0$R18),mean(env0_acc0$R19),mean(env0_acc0$R20),
               mean(env0_acc0$R21), mean(env0_acc0$R22),mean(env0_acc0$R23),mean(env0_acc0$R24),mean(env0_acc0$R25),
               mean(env0_acc0$R26), mean(env0_acc0$R27),mean(env0_acc0$R28),mean(env0_acc0$R29),mean(env0_acc0$R30),
               mean(env0_acc0$R31), mean(env0_acc0$R32),mean(env0_acc0$R33),mean(env0_acc0$R34),mean(env0_acc0$R35),
               mean(env0_acc0$R36), mean(env0_acc0$R37),mean(env0_acc0$R38),mean(env0_acc0$R39),mean(env0_acc0$R40),
               mean(env0_acc0$R41), mean(env0_acc0$R42),mean(env0_acc0$R43),mean(env0_acc0$R44),mean(env0_acc0$R45),
               mean(env0_acc0$R46), mean(env0_acc0$R47),mean(env0_acc0$R48),mean(env0_acc0$R49),mean(env0_acc0$R50),
               mean(env0_acc0$R51), mean(env0_acc0$R52),mean(env0_acc0$R53),mean(env0_acc0$R54),mean(env0_acc0$R55),
               mean(env0_acc0$R56), mean(env0_acc0$R57),mean(env0_acc0$R58),mean(env0_acc0$R59),mean(env0_acc0$R60))

env0_acc1 <- data[ which(data$Environment == 0 & data$Accountability == 1), ]
env0_acc1 <- c(mean(env0_acc1$R01), mean(env0_acc1$R02),mean(env0_acc1$R03),mean(env0_acc1$R04),mean(env0_acc1$R05),
               mean(env0_acc1$R06), mean(env0_acc1$R07),mean(env0_acc1$R08),mean(env0_acc1$R09),mean(env0_acc1$R10),
               mean(env0_acc1$R11), mean(env0_acc1$R12),mean(env0_acc1$R13),mean(env0_acc1$R14),mean(env0_acc1$R15),
               mean(env0_acc1$R16), mean(env0_acc1$R17),mean(env0_acc1$R18),mean(env0_acc1$R19),mean(env0_acc1$R20),
               mean(env0_acc1$R21), mean(env0_acc1$R22),mean(env0_acc1$R23),mean(env0_acc1$R24),mean(env0_acc1$R25),
               mean(env0_acc1$R26), mean(env0_acc1$R27),mean(env0_acc1$R28),mean(env0_acc1$R29),mean(env0_acc1$R30),
               mean(env0_acc1$R31), mean(env0_acc1$R32),mean(env0_acc1$R33),mean(env0_acc1$R34),mean(env0_acc1$R35),
               mean(env0_acc1$R36), mean(env0_acc1$R37),mean(env0_acc1$R38),mean(env0_acc1$R39),mean(env0_acc1$R40),
               mean(env0_acc1$R41), mean(env0_acc1$R42),mean(env0_acc1$R43),mean(env0_acc1$R44),mean(env0_acc1$R45),
               mean(env0_acc1$R46), mean(env0_acc1$R47),mean(env0_acc1$R48),mean(env0_acc1$R49),mean(env0_acc1$R50),
               mean(env0_acc1$R51), mean(env0_acc1$R52),mean(env0_acc1$R53),mean(env0_acc1$R54),mean(env0_acc1$R55),
               mean(env0_acc1$R56), mean(env0_acc1$R57),mean(env0_acc1$R58),mean(env0_acc1$R59),mean(env0_acc1$R60))

env0_acc2 <- data[ which(data$Environment == 0 & data$Accountability == 2), ]
env0_acc2 <- c(mean(env0_acc2$R01), mean(env0_acc2$R02),mean(env0_acc2$R03),mean(env0_acc2$R04),mean(env0_acc2$R05),
               mean(env0_acc2$R06), mean(env0_acc2$R07),mean(env0_acc2$R08),mean(env0_acc2$R09),mean(env0_acc2$R10),
               mean(env0_acc2$R11), mean(env0_acc2$R12),mean(env0_acc2$R13),mean(env0_acc2$R14),mean(env0_acc2$R15),
               mean(env0_acc2$R16), mean(env0_acc2$R17),mean(env0_acc2$R18),mean(env0_acc2$R19),mean(env0_acc2$R20),
               mean(env0_acc2$R21), mean(env0_acc2$R22),mean(env0_acc2$R23),mean(env0_acc2$R24),mean(env0_acc2$R25),
               mean(env0_acc2$R26), mean(env0_acc2$R27),mean(env0_acc2$R28),mean(env0_acc2$R29),mean(env0_acc2$R30),
               mean(env0_acc2$R31), mean(env0_acc2$R32),mean(env0_acc2$R33),mean(env0_acc2$R34),mean(env0_acc2$R35),
               mean(env0_acc2$R36), mean(env0_acc2$R37),mean(env0_acc2$R38),mean(env0_acc2$R39),mean(env0_acc2$R40),
               mean(env0_acc2$R41), mean(env0_acc2$R42),mean(env0_acc2$R43),mean(env0_acc2$R44),mean(env0_acc2$R45),
               mean(env0_acc2$R46), mean(env0_acc2$R47),mean(env0_acc2$R48),mean(env0_acc2$R49),mean(env0_acc2$R50),
               mean(env0_acc2$R51), mean(env0_acc2$R52),mean(env0_acc2$R53),mean(env0_acc2$R54),mean(env0_acc2$R55),
               mean(env0_acc2$R56), mean(env0_acc2$R57),mean(env0_acc2$R58),mean(env0_acc2$R59),mean(env0_acc2$R60))

env1_acc0 <- data[ which(data$Environment == 1 & data$Accountability == 0), ]
env1_acc0 <- c(mean(env1_acc0$R01), mean(env1_acc0$R02),mean(env1_acc0$R03),mean(env1_acc0$R04),mean(env1_acc0$R05),
               mean(env1_acc0$R06), mean(env1_acc0$R07),mean(env1_acc0$R08),mean(env1_acc0$R09),mean(env1_acc0$R10),
               mean(env1_acc0$R11), mean(env1_acc0$R12),mean(env1_acc0$R13),mean(env1_acc0$R14),mean(env1_acc0$R15),
               mean(env1_acc0$R16), mean(env1_acc0$R17),mean(env1_acc0$R18),mean(env1_acc0$R19),mean(env1_acc0$R20),
               mean(env1_acc0$R21), mean(env1_acc0$R22),mean(env1_acc0$R23),mean(env1_acc0$R24),mean(env1_acc0$R25),
               mean(env1_acc0$R26), mean(env1_acc0$R27),mean(env1_acc0$R28),mean(env1_acc0$R29),mean(env1_acc0$R30),
               mean(env1_acc0$R31), mean(env1_acc0$R32),mean(env1_acc0$R33),mean(env1_acc0$R34),mean(env1_acc0$R35),
               mean(env1_acc0$R36), mean(env1_acc0$R37),mean(env1_acc0$R38),mean(env1_acc0$R39),mean(env1_acc0$R40),
               mean(env1_acc0$R41), mean(env1_acc0$R42),mean(env1_acc0$R43),mean(env1_acc0$R44),mean(env1_acc0$R45),
               mean(env1_acc0$R46), mean(env1_acc0$R47),mean(env1_acc0$R48),mean(env1_acc0$R49),mean(env1_acc0$R50),
               mean(env1_acc0$R51), mean(env1_acc0$R52),mean(env1_acc0$R53),mean(env1_acc0$R54),mean(env1_acc0$R55),
               mean(env1_acc0$R56), mean(env1_acc0$R57),mean(env1_acc0$R58),mean(env1_acc0$R59),mean(env1_acc0$R60))

env1_acc1 <- data[ which(data$Environment == 1 & data$Accountability == 1), ]
env1_acc1 <- c(mean(env1_acc1$R01), mean(env1_acc1$R02),mean(env1_acc1$R03),mean(env1_acc1$R04),mean(env1_acc1$R05),
               mean(env1_acc1$R06), mean(env1_acc1$R07),mean(env1_acc1$R08),mean(env1_acc1$R09),mean(env1_acc1$R10),
               mean(env1_acc1$R11), mean(env1_acc1$R12),mean(env1_acc1$R13),mean(env1_acc1$R14),mean(env1_acc1$R15),
               mean(env1_acc1$R16), mean(env1_acc1$R17),mean(env1_acc1$R18),mean(env1_acc1$R19),mean(env1_acc1$R20),
               mean(env1_acc1$R21), mean(env1_acc1$R22),mean(env1_acc1$R23),mean(env1_acc1$R24),mean(env1_acc1$R25),
               mean(env1_acc1$R26), mean(env1_acc1$R27),mean(env1_acc1$R28),mean(env1_acc1$R29),mean(env1_acc1$R30),
               mean(env1_acc1$R31), mean(env1_acc1$R32),mean(env1_acc1$R33),mean(env1_acc1$R34),mean(env1_acc1$R35),
               mean(env1_acc1$R36), mean(env1_acc1$R37),mean(env1_acc1$R38),mean(env1_acc1$R39),mean(env1_acc1$R40),
               mean(env1_acc1$R41), mean(env1_acc1$R42),mean(env1_acc1$R43),mean(env1_acc1$R44),mean(env1_acc1$R45),
               mean(env1_acc1$R46), mean(env1_acc1$R47),mean(env1_acc1$R48),mean(env1_acc1$R49),mean(env1_acc1$R50),
               mean(env1_acc1$R51), mean(env1_acc1$R52),mean(env1_acc1$R53),mean(env1_acc1$R54),mean(env1_acc1$R55),
               mean(env1_acc1$R56), mean(env1_acc1$R57),mean(env1_acc1$R58),mean(env1_acc1$R59),mean(env1_acc1$R60))

env1_acc2 <- data[ which(data$Environment == 1 & data$Accountability == 2), ]
env1_acc2 <- c(mean(env1_acc2$R01), mean(env1_acc2$R02),mean(env1_acc2$R03),mean(env1_acc2$R04),mean(env1_acc2$R05),
               mean(env1_acc2$R06), mean(env1_acc2$R07),mean(env1_acc2$R08),mean(env1_acc2$R09),mean(env1_acc2$R10),
               mean(env1_acc2$R11), mean(env1_acc2$R12),mean(env1_acc2$R13),mean(env1_acc2$R14),mean(env1_acc2$R15),
               mean(env1_acc2$R16), mean(env1_acc2$R17),mean(env1_acc2$R18),mean(env1_acc2$R19),mean(env1_acc2$R20),
               mean(env1_acc2$R21), mean(env1_acc2$R22),mean(env1_acc2$R23),mean(env1_acc2$R24),mean(env1_acc2$R25),
               mean(env1_acc2$R26), mean(env1_acc2$R27),mean(env1_acc2$R28),mean(env1_acc2$R29),mean(env1_acc2$R30),
               mean(env1_acc2$R31), mean(env1_acc2$R32),mean(env1_acc2$R33),mean(env1_acc2$R34),mean(env1_acc2$R35),
               mean(env1_acc2$R36), mean(env1_acc2$R37),mean(env1_acc2$R38),mean(env1_acc2$R39),mean(env1_acc2$R40),
               mean(env1_acc2$R41), mean(env1_acc2$R42),mean(env1_acc2$R43),mean(env1_acc2$R44),mean(env1_acc2$R45),
               mean(env1_acc2$R46), mean(env1_acc2$R47),mean(env1_acc2$R48),mean(env1_acc2$R49),mean(env1_acc2$R50),
               mean(env1_acc2$R51), mean(env1_acc2$R52),mean(env1_acc2$R53),mean(env1_acc2$R54),mean(env1_acc2$R55),
               mean(env1_acc2$R56), mean(env1_acc2$R57),mean(env1_acc2$R58),mean(env1_acc2$R59),mean(env1_acc2$R60))

# Generate Perfect Game vector (unused)
perfect_game <- seq(2,120,2) 

# Generate Trial # vector
trials <- c(1:60)

# Combine above vectors into dataframe 
per_round_data <- data.frame(trials, perfect_game, env0_acc0, env0_acc1, env0_acc2, env1_acc0, env1_acc1, env1_acc2)

# Study 1 Figure 8
# Graph 1
score_after_each_round <- ggplot(data=per_round_data) + 
  theme_bw() +
  geom_line(aes(x=trials, y=env0_acc0, colour="1"), size = 2) + 
  geom_line(aes(x=trials, y=env0_acc1, colour="2"), size = 2) + 
  geom_line(aes(x=trials, y=env0_acc2, colour="3"), size = 2) + 
  geom_line(aes(x=trials, y=env1_acc0, colour="4"), size = 2) + 
  geom_line(aes(x=trials, y=env1_acc1, colour="5"), size = 2) + 
  geom_line(aes(x=trials, y=env1_acc2, colour="6"), size = 2) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.text=element_text(size=13),
        text = element_text(size=20)) +
  labs(x = "Trial", y = "Total Score", title= "Experiment 1b Scores Across Conditions", color = "Conditions") +
  scale_colour_manual(labels = c("Random Env & No Accountability", 
                                 "Random Env & Outcome", 
                                 "Random Env & Process", 
                                 "Non-random Env & No Accountability", 
                                 "Non-random Env & Outcome",  
                                 "Non-random Env & Process"), 
                      values=c("blue", 
                               "green", 
                               "red", 
                               "darkblue", 
                               "darkgreen", 
                               "darkred")) +
  guides(linetype = guide_legend(override.aes = list(size = 1.5)))

score_after_each_round

### New Graph

score_after_each_round <- ggplot(data=per_round_data) + 
  theme_bw() +
  geom_line(aes(x=trials, y=env0_acc0, colour="1"), size = 2) + 
  geom_line(aes(x=trials, y=env0_acc1, colour="2"), size = 2) + 
  geom_line(aes(x=trials, y=env0_acc2, colour="3"), size = 2) + 
  geom_line(aes(x=trials, y=env1_acc0, colour="4"), size = 2) + 
  geom_line(aes(x=trials, y=env1_acc1, colour="5"), size = 2) + 
  geom_line(aes(x=trials, y=env1_acc2, colour="6"), size = 2) + 
  theme(plot.title = element_text(hjust = 0.5),  legend.position="bottom", legend.title=element_blank()) +
  labs(x = "Trial", y = "Total Score", title= "Experiment 1b Score Across Conditions", color = "Conditions") +
  scale_colour_manual(labels = c("Random Env & No Accountability", 
                                 "Random Env & Outcome", 
                                 "Random Env & Process", 
                                 "Non-random Env & No Accountability", 
                                 "Non-random Env & Outcome",  
                                 "Non-random Env & Process"), 
                      values=c("blue", 
                               "green", 
                               "red", 
                               "darkblue", 
                               "darkgreen", 
                               "darkred")) +
  guides(linetype = guide_legend(override.aes = list(size = 1.5))) 

score_after_each_round

# Graph 2
# unused
score_after_each_round_only_random <- ggplot(data=per_round_data) + 
  theme_bw() +
  geom_line(aes(x=trials, y=env0_acc0, colour="1"), size = 1) + 
  geom_line(aes(x=trials, y=env0_acc1, colour="2"), size = 1) + 
  geom_line(aes(x=trials, y=env0_acc2, colour="3"), size = 1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Trial", y = "Total Score", title= "Average Score Across Conditions", color = "Conditions") +
  scale_colour_manual(labels = c("Random Environment w/ No Accountability", 
                                 "Random Environment w/ Outcome Accountability", 
                                 "Random Environment w/ Process Accountability"), 
                      values=c("darkblue", 
                               "darkgreen", 
                               "darkred")) +
  guides(linetype = guide_legend(override.aes = list(size = 5)))
score_after_each_round_only_random


#########################################################
################ Demographic Information ################
#########################################################

### Subset demographic info ###
demographic <- subset(data, select = c("educ", "age", "gender", "race"))



# View age
v1 <- subset(demographic, age == 1)
v2 <- subset(demographic, age == 2)
v3 <- subset(demographic, age == 3)
v4 <- subset(demographic, age == 4)
v5 <- subset(demographic, age == 5)
v6 <- subset(demographic, age == 6)
v7 <- subset(demographic, age == 7)
# Clean age
rm(v1, v2, v3, v4, v5, v6, v7)

# View gender
v1 <- subset(demographic, gender == 1)
v2 <- subset(demographic, gender == 2)
# Clean gender 
rm(v1, v2)

# View race
v1 <- subset(demographic, race == 1)
v2 <- subset(demographic, race == 2)
v3 <- subset(demographic, race == 3)
v4 <- subset(demographic, race == 4)
v5 <- subset(demographic, race == 5)
v6 <- subset(demographic, race == 6)
v7 <- subset(demographic, race == 7)
# Clean race
rm(v1, v2, v3, v4, v5, v6, v7)

# View educ
v1 <- subset(demographic, educ == 1)
v2 <- subset(demographic, educ == 2)
v3 <- subset(demographic, educ == 3)
v4 <- subset(demographic, educ == 4)
v5 <- subset(demographic, educ == 5)
v6 <- subset(demographic, educ == 6)
v7 <- subset(demographic, educ == 7)
v8 <- subset(demographic, educ == 8)
# Clean educ
rm(v1, v2, v3, v4, v5, v6, v7, v8)

########################################
################ Alphas ################
########################################

consc <- subset(data, select = c("consc_1", "consc_2*", "consc_3", "consc_4*"))
alpha(consc)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.78      0.78    0.77      0.47 3.6 0.038  4.5 1.3
# 
# lower alpha upper     95% confidence boundaries
# 0.71 0.78 0.85 
# 
# Reliability if an item is dropped:
#          raw_alpha std.alpha G6(smc) average_r S/N alpha se
# consc_1       0.73      0.73    0.69      0.47 2.7    0.047
# consc_2*      0.74      0.75    0.67      0.50 3.0    0.046
# consc_3       0.76      0.76    0.71      0.51 3.2    0.044
# consc_4*      0.66      0.67    0.60      0.41 2.1    0.062
# 
# Item statistics 
#           n raw.r std.r r.cor r.drop mean  sd
# consc_1  91  0.77  0.78  0.67   0.57  4.1 1.7
# consc_2* 91  0.78  0.75  0.67   0.56  4.1 1.8
# consc_3  91  0.71  0.74  0.60   0.52  5.5 1.4
# consc_4* 91  0.85  0.84  0.79   0.70  4.3 1.7
# 
# Non missing response frequency for each item
#             1    2    3    4    5    6    7 miss
# consc_1  0.07 0.16 0.20 0.09 0.24 0.19 0.05    0
# consc_2* 0.05 0.19 0.23 0.08 0.13 0.22 0.10    0
# consc_3  0.01 0.07 0.01 0.10 0.20 0.38 0.23    0
# consc_4* 0.03 0.11 0.30 0.10 0.12 0.24 0.10    0

extra <- subset(data, select = c("extro_1", "extro_2*", "extro_3", "extro_4*"))
alpha(extra)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.84      0.84    0.81      0.56 5.1 0.028  4.5 1.3
# 
# lower alpha upper     95% confidence boundaries
# 0.78 0.84 0.89 
# 
# Reliability if an item is dropped:
#          raw_alpha std.alpha G6(smc) average_r S/N alpha se
# extro_1       0.80      0.80    0.75      0.57 4.0    0.038
# extro_2*      0.81      0.81    0.74      0.59 4.3    0.034
# extro_3       0.81      0.81    0.76      0.59 4.2    0.035
# extro_4*      0.75      0.75    0.68      0.50 3.0    0.045
# 
# Item statistics 
#           n raw.r std.r r.cor r.drop mean  sd
# extro_1  91  0.81  0.81  0.72   0.66  3.9 1.6
# extro_2* 91  0.79  0.79  0.71   0.62  5.0 1.6
# extro_3  91  0.80  0.80  0.70   0.63  4.5 1.6
# extro_4* 91  0.87  0.87  0.84   0.76  4.7 1.6
# 
# Non missing response frequency for each item
#             1    2    3    4    5    6    7 miss
# extro_1  0.07 0.16 0.19 0.16 0.27 0.08 0.07    0
# extro_2* 0.01 0.09 0.13 0.09 0.20 0.32 0.16    0
# extro_3  0.04 0.05 0.26 0.08 0.25 0.21 0.10    0
# extro_4* 0.01 0.04 0.24 0.18 0.15 0.22 0.15    0

agree <- subset(data, select = c("agree_1", "agree_2*", "agree_3", "agree_4*"))
alpha(agree)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
# 0.74      0.75    0.81      0.42 2.9 0.046  5.5 0.92
# 
# lower alpha upper     95% confidence boundaries
# 0.65 0.74 0.83 
# 
# Reliability if an item is dropped:
#          raw_alpha std.alpha G6(smc) average_r S/N alpha se
# agree_1       0.68      0.68    0.65      0.41 2.1    0.057
# agree_2*      0.64      0.65    0.66      0.38 1.9    0.069
# agree_3       0.70      0.68    0.68      0.42 2.2    0.052
# agree_4*      0.71      0.74    0.71      0.48 2.8    0.056
# 
# Item statistics 
#           n raw.r std.r r.cor r.drop mean  sd
# agree_1  91  0.73  0.77  0.71   0.55  5.7 1.1
# agree_2* 91  0.82  0.79  0.72   0.62  5.4 1.4
# agree_3  91  0.73  0.76  0.68   0.51  5.3 1.2
# agree_4* 91  0.72  0.70  0.60   0.49  5.7 1.2
# 
# Non missing response frequency for each item
#             1    2    3    4    5    6    7 miss
# agree_1  0.01 0.00 0.03 0.05 0.23 0.48 0.19    0
# agree_2* 0.02 0.02 0.08 0.09 0.16 0.44 0.19    0
# agree_3  0.01 0.00 0.08 0.12 0.29 0.37 0.13    0
# agree_4* 0.00 0.03 0.03 0.09 0.18 0.42 0.25    0

open <- subset(data, select = c("open_1", "open_2*", "open_3*", "open_4*"))
alpha(open)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.82      0.82    0.82      0.53 4.6 0.032  5.1 1.2
# 
# lower alpha upper     95% confidence boundaries
# 0.75 0.82 0.88 
# 
# Reliability if an item is dropped:
#         raw_alpha std.alpha G6(smc) average_r S/N alpha se
# open_1       0.80      0.81    0.77      0.58 4.2    0.037
# open_2*      0.71      0.71    0.66      0.45 2.4    0.053
# open_3*      0.81      0.81    0.75      0.59 4.4    0.034
# open_4*      0.75      0.76    0.74      0.51 3.1    0.048
# 
# Item statistics 
#          n raw.r std.r r.cor r.drop mean  sd
# open_1  91  0.77  0.76  0.65   0.57  5.1 1.6
# open_2* 91  0.88  0.89  0.86   0.77  5.3 1.4
# open_3* 91  0.74  0.75  0.66   0.54  5.0 1.5
# open_4* 91  0.83  0.83  0.75   0.68  5.2 1.6
# 
# Non missing response frequency for each item
#            1    2    3    4    5    6    7 miss
# open_1  0.03 0.05 0.09 0.09 0.26 0.24 0.23    0
# open_2* 0.01 0.05 0.05 0.10 0.27 0.31 0.20    0
# open_3* 0.01 0.08 0.08 0.16 0.25 0.27 0.14    0
# open_4* 0.02 0.05 0.09 0.10 0.24 0.27 0.22    0

neuro <- subset(data, select = c("neuro_1", "neuro_2*", "neuro_3", "neuro_4*"))
alpha(neuro)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.72      0.72    0.68      0.39 2.5 0.049  3.7 1.2
# 
# lower alpha upper     95% confidence boundaries
# 0.62 0.72 0.81 
# 
# Reliability if an item is dropped:
#          raw_alpha std.alpha G6(smc) average_r S/N alpha se
# neuro_1       0.56      0.56    0.46      0.30 1.3    0.080
# neuro_2*      0.70      0.70    0.65      0.44 2.4    0.055
# neuro_3       0.65      0.65    0.56      0.39 1.9    0.063
# neuro_4*      0.69      0.69    0.63      0.42 2.2    0.057
# 
# Item statistics 
#           n raw.r std.r r.cor r.drop mean  sd
# neuro_1  91  0.83  0.83  0.78   0.65  3.4 1.6
# neuro_2* 91  0.67  0.68  0.49   0.42  3.6 1.5
# neuro_3  91  0.73  0.73  0.63   0.50  3.5 1.6
# neuro_4* 91  0.71  0.70  0.53   0.45  4.2 1.6
# 
# Non missing response frequency for each item
#             1    2    3    4    5    6    7 miss
# neuro_1  0.10 0.27 0.20 0.11 0.23 0.05 0.03    0
# neuro_2* 0.03 0.22 0.31 0.11 0.20 0.10 0.03    0
# neuro_3  0.12 0.20 0.21 0.14 0.25 0.07 0.01    0
# neuro_4* 0.03 0.16 0.16 0.11 0.29 0.18 0.07    0

need.cog <- subset(data, select = c("cognition_1", "cognition_2", "cognition_3*", "cognition_4*", "cognition_5*", "cognition_6", "cognition_7*", "cognition_8*", "cognition_9*", "cognition_10", "cognition_11", "cognition_12*", "cognition_13", "cognition_14", "cognition_15", "cognition_16*", "cognition_17*", "cognition_18"))
alpha(need.cog)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
# 0.9      0.91    0.93      0.35 9.8 0.015  4.7 0.83
# 
# lower alpha upper     95% confidence boundaries
# 0.87 0.9 0.93 
# 
# Reliability if an item is dropped:
#               raw_alpha std.alpha G6(smc) average_r S/N alpha se
# cognition_1        0.90      0.90    0.92      0.35 9.2    0.016
# cognition_2        0.90      0.90    0.92      0.34 8.9    0.016
# cognition_3*       0.89      0.90    0.92      0.34 8.9    0.016
# cognition_4*       0.89      0.90    0.92      0.34 8.8    0.016
# cognition_5*       0.90      0.91    0.93      0.36 9.5    0.015
# cognition_6        0.90      0.90    0.92      0.35 9.3    0.016
# cognition_7*       0.90      0.91    0.93      0.36 9.5    0.015
# cognition_8*       0.90      0.91    0.93      0.36 9.7    0.015
# cognition_9*       0.89      0.90    0.92      0.34 8.9    0.016
# cognition_10       0.90      0.90    0.92      0.35 9.0    0.016
# cognition_11       0.90      0.90    0.92      0.35 9.0    0.016
# cognition_12*      0.90      0.90    0.92      0.35 9.3    0.016
# cognition_13       0.90      0.90    0.93      0.36 9.4    0.015
# cognition_14       0.90      0.90    0.92      0.35 9.1    0.016
# cognition_15       0.90      0.90    0.92      0.35 9.2    0.016
# cognition_16*      0.90      0.91    0.93      0.36 9.6    0.015
# cognition_17*      0.90      0.90    0.93      0.35 9.3    0.016
# cognition_18       0.90      0.90    0.92      0.36 9.4    0.015
# 
# Item statistics 
#                n raw.r std.r r.cor r.drop mean  sd
# cognition_1   91  0.62  0.62  0.60   0.56  4.5 1.2
# cognition_2   91  0.70  0.71  0.70   0.66  5.1 1.1
# cognition_3*  91  0.72  0.73  0.72   0.68  5.0 1.3
# cognition_4*  91  0.75  0.75  0.75   0.71  4.8 1.2
# cognition_5*  91  0.54  0.53  0.49   0.47  4.9 1.4
# cognition_6   91  0.60  0.59  0.57   0.53  4.1 1.5
# cognition_7*  91  0.55  0.53  0.50   0.47  4.3 1.6
# cognition_8*  91  0.50  0.48  0.44   0.42  3.9 1.5
# cognition_9*  91  0.73  0.72  0.71   0.68  4.0 1.5
# cognition_10  91  0.68  0.69  0.68   0.63  5.1 1.2
# cognition_11  91  0.68  0.70  0.69   0.64  5.0 1.2
# cognition_12* 91  0.59  0.59  0.56   0.53  5.3 1.3
# cognition_13  91  0.55  0.57  0.54   0.49  4.5 1.2
# cognition_14  91  0.66  0.67  0.65   0.61  5.1 1.3
# cognition_15  91  0.62  0.63  0.61   0.57  4.9 1.3
# cognition_16* 91  0.52  0.51  0.48   0.45  4.3 1.5
# cognition_17* 91  0.61  0.60  0.57   0.54  4.5 1.5
# cognition_18  91  0.57  0.57  0.54   0.50  4.8 1.4
# 
# Non missing response frequency for each item
#                  1    2    3    4    5    6    7 miss
# cognition_1   0.00 0.03 0.21 0.24 0.29 0.19 0.04    0
# cognition_2   0.00 0.02 0.08 0.16 0.34 0.32 0.08    0
# cognition_3*  0.00 0.02 0.11 0.19 0.31 0.24 0.13    0
# cognition_4*  0.00 0.04 0.09 0.21 0.36 0.24 0.05    0
# cognition_5*  0.01 0.05 0.14 0.08 0.36 0.26 0.09    0
# cognition_6   0.01 0.14 0.25 0.16 0.22 0.15 0.05    0
# cognition_7*  0.02 0.11 0.24 0.11 0.23 0.22 0.07    0
# cognition_8*  0.02 0.13 0.33 0.18 0.13 0.18 0.03    0
# cognition_9*  0.02 0.14 0.24 0.24 0.18 0.13 0.04    0
# cognition_10  0.00 0.03 0.08 0.15 0.30 0.33 0.11    0
# cognition_11  0.00 0.03 0.07 0.22 0.34 0.26 0.08    0
# cognition_12* 0.00 0.04 0.05 0.10 0.31 0.31 0.19    0
# cognition_13  0.00 0.07 0.13 0.26 0.35 0.13 0.05    0
# cognition_14  0.00 0.05 0.05 0.16 0.34 0.26 0.12    0
# cognition_15  0.00 0.03 0.09 0.26 0.29 0.21 0.12    0
# cognition_16* 0.01 0.15 0.15 0.18 0.25 0.22 0.03    0
# cognition_17* 0.02 0.09 0.16 0.12 0.35 0.18 0.08    0
# cognition_18  0.01 0.05 0.15 0.15 0.25 0.27 0.10    0

aomt <- subset(data, select = c("aomt_1", "aomt_2", "aomt_3", "aomt_4*", "aomt_5*", "aomt_6*", "aomt_7*", "aomt_8", "aomt_9"))
alpha(aomt)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
# 0.78      0.78    0.79      0.28 3.5 0.035  5.2 0.78
# 
# lower alpha upper     95% confidence boundaries
# 0.71 0.78 0.84 
# 
# Reliability if an item is dropped:
#         raw_alpha std.alpha G6(smc) average_r S/N alpha se
# aomt_1       0.76      0.76    0.77      0.28 3.1    0.038
# aomt_2       0.75      0.75    0.75      0.27 2.9    0.039
# aomt_3       0.75      0.75    0.76      0.27 2.9    0.040
# aomt_4*      0.75      0.75    0.76      0.27 3.0    0.039
# aomt_5*      0.78      0.78    0.79      0.31 3.6    0.034
# aomt_6*      0.73      0.73    0.74      0.26 2.8    0.043
# aomt_7*      0.75      0.75    0.77      0.28 3.1    0.038
# aomt_8       0.74      0.74    0.75      0.26 2.9    0.041
# aomt_9       0.78      0.78    0.79      0.31 3.6    0.034
# 
# Item statistics 
#          n raw.r std.r r.cor r.drop mean  sd
# aomt_1  91  0.57  0.58  0.50   0.43  5.0 1.2
# aomt_2  91  0.63  0.66  0.62   0.53  5.8 1.0
# aomt_3  91  0.64  0.65  0.60   0.52  5.4 1.2
# aomt_4* 91  0.64  0.64  0.59   0.51  5.7 1.4
# aomt_5* 91  0.43  0.42  0.30   0.27  4.3 1.3
# aomt_6* 91  0.73  0.71  0.69   0.60  4.3 1.5
# aomt_7* 91  0.61  0.61  0.54   0.47  5.8 1.3
# aomt_8  91  0.68  0.68  0.63   0.56  4.9 1.4
# aomt_9  91  0.44  0.44  0.32   0.27  5.1 1.3
# 
# Non missing response frequency for each item
#            1    2    3    4    5    6    7 miss
# aomt_1  0.00 0.01 0.11 0.22 0.29 0.26 0.11    0
# aomt_2  0.00 0.00 0.04 0.07 0.16 0.45 0.27    0
# aomt_3  0.00 0.02 0.04 0.14 0.27 0.31 0.21    0
# aomt_4* 0.01 0.01 0.08 0.08 0.14 0.36 0.32    0
# aomt_5* 0.00 0.05 0.29 0.16 0.34 0.11 0.04    0
# aomt_6* 0.01 0.09 0.26 0.18 0.20 0.18 0.09    0
# aomt_7* 0.01 0.02 0.04 0.08 0.08 0.43 0.34    0
# aomt_8  0.01 0.05 0.09 0.20 0.26 0.30 0.09    0
# aomt_9  0.00 0.05 0.05 0.19 0.31 0.26 0.13    0

######################################################################

## Study 1
## Figure 9

# Density plots

control <- subset(data, Accountability==0 & Environment==0)
non.random <- subset(data, Accountability==0 & Environment==1)
outcome.random <- subset(data, Accountability==1 & Environment==0)
process.random <- subset(data, Accountability==2 & Environment==0)
outcome.nonrandom <- subset(data, Accountability==1 & Environment==1)
process.nonrandom <- subset(data, Accountability==2 & Environment==1)
plot(density(data$R60))

# set up ideal distribution of scores

expected.value.random <- rnorm(10000, -15, 10.06)
optimal.bayesian.6040 <- rnorm(10000, 78, 10)
optimal.bayesian.4060 <- rnorm(10000, 48, 10)

# plot overall distributions

graphics.off()
par(mfrow=c(1,1))

density.labels.6040 <- c("Control", "Non Random / No Acc", "Outcome Random", "Process Random", 
                         "Outcome Non Random", "Process Non Random")

plot(density(control$R60), ylim=c(0,0.050), ylab="Probability Density", xlab="Score", col="black", main="", lwd=2.5,
     xlim=c(-90, 120))
title(main="Score Distribution by Condition", col.main="black")
lines(density(non.random$R60), col="blue", lwd=2.5)
lines(density(outcome.random$R60), col="red", lwd=2.5)
lines(density(process.random$R60), col="green", lwd=2.5)
lines(density(outcome.nonrandom$R60), col="yellow", lwd=2.5)
lines(density(process.nonrandom$R60), col="purple", lwd=2.5)
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.4060), col="brown", lwd=2.5)
legend("topleft",  c("Random / No Acc", "Non Random / No Acc", "Outcome Random", "Process Random", 
                     "Outcome Non Random", "Process Non Random", "Expected Random", "Optimal Bayesian"),
       lty=1, col=c('black', 'blue', 'red', 'green', 'yellow', 'purple', 'orange', 'brown'), bty='n', cex=.9)


### Study 1 Figure 10
### Six panel plots

par(mfrow=c(2,3))
plot(density(control$R60), ylim=c(0,0.050), 
     ylab="Probability Density", xlab="Score", col="black", main="Random No Acc",
     xlim=c(-90, 120), lwd=2.5)
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.4060), col="brown", lwd=2.5)

plot(density(outcome.random$R60), ylim=c(0,0.050), 
     ylab="Probability Density", xlab="Score", col="red", main="Outcome Random",
     xlim=c(-90, 120), lwd=2.5)
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.4060), col="brown", lwd=2.5)

plot(density(process.random$R60), ylim=c(0,0.050), 
     ylab="Probability Density", xlab="Score", col="green", main="Process Random",
     xlim=c(-90, 120), lwd=2.5)
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.4060), col="brown", lwd=2.5)

plot(density(non.random$R60), ylim=c(0,0.050), 
     ylab="Probability Density", xlab="Score", col="blue", main="Non Random No Acc",
     xlim=c(-90, 120), lwd=2.5)
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.4060), col="brown", lwd=2.5)

plot(density(outcome.nonrandom$R60), ylim=c(0,0.050), 
     ylab="Probability Density", xlab="Score", col="yellow", main="Outcome Non Random",
     xlim=c(-90, 120), lwd=2.5)
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.4060), col="brown", lwd=2.5)

plot(density(process.nonrandom$R60), ylim=c(0,0.050), 
     ylab="Probability Density", xlab="Score", col="purple", main="Process Non Random",
     xlim=c(-90, 120), lwd=2.5)
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.4060), col="brown", lwd=2.5)

summary(lm(R60~ Environment + Accountability + aomt.avg + Environment * Accountability + Environment * Accountability * aomt.avg, data))
