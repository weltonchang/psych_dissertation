### Study 1, Experiment 1 ###
### Sub-Study (60-40) Complete ###
### welton@sas.upenn.edu ###
# last updated: 26 Nov 2017 #

options(scipen = 666)

#################
### Libraries ###
#################

require(alpha)
require(plyr)
require(grid)
require(ggplot2)
library(plyr)
library(grid)
library(ggplot2)

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

data <- X20160217_study2_LQ_4060
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
# t = 1.1037, df = 96.258, p-value = 0.2725
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2105364  0.7379703
# sample estimates:
#   mean of x mean of y 
# 2.610656  2.346939 



### Summary Statistics ###

# Score
ddply(data, c("Environment", "Accountability"), summarise,
      N    = length(R60),
      mean = mean(R60),
      sd   = sd(R60),
      se   = sd / sqrt(N))
# ---------------------------------------------------------
# Environment Accountability  N       mean       sd       se
#           0              0 43 -16.662791 22.85506 3.485364
#           0              1 38 -18.618421 24.20337 3.926305
#           0              2 27 -15.407407 19.88913 3.827666
#           1              0 25   2.940000 28.21174 5.642349
#           1              1 23   6.934783 30.41355 6.341664
#           1              2 22  11.045455 27.27414 5.814866

# Moves
ddply(data, c("Environment", "Accountability"), summarise,
      N    = length(Moves),
      mean = mean(Moves),
      sd   = sd(Moves),
      se   = sd / sqrt(N))
# ---------------------------------------------------------
# Environment Accountability  N     mean       sd       se
#           0              0 43 25.81395 15.85388 2.417694
#           0              1 38 22.47368 14.59866 2.368216
#           0              2 27 22.40741 15.63590 3.009131
#           1              0 25 21.24000 13.09351 2.618702
#           1              1 23 15.73913 10.15434 2.117327
#           1              2 22 15.54545 13.63024 2.905978

# Hits
ddply(data, c("Environment", "Accountability"), summarise,
      N    = length(Hits),
      mean = mean(Hits),
      sd   = sd(Hits),
      se   = sd / sqrt(N))
# ---------------------------------------------------------
# Environment Accountability  N     mean       sd       se
#           0              0 43 17.90698 6.725268 1.025594
#           0              1 38 17.31579 6.830398 1.108037
#           0              2 27 16.88889 5.819948 1.120050
#           1              0 25 23.88000 8.348253 1.669651
#           1              1 23 25.17391 8.896640 1.855078
#           1              2 22 25.54545 7.896517 1.683543

# Misses
ddply(data, c("Environment", "Accountability"), summarise,
      N    = length(Misses),
      mean = mean(Misses),
      sd   = sd(Misses),
      se   = sd / sqrt(N))
# ---------------------------------------------------------
# Environment Accountability  N     mean       sd       se
#           0              0 43 42.09302 6.725268 1.025594
#           0              1 38 42.68421 6.830398 1.108037
#           0              2 27 43.11111 5.819948 1.120050
#           1              0 25 36.12000 8.348253 1.669651
#           1              1 23 34.82609 8.896640 1.855078
#           1              2 22 34.45455 7.896517 1.683543



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
env_anova <- aov(R60 ~ Environment, data=data)
posthoc_R60 <- TukeyHSD(x=env_anova, data$Environment, conf.level=0.95)
summary(posthoc_R60)
plot(posthoc_R60)

summary(aov(Moves ~ Environment, data=data)) #s
summary(aov(Hits ~ Environment, data=data)) #s
summary(aov(Misses ~ Environment, data=data)) #s
summary(aov(CommentCount ~ Environment, data=data)) #ns
summary(aov(CommentAvgLen ~ Environment, data=data)) #ns

# Accountability + Environment
summary(aov(R60 ~ Accountability + Environment, data=data)) #s (env)

acc_env_anova <- aov(R60 ~ Accountability + Environment + Accountability * Environment, data=data)
summary(acc_env_anova)
posthoc_R60_3 <- TukeyHSD(x=acc_env_anova, conf.level=0.95)
summary(posthoc_R60_3)
plot(posthoc_R60_3)

summary(aov(Moves ~ Accountability + Environment, data=data)) #s (env)
summary(aov(Hits ~ Accountability + Environment, data=data)) #s (env)
summary(aov(Misses ~ Accountability + Environment, data=data)) #s (env)
summary(aov(CommentCount ~ Accountability + Environment, data=data)) #s (acc)
summary(aov(CommentAvgLen ~ Accountability + Environment, data=data)) #s (acc)

# Accountability * Environment
summary(aov(R60 ~ Accountability * Environment, data=data)) #s (env)

int_anova <- aov(R60 ~ Accountability * Environment, data=data)
posthoc_R60_2 <- TukeyHSD(x=int_anova, conf.level=0.95)
summary(posthoc_R60_2)
plot(posthoc_R60_2)

summary(aov(Moves ~ Accountability * Environment, data=data)) #s (env)
summary(aov(Hits ~ Accountability * Environment, data=data)) #s (env)
summary(aov(Misses ~ Accountability * Environment, data=data)) #s (env)
summary(aov(CommentCount ~ Accountability * Environment, data=data)) #s (acc; acc:env)
summary(aov(CommentAvgLen ~ Accountability * Environment, data=data)) #s (acc)

summary(aov(CommentAvgLen ~ Environment + aomt.avg, data=data))

### Wilcox Tests ###

# Score
wilcox.test(acc1$R60, acc2$R60, paired = FALSE)
# W = 1274.5, p-value = 0.1868
# alternative hypothesis: true location shift is not equal to 0

# Moves
wilcox.test(acc1$Moves, acc2$Moves, paired = FALSE)
# W = 1553.5, p-value = 0.7248
# alternative hypothesis: true location shift is not equal to 0

# Hits
wilcox.test(acc1$Hits, acc2$Hits, paired = FALSE)
# W = 1414.5, p-value = 0.6321
# alternative hypothesis: true location shift is not equal to 0

# Misses
wilcox.test(acc1$Misses, acc2$Misses, paired = FALSE)
# W = 1574.5, p-value = 0.6321
# alternative hypothesis: true location shift is not equal to 0



### Anova copies of Wilcox tests ###

# Create dataframe with only observations where accountability= 1 or 2
only_acc_obs <- rbind(acc1, acc2)

# Score
summary(aov(R60 ~ Accountability, data=only_acc_obs))
#                 Df Sum Sq Mean Sq F value Pr(>F)
# Accountability   1    808   808.0   1.017  0.315
# Residuals      108  85769   794.2 

# Moves
summary(aov(Moves ~ Accountability, data=only_acc_obs))
#                 Df Sum Sq Mean Sq F value Pr(>F)
# Accountability   1     10   10.04    0.05  0.823
# Residuals      108  21633  200.30 

# Hits
summary(aov(Hits ~ Accountability, data=only_acc_obs))
#                 Df Sum Sq Mean Sq F value Pr(>F)
# Accountability   1      7    6.71   0.097  0.756
# Residuals      108   7451   68.99

# Misses
summary(aov(Misses ~ Accountability, data=only_acc_obs))
#                 Df Sum Sq Mean Sq F value Pr(>F)
# Accountability   1      7    6.71   0.097  0.756
# Residuals      108   7451   68.99 

###############
## Graphics ###
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

# Study 1 Figure 5

score_after_each_round <- ggplot(data=per_round_data) + 
  theme_bw() +
  geom_line(aes(x=trials, y=env0_acc0, colour="1"), size = 2) + 
  geom_line(aes(x=trials, y=env0_acc1, colour="2"), size = 2) + 
  geom_line(aes(x=trials, y=env0_acc2, colour="3"), size = 2) + 
  geom_line(aes(x=trials, y=env1_acc0, colour="4"), size = 2) + 
  geom_line(aes(x=trials, y=env1_acc1, colour="5"), size = 2) + 
  geom_line(aes(x=trials, y=env1_acc2, colour="6"), size = 2) + 
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),  
        legend.position="bottom", legend.title=element_blank()) +
  labs(x = "Trial #", y = "Total Score", title= "Experiment 1a Scores Across Conditions", color = "Conditions") +
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

# Graph 2 # Unused in Text
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
# 0.78      0.79    0.75      0.48 3.7 0.026  4.5 1.2
# 
# lower alpha upper     95% confidence boundaries
# 0.73 0.78 0.83 
# 
# Reliability if an item is dropped:
#          raw_alpha std.alpha G6(smc) average_r S/N alpha se
# consc_1       0.70      0.71    0.63      0.44 2.4    0.036
# consc_2*      0.75      0.76    0.69      0.51 3.2    0.030
# consc_3       0.76      0.77    0.70      0.52 3.3    0.031
# consc_4*      0.67      0.70    0.61      0.43 2.3    0.041
# 
# Item statistics 
#            n raw.r std.r r.cor r.drop mean  sd
# consc_1  178  0.82  0.81  0.73   0.63  4.1 1.7
# consc_2* 178  0.77  0.75  0.62   0.55  4.2 1.7
# consc_3  178  0.68  0.74  0.60   0.53  5.5 1.1
# consc_4* 178  0.84  0.82  0.76   0.68  4.4 1.6
# 
# Non missing response frequency for each item
#             1    2    3    4    5    6    7 miss
# consc_1  0.07 0.16 0.15 0.07 0.33 0.17 0.05    0
# consc_2* 0.04 0.12 0.30 0.08 0.13 0.25 0.07    0
# consc_3  0.01 0.01 0.06 0.07 0.33 0.38 0.16    0
# consc_4* 0.04 0.09 0.20 0.16 0.22 0.19 0.10    0

extra <- subset(data, select = c("extro_1", "extro_2*", "extro_3", "extro_4*"))
alpha(extra)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.84      0.84     0.8      0.57 5.3 0.019  4.2 1.3
# 
# lower alpha upper     95% confidence boundaries
# 0.8 0.84 0.88 
# 
# Reliability if an item is dropped:
#          raw_alpha std.alpha G6(smc) average_r S/N alpha se
# extro_1       0.80      0.80    0.74      0.58 4.1    0.026
# extro_2*      0.80      0.80    0.73      0.57 3.9    0.026
# extro_3       0.82      0.82    0.76      0.60 4.6    0.023
# extro_4*      0.77      0.78    0.70      0.54 3.5    0.029
# 
# Item statistics 
#            n raw.r std.r r.cor r.drop mean  sd
# extro_1  178  0.81  0.82  0.73   0.67  3.9 1.5
# extro_2* 178  0.83  0.83  0.75   0.68  4.5 1.6
# extro_3  178  0.80  0.79  0.68   0.63  4.2 1.7
# extro_4* 178  0.86  0.86  0.80   0.73  4.2 1.6
# 
# Non missing response frequency for each item
#             1    2    3    4    5    6    7 miss
# extro_1  0.04 0.19 0.17 0.23 0.23 0.12 0.02    0
# extro_2* 0.02 0.10 0.20 0.14 0.21 0.22 0.11    0
# extro_3  0.06 0.16 0.15 0.15 0.23 0.20 0.06    0
# extro_4* 0.03 0.12 0.21 0.20 0.19 0.17 0.08    0

agree <- subset(data, select = c("agree_1", "agree_2*", "agree_3", "agree_4*"))
alpha(agree)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean sd
# 0.83      0.83    0.82      0.56 5.1 0.021  5.5  1
# 
# lower alpha upper     95% confidence boundaries
# 0.79 0.83 0.87 
# 
# Reliability if an item is dropped:
#          raw_alpha std.alpha G6(smc) average_r S/N alpha se
# agree_1       0.78      0.78    0.73      0.54 3.5    0.028
# agree_2*      0.76      0.77    0.71      0.53 3.3    0.032
# agree_3       0.81      0.82    0.76      0.60 4.4    0.023
# agree_4*      0.79      0.80    0.73      0.57 4.0    0.028
# 
# Item statistics 
#            n raw.r std.r r.cor r.drop mean  sd
# agree_1  178  0.81  0.83  0.76   0.69  5.8 1.1
# agree_2* 178  0.86  0.85  0.79   0.72  5.3 1.4
# agree_3  178  0.78  0.78  0.68   0.60  5.3 1.3
# agree_4* 178  0.82  0.81  0.73   0.66  5.5 1.3
# 
# Non missing response frequency for each item
#             1    2    3    4    5    6    7 miss
# agree_1  0.00 0.01 0.04 0.04 0.25 0.41 0.25    0
# agree_2* 0.00 0.02 0.15 0.08 0.17 0.40 0.19    0
# agree_3  0.01 0.03 0.06 0.10 0.31 0.33 0.16    0
# agree_4* 0.00 0.01 0.12 0.08 0.16 0.38 0.24    0

open <- subset(data, select = c("open_1", "open_2*", "open_3*", "open_4*"))
alpha(open)
# raw_alpha std.alpha G6(smc) average_r S/N  ase mean  sd
# 0.76      0.75    0.76      0.44 3.1 0.03  4.9 1.1
# 
# lower alpha upper     95% confidence boundaries
# 0.7 0.76 0.82 
# 
# Reliability if an item is dropped:
#         raw_alpha std.alpha G6(smc) average_r S/N alpha se
# open_1       0.77      0.77    0.69      0.52 3.3    0.031
# open_2*      0.66      0.66    0.66      0.39 1.9    0.044
# open_3*      0.74      0.74    0.68      0.48 2.8    0.034
# open_4*      0.62      0.61    0.58      0.34 1.6    0.050
# 
# Item statistics 
#           n raw.r std.r r.cor r.drop mean  sd
# open_1  178  0.66  0.67  0.55   0.43  5.1 1.4
# open_2* 178  0.81  0.80  0.71   0.62  4.8 1.5
# open_3* 178  0.72  0.71  0.60   0.49  4.5 1.5
# open_4* 178  0.85  0.85  0.80   0.70  5.0 1.5
# 
# Non missing response frequency for each item
#            1    2    3    4    5    6    7 miss
# open_1  0.01 0.04 0.11 0.11 0.29 0.29 0.15    0
# open_2* 0.02 0.06 0.15 0.11 0.28 0.27 0.11    0
# open_3* 0.02 0.08 0.17 0.20 0.21 0.27 0.05    0
# open_4* 0.02 0.04 0.15 0.11 0.21 0.34 0.13    0

neuro <- subset(data, select = c("neuro_1", "neuro_2*", "neuro_3", "neuro_4*"))
alpha(neuro)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.68      0.68    0.66      0.35 2.1 0.039  3.8 1.2
# 
# lower alpha upper     95% confidence boundaries
# 0.61 0.68 0.76 
# 
# Reliability if an item is dropped:
#          raw_alpha std.alpha G6(smc) average_r S/N alpha se
# neuro_1       0.52      0.53    0.44      0.27 1.1    0.063
# neuro_2*      0.63      0.62    0.59      0.35 1.6    0.048
# neuro_3       0.59      0.60    0.50      0.33 1.5    0.053
# neuro_4*      0.70      0.69    0.63      0.43 2.2    0.039
# 
# Item statistics 
#            n raw.r std.r r.cor r.drop mean  sd
# neuro_1  178  0.81  0.79  0.74   0.60  3.7 1.7
# neuro_2* 178  0.68  0.71  0.53   0.44  3.7 1.5
# neuro_3  178  0.75  0.73  0.64   0.50  3.7 1.7
# neuro_4* 178  0.61  0.63  0.41   0.33  4.2 1.5
# 
# Non missing response frequency for each item
#             1    2    3    4    5    6    7 miss
# neuro_1  0.07 0.29 0.15 0.12 0.16 0.19 0.03    0
# neuro_2* 0.03 0.22 0.24 0.15 0.21 0.15 0.00    0
# neuro_3  0.09 0.23 0.14 0.15 0.21 0.15 0.03    0
# neuro_4* 0.03 0.14 0.17 0.16 0.29 0.15 0.06    0

need.cog <- subset(data, select = c("cognition_1", "cognition_2", "cognition_3*", "cognition_4*", "cognition_5*", "cognition_6", "cognition_7*", "cognition_8*", "cognition_9*", "cognition_10", "cognition_11", "cognition_12*", "cognition_13", "cognition_14", "cognition_15", "cognition_16*", "cognition_17*", "cognition_18"))
alpha(need.cog)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
# 0.88      0.88     0.9      0.29 7.2 0.013  4.5 0.78
# 
# lower alpha upper     95% confidence boundaries
# 0.85 0.88 0.9 
# 
# Reliability if an item is dropped:
#               raw_alpha std.alpha G6(smc) average_r S/N alpha se
# cognition_1        0.87      0.87    0.90      0.28 6.8    0.014
# cognition_2        0.87      0.87    0.89      0.28 6.7    0.014
# cognition_3*       0.87      0.87    0.89      0.28 6.5    0.015
# cognition_4*       0.87      0.87    0.89      0.28 6.6    0.015
# cognition_5*       0.87      0.87    0.90      0.29 6.9    0.014
# cognition_6        0.87      0.87    0.90      0.29 6.9    0.014
# cognition_7*       0.87      0.87    0.89      0.28 6.6    0.015
# cognition_8*       0.88      0.88    0.90      0.30 7.3    0.013
# cognition_9*       0.87      0.87    0.90      0.29 6.9    0.014
# cognition_10       0.86      0.87    0.89      0.27 6.4    0.015
# cognition_11       0.87      0.87    0.89      0.28 6.6    0.014
# cognition_12*      0.87      0.87    0.90      0.29 6.8    0.014
# cognition_13       0.87      0.87    0.90      0.29 6.9    0.014
# cognition_14       0.88      0.88    0.90      0.30 7.1    0.014
# cognition_15       0.87      0.87    0.90      0.29 6.9    0.014
# cognition_16*      0.87      0.87    0.90      0.29 7.0    0.014
# cognition_17*      0.87      0.87    0.90      0.29 6.8    0.014
# cognition_18       0.88      0.88    0.90      0.30 7.4    0.013
# 
# Item statistics 
#                 n raw.r std.r r.cor r.drop mean  sd
# cognition_1   178  0.59  0.60  0.58   0.53  4.4 1.3
# cognition_2   178  0.60  0.61  0.59   0.54  4.8 1.3
# cognition_3*  178  0.70  0.70  0.69   0.65  4.7 1.4
# cognition_4*  178  0.69  0.68  0.67   0.63  4.6 1.4
# cognition_5*  178  0.55  0.54  0.51   0.47  4.7 1.5
# cognition_6   178  0.53  0.54  0.50   0.45  4.1 1.4
# cognition_7*  178  0.66  0.65  0.64   0.60  4.4 1.5
# cognition_8*  178  0.41  0.40  0.35   0.32  3.8 1.3
# cognition_9*  178  0.57  0.56  0.52   0.49  3.9 1.4
# cognition_10  178  0.73  0.74  0.73   0.69  4.9 1.3
# cognition_11  178  0.66  0.68  0.66   0.61  4.9 1.2
# cognition_12* 178  0.58  0.57  0.55   0.51  5.0 1.4
# cognition_13  178  0.55  0.56  0.53   0.48  4.3 1.3
# cognition_14  178  0.44  0.45  0.40   0.36  4.7 1.3
# cognition_15  178  0.55  0.56  0.53   0.48  4.8 1.3
# cognition_16* 178  0.52  0.51  0.47   0.44  4.0 1.4
# cognition_17* 178  0.59  0.58  0.55   0.51  4.4 1.5
# cognition_18  178  0.35  0.36  0.29   0.26  4.7 1.4
# 
# Non missing response frequency for each item
#                  1    2    3    4    5    6    7 miss
# cognition_1   0.01 0.06 0.22 0.23 0.26 0.18 0.04    0
# cognition_2   0.00 0.04 0.17 0.16 0.32 0.23 0.08    0
# cognition_3*  0.00 0.08 0.12 0.17 0.35 0.17 0.10    0
# cognition_4*  0.01 0.07 0.18 0.18 0.28 0.23 0.06    0
# cognition_5*  0.01 0.06 0.16 0.19 0.21 0.27 0.09    0
# cognition_6   0.03 0.13 0.19 0.23 0.25 0.13 0.03    0
# cognition_7*  0.01 0.10 0.25 0.14 0.24 0.21 0.05    0
# cognition_8*  0.03 0.12 0.29 0.22 0.22 0.10 0.01    0
# cognition_9*  0.02 0.13 0.31 0.17 0.22 0.10 0.04    0
# cognition_10  0.01 0.03 0.11 0.21 0.29 0.27 0.09    0
# cognition_11  0.00 0.03 0.12 0.16 0.31 0.30 0.07    0
# cognition_12* 0.01 0.04 0.12 0.11 0.30 0.29 0.13    0
# cognition_13  0.01 0.08 0.17 0.29 0.27 0.16 0.03    0
# cognition_14  0.00 0.06 0.13 0.22 0.28 0.26 0.05    0
# cognition_15  0.02 0.02 0.15 0.21 0.28 0.25 0.07    0
# cognition_16* 0.05 0.11 0.21 0.25 0.23 0.11 0.03    0
# cognition_17* 0.00 0.10 0.24 0.14 0.28 0.17 0.08    0
# cognition_18  0.01 0.07 0.17 0.13 0.33 0.22 0.07    0

aomt <- subset(data, select = c("aomt_1", "aomt_2", "aomt_3", "aomt_4*", "aomt_5*", "aomt_6*", "aomt_7*", "aomt_8", "aomt_9"))
alpha(aomt)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
# 0.76      0.76    0.79      0.26 3.1 0.028    5 0.73
# 
# lower alpha upper     95% confidence boundaries
# 0.7 0.76 0.81 
# 
# Reliability if an item is dropped:
#         raw_alpha std.alpha G6(smc) average_r S/N alpha se
# aomt_1       0.75      0.75    0.78      0.27 3.0    0.028
# aomt_2       0.71      0.71    0.75      0.23 2.5    0.033
# aomt_3       0.72      0.72    0.76      0.24 2.6    0.032
# aomt_4*      0.73      0.73    0.76      0.25 2.7    0.031
# aomt_5*      0.75      0.76    0.79      0.28 3.1    0.029
# aomt_6*      0.73      0.73    0.77      0.25 2.7    0.032
# aomt_7*      0.72      0.72    0.74      0.24 2.6    0.033
# aomt_8       0.74      0.74    0.77      0.26 2.8    0.029
# aomt_9       0.75      0.75    0.78      0.27 3.0    0.029
# 
# Item statistics 
#           n raw.r std.r r.cor r.drop mean  sd
# aomt_1  178  0.49  0.50  0.41   0.33  4.6 1.3
# aomt_2  178  0.68  0.70  0.67   0.58  5.7 1.1
# aomt_3  178  0.65  0.66  0.61   0.52  5.3 1.2
# aomt_4* 178  0.61  0.60  0.56   0.47  5.6 1.3
# aomt_5* 178  0.46  0.46  0.33   0.30  4.0 1.2
# aomt_6* 178  0.63  0.61  0.55   0.48  4.4 1.4
# aomt_7* 178  0.68  0.66  0.65   0.53  5.6 1.4
# aomt_8  178  0.56  0.56  0.49   0.40  4.8 1.3
# aomt_9  178  0.47  0.50  0.39   0.32  5.0 1.1
# 
# Non missing response frequency for each item
#            1    2    3    4    5    6    7 miss
# aomt_1  0.01 0.07 0.08 0.34 0.25 0.20 0.06    0
# aomt_2  0.01 0.00 0.02 0.12 0.21 0.38 0.26    0
# aomt_3  0.01 0.02 0.05 0.16 0.29 0.32 0.16    0
# aomt_4* 0.00 0.02 0.08 0.07 0.21 0.37 0.25    0
# aomt_5* 0.01 0.11 0.26 0.24 0.29 0.08 0.01    0
# aomt_6* 0.00 0.11 0.17 0.21 0.30 0.12 0.08    0
# aomt_7* 0.00 0.04 0.07 0.12 0.13 0.34 0.30    0
# aomt_8  0.01 0.04 0.10 0.25 0.30 0.18 0.12    0
# aomt_9  0.00 0.02 0.07 0.27 0.31 0.25 0.08    0


##################################################
###          More Figures start here           ###
##################################################

# Study 1 / Figure 3

graphics.off()
x   <- seq(-90,120,length=10000)
y   <- dnorm(x,mean=-15, sd=10.06)
plot(x,y, type="l", lwd=1, ylab="Probability Density", xlab="Score", xlim=c(-70, 90), cex.lab=1.25)


# Study 1 / Figure 5
# Density plots

process.only <- subset(data, Accountability==2)
head(process.only)
summary(lm(R60~aomt.avg, process.only))
summary(lm(R60~Accountability * aomt.avg, data))

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

# label distributions
density.labels.6040 <- c("Control", "Non Random / No Acc", "Outcome Random", "Process Random", 
                         "Outcome Non Random", "Process Non Random")

# reset grahical parameters
graphics.off()
par(mfrow=c(1,1))

# 
plot(density(control$R60), ylim=c(0,0.050), ylab="Probability Density", xlab="Total Score", 
     col="black", main="", lwd=2.5,
     xlim=c(-90, 120))
title(main="Score Distribution by Condition", col.main="black", lwd=2.5)
lines(density(non.random$R60), col="blue", lwd=2.5)
lines(density(outcome.random$R60), col="red", lwd=2.5)
lines(density(process.random$R60), col="green", lwd=2.5)
lines(density(outcome.nonrandom$R60), col="yellow", lwd=2.5)
lines(density(process.nonrandom$R60), col="purple", lwd=2.5)
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.6040), col="brown", lwd=2.5)
legend("topleft",  c("Random / No Acc", "Non Random / No Acc", "Outcome Random", "Process Random", 
                      "Outcome Non Random", "Process Non Random", "Expected Random", "Optimal Bayesian"),
       lty=1, col=c('black', 'blue', 'red', 'green', 'yellow', 'purple', 'orange', 'brown'), bty='n', cex=0.9)


### Study 1, Figure 6
### Six panel plots

# set graphical output parameters
graphics.off()
par(mfrow=c(2,3))


# each plot

plot(density(control$R60), ylim=c(0,0.050), 
     ylab="Probability Density", xlab="Total Score", col="black", main="Random No Acc", lwd=2.5,
     xlim=c(-90, 120))
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.6040), col="brown", lwd=2.5)

plot(density(outcome.random$R60), ylim=c(0,0.050), 
     ylab="Probability Density", xlab="Total Score", col="red", main="Random Outcome", lwd=2.5,
     xlim=c(-90, 120))
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.6040), col="brown", lwd=2.5)

plot(density(process.random$R60), ylim=c(0,0.050), 
     ylab="Probability Density", xlab="Total Score", col="green", main="Random Process", lwd=2.5,
     xlim=c(-90, 120))
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.6040), col="brown", lwd=2.5)

plot(density(non.random$R60), ylim=c(0,0.050), 
     ylab="Probability Density", xlab="Total Score", col="blue", main="Non Random No Acc", lwd=2.5,
     xlim=c(-90, 120))
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.6040), col="brown", lwd=2.5)

plot(density(outcome.nonrandom$R60), ylim=c(0,0.050), 
     ylab="Probability Density", xlab="Total Score", col="yellow", main="Non Random Outcome",lwd=2.5,
     xlim=c(-90, 120))
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.6040), col="brown", lwd=2.5)

plot(density(process.nonrandom$R60), ylim=c(0,0.050), 
     ylab="Probability Density", xlab="Total Score", col="purple", main="Non Random Process", lwd=2.5,
     xlim=c(-90, 120))
lines(density(expected.value.random), col="orange", lwd=2.5)
lines(density(optimal.bayesian.6040), col="brown", lwd=2.5)
