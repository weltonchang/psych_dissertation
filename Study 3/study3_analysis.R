#### Study 3 analysis
####

data <- X_02_CLEAN_Accountability_Political_Ideology_Need_for_Closure_and_Policy_Preferences

head(data)

## clean data

newdata <- subset(data, uncertainty_1 > 0 | uncertainty_2 > 0, 
                  select=c(userid, account, uncertainty_1, uncertainty_2, likelihood_e, 
                           reduc_e, likelihood_s, reduc_s, pol.party))

newdata <- subset(newdata, uncertainty_2 > 0, 
                  select=c(userid, account, uncertainty_1, uncertainty_2, likelihood_e, 
                           reduc_e, likelihood_s, reduc_s, pol.party))

library(plyr)

newdata <- data.frame(newdata)
newdata <- na.omit(newdata)

#### Set Variables

# Accountability
newdata$account <- as.factor(newdata$account)
newdata$account <- ordered(newdata$account,
                     levels = c(0,1),
                     labels = c("No Account", "Account"))

# uncertainty

newdata$uncertainty_1 <- as.factor(newdata$uncertainty_1)
newdata$uncertainty_1 <- ordered(newdata$uncertainty_1, 
                                 levels = c(1,2,3),
                                 labels = c("Low", "Medium", "High"))


newdata$uncertainty_2 <- as.factor(newdata$uncertainty_2)
newdata$uncertainty_2 <- ordered(newdata$uncertainty_2, 
                                 levels = c(1,2,3),
                                 labels = c("Low", "Medium", "High"))

# political party

require("car")    

recode(newdata$pol.party, "4:7=4")
newdata$pol.party <- as.factor(newdata$pol.party)

newdata$pol.party <- ordered(newdata$pol.party, 
                                 levels = c(1,2,3,4),
                                 labels = c("Dem", "Repub", "Ind", "Other"))

newdata <- subset(newdata, pol.party != 'Other')

# descriptive


ddply(newdata, c("account", "uncertainty_1", "uncertainty_2"), summarise, 
      N = length(userid),
      LikeE = mean(likelihood_e),
      MeanEReduc = mean(reduc_e),
      LikeS = mean(likelihood_s),
      MeanSReduc = mean(reduc_s))

ddply(newdata, c("account", "uncertainty_1"), summarise, 
      N = length(userid),
      LikeE = mean(likelihood_e),
      MeanEReduc = mean(reduc_e),
      LikeS = mean(likelihood_s),
      MeanSReduc = mean(reduc_s))

ddply(newdata, c("account", "uncertainty_2"), summarise, 
      N = length(userid),
      LikeE = mean(likelihood_e),
      MeanEReduc = mean(reduc_e),
      LikeS = mean(likelihood_s),
      MeanSReduc = mean(reduc_s))

ddply(newdata, c("account", "pol.party", "uncertainty_1"), summarise, 
      N = length(userid),
      LikeE = mean(likelihood_e),
      MeanEReduc = mean(reduc_e))

ddply(newdata, c("account", "pol.party", "uncertainty_2"), summarise, 
      N = length(userid),
      LikeS = mean(likelihood_s),
      MeanSReduc = mean(reduc_s))

ddply(newdata, c("pol.party", "uncertainty_1"), summarise, 
      N = length(userid),
      LikeE = mean(likelihood_e),
      MeanEReduc = mean(reduc_e))

ddply(newdata, c("pol.party", "uncertainty_2"), summarise, 
      N = length(userid),
      LikeS = mean(likelihood_s),
      MeanSReduc = mean(reduc_s))



###########

# Real descriptives

ddply(newdata, c("account", "pol.party", "uncertainty_1"), summarise, 
      N = length(userid),
      LikeE = mean(likelihood_e),
      MeanEReduc = mean(reduc_e))

ddply(newdata, c("account", "pol.party", "uncertainty_2"), summarise, 
      N = length(userid),
      LikeS = mean(likelihood_s),
      MeanSReduc = mean(reduc_s))


########## Regressions and ANOVAs (most interactions are not significant)

like_e <- lm(likelihood_e ~ account + uncertainty_1 + pol.party, data=newdata)
summary(like_e)
  
like_s <- lm(likelihood_s ~ account + uncertainty_2 + pol.party, data=newdata)
summary(like_s)

# testing H4

### Likelihood
test.le <- aov(likelihood_e ~ account + uncertainty_1 + pol.party + pol.party*account, data=newdata)
summary(test.le)

test.ls <- aov(likelihood_s ~ account + uncertainty_2 + pol.party + pol.party*account, data=newdata)
summary(test.ls)

test.le2 <- aov(likelihood_e ~ account + uncertainty_1 + pol.party + pol.party*uncertainty_1, data=newdata)
summary(test.le2)

test.ls2 <- aov(likelihood_s ~ account + uncertainty_2 + pol.party + pol.party*uncertainty_2, data=newdata)
summary(test.ls2)

### Reduction in Uncertainty

### Testing H1

test.re.pol <- aov(reduc_e ~ uncertainty_1 + pol.party, data=newdata)
summary(test.re.pol)

test.re.pol2 <- aov(reduc_s ~ uncertainty_2 + pol.party, data=newdata)
summary(test.re.pol2)

### Testing H2 and H3

test.re <- aov(reduc_e ~ account + uncertainty_1 + pol.party + pol.party*account, data=newdata) #ns
summary(test.re)

test.rs <- aov(reduc_s ~ account + uncertainty_2 + pol.party + pol.party*account, data=newdata) #ns
summary(test.rs)

test.re.lm <- lm(reduc_e ~ account + uncertainty_1 + pol.party + pol.party*account, data=newdata) #ns
summary(test.re.lm)

test.rs.lm <- lm(reduc_s ~ account + uncertainty_2 + pol.party + pol.party*account, data=newdata) #ns
summary(test.rs.lm)

#### Ensuring that uncertainty level didn't cause the effects

test.re2 <- aov(reduc_e ~ account + uncertainty_1 + pol.party + pol.party*uncertainty_1, data=newdata) #ns
summary(test.re2)

test.rs2 <- aov(reduc_s ~ account + uncertainty_2 + pol.party + pol.party*uncertainty_2, data=newdata) #ns
summary(test.rs2)
