library(ggplot2)
library(plyr)
library(psych)
library(grid)

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

#####################################

options(scipen = 999)
options(digits = 5)

#####################################

data <- bayes_freedonia
rm(bayes_freedonia)

LC <- data[which(data$certainty == 0),]
HC <- data[which(data$certainty == 1),]

head(LC)
head(HC)

#####################################

data$certainty <- as.factor(data$certainty)
data$account <- as.factor(data$account)

#####################################


#############################
##### Descriptive Stats #####
#############################

count(data$educ)
# x freq
# 2   53
# 3  263
# 4    5
# 5   44
# 7    1

count(data$age)
# x freq
# 1  365
# 5    1

count(data$gender)
# x freq
# 1  149
# 2  217

count(data$race)
# x freq
# 1  187
# 2   23
# 3   37
# 4  107
# 6    2
# 7   17


##################
##### Alphas #####
##################

consc_vars = c("consc_1", "consc_2*", "consc_3", "consc_4*")
extra_vars <- c("extra_1", "extra_2*", "extra_3", "extra_4*")
agree_vars <- c("agree_1", "agree_2*", "agree_3", "agree_4*")
open_vars <- c("open_1", "open_2*", "open_3*", "open_4*")
neuro_vars <- c("neuro_1", "neuro_2*", "neuro_3", "neuro_4*")
need.cog_vars <- c("need.cog_1", "need.cog_2", "need.cog_3*", "need.cog_4*", "need.cog_5*", "need.cog_6", "need.cog_7*", "need.cog_8*", "need.cog_9*", "need.cog_10", "need.cog_11", "need.cog_12*", "need.cog_13", "need.cog_14", "need.cog_15", "need.cog_16*", "need.cog_17*", "need.cog_18")
aomt_vars <- c("aomt_1", "aomt_2", "aomt_3", "aomt_4*", "aomt_5*", "aomt_6*", "aomt_7*", "aomt_8", "aomt_9")

consc <- alpha( subset( data, select = consc_vars ) )
extra <- alpha( subset( data, select = extra_vars ) )
agree <- alpha( subset( data, select = agree_vars ) )
open <- alpha( subset( data, select = open_vars ) )
neuro <- alpha( subset( data, select = neuro_vars ) )
need.cog <- alpha( subset( data, select = need.cog_vars ) )
aomt <- alpha( subset( data, select = aomt_vars ) )

consc
# Reliability analysis   
# Call: alpha(x = subset(data, select = consc_vars))
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.77      0.77    0.74      0.46 3.4 0.019  4.4 1.2
# 
# lower alpha upper     95% confidence boundaries
# 0.73 0.77 0.81 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# consc_1       0.71      0.71    0.65      0.45 2.4    0.025
# consc_2*      0.74      0.74    0.66      0.49 2.9    0.023
# consc_3       0.75      0.76    0.69      0.51 3.1    0.022
# consc_4*      0.64      0.66    0.57      0.39 1.9    0.032
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# consc_1  366  0.79  0.78  0.68   0.58  4.1 1.6
# consc_2* 366  0.76  0.74  0.62   0.53  3.9 1.7
# consc_3  366  0.68  0.72  0.57   0.50  5.4 1.2
# consc_4* 366  0.85  0.84  0.79   0.70  4.2 1.6
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# consc_1  0.03 0.19 0.20 0.07 0.28 0.18 0.05    0
# consc_2* 0.05 0.18 0.27 0.10 0.16 0.17 0.07    0
# consc_3  0.01 0.01 0.05 0.10 0.28 0.39 0.16    0
# consc_4* 0.02 0.13 0.26 0.15 0.18 0.19 0.08    0

extra
# Reliability analysis   
# Call: alpha(x = subset(data, select = extra_vars))
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.85      0.85    0.83       0.6 5.9 0.013  4.4 1.3
# 
# lower alpha upper     95% confidence boundaries
# 0.83 0.85 0.88 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# extra_1       0.82      0.82    0.77      0.61 4.7    0.017
# extra_2*      0.82      0.82    0.75      0.60 4.5    0.016
# extra_3       0.82      0.82    0.77      0.61 4.7    0.017
# extra_4*      0.80      0.80    0.73      0.56 3.9    0.018
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# extra_1  366  0.83  0.82  0.74   0.68  4.0 1.6
# extra_2* 366  0.82  0.83  0.76   0.68  4.7 1.5
# extra_3  366  0.83  0.82  0.73   0.68  4.3 1.6
# extra_4* 366  0.85  0.86  0.81   0.74  4.4 1.4
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# extra_1  0.07 0.14 0.17 0.20 0.25 0.12 0.05    0
# extra_2* 0.01 0.06 0.18 0.17 0.21 0.26 0.11    0
# extra_3  0.04 0.12 0.19 0.12 0.26 0.22 0.05    0
# extra_4* 0.01 0.07 0.20 0.22 0.21 0.23 0.05    0

agree
# Reliability analysis   
# Call: alpha(x = subset(data, select = agree_vars))
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
# 0.83      0.83    0.82      0.55 4.9 0.015  5.6 0.96
# 
# lower alpha upper     95% confidence boundaries
# 0.8 0.83 0.86 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# agree_1       0.78      0.78    0.72      0.54 3.5    0.020
# agree_2*      0.77      0.77    0.72      0.53 3.4    0.021
# agree_3       0.80      0.80    0.74      0.57 3.9    0.018
# agree_4*      0.79      0.80    0.74      0.57 4.0    0.019
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# agree_1  366  0.82  0.83  0.76   0.68  5.8 1.1
# agree_2* 366  0.85  0.84  0.77   0.70  5.4 1.3
# agree_3  366  0.79  0.80  0.71   0.63  5.4 1.1
# agree_4* 366  0.80  0.79  0.71   0.64  5.6 1.2
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# agree_1  0.00 0.02 0.03 0.03 0.18 0.49 0.24    0
# agree_2* 0.01 0.03 0.10 0.07 0.21 0.43 0.16    0
# agree_3  0.00 0.01 0.06 0.09 0.29 0.42 0.13    0
# agree_4* 0.00 0.01 0.05 0.07 0.19 0.45 0.21    0

open
# Reliability analysis   
# Call: alpha(x = subset(data, select = open_vars))
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.81      0.82     0.8      0.53 4.4 0.016  5.2 1.1
# 
# lower alpha upper     95% confidence boundaries
# 0.78 0.81 0.84 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# open_1       0.77      0.77    0.70      0.53 3.3    0.021
# open_2*      0.73      0.74    0.69      0.49 2.9    0.025
# open_3*      0.81      0.81    0.75      0.59 4.4    0.017
# open_4*      0.74      0.75    0.68      0.49 2.9    0.023
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# open_1  366  0.79  0.80  0.72   0.63  5.3 1.3
# open_2* 366  0.84  0.84  0.76   0.70  5.3 1.3
# open_3* 366  0.76  0.74  0.61   0.54  4.8 1.5
# open_4* 366  0.82  0.83  0.77   0.68  5.3 1.3
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# open_1  0.00 0.03 0.09 0.08 0.28 0.36 0.16    0
# open_2* 0.01 0.03 0.10 0.09 0.24 0.39 0.15    0
# open_3* 0.01 0.06 0.19 0.13 0.21 0.31 0.09    0
# open_4* 0.01 0.03 0.08 0.10 0.21 0.42 0.15    0

neuro
# Reliability analysis   
# Call: alpha(x = subset(data, select = neuro_vars))
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.7      0.69    0.65      0.36 2.3 0.026    4 1.1
# 
# lower alpha upper     95% confidence boundaries
# 0.64 0.7 0.75 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# neuro_1       0.60      0.60    0.51      0.34 1.5    0.036
# neuro_2*      0.65      0.64    0.57      0.37 1.8    0.032
# neuro_3       0.59      0.59    0.49      0.32 1.4    0.038
# neuro_4*      0.68      0.68    0.60      0.42 2.1    0.029
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# neuro_1  366  0.76  0.75  0.64   0.52  3.7 1.7
# neuro_2* 366  0.70  0.71  0.54   0.46  3.9 1.5
# neuro_3  366  0.77  0.76  0.67   0.55  3.8 1.6
# neuro_4* 366  0.65  0.66  0.47   0.39  4.5 1.5
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# neuro_1  0.07 0.26 0.16 0.14 0.23 0.11 0.04    0
# neuro_2* 0.03 0.20 0.22 0.16 0.23 0.12 0.04    0
# neuro_3  0.07 0.19 0.21 0.13 0.25 0.12 0.03    0
# neuro_4* 0.02 0.11 0.14 0.16 0.30 0.20 0.06    0

need.cog
# Reliability analysis   
# Call: alpha(x = subset(data, select = need.cog_vars))
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
# 0.83      0.83    0.85      0.22   5 0.013  4.6 0.82
# 
# lower alpha upper     95% confidence boundaries
# 0.81 0.83 0.86 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# need.cog_1        0.82      0.82    0.84      0.22 4.7    0.014
# need.cog_2        0.82      0.82    0.84      0.21 4.6    0.014
# need.cog_3*       0.82      0.82    0.84      0.21 4.6    0.014
# need.cog_4*       0.82      0.82    0.84      0.21 4.6    0.014
# need.cog_5*       0.82      0.82    0.84      0.22 4.7    0.014
# need.cog_6        0.83      0.83    0.84      0.22 4.8    0.013
# need.cog_7*       0.82      0.83    0.84      0.22 4.8    0.013
# need.cog_8*       0.83      0.83    0.85      0.23 5.0    0.013
# need.cog_9*       0.83      0.83    0.84      0.22 4.9    0.013
# need.cog_10       0.82      0.83    0.84      0.22 4.7    0.014
# need.cog_11       0.82      0.82    0.84      0.21 4.6    0.014
# need.cog_12*      0.82      0.83    0.84      0.22 4.7    0.013
# need.cog_13       0.82      0.83    0.84      0.22 4.7    0.014
# need.cog_14       0.82      0.83    0.84      0.22 4.8    0.013
# need.cog_15       0.82      0.82    0.84      0.22 4.7    0.014
# need.cog_16*      0.83      0.83    0.85      0.22 4.9    0.013
# need.cog_17*      0.82      0.83    0.84      0.22 4.7    0.014
# need.cog_18       0.83      0.83    0.85      0.23 5.0    0.013
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# need.cog_1   366  0.54  0.54  0.50   0.46  4.4 1.6
# need.cog_2   366  0.59  0.60  0.57   0.52  4.8 1.6
# need.cog_3*  366  0.61  0.61  0.59   0.54  4.9 1.5
# need.cog_4*  366  0.61  0.62  0.60   0.54  4.7 1.5
# need.cog_5*  366  0.53  0.54  0.51   0.45  4.9 1.5
# need.cog_6   366  0.47  0.47  0.42   0.38  4.1 1.6
# need.cog_7*  366  0.52  0.51  0.47   0.42  4.4 1.7
# need.cog_8*  366  0.40  0.39  0.33   0.30  4.1 1.7
# need.cog_9*  366  0.45  0.45  0.40   0.36  3.9 1.7
# need.cog_10  366  0.52  0.53  0.50   0.44  5.1 1.5
# need.cog_11  366  0.57  0.57  0.55   0.49  5.0 1.5
# need.cog_12* 366  0.50  0.51  0.47   0.42  5.1 1.5
# need.cog_13  366  0.51  0.51  0.47   0.42  4.5 1.6
# need.cog_14  366  0.50  0.50  0.46   0.41  4.7 1.6
# need.cog_15  366  0.54  0.54  0.51   0.46  4.7 1.6
# need.cog_16* 366  0.44  0.43  0.37   0.34  4.1 1.8
# need.cog_17* 366  0.53  0.52  0.48   0.44  4.8 1.6
# need.cog_18  366  0.37  0.36  0.30   0.26  4.8 1.6
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# need.cog_1   0.02 0.15 0.16 0.18 0.19 0.24 0.07    0
# need.cog_2   0.03 0.08 0.10 0.11 0.23 0.35 0.08    0
# need.cog_3*  0.02 0.07 0.11 0.12 0.28 0.30 0.11    0
# need.cog_4*  0.03 0.08 0.10 0.16 0.26 0.29 0.08    0
# need.cog_5*  0.03 0.06 0.08 0.13 0.26 0.34 0.10    0
# need.cog_6   0.05 0.14 0.20 0.15 0.21 0.19 0.05    0
# need.cog_7*  0.04 0.13 0.20 0.10 0.18 0.25 0.09    0
# need.cog_8*  0.04 0.19 0.19 0.17 0.14 0.19 0.07    0
# need.cog_9*  0.04 0.23 0.19 0.15 0.15 0.18 0.06    0
# need.cog_10  0.03 0.06 0.05 0.14 0.19 0.38 0.14    0
# need.cog_11  0.03 0.07 0.06 0.13 0.22 0.36 0.13    0
# need.cog_12* 0.03 0.04 0.09 0.11 0.22 0.39 0.13    0
# need.cog_13  0.03 0.11 0.17 0.16 0.21 0.24 0.08    0
# need.cog_14  0.04 0.08 0.10 0.15 0.20 0.34 0.08    0
# need.cog_15  0.04 0.08 0.11 0.18 0.18 0.30 0.11    0
# need.cog_16* 0.07 0.16 0.17 0.15 0.19 0.18 0.09    0
# need.cog_17* 0.03 0.08 0.13 0.11 0.25 0.25 0.14    0
# need.cog_18  0.04 0.09 0.11 0.11 0.22 0.32 0.11    0

aomt
# Reliability analysis   
# Call: alpha(x = subset(data, select = aomt_vars))
# 
# raw_alpha std.alpha G6(smc) average_r S/N  ase mean   sd
# 0.74      0.76    0.76      0.26 3.1 0.02  5.1 0.73
# 
# lower alpha upper     95% confidence boundaries
# 0.71 0.74 0.78 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# aomt_1       0.72      0.74    0.73      0.26 2.8    0.022
# aomt_2       0.71      0.72    0.72      0.24 2.5    0.023
# aomt_3       0.70      0.71    0.71      0.23 2.4    0.024
# aomt_4*      0.72      0.73    0.73      0.25 2.7    0.023
# aomt_5*      0.76      0.77    0.76      0.29 3.3    0.019
# aomt_6*      0.75      0.75    0.75      0.28 3.1    0.020
# aomt_7*      0.70      0.71    0.71      0.24 2.5    0.023
# aomt_8       0.72      0.73    0.73      0.26 2.8    0.022
# aomt_9       0.72      0.73    0.73      0.25 2.7    0.022
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# aomt_1  366  0.56  0.56  0.48   0.41  4.8 1.2
# aomt_2  366  0.65  0.67  0.62   0.53  5.8 1.1
# aomt_3  366  0.69  0.70  0.66   0.57  5.5 1.2
# aomt_4* 366  0.60  0.61  0.55   0.46  5.8 1.2
# aomt_5* 366  0.39  0.38  0.24   0.21  4.0 1.3
# aomt_6* 366  0.51  0.47  0.35   0.30  4.4 1.5
# aomt_7* 366  0.66  0.67  0.63   0.54  6.0 1.2
# aomt_8  366  0.58  0.58  0.50   0.42  5.0 1.4
# aomt_9  366  0.59  0.60  0.53   0.45  5.0 1.2
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# aomt_1  0.01 0.03 0.09 0.24 0.33 0.22 0.08    0
# aomt_2  0.00 0.02 0.02 0.04 0.19 0.44 0.28    0
# aomt_3  0.00 0.03 0.05 0.08 0.27 0.39 0.17    0
# aomt_4* 0.01 0.02 0.04 0.06 0.15 0.47 0.25    0
# aomt_5* 0.02 0.09 0.29 0.22 0.23 0.14 0.00    0
# aomt_6* 0.03 0.10 0.19 0.14 0.28 0.19 0.07    0
# aomt_7* 0.01 0.01 0.03 0.07 0.10 0.40 0.38    0
# aomt_8  0.01 0.04 0.11 0.15 0.27 0.29 0.13    0
# aomt_9  0.01 0.03 0.07 0.22 0.31 0.28 0.08    0

rm(agree_vars, aomt_vars, consc_vars, extra_vars, need.cog_vars, neuro_vars, open_vars)


###############################
##### Condition Summaries #####
###############################


condition.summary <-ddply(data, c("certainty","account"), summarise, N = length(id), 
                          mean = mean(ev7), sd = sd(ev7), se = sd/sqrt(N))
# certainty account  N    mean      sd       se
# 1         0       0 92 0.47315 0.14516 0.015134
# 2         0       1 86 0.50012 0.12356 0.013324
# 3         1       0 97 0.52433 0.18402 0.018684
# 4         1       1 91 0.49820 0.13014 0.013642


###################
##### T Tests #####
###################

t.test(LC$avereldiff, HC$avereldiff) # s
# Welch Two Sample t-test
# 
# data:  LC$avereldiff and HC$avereldiff
# t = -7.87, df = 263, p-value = 0.000000000000095
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.036179 -0.021694
# sample estimates:
#   mean of x mean of y 
# 0.0010754 0.0300122  


t.test(LC$percent_change1, HC$percent_change1) # s
# Welch Two Sample t-test
# 
# data:  LC$percent_change1 and HC$percent_change1
# t = -2.34, df = 214, p-value = 0.02
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.335499 -0.028931
# sample estimates:
#   mean of x mean of y 
# -0.140900  0.041315

t.test(LC$percent_change2, HC$percent_change2)
# Welch Two Sample t-test
# 
# data:  LC$percent_change2 and HC$percent_change2
# t = -1.57, df = 345, p-value = 0.12
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.157014  0.017855
# sample estimates:
#   mean of x mean of y 
# -0.22338  -0.15380 

t.test(LC$percent_change3, HC$percent_change3)
# Welch Two Sample t-test
# 
# data:  LC$percent_change3 and HC$percent_change3
# t = 1.15, df = 181, p-value = 0.25
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.83773  3.15698
# sample estimates:
#   mean of x mean of y 
# 1.72520   0.56557

t.test(LC$percent_change4, HC$percent_change4)
# Welch Two Sample t-test
# 
# data:  LC$percent_change4 and HC$percent_change4
# t = -0.799, df = 334, p-value = 0.42
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.87535  0.36957
# sample estimates:
#   mean of x mean of y 
# 0.88794   1.14083 

t.test(LC$percent_change5, HC$percent_change5)
# Welch Two Sample t-test
# 
# data:  LC$percent_change5 and HC$percent_change5
# t = -0.132, df = 285, p-value = 0.89
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.37501  0.32783
# sample estimates:
#   mean of x mean of y 
# 0.050070  0.073663

t.test(LC$percent_change6, HC$percent_change6)
# Welch Two Sample t-test
# 
# data:  LC$percent_change6 and HC$percent_change6
# t = -0.599, df = 307, p-value = 0.55
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.06432  0.56743
# sample estimates:
#   mean of x mean of y 
# -0.80209  -0.55364


##################
##### ANOVAs #####
##################

summary(aov(avedev ~ certainty*account, data = data))
#                    Df Sum Sq Mean Sq F value Pr(>F)  
# certainty           1  0.012 0.01172    1.55  0.214  
# account             1  0.029 0.02887    3.82  0.051 .
# certainty:account   1  0.029 0.02924    3.87  0.050 .
# Residuals         362  2.737 0.00756 

#### Regression analyses for Figure 5

LC <- data[which(data$certainty == 0),]
HC <- data[which(data$certainty == 1),]

regLC <- lm(avedev ~ ev1reldiff + ev2reldiff + ev3reldiff + ev4reldiff + ev5reldiff + ev6reldiff + ev7reldiff, data=LC)
summary(regLC)

regHC <- lm(avedev ~ ev1reldiff + ev2reldiff + ev3reldiff + ev4reldiff + ev5reldiff + ev6reldiff + ev7reldiff, data=HC)
summary(regHC)

####

LCNA <- LC[which(LC$account == 0),]
LCA <- LC[which(LC$account==1),]
HCNA <- HC[which(HC$account==0),]
HCA <- HC[which(HC$account==1),]

regLCNA <- lm(avedev ~ ev1 + ev2 + ev3 + ev4 + ev5 + ev6 + ev7, data=LCNA)
summary(regLCNA)

regLCA <- lm(avedev ~ ev1 + ev2 + ev3 + ev4 + ev5 + ev6 + ev7, data=LCA)
summary(regLCA)

regHCNA <- lm(avedev ~ ev1 + ev2 + ev3 + ev4 + ev5 + ev6 + ev7, data=HCNA)
summary(regHCNA)

regHCA <- lm(avedev ~ ev1 + ev2 + ev3 + ev4 + ev5 + ev6 + ev7, data=HCA)
summary(regHCA)

### Actual

regLCNA <- lm(avedev ~ ev1reldiff + ev2reldiff + ev3reldiff + ev4reldiff + ev5reldiff + ev6reldiff + ev7reldiff, data=LCNA)
summary(regLCNA)

regLCA <- lm(avedev ~ ev1reldiff + ev2reldiff + ev3reldiff + ev4reldiff + ev5reldiff + ev6reldiff + ev7reldiff, data=LCA)
summary(regLCA)

regHCNA <- lm(avedev ~ ev1reldiff + ev2reldiff + ev3reldiff + ev4reldiff + ev5reldiff + ev6reldiff + ev7reldiff, data=HCNA)
summary(regHCNA)

regHCA <- lm(avedev ~ ev1reldiff + ev2reldiff + ev3reldiff + ev4reldiff + ev5reldiff + ev6reldiff + ev7reldiff, data=HCA)
summary(regHCA)


#################
##### Plots #####
#################

actual.low <- c(mean(LC$percent_change1), 
  mean(LC$percent_change2), 
  mean(LC$percent_change3), 
  mean(LC$percent_change4), 
  mean(LC$percent_change5), 
  mean(LC$percent_change6))

actual.high<- c(mean(HC$percent_change1), 
  mean(HC$percent_change2), 
  mean(HC$percent_change3), 
  mean(HC$percent_change4), 
  mean(HC$percent_change5), 
  mean(HC$percent_change6))

### Graph against normative solution

norm.low <- c(0.0778, 0.3111, 0.37808, 0.09628, 0.069, 0.069)
norm.high <- c(0.0073, 0.0419, 0.2944, 0.1629, 0.0543, 0.0543)

norm.low.2 <- c((7.78/70), (-31.11/77.78), (-37.8/46.67), (9.628/8.862), (6.9/18.49), (6.9/25.39))
norm.high.2 <- c((0.7/97.79), (4.19/98.52), (-29.44/94.33), (16.29/64.89), (5.43/81.18), (5.43/86.61))

plot(norm.low.2, type="l", lwd=3, lty="dashed", ylim=c(-1,2), col="lightblue", xlab="Evidence", ylab="Percent Change")
lines(norm.high.2, col="pink", lwd=3, lty="dashed")
lines(actual.low, col="blue", lwd=3)
lines(actual.high, col="red", lwd=3)
legend("topright",  c("Optimal Bayesian Low", "Optimal Bayesian High", "Actual Weak Prior", "Actual Strong Prior"),
       lty=1, col=c('lightblue', 'pink', 'blue', 'red'), bty='n', cex=.75)

graphs
graphs$certainty <- as.factor(graphs$certainty)

library(ggplot2)

avg_per_change_graph <- ggplot(data = graphs, 
                               aes(x = X__1, 
                                   y = percent, 
                                   group = certainty, 
                                   colour = certainty)) +
  theme_minimal() +
  geom_line(size = 2) +
  labs(y = "Average Percent Change", 
       x = "Evidence") +
  scale_colour_manual(values = c("blue",
                               "red"),
                      name = " ",
                      labels = c("Low certainty", 
                               "High certainty"))+
  guides(colour = guide_legend(reverse = TRUE)) +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        axis.text = element_text(size = 10), 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10), 
        legend.key.size = unit(1, "lines"), 
        legend.key = element_rect(fill = "white"),
        legend.margin = unit(1, "lines"))

avg_per_change_graph
  
  