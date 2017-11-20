### Study 2b
### Public Observers

### Demographics
options(scipen=999)

#Set 1: Import data set

#Create local variable 
data2 <- political_posturing_observers

#Remove deprecated variable 
rm(political_posturing_observers)

#Extract demographic information 
demographic <- subset(data2, select = c("educ", "age", "gender", "race"))

summary(data2$age)
summary(data2$gender==1)
summary(data2$gender==2)
summary(data2$race==1)

95/141 # 67% female
63/141 # 44% Caucasian

#View age
v1 <- subset(demographic, age == 1)
v2 <- subset(demographic, age == 2)
v3 <- subset(demographic, age == 3)
v4 <- subset(demographic, age == 4)
v5 <- subset(demographic, age == 5)
v6 <- subset(demographic, age == 6)
v7 <- subset(demographic, age == 7)
#Clean age
rm(v1, v2, v3, v4, v5, v6, v7)

#View gender
v1 <- subset(demographic, gender == 1)
v2 <- subset(demographic, gender == 2)
#Clean gender 
rm(v1, v2)

#View race
v1 <- subset(demographic, race == 1)
v2 <- subset(demographic, race == 2)
v3 <- subset(demographic, race == 3)
v4 <- subset(demographic, race == 4)
v5 <- subset(demographic, race == 5)
v6 <- subset(demographic, race == 6)
v7 <- subset(demographic, race == 7)
#Clean race
rm(v1, v2, v3, v4, v5, v6, v7)

#View educ
v1 <- subset(demographic, educ == 1)
v2 <- subset(demographic, educ == 2)
v3 <- subset(demographic, educ == 3)
v4 <- subset(demographic, educ == 4)
v5 <- subset(demographic, educ == 5)
v6 <- subset(demographic, educ == 6)
v7 <- subset(demographic, educ == 7)
v8 <- subset(demographic, educ == 8)
#Clean educ
rm(v1, v2, v3, v4, v5, v6, v7, v8)

#Extract demographic information 
demographic <- subset(data, select = c("orient", "orient.2_1", "orient.2_2", "orient.2_3"))

#View political party affiliation
v1 <- subset(demographic, orient == 1)
v2 <- subset(demographic, orient == 2)
v3 <- subset(demographic, orient == 3)
v4 <- subset(demographic, orient == 4)
v5 <- subset(demographic, orient == 5)
v6 <- subset(demographic, orient == 6)
v7 <- subset(demographic, orient == 7)
#Clean political party affiliation
rm(v1, v2, v3, v4, v5, v6, v7)

#View identifies as
v1 <- subset(demographic, orient.2_1 == 1)
v2 <- subset(demographic, orient.2_1 == 2)
v3 <- subset(demographic, orient.2_1 == 3)
v4 <- subset(demographic, orient.2_1 == 4)
v5 <- subset(demographic, orient.2_1 == 5)
v6 <- subset(demographic, orient.2_1 == 6)
v7 <- subset(demographic, orient.2_1 == 7)
v8 <- subset(demographic, orient.2_1 == 8)
v9 <- subset(demographic, orient.2_1 == 9)
#Clean identifies as
rm(v1, v2, v3, v4, v5, v6, v7, v8, v9)

#View socially
v1 <- subset(demographic, orient.2_2 == 1)
v2 <- subset(demographic, orient.2_2 == 2)
v3 <- subset(demographic, orient.2_2 == 3)
v4 <- subset(demographic, orient.2_2 == 4)
v5 <- subset(demographic, orient.2_2 == 5)
v6 <- subset(demographic, orient.2_2 == 6)
v7 <- subset(demographic, orient.2_2 == 7)
v8 <- subset(demographic, orient.2_2 == 8)
v9 <- subset(demographic, orient.2_2 == 9)
#Clean socially
rm(v1, v2, v3, v4, v5, v6, v7, v8, v9)

#View economically
v1 <- subset(demographic, orient.2_3 == 1)
v2 <- subset(demographic, orient.2_3 == 2)
v3 <- subset(demographic, orient.2_3 == 3)
v4 <- subset(demographic, orient.2_3 == 4)
v5 <- subset(demographic, orient.2_3 == 5)
v6 <- subset(demographic, orient.2_3 == 6)
v7 <- subset(demographic, orient.2_3 == 7)
v8 <- subset(demographic, orient.2_3 == 8)
v9 <- subset(demographic, orient.2_3 == 9)
#Clean economically
rm(v1, v2, v3, v4, v5, v6, v7, v8, v9)

consc <- subset(data, select = c("consc_1", "consc_2", "consc_3", "consc_4"))
alpha(consc)
# Reliability analysis   
# Call: alpha(x = consc)
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.81      0.81    0.78      0.52 4.3 0.025  4.7 1.2
# 
# lower alpha upper     95% confidence boundaries
# 0.76 0.81 0.86 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# consc_1      0.75      0.76    0.69      0.51 3.1    0.033
# consc_2      0.75      0.76    0.69      0.51 3.2    0.035
# consc_3      0.79      0.80    0.73      0.56 3.9    0.030
# consc_4      0.72      0.74    0.66      0.49 2.9    0.039
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# consc_1 130  0.79  0.81  0.72   0.63  4.4 1.5
# consc_2 130  0.84  0.81  0.72   0.65  4.2 1.8
# consc_3 130  0.71  0.76  0.62   0.56  5.6 1.1
# consc_4 130  0.85  0.83  0.76   0.69  4.4 1.7
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# consc_1 0.02 0.11 0.18 0.04 0.41 0.22 0.02 0.08
# consc_2 0.07 0.13 0.21 0.12 0.12 0.26 0.08 0.08
# consc_3 0.00 0.03 0.03 0.04 0.28 0.41 0.22 0.08
# consc_4 0.05 0.13 0.16 0.12 0.22 0.24 0.08 0.08

extra <- subset(data, select = c("extra_1", "extra_2", "extra_3", "extra_4"))
alpha(extra)
# Reliability analysis   
# Call: alpha(x = extra)
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.88      0.88    0.86      0.64 7.2 0.017    4 1.4
# 
# lower alpha upper     95% confidence boundaries
# 0.85 0.88 0.91 

# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# extra_1      0.85      0.85    0.80      0.65 5.6    0.022
# extra_2      0.84      0.84    0.79      0.65 5.5    0.023
# extra_3      0.86      0.86    0.82      0.68 6.3    0.020
# extra_4      0.82      0.82    0.76      0.60 4.6    0.026
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# extra_1 130  0.85  0.85  0.77   0.73  3.6 1.5
# extra_2 130  0.86  0.86  0.80   0.74  4.4 1.6
# extra_3 130  0.83  0.83  0.73   0.69  4.0 1.6
# extra_4 130  0.89  0.89  0.86   0.80  4.2 1.6
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# extra_1 0.08 0.20 0.20 0.19 0.24 0.06 0.02 0.08
# extra_2 0.04 0.08 0.22 0.14 0.22 0.22 0.08 0.08
# extra_3 0.08 0.15 0.15 0.10 0.35 0.15 0.02 0.08
# extra_4 0.06 0.07 0.26 0.14 0.22 0.20 0.05 0.08

agree <- subset(data, select = c("agree_1", "agree_2", "agree_3", "agree_4"))
alpha(agree)
# Reliability analysis   
# Call: alpha(x = agree)
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
# 0.85      0.85    0.83      0.59 5.7 0.021  5.6 0.99
# 
# lower alpha upper     95% confidence boundaries
# 0.81 0.85 0.89 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# agree_1      0.78      0.78    0.73      0.54 3.5    0.032
# agree_2      0.76      0.77    0.72      0.53 3.4    0.035
# agree_3      0.86      0.87    0.82      0.69 6.6    0.019
# agree_4      0.81      0.82    0.76      0.60 4.5    0.027
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# agree_1 130  0.87  0.88  0.83   0.77  5.9 1.0
# agree_2 130  0.89  0.88  0.85   0.78  5.5 1.3
# agree_3 130  0.73  0.74  0.60   0.55  5.3 1.2
# agree_4 130  0.83  0.82  0.75   0.68  5.7 1.3
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# agree_1 0.00 0.01 0.05 0.02 0.20 0.45 0.27 0.08
# agree_2 0.00 0.03 0.08 0.09 0.18 0.42 0.21 0.08
# agree_3 0.01 0.01 0.07 0.10 0.35 0.35 0.12 0.08
# agree_4 0.00 0.02 0.05 0.07 0.22 0.34 0.30 0.08

open <- subset(data, select = c("open_1", "open_2", "open_3", "open_4"))
alpha(open)
# Reliability analysis   
# Call: alpha(x = open)
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.83      0.83    0.81      0.55 4.9 0.024  5.1 1.1
# 
# lower alpha upper     95% confidence boundaries
# 0.78 0.83 0.87 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# open_1      0.82      0.82    0.76      0.61 4.6    0.026
# open_2      0.76      0.76    0.73      0.52 3.2    0.036
# open_3      0.82      0.82    0.76      0.60 4.5    0.027
# open_4      0.73      0.73    0.66      0.47 2.7    0.040
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# open_1 130  0.76  0.76  0.66   0.57  5.1 1.4
# open_2 130  0.84  0.84  0.76   0.70  5.0 1.4
# open_3 130  0.78  0.77  0.65   0.58  4.8 1.5
# open_4 130  0.88  0.89  0.85   0.78  5.3 1.3
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# open_1 0.02 0.04 0.06 0.09 0.35 0.30 0.13 0.08
# open_2 0.01 0.05 0.12 0.13 0.25 0.35 0.10 0.08
# open_3 0.01 0.05 0.18 0.16 0.20 0.30 0.11 0.08
# open_4 0.01 0.02 0.08 0.12 0.28 0.32 0.18 0.08

neuro <- subset(data, select = c("neuro_1", "neuro_2", "neuro_3", "neuro_4"))
alpha(neuro)
# Reliability analysis   
# Call: alpha(x = neuro)
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.81      0.81    0.77      0.52 4.3 0.026  3.9 1.3
# 
# lower alpha upper     95% confidence boundaries
# 0.76 0.81 0.86 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# neuro_1      0.75      0.75    0.67      0.49 2.9    0.037
# neuro_2      0.80      0.80    0.73      0.57 3.9    0.029
# neuro_3      0.74      0.74    0.65      0.48 2.8    0.039
# neuro_4      0.77      0.77    0.70      0.52 3.3    0.034
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# neuro_1 130  0.82  0.82  0.74   0.66  3.7 1.6
# neuro_2 130  0.76  0.75  0.61   0.55  3.7 1.7
# neuro_3 130  0.83  0.83  0.76   0.68  3.9 1.6
# neuro_4 130  0.79  0.79  0.69   0.62  4.3 1.6
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# neuro_1 0.08 0.22 0.18 0.13 0.30 0.07 0.03 0.08
# neuro_2 0.07 0.18 0.28 0.11 0.19 0.10 0.07 0.08
# neuro_3 0.07 0.18 0.18 0.14 0.24 0.15 0.03 0.08
# neuro_4 0.04 0.12 0.15 0.18 0.25 0.19 0.05 0.08

need.cog <- subset(data, select = c("need.cog_1", "need.cog_2", "need.cog_3", "need.cog_4", "need.cog_5", "need.cog_6", "need.cog_7", "need.cog_8", "need.cog_9", "need.cog_10", "need.cog_11", "need.cog_12", "need.cog_13", "need.cog_14", "need.cog_15", "need.cog_16", "need.cog_17", "need.cog_18"))
alpha(need.cog)
# Reliability analysis   
# Call: alpha(x = need.cog)
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.88      0.89    0.91       0.3 7.7 0.014  4.6 0.8
# 
# lower alpha upper     95% confidence boundaries
# 0.85 0.88 0.91 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# need.cog_1       0.87      0.88    0.90      0.29 7.1    0.016
# need.cog_2       0.87      0.88    0.90      0.29 7.1    0.016
# need.cog_3       0.87      0.87    0.90      0.29 6.9    0.016
# need.cog_4       0.87      0.87    0.90      0.29 6.8    0.016
# need.cog_5       0.87      0.88    0.90      0.29 7.0    0.016
# need.cog_6       0.88      0.88    0.90      0.30 7.5    0.015
# need.cog_7       0.87      0.88    0.90      0.30 7.2    0.015
# need.cog_8       0.88      0.89    0.91      0.32 7.8    0.014
# need.cog_9       0.88      0.88    0.90      0.30 7.4    0.015
# need.cog_10      0.88      0.88    0.90      0.30 7.3    0.015
# need.cog_11      0.88      0.88    0.90      0.30 7.2    0.015
# need.cog_12      0.88      0.88    0.90      0.31 7.5    0.015
# need.cog_13      0.87      0.88    0.90      0.29 7.0    0.016
# need.cog_14      0.88      0.88    0.90      0.30 7.4    0.015
# need.cog_15      0.87      0.88    0.90      0.29 7.1    0.016
# need.cog_16      0.88      0.89    0.91      0.31 7.7    0.014
# need.cog_17      0.88      0.88    0.90      0.30 7.3    0.015
# need.cog_18      0.88      0.89    0.91      0.31 7.7    0.014
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# need.cog_1  129  0.66  0.66  0.65   0.60  4.3 1.3
# need.cog_2  129  0.65  0.66  0.64   0.59  4.6 1.3
# need.cog_3  129  0.71  0.71  0.70   0.66  5.0 1.3
# need.cog_4  129  0.74  0.75  0.74   0.70  4.8 1.3
# need.cog_5  129  0.68  0.68  0.67   0.63  5.0 1.3
# need.cog_6  129  0.53  0.52  0.47   0.44  4.0 1.5
# need.cog_7  129  0.61  0.60  0.58   0.54  4.6 1.5
# need.cog_8  129  0.40  0.38  0.33   0.30  3.7 1.5
# need.cog_9  129  0.56  0.54  0.51   0.48  3.8 1.5
# need.cog_10 129  0.56  0.57  0.54   0.49  5.0 1.3
# need.cog_11 129  0.59  0.60  0.58   0.53  4.9 1.2
# need.cog_12 129  0.48  0.50  0.46   0.41  5.3 1.2
# need.cog_13 129  0.68  0.69  0.68   0.63  4.2 1.3
# need.cog_14 129  0.54  0.54  0.51   0.46  4.7 1.4
# need.cog_15 129  0.66  0.66  0.64   0.60  4.7 1.3
# need.cog_16 129  0.44  0.42  0.37   0.34  3.9 1.5
# need.cog_17 129  0.58  0.57  0.55   0.51  4.8 1.5
# need.cog_18 129  0.41  0.42  0.36   0.33  4.8 1.4
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# need.cog_1  0.02 0.07 0.20 0.22 0.34 0.10 0.05 0.09
# need.cog_2  0.01 0.05 0.17 0.15 0.33 0.26 0.03 0.09
# need.cog_3  0.01 0.04 0.09 0.18 0.31 0.25 0.12 0.09
# need.cog_4  0.02 0.02 0.12 0.19 0.36 0.20 0.09 0.09
# need.cog_5  0.00 0.05 0.09 0.16 0.29 0.28 0.12 0.09
# need.cog_6  0.05 0.14 0.22 0.14 0.34 0.08 0.04 0.09
# need.cog_7  0.03 0.05 0.17 0.19 0.25 0.26 0.06 0.09
# need.cog_8  0.08 0.15 0.29 0.18 0.19 0.09 0.04 0.09
# need.cog_9  0.05 0.16 0.29 0.19 0.20 0.06 0.05 0.09
# need.cog_10 0.00 0.05 0.10 0.16 0.28 0.28 0.13 0.09
# need.cog_11 0.01 0.03 0.09 0.19 0.36 0.24 0.08 0.09
# need.cog_12 0.01 0.02 0.05 0.15 0.29 0.34 0.14 0.09
# need.cog_13 0.02 0.09 0.20 0.22 0.32 0.12 0.03 0.09
# need.cog_14 0.02 0.06 0.12 0.19 0.33 0.21 0.08 0.09
# need.cog_15 0.01 0.03 0.14 0.23 0.30 0.19 0.09 0.09
# need.cog_16 0.05 0.15 0.23 0.18 0.22 0.14 0.03 0.09
# need.cog_17 0.03 0.05 0.16 0.14 0.25 0.26 0.12 0.09
# need.cog_18 0.02 0.05 0.15 0.12 0.35 0.26 0.06 0.09

aomt <- subset(data, select = c("aomt_1", "aomt_2", "aomt_3", "aomt_4", "aomt_5", "aomt_6", "aomt_7", "aomt_8", "aomt_9"))
alpha(aomt)
# Reliability analysis   
# Call: alpha(x = aomt)
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
# 0.78      0.79    0.81       0.3 3.8 0.027  5.1 0.79
# 
# lower alpha upper     95% confidence boundaries
# 0.73 0.78 0.84 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# aomt_1      0.77      0.78    0.80      0.30 3.5    0.030
# aomt_2      0.76      0.77    0.79      0.29 3.3    0.031
# aomt_3      0.73      0.74    0.77      0.26 2.9    0.034
# aomt_4      0.75      0.76    0.77      0.28 3.2    0.031
# aomt_5      0.80      0.81    0.82      0.34 4.1    0.026
# aomt_6      0.76      0.77    0.79      0.29 3.3    0.031
# aomt_7      0.74      0.75    0.76      0.27 3.0    0.032
# aomt_8      0.78      0.78    0.80      0.31 3.6    0.028
# aomt_9      0.77      0.78    0.80      0.31 3.6    0.029
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# aomt_1 129  0.59  0.58  0.48   0.44  4.7 1.4
# aomt_2 129  0.63  0.65  0.59   0.52  5.9 1.1
# aomt_3 129  0.77  0.78  0.76   0.69  5.5 1.2
# aomt_4 129  0.66  0.67  0.66   0.55  5.7 1.2
# aomt_5 129  0.39  0.39  0.27   0.21  4.1 1.3
# aomt_6 129  0.65  0.63  0.56   0.50  4.4 1.5
# aomt_7 129  0.72  0.74  0.74   0.62  5.8 1.2
# aomt_8 129  0.56  0.54  0.46   0.39  4.7 1.5
# aomt_9 129  0.55  0.54  0.45   0.39  4.8 1.3
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7 miss
# aomt_1 0.01 0.05 0.12 0.28 0.21 0.24 0.09 0.09
# aomt_2 0.01 0.01 0.02 0.06 0.21 0.36 0.33 0.09
# aomt_3 0.00 0.02 0.05 0.13 0.25 0.34 0.21 0.09
# aomt_4 0.01 0.01 0.04 0.10 0.16 0.40 0.29 0.09
# aomt_5 0.02 0.09 0.25 0.22 0.31 0.07 0.05 0.09
# aomt_6 0.02 0.09 0.23 0.14 0.30 0.14 0.09 0.09
# aomt_7 0.01 0.00 0.06 0.05 0.19 0.37 0.33 0.09
# aomt_8 0.01 0.09 0.14 0.17 0.31 0.19 0.10 0.09
# aomt_9 0.02 0.05 0.07 0.22 0.34 0.22 0.07 0.09

orient.2 <- subset(data, select = c("orient.2_1", "orient.2_2", "orient.2_3"))
alpha(orient.2)
# Reliability analysis   
# Call: alpha(x = orient.2)
# 
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
# 0.85      0.86    0.87      0.66   6 0.023    4 1.5
# 
# lower alpha upper     95% confidence boundaries
# 0.81 0.85 0.9 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se
# orient.2_1      0.62      0.62    0.45      0.45 1.7    0.063
# orient.2_2      0.87      0.87    0.77      0.77 6.8    0.022
# orient.2_3      0.87      0.87    0.77      0.77 6.7    0.022
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean  sd
# orient.2_1 128  0.96  0.96  0.96   0.90  4.0 1.6
# orient.2_2 128  0.84  0.84  0.76   0.64  4.7 1.7
# orient.2_3 128  0.84  0.84  0.76   0.64  3.3 1.7
# 
# Non missing response frequency for each item
# 1    2    3    4    5    6    7    8    9 miss
# orient.2_1 0.04 0.11 0.28 0.18 0.24 0.06 0.06 0.02 0.00 0.09
# orient.2_2 0.04 0.04 0.19 0.16 0.26 0.12 0.16 0.02 0.02 0.09
# orient.2_3 0.14 0.22 0.28 0.15 0.10 0.05 0.05 0.02 0.00 0.09

summary(aov(data$aomt ~ data$need.cog))
# Df Sum Sq Mean Sq F value   Pr(>F)    
# data$need.cog   1  18.02  18.024    36.7 1.45e-08 ***
#   Residuals     127  62.37   0.491                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 12 observations deleted due to missingness

summary(aov(data$need.cog ~ data$aomt))
# Df Sum Sq Mean Sq F value   Pr(>F)    
# data$aomt     1  18.18  18.184    36.7 1.45e-08 ***
#   Residuals   127  62.92   0.495                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 12 observations deleted due to missingness

lm(data$aomt ~ data$need.cog)
# Call:
#   lm(formula = data$aomt ~ data$need.cog)
# 
# Coefficients:
#   (Intercept)  data$need.cog  
# 2.9080         0.4714  

lm(data$need.cog ~ data$aomt)
# Call:
#   lm(formula = data$need.cog ~ data$aomt)
# 
# Coefficients:
#   (Intercept)    data$aomt  
# 2.1563       0.4756  
