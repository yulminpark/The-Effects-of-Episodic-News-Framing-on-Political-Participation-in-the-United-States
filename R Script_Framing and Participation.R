
################################################################################
########    R Coding Script    #################################################
################################################################################
########    The Effects of Episodic News Framing                        ########
########           Political Participation in the United States         ########
########    Data:  2020 CCES Module                                     ########
################################################################################
################################################################################


### Get data: 2020 CCES Module
install.packages("dplyr")
install.packages("tidyverse")
install.packages("haven")

library(dplyr)
library(tidyverse)
library(haven)

data <- read_sav("CCES20_Module_output.sav")


### Rename variables
colnames(data)
names(data)[1]  <- "state"
names(data)[2]  <- "post_thematic"
names(data)[3]  <- "post_episodic"
names(data)[4]  <- "frame1"
names(data)[5]  <- "frame2"
names(data)[6]  <- "pre"
names(data)[7]  <- "educ"
names(data)[8]  <- "interest"
names(data)[9]  <- "discuss"
names(data)[10] <- "age"
names(data)[11] <- "female"
names(data)[12] <- "income"
colnames(data)


### Re-code dependent variables (DVs) and independent variables (IVs)
install.packages("stats")
install.packages("base")

library(stats)
library(base)


## DV1: Potential for participation (upon exposure to 'thematic' frame)
table(data$post_thematic) #   1   2   9 
                          # 243 473 280 
data$post_thematic[data$post_thematic == 9] <- NA
data$post_thematic[data$post_thematic == 2] <- 0
table(data$post_thematic) #   0   1
                          # 473 243
summary(data$post_thematic)

# Factor DV1
data <- data %>%
  mutate(post_t_f = factor(post_thematic, levels = c(0,1), 
                           labels = c("no", "yes")))
select(data, post_thematic, post_t_f)


## DV2: Potential for participation (upon exposure to 'episodic' frame)
table(data$post_episodic) #   1   2   9 
                          # 229 489 277  
data$post_episodic[data$post_episodic == 9] <- NA
data$post_episodic[data$post_episodic == 2] <- 0
table(data$post_episodic) #   0   1
                          # 489 229
summary(data$post_episodic)

# Factor DV2
data <- data %>%
  mutate(post_e_f = factor(post_episodic, levels = c(0,1), 
                             labels = c("no", "yes")))
select(data, post_episodic, post_e_f)


## IV1: Selective exposure to news frames
  ## IV1-1: Selective exposure to episodic news frames
library(tibble)

table(data$frame1)  #    1   2   3   9 
                    #  245 265 209 280 
table(data$frame2)  #    1   2   3   9 
                    #  243 301 172 280 

data$frame1[data$frame1 == 9] <- NA
data$frame2[data$frame2 == 9] <- NA
table(data$frame1)  #    1   2   3
                    #  245 265 209
table(data$frame2)  #    1   2   3
                    #  243 301 172

data$episodic[data$frame1 == 1 & data$frame2 == 1] <- 1
data$episodic[data$frame1 == 1 & data$frame2 == 2] <- 2
data$episodic[data$frame1 == 2 & data$frame2 == 1] <- 2
data$episodic[data$frame1 == 2 & data$frame2 == 2] <- 3

library(mosaic)
tally(~episodic, data=data) # 1: 93 2: 176 3: 103

  # Factor IV1-1
data <- data %>%
  mutate(episodic_f = factor(episodic, levels = c(1, 2, 3), 
                             labels = c("thematic", "mixed", "episodic")))
select(data, episodic, episodic_f)
table(data$episodic_f)


  ## IV1-2: Selective exposure to thematic news frames
data$thematic <- -(data$episodic - 4)
tally(~thematic, data=data) # 1: 268 2: 176 3: 218
table(data$thematic) #  E-- mix -- T
                     #   1    2    3
                     # 103  176   93

  # Factor IV1-2
data <- data %>%
  mutate(thematic_f = factor(thematic, levels = c(1, 2, 3), 
                             labels = c("episodic", "mixed", "thematic")))
select(data, thematic, thematic_f)


## IV2: Past participation (pre-exposure to thematic/episodic frame)
table(data$pre) #   1   2   3   4 
                # 751  96  66  85 
summary(data$pre)


## IV3: Highest level of education
table(data$educ)   #   1   2   3   4   5   6 
                   #  37 275 216 105 229 138
data$educ[data$educ == 4] <- 3
data$educ[data$educ == 5] <- 4
data$educ[data$educ == 6] <- 5
table(data$educ)   #   1   2   3   4   5 
                   #  37 275 321 229 138 

# Factor IV3
data <- data %>%
  mutate(educ_f = factor(educ, levels = c(1, 2, 3, 4, 5), 
                        labels = c("below high school", "high school",
                                   "some college", "4-year college", "graduate level")))
select(data, educ, educ_f)


## IV4: Political interest
table(data$interest) #   1   2   3   4   7 
                     # 514 281 111  55  38 
data$interest[data$interest == 7] <- NA
data$interest <- -(data$interest - 5)
table(data$interest) #   1   2   3   4
                     #  55 111 281 514 


## IV5: Interpersonal communications
table(data$discuss)  #   1   2   3   4   5 
                     # 305 117 158 254 162
data$discuss <- -(data$discuss - 6)
table(data$discuss)  #   1   2   3   4   5 
                     # 162 254 158 117 305 


## Control1: Age
table(data$age)
data$age[data$age == 2002] <- 1
data$age[data$age == 2001] <- 1
data$age[data$age == 2000] <- 1
data$age[data$age == 1999] <- 2
data$age[data$age == 1998] <- 2
data$age[data$age == 1997] <- 2
data$age[data$age == 1996] <- 2
data$age[data$age == 1995] <- 3
data$age[data$age == 1994] <- 3
data$age[data$age == 1993] <- 3
data$age[data$age == 1992] <- 3
data$age[data$age == 1991] <- 3
data$age[data$age == 1990] <- 4
data$age[data$age == 1989] <- 4
data$age[data$age == 1988] <- 4
data$age[data$age == 1987] <- 4
data$age[data$age == 1986] <- 4
data$age[data$age == 1985] <- 5
data$age[data$age == 1984] <- 5
data$age[data$age == 1983] <- 5
data$age[data$age == 1982] <- 5
data$age[data$age == 1981] <- 5
data$age[data$age == 1980] <- 6
data$age[data$age == 1979] <- 6
data$age[data$age == 1978] <- 6
data$age[data$age == 1977] <- 6
data$age[data$age == 1976] <- 6
data$age[data$age == 1975] <- 7
data$age[data$age == 1974] <- 7
data$age[data$age == 1973] <- 7
data$age[data$age == 1972] <- 7
data$age[data$age == 1971] <- 7
data$age[data$age == 1970] <- 8
data$age[data$age == 1969] <- 8
data$age[data$age == 1968] <- 8
data$age[data$age == 1967] <- 8
data$age[data$age == 1966] <- 8
data$age[data$age == 1965] <- 9
data$age[data$age == 1964] <- 9
data$age[data$age == 1963] <- 9
data$age[data$age == 1962] <- 9
data$age[data$age == 1961] <- 9
data$age[data$age == 1960] <- 10
data$age[data$age == 1959] <- 10
data$age[data$age == 1958] <- 10
data$age[data$age == 1957] <- 10
data$age[data$age == 1956] <- 10
data$age[data$age == 1955] <- 11
data$age[data$age == 1954] <- 11
data$age[data$age == 1953] <- 11
data$age[data$age == 1952] <- 11
data$age[data$age == 1951] <- 11
data$age[data$age == 1950] <- 12
data$age[data$age == 1949] <- 12
data$age[data$age == 1948] <- 12
data$age[data$age == 1947] <- 12
data$age[data$age == 1946] <- 12
data$age[data$age < 1946 & data$age > 12] <- 13
table(data$age)
 #   1   2   3   4   5   6   7   8   9  10  11  12  13 
 #  35  60 103  70  92  86  51  91 103  94  82  64  69 


## Control2: Gender
table(data$female)
data <- data %>%
  mutate(female_f = factor(female, levels = c(1, 2), 
                         labels = c("male", "female")))
select(data, female, female_f)


## Control3: Income
table(data$income)
data$income[data$income == 97] <- NA
table(data$income)


### Estimate logit regression models
install.packages("ggplot2")
install.packages("broom")
install.packages("ggpubr")

library(ggplot2)
library(broom)
library(ggpubr)
library(aod)


## Testing Hypothesis 1 (H1): 
##     "Citizens prefer to read episodic news stories to thematic news stories."

table(data$episodic_f)


## Testing Hypotheses 2 (H2) and 3 (H3):
## H2: "Episodic news framing affects the citizen's willingness to participate 
##      in a protest, rally, or demonstration."
## H3: "This effect is stronger for the citizens that select episodic frames 
##      than for those that select thematic frames."

model1 <- glm(post_t_f ~ thematic_f, 
              data = data, family = "binomial")
summary(model1)
model2 <- glm(post_e_f ~ episodic_f, 
              data = data, family = "binomial")
summary(model2)


model3 <- glm(post_t_f ~ thematic_f + pre, 
              data = data, family = "binomial")
summary(model3)
model4 <- glm(post_e_f ~ episodic_f + pre, 
              data = data, family = "binomial")
summary(model4)


model5 <- glm(post_t_f ~ thematic_f + pre + educ_f + interest + discuss, 
             data = data, family = "binomial")
summary(model5)
model6 <- glm(post_e_f ~ episodic_f + pre + educ_f + interest + discuss, 
              data = data, family = "binomial")
summary(model6)


model5_1 <- glm(post_t_f ~ episodic_f + pre + educ_f + interest + discuss, 
                data = data, family = "binomial")
summary(model5_1)
model6_1 <- glm(post_e_f ~ thematic_f + pre + educ_f + interest + discuss, 
              data = data, family = "binomial")
summary(model6_1)

# Adding control variables to the models

model7 <- glm(post_t_f ~ thematic_f + pre + educ_f + interest + discuss 
              + age + female_f + income,
              data = data, family = "binomial")
summary(model7)
model8 <- glm(post_e_f ~ episodic_f + pre + educ_f + interest + discuss 
              + age + female_f + income,
              data = data, family = "binomial")
summary(model8)


model7_1 <- glm(post_t_f ~ episodic_f + pre + educ_f + interest + discuss 
                + age + female_f + income, 
                data = data, family = "binomial")
summary(model7_1)
model8_1 <- glm(post_e_f ~ thematic_f + pre + educ_f + interest + discuss 
                 + age + female_f + income, 
                 data = data, family = "binomial")
summary(model8_1)


### Confidence Intervals (CIs), using profiled log-likelihood
confint(model7)
confint(model8)


### CIs using standard errors
confint.default(model7)
confint.default(model8)


### Testing for an overall effect of thematic/episodic using wald.test function
library(aod)

wald.test(b=coef(model7), Sigma = vcov(model7), Terms = 2:3)
  # Chi-squared test:
  # X2 = 5.1, df = 2, P(> X2) = 0.078

wald.test(b=coef(model8), Sigma = vcov(model8), Terms = 2:3)
  # Chi-squared test:
  # X2 = 10.1, df = 2, P(> X2) = 0.0063


### Exponentiate the coefficients and interpret them as odds-ratios:
##  (1) Odds ratios only

exp(coef(model7)) # thematic framing effect
exp(coef(model8)) # episodic framing effect


##  (2) Odds ratios and 95% CI

exp(cbind(OR = coef(model7), confint(model7)))

#                             OR      2.5 %    97.5 %
# (Intercept)          0.3798352 0.02762070 5.3274323
# thematic_fmixed      0.4530789 0.21688336 0.9286326
# thematic_fthematic   0.4578382 0.19097245 1.0759861
# pre                  2.6831976 1.92997337 3.8812119
# educ_fhigh school    0.7256443 0.10941045 4.5110648
# educ_fsome college   0.3824867 0.05952394 2.2869103
# educ_f4-year college 0.3917354 0.05760513 2.4808823
# educ_fgraduate level 0.2621875 0.03453030 1.8534591
# interest             1.2048887 0.71239276 2.0736239
# discuss              1.3569582 1.06409474 1.7498952
# age                  0.9030664 0.82014400 0.9921142
# female_ffemale       0.7554954 0.39570133 1.4253864
# income               0.9773637 0.88190969 1.0847192

exp(cbind(OR = coef(model8), confint(model8)))

#                            OR      2.5 %    97.5 %
#  (Intercept)          0.1383463 0.01112866 1.6087703
# episodic_fmixed      1.0073386 0.48605600 2.1084920
# episodic_fepisodic   2.8913421 1.29336868 6.6379207
# pre                  2.0644505 1.52618878 2.8680073
# educ_fhigh school    0.6311066 0.12591595 2.9549365
# educ_fsome college   0.3884100 0.07992868 1.7463459
# educ_f4-year college 0.2771642 0.05290256 1.3428531
# educ_fgraduate level 0.2629615 0.04416064 1.4581373
# interest             1.7116539 1.01382953 2.9832510
# discuss              1.1650892 0.92370660 1.4781965
# age                  0.8713053 0.79451401 0.9520807
# female_ffemale       1.0650056 0.57105154 1.9776712
# income               0.9713420 0.88101188 1.0724702


### CI plots
install.packages("dotwhisker")

library(dotwhisker)

dwplot(model7) # thematic framing effect
dwplot(model8) # episodic framing effect

dwplot(model7) + 
  xlab("Coefficient estimate") + 
  ylab("") +
  ggtitle("") +
  theme(
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey80"),
    legend.title = element_blank()
  ) +
  scale_y_discrete(labels=c("Income", "Gender", "Age", "Discussion", "Political Interest", 
                            "Graduate level", "4-year college", "Some college", "High school",
                            "Past participation", "Thematic", "Mixed type")) +
  theme_bw()+
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"), legend.position="none") 

dwplot(model8) + 
  xlab("Coefficient estimate") + 
  ylab("") +
  ggtitle("") +
  theme(
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey80"),
    legend.title = element_blank()
  ) +
  scale_y_discrete(labels=c("Income", "Gender", "Age", "Discussion", "Political Interest", 
                            "Graduate level", "4-year college", "Some college", "High school",
                            "Past participation", "Episodic", "Mixed type")) +
  theme_bw()+
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title = element_text(face="bold"), legend.position="none") 


## CI plots for both thematic and episodic effects in one graph
dwplot(list(model7, model8),
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2
       )) +
  xlab("Coefficient estimate") + 
  ylab("") +
  ggtitle("") +
  theme(
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey80"),
    legend.title = element_blank()
  ) +
  scale_y_discrete(labels=c("Episodic", "Mixed (Episodic)", "Income", "Gender", "Age", 
                            "Discussion", "Political interest", 
                            "Graduate level", "4-year college", "Some college", "High school",
                            "Past participation", "Thematic", "Mixed (Thematic)")) +
  theme_bw() +
  scale_colour_grey(start = .4, end = .7,
                    name = "",
                    labels = c("Episodic", "Thematic"))


### Summary/Descriptions of variables
library(tidyverse)

install.packages("RNHANES")
install.packages("tableone")

library(RNHANES)
library(tableone)
library(labelled)

data %>% 
  CreateTableOne(vars = select(data, post_thematic) %>% names(), data = .) %>% 
  kableone()

data %>% 
  CreateTableOne(vars = select(data, post_episodic) %>% names(), data = .) %>% 
  kableone()

data %>% 
  CreateTableOne(vars = select(data, thematic_f, episodic_f, pre, educ_f, interest, discuss) %>% names(), data = .) %>% 
  kableone()

data %>% 
  CreateTableOne(vars = select(data, age, female_f, income) %>% names(), data = .) %>% 
  kableone()


### Histograms of variables
library(ggplot2)

## DV1: Potential for participation (upon exposure to 'thematic' frame)
ggplot(data = na.omit(data), 
       aes(x = factor(post_thematic))) +
  labs(title = "                       Potential for Participation",
       subtitle = "                           upon exposure to thematic frame",
       x = "Willingness for participation", 
       y = "Number of respondents") +
  geom_bar(fill = "black", alpha = 0.4, stat = "count", width = 0.7) +
  theme_classic()
table(data$post_thematic) # 0: 473; 1: 243


## DV2: Potential for participation (upon exposure to 'episodic' frame)
ggplot(data = na.omit(data), 
       aes(x = factor(post_episodic))) +
  labs(title = "                       Potential for Participation",
       subtitle = "                           upon exposure to episodic frame",
       x = "Willingness for participation", 
       y = "Number of respondents") +
  geom_bar(fill = "black", alpha = 0.4, stat = "count", width = 0.7) +
  theme_classic()
table(data$post_episodic) # 0: 489; 1: 229


## IV1: Selective exposure to news frames
ggplot(data = na.omit(data), aes(episodic_f)) +
  labs(title = "            Selective Exposure to News Frames",
       x = "Type of news framing",
       y = "Number of respondents") +
  geom_bar(fill = "black",
           alpha = 0.4, width = 0.8) +
  theme_classic()
table(data$episodic_f)


## IV2: Past participation (pre-exposure to thematic/episodic frame)
ggplot(data, aes(pre)) +
  labs(x = "Number of participation in the past",
       y = "Number of respondents") +
  geom_bar(fill = "black", alpha = 0.3) +
  annotate(geom = "text", x = 1, y = 770, label = "751", color = "black") +
  annotate(geom = "text", x = 2, y = 116, label = "96", color = "black") +
  annotate(geom = "text", x = 3, y = 86, label = "66", color = "black") +
  annotate(geom = "text", x = 4, y = 105, label = "85", color = "black") +
  geom_bar(fill = "black", alpha = 0.1) +
  scale_x_discrete(limits = c("1", "2", "3", "4"), 
                   labels = c("0", "1", "2", "3+")) +
  theme_light()
table(data$pre)


## IV3: Highest level of education
ggplot(data = na.omit(data), aes(educ_f)) +
  labs(title = "",
       x = "Highest level of education",
       y = "Number of respondents") +
  geom_bar(fill = "black",
           alpha = 0.3, width = 0.9) +
  theme_classic()
table(data$educ_f)


## IV4: Political interest
ggplot(data = na.omit(data), aes(interest)) +
  labs(title = "",
       x = "Political interest",
       y = "Number of respondents") +
  geom_bar(fill = "black",
           alpha = 0.4, width = 0.9) +
  theme_classic()
table(data$interest)


## IV5: Interpersonal communication
ggplot(data = na.omit(data), aes(discuss)) +
  labs(title = "                                     Interpersonal Communication",
       x = "Interpersonal communication",
       y = "Number of respondents") +
  geom_bar(fill = "black",
           alpha = 0.4, width = 0.9) +
  theme_classic()
table(data$discuss)


## Control1: Age
ggplot(data = na.omit(data), aes(age)) +
  labs(x = "Age",
       y = "Number of respondents") +
  geom_bar(fill = "black",
           alpha = 0.4, width = 0.9) +
  theme_classic()
table(data$age)


## Control2: Gender
ggplot(data = na.omit(data), aes(female_f)) +
  labs(x = "Gender",
       y = "Number of respondents") +
  geom_bar(fill = "black",
           alpha = 0.4, width = 0.9) +
  theme_classic()
table(data$female_f)


## Control3: Income
ggplot(data = na.omit(data), aes(income)) +
  labs(x = "Income",
       y = "Number of respondents") +
  geom_bar(fill = "black",
           alpha = 0.4, width = 0.9) +
  theme_classic()
table(data$income)


############################### END OF SCRIPT ##################################
################################################################################