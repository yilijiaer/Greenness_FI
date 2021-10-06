library(table1)
library(lubridate)
library(readstata13)
library(psych)
library(dplyr) 
library("survival")
library("survminer")
library(openxlsx)
library(Hmisc)
library(geepack)
library(sjmisc)
library(magrittr)
library(gtools)
library(broom)
greenness_FI<- read.xlsx("D:/greenness structure_NDVI_ADL_MMSE_FRAILTY/greenness_FRAILTY_08_14 _20210310.xlsx", sheet = 1)

#quatiles
greenness_FI$LPI_quartile <- ntile(greenness_FI$LPI, 4)  
greenness_FI$SHAPE_AM_quartile <- ntile(greenness_FI$SHAPE_AM, 4) 
greenness_FI$COHESION_quartile <- ntile(greenness_FI$COHESION, 4) 
greenness_FI$ED_quartile <- ntile(greenness_FI$ED, 4) 
greenness_FI$FRAC_AM_quartile <- ntile(greenness_FI$FRAC_AM, 4) 
greenness_FI$PLADJ_quartile <- ntile(greenness_FI$PLADJ, 4) 

#factor
greenness_FI$NDVI_con_quartile <- factor(greenness_FI$NDVI_con_quartile)
greenness_FI$LPI_quartile <- factor(greenness_FI$LPI_quartile)
greenness_FI$SHAPE_AM_quartile <- factor(greenness_FI$SHAPE_AM_quartile)
greenness_FI$COHESION_quartile <- factor(greenness_FI$COHESION_quartile)
greenness_FI$ED_quartile <- factor(greenness_FI$ED_quartile)
greenness_FI$FRAC_AM_quartile <- factor(greenness_FI$FRAC_AM_quartile)
greenness_FI$PLADJ_quartile <- factor(greenness_FI$PLADJ_quartile)
greenness_FI$residence <- factor(greenness_FI$residence)
greenness_FI$sex <- factor(greenness_FI$sex)
greenness_FI$marriage <- factor(greenness_FI$marriage)
greenness_FI$exercise <- factor(greenness_FI$exercise)
greenness_FI$smoking <- factor(greenness_FI$smoking)
greenness_FI$alcohol <- factor(greenness_FI$alcohol)
greenness_FI$education_2 <- factor(greenness_FI$education_2)

#Table 1
table1(~NDVI2008+LPI+SHAPE+COHESION+FI2008
       |factor(sex), data=greenness_FI)

#Table2-cross-sectional
#linear
regression<-lm(FI2008~NDVI_con_quartile #LPI_quartile, SHAPE_quartile, COHESION_quartile
                     +age+sex+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence
                     +BMI+smoking+alcohol+exercise+PM25, data=greenness_FI)
summary(regression)
#logistic
regression <- glm(FI_08_binary~NDVI_con_quartile #LPI_quartile, SHAPE_quartile, COHESION_quartile
                  +age+sex+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence
                  +BMI+smoking+alcohol+exercise+PM25,
                  data=greenness_FI)
summary(regression)
                  
#Table2-GEE
model1 <- geeglm(FI~NDVI_con_quartile+age+sex+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence #LPI_quartile, SHAPE_quartile, COHESION_quartile
                 +BMI+smoking+alcohol+exercise+PM25,
                 id=id, 
                 family= gaussian, 
                 data=na.omit(greenness_FI))
summary(greenness_FI$NDVI_con_quartile)
summary(model1)
confint(model1)

model1 <- geeglm(FI_binary~NDVI_con_quartile+age+sex+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence
                 +BMI+smoking+alcohol+exercise+PM25,
               id=id, 
               family= binomial(link = "logit"), 
               data=na.omit(greenness_FI))
summary(greenness_FI$NDVI_con_quartile)
summary(model1)
confint(model1)

#Figure2-Subgroup-changes in FI
greenness_FI_nochange<- read.xlsx("D:/greenness_FRAILTY_08_14 _20210310_nochange.xlsx", sheet = 1)
greenness_FI_increase<- read.xlsx("D:/greenness_FRAILTY_08_14 _20210310_increase.xlsx", sheet = 1)

summary(geeglm(FI~NDVI_con+age+sex+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence #LPI, SHAPE, COHESION
               +BMI+smoking+alcohol+exercise+PM25,
               id=id, 
               family= gaussian, 
               data=na.omit(greenness_FI_nochange)))

summary(geeglm(FI~NDVI_con+age+sex+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence #LPI, SHAPE, COHESION
               +BMI+smoking+alcohol+exercise+PM25,
               id=id, 
               family= gaussian, 
               data=na.omit(greenness_FI_increase)))

#Figure2-Subgroup-city/rural
city <- subset(greenness_FI,residence==1)
town <- subset(greenness_FI,residence==2)
rural <- subset(greenness_FI,residence==3)

summary(geeglm(FI~NDVI_con+age+sex+entrant_year+marital_status+geographic+literacy+income+financial_independence #LPI, SHAPE, COHESION
               +BMI+smoking+alcohol+exercise+PM25,
               id=id, 
               family= gaussian, 
               data=na.omit(city)))

#Figure2-Subgroup-gender
male <- subset(greenness_FI,sex==1)
female <- subset(greenness_FI,sex==2)

summary(geeglm(FI~NDVI_con+age+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence #LPI, SHAPE, COHESION
               +BMI+smoking+alcohol+exercise+PM25,
               id=id, 
               family= gaussian, 
               data=na.omit(male)))

#Figure2-Subgroup-age
a <- subset(greenness_FI,age<=79)
b <- subset(greenness_FI,age<=89)
c <- subset(greenness_FI,age<=99)
d <- subset(greenness_FI,100<=age)
summary(geeglm(FI~NDVI_con+sex+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence #LPI, SHAPE, COHESION
               +BMI+smoking+alcohol+exercise+PM25,
               id=id, 
               family= gaussian, 
               data=na.omit(a)))

#Figure2-Subgroup-edu
edu <- subset(greenness_FI,education_2==1)
unedu <- subset(greenness_FI,education_2==0)
summary(geeglm(FI~NDVI_con+age+sex+entrant_year+marital_status+geographic+residence+income+financial_independence #LPI, SHAPE, COHESION
               +BMI+smoking+alcohol+exercise+PM25,
               id=id, 
               family= gaussian, 
               data=na.omit(edu)))

#Figure2-Subgroup-marriage
yes <- subset(greenness_FI,marriage==1)
no <- subset(greenness_FI,marriage==2)
summary(geeglm(FI~NDVI_con+age+sex+entrant_year+geographic+residence+literacy+income+financial_independence #LPI, SHAPE, COHESION
               +BMI+smoking+alcohol+exercise+PM25,
               id=id, 
               family= gaussian, 
               data=na.omit(yes)))
summary(model1)
exp(model1$coefficients)
exp(confint(model1))

#Figure2-Subgroup-PM2.5
low <- subset(greenness_FI,PM25<=51.6)
high <- subset(greenness_FI,51.6<PM25)
summary(geeglm(FI~NDVI_con+age+sex+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence #LPI, SHAPE, COHESION
               +BMI+smoking+alcohol+exercise,
               id=id, 
               family= gaussian, 
               data=na.omit(low)))

#pearson
cor<-cor.test(greenness_FI$NDVI_con, greenness_FI$COHESION, method = "spearman")

#Sensitivity analysis
#linear
regression<-lm(FI2008~ED_quartile #FRAC_quartile, PLADJ_quartile
               +age+sex+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence
               +BMI+smoking+alcohol+exercise+PM25, data=greenness_FI)
summary(regression)
#logistic
regression <- glm(FI_08_binary~ED_quartile #FRAC_quartile, PLADJ_quartile
                  +age+sex+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence
                  +BMI+smoking+alcohol+exercise+PM25,
                  data=greenness_FI)
summary(regression)

#GEE
model1 <- geeglm(FI~ED_quartile+age+sex+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence #LPI_quartile, SHAPE_quartile, COHESION_quartile
                 +BMI+smoking+alcohol+exercise+PM25,
                 id=id, 
                 family= gaussian, 
                 data=na.omit(greenness_FI))
summary(greenness_FI$ED_quartile)
summary(model1)
confint(model1)

model1 <- geeglm(FI_binary~ED_quartile+age+sex+entrant_year+marital_status+geographic+residence+literacy+income+financial_independence
                 +BMI+smoking+alcohol+exercise+PM25,
                 id=id, 
                 family= binomial(link = "logit"), 
                 data=na.omit(greenness_FI))
summary(greenness_FI$ED_quartile)
summary(model1)
confint(model1)
