
library(lme4)

rates <- read.csv("data/cakd1314.csv", header=TRUE)

head(rates)
summary(rates)
colnames(rates)

fit <- glmer(cbind(PBE.,ENROLLMENT-PBE.) ~ (1|COUNTY) + (1|CITY) + PUBLIC.PRIVATE, data=rates, family="binomial")

fit
summary(fit)

fixef(fit)
ranef(fit)