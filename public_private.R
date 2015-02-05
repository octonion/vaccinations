
library(lme4)

rates <- read.csv("data/cakd1314.csv", header=TRUE)

head(rates)
summary(rates)
colnames(rates)

rates$CITY <- paste(rates$COUNTY, ":" , rates$CITY, sep="")

rates$SCHOOL <- paste(rates$COUNTY, ":" , rates$CITY, ":", rates$SCHOOL.NAME, sep="")

# Should be equivalent to:
#fit <- glmer(cbind(PBE.,ENROLLMENT-PBE.) ~ (1|COUNTY/CITY/SCHOOL) + PUBLIC.PRIVATE, data=rates, family="binomial")

model0 <- cbind(PBE.,ENROLLMENT-PBE.) ~ (1|COUNTY) + (1|CITY) + (1|SCHOOL)
model1 <- cbind(PBE.,ENROLLMENT-PBE.) ~ (1|COUNTY) + (1|CITY) + (1|SCHOOL) + PUBLIC.PRIVATE

fit0 <- glmer(model0, data=rates, family="binomial")
fit1 <- glmer(model1, data=rates, family="binomial")

anova(fit0)
anova(fit1)
anova(fit0,fit1)

summary(fit1)

fixef(fit1)
ranef(fit1)
