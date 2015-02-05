
#library(dplyr)
library(lme4)

rates <- read.csv("data/cakd1314.csv", header=TRUE, stringsAsFactors=FALSE)
poverty <- read.csv("data/frpm1314.csv", header=TRUE, stringsAsFactors=FALSE)
dim(rates)
dim(poverty)
joined <- merge(rates, poverty, by="School.Code", all.x=TRUE)
dim(joined)

joined$School <- as.factor(paste(joined$County, ":" , joined$City, ":", joined$School.Name.x, sep=""))
joined$City <- as.factor(paste(joined$County, ":" , joined$City, sep=""))
joined$County <- as.factor(joined$County)

joined$Funding <- joined$Charter.Funding.Type

joined[is.na(joined$Funding),]$Funding <- "Private"
joined[(joined$Funding==""),]$Funding <- "aPublic"

joined$Funding <- as.factor(joined$Funding)
joined$Public.Private <- as.factor(joined$Public.Private)

summary(joined$Funding)
summary(joined$Public.Private)

model0 <- cbind(PBE.,Enrollment-PBE.) ~ (1|County) + (1|City) + (1|School)
model1 <- cbind(PBE.,Enrollment-PBE.) ~ (1|County) + (1|City) + (1|School) + Funding

fit0 <- glmer(model0, data=joined, family="binomial")
fit1 <- glmer(model1, data=joined, family="binomial")

anova(fit0)
anova(fit1)
anova(fit0,fit1)

summary(fit1)

fixef(fit1)
ranef(fit1)
