library(survey)
library(summarytools)
library(foreign)
library(tidyverse)

df.raw <- read.spss("ATP W34.sav", to.data.frame = T) 
df <- df.raw[,c("QKEY","WEIGHT_W34","F_SEX_FINAL","F_RACECMB_RECRUITMENT","F_RACETHN_RECRUITMENT","FUD22_W34")]

dsgn <- svydesign(id=~QKEY,
                  weights=~WEIGHT_W34, 
                  data=df
)

test1 <- t(svyby(~FUD22_W34, ~F_SEX_FINAL, design = dsgn, FUN = svymean, na.rm.all = T))
test2 <- t(svyby(~FUD22_W34, ~F_RACECMB_RECRUITMENT, design = dsgn, FUN = svymean, na.rm.all = T))
test3 <- t(svyby(~FUD22_W34, ~F_RACETHN_RECRUITMENT, design = dsgn, FUN = svymean, na.rm.all = T))
test <- cbind(test1,test2,test3)
test <- test[-1,]
rm(test1,test2,test3)

test.num <- as.data.frame(apply(t(test), 1,as.numeric))
rownames(test.num) <- rownames(test)
test.num = floor(test.num*100 + .5)/100