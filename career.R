ref <- read.csv("C:/Users/User/Downloads/Data Science/Career/data/item_ref1.csv")

code<-item_ref1$TitleCode
long<-item_ref1$TitleLong
min<-item_ref1$SalaryMin
max<-item_ref1$SalaryMax

getwd()
setwd("C:/Users/User/Downloads/Data Science/Career/data")
save(code, long, min, max, file = "itemref1.rda")
rm(code, long, min, max)
load(file = "itemref1.rda")


## Mau Kerja load pairs file

assignment2 <- read.csv("C:/Users/User/Downloads/Data Science/Career/data/item_pairs2.csv")
head(assignment2)
satu<-item_pairs2$Item1
namasatu<-item_pairs1$Item1Name
dua<-item_pairs2$Item2
namadua<-item_pairs2$Item2Name
beza<-item_pairs2$SalaryDiff
gajisatu<-item_pairs2$Salary1
gajidua<-item_pairs2$Salary2
minsatu<-item_pairs2$Salary1Min
mindua<-item_pairs2$Salary2Min
pautan<-item_pairs2$Hyperlink 

##getwd()
##setwd("C:/Users/User/Downloads/Data Science/Career/data")
save(satu, namasatu, dua, namadua, beza, gajisatu, gajidua
     , minsatu, mindua, pautan, file = "itempairs2.rda")
rm(satu, namasatu, dua, namadua, beza, gajisatu, gajidua
   , minsatu, mindua, pautan)

load(file = "itempairs2.rda")


## Jobstreet load pairs file

assignment3 <- read.csv("C:/Users/User/Downloads/Data Science/Career/data/item_pairs1.csv")
head(assignment3)
one<-item_pairs1$Item1
onename<-item_pairs1$Item1Name
two<-item_pairs1$Item2
twoname<-item_pairs1$Item2Name
diff<-item_pairs1$SalaryDiff
onesalary<-item_pairs1$Salary1
twosalary<-item_pairs1$Salary2
onemin<-item_pairs1$Salary1Min
twomin<-item_pairs1$Salary2Min
link<-item_pairs1$Hyperlink

##getwd()
##setwd("C:/Users/User/Downloads/Data Science/Career/data")
save(one, onename, two, twoname, diff, onesalary, twosalary
     , onemin, twomin, link, file = "itempairs1.rda")
rm(one, onename, two, twoname, diff, onesalary, twosalary
   , onemin, twomin,link)

loadd(file = "itempairs1.rda")


## rda to csv
load(file = "C:/Users/User/Downloads/Data Science/Career/data/itemref1.rda")
write.csv(ref, file="out.csv") 

save(item_pairs_1,file="item_pairs_1.rda")

write.csv(item_pairs1, file = "item_pairs_1.csv")