################ Linked CPES analysis - power calculations language status ###################

#Attempting power calculations for linked CPES analysis of language status

############################################################################# 

install.packages("pwr")

library(pwr)

#power calculation for 2 proportions with different sample sizes


##### APPROACH 2 - dealing with different sample sizes  https://rpubs.com/sypark0215/223385
#stage at diagnosis
n1=149993 #sample size NES
n2=7220 #sample size NNES
p1=0.425 #% diagnosed late NES
p2=0.411 #% diagnosed late NNES
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.028, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 65%


##### REPEAT APPROACH 2 for screening route to diag
n1=31929 #sample size NES (screenable cancers only)
n2=388 #sample size NNES (screenable cancers only)
p1=0.339 #% screening NES
p2=0.299 #% screening NNES
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.086, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 39%


##### REPEAT APPROACH 2 for emergency presentation route to diag
n1=133964 #sample size NES
n2=6399 #sample size NNES
p1=0.082 #% EP NES
p2=0.098 #% EP NNES
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.056, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 99%


##### REPEAT APPROACH 2 for 1yr survival
n1=188892 #sample size NES
n2=9558 #sample size NNES
p1=0.985 #% survived 1yr NES
p2=0.991 #% survived 1yr NNES
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.056, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 99%


##### REPEAT APPROACH 2 for 5yr survival
n1=75084 #sample size NES
n2=3384 #sample size NNES
p1=0.78 #% survived 5yrs NES
p2=0.83 #% survived 5yrs NNES
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.123, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 99%