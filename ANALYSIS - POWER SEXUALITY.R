################ Linked CPES analysis - power calculations sexuality ###################

#Attempting power calculations for linked CPES analysis of sexuality

#Created by Lizzie Augarde July 2024
#Change log: 
############################################################################# 

install.packages("pwr")

library(pwr)

#power calculation for 2 proportions with different sample sizes


#####stage at diagnosis - are sexual minority respondents more likely to be diagnosed at stage 3 or 4 than heterosexual?
n1=149476 #sample size hetero
n2=2347 #sample size sexual minority
p1=0.415 #% diagnosed late hetero
p2=0.411 #% diagnosed late sexual minority
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.008, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 7%


##### Screening route to diag - are sexual minority respondents more likely to be diagnosed via screening than heterosexual?
n1=32247 #sample size hetero (screenable cancers only)
n2=397 #sample size sexual minority (screenable cancers only)
p1=0.339 #% screening hetero
p2=0.302 #% screening sexual minority
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.079, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 35%


##### Emergency presentation route to diag - are sexual minority respondents more likely to be diagnosed via EP than heterosexual?
n1=132590 #sample size hetero 
n2=2112 #sample size sexual minority 
p1=0.083 #% EP hetero
p2=0.093 #% EP sexual minority
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.035, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 36%


##### 1yr survival - are sexual minority respondents more likely to survive at least one year than heterosexual?
n1=188569 #sample size hetero
n2=3873 #sample size sexual minority
p1=0.985 #% survived 1yr hetero
p2=0.988 #% survived 1yr sexual minority
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.026, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 36%


##### 5yr survival - are sexual minority respondents more likely to survive at least 5 years than heterosexual?
n1=74118 #sample size hetero
n2=1331 #sample size sexual minority
p1=0.784 #% survived 5yrs hetero
p2=0.808 #% survived 5yrs sexual minority
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.060, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 58%