################ Linked CPES analysis - power calculations sexuality ###################

#Attempting power calculations for linked CPES analysis of sexuality

############################################################################# 

install.packages("pwr")

library(pwr)

#power calculation for 2 proportions with different sample sizes


##### INITIAL APPROACH - finding sample size needed for 80% power https://rpubs.com/mbounthavong/sample_size_power_analysis_R

#different in prevalence between hetero and sexm = 0.4% so:
#alpha/siglevel 0.05
#power 0.8
#p1=0.415
#p2=0.411
power1 = pwr.2p.test(h = ES.h(p1 = 0.415, p2 = 0.411), sig.level = 0.05, power = .80)
#need a sample size of 237849 heterosexual, 4756 sexual minority based on ratio in the survey of 1:0.02
#true sample is 149476:2347 so too small for 80% power

power2 = pwr.2p.test(h = ES.h(p1 = 0.415, p2 = 0.411), sig.level = 0.05, power = .60)
#power is roughly 60% with existing sample size 

plot(power1)


##### APPROACH 2 - dealing with different sample sizes  https://rpubs.com/sypark0215/223385
#stage at diagnosis
n1=149476 #sample size hetero
n2=2347 #sample size sexual minority
p1=0.415 #% diagnosed late hetero
p2=0.411 #% diagnosed late sexual minority
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.008, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 7%


##### REPEAT APPROACH 2 for screening route to diag
n1=132590 #sample size hetero
n2=2112 #sample size sexual minority
p1=0.108 #% screening hetero
p2=0.08 #% screening sexual minority
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.096, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 99%


##### REPEAT APPROACH 2 for emergency presentation route to diag
n1=132590 #sample size hetero
n2=2112 #sample size sexual minority
p1=0.083 #% EP hetero
p2=0.093 #% EP sexual minority
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.035, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 36%


##### REPEAT APPROACH 2 for 1yr survival
n1=188569 #sample size hetero
n2=3873 #sample size sexual minority
p1=0.985 #% survived 1yr hetero
p2=0.988 #% survived 1yr sexual minority
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.026, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 36%


##### REPEAT APPROACH 2 for 5yr survival
n1=74118 #sample size hetero
n2=1331 #sample size sexual minority
p1=0.784 #% survived 5yrs hetero
p2=0.808 #% survived 5yrs sexual minority
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2))); h #h = effect size (0.060, very small)
pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.05) #power 58%