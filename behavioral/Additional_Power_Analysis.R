require(pwr)
##behavioral, NCC n = 27
##pre-post change
#d in tutoring .698
pwr.t.test(d=0.698, n = 52, sig.level = 0.05,type = "paired",alternative = "two.sided")
pwr.t.test(d=0.698, n = 27, sig.level = 0.05,type = "paired",alternative = "two.sided")
#correlation pre-post
#d in tutoring 2.28
pwr.r.test(n=52,r=-0.752,sig.level = 0.05,alternative = "two.sided")
pwr.r.test(n=27,r=-0.752,sig.level = 0.05,alternative = "two.sided")
##SEM 
#NCC n = 24
##pwrSEM: https://yilinandrewang.shinyapps.io/pwrSEM/
#power = 0.44
2*0.216/sqrt(1-0.216^2)
2*(-0.219)/sqrt(1-0.219^2)

##imaging NCC n = 17
pwr.r.test(n=17,r=.55,sig.level = 0.05,alternative = "two.sided")
pwr.r.test(n=17,r=.372,sig.level = 0.05,alternative = "two.sided")
pwr.r.test(n=17,r=.338,sig.level = 0.05,alternative = "two.sided")
pwr.r.test(n=17,r=.481,sig.level = 0.05,alternative = "two.sided")

pwr.r.test(n=17,r=.361,sig.level = 0.05,alternative = "two.sided")
pwr.r.test(n=17,r=.449,sig.level = 0.05,alternative = "two.sided")

pwr.r.test(n=38,r=.55,sig.level = 0.05,alternative = "two.sided")
pwr.r.test(n=38,r=.372,sig.level = 0.05,alternative = "two.sided")
pwr.r.test(n=38,r=.338,sig.level = 0.05,alternative = "two.sided")
pwr.r.test(n=38,r=.481,sig.level = 0.05,alternative = "two.sided")

pwr.r.test(n=38,r=.361,sig.level = 0.05,alternative = "two.sided")
pwr.r.test(n=38,r=.449,sig.level = 0.05,alternative = "two.sided")
