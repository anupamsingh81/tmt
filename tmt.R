a1 = rnorm(1000,0.04,0.005)
b1=rnorm(1000,1.34,0.110)
c1= rnorm(1000,1.91,0.270)
d1 = rnorm(1000,0.64,0.159)

Age = rnorm(1000,60,15)
Male = c(rep(1,672),rep(0,328))
TypicalAngina = c(rep(1,530),rep(0,470))

AtypicalAngina = c(rep(0,530),rep(1,267),rep(0,203))

z= -4.37+ a1*Age+b1*Male+c1*TypicalAngina+d1*AtypicalAngina


pr = 1/(1+exp(-z))         # pass through an inv-logit function
 y = rbinom(1000,1,pr)      # bernoulli response variable

   #now feed it to glm:
 df = data.frame(y,Age,Male,TypicalAngina,AtypicalAngina)
 glm( y~.,data=df,family="binomial")

 library(rms)
 
 
 
 e <- datadist(df)
 options(datadist='e') 
 modx = lrm(y~.,data = df)
 modx
 summary(modx)
 nx =nomogram(modx,fun=plogis,
             fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
             funlabel="Risk of CAD")
 plot(nx)
 datadist(hematemesis1)