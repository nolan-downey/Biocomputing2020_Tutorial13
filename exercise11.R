#Exercise 11
#Nolan Downey


#Coefficient declarations
rn = 0.1
rm = 0.1
rna = -0.1
rma = 0.05
K = 1000000

timesteps = 600
t = 2

Ns = data.frame(time=1:timesteps,sim1=rep(0,timesteps))
Ns[1,2:3]=99
Ms = data.frame(time=1:timesteps,sim1=rep(0,timesteps))
Ms[1,2:3]=1

while(t <= timesteps) {
  if(t <= 200) {
     Ns$sim1[t] <- Ns$sim1[t-1] + rn*Ns$sim1[t-1]*(1-((Ns$sim1[t-1]+Ms$sim1[t-1])/K))
     Ms$sim1[t] <- Ms$sim1[t-1] + rm*Ms$sim1[t-1]*(1-((Ms$sim1[t-1]+Ns$sim1[t-1])/K))
    }
  else if(t > 200) {
    Ns$sim1[t] <- Ns$sim1[t-1] + rna*Ns$sim1[t-1]*(1-((Ns$sim1[t-1]+Ms$sim1[t-1])/K))
    Ms$sim1[t] <- Ms$sim1[t-1] + rma*Ms$sim1[t-1]*(1-((Ms$sim1[t-1]+Ns$sim1[t-1])/K))
  }
  t = t + 1
}



Ns2 <- data.frame(time=c(Ns$time,Ms$time),N=c(Ns$sim1,Ms$sim1),sim=rep(c("Non-Mutant Cells","Mutant Cells"),each=timesteps))

library(ggplot2)
ggplot(data=Ns2,aes(x=time,y=N,color=sim))+
  geom_line(size=2) +
  theme_classic() +
  xlab("Time (In Days)") +
  ylab("Number of Cells")

