Data<-c(s=14,k=4)

#Given the data is generated from a binomial distribution
#Binomial distribution is generated from a Beta distribution
#Given mu=0.136 and sigma=0.1034
mu<-0.136
sigma<-0.1034

alpha<-mu*((mu*(1-mu)/sigma^2)-1)
beta<-(1-mu)*((mu*(1-mu)/sigma^2)-1)
prior<-c(alpha,beta)

#posterior = prior * likelihood

(Posterior<-c(alpha+Data['k'],Data["s"]-Data["k"]+beta))

#With Bayesian approach
Omega<-Theta<-seq( 0 , 1 , length=101 )

#Now we know omega has mode = 0.4 and k=12
#So calculating alpha_omega and Beta_omega
alpha_omega<-0.4*(12-2)+1
beta_omega<-(1-0.4)*(12-2)+1

#Now we have theta,omega,alpha_omega,beta_omega,k
#Mode of beta used for prior is unknown.
#So we must write a function to determine the prior
k<-20 #Given

joint.prior<-function(omega,theta,alpha_omega,beta_omega,k)
{
  res<-dbeta(omega,alpha_omega,beta_omega)*dbeta(theta,omega*(k-2)+1,(1-omega)*(k-2)+1)
  res
}

#Now crreating the grid
dens<-expand.grid(Omega,Theta)
colnames(dens)<-c('Omega','Theta')

#Now applying the joint prior based on this grid
#I.e. for every value of omega and theta what is the joint prior distirubtion
#Here we need a grid because there are two parameters, theta and omega that are conditional
#Earlier we had only one
dens$Prior<-apply(dens,1,function(z) joint.prior(z[1],z[2],alpha_omega,beta_omega,k))

#Converting to a 2D matrix
Prior.theta.omega<-matrix(dens$Prior,101,101)

#Normalizing
Prior.theta.omega<-Prior.theta.omega/sum(Prior.theta.omega)

#Marginal for Omega colsums
Prior.omega.marginal<-apply(Prior.theta.omega,2,sum)
Prior.omega.marginal<-Prior.omega.marginal/sum(Prior.omega.marginal)*100

#Marginal for Theta rowsums
Prior.theta.marginal<-apply(Prior.theta.omega,1,sum)
Prior.theta.marginal<-Prior.theta.marginal/sum(Prior.theta.marginal)*100 #Theta marginal prior

#Theta is binomial created from a beta with omega and K as parameters
matplot(Omega,cbind(Prior.omega.marginal,
                    dbeta(Omega,alpha_omega,beta_omega)),
        type="l",ylab="Marginal p(omega)")

#Now to find likelihood

likeli<-function(Theta,s,k)
  {
  Theta^k*(1-Theta)^(s-k)
  }

likelihood<-likeli(Theta,14,4)
plot(Theta,likelihood,type="l",ylab="p(y|theta)",main="Likelihood")

Posterior2<-apply(Prior.theta.omega,2,function(z) z*likelihood)
Posterior2<-Posterior2/sum(Posterior2)

Posterior2[1:5,1:5]

persp(Theta,Omega,Posterior2,d=1,theta=-25,phi=20,main="Joint Posterior Distribution")
contour(x=Omega,y=Theta,z=Posterior2,ylab="omega",xlab="theta",
main="Joint Posterior Dist
ribution")
