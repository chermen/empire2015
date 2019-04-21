setwd("~")

library(foreign)
library(bayesm)
library(mcmcplots)
library(memisc)
library(pscl)
library(lattice)
library(effects)
library(visreg)
library(MASS)
library(rsm)
library(reshape2)
library(ggplot2)


#read in the data file
colsum<-read.dta("colsum5.dta")


###############List of variables
#Depedendent variable incidents of indiscriminate violence by the government, GOV_FIRE 

#Independent variables 

#popul - mean population/10000 of a district
#urban - degree of urbanization in a district
#elev - mean elevation/1000 of a district
#OIL - the proportion of villages in a district located on petroleum reserves
#FOREST - the proportion of the village(s) with forested terrain in a district
#g1_02 - the proportion of the largest ethnic group in a district
#REBEL_VIO2 - incidents of rebel violence (not in publication)


####Negative Binomial models

#Model 1
nb1<-glm.nb(GOV_FIRE~popul+urban, control=glm.control(maxit=100), data=colsum )
summary(nb1)
mtable(nb1)

#Model 2
nb2<-glm.nb(GOV_FIRE~ elev+OIL, control=glm.control(maxit=100), data=colsum )
summary(nb2)
mtable(nb2)

#Model 3
nb3<-glm.nb(GOV_FIRE~FOREST, control=glm.control(maxit=100), data=colsum )
summary(nb3)
mtable(nb3)

#Model 4
nb4<-glm.nb(GOV_FIRE~ g1_02, control=glm.control(maxit=100), data=colsum )
summary(nb4)
mtable(nb4)

#Model 5
nb5<-glm.nb(GOV_FIRE~ FOREST+g1_02, control=glm.control(maxit=100), data=colsum )
summary(nb5)
mtable(nb5)


#Model 6
nb6<-glm.nb(GOV_FIRE~(FOREST*g1_02), control=glm.control(maxit=100), 
            data=colsum )
summary(nb6)
mtable(nb6)

#Model 7
nb7<-glm.nb(GOV_FIRE~popul+urban+(FOREST*g1_02), control=glm.control(maxit=100), 
            data=colsum )
summary(nb7)
mtable(nb7)

#Model 8
nb8<-glm.nb(GOV_FIRE~elev+OIL+(FOREST*g1_02), 
            control=glm.control(maxit=100), data=colsum )
summary(nb8)
mtable(nb8)

#Model 9
nb9<-glm.nb(GOV_FIRE~meanpop+urban+OIL+(FOREST*g1_02), 
              control=glm.control(maxit=100), data=colsum )
summary(nb9)
mtable(nb9)

#Model for creating marginal plots
nb10<-glm.nb(GOV_FIRE~meanpop+urban+OIL+FOREST+g1_02, 
            control=glm.control(maxit=100), data=colsum )
summary(nb10)
mtable(nb10)

plot(effect("FOREST", xlevels=20, nb10, xlab="Percentage Forested Terrain", 
            ylab="Incidents of Indiscriminate Violence", main="")) 

plot(effect("g1_02", xlevels=20, nb10, xlab="Proportion of Ethnicity", 
            ylab="Incidents of Indiscriminate Violence", main="")) 



visreg2d(nb10, xvar="FOREST", yvar="g1_02", type=c("conditional"),
         trans=I, scale=c("response"), main="Interaction of Forest and Ethnicity",
         plot.type="image", 
         cond=list(), print.cond=FALSE, whitespace=0.2)

myvars <- c("GOV_FIRE","meanpop","urban","elev","OIL","FOREST","g1_02")
newdat <- colsum[myvars]
ndat<-na.omit(newdat)


dat <- data.frame(g1_02 = rep(seq(from = min(ndat$g1_02), 
                                  to = max(ndat$g1_02), length.out = 1000)),
                  FOREST = rep(seq(0.0, 0.95, 0.1)),meanpop=mean(ndat$meanpop),
                  elev=mean(ndat$elev),OIL=mean(ndat$OIL),urban=mean(ndat$urban))

dat <- cbind(dat, predict(nb10, dat, type = "response", se.fit=T))


library(akima)
library(plot3D)
wframe<-interp(dat$g1_02,dat$FOREST,dat$fit)
persp3D(wframe$x,wframe$y,wframe$z)


###Or

visreg2d(nb10,  "FOREST", "g1_02", type=c("conditional"),
         trans=I, scale=c("response"), xlab=list(label="Deprivation"), 
         ylab="Mobilization",main="", zlab=list(label="Riots", cex=1, rot=90), phi = 15, theta = 60,    
         plot.type="persp", nn=49,
         cond=list(), print.cond=FALSE, whitespace=0.1)

        



        ###Bayesian models
        ###Using metropolis random walk alg from bayesm package

colsum$Population<-colsum$meanpop/10000
colsum$Elevation<-colsum$elev/1000

        nobs <- 200       # Number of observations
        nvar=8            # Number of X variables
        Vbeta = diag(nvar)*0.01
        colsum$interaction<-colsum$g1_02*colsum$FOREST # Interaction term
        
        # Construct the regdata (containing X)
        simnegbindata = NULL
        #beta = c(coefficients(nb6.4))
        beta<-c(0,0,0,0,0,0,0,0)
        X = cbind(rep(1,nobs),colsum$Population,colsum$urban,colsum$Elevation,colsum$OIL,
                  colsum$FOREST,colsum$g1_02,colsum$interaction)
        simnegbindata = list(y=colsum$GOV_FIRE, X=X, beta=beta)
        Data1 = simnegbindata
        Mcmc1 = list(R=100000)
        
        
        out = rnegbinRw(Data=Data1,Mcmc=Mcmc1)
        plot(out$betadraw)
        betas<-as.matrix(out$betadraw)
        colnames(betas)[1:8] <- c("Intercept","Population(log)","Urban","Elevation","Oil",
                                  "Forest","Ethnicity","Forest:Ethnicity")
        denplot(betas, parms=c("Forest","Ethnicity", "Forest:Ethnicity"))
        caterplot(betas, parms=c("Population(log)","Urban","Elevation","Oil","Forest",
                                 "Ethnicity", "Forest:Ethnicity"), 
                  horizontal=T,labels.loc="above", denstrip=T)
        
        
        betasdat<-as.data.frame(betas)
        forest<-betasdat[c(6)]
        forgg<-melt(forest)
        head(forgg)
        m <- ggplot(forgg, aes(x = value))
        m + geom_density(adjust=5)+
          xlab("Posterior Estimates") +
          ylab("Density") +
          ggtitle("Forest")
        ggsave("forestnb.pdf")
        dev.off()
        
        ethnic<-betasdat[c(7)]
        ethgg<-melt(ethnic)
        head(ethgg)
        m <- ggplot(ethgg, aes(x = value))
        m + geom_density(adjust=5)+
          xlab("Posterior Estimates") +
          ylab("Density") +
          ggtitle("Ethnicity")
        ggsave("ethnicnb.pdf")
        dev.off()
        
        inter<-betasdat[c(8)]
        intgg<-melt(inter)
        head(intgg)
        m <- ggplot(intgg, aes(x = value))
        m + geom_density(adjust=5)+
          xlab("Posterior Estimates") +
          ylab("Density") +
          ggtitle("Interaction of Forest and Ethnicity")
        ggsave("internb.pdf")
        dev.off()
        citation(package="bayesm")
        cat("Summary of alpha/beta draw",fill=TRUE)
        summary(out$alphadraw,tvalues=alpha)
        summary(out$betadraw,tvalues=beta)
        if(0){
          ## plotting examples
          plot(out$betadraw)
        }
        
        ##Table
        
        beta.sims <- t(out$betadraw) # transpose of b2$beta
        apply(beta.sims, 1, quantile, probs = c(0.025, 0.975))
        apply(beta.sims, 1, mean)
       
        

###Zero-Inflated Negative Binomial Models (not included in the publication)


summary(z1 <- zeroinfl(GOV_FIRE~FOREST+g1_02 | REB_VIOL_2, data = colsum, 
                       dist = "negbin"))        
summary(z2 <- zeroinfl(GOV_FIRE~(FOREST*g1_02) | REB_VIOL_2, 
                       data = colsum, dist = "negbin"))
summary(z9 <- zeroinfl(GOV_FIRE~popul+urban+elev+OIL+FOREST+g1_02+
                         (FOREST*g1_02) | REB_VIOL_2, data = colsum, dist = "negbin"))
summary(p9 <- zeroinfl(GOV_FIRE~meanpop+urban+OIL+
                         (FOREST*g1_02) | REB_VIOL_2, data = colsum, dist = "poisson"))

###compare NB and ZINB
vuong(nb9, z9)

###hurdle models (not included in the publication)

h1<- hurdle(GOV_FIRE~meanpop+urban+elev+OIL+(FOREST*g1_02),
            data=colsum, control=hurdle.control(maxit=200), dist="negbin")
summary(h1)
        