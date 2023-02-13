# library(shiny)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(plyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#options(shiny.sanitize.errors = T)
Norms2010 <- readRDS(file='data/Norms2010.RDS')
Norms2010$ycat <- as.character(Norms2010$Year_)

normalize <- function(x){(x-mean(x))/ifelse(length(x) <=1,1, sd(x))}
normalizep <- function(x){(x)/mean(x)}


n2 <- Norms2010[,c(1:7,which(colnames(Norms2010) %in% 't01'):which(colnames(Norms2010) %in% 't12'))] |> 
  tidyr::pivot_longer(cols  = -c(1:7), names_to = 'month0', values_to = 'Tmp') |> 
  mutate(month = as.numeric(substr(month0, 2,3)), month0=NULL)
n3 <- Norms2010[,c(1,which(colnames(Norms2010) %in% 'Year_'),which(colnames(Norms2010) %in% 'pp01'):which(colnames(Norms2010) %in% 'pp12'))] |> 
  tidyr::pivot_longer(cols  = -c(1:2), names_to = 'month0', values_to = 'Ppt')|> 
  mutate(month = as.numeric(substr(month0, 3,4)), month0=NULL)
n2 <- n2 |> left_join(n3) 
n2 <- n2 |> group_by(Station_ID, month) |> mutate(tnorm = normalize(Tmp), ppositive = mean(Ppt > 0)*100,pnorm = normalizep(Ppt), tmean = mean(Tmp), pmean=mean(Ppt))

ggplot()+
  geom_density(data = subset(n2, pmean >= 20 & pmean < 40 & Ppt > 0), aes(x=pnorm), col='magenta')+
  geom_density(data = subset(n2, pmean >= 20 & pmean < 40), aes(x=pnorm), col='red')+
  geom_density(data = subset(n2, pmean >= 50 & pmean < 100), aes(x=pnorm), col='green')+
  geom_density(data = subset(n2, pmean >= 500 & pmean < 1000), aes(x=pnorm), col='blue')

ggplot()+
  geom_smooth(data = subset(n2, pmean >= 0 & pmean < 100), aes(x=pmean, y=ppositive), col='red')
  
z <- rgamma(50, 3, 10) + rnorm(50, 0, .02)
z
ggplot()+
  geom_density(aes(x= z), col='red')

#install 'fitdistrplus' package if not already installed
install.packages('gamlss.dist')

#load package
library(gamlss.dist)
#fit our dataset to a gamma distribution using mle

z=subset(n2, pmean >= 5 & pmean < 10)$pnorm+0.1
fit <- fitdist(z, distr = "gamma")

#view the summary of the fit 
summary(fit)
plot(fit)
