##### Overview of R codes, scripts and packages used for the statistical analysis in Parmentier et al. – Ecological Informatics

##### R packages and datasets

# Load needed R packages:
library(readxl)
library(vegan) # For calculating diversity indices: S, H
library(ggplot2)
library(glmmTMB)
library(performance) # For model checks; check_collinearity, check_zeroinflation, etc.
library(here)
library(dplyr)
library(tidyr)

# First load all needed datasets in R:
My dataset<- read_excel(  here("data/my dataset.xlsx") #change “my dataset” in the name of the dataset (in blue)


##### Assessment of management effect on pollinator diversity
# In the paper the following diversity indices were frequently used related to pollinator diversity: abundance (N), number of species (S), Shannon-diversity index (H)

PolAlldiv<- PollAll2122
PolAlldat<- PollAll2122dat

#calculate S and H:
PolAlldiv$S <- specnumber(PolAlldat)
PolAlldiv$H <- diversity(PolAlldat)

#run GLMM models:
# Basic models 

PolAllN<-glmmTMB(N~Site+(1|Year)+(1|Location), family=poisson() , data=PollAlldiv)
summary(PolAllN)

PolAllS<-glmmTMB(S~Site+(1|Year)+(1|Location), family=poisson() , data=PollAlldiv)
summary(PolAllS)

PolAllH<-glmmTMB(H~Site+(1|Year)+(1|Location), family= gaussian , data=PollAlldiv)
summary(PolAllH)

# Models including spatiotemporal mowing effects (mowzone and Timing):

PolAllN <-glmmTMB(N~Site+Mowzone+Mowdate + (1|Year)+(1|Location), family=nbinom1 , data= PolAlldiv)
summary(PolAllN)

PolAllS <-glmmTMB(S~Site+Mowzone+Mowdate + (1|Year)+(1|Location), family=poisson , data= PolAlldiv)
summary(PolAllS)

PolAllH <-glmmTMB(H~Site+Mowzone+Mowdate +(1|Location), family=gaussian, data= PolAlldiv)
summary(PolAllH)

# Model checks:

check_distribution(PolAllN)      # change N by S or H for different diversity models
check_collinearity(PolAllN) 
check_zeroinflation(PolAllN) 
check_autocorrelation(PolAllN)
Assessment of management effect on microclimate as DCT power frequency
# Composing the datasets with thermal frequency bands (freq1, freq2, freq3), with needed model factors:

Insect_freq_test1<-Power300_freq1
Insect_freq_test1$Location<-as.factor(Insect_freq_test1$Location)
Insect_freq_test1$Site<-as.factor(Insect_freq_test1$Site)
Insect_freq_test1$Timing<-as.factor(Insect_freq_test1$Timing)
Insect_freq_test1$Part<-as.factor(Insect_freq_test1$Part)
Insect_freq_test1$Date2<-as.factor(Insect_freq_test1$Date)
Insect_freq_test1$pos<- numFactor(Insect_freq_test1$x_lat4, Insect_freq_test1$y_lon4)
View(Insect_freq_test1)
glimpse(Insect_freq_test1)

Insect_freq_test2<-Power300_freq2
Insect_freq_test2$Location<-as.factor(Insect_freq_test2$Location)
Insect_freq_test2$Site<-as.factor(Insect_freq_test2$Site)
Insect_freq_test2$Timing<-as.factor(Insect_freq_test2$Timing)
Insect_freq_test2$Part<-as.factor(Insect_freq_test2$Part)
Insect_freq_test2$Date2<-as.factor(Insect_freq_test2$Date)
Insect_freq_test2$pos<- numFactor(Insect_freq_test2$x_lat4, Insect_freq_test2$y_lon4)
View(Insect_freq_test2)
glimpse(Insect_freq_test2)

Insect_freq_test3<-Power300_freq3
Insect_freq_test3$Location<-as.factor(Insect_freq_test3$Location)
Insect_freq_test3$Site<-as.factor(Insect_freq_test3$Site)
Insect_freq_test3$Timing<-as.factor(Insect_freq_test2$Timing)
Insect_freq_test3$Part<-as.factor(Insect_freq_test3$Part)
Insect_freq_test3$Date2<-as.factor(Insect_freq_test3$Date)
Insect_freq_test3$pos<- numFactor(Insect_freq_test3$x_lat4, Insect_freq_test3$y_lon4)
View(Insect_freq_test3)
glimpse(Insect_freq_test3)

# Statistical tests with GLMMs:

Ins22_freq.2 <- glmmTMB(freq2 ~ Site +(1|Location) +(1|Date2)+exp(pos + 0 |Location), family=beta_family(), data=Insect_freq_test1)
summary (Ins22_freq.2)  
# change family to check different model families: Gaussian, Poisson, gamma, nbinom1, nbinom2 
# change ‘freq2’ in ‘freq1’ or ‘freq3’ to check effects of other DCT power frequency bands

# Model checks:

check_distribution(Ins22xfreq1.x)      # change 1 by 2 or 3 for different patch sizes
check_collinearity(Ins22xfreq1.x) 
check_dag(Ins22xfreq1.x)
check_autocorrelation(Ins22xfreq1.x)



##### Assessment of microclimatic effects on insect diversity

# Composing the different overlay datasets of pollinators in thermal patches (2x2, 4x4 and 8x8 m), with needed model factors:

Insectxfreq_test1<-insect_thermal2x2_LP
Insectxfreq_test1$Location<-as.factor(Insectxfreq_test1$Location)
Insectxfreq_test1$Site<-as.factor(Insectxfreq_test1$Site)
Insectxfreq_test1$Timing<-as.factor(Insectxfreq_test1$Timing)
Insectxfreq_test1$Part<-as.factor(Insectxfreq_test1$Part)
Insectxfreq_test1$Date<-as.factor(Insectxfreq_test1$Dat)
Insectxfreq_test1$pos<- numFactor(Insectxfreq_test1$x_lat, Insectxfreq_test1$y_lon)
View(Insectxfreq_test1)
glimpse(Insectxfreq_test1)

Insectxfreq_test2<-insect_thermal4x4_LP
Insectxfreq_test2$Location<-as.factor(Insectxfreq_test2$Location)
Insectxfreq_test2$Site<-as.factor(Insectxfreq_test2$Site)
Insectxfreq_test2$Timing<-as.factor(Insectxfreq_test2$Timing)
Insectxfreq_test2$Part<-as.factor(Insectxfreq_test2$Part)
Insectxfreq_test2$Date2<-as.factor(Insectxfreq_test2$Dat)
Insectxfreq_test2$pos<- numFactor(Insectxfreq_test2$x_lat, Insectxfreq_test2$y_lon)
View(Insectxfreq_test2)
glimpse(Insectxfreq_test2)
Insectxfreq_test3<-insect_thermal8x8_LP
Insectxfreq_test3$Location<-as.factor(Insectxfreq_test3$Location)
Insectxfreq_test3$Site<-as.factor(Insectxfreq_test3$Site)
Insectxfreq_test3$Timing<-as.factor(Insectxfreq_test3$Timing)
Insectxfreq_test3$Part<-as.factor(Insectxfreq_test3$Part)
Insectxfreq_test3$Date2<-as.factor(Insectxfreq_test3$Dat)
Insectxfreq_test3$pos<- numFactor(Insectxfreq_test3$x_lat, Insectxfreq_test3$y_lon)
View(Insectxfreq_test3)
glimpse(Insectxfreq_test3)

# Statistical tests with GLMMs:
Ins22xfreq1.x <- glmmTMB(N_patch ~ freq1*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test1)
Ins22xfreq1.x <- glmmTMB(N_patch ~ freq2*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test1)
Ins22xfreq1.x <- glmmTMB(N_patch ~ freq3*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test1)


Ins22xfreq1.x <- glmmTMB(S_patch ~ freq1*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test1)
Ins22xfreq1.x <- glmmTMB(S_patch ~ freq2*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test1)
Ins22xfreq1.x <- glmmTMB(S_patch ~ freq3*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test1)

# change x to test different model families: Gaussian, Poisson, gamma, nbinom1, nbinom2 

summary(Ins22xfreq1.x)

Ins22xfreq2.x <- glmmTMB(N_patch ~ freq1*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test2)
Ins22xfreq2.x <- glmmTMB(N_patch ~ freq2*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test2)
Ins22xfreq2.x <- glmmTMB(N_patch ~ freq3*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test2)
Ins22xfreq2.x <- glmmTMB(S_patch ~ freq1*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test2)
Ins22xfreq2.x <- glmmTMB(S_patch ~ freq2*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test2)
Ins22xfreq2.x <- glmmTMB(S_patch ~ freq3*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test2)


# change x to test different model families: Gaussian, Poisson, gamma, nbinom1, nbinom2 

summary(Ins22xfreq2.x)

Ins22xfreq3.x <- glmmTMB(N_patch ~ freq1*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test2)
Ins22xfreq3.x <- glmmTMB(N_patch ~ freq2*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test2)
Ins22xfreq3.x <- glmmTMB(N_patch ~ freq3*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test2)

Ins22xfreq3.x <- glmmTMB(S_patch ~ freq1*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test2)
Ins22xfreq3.x <- glmmTMB(S_patch ~ freq2*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test2)
Ins22xfreq3.x <- glmmTMB(S_patch ~ freq3*Site*Part + Timing +(1|Location) +(1|Date)+exp(pos +0|Location), family=Poisson(), data=Insectxfreq_test2)

# change x to have different model family distributions tested: Gaussian, Poisson, gamma, nbinom1, nbinom2 
# Best final models without interaction effects for 

summary(Ins22xfreq3.x)

# Model checks:

check_distribution(Ins22xfreq1.x)      # change 1 by 2 or 3 for different patch sizes
check_collinearity(Ins22xfreq1.x)
check_dag(Ins22xfreq1.x)
check_autocorrelation(Ins22xfreq1.x)
check_zeroinflation(Ins22xfreq1.x)
check_outliers(Ins22xfreq1.x)


# Example of drawing plots to visualise effects, via GGplot:
ggplot(data = insectxthermal2x2_LP, 
  aes(x = freq2, y = H, color = Timing, fill = Timing)) +  
# change” color = Part”/ ” fill = Part” to visualise different effects of mowing zones
  geom_point(alpha = 0.4) +                           
  geom_smooth(method ="glm", se = TRUE, alpha = 0.15) + facet_wrap(~Site)
theme_minimal() +  labs(title = " DCT Power frequency bands per pollinator abundance (N), 2x2m plots",    x = "N patch",    y = "frequency (DCT power)"
  )



