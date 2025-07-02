########
#   Statistical analyses and figures for: Experimental evidence indicates lake 
#   sturgeon are susceptible to an ecological trap
#######


library(lme4)
library(ggplot2)
library(effects)
library(MuMIn)
library(bbmle)

# Read in csv file, format data (set temperature, lux, and density as factors)
data<-read.csv("Raceway for ch2.csv")
data$y<- 40 - data$Sturgeon # Total of 40 sturgeon wer released in each trial
x <- cbind(data$Sturgeon, data$y)
View(data)
data$Temp <- as.factor(data$Temp)
data$Lux <- as.factor(data$Lux)
data$Density <- as.factor(data$Density)
View(data)

unique(data$Family)

# A preliminary fixed effect model 

glm.2 <- glm(cbind(Sturgeon,y) ~ Temp + Density + Lux + Temp*Density + Temp*Lux +
             Density*Lux, family = binomial, data=data )
summary(glm.2)

nrow(data)

# Create mixed effect generalized linear model looking at sturgeon survival
## as a function of rearing temperature, prey density, light level, and all two-
## way interactions. Family (full siblings) is included as a random effect.
m <- glmer(x ~ Temp + Density + Lux + Temp*Density + Temp*Lux +
                    Density*Lux + (1|Family), family = binomial, data=data )
summary(m)



### Create draft figures showing interactions between important terms in model

# Displaying the interaction of temperature and drifting prey density

effects_temp_density <- effect(term = c("Temp", "Density"),
                            mod = m)
summary(effects_temp_density)


x_temp_density <- as.data.frame(effects_temp_density)


ggplot(data= x_temp_density, aes(x = Temp, y = fit, fill = Density))+
  
  geom_point(data = x_temp_density, aes(x = Temp, y = fit, color = Density), size = 4)+
  
  geom_errorbar(data = x_temp_density, aes(x = Temp, ymin = fit-se, ymax = fit+se), 
                alpha = 0.7, width = 0.3 )+
  
  geom_errorbar(data = x_temp_density, aes(x = Temp, ymin = fit-se, ymax = fit+se, 
                                          color = Density),  alpha = 0.2, colour = NA )+
  geom_line(data = x_temp_density, aes(group = Density, color = Density))+
  labs(x = "Rearing temperature (C)", y = "Proportion Surviving")+
  scale_color_manual(labels = c("High","Low"), values = c("#F8766D","#00BFC4"))+
  theme_classic()



# Displaying the interaction of temperature and light level (lux)

effects_temp_lux <- effect(term = c("Temp", "Lux"),
                               mod = m)
summary(effects_temp_lux)


x_temp_lux <- as.data.frame(effects_temp_lux)


ggplot(data= x_temp_lux, aes(x = Temp, y = fit, fill = Lux))+
  
  geom_point(data = x_temp_lux, aes(x = Temp, y = fit, color = Lux), size = 2)+
  
  geom_errorbar(data = x_temp_lux, aes(x = Temp, ymin = fit-se, ymax = fit+se), 
                alpha = 0.7, width = 0.3 )+
  
  geom_errorbar(data = x_temp_lux, aes(x = Temp, ymin = fit-se, ymax = fit+se, 
                                           color = Lux),  alpha = 0.2, colour = NA )+
  geom_line(data = x_temp_lux, aes(group = Lux, color = Lux))+
  labs(x = "Rearing temperature (C)", y = "Proportion Surviving")+
  scale_color_manual(labels = c("0.0","0.2"), values = c("#F8766D","#00BFC4"))+
  theme_classic()


# Displaying the interaction of drifting prey density and light level (lux)

effects_density_lux <- effect(term = c("Density", "Lux"),
                           mod = m)
summary(effects_density_lux)


x_density_lux <- as.data.frame(effects_density_lux)


ggplot(data= x_density_lux, aes(x = Density, y = fit, fill = Lux))+
  
  geom_point(data = x_density_lux, aes(x = Density, y = fit, color = Lux), size = 2)+
  
  geom_errorbar(data = x_density_lux, aes(x = Density, ymin = fit-se, ymax = fit+se), 
                alpha = 0.7, width = 0.3 )+
  
  geom_errorbar(data = x_density_lux, aes(x = Density, ymin = fit-se, ymax = fit+se, 
                                       color = Lux),  alpha = 0.2, colour = NA )+
  geom_line(data = x_density_lux, aes(group = Lux, color = Lux))+
  
  labs(x = "Density", y = "Proportion Surviving")+
  scale_color_manual(labels = c("0.0","0.2"), values = c("#F8766D","#00BFC4"))+
  scale_x_discrete(labels = c("High","Low"))+
  theme_classic()

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

color_list <- ggplotColours(n=12)
################################################################################
### Repeat above figures with 95% CI, final ###
################################################################################

effects_temp_density <- effect(term = c("Temp", "Density"),
                               mod = m)
summary(effects_temp_density)
View(effects_temp_density)

x_temp_density <- as.data.frame(effects_temp_density)


ggplot(data= x_temp_density, aes(x = Temp, y = fit, fill = Density))+
  
  geom_point(data = x_temp_density, aes(x = Temp, y = fit, color = Density), size = 2)+
  
  geom_errorbar(data = x_temp_density, aes(x = Temp, ymin = lower, ymax = upper), 
                alpha = 0.7, width = 0.3 )+
  labs(x = "Rearing Temperature (C)", y = "Proportion Surviving")+
  scale_fill_discrete(name = "Density", labels = c("High", "Low"))+
  theme_classic()

#####

effects_temp_lux <- effect(term = c("Temp", "Lux"),
                           mod = m)
summary(effects_temp_lux)


x_temp_lux <- as.data.frame(effects_temp_lux)


ggplot(data= x_temp_lux, aes(x = Temp, y = fit, fill = Lux))+
  
  geom_point(data = x_temp_lux, aes(x = Temp, y = fit, color = Lux), size = 2)+
  
  geom_errorbar(data = x_temp_lux, aes(x = Temp, ymin = lower, ymax = upper), 
                alpha = 0.7, width = 0.3 )+
  labs(x = "Temp", y = "Proportion Surviving")+
  scale_fill_discrete(name = "Lux", labels = c("0.00", "0.20"))+
  theme_classic()



ggplot(data= x_temp_lux, aes(x = Lux, y = fit, fill = Temp))+
  
  geom_point(data = x_temp_lux, aes(x = Lux, y = fit, color = Temp), size = 2)+
  
  geom_errorbar(data = x_temp_lux, aes(x = Lux, ymin = lower, ymax = upper), 
                alpha = 0.7, width = 0.3 )+
  labs(x = "Temp", y = "Proportion Surviving")+
  scale_fill_discrete(name = "Temp", labels = c("10C", "18C"))+
  theme_classic()


######

effects_density_lux <- effect(term = c("Density", "Lux"),
                              mod = m)
summary(effects_density_lux)


x_density_lux <- as.data.frame(effects_density_lux)


ggplot(data= x_density_lux, aes(x = Density, y = fit, fill = Lux))+
  
  geom_point(data = x_density_lux, aes(x = Density, y = fit, color = Lux), size = 2)+
  
  geom_errorbar(data = x_density_lux, aes(x = Density, ymin = lower, ymax = upper), 
                alpha = 0.7, width = 0.3 )+
  labs(x = "Density", y = "Proportion Surviving")+
  theme_classic()

ggplot(data= x_density_lux, aes(x = Lux, y = fit, fill = Density))+
  
  geom_point(data = x_density_lux, aes(x = Lux, y = fit, color = Density), size = 2)+
  
  geom_errorbar(data = x_density_lux, aes(x = Lux, ymin = lower, ymax = upper), 
                alpha = 0.7, width = 0.3 )+
  labs(x = "Lux", y = "Proportion Surviving")+
  scale_fill_discrete(name = "Density", labels = c("High", "Low"))+
  theme_classic()


#########################################################################


effects_density<- effect(term = c("Density"),
                              mod = m)
summary(effects_density)


x_density <- as.data.frame(effects_density)


ggplot(data= x_density, aes(x = Density, y = fit))+
  
  geom_point(data = x_density, aes(x = Density, y = fit), size = 2)+
  
  geom_errorbar(data = x_density_lux, aes(x = Density, ymin = fit-CI, ymax = fit+CI), 
                alpha = 0.7, width = 0.3 )+
  
  geom_errorbar(data = x_density_lux, aes(x = Density, ymin = fit-CI, ymax = fit+CI),  alpha = 0.2, colour = NA )+
  labs(x = "Density", y = "Proportion Surviving")+
  theme_classic()






