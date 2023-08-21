##Weng Yuen Chin (MRes EEC 22-23)
##R script for the summer project data analysis

##Load all required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(pwr)
library(lme4)
library(ggpubr)

##1.Temperature trial
#Load dataset (save the "Temperature"" sheet as csv from the raw data)
df <- read.csv(file="/Users/User/Desktop/R/temp.csv", header = T) #Please modify the file path accordingly
#Data wrangling
df <- df %>% filter(Hot.Temp...C. != "NA", Cold.Temp...C. != "NA", RH.... != "NA", Position != "NA",
                    Hot.Temp...C. != "28 (NA)", Hot.Temp...C. != "22.7 (NA)",Hot.Temp...C. != "23.1 (NA)",
                    Hot.Temp...C. != "23.1 (NA)", Hot.Temp...C. != "Screen Died (NA)", 
                    Hot.Temp...C. != "23.4 (NA)",Hot.Temp...C. != "24.5 (NA)",Hot.Temp...C. != "26.2 (NA)",
                    Hot.Temp...C. != "24.2 (NA)",Hot.Temp...C. != "")
df$Controller.Temp <- as.numeric(df$Controller.Temp...C.)
df$Hot.Temp <- as.numeric(df$Hot.Temp...C.)
df$Cold.Temp <- as.numeric(df$Cold.Temp...C.)
df$RH <- as.numeric(df$RH....)
df$Experiment.Date <- as.Date(df$Experiment.Date, "%d/%m/%Y")
df$Observed.Date <- as.Date(df$Observed.Date, "%d/%m/%Y")
#Calculate the temperature difference and convert the values to cumulative probability
df$Temp <- numeric(nrow(df))
for (i in 1:nrow(df)) {
  if (df$Position[i] == "Cold End") {
    df$Temp[i] <- df$Cold.Temp[i] - df$Controller.Temp[i]
  }
  if (df$Position[i] == "Hot End") {
    df$Temp[i] <- df$Hot.Temp[i] - df$Controller.Temp[i]
  }
}
df$probit <- pnorm(df$Temp)
#Probit regression
#Overall data
glmer_pro <- glmer(probit ~ Controller.Temp + (1|Site), data = df, family = binomial("probit"))
#Filter out data with sex and fit another probit regression
df_sex <- df %>% filter(Sex != "NA")
glmersex <- glmer(probit ~ Controller.Temp + (1|Sex), data = df_sex, family = binomial("probit"))
#Calculate the optimum temperature
op_T_pro <- -(fixef(glmer_pro)[1])/(fixef(glmer_pro)[2])
#Plot
ggplot(df, aes(x=Controller.Temp, y=probit)) + geom_point() +
  stat_smooth(method = "glm", color = "black", se = T,
              method.args = list(family=binomial(link = "probit"))) + 
  theme_classic() +
  labs(x = "Controller temperature (°C)",
       y = "Cumulative probability")


##2. Field temperature
#Load dataset (save the "Field Temp"" sheet as csv from the raw data)
field <- read.csv(file = "/Users/User/Desktop/R/field.csv", header = T) #Please modify the file path accordingly
#Data wrangling
field$Habitat <- character(nrow(field))
for (i in 1:nrow(field)) { #Loop to categorise the habitat type
  if (field$Site[i] == "Long Beach") {
    field$Habitat[i] <- "Degraded"
  }
  if (field$Site[i] != "Long Beach") {
    field$Habitat[i] <- "Native"
  }
}
#Create two data frames for both habitats
field_native <- field %>% filter(Habitat == "Native")
field_degraded <- field %>% filter(Habitat == "Degraded")
#Create data frames for each distance interval and compare using t-test
#1 m
field_native_1 <- field_native %>% filter(Distance.from.Coast..m. == 1) %>% 
  select(Min.Temp.on.Surface..In.)%>% 
  na.omit()
field_degraded_1 <- field_degraded %>% filter(Distance.from.Coast..m. == 1) %>% 
  select(Min.Temp.on.Surface..In.)%>% 
  na.omit()
t.test(field_native_1$Min.Temp.on.Surface..In., mu = op_T_pro) #Compare to the optimum temperature
t.test(field_degraded_1$Min.Temp.on.Surface..In., mu = op_T_pro)
t.test(field_native_1$Min.Temp.on.Surface..In., field_degraded_1$Min.Temp.on.Surface..In.) #Compare both habitats
#25 m
field_native_25 <- field_native %>% filter(Distance.from.Coast..m. == 25) %>% 
  select(Min.Temp.on.Surface..In.)%>% 
  na.omit()
field_degraded_25 <- field_degraded %>% filter(Distance.from.Coast..m. == 25) %>% 
  select(Min.Temp.on.Surface..In.)%>% 
  na.omit()
t.test(field_native_25$Min.Temp.on.Surface..In., mu = op_T_pro)
t.test(field_degraded_25$Min.Temp.on.Surface..In., mu = op_T_pro)
t.test(field_native_25$Min.Temp.on.Surface..In., field_degraded_25$Min.Temp.on.Surface..In.)
#50 m
field_native_50 <- field_native %>% filter(Distance.from.Coast..m. == 50) %>% 
  select(Min.Temp.on.Surface..In.)%>% 
  na.omit()
field_degraded_50 <- field_degraded %>% filter(Distance.from.Coast..m. == 50) %>% 
  select(Min.Temp.on.Surface..In.)%>% 
  na.omit()
t.test(field_native_50$Min.Temp.on.Surface..In., mu = op_T_pro)
t.test(field_degraded_50$Min.Temp.on.Surface..In., mu = op_T_pro)
t.test(field_native_50$Min.Temp.on.Surface..In., field_degraded_50$Min.Temp.on.Surface..In.)


##3. Food trial
#Power test before trial
pwr.anova.test(k = 4, n = NULL, f = 0.5, sig.level = 0.05, power = 0.8)
#Load dataset (save the "Diet"" sheet as csv from the raw data)
food <- read.csv(file="/Users/User/Desktop/R/food.csv", header = T) #Please modify the file path accordingly
#Data wrangling
food <- food[-(1:27),]
food$Food.Type <- as.factor(food$Food.Type)
food$Food.consumed <- as.numeric(food$Food.consumed)
food <- food %>% filter(Row != "NA", Genus != "Control", Food.Weight..g. != "NA",
                        Remark != "Dead when observed in the morning",
                        Remark != "Active at night, dead in the morning")
food_sex <- food %>% filter(Sex != "NA") #Filter out entries that contain the sex of the crickets
#Normality Test
food_Jam <- food %>% filter(Food.Type == "Jam") %>% select(Food.consumed)
shapiro.test(food_Jam$Food.consumed)
food_HD <- food %>% filter(Food.Type == "Hotdog") %>% select(Food.consumed)
shapiro.test(food_HD$Food.consumed)
food_BT <- food %>% filter(Food.Type == "Black Triggerfish") %>% select(Food.consumed)
shapiro.test(food_BT$Food.consumed)
food_SLC <- food %>% filter(Food.Type == "Sally Lightfoot Crab") %>% select(Food.consumed)
shapiro.test(food_SLC$Food.consumed)
#Data analysis
foodm <- lmer(Food.consumed ~ 0 + Food.Type + (1|Site), data = food)
lmer(Food.consumed ~ 0 + Food.Type + (1|Site) + (1|Sex), data = food_sex)
#Plot
ggboxplot(food, x = "Food.Type", y = "Food.consumed") + 
  labs(y = "Food consumed (g)",
       x = "Food type") +
  scale_x_discrete(labels=c("Black triggerfish","Hotdog","Jam","Sally Lightfoot crab"))


##4. Competition
#Load dataset (save the "Competition"" sheet as csv from the raw data)
com <- read.csv(file = "/Users/User/Desktop/R/comp.csv", header = T) #Please modify the file path accordingly
#Data wrangling
com$Deployed.Date <- as.Date(com$Deployed.Date, "%d/%m/%Y")
com$Collection.Date <- as.Date(com$Collection.Date, "%d/%m/%Y")
com$Distance.from.Coast..m. <- as.numeric(com$Distance.from.Coast..m.)
com$Habitat <- character(nrow(com)) 
for (i in 1:nrow(com)) { #Loop to categorise the habitat type
  if (com$Site[i] == "Long Beach") {
    com$Habitat[i] <- "Degraded"
  }
  if (com$Site[i] != "Long Beach") {
    com$Habitat[i] <- "Native"
  }
}
com <- com %>% filter(First.Species.to.Arrive != "NA")
com$First <- numeric(nrow(com))
for (i in 1:nrow(com)) { #Loop to categorise whether the scaly crickets arrived first
  if (com$First.Species.to.Arrive[i] == "Discophallus sp.") {
    com$First[i] <- 1
  }
  else {
    com$First[i] <- 0
  }
}
#Create two data frames for both habitats
com_p <- com %>% filter(Habitat == "Native")
com_d <- com %>% filter(Habitat == "Degraded")
#Pool all data without taking distance intervals into account
com_count <- com %>% group_by(Habitat,First,Distance.from.Coast..m.) %>% count()
com_count_site <- com %>% group_by(Site,First,Distance.from.Coast..m.) %>% count()
com_count_0 <- com %>% filter(First==0) %>% group_by(Habitat,First,Distance.from.Coast..m.) %>% 
  count()
com_count_1 <- com %>% filter(First==1) %>% group_by(Habitat,First,Distance.from.Coast..m.) %>% 
  count()
com_count_both <- rbind(com_count_0,com_count_1)
com_count_pool <- com %>% group_by(Habitat,First) %>% count() #Pooled data
#Analysis
#Test for each distance interval
#Scaly crickets in both habitats
#0m
fisher.test(matrix(c(1, 6, 2, 0), nrow = 2, byrow = TRUE))
#10m
fisher.test(matrix(c(1, 4, 2, 2), nrow = 2, byrow = TRUE))
#20m
fisher.test(matrix(c(0, 3, 3, 3), nrow = 2, byrow = TRUE))
#30m
fisher.test(matrix(c(0, 3, 3, 3), nrow = 2, byrow = TRUE))
#40m
fisher.test(matrix(c(0, 3, 3, 3), nrow = 2, byrow = TRUE))
#Banded crickets in both habitats
#0m
fisher.test(matrix(c(1, 0, 2, 6), nrow = 2, byrow = TRUE))
#10m
fisher.test(matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE))
#20m
fisher.test(matrix(c(2, 1, 1, 5), nrow = 2, byrow = TRUE))
#30m
fisher.test(matrix(c(1, 2, 2, 4), nrow = 2, byrow = TRUE))
#40m
fisher.test(matrix(c(0, 2, 3, 4), nrow = 2, byrow = TRUE))

#Compare both species in each habitat
#0m
#Degraded
fisher.test(matrix(c(1, 1, 2, 2), nrow = 2, byrow = TRUE))
#Native
fisher.test(matrix(c(6, 0, 3, 9), nrow = 2, byrow = TRUE)) #Significant
#10m
#Degraded
fisher.test(matrix(c(1, 1, 2, 2), nrow = 2, byrow = TRUE))
#Native
fisher.test(matrix(c(4, 2, 5, 7), nrow = 2, byrow = TRUE))
#20m
#Degraded
fisher.test(matrix(c(2, 0, 1, 3), nrow = 2, byrow = TRUE))
#Native
fisher.test(matrix(c(3, 1, 6, 8), nrow = 2, byrow = TRUE))
#30m
#Degraded
fisher.test(matrix(c(1, 0, 2, 3), nrow = 2, byrow = TRUE))
#Native
fisher.test(matrix(c(3, 2, 6, 7), nrow = 2, byrow = TRUE))
#40m
#Degraded
fisher.test(matrix(c(0, 0, 3, 3), nrow = 2, byrow = TRUE))
#Native
fisher.test(matrix(c(3, 2, 6, 7), nrow = 2, byrow = TRUE))

#Analysis for counts without taking distance interval into account
#Scaly crickets in both habitats
fisher.test(matrix(c(19, 2, 11, 13), nrow = 2, byrow = TRUE)) #Significant
#Banded crickets in both habitats
fisher.test(matrix(c(5, 7, 10, 23), nrow = 2, byrow = TRUE))

#Compare both species in each habitat
#Native
fisher.test(matrix(c(19, 7, 11, 23), nrow = 2, byrow = TRUE)) #Significant
#Degraded
fisher.test(matrix(c(2, 5, 13, 10), nrow = 2, byrow = TRUE))
#Plot
com_count_pool_plot <- com_count_pool
com_count_pool_plot$First <- as.character(com_count_pool_plot$First)
ggplot(com_count_pool_plot, aes(x=First, y=n, group=Habitat, color=Habitat,fill = First)) + 
  geom_bar(stat = "identity", position = position_dodge(0.925), linewidth = 1.25) +
  labs(x = "Cricket species",
       y = "Frequency",
       fill = "Cricket species") +
  scale_color_manual(values = c("black","red")) +
  scale_fill_discrete(labels=c('Banded cricket', 'Scaly cricket'))+
  scale_x_discrete(labels=c('Banded cricket', 'Scaly cricket'))+
  theme_classic() +
  ylim(0,20)


##5. Predation
#Load dataset (save the "Predation"" sheet as csv from the raw data)
pred <- read.csv(file = "/Users/User/Desktop/R/pred.csv", header = T) #Please modify the file path accordingly
#Data wrangling
pred$Distance.from.Coast <- as.numeric(pred$Distance.from.Coast)
pred <- pred %>% filter(Eaten.or.Not.in.Recording != "NA")
pred$eaten <- numeric(nrow(pred))
for (i in 1:nrow(pred)) { #Loop to categorise whether the crickets were eaten or not
  if (pred$Eaten.or.Not.in.Recording[i] == "Yes") {
    pred$eaten[i] <- 1
  }
  if (pred$Eaten.or.Not.in.Recording[i] == "No") {
    pred$eaten[i] <- 0
  }
}
pred$Habitat <- character(nrow(pred))
for (i in 1:nrow(pred)) { #Loop to categorise the habitat type
  if (pred$Site[i] == "Long Beach") {
    pred$Habitat[i] <- "Degraded"
  }
  if (pred$Site[i] != "Long Beach") {
    pred$Habitat[i] <- "Native"
  }
}
#Pooled the data without taking distance interval into account
pred_count <- pred %>% filter(Eaten.or.Not.in.Recording == "Yes") %>% group_by(Habitat, Distance.from.Coast, Eaten.By) %>% 
  count()
pred_count_pool <- pred %>% filter(Eaten.or.Not.in.Recording == "Yes") %>% group_by(Habitat, Eaten.By) %>% 
  count() #Pooled data
#Create two data frames for both habitats
pred_count_p <- pred_count %>% filter(Habitat == "Native")
pred_count_d <- pred_count %>% filter(Habitat == "Degraded")
#Analysis
#Black rat
fisher.test(matrix(c(8,0,22,15), nrow = 2, ncol = 2, byrow = TRUE)) #Significant
#American cockroach
fisher.test(matrix(c(1,1,29,14), nrow = 2, ncol = 2, byrow = TRUE))
#Scaly cricket
fisher.test(matrix(c(1,4,14,26), nrow = 2, ncol = 2, byrow = TRUE))
#Gecko
fisher.test(matrix(c(1,0,14,30), nrow = 2, ncol = 2, byrow = TRUE))
#Longhorn crazy ant
fisher.test(matrix(c(1,0,14,30), nrow = 2, ncol = 2, byrow = TRUE))
#Plot
pred_count_pool[nrow(pred_count_pool) + 1,] <- list("Degraded", "Black rat", 0)
pred_count_pool[nrow(pred_count_pool) + 1,] <- list("Native", "Gecko", 0)
pred_count_pool[nrow(pred_count_pool) + 1,] <- list("Native", "Longhorn crazy ant", 0)
pred_count_pool <- pred_count_pool[order(pred_count_pool$Eaten.By),]
pred_count_pool <- pred_count_pool[order(pred_count_pool$Habitat),]
ggplot(pred_count_pool, aes(x=Eaten.By, y=n, group=Habitat, color=Habitat,fill = Eaten.By)) + 
  geom_bar(stat = "identity", position = position_dodge(0.925), linewidth = 1.25) +
  labs(x = "Predator type",
       y = "Frequency",
       fill = "Predator type") +
  scale_color_manual(values = c("black","red")) +
  scale_fill_discrete(labels=c('American cockroach', 'Black rat', 'Scaly cricket',
                               'Gecko', 'Longhorn crazy ant'))+
  scale_x_discrete(labels=c('American cockroach', 'Black rat', 'Scaly cricket',
                            'Gecko', 'Longhorn crazy ant'))+
  theme_classic()