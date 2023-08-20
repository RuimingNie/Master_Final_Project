### Master Final Report ###

### Part 1: Soil texture analysis###
rm(list = ls())
getwd()
setwd("/Users/ruimingnie/Desktop")
data<- read.csv("Soil_Texture_Analysis.csv")

##Sand##
model1<- lm(data$X.Sand~data$Type)
m<- glm(data$X.Sand~data$Type)
summary(m)
summary(model1)
hist(data$X.Sand)
anova(model1)

#check normal ditribution
par(mfrow=c(2,2))
plot(model1)
m1_resid<- resid(model1)
hist(m1_resid)
#Good! it seems roughly normal distribution 

#compare different groups by using Tukey HSD
#check normality in each land type
hist(data$X.Sand[1:17])
hist(data$X.Sand[18:34])
hist(data$X.Sand[35:51])
#it looks good!

table1<- aov(data$X.Sand~data$Type)
summary(table1)
TukeyHSD(table1)

#data visualisation
# Load Libraries #
library(tidyverse)
library(rstatix)
library(ggpubr)
# Fit Data #
data<- na.omit(data)
data<-data[order(data$Type),]
fit_sand <- data %>% 
  anova_test(X.Sand ~ Type) %>% 
  add_significance()
#Run Tukey#
tukey_sand <- data %>% 
  tukey_hsd(X.Sand ~ Type) %>% 
  add_significance() %>% 
  add_xy_position()
#Plot #
b1<-ggboxplot(data,x="Type", y="X.Sand")+
  labs(x="Land type", y="Sand (%)")+
  font("xlab",size = 15)+
  font("ylab",size = 15)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  stat_pvalue_manual(tukey_sand, hide.ns = T)+
  labs(subtitle = get_test_label(fit_sand, detailed = TRUE),
       caption = get_pwc_label(tukey_sand))+
  geom_boxplot(fill="lightgrey", color="black")
print(b1)


##Clay##
hist(data$X.Clay)
model2<-lm(data$X.Clay~data$Type)
summary(model2)
anova(model2)
par(mfrow=c(2,2))
plot(model2)
#check normal ditribution
m2_resid<- resid(model2)
hist(m2_resid)
#good! it looks roughly normal distribution 

#compare different groups by using Tukey HSD
#check normality 
par(mfrow=c(1,3))
hist(data$X.Clay[1:17], main = "PAS")
hist(data$X.Clay[18:34], main = "LOG")
hist(data$X.Clay[35:51], main = "INT")
#it looks good!

table2<- aov(data$X.Clay~data$Type)
summary(table2)
TukeyHSD(table2)

#data visualisation
fit_clay <- data %>% 
  anova_test(X.Clay ~ Type) %>%  
  add_significance()
#Run Tukey #
tukey_clay <- data %>% 
  tukey_hsd(X.Clay ~ Type) %>% 
  add_significance() %>% 
  add_xy_position()
#adjust the label position
tukey_clay$y.position<-c(0,57,54)
# Plot#
b2<-ggboxplot(data, x="Type",
              y="X.Clay")+
  labs(x="Land type", y="Clay (%)")+
  font("xlab",size = 15)+
  font("ylab",size = 15)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  stat_pvalue_manual(tukey_clay, hide.ns = T, label.y=c(0,50,55))+
  labs(subtitle = get_test_label(fit_clay, detailed = TRUE),
       caption = get_pwc_label(tukey_clay))+
  geom_boxplot(fill="lightgrey", color="black")
print(b2)

##Silt##
hist(data$X.Silt)
model3<- lm(data$X.Silt~data$Type)
summary(model3)
anova(model3)

#check normal ditribution
par(mfrow=c(2,2))
plot(model3)
m3_resid<- resid(model3)
shapiro.test(m3_resid)
hist(m3_resid)
#good! it looks roughly normal distribution 

#compare different groups by using Tukey HSD
#check normality 
par(mfrow=c(1,3))
hist(data$X.Silt[1:17], main = "PAS")
hist(data$X.Silt[18:34], main = "LOG")
hist(data$X.Silt[35:51], main = "INT")
#Looks good!

table3<- aov(data$X.Silt~data$Type)
summary(table3)
TukeyHSD(table3)


#data visualisation
fit_silt <- data %>% 
  anova_test(X.Silt ~ Type) %>% 
  add_significance()
# Run Tukey#
tukey_silt <- data %>% 
  tukey_hsd(X.Silt ~ Type) %>% 
  add_significance() %>% 
  add_xy_position()
# Plot #
b3<-ggboxplot(data,
              x="Type",
              y="X.Silt")+
  labs(x="Land type", y="Silt (%)")+
  font("xlab",size = 15)+
  font("ylab",size = 15)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  stat_pvalue_manual(tukey_silt, hide.ns = T)+
  labs(subtitle = get_test_label(fit_silt, detailed = TRUE),
       caption = get_pwc_label(tukey_silt))+
  geom_boxplot(fill="lightgrey", color="black")
print(b3)

##Combine soil texture results together##
f<- ggarrange(b1, b2,b3,
              labels = c("B(1)","B(2)","B(3)"),
              ncol = 3, nrow =1)
print(f)



### Part 2: Leaf area index(LAI) analysis ###
#load data 
data1<- read.csv("LAI.csv")
#reorder the rows
data1<-data1[order(data1$Type),]

par(mfrow=c(1,1),mar=c(4,4,1,1))
boxplot(data1$LAI~data1$Type, xlab = "Type", ylab = "LAI")
model4<- lm(data1$LAI~data1$Type)
summary(model4)
anova(model4)

#check normal ditribution
par(mfrow=c(2,2))
plot(model4)
m4_resid<- resid(model4)
hist(m4_resid)
#good! it looks roughly normal distribution 

#compare different groups by using Tukey HSD
#check normality 
hist(data1$LAI[1:54], main="INT")
hist(data1$LAI[55:108], main="LOG")
hist(data1$LAI[109:162], main="PAS")
#the distribution in PAS is not normal
#I can not use Tukey HSD
#use non parametric alternatives -->  Kruskal Wallis test
kruskal.test(LAI~Type, data = data1)
pairwise.wilcox.test(data1$LAI, data1$Type, p.adjust.method = "bonferroni") 

#data visualisation 
#create comparisons
my_comparison <-list(c("I", "L"), c("L", "P"), c("I", "P"))
test <- compare_means(LAI~Type, comparisons = my_comparison,  
                      method='wilcox.test',p.adjust.method = "bonferroni", 
                      data = data1) 
#add the y.position
test <- test %>% mutate(y.position = c(2.9, 3.5, 3.2))
b4<-ggboxplot(data1, x="Type", y="LAI")+
  labs(x="Land type", y="LAI")+
  font("xlab",size = 15)+
  font("ylab",size = 15)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  geom_boxplot(fill="lightgrey", color="black")+
  stat_pvalue_manual(test, label = "p.signif", hide.ns = T)+
  stat_compare_means(label.y = 4, size=6)
print(b4)


### Part 3: functional trait ###
setwd("/Users/ruimingnie/Desktop/Master_Project/Funtional trait")
library(dplyr)
library(readxl)
library(EnvStats)

#loading data
trait<- read_xlsx("All_species_traits.xlsx")
juv<- read.csv("juv_hec.csv")

# combine two files
colnames(trait)[1]<-"species"
ft_juv<- left_join(juv, trait , by="species")

#replace unknown data 
library(gdata)
ft_juv[ft_juv=="Unknown"]<-NA
ft_juv1<- ft_juv%>% group_by(plot_code,subplot_code, species)%>%count()
#remove PLA data
ft_juv1<-ft_juv1[1:744,]
# combine two data sets
ft_juv_f<- left_join(ft_juv1, ft_juv , by= c("plot_code"="plot_code","subplot_code"="subplot_code", "species"="species"))

# remove duplicate rows
library(tidyverse)
ft_juv_f<- ft_juv_f %>% distinct(site, species, .keep_all = TRUE)

##convert the species data frame
library(tidyr)
ft_juv2<- ft_juv_f%>%ungroup()%>% select("site", "species","n")
#remove unrecognised (NA) species
x<-is.na(ft_juv2$species)
sum(x=="TRUE")
ft_juv2<-ft_juv2%>%filter(species!= "NA")
gather_species_juv<- pivot_wider(ft_juv2, names_from = "site", values_from = "n")
#reorder the rows by species name
gather_species_juv<-gather_species_juv[order(gather_species_juv$species),]
gather_species_juv<- as.matrix(gather_species_juv)
x<-as.character(gather_species_juv[,1])
#change the rows name to species name
rownames(gather_species_juv)<- as.character(gather_species_juv[,1])
gather_species_juv<- as.data.frame(gather_species_juv)
gather_species_juv<-gather_species_juv%>%select(-1)
#check the type of data
class(gather_species_juv$INT_10B)
#change to data type to numeric
gather_species_juv<-lapply(gather_species_juv, as.numeric)
gather_species_juv<- as.data.frame(gather_species_juv)
rownames(gather_species_juv)<- x
class(gather_species_juv)

# check species richness
sum(ft_juv2$n[1:337])
sum(ft_juv2$n[338:636])
sum(ft_juv2$n[637:736])

#functional trait data frame
gather_trait_juv<- 
  ft_juv_f%>%
  ungroup()%>%
  select(species,foliar_habit,`SLA (mm2/mg)`,`Maximum height (m)`,
         `LDMC (mg/g)`,`Wood density (mg/g)`,compoundness,`C:N`,
         `Leaf density (g/cm3)`, `Leaf thickness (mm)`, Delta13C)
gather_trait_juv<-gather_trait_juv[order(gather_trait_juv$species),]
#remove replicated rows
gather_trait_juv<- gather_trait_juv %>% distinct(species, .keep_all = TRUE)
#remove unrecognised species
gather_trait_juv<-gather_trait_juv[1:68,]
gather_trait_juv<- as.matrix(gather_trait_juv)
t<- as.character(gather_trait_juv[,1])
rownames(gather_trait_juv)<- t
#check the rows name in two dataframe
x==t
gather_trait_juv<- as.data.frame(gather_trait_juv)
gather_trait_juv<-gather_trait_juv%>%select(-1)

#check data type 
#change to factor
gather_trait_juv$foliar_habit<- as.factor(gather_trait_juv$foliar_habit)
class(gather_trait_juv$foliar_habit)
gather_trait_juv$compoundness<- as.factor(gather_trait_juv$compoundness)
class(gather_trait_juv$compoundness)
#change to numeric 
gather_trait_juv$`C:N`<- as.numeric(gather_trait_juv$`C:N`)
class(gather_trait_juv$`C:N`)
gather_trait_juv$Delta13C<- as.numeric(gather_trait_juv$Delta13C)
class(gather_trait_juv$Delta13C)
gather_trait_juv$`SLA (mm2/mg)`<- as.numeric(gather_trait_juv$`SLA (mm2/mg)`)
class(gather_trait_juv$`SLA (mm2/mg)`)
gather_trait_juv$`LDMC (mg/g)`<- as.numeric(gather_trait_juv$`LDMC (mg/g)`)
class(gather_trait_juv$`LDMC (mg/g)`)
gather_trait_juv$`Wood density (mg/g)`<- as.numeric(gather_trait_juv$`Wood density (mg/g)`)
class(gather_trait_juv$`Wood density (mg/g)`)
gather_trait_juv$`Maximum height (m)`<- as.numeric(gather_trait_juv$`Maximum height (m)`)
class(gather_trait_juv$`Maximum height (m)`)
gather_trait_juv$`Leaf thickness (mm)`<- as.numeric(gather_trait_juv$`Leaf thickness (mm)`)
class(gather_trait_juv$`Leaf thickness (mm)`)
gather_trait_juv$`Leaf density (g/cm3)`<- as.numeric(gather_trait_juv$`Leaf density (g/cm3)`)
class(gather_trait_juv$`Leaf density (g/cm3)`)

#Check normality
#Try transforming data to make more normal distribution
#Transform trait data for a CWM is not to meet any kind of model assumption.
#Because extreme trait values will have a disproportionate effect on the CWM value.
par(mfrow = c(1, 1))
par(mar = c(3, 3, 1, 1))
hist(gather_trait_juv$Delta13C)
#No, it does not look better

hist(gather_trait_juv$`C:N`)
hist(log(gather_trait_juv$`C:N`))
gather_trait_juv$`C:N`<-log(gather_trait_juv$`C:N`)
#Yes, it looks better

hist(gather_trait_juv$`SLA (mm2/mg)`)
hist(log(gather_trait_juv$`SLA (mm2/mg)`))
gather_trait_juv$`SLA (mm2/mg)`<-log(gather_trait_juv$`SLA (mm2/mg)`)
#yes, it looks better

hist(gather_trait_juv$`LDMC (mg/g)`)
hist(log(gather_trait_juv$`LDMC (mg/g)`))
gather_trait_juv$`LDMC (mg/g)`<-log(gather_trait_juv$`LDMC (mg/g)`)
#yes, it looks better

hist(gather_trait_juv$`Wood density (mg/g)`)
hist(log(gather_trait_juv$`Wood density (mg/g)`))
#No, it does not look better

hist(gather_trait_juv$`Maximum height (m)`)
hist(log(gather_trait_juv$`Maximum height (m)`))
gather_trait_juv$`Maximum height (m)`<-log(gather_trait_juv$`Maximum height (m)`)
#No, it does not look better

hist(gather_trait_juv$`Leaf thickness (mm)`)
hist(log(gather_trait_juv$`Leaf thickness (mm)`))
#No, it does not look better

hist(gather_trait_juv$`Leaf density (g/cm3)`)
hist(log(gather_trait_juv$`Leaf density (g/cm3)`))
#No, it does not look better

#CWM calculation
library(FD)
#Check the rows name 
rownames(gather_trait_juv) == rownames(gather_species_juv)
final_juv<- functcomp(gather_trait_juv,t(gather_species_juv), CWM.type = "all")

#combine the same column
final_juv<- final_juv%>% mutate(foliar_habit_Brevi.deciduous, foliar_habit_Brevi.deciduous+foliar_habit_Drought.deciduous)
#Add new column (plot code)
final_juv<- final_juv%>% mutate(plot_code = "INT")
#check the sample size of each land type
a<- select(gather_species_juv, contains("INT"))
b<-select(gather_species_juv, contains("LOG"))
c<- select(gather_species_juv, contains("PAS"))
final_juv$plot_code[55:107]<- "LOG"
final_juv$plot_code[108:154]<- "PAS"

#fit linear model: significant difference_brevi deciduous
boxplot(final_juv$`foliar_habit_Brevi.deciduous + foliar_habit_Drought.deciduous` ~final_juv$plot_code, ylab = "Brevi.deciduous", xlab = "plot code" )
m1<- lm(final_juv$`foliar_habit_Brevi.deciduous + foliar_habit_Drought.deciduous` ~final_juv$plot_code)
summary(m1)
#check normal ditribution
par(mfrow=c(2,2))
plot(m1)
model1_resid<- resid(m1)
shapiro.test(model1_resid)
hist(model1_resid)
#Good! it seems roughly normal distribution 
#compare different groups 
#check normality 
par(mfrow=c(1,3))
hist(final_juv$`foliar_habit_Brevi.deciduous + foliar_habit_Drought.deciduous`[1:54], main = "INT")
hist(final_juv$`foliar_habit_Brevi.deciduous + foliar_habit_Drought.deciduous`[55:107], main = "LOG")
hist(final_juv$`foliar_habit_Brevi.deciduous + foliar_habit_Drought.deciduous`[108:154], main = "PAS")
#the distribution in PAS and LOG is not normal
#I can not use Tukey HSD
#use non parametric alternatives -->  Kruskal Wallis test
kruskal.test(`foliar_habit_Brevi.deciduous + foliar_habit_Drought.deciduous`~plot_code, data = final_juv)
pairwise.wilcox.test(final_juv$`foliar_habit_Brevi.deciduous + foliar_habit_Drought.deciduous`, final_juv$plot_code, p.adjust.method = "bonferroni") 


#fit linear model: significant difference_deciduous
boxplot(final_juv$foliar_habit_Deciduous ~ final_juv$plot_code, ylab = "deciduous", xlab = "plot code")
m2<- lm(final_juv$foliar_habit_Deciduous ~ final_juv$plot_code)
summary(m2)
#check normal ditribution
par(mfrow=c(2,2))
plot(m2)
model2_resid<- resid(m2)
shapiro.test(model2_resid)
hist(model2_resid)
#Good! it seems roughly normal distribution 
#compare different groups 
#check normality 
par(mfrow=c(1,3))
hist(final_juv$foliar_habit_Deciduous[1:54], main = "INT")
hist(final_juv$foliar_habit_Deciduous[55:107], main = "LOG")
hist(final_juv$foliar_habit_Deciduous[108:154], main = "PAS")
#the distribution in PAS and INT is not normal
#I can not use Tukey HSD
#use non parametric alternatives -->  Kruskal Wallis test
kruskal.test(foliar_habit_Deciduous~plot_code, data = final_juv)
pairwise.wilcox.test(final_juv$foliar_habit_Deciduous, final_juv$plot_code, p.adjust.method = "bonferroni") 


#fit linear model:significant difference __foliar_habit_Evergreen
boxplot(final_juv$foliar_habit_Evergreen ~ final_juv$plot_code, ylab = "Evergreen", xlab = "plot code")
m3<- lm(final_juv$foliar_habit_Evergreen ~ final_juv$plot_code)
summary(m3)
#check normal ditribution
par(mfrow=c(2,2))
plot(m3)
model3_resid<- resid(m3)
shapiro.test(model3_resid)
hist(model3_resid)
#Good! it seems roughly normal distribution 
#compare different groups 
#check normality 
par(mfrow=c(1,3))
hist(final_juv$foliar_habit_Evergreen[1:54], main = "INT")
hist(final_juv$foliar_habit_Evergreen[55:107], main = "LOG")
hist(final_juv$foliar_habit_Evergreen[108:154], main = "PAS")
#the distribution in PAS is not normal
#I can not use Tukey HSD
#use non parametric alternatives -->  Kruskal Wallis test
kruskal.test(foliar_habit_Evergreen~plot_code, data = final_juv)
pairwise.wilcox.test(final_juv$foliar_habit_Evergreen, final_juv$plot_code, p.adjust.method = "bonferroni") 

#Plot#
#Combine the foliar habit factors
group<- final_juv[,c(15,2,4,16)]
head(group)
colnames(group)<-c("Brevi deciduous species", "Deciduous species", "Evergreen species",
                   "Plot_code")
group1<-pivot_longer(group, names_to = "Foliar habit", values_to = "CWM", cols = ends_with("species"))


ggplot(group1,  aes(x=Plot_code, y=CWM, fill= `Foliar habit`)) + 
  labs(x="Land type", y="Foliar habit CWM")+
  theme(axis.title.y = element_text(margin = margin(t = 10, r = 20, b = 10, l = 10)))+
  geom_boxplot(width=0.5)+theme_classic()

#fit linear model:significant difference __ Delta13C
boxplot(final_juv$Delta13C ~ final_juv$plot_code, ylab = "Delta13C", xlab = "plot code")
m4<- lm(final_juv$Delta13C ~ final_juv$plot_code)
summary(m4)
#check normal ditribution
par(mfrow=c(2,2))
plot(m4)
model4_resid<- resid(m4)
shapiro.test(model4_resid)
hist(model4_resid)
#Good! it seems roughly normal distribution 
#compare different groups 
#check normality 
par(mfrow=c(1,3))
hist(final_juv$Delta13C[1:54], main = "INT")
hist(final_juv$Delta13C[55:107], main = "LOG")
hist(final_juv$Delta13C[108:154], main = "PAS")
#all distributions are not normal
#I can not use Tukey HSD
#use non parametric alternatives -->  Kruskal Wallis test
kruskal.test(Delta13C~plot_code, data = final_juv)
##no significant difference among land types!
#Median better to represent the central location rather than mean

#Plot#
#create comparisons 
my_comparisons <-list(c("INT", "LOG"), c("LOG", "PAS"), c("INT", "PAS"))
test4 <- compare_means(Delta13C~plot_code, comparisons = my_comparisons,  
                       method='wilcox.test',p.adjust.method = "bonferroni", 
                       data = final_juv) 

test4 <- test4 %>% mutate(y.position = c(0, 0,0))
plot4<-ggboxplot(final_juv, x="plot_code", y="Delta13C")+
  stat_n_text(y.pos = -33)+
  labs(x="Land type", y="Delta13C")+
  stat_pvalue_manual(test4, label = "p.signif", hide.ns = T)+
  stat_compare_means(label.y = -25, size=5)+
  geom_boxplot(fill="lightgrey", color="black")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x  = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)))+
  font("ylab",size = 17)
print(plot4)

#fit linear model: significant difference __ SLA..mm2.mg.
boxplot(final_juv$SLA..mm2.mg. ~ final_juv$plot_code, ylab = "SLA..mm2.mg.", xlab = "plot code")
m5<- lm(final_juv$SLA..mm2.mg. ~ final_juv$plot_code)
summary(m5)
#check normal ditribution
par(mfrow=c(2,2))
plot(m5)
model5_resid<- resid(m5)
shapiro.test(model5_resid)
hist(model5_resid)
#Good! it seems roughly normal distribution 
#compare different groups 
#check normality 
par(mfrow=c(1,3))
hist(final_juv$SLA..mm2.mg.[1:54], main = "INT")
hist(final_juv$SLA..mm2.mg.[55:107], main = "LOG")
hist(final_juv$SLA..mm2.mg.[108:154], main = "PAS")
#the distribution of PAS is not normal
#I can not use Tukey HSD
#use non parametric alternatives -->  Kruskal Wallis test
kruskal.test(SLA..mm2.mg.~plot_code, data = final_juv)
pairwise.wilcox.test(final_juv$SLA..mm2.mg., final_juv$plot_code, p.adjust.method = "bonferroni") 
#Median better to represent the central location rather than mean
#Plot#
test5 <- compare_means(SLA..mm2.mg.~plot_code, comparisons = my_comparisons,  
                       method='wilcox.test',p.adjust.method = "bonferroni", 
                       data = final_juv) 

test5 <- test5 %>% mutate(y.position = c(3.5, 3.6,3.36))
plot5<-ggboxplot(final_juv, x="plot_code", y="SLA..mm2.mg.")+
  stat_n_text(y.pos = 1.8)+
  labs(x="Land type", y="CWM log(SLA)")+
  ylim(1.8,4.3)+
  stat_pvalue_manual(test5, label = "p.signif", hide.ns = T)+
  stat_compare_means(label.y = 4.3, size=5)+
  geom_boxplot(fill="lightgrey", color="black")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x  = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)))+
  font("ylab",size = 17)
print(plot5)


#fit linear model:significant difference __ LDMC..mg.g.
boxplot(final_juv$LDMC..mg.g. ~ final_juv$plot_code, ylab = "LDMC..mg.g.", xlab = "plot code")
m6<- lm(final_juv$LDMC..mg.g. ~ final_juv$plot_code)
summary(m6)
#check normal ditribution
par(mfrow=c(2,2))
plot(m6)
model6_resid<- resid(m6)
shapiro.test(model6_resid)
hist(model6_resid)
#No! the distribution is not normal 
#compare different groups 
#check normality 
par(mfrow=c(1,3))
hist(final_juv$LDMC..mg.g.[1:54], main = "INT")
hist(final_juv$LDMC..mg.g.[55:107], main = "LOG")
hist(final_juv$LDMC..mg.g.[108:154], main = "PAS")
#all distribution are not normal
#I can not use Tukey HSD
#use non parametric alternatives -->  Kruskal Wallis test
kruskal.test(LDMC..mg.g.~plot_code, data = final_juv)
pairwise.wilcox.test(final_juv$LDMC..mg.g., final_juv$plot_code, p.adjust.method = "bonferroni") 
#Median better to represent the central location rather than mean
#Plot#
test6 <- compare_means(LDMC..mg.g.~plot_code, comparisons = my_comparisons,  
                       method='wilcox.test',p.adjust.method = "bonferroni", 
                       data = final_juv) 

test6 <- test6 %>% mutate(y.position = c(6.2, 0,0))
plot6<-ggboxplot(final_juv, x="plot_code", y="LDMC..mg.g.")+
  stat_n_text(y.pos = 5)+
  labs(x="Land type", y="CWM log(LDMC)")+  ylim(5,6.6)+
  stat_pvalue_manual(test6, label = "p.signif", hide.ns = T)+
  stat_compare_means(label.y = 6.6, size=5)+
  geom_boxplot(fill="lightgrey", color="black")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x  = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)))+
  font("ylab",size = 17)
print(plot6)


#fit linear model: significant difference __ Wood.density..mg.g.
boxplot(final_juv$Wood.density..mg.g. ~ final_juv$plot_code, ylab = "Wood.density..mg.g.", xlab = "plot code")
m7<- lm(final_juv$Wood.density..mg.g. ~ final_juv$plot_code)
summary(m7)
#check normal ditribution
par(mfrow=c(2,2))
plot(m7)
model7_resid<- resid(m7)
shapiro.test(model7_resid)
hist(model7_resid)
#No! it seems not normal distribution 
#I can not use linear model 
#compare different groups 
#check normality 
par(mfrow=c(1,3))
hist(final_juv$Wood.density..mg.g.[1:54], main = "INT")
hist(final_juv$Wood.density..mg.g.[55:107], main = "LOG")
hist(final_juv$Wood.density..mg.g.[108:154], main = "PAS")
#all distribution are not normal
#I can not use Tukey HSD
#use non parametric alternatives -->  Kruskal Wallis test
kruskal.test(Wood.density..mg.g.~plot_code, data = final_juv)
pairwise.wilcox.test(final_juv$Wood.density..mg.g., final_juv$plot_code, p.adjust.method = "bonferroni") 
#Median better to represent the central location rather than mean
#Plot#
test7 <- compare_means(Wood.density..mg.g.~plot_code, comparisons = my_comparisons,  
                       method='wilcox.test',p.adjust.method = "bonferroni", 
                       data = final_juv) 

test7 <- test7 %>% mutate(y.position = c(1, 0,0))
plot7<-ggboxplot(final_juv, x="plot_code", y="Wood.density..mg.g.")+
  labs(x="Land type", y="CWM WD")+
  stat_n_text(y.pos = 0)+
  ylim(0,1.4)+
  stat_pvalue_manual(test7, label = "p.signif", hide.ns = T)+
  stat_compare_means(label.y = 1.4, size=5)+
  geom_boxplot(fill="lightgrey", color="black")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x  = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)))+
  font("ylab",size = 17)
print(plot7)

#fit linear model: significant difference __ Maximum.height..m.
boxplot(final_juv$Maximum.height..m. ~ final_juv$plot_code, ylab = "Maximum.height..m.", xlab = "plot code")
m8<- lm(final_juv$Maximum.height..m. ~ final_juv$plot_code)
summary(m8)
#check normal ditribution
par(mfrow=c(2,2))
plot(m8)
model8_resid<- resid(m8)
shapiro.test(model8_resid)
hist(model8_resid)
#Good! p>0.05
#compare different groups 
#check normality 
par(mfrow=c(1,3))
hist(final_juv$Maximum.height..m.[1:54], main = "INT")
hist(final_juv$Maximum.height..m.[55:107], main = "LOG")
hist(final_juv$Maximum.height..m.[108:154], main = "PAS")
#the distribution of PAS is not normal
#I can not use Tukey HSD
#use non parametric alternatives -->  Kruskal Wallis test
kruskal.test(Maximum.height..m.~plot_code, data = final_juv)
pairwise.wilcox.test(final_juv$Maximum.height..m., final_juv$plot_code, p.adjust.method = "bonferroni") 
#Plot#
test8 <- compare_means(Maximum.height..m.~plot_code, comparisons = my_comparisons,  
                        method='wilcox.test',p.adjust.method = "bonferroni", 
                        data = final_juv) 

test8 <- test8 %>% mutate(y.position = c(3.6, 4,3.8))
plot8<-ggboxplot(final_juv, x="plot_code", y="Maximum.height..m.")+
  stat_n_text()+
  labs(x="Land type", y="CWM log(Max height)")+
  stat_pvalue_manual(test8, label = "p.signif", hide.ns = T)+
  stat_compare_means(label.y = 4.5, size=5)+
  geom_boxplot(fill="lightgrey", color="black")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x  = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)))+
  font("ylab",size = 17)
print(plot8)


#fit linear model: significant difference __ Leaf.thickness..mm.
boxplot(final_juv$Leaf.thickness..mm. ~ final_juv$plot_code, ylab = "Leaf.thickness..mm.", xlab = "plot code")
m9<- lm(final_juv$Leaf.thickness..mm. ~ final_juv$plot_code)
summary(m9)
#check normal ditribution
par(mfrow=c(2,2))
plot(m9)
model9_resid<- resid(m9)
shapiro.test(model9_resid)
hist(model9_resid)
#No! it seems not normal distribution 
#I can not use linear model 
#compare different groups 
#check normality 
par(mfrow=c(1,3))
hist(final_juv$Leaf.thickness..mm.[1:54], main = "INT")
hist(final_juv$Leaf.thickness..mm.[55:107], main = "LOG")
hist(final_juv$Leaf.thickness..mm.[108:154], main = "PAS")
#all distributions are not normal
#I can not use Tukey HSD
#use non parametric alternatives -->  Kruskal Wallis test
kruskal.test(Leaf.thickness..mm.~plot_code, data = final_juv)
pairwise.wilcox.test(final_juv$Leaf.thickness..mm., final_juv$plot_code, p.adjust.method = "bonferroni") 
#Plot#
test9 <- compare_means(Leaf.thickness..mm.~plot_code, comparisons = my_comparisons,  
                        method='wilcox.test',p.adjust.method = "bonferroni", 
                        data = final_juv) 

test9 <- test9 %>% mutate(y.position = c(0.35, 0,0))
plot9<-ggboxplot(final_juv, x="plot_code", y="Leaf.thickness..mm.")+
  ylim(0.18,0.4)+
  stat_n_text(y.pos = 0.18)+
  labs(x="Land type", y="CWM LT")+
  stat_pvalue_manual(test9, label = "p.signif", hide.ns = T)+
  stat_compare_means(label.y = 0.4, size=5)+
  geom_boxplot(fill="lightgrey", color="black")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x  = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)))+
  font("ylab",size = 17)
print(plot9)

#check which one is lager 
median(final_juv$Leaf.thickness..mm.[1:54], na.rm = T)-median(final_juv$Leaf.thickness..mm.[55:107],na.rm = T)
#larger in INT

#fit linear model: significant difference __ Leaf.density..g.cm3.
boxplot(final_juv$Leaf.density..g.cm3. ~ final_juv$plot_code, ylab = "Leaf.density..g.cm3.", xlab = "plot code")
m10<- lm(final_juv$Leaf.density..g.cm3. ~ final_juv$plot_code)
summary(m10)
#check normal ditribution
par(mfrow=c(2,2))
plot(m10)
model10_resid<- resid(m10)
shapiro.test(model10_resid)
hist(model10_resid)
#No! it seems not normal distribution 
#I can not use linear model 
#compare different groups 
#check normality 
par(mfrow=c(1,3))
hist(final_juv$Leaf.density..g.cm3.[1:54], main = "INT")
hist(final_juv$Leaf.density..g.cm3.[55:107], main = "LOG")
hist(final_juv$Leaf.density..g.cm3.[108:154], main = "PAS")
#all distributions are not normal
#I can not use Tukey HSD
#use non parametric alternatives -->  Kruskal Wallis test
kruskal.test(Leaf.density..g.cm3.~plot_code, data = final_juv)
#no significant difference among land types
#Plot#
test10 <- compare_means(Leaf.density..g.cm3.~plot_code, comparisons = my_comparisons,  
                        method='wilcox.test',p.adjust.method = "bonferroni", 
                        data = final_juv) 

test10 <- test10 %>% mutate(y.position = c(0.35, 0.1,0.1))
plot10<-ggboxplot(final_juv, x="plot_code", y="Leaf.density..g.cm3.")+
  stat_n_text()+
  labs(x="Land type", y="CWM Leaf density")+
  stat_pvalue_manual(test10, label = "p.signif", hide.ns = T)+
  stat_compare_means(label.y = 0.45, size=5)+
  geom_boxplot(fill="lightgrey", color="black")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x  = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)))+
  font("ylab",size = 17)
print(plot10)


#fit linear model: significant difference __ compoundness_Compound
boxplot(final_juv$compoundness_Compound ~ final_juv$plot_code, ylab = "Compound leaf (%)", xlab = "plot code")
m11<- lm(final_juv$compoundness_Compound ~ final_juv$plot_code)
summary(m11)
#check normal ditribution
par(mfrow=c(2,2))
plot(m11)
model11_resid<- resid(m11)
shapiro.test(model11_resid)
hist(model11_resid)
#Good! it seems roughly normal distribution 
#compare different groups 
#check normality 
par(mfrow=c(1,3))
hist(final_juv$compoundness_Compound[1:54], main = "INT")
hist(final_juv$compoundness_Compound[55:107], main = "LOG")
hist(final_juv$compoundness_Compound[108:154], main = "PAS")
#all distributions are not normal
#I can not use Tukey HSD
#use non parametric alternatives -->  Kruskal Wallis test
kruskal.test(compoundness_Compound~plot_code, data = final_juv)
pairwise.wilcox.test(final_juv$compoundness_Compound, final_juv$plot_code, p.adjust.method = "bonferroni") 
#Plot#
test11 <- compare_means(compoundness_Compound~plot_code, comparisons = my_comparisons,  
                        method='wilcox.test',p.adjust.method = "bonferroni", 
                        data = final_juv) 

test11 <- test11 %>% mutate(y.position = c(1.1, 1.2,0))
plot11<-ggboxplot(final_juv, x="plot_code", y="compoundness_Compound")+
  stat_n_text(y.pos = -0.15)+
  labs(x="Land type", y="CWM Compound leaf")+
  stat_pvalue_manual(test11, label = "p.signif", hide.ns = T)+
  stat_compare_means(label.y = 1.39, size=5)+
  ylim(-0.150,1.4)+
  geom_boxplot(fill="lightgrey", color="black")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x  = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)))+
  font("ylab",size = 17)
print(plot11)

#combine nine graphs 
library(ggplot2)
library(ggpubr)


figure<- ggarrange(plot8,plot5,plot11,plot6,plot7,plot9,
                   labels = c("A","B","C","D","E","F","G"),
                   ncol = 3, nrow = 2)
print(figure)



## Part 4 Correlation between functional traits and resource availability##
#Combine two data set according to the bulked sample cheat sheet
rownames(final_juv)
final_juv<- final_juv%>% mutate(Sample.number="I1")
number1<-c("I16","I17","I17","I17", "I17", "I1","I2","I3","I4","I5","I1","I2", "I3",
           "I4","I5","I6","I2","I3", "I4","I10","I6","I7","I8","I9",
           "I10","I6","I7","I8","I9","I10","I11","I7","I8","I9","I15","I11",
           "I12","I13","I14","I15","I11","I12","I13","I14","I15","I16","I12",
           "I13","I14","I17","I5","I1","I17","I16","L16","L17","L17","L17","L1",
           "L2","L3","L4","L5","L1","L2","L3","L4","L5","L6","L2","L3","L4","L10",
           "L6","L7","L8","L9","L10","L6","L7","L8","L9","L10","L11","L7","L8","L9",
           "L15","L11","L12","L13","L14","L15","L11","L12","L13","L14","L15","L16",
           "L12","L13","L14","L17","L5","L1","L17","L16","P16", "P17","P17","P1",
           "P2","P3","P4","P5","P1","P2","P3","P4","P5","P6","P2","P4","P10", 
           "P6","P7","P8","P9","P10", "P6","P7","P8","P9","P10", "P11","P7","P8","P15",
           "P11", "P12", "P13", "P14", "P11","P12", "P13", "P14","P15","P16",
           "P12", "P13", "P14","P17", "P5", "P1")
final_juv$Sample.number<-number1

#LAI data, according to the name of te photo and bulked sample cheat sheet
s1<-c("I1","I2","I3","I4","I5","I6","I7","I8","I9", "I10","I11","I12","I13","I14","I15","I16","I17","I1","I1","I2",
      "I3","I4","I5","I6","I7","I8","I9", "I10","I11","I12","I13","I14","I15","I16","I17","I1","I1","I2","I3","I4","I5",
      "I6","I7","I8","I9", "I10","I11","I12","I13","I14","I15","I16","I17","I1")
s2<-c("L1","L2","L3","L4","L5","L6","L7","L8","L9","L10","L11","L12","L13",
      "L14","L15","L16","L17","L1","L1","L2","L3","L4","L5","L6","L7","L8","L9",
      "L10","L11","L12","L13","L14","L15","L16","L17","L1","L1","L2","L3","L4",
      "L5","L6","L7","L8","L9","L10","L11","L12","L13","L14","L15","L16","L17","L1")
s3<-c("P1","P2","P3","P4","P5", "P6","P7","P8","P9","P10","P11","P12", 
      "P13", "P14","P15","P16","P17","P1","P1","P2","P3","P4","P5", 
      "P6","P7","P8","P9","P10","P11","P12", "P13", "P14","P15","P16","P17",
      "P1","P1","P2","P3","P4","P5", "P6","P7","P8","P9","P10","P11","P12", "P13",
      "P14","P15","P16","P17","P1")
LAI<- data1%>% mutate(Sample.number="I1")
LAI$Sample.number[1:54]<-s1
LAI$Sample.number[55:108]<-s2
LAI$Sample.number[109:162]<-s3


LAI<- data1%>% mutate(Sample.number="I1")
LAI$Sample.number[1:54]<-s1
LAI$Sample.number[55:108]<-s2
LAI$Sample.number[109:162]<-s3

#extract the soil variables
soil<- data%>% select(1, 16, 18, 19)
#Combine the soil data with species data set 
juv_soil<-left_join(final_juv,soil, by="Sample.number")
#Combine the soil data with species dataset and soil results
juv_f<-left_join(juv_soil,LAI, by="Sample.number")

#check normality: Spearman or pearson
shapiro.test(juv_f$foliar_habit_Brevi.deciduous)
library("ggpubr")
ggqqplot(juv_f$foliar_habit_Brevi.deciduous)
#non-normal distribution

class(juv_f)
juv_f1<-juv_f%>% select(15,2,4:9,12:14,18,19,22)
juv_f1<-as.matrix(juv_f1)
head(juv_f1)
#create colnames
colnames(juv_f1)<-c("Brevi_deciduous", "Deciduous","Evergreen", "SLA","Maximum height", "LDMC",
                    "WD", "Compound leaf",  "LD",
                    "LT", "Foliar delta13C", "Sand%", "Clay%","LAI")
library("Hmisc")
cor_juv<- rcorr( juv_f1[,12:14],juv_f1[,1:11], type =c("spearman"))
cor_juv
cor_juvr<-cor_juv$r
cor_juvp<-cor_juv$P
flat_cor_mat <- function(cor_r, cor_p){
  #This function provides a simple formatting of a correlation matrix
  #into a table with 4 columns containing :
  # Column 1 : row names (variable 1 for the correlation test)
  # Column 2 : column names (variable 2 for the correlation test)
  # Column 3 : the correlation coefficients
  # Column 4 : the p-values of the correlations
  library(tidyr)
  library(tibble)
  cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
  cor_r <- gather(cor_r, column, cor, -1)
  cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
  cor_p <- gather(cor_p, column, p, -1)
  cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
  cor_p_matrix
}
my_cor_matrix_juv <- flat_cor_mat(cor_juv$r, cor_juv$P)
my_cor_matrix_juv<- my_cor_matrix_juv[1:51,]
my_cor_matrix_juv1<-my_cor_matrix_juv%>% mutate(p_signif=p<0.05)


library(corrplot)
par(mfrow=c(1,1),mar=c(1,1,3,3))
#remove the error, by replacing NA with 0
cor_juvp[is.na(cor_juvp)] <- 0
#final correlation diagram
corrplot(cor_juvr, p.mat =  cor_juvp,method = "number", type = "upper", tl.col = "black" )










