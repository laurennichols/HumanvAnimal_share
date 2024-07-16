
#load libraries
library(tidyverse)
library(ggrepel)

#set local working directory
setwd("/Users/lmn7/Documents/Datasets/human_vs_animal")

#read in data
dat = read.csv("Human_vs_animal_fight_gender.csv" , header=T)

#reassign at factors and calculate percentage
dat$Animal = as.factor(dat$Animal)
dat$Gender = as.factor(dat$Gender)
dat$Percent = dat$Percent*100

#reformat data from long to wide data
#Wide_data = spread(dat, Gender, Percent)

#Encode values using color
ggplot(dat, aes(Gender, Animal, fill=Percent))+
  geom_tile()

#Encode values using size of dot
ggplot(dat, aes(Gender, Animal))+
  geom_point(aes(size=Percent))

#Faceted bar plots showing female and male. 
#Easy to compare within a gender, harder to compare across genders
ggplot(dat, aes(reorder(Animal, Percent), Percent, fill=Gender))+
  geom_bar(stat='identity', position='dodge')+
  facet_wrap(~Gender)
  
#Paired bar plots.
#easier to compare between genders for individual animals, but very busy
ggplot(dat, aes(reorder(Animal, Percent), Percent, fill=Gender))+
  geom_bar(stat='identity', position='dodge')+
  coord_flip()

#Paired dot plot
#Good for showing absolute differences between animals
#as well as differences between males and females
ggplot(dat, aes(Gender, Percent, col=Animal, group=Animal))+
  geom_point(size=3, show.legend = F)+
  geom_line(alpha=.3, show.legend = F)+
  theme_classic()+
  geom_text_repel(data=dat %>% filter(Gender=='Male'), 
             aes(x=Gender, y=Percent, label=Animal),
             hjust = "left",
             nudge_x = 0.05,
             size = 3,
             col='darkgrey')
  


#plot female vs male for each animal
#Good for valence (above or below) for each animal, but maskes some differences, 
#especially on the lower end of the graph
ggplot(Wide_data, aes(Female, Male))+
  geom_point(aes(col=Animal))+
  theme_classic()+
  geom_text_repel(aes(label=Animal),
                  hjust = "left",
                  nudge_x = 0.05,
                  size = 3,
                  col='darkgrey')+
  geom_abline(intercept = 0, slope = 1, size = 0.5, linetype= "dashed") 
  

#Dumbell plot - arranges based on FEMALE descending order
#Good for showing absolute differences between animals as well as 
#differences between the genders for each animal 
dat %>% 
  arrange(Gender,Percent) %>% 
  mutate(Animal = factor(Animal, unique(Animal))) %>%
  ggplot(aes(Animal, Percent, col=Gender))+
  geom_line(aes(group=Animal), col='grey', size=3, alpha=.3)+
  geom_point(size=4)+
  coord_flip()+
  theme_minimal()+
  ylim(0,100)

#Dumbell plot - arranges based on MALE descending order
#Same as the above figure but sorts the order based on male averages. 
#Which you sort on will be identified as the viewer as the "reference"
#note the slight differences in the stories that emerge/dissappear

dat %>% 
  arrange(desc(Gender),Percent) %>% 
  mutate(Animal = factor(Animal, unique(Animal))) %>%
  ggplot(aes(Animal, Percent, col=Gender))+
  geom_line(aes(group=Animal), col='grey', size=3, alpha=.3)+
  geom_point(size=4)+
  coord_flip()+
  theme_minimal()+
  ylim(0,100)


#######
#Now lets add an additional variable, lets compare Americans
#and Brits"


#load UK vs US - aggregated data
dat = read.csv("Humans_vs_animals_UKvsUS.csv", header=T)

dat$Animal = as.factor(dat$Animal)
dat$Nat = as.factor(dat$Nat)
dat$Gender = as.factor(dat$Gender)
dat$Percent = dat$Percent*100


#faceted bar plots
ggplot(dat, aes(reorder(Animal, Percent), Percent, fill=Nat))+
  geom_bar(stat='identity', position='dodge')+
  coord_flip()+
  facet_wrap(~Gender)

#faceted "reaction norm" graph faceted on Gender
#compare uk vs us within gender
ggplot(dat, aes(Gender, Percent, group=Animal,
                col = Animal))+
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_wrap(~Nat)

#faceted "reaction norm" graph faceted on Nationality
#good for comparing gender within nationality 
ggplot(dat, aes(Nat, Percent, group=Animal,
                col = Animal))+
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_wrap(~Gender)

#same as above but showing how you can highlight one bar
ggplot(dat, aes(Gender, Percent, group=Animal,
                col = Animal=="Goose"))+
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_wrap(~Nat)

#dumbell plots facetted on nationality and sorted according to female average
#you could instead facet by gender if you wanted to focus on the differences between american and UK within gender
#could also use males as the reference rather than females, to highlight slightly different story
dat %>% 
  arrange(Gender,Percent) %>% 
  mutate(Animal = factor(Animal, unique(Animal))) %>%
  ggplot(aes(Animal, Percent, col=Gender))+
  geom_line(aes(group=Animal), col='grey', size=3, alpha=.3)+
  geom_point(size=4)+
  coord_flip()+
  facet_wrap(~Nat)+
  theme_minimal()+
  ylim(0,100)

