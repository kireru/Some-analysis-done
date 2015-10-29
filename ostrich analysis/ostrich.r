Ostrich2<- read.csv(file="ostrich final data.csv",head=TRUE,stringsAsFactors=F,sep=",")
Ostrich<- read.csv(file="ostritch data final -  2015.csv",head=TRUE,sep=",")

#CHECKING DATA

str(Ostrich)
summary(Ostrich) 
mydata$sum <- mydata$x1 + mydata$x2

library(dplyr)
library(tidyr)

mytable <- table(Group.size,Adult.male,Adult.females,Juveniles,chicks, data=Ostrich) # A will be rows, B will be columns

margin.table(mytable, 1) # A frequencies (summed over B)
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages

library(ggplot2)

Ostrich$Adults <- Ostrich$Adult.male + Ostrich$Adult.females
Ostrich$chicks <- as.numeric(Ostrich$chicks)
Ostrich$Young<- Ostrich$Juveniles + Ostrich$chicks

#PIE CHART

Area_type <- table(Ostrich$Habitat.type)
pct <- round(Area_type/sum(Area_type)*100)
lbls <- paste(names(Area_type), "\n", Area_type, sep="")
pie(Area_type, labels = lbls,
   main="Pie Chart of Area_type\n (sample sizes)")

#PIE CHART Percentage of type

Area_type <- table(Ostrich$Habitat.type)
pct <- round(Area_type/sum(Area_type)*100)
lbls <- paste(names(Area_type), pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(Area_type,labels = lbls, col=rainbow(length(lbls)),
   main="Area type Representation")

#PIE CHART Percentage For habitat with legends

Habitat <- table(Ostrich$Habitat)
pct <- round(Habitat/sum(Habitat)*100)
cols<- c("slategray1", "yellowgreen", "tan4","whitesmoke")
pielabels<- paste(pct, "%", sep="")
pie(Habitat, main="Area type Representation",col=cols,labels=pielabels, cex=0.8)
legend("topright", c("1","2","3","4"),col=cols, cex=0.8,fill=cols)

#PIE CHART Percentage For habitat with labels
pielabels<- paste(pct, "%", sep="")
lbls <- paste(names(Habitat), pct) # add percents to labels
pie(Habitat,labels = lbls, col=rainbow(length(lbls)),
   main="Area type Representation")

#SUMMARIZING
sub.exp <- Ostrich %>% select(Group.size, Habitat.type, X2007:X2011) 
sub.exp %>% summarise(Group.size1 = mean(Group.size))
Ostrich %>%
        group_by(Habitat.type)%>% 
        summarise(Mean_Habitat.type = Group.size, na.rm=TRUE)

#comparing groups
ggplot(Ostrich, aes(x = Adults, fill = Habitat.type)) +
  geom_density() 

ggplot(Ostrich, aes(x = Adults, fill = Region)) +
  geom_density() +
  facet_grid(Weather_Condition ~ .)

#comparing  activity 
Activity.1 <- subset(Ostrich, Activity > 0)
ggplot(Activity.1, aes(x = Adults, fill = Activity)) +
  geom_density() +
  facet_grid(Weather_Condition ~ .)

ggplot(Ostrich, aes(x = Young, fill = Region)) +
  geom_density() +
  facet_grid(Weather_Condition ~ .)
# the data has errors because it reflect NAs as part of the data. as shown
p <- qplot(Adults, Group.size, data=Ostrich, shape=Region, color=Region,
   facets=Season~Habitat, main="Scatterplots of Group size vs. Ostrich Adults",
   xlab="Adults", ylab="Group.size")

# White background and black grid lines
p + theme_bw()
p + theme(axis.title=element_text(face="bold.italic",
   size="12", color="brown"), legend.position="top") 

#Subsetting the data
Habitat.1 <- subset(Ostrich, Habitat > 0)
#drawing the plot again. you reliaze that the errors have been eliminated now.
p <- qplot(Adults, Group.size, data=Habitat.1, shape=Region, color=Region,
   facets=Season~Habitat, main="Scatterplots of Group size vs. Ostrich Adults",
   xlab="Adults", ylab="Group.size")

# White background and black grid lines
p + theme_bw()
p + theme(axis.title=element_text(face="bold.italic",
   size="12", color="brown"), legend.position="top")
Ostrichs <- Ostrich$Adult.male + Ostrich$Adult.females + Ostrich$Juveniles + Ostrich$chicks
Ostrichs.1 <- as.numeric(Ostrichs)
Ostrich$Season.1 <- as.numeric(Ostrich$Season)

#boxplot
boxplot(Ostrichs, xlab="Total Ostriches")

op<-par(mfrow=c(2,2), mar=c(4,3,3,2))
boxplot(Ostrich$Adult.male,
        Ostrich$Adult.females,
        Ostrich$Juveniles ,
        Ostrich$chicks ,
        names=c("MA","FM","JU","CH"))

Ostrich$chicks <- as.numeric(Ostrich$chicks)

Ostrichs.SQ<- sqrt(Ostrichs)
Ostrich$Adult.male.SQ <- sqrt(Ostrich$Adult.male)
Ostrich$Adult.females.SQ <- sqrt(Ostrich$Adult.females)
Ostrich$Juveniles.SQ <- sqrt(Ostrich$Juveniles)
Ostrich$chicks.SQ <- sqrt(Ostrich$chicks)

boxplot(Ostrich$Adult.male.SQ,
        Ostrich$Adult.females.SQ,
        Ostrich$Juveniles.SQ,
        Ostrich$chicks.SQ,
        names=c("MA","FM","JU","CH"))

Ostrich$fHabitat <- factor(Ostrich$Habitat)
Ostrich$fSeason <- factor(Ostrich$Season)
Ostrich$fGroup.size  <- factor(Ostrich$Group.size )

boxplot(Ostrichs.SQ ~ fHabitat,
        data=Ostrich)

boxplot(Adult.male.SQ ~ fSeason,
        data=Ostrich)

par(op)
Ostrich$Season.1 <- as.numeric(Ostrich$Season)
op<-par(mfrow=c(2,2), mar=c(4,3,3,2))
dotchart(Ostrich$Adult.male, main = "Adult.male", pch=Ostrich$Season.1)
dotchart(Ostrich$Adult.females, main = "Adult.females", pch=Ostrich$Season.1)
dotchart(Ostrich$Juveniles, main = "Juveniles", pch=Ostrich$Season.1)
dotchart(Ostrich$chicks, main = "chicks", pch=Ostrich$Season.1)
par(op)

par(op)
op<-par(mfrow=c(2,2), mar=c(4,3,3,2))
hist(Ostrich$Juveniles, main="Juveniles",xlab="")
hist(Ostrich$chicks, main="chicks",xlab="")
hist(Ostrich$Adult.male, main="Males",xlab="")
hist(Ostrich$Adult.females, main="Females",xlab="")
par(op)

Ostrich$Adult.male.SQ <- sqrt(Ostrich$Adult.male)
Ostrich$Adult.male.SQ4 <- Ostrich$Adult.male^(0.25)
Ostrich$Adult.male.Log <- log(Ostrich$Adult.male+1)

op<-par(mfrow=c(2,2), mar=c(4,3,3,2))
qqnorm(Ostrich$Adult.male,main="None (p=1)")
qqline(Ostrich$Adult.male)
qqnorm(Ostrich$Adult.male.SQ,main="Square root (p=0.5)")
qqline(Ostrich$Adult.male.SQ)
qqnorm(Ostrich$Adult.male.SQ4,main="Fourth root (p=0.25)")
qqline(Ostrich$Adult.male.SQ4)
qqnorm(Ostrich$Adult.male.Log,main="Logarirthmic (p=0)")
qqline(Ostrich$Adult.male.Log)
par(op)

source("MyLibrary.R")
Ostrich$Season.SQ <- sqrt(Ostrich$Season)
Z <- cbind(Ostrich$Adult.male,Ostrich$Adult.females,
           Ostrich$Juveniles,Ostrich$chicks,
           Ostrich$Season)
pairs(Z,lower.panel=panel.cor,
       labels = c("MA","FA","JU","CH","Season"))


plot(x = Ostrich$Region, y = Ostrich$Group.size, xlab="Region", ylab = "Group size")
Ostrich$fSeason <- factor(Ostrich$Season)
Ostrich$fHabitat <- factor(Ostrich$Habitat)
coplot(Region~Group.size | fSeason * fHabitat, data = Ostrich)

Ostrich$Adults <- Ostrich$Adult.male + Ostrich$Adult.females
Ostrich$Young<- Ostrich$Juveniles + Ostrich$chicks

#ANOVA
Ostrich_model.1<-lm(Ostrichs ~ Season, data = Ostrich)
anova(Ostrich_model.1)

#     Backward selection
Ostrich_model.2<-lm(Ostrichs ~ Season+Region+factor(Habitat), 
                 data = Ostrich)
step(Ostrich_model.2, direction = "backward")
#     One variable is dropped in turn
drop1(Ostrich_model.2, test = "F")

#poisson adults
Ostrich_poisson <- glm(Ostrich$Adults ~ Ostrich$Habitat, 
					data = Ostrich, family = poisson)
summary(Ostrich_poisson)

# poisson young
Ostrichy_poisson <- glm(Ostrich$Young ~ Ostrich$Habitat, 
					data = Ostrich, 
					family = poisson)
summary(Ostrichy_poisson)

Ostrichs <- Ostrich$Adult.male + Ostrich$Adult.females + Ostrich$Juveniles + Ostrich$chicks

OstrichTotal_poisson <- glm(Ostrichs ~ Ostrich$Season, 
					data = Ostrich, 
					family = poisson)
summary(OstrichTotal_poisson)

Ostrich_poisson.1 <- glm(Ostrichs ~ Season+ 
						factor(Region) + 
						factor(Habitat), 
						data = Ostrich, 
						family = poisson)
drop1(Ostrich_poisson.1, test = "Chisq")


#PART B

Ostrich3<- read.csv(file="OSTRICH BEHAVIOUR RECORDS.csv",head=TRUE,sep=",")
str(Ostrich3)
summary(Ostrich3)

Behavior<-c(Ostrich3$FEEDING,Ostrich3$RESTING, Ostrich3$PREENING, Ostrich3$INCUBATING,
	     Ostrich3$AGRESSION,Ostrich3$VIGILANT,Ostrich3$MOVING,Ostrich3$MOVING.OUT.OF.SIGHT,Ostrich3$RUNNING)

require(graphics)
library(lattice)

# boxplots for each combination of two factors

bwplot(FID~REGION|Season, data=Ostrich3,
   ylab="Cylinders", xlab="Miles per Gallon",
   main="Mileage by Cylinders and Gears")

#Re plotting changing directions
FID.1 <- subset(Ostrich3, FID > 1)
bwplot(FID~REGION|Season, data=FID.1,
   ylab="Cylinders", xlab="Miles per Gallon",
   main="Mileage by Cylinders and Gears",layout=c(1,2))

# kernel density plots by factor level (alternate layout)

#plotting the data
densityplot(~FID|Season,data=FID.1,
   main="Density Plot of FID",
   xlab="FID",layout=c(1,2))

# xyplot for each combination
 
xyplot(FEEDING + RESTING ~ PREENING + INCUBATING | REGION,
       data=Ostrich3, scales = "free", layout = c(2, 2),
       auto.key = list(x = .6, y = .7, corner = c(0, 0)))


stripplot(behavior.bind~REGION , data=Ostrich3, aspect = 1,
          jitter.data = TRUE, xlab = "Species")

bwplot(FID ~ REGION,Ostrich3$DIST..TO.OSTRICH,data=Ostrich3, groups = HABITAT,
       panel = "panel.superpose",
       panel.groups = "panel.linejoin",
       xlab = "treatment",
       key = list(lines = Rows(trellis.par.get("superpose.line"),
                  c(1:7, 1)),
                  text = list(lab = as.character(unique(Ostrich3$SPECIES))),
                  columns = 4, title = "Row position"))

# combining the output (The combine step)
behavior.bind <- cbind(Ostrich3$FEEDING,Ostrich3$RESTING,Ostrich3$RESTING,Ostrich3$INCUBATING,Ostrich3$AGRESSION,
				Ostrich3$VIGILANT,Ostrich3$MOVING,Ostrich3$MOVING.OUT.OF.SIGHT,Ostrich3$RUNNING)
library(lattice)

xyplot(FEEDING + RESTING + 
       INCUBATING + AGRESSION + 
       VIGILANT + MOVING + 
       MOVING.OUT.OF.SIGHT + 
       RUNNING  ~ HABITAT, data = Ostrich3, 
       col = 1, outer = T, 
       xlab = "Habitat ", ylab = "Number Recorded",  
       scales = list(x = list(relation = "same"), 
                     y = list(relation = "free")), 
       background = "white")

library(ggplot2)
ggplot((Ostrich3, aes(x = mean.set , fill = Season)) +
  geom_density()

