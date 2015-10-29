reverend<- read.csv(file="reverend.csv",head=TRUE,sep=",")
head(reverend)
summary(reverend)
str(reverend)
dim(reverend)
nrow(reverend)
ncol(reverend)

# Pie Chart from data frame with Appended Sample Sizes
gender <- table(reverend$Gender)
pct <- round(gender/sum(gender)*100)
lbls <- paste(names(gender), "\n", gender, sep="")
pie(gender, labels = lbls,
   main="Pie Chart of Gender\n (sample sizes)")

# Pie Chart with Percentages
gender <- table(reverend$Gender)
pct <- round(gender/sum(gender)*100)
lbls <- paste(names(gender), pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
png(filename="my.png")
pie(gender,labels = lbls, col=rainbow(length(lbls)),
   main="Gender Representation")
dev.off()

library(ggplot2)
#scatter plot
scatter <- ggplot(data=reverend, aes(x = Education_level, y = Age_bracket)) 
scatter + geom_point(aes(color=Type_of_Alcohol_your_relative_consumes)) +
  xlab("Education level") +  ylab("Age bracket") +
  ggtitle("Sepal Length-Width")

#scatter plot with group by
scatter <- ggplot(data=reverend, aes(x = Education_level, y = Age_bracket)) 
scatter + geom_point(aes(color=Type_of_Alcohol_your_relative_consumes, shape=Type_of_Alcohol_your_relative_consumes)) +
  xlab("Education level") +  ylab("Age bracket") +
  ggtitle("Sepal Length-Width")
#bar plots
png(filename="2.png")
ggplot(reverend, aes(x = Age_bracket))+ theme_minimal() + geom_bar()
dev.off()
# with another theme
png(filename="1.png")
ggplot(reverend, aes(x = Level_of_education))+ theme_minimal() + geom_bar( fill="#990000", colour="black")+
  ggtitle("Highest level of Education")
dev.off()

png(filename="3.png")
ggplot(reverend, aes(x = Employment_Status))+ theme_minimal() + geom_bar( fill="#339900", colour="black")+
  ggtitle("Employment Status")
dev.off()

ggplot(reverend, aes(x = Ever_sought_help_for_your_relative))+ theme_minimal() + geom_bar( fill="#339900", colour="black")+
  ggtitle("Ever sought help for your relative")
dev.copy(jpeg,filename="p5.jpg");
dev.off ()

##fill with color
ggplot(reverend, aes(x = Employment_Status))+ theme_minimal() + geom_bar( fill="#990000", colour="black")
ggplot(reverend, aes(x = Type_of_Alcohol_your_relative_consumes))+ theme_minimal() + geom_bar( fill="#00FF00", colour="black")

#Bar plots for 2 Categorical Variables
png("image.png", width = 900, height = 400)
ggplot(reverend, aes(x = Type_of_Alcohol_your_relative_consumes, 
fill = Level_of_education))+ geom_bar()+ stat_bin(geom = "text",
aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 1) 
dev.off ()

#PLOTTING WITH PERCENTAGE

library(scales)
ggplot(reverend, aes(x = Ever_sought_help_for_your_relative))+ theme_minimal() + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent) + 
ggtitle("Ever sought help for your relative")


ggplot(reverend, aes(x = Type_of_Alcohol_your_relative_consumes, 
fill = Ever_sought_help_for_your_relative))+ geom_bar() + scale_y_continuous(labels=percent) + stat_bin(geom = "text",
aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 1)


ggplot(reverend, aes(x = Type_of_Alcohol_your_relative_consumes, 
fill = Education_level))+ geom_bar()+  ggtitle("type of alcohol on education level")
# separating


ggplot(reverend, aes(x = Type_of_Alcohol_your_relative_consumes, 
fill = Ever_sought_help_for_your_relative))+ geom_bar(position = "dodge") 


#Dot plots
ggplot(reverend, aes(x = Type_of_Alcohol_your_relative_consumes)) + geom_dotplot()

#Density Plots
ggplot(reverend, aes(x = Type_of_Alcohol_your_relative_consumes)) + geom_density()
ggplot(reverend, aes(x = Type_of_Alcohol_your_relative_consumes)) + geom_density(adjust = 4)
ggplot(reverend, aes(x = Type_of_Alcohol_your_relative_consumes)) + geom_density(adjust = 0.25)

#Box and Whisker Plots
ggplot(reverend, aes(x = Type_of_Alcohol_your_relative_consumes, 
y = Employment_Status)) + geom_boxplot()

#flipping
ggplot(reverend, aes(x = Type_of_Alcohol_your_relative_consumes,
 y = Frequency)) + geom_boxplot() + coord_flip()

## pie charts
ggplot(reverend,aes(x = factor(""), fill = Gender)) +geom_bar() +coord_polar(theta = "y") +scale_x_discrete("")
ggplot(reverend, aes(x=factor(1), Age_bracket, 
fill=as.factor(paste(Gender,Age_bracket, sep=" - ")))) + geom_bar(stat="identity", width=1) + ggtitle("Rainfall - 2014")+coord_polar(theta = "y")

# create an empty variable foo by repeating 0 for the number of cases in
# students
foo = rep(0, nrow(reverend))
# set the positions where Level == 'Chang,aa/ Kumikumi/ illegal brews' to be 1
foo[with(reverend, Level == "Chang,aa/ Kumikumi/ illegal brews")] = 1
# now do the others
6
foo[with(reverend, Level == "First generation/ Braded bear")] = 2
foo[with(reverend, Level == "Traditional Liquor/ Muratina/ Beers prepared at home for occassions")] = 3
foo[with(reverend, Level == "None")] = 4
# look at foo to see if it looks right
foo
ggplot(reverend, aes(x = Type_of_Alcohol_your_relative_consumes)) + geom_bar()



#summary tables
with(reverend, table(Gender))
with(reverend, table(Gender,Age_bracket))
with(reverend, table(Type_of_Alcohol_your_relative_consumes))
with(reverend, table(Type_of_Alcohol_your_relative_consumes,Education_level))
tab = with(reverend, table(Gender))
tab/sum(tab)


library(sqldf)

